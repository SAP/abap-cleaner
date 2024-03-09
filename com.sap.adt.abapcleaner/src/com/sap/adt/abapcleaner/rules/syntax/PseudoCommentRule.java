package com.sap.adt.abapcleaner.rules.syntax;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentifier;

public class PseudoCommentRule extends RuleForTokens {
   private static boolean isInitialized;
   private static HashMap<String, String> pragmaOfPseudoComment;

   private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer pragmas to pseudo comments", "#prefer-pragmas-to-pseudo-comments"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Prefer Pragmas to Pseudo Comments", "prefer-pragmas-to-pseudo-comments.md"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Pseudo Comments for the Extended Program Check", "abenpseudo_comment_slin.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Pragmas", "abenpragma.htm") };

	@Override
	public RuleID getID() { return RuleID.PSEUDO_COMMENT; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Replace obsolete pseudo comments with pragmas"; }

	@Override
	public String getDescription() { return "Replaces obsolete pseudo comments (\"#EC ...) for the Extended Program Check (SLIN) with corresponding pragmas."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.0 EhP2. Note that pseudo comments for Code Inspector (\"#EC CI_...) are kept, as they have not been replaced by pragmas yet."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 20); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	public int getRequiredAbapRelease() { return ABAP.REQUIRED_RELEASE_702; }

	public boolean isEssential() { return true; }
	
	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_pseudo_comments." 
			+ LINE_SEP + "    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. \"#EC NOTEXT" 
			+ LINE_SEP
			+ LINE_SEP + "    DATA: a TYPE string, \"#ec NEEDED  \" additional textual comment" 
			+ LINE_SEP + "          b TYPE string." 
			+ LINE_SEP
			+ LINE_SEP + "    a = b." 
			+ LINE_SEP
			+ LINE_SEP + "    \" pseudo comments for Code Inspector (CI_...) are kept" 
			+ LINE_SEP + "    LOOP AT lt_data ASSIGNING <ls_data> WHERE id <= iv_id. \"#EC CI_SORTSEQ" 
			+ LINE_SEP + "      MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. \"#ec ENHOK" 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP
			+ LINE_SEP + "    DO 5 TIMES." 
			+ LINE_SEP + "      \"#EC NEEDED" 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP
			+ LINE_SEP + "    TRY." 
			+ LINE_SEP + "      GET BADI lo_any_badi." 
			+ LINE_SEP + "    CATCH cx_badi_not_implemented. \"#EC NO_HANDLER nothing to do here" 
			+ LINE_SEP + "    ENDTRY." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private static void initialize() {
		// read the map of pseudo comments and corresponding pragma from the resource;  
		// if needed, this file can be updated from table SLIN_DESC (columns PSEUDO_COM and PRAGMA) by removing all 
		// NOCOMMENT entries, and correcting the pragma 'ENHOK' into 'ENH_OK' to avoid a Syntax Check warning
		pragmaOfPseudoComment = initPragmaOfPseudoComment("pseudo_comments.txt");
		isInitialized = true;
	}

	private static HashMap<String, String> initPragmaOfPseudoComment(String resourceName) {
		HashMap<String, String> map = new HashMap<>();
		
		InputStream resourceStream = CommentIdentifier.class.getClassLoader().getResourceAsStream(resourceName);
		if (resourceStream == null)
			return map;
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(resourceStream, StandardCharsets.UTF_8));
		
		HashSet<String> ambiguousKeys = new HashSet<>();
		try {
			String line;
			do {
				line = bufferedReader.readLine();
				if (line == null || line.length() == 0) 
					continue;

				int tabPos = line.indexOf('\t');
				if (tabPos < 0)
					continue;
				
				String pseudoComment = line.substring(0, tabPos).toUpperCase();
				String pragma = line.substring(tabPos + 1).toUpperCase();
				if (!StringUtil.isNullOrEmpty(pseudoComment) && !StringUtil.isNullOrEmpty(pragma)) {
					String pragmaInMap = map.get(pseudoComment);
					if (pragmaInMap == null) {
						map.put(pseudoComment, pragma);
					} else if (!pragma.equals(pragmaInMap)) {
						// ambiguous mapping found - e.g., for pseudo comment "#EC LIT_INCOMP, there are two pragmas: 
						// ##LIT_COMPATIBLE and ##LIT_INCOMP 
						ambiguousKeys.add(pseudoComment);
					}
				}
			} while (line != null);

			bufferedReader.close();
		} catch (IOException e) {
		}

		// remove ambiguous mappings
		for (String ambiguousKey : ambiguousKeys) {
			map.remove(ambiguousKey);
		}

		return map;
	}

	public PseudoCommentRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!token.isPseudoComment())
			return false;

		if (!isInitialized)
			initialize();
		if (pragmaOfPseudoComment.isEmpty())
			return false;
		
		// pseudo comments only work if "#EC is followed by exactly one space, and if the code (e.g. NEEDED) is upper case
		// (while "#EC could also be lower or mixed case); we therefore only replace valid pseudo comments
		String pseudoComment = token.getText().substring(ABAP.PSEUDO_COMMENT_EC_PREFIX.length());
		
		// the comment may continue with a textual comment after a space, which is simply ignored by the Extended Program Check
		String textComment = null;
		int spacePos = pseudoComment.indexOf(' ');
		if (spacePos > 0) {
			textComment = pseudoComment.substring(spacePos).trim();
			pseudoComment = pseudoComment.substring(0, spacePos);
		}

		// determine the corresponding pragma name(e.g. pragma name "WARN_OK" for pseudo comment "WARNOK"); 
		// some pseudo comments such as those for Code Inspector "CI_..." have no matching pragma
		String pragmaName = pragmaOfPseudoComment.get(pseudoComment);
		if (pragmaName == null)
			return false;

		// create the pragma Token and determine where to insert it
		Token pragma = Token.createForAbap(0, 1, ABAP.PRAGMA_SIGN + pragmaName, TokenType.PRAGMA, token.sourceLineNum);
		Token prev = (token.isFirstTokenInLine() && !token.isOnlyTokenInLine()) ? null : token.getPrev();

		// if the pseudo comment is a stand-alone comment line, ...
		Command ruleUseCommand = command;
		if (command.isCommentLine()) {
			// ... determine whether the pragma should be moved to the end of the previous Command. This is required for
			// pragmas in empty blocks, e.g. ##NO_HANDLER in empty CATCH blocks, as well as ##NEEDED in other blocks  
			// (DO, LOOP, TRY, WHILE, METHOD, ...), whereas the pseudo comment also works as a comment inside the block
			if (!PragmaPositionRule.pragmaBelongsToPrevCommand(pragmaName, command)) 
				return false;
			Command prevCommand = command.getPrevNonCommentCommand();
			prev = prevCommand.getLastNonCommentToken();
			ruleUseCommand = prevCommand;
		}

		// insert the pragma Token in the correct position
		if (prev != null && prev.isCommaOrPeriod()) {
			// put the pragma left of the comma or period which preceded the pseudo comment
			prev.insertLeftSibling(pragma);

		} else if (prev != null && prev.isChainColon()) {
			// the pseudo comment is directly preceded by a chain colon; a pragma would now have to go before the chain 
			// colon; however, we do NOT do that, because the pragma would then be effective for all parts of the chain, 
			// which is NOT the effect of the pseudo comment
			return false;
			
		} else {
			pragma.copyWhitespaceFrom(token);
			token.setWhitespace();
			token.insertLeftSibling(pragma);
		}
		
		// remove the pseudo comment, or shorten it to only contain the extra textual comment (unless that is only a period ".")
		// sometimes, the extra textual comment is another "#EC pseudo comment, but such a pseudo comment has no effect in the  
		// Extended Program Check, so we keep it 'ineffective'
		if (StringUtil.isNullOrEmpty(textComment) || textComment.equals(".")) {
			if (command.isCommentLine()) {
				// remove the whole Command, because it only consisted of this pseudo comment 
				try {
					command.removeFromCode();
				} catch(UnexpectedSyntaxException ex) {
					throw new UnexpectedSyntaxAfterChanges(this,  ex);
				}
			} else { 
				token.removeFromCommand();
			}
		} else if (textComment.startsWith(ABAP.COMMENT_SIGN_STRING) && !textComment.startsWith(ABAP.PSEUDO_COMMENT_EC_PREFIX)) {
			token.setText(textComment, false);
		} else {
			token.setText(ABAP.COMMENT_SIGN_STRING + " " + textComment, false);
		}
		
		if (ruleUseCommand != command)
			code.addRuleUse(this, ruleUseCommand);

		return true;
	}
}