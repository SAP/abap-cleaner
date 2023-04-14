package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class ChainRule extends RuleForCommands {
	static final String displayName = "Unchain into multiple statements";
	
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Do not chain up-front declarations", "#do-not-chain-up-front-declarations"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Chain Declaration Usage", "chain-declaration-usage.md"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Only use chained statements where appropriate", "abenchained_statements_guidl.htm") };

	@Override
	public RuleID getID() { return RuleID.DECLARATION_CHAIN; } // for compatibility, this old RuleID is kept, while the class was renamed from DeclarationChainRule to ChainRule 

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() {
		return "Resolves a chain (DATA:, FIELD-SYMBOLS:, ASSERT: etc.) into multiple standalone statements. The chain is kept, however, for declarations with BEGIN OF ... END OF blocks.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 21); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.LOCAL_DECLARATION_ORDER, RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_DECLARATIONS } ; }

	@Override
	public boolean isActiveByDefault() { return true; } 
		
	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS cl_unchaining DEFINITION." 
 			+ LINE_SEP + "  PUBLIC SECTION." 
 			+ LINE_SEP + "    CONSTANTS: any_constant TYPE i VALUE 1,"
 			+ LINE_SEP + "               other_constant TYPE i VALUE 2."
 			+ LINE_SEP 
			+ LINE_SEP + "    \" BEGIN OF ... END OF blocks are always kept as a chain:" 
			+ LINE_SEP + "    TYPES: " 
			+ LINE_SEP + "      BEGIN of xy," 
			+ LINE_SEP + "        a TYPE x," 
			+ LINE_SEP + "        b TYPE y," 
			+ LINE_SEP + "      END OF xy." 
 			+ LINE_SEP 
 			+ LINE_SEP + "    METHODS:" 
 			+ LINE_SEP + "      setup," 
 			+ LINE_SEP + "      unchain." 
 			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP + "" 
 			+ LINE_SEP + "CLASS cl_unchaining IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD unchain." 
			+ LINE_SEP + "    CONSTANTS: " 
			+ LINE_SEP + "      lc_list_price_1200 TYPE ty_list_price VALUE 1200," 
			+ LINE_SEP + "      lc_amount_1000 TYPE ty_amount VALUE 1000," 
			+ LINE_SEP + "      lc_num_contract_change TYPE ty_sequence_number VALUE 1." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" alignment within the declaration line is not changed; empty lines are kept" 
			+ LINE_SEP + "    DATA: lth_any_hash_table TYPE ty_th_hash_table, \" comment" 
			+ LINE_SEP + "          lo_contract TYPE REF TO cl_contract  ##NEEDED," 
			+ LINE_SEP 
			+ LINE_SEP + "          ls_structure TYPE if_any_interface=>ty_s_structure," 
			+ LINE_SEP + "          mv_bool_variable TYPE abap_bool VALUE abap_false ##NO_TEXT." 
			+ LINE_SEP 
			+ LINE_SEP + "    FIELD-SYMBOLS: \" comment above the first identifier" 
			+ LINE_SEP + "      <ls_data>      TYPE ty_s_data, ##PRAGMA_IN_WRONG_POSITION" 
			+ LINE_SEP + "      <ls_amount>  LIKE LINE OF its_amount, \" comment" 
			+ LINE_SEP 
			+ LINE_SEP + "      \" comment line within the chain" 
			+ LINE_SEP + "      <ls_contract_data> TYPE ty_s_contract_data," 
			+ LINE_SEP + "      <ls_parameter>   LIKE LINE OF mt_parameter." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT: lv_count = 1," 
			+ LINE_SEP + "            lts_table IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR: ev_result_a, ev_result_b." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" WRITE: chains make sense, as the output is on one line, too; they are therefore kept" 
			+ LINE_SEP + "    WRITE: / `text`, iv_value, `more text`, iv_other_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value += 2 ** : 1, 2, 3." 
			+ LINE_SEP 
			+ LINE_SEP + "    add_value( lv_value ):,,." 
			+ LINE_SEP 
			+ LINE_SEP + "    CALL METHOD any_method" 
			+ LINE_SEP + "      EXPORTING iv_value_a = 'text'" 
			+ LINE_SEP + "                iv_value_b = : 42, 84." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS." ;
   }

   final ConfigBoolValue configExecuteOnClassDefinitionSections = new ConfigBoolValue(this, "ExecuteOnClassDefinitionSections", "Unchain declarations in CLASS ... DEFINITION sections", true, false, LocalDate.of(2022, 4, 9));
   final ConfigBoolValue configExecuteOnLocalDeclarations = new ConfigBoolValue(this, "ExecuteOnLocalDeclarations", "Unchain declarations in methods etc.", true);
   final ConfigBoolValue configExecuteOnSimpleCommands = new ConfigBoolValue(this, "ExecuteOnSimpleCommands", "Unchain simple commands (chain after first keyword, e.g. ASSERT:, CHECK:, CLEAR:, FREE:) except WRITE:", false, false, LocalDate.of(2022, 4, 9));
   final ConfigBoolValue configExecuteOnComplexCommands = new ConfigBoolValue(this, "ExecuteOnComplexCommands", "Unchain complex commands (a += : 1,2,3 etc.)", true, false, LocalDate.of(2022, 4, 9));

   private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnClassDefinitionSections, configExecuteOnLocalDeclarations, configExecuteOnSimpleCommands, configExecuteOnComplexCommands };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ChainRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override 
	protected boolean skipDeclarationsInsideBeginOf() { 
		// skip this Command if it is a declaration inside a BEGIN OF block which was opened by a previous Command
		return true; 
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!command.containsChainColon())
			return false;

		// ensure the rule is configured to be executed on this command
		boolean execute;
		if (command.isInClassDefinition()) {
			execute = command.isDeclaration() || command.isDeclarationInclude() || command.isDeclarationInClassDef() ? configExecuteOnClassDefinitionSections.getValue() : false;
		} else if (command.isDeclaration() || command.isDeclarationInclude()) {
			execute = configExecuteOnLocalDeclarations.getValue();
		} else {
			execute = command.isSimpleChain() ? configExecuteOnSimpleCommands.getValue() : configExecuteOnComplexCommands.getValue();
			if (command.firstCodeTokenIsKeyword("WRITE"))
				execute = false;
		}
		if (!execute)
			return false;
		
		// ensure this is a chain with multiple elements, i.e. there is a comma in the command;
		// we expect the chain sign to be top-level (i.e. not inside parentheses etc.); this is ensured by Token.addNext()
		Token chainSign = command.getFirstToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ABAP.COLON_SIGN_STRING);
		if (chainSign == null || chainSign == command.getFirstToken())
			return false;
		Token comma = chainSign.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ABAP.COMMA_SIGN_STRING);
		if (comma == null)
			return false;

		// TODO: chains that just *contain* a BEGIN OF ... END OF section may nevertheless be partly unchained ...
		if (command.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, "BEGIN OF"))
			return false;

		Command originalCommand = (command.originalCommand != null) ? command.originalCommand : command;
		command.originalCommand = originalCommand;

		// get information on the first token of the to-be-repeated 'part A' section
		Token firstTokenOfPartA = command.getFirstToken();
		int indent = firstTokenOfPartA.getStartIndexInLine();
		int startLineBreaks = firstTokenOfPartA.lineBreaks;
		boolean useStartLineBreaks = true;

		// if the chain colon is only preceded by the first keyword (typically, a declaration keyword) or keyword collocation (like CALL METHOD) ...  
		Token prevNonComment = chainSign.getPrevNonCommentToken();
		boolean wasColonAfterInitialKeywords = firstTokenOfPartA.isKeyword() && (prevNonComment == firstTokenOfPartA.getLastTokenOfKeywordCollocation());
		if (wasColonAfterInitialKeywords) {
			// ... then split out possible comments which directly precede the colon sign into a new command
			ArrayList<Command> newCommentCommands = command.splitOutCommentLinesAfter(prevNonComment); 
			if (!newCommentCommands.isEmpty()) {
				useStartLineBreaks = false; // leading empty lines have now been moved to the extracted comment line
				code.addRuleUses(this, newCommentCommands);
			}
		}
		int startLineBreaksPartB = 0;
		int startSpacesLeftPartB = 1;
		if (!wasColonAfterInitialKeywords && (chainSign.lineBreaks > 0 || chainSign.getNextNonCommentToken().lineBreaks > 0)) {
			startLineBreaksPartB = 1;
			startSpacesLeftPartB = chainSign.getNextNonCommentToken().getStartIndexInLine();
		}
		
		// only now, after possible split-out of comments, determine the last token of the to-be-repeated 'part A' section
		Token lastTokenOfPartA = chainSign.getPrev();
		
		boolean moveFollowingLinesLeft = (chainSign.getNextNonCommentToken().lineBreaks == 0); // i.e. if the code continues behind the ":", not below it
		chainSign.removeFromCommand(moveFollowingLinesLeft);

		// remove all further colons from the command (see comment inside this method) 
		command.removeAllFurtherChainColons();	
		
		do {
			// if the chain colon was found directly after the first keyword (typically, a declaration keyword) or keyword collocation (like CALL METHOD) ...  
			if (wasColonAfterInitialKeywords) {
				// move comment lines (or an initial line-end comment behind the keyword or the chain sign) into a new command
				ArrayList<Command> newCommentCommands = command.splitOutCommentLinesAfter(lastTokenOfPartA); 
				if (!newCommentCommands.isEmpty()) {
					useStartLineBreaks = false; // leading empty lines have now been moved to the extracted comment line
					code.addRuleUses(this, newCommentCommands);
				}
			}
			
			// ensure there is at least one space before the next Token (e.g. "DATA:identifier,other_identifier")
			Token firstTokenOfPartB = lastTokenOfPartA.getNext();
			if (firstTokenOfPartB.lineBreaks == 0 && firstTokenOfPartB.spacesLeft == 0)
				firstTokenOfPartB.spacesLeft = 1;

			Token periodOrComma = lastTokenOfPartA.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (periodOrComma.isComma()) {
				// change the "," into ".' and determine the end of the section  
				periodOrComma.setText(ABAP.DOT_SIGN_STRING, false);
				periodOrComma.type = TokenType.PERIOD;
				Token lastTokenOfPartB = periodOrComma.getNext().isCommentAfterCode() ? periodOrComma.getNext() : periodOrComma;
				if (lastTokenOfPartB.getNext() != null && lastTokenOfPartB.getNext().isPragma() && lastTokenOfPartB.getNext().lineBreaks == 0) // pragma behind the , is a common mistake; we move the pragma along with the declaration line
					lastTokenOfPartB = lastTokenOfPartB.getNext();

				// copy the to-be-repeated 'part A' section to a new Command above the current one
				Command newCommand = null;
				Term partB = null;
				try {
					int lineBreaks = useStartLineBreaks ? startLineBreaks : Math.max(firstTokenOfPartB.lineBreaks, 1);
					newCommand = command.copyTokenRangeToNewCommand(firstTokenOfPartA, lastTokenOfPartA.getNext(), lineBreaks, indent);
					useStartLineBreaks = false;
					// create a Term for part B (reusing same try block)
					partB = Term.createForTokenRange(firstTokenOfPartB, lastTokenOfPartB);
				} catch (UnexpectedSyntaxException ex) {
					throw new UnexpectedSyntaxAfterChanges(this, ex);
				}

				// move 'part B' to the new Command - including the "," (which is now a "."!) and possibly the comment after it 
				int oldIndent = firstTokenOfPartB.getStartIndexInLine();
				partB.removeFromCommand(false); 
				if (firstTokenOfPartB.isPeriod() && startLineBreaksPartB == 0)
					firstTokenOfPartB.setWhitespace(0, 0);
				else
					firstTokenOfPartB.setWhitespace(startLineBreaksPartB, startSpacesLeftPartB);
				newCommand.getLastToken().insertRightSibling(partB);
				try {
					newCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
				} catch (ParseException e) {
					throw new UnexpectedSyntaxAfterChanges(null, originalCommand, "parse error in extracted declaration");
				}
				int newIndent = partB.firstToken.getStartIndexInLine();
				newCommand.addIndent(newIndent - oldIndent, oldIndent, partB.firstToken);

				command.insertLeftSibling(newCommand);
				code.addRuleUse(this, newCommand);
				// continue with loop

			} else {
				// last element: simply move the identifier behind the keyword
				firstTokenOfPartA.lineBreaks = useStartLineBreaks ? startLineBreaks : Math.max(firstTokenOfPartB.lineBreaks, 1);
				int oldIndent = firstTokenOfPartB.getStartIndexInLine();
				if (firstTokenOfPartB.isPeriod() && startLineBreaksPartB == 0)
					firstTokenOfPartB.setWhitespace(0, 0);
				else
					firstTokenOfPartB.setWhitespace(startLineBreaksPartB, startSpacesLeftPartB);
				int newIndent = firstTokenOfPartB.getStartIndexInLine();
				command.addIndent(newIndent - oldIndent, oldIndent, firstTokenOfPartB);
				useStartLineBreaks = false; // pro forma
				return true;
			}
		} while (true);
	}
}
