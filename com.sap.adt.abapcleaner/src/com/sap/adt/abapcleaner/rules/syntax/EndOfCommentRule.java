package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class EndOfCommentRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't add method signature and end-of comments", "#dont-add-method-signature-and-end-of-comments") };

	@Override
	public RuleID getID() { return RuleID.END_OF_COMMENT; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Remove end-of comments"; }

	@Override
	public String getDescription() { return "Removes comments after ENDCLASS, ENDMETHOD, ENDLOOP, ENDIF etc., esp. if they are redundant, only repeating the method name, loop variable etc."; }

	@Override
	public String getHintsAndRestrictions() { return "Pseudo-comments are always kept."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 6, 30); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS lcl_any_class DEFINITION." 
			+ LINE_SEP + "  PUBLIC SECTION."
			+ LINE_SEP + "    DATA mv_any_attribute TYPE i."
			+ LINE_SEP + "ENDCLASS.                    \" LCL_ANY_CLASS definition"
			+ LINE_SEP
			+ LINE_SEP
			+ LINE_SEP + "CLASS lcl_any_class IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD remove_end_of_comments." 
			+ LINE_SEP + "    \" most end-of comments in this method are redundant, meaning that they only" 
			+ LINE_SEP + "    \" repeat the beginning of the corresponding METHOD, LOOP, IF etc. statement" 
			+ LINE_SEP + "    LOOP AT its_item INTO DATA(ls_item)." 
			+ LINE_SEP + "      AT NEW group."
			+ LINE_SEP + "        init( its_item-group )."
			+ LINE_SEP + "      ENDAT. \" new group" 
			+ LINE_SEP 
			+ LINE_SEP + "      IF ls_item-inner_item IS NOT INITIAL." 
			+ LINE_SEP + "        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "          \" some useful comment" 
			+ LINE_SEP + "        ENDLOOP. \" at ls_item-inner_item" 
			+ LINE_SEP + "      ENDIF. \" ls_item-inner_item not initial." 
			+ LINE_SEP 
			+ LINE_SEP + "      AT END OF group."
			+ LINE_SEP + "        finalize( its_item-group )."
			+ LINE_SEP + "      ENDAT. \" some non-redundant comment" 
			+ LINE_SEP + "    ENDLOOP. \" at item" 
			+ LINE_SEP + "  ENDMETHOD.                    \" REMOVE_END_OF_COMMENTS" 
			+ LINE_SEP
			+ LINE_SEP + "  METHOD other_method." 
			+ LINE_SEP + "    DO iv_num_processes TIMES." 
			+ LINE_SEP + "      CASE sy-index." 
			+ LINE_SEP + "        WHEN 1." 
			+ LINE_SEP + "          initialize( )." 
			+ LINE_SEP + "          process( )." 
			+ LINE_SEP + "        WHEN OTHERS." 
			+ LINE_SEP + "          process( )." 
			+ LINE_SEP + "      ENDCASE. \" sy-index" 
			+ LINE_SEP + "    ENDDO. \" num processes times" 
			+ LINE_SEP + "  ENDMETHOD. \" remove_end_of_comments [copy error!]" 
			+ LINE_SEP + "ENDCLASS. \" lcl_any_class IMPLEMENTATION";
   }

	private final String[] endOfCommentActionTexts = new String[] { "always keep", "remove if redundant", "always remove" };

	final ConfigEnumValue<EndOfCommentAction> configEndOfCommentActionInsideMethod = new ConfigEnumValue<EndOfCommentAction>(this, "EndOfCommentActionInsideMethod", "Action for end-of comments inside methods:", endOfCommentActionTexts, EndOfCommentAction.values(), EndOfCommentAction.REMOVE_REDUNDANT);
	final ConfigEnumValue<EndOfCommentAction> configEndOfCommentActionOutsideMethod = new ConfigEnumValue<EndOfCommentAction>(this, "EndOfCommentActionOutsideMethod", "Action for end-of comments outside of methods:", endOfCommentActionTexts, EndOfCommentAction.values(), EndOfCommentAction.REMOVE_REDUNDANT);
	final ConfigIntValue configLineLimitInsideMethod = new ConfigIntValue(this, "LineLimitInsideMethod", "Keep comment in method if opening command is >", "lines away", 10, 50, 999);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configEndOfCommentActionInsideMethod, configEndOfCommentActionOutsideMethod, configLineLimitInsideMethod };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EndOfCommentRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.getClosesLevel() || command.getOpensLevel())
			return false;
		if (!command.getLastToken().isCommentAfterCode())
			return false;

		Command openingCommand = command.getOpeningCommand();
		if (openingCommand == null)
			return false;
		
		EndOfCommentAction endOfCommentAction;
		int lineLimit = -1;
		if (command.firstCodeTokenIsAnyKeyword("ENDMETHOD", "ENDFUNCTION", "ENDFORM", "ENDMODULE", "ENDCLASS", "ENDINTERFACE")) {
			endOfCommentAction = EndOfCommentAction.forValue(configEndOfCommentActionOutsideMethod.getValue());
		} else {
			endOfCommentAction = EndOfCommentAction.forValue(configEndOfCommentActionInsideMethod.getValue());
			lineLimit = configLineLimitInsideMethod.getValue();
		}
		if (endOfCommentAction == EndOfCommentAction.KEEP)
			return false;
		if (lineLimit > 0 && command.getSourceLineNumStart() - openingCommand.getSourceLineNumStart() > lineLimit) 
			return false;
		
		Token comment = command.getLastToken();
		if (comment.isPseudoComment())
			return false;
		
		if (endOfCommentAction == EndOfCommentAction.REMOVE_REDUNDANT) {
			// only remove the comment if it matches the start of the corresponding opening Command; examples: 
			// - end-of comment "any_method" matches "METHOD any_method."
			// - end-of comment "sy-index" matches "CASE sy-index."
			// - end-of comment "at table item" matches "LOOP AT its_table_item ..."
			if (!commentMatchesStartOfCommand(comment, openingCommand)) {
				return false;
			}
		}
		
		comment.removeFromCommand();
		return true;
	}
	
	private boolean commentMatchesStartOfCommand(Token commentToken, Command openingCommand) {
		// try to match the comment text with the opening Command
		String comment = StringUtil.removePrefix(commentToken.getText(), ABAP.COMMENT_SIGN_STRING, false).trim();
		Token token = openingCommand.getFirstCodeToken();
		
		while (token != null) {
			// return true if the complete comment text could be matched or only "..." is left
			if (comment.length() == 0 || comment.equals(".") || comment.equals("..") || comment.equals("...") || comment.equals("...."))
				return true;

			String matchText = token.getText();
			
			// if a keyword is matched, also expect a space after it
			if (token.isKeyword() && token.getNext() != null && !token.getNext().isAttached() && comment.length() > matchText.length())
				matchText += " ";
			
			if (StringUtil.startsWith(comment, matchText, true)) {
				comment = comment.substring(matchText.length()).trim();

			} else if (token.isKeyword() || token.isPragma() || token.isComment()) {
				// continue with the next Token, because keywords, pragmas and comments are optional during this matching
			
			} else if (token.isIdentifier() && token.getText().indexOf('_') >= 0) {
				// match the identifier (e.g. 'lts_table_item') with the comment (e.g. 'table item'), 
				comment = removeMatchingIdentifier(comment, token.getText());
				if (comment == null) {
					return false;
				}

			} else if (token.textEqualsAny("(", ")")) {
				// continue with the next Token, because this may be omitted
				
			} else {
				// stop matching
				break;
			}

			token = token.getNext();
		}
		
		return (comment.length() == 0);
	}
	
	private String removeMatchingIdentifier(String comment, String identifier) {
		// match the comment with the identifier, allowing the comment to separate with space (instead of _) and omit prefixes
		String[] matchBits = StringUtil.split(identifier, '_', true);
		// regard the first matchBit as optional, because it could be a prefix such as 'ITS_...'
		boolean isOptional = (matchBits.length > 1);
		
		for (String matchBit : matchBits) {
			if (StringUtil.startsWith(comment, matchBit, true)) {
				comment = comment.substring(matchBit.length()).trim();
			} else if (comment.length() > 0 && !isOptional) {
				// matching failed
				return null;
			}
			isOptional = false;
		}
		return comment;
	}
}