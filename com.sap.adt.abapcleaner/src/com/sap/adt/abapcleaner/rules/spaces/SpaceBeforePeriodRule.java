package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;

public class SpaceBeforePeriodRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Condense your code", "#condense-your-code") };

	@Override
	public RuleID getID() { return RuleID.SPACE_BEFORE_PERIOD; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Remove space before commas and period"; }

	@Override
	public String getDescription() { return "Removes spaces before chain commas and before the period at the end of a statement."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
         + LINE_SEP + "CLASS any_class DEFINITION ."
         + LINE_SEP + "  PUBLIC SECTION ."
         + LINE_SEP + "    DATA: mv_any_data TYPE i   ,"
         + LINE_SEP + "          mv_other_data TYPE string . "
         + LINE_SEP + ""
         + LINE_SEP + "    METHODS space_before_period_or_comma ."
         + LINE_SEP + "ENDCLASS."
         + LINE_SEP + ""
         + LINE_SEP + ""
         + LINE_SEP + "CLASS any_class IMPLEMENTATION."
			+ LINE_SEP + "  METHOD space_before_period_or_comma ." 
			+ LINE_SEP + "    DATA: lo_object TYPE cl_any_class ##NEEDED" 
			+ LINE_SEP + "          , lo_other_object TYPE cl_other_class ." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR:" 
			+ LINE_SEP + "      ev_any_value  ," 
			+ LINE_SEP + "      ev_other_value \" comment" 
			+ LINE_SEP + "      , ev_third_value" 
			+ LINE_SEP + "*      comment line" 
			+ LINE_SEP + "       \" another comment line" 
			+ LINE_SEP + "      , ev_fourth_value" 
			+ LINE_SEP + "    ." 
			+ LINE_SEP 
			+ LINE_SEP + "    TRY ." 
			+ LINE_SEP + "        any_operation( ) ." 
			+ LINE_SEP + "      CATCH cx_any ." 
			+ LINE_SEP + "    ENDTRY ." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = 42 \" comment" 
			+ LINE_SEP + "    ." 
			+ LINE_SEP + "  ENDMETHOD."
      	+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigBoolValue configExecuteOnComma = new ConfigBoolValue(this, "ExecuteOnComma", "Remove space before chain commas", true, false, LocalDate.of(2023, 3, 10));
	final ConfigBoolValue configExecuteOnPeriod = new ConfigBoolValue(this, "ExecuteOnPeriod", "Remove space before period", true);
	final ConfigBoolValue configMoveAcrossCommentLines = new ConfigBoolValue(this, "MoveAcrossCommentLines", "Move comma or period across comment lines", true, false, LocalDate.of(2024, 7, 5));
	final ConfigBoolValue configExecuteOnClassDefinitionSections = new ConfigBoolValue(this, "ExecuteOnClassDefinitionSections", "Execute on CLASS ... DEFINITION sections, too", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnPeriod, configExecuteOnComma, configMoveAcrossCommentLines, configExecuteOnClassDefinitionSections };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public SpaceBeforePeriodRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!configExecuteOnClassDefinitionSections.getValue() && command.isInClassDefinition())
			return false;
		else if (command.isCommentLine())
			return false;

		boolean executeOnComma = configExecuteOnComma.getValue();
		boolean executeOnPeriod = configExecuteOnPeriod.getValue();
		if (!executeOnComma && !executeOnPeriod)
			return false;

		// find all commas and the period (but use a shortcut if only the period shall be processed)
		Token token = executeOnComma ? command.getFirstToken() : command.getLastNonCommentToken();
		boolean changed = false;
		while (token != null) {
			token = token.getLastTokenOfSequence(true, true, null, TokenSearch.ASTERISK, ".|,");
			if (token == null)
				break;
			if ((executeOnComma && token.isComma()) || (executeOnPeriod && token.isPeriod())) {
				if (executeOnCommaOrPeriod(command, token)) {
					changed = true;
				}
			}
			token = token.getNext();
		}
		return changed;
	}

	private boolean executeOnCommaOrPeriod(Command command, Token token) throws UnexpectedSyntaxAfterChanges {
		Token prev = token.getPrev();
		if (prev == null)
			return false;

		// if the previous Token is a "/" (as in 'WRITE: / 'text', / .'), keep one space
		int requiredSpaces = prev.textEquals("/") ? 1 : 0;
		if (token.lineBreaks == 0 && token.spacesLeft <= requiredSpaces)
			return false;

		// process a period or comma that is already on the same line as the previous Token
		if (token.lineBreaks == 0) {
			token.spacesLeft = requiredSpaces;
			return true;
		} 
		
		if (prev.isCommentLine()) { 
			// if the previous Token is a whole comment line, only continue if configured
			if (!configMoveAcrossCommentLines.getValue()) {
				return false;
			}
			
			// skip the following case, in which older ABAP versions automatically generated this comment
			// when saving "FUNCTION any_function.":
			//   FUNCTION any_function
			//    " You can use the template 'functionModuleParameter' to add here the signature!
			//   .
			// If this rule now moved the period, a second comment would be generated another time when saving.
			if (command.firstCodeTokenIsKeyword("FUNCTION")
					&& StringUtil.contains(prev.getText(), "add here the signature")
					&& command.getFirstCodeToken().getNextCodeSibling() == token.getPrevCodeSibling()) {
				return false;
			}
		} 

		
		// process a period or comma which is currently on the next line
		Token next = token.getNext();
		// in case of a comma, ensure that any following Token will be on its own line
		if (token.isComma() && next != null && next.lineBreaks == 0) {
			next.copyWhitespaceFrom(token);
		}

		if (!prev.isComment()) {
			// move the token to the end of the previous line:
			// - in case of a comma, any following Token was already moved to an own line (see above)
			// - in case of a period, move the period and possibly the line-end comment after it
			token.setWhitespace(0, requiredSpaces);
			return true;

		} else if (token.isPeriod() && next != null) {
			// do not attempt to merge comments before and after the period
			return false;
		
		} else {
			// determine whether moving the Token will result in trailing comment lines
			boolean trailingCommentLines = (token.getNext() == null && prev.isCommentLine());
			// move to the first comment of multiple line-end comments and comment lines
			Token firstComment = prev.getPrevWhileComment().getNext();

			// move the period or comma between the code and the line-end comment in the previous line
			token.removeFromCommand(false, true);
			token.setWhitespace(0, requiredSpaces);
			if (trailingCommentLines) {
				// test referential integrity only after splitting out trailing comment lines
				firstComment.insertLeftSibling(token, false, true);
				command.splitOutTrailingCommentLines(command, true);
				command.testReferentialIntegrity(true);
			} else {
				firstComment.insertLeftSibling(token);
			}
			return true;
		}
	}
}
