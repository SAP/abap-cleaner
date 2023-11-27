package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class AddToEtcRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid obsolete language elements", "#avoid-obsolete-language-elements"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Use the operator format", "abencalc_expresssion_guidl.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Obsolete Calculation Statements: ADD", "abapadd.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Obsolete Calculation Statements: SUBTRACT, MULTIPLY, DIVIDE", "abapsubtract_multiply_divide.htm") };

	@Override
	public RuleID getID() { return RuleID.ADD_TO_ETC; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace obsolete ADD ... TO etc. with += etc."; }

	@Override
	public String getDescription() { return "Replaces obsolete ADD TO, SUBTRACT FROM, MULTIPLY BY and DIVIDE BY statements with the corresponding calculation assignment operators +=, -=, *=, and /=."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.54. For older syntax, the statements can be replaced with a = a + ... etc. (see options)"; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 1); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_obsolete_add_to_etc." 
			+ LINE_SEP + "    ADD 1 TO ls_struc-component." 
			+ LINE_SEP + "    ADD iv_value TO lv_length." 
			+ LINE_SEP 
			+ LINE_SEP + "    SUBTRACT lo_typedesc->length FROM lv_length." 
			+ LINE_SEP + "    SUBTRACT is_struc-component FROM lv_length." 
			+ LINE_SEP 
			+ LINE_SEP + "    SUBTRACT 1 FROM ls_any_structure-item_key." 
			+ LINE_SEP + "    SUBTRACT lo_typedesc->length FROM lv_length." 
			+ LINE_SEP 
			+ LINE_SEP + "    MULTIPLY iv_value BY 2." 
			+ LINE_SEP + "    MULTIPLY lv_value BY ls_any_structure-component." 
			+ LINE_SEP 
			+ LINE_SEP + "    DIVIDE lv_value BY lo_instance->attribute." 
			+ LINE_SEP + "    DIVIDE lv_value BY lo_instance->ms_structure-component." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" chains can only be processed if they are first unchained" 
			+ LINE_SEP + "    ADD 10 TO: lv_value, lv_other." 
			+ LINE_SEP 
			+ LINE_SEP + "    SUBTRACT: 1 FROM lv_value, 2 FROM lv_other." 
			+ LINE_SEP 
			+ LINE_SEP + "    MULTIPLY iv_value BY: 2, 3, 5." 
			+ LINE_SEP 
			+ LINE_SEP + "    DIVIDE iv_value: BY lv_any, BY lv_other." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigEnumValue<AddToReplacementStyleForOldRelease> configReplacementStyleForOldRelease = new ConfigEnumValue<AddToReplacementStyleForOldRelease>(this, "AddToReplacementStyle", "If cleanup is restricted to NetWeaver < 7.54 syntax,",
			new String[] { "keep obsolete statements", "replace with 'a = a + ...' etc." }, AddToReplacementStyleForOldRelease.values(), AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP, AddToReplacementStyleForOldRelease.KEEP, LocalDate.of(2022, 8, 26));
	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain ADD:, SUBTRACT: etc. chains (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));

	private final ConfigValue[] configValues = new ConfigValue[] { configReplacementStyleForOldRelease, configProcessChains };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private AddToReplacementStyleForOldRelease getReplacementStyleForOldRelease () {
		return AddToReplacementStyleForOldRelease.forValue(configReplacementStyleForOldRelease.getValue());
	}

	public AddToEtcRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstCodeToken();
		if (firstToken == null)
			return false;
		
		if (	!firstToken.matchesOnSiblings(true, "ADD", TokenSearch.ASTERISK, "TO")
			&& !firstToken.matchesOnSiblings(true, "SUBTRACT", TokenSearch.ASTERISK, "FROM")
			&& !firstToken.matchesOnSiblings(true, "MULTIPLY", TokenSearch.ASTERISK, "BY")
			&& !firstToken.matchesOnSiblings(true, "DIVIDE", TokenSearch.ASTERISK, "BY")) {
			return false;
		}
		
		// determine whether introducing calculation assignment operators is allowed by the release restrictions;
		// if not, determine whether the obsolete statement should be kept or replaced with 'a = a + ...' etc.
		boolean areCalcAssignOpsAllowed = isCleanupAllowedFor(ABAP.REQUIRED_RELEASE_754, code, releaseRestriction);
		if (!areCalcAssignOpsAllowed && getReplacementStyleForOldRelease() == AddToReplacementStyleForOldRelease.KEEP)
			return false;
		
		ArrayList<Command> unchainedCommands = unchain(code, command, configProcessChains.getValue());
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return false;
		
		for (Command unchainedCommand : unchainedCommands) {
			if (	replaceAddOrSubtract(code, unchainedCommand, "ADD", "TO", "+", areCalcAssignOpsAllowed) 
				|| replaceAddOrSubtract(code, unchainedCommand, "SUBTRACT", "FROM", "-", areCalcAssignOpsAllowed)
				|| replaceMultiplyOrDivide(code, unchainedCommand, "MULTIPLY", "BY", "*", areCalcAssignOpsAllowed) 
				|| replaceMultiplyOrDivide(code, unchainedCommand, "DIVIDE", "BY", "/", areCalcAssignOpsAllowed)) {
				
				code.addRuleUse(this, unchainedCommand);
			}
   	}
		return false; // addRuleUse() was already called above
	}

	private boolean replaceAddOrSubtract(Code code, Command command, String keyword1Text, String keyword2Text, String operator, boolean areCalcAssignOpsAllowed) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(true, keyword1Text, TokenSearch.ANY_ARITHMETIC_EXPRESSION, keyword2Text, TokenSearch.ANY_IDENTIFIER, "."))
			return false;

		Token keyword1 = firstToken;
		Term term;
		try {
			term = Term.createArithmetic(keyword1.getNextCodeToken());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		int termPos = term.firstToken.getStartIndexInLine();
		Token keyword2 = term.getNextCodeToken();
		Token destVariable = keyword2.getNextCodeToken();
		int sourceLineNum = keyword1.sourceLineNum;

		// insert destination variable before keyword1 ("ADD", "SUBTRACT")
		destVariable.removeFromCommand();
		destVariable.copyWhitespaceFrom(keyword1);
		keyword1.insertLeftSibling(destVariable);
		keyword1.setWhitespace();

		if (areCalcAssignOpsAllowed) {
			// insert calculation assignment operator (+= or -=) 
			keyword1.insertLeftSibling(Token.createForAbap(0, 1, operator + "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		} else {
			// insert assignment operator (=), destination variable (again) and operator (+ or -) 
			keyword1.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
			keyword1.insertLeftSibling(Token.createForAbap(0, 1, destVariable.getText(), TokenType.IDENTIFIER, sourceLineNum));
			keyword1.insertLeftSibling(Token.createForAbap(0, 1, operator, TokenType.OTHER_OP, sourceLineNum));
		}

		// remove keyword1 ("ADD", "SUBTRACT") and keyword2 ("TO", "FROM")
		keyword1.removeFromCommand();
		keyword2.removeFromCommand();

		term.addIndent(term.firstToken.getStartIndexInLine() - termPos);

		command.invalidateMemoryAccessType();

		return true;
	}

	private boolean replaceMultiplyOrDivide(Code code, Command command, String keyword1Text, String keyword2Text, String operator, boolean areCalcAssignOpsAllowed) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(true, keyword1Text, TokenSearch.ANY_IDENTIFIER, keyword2Text, TokenSearch.ANY_ARITHMETIC_EXPRESSION, "."))
			return false;
		// do not allow a comment after the first keyword (or otherwise, the comment would have to be moved up into an own Command)  
		if (!firstToken.getNext().isIdentifier())
			return false;
		
		Token keyword1 = firstToken;
		Token destVariable = keyword1.getNextCodeToken();
		Token keyword2 = destVariable.getNextCodeToken();
		Term term;
		try {
			term = Term.createArithmetic(keyword2.getNextCodeToken());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		int termPos = term.firstToken.getStartIndexInLine();
		int sourceLineNum = destVariable.sourceLineNum;
		
		destVariable.copyWhitespaceFrom(keyword1);
		
		if (areCalcAssignOpsAllowed) {
			// insert calculation assignment operator (*= or /=)
			keyword2.insertLeftSibling(Token.createForAbap(0, 1, operator + "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		} else {
			// insert assignment operator (=), destination variable (again) and operator (* or /) 
			keyword2.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
			keyword2.insertLeftSibling(Token.createForAbap(0, 1, destVariable.getText(), TokenType.IDENTIFIER, sourceLineNum));
			keyword2.insertLeftSibling(Token.createForAbap(0, 1, operator, TokenType.OTHER_OP, sourceLineNum));
		}

		// remove keyword1 ("DIVIDE", "MULTIPLY") and keyword2 ("BY")
		keyword1.removeFromCommand();
		keyword2.removeFromCommand();

		term.addIndent(term.firstToken.getStartIndexInLine() - termPos);

		command.invalidateMemoryAccessType();

		return true;
	}
}
