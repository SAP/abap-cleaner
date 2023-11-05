package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class AssertClassRule extends RuleForCommands {
	public static final String displayName = "Use assert class instead of ASSERT";
	
	private static final String METHOD_NAME_BOUND = "assert_bound";
	private static final String METHOD_NAME_NOT_BOUND = "assert_not_bound";
	private static final String METHOD_NAME_INITIAL = "assert_initial";
	private static final String METHOD_NAME_NOT_INITIAL = "assert_not_initial";
	private static final String METHOD_NAME_FALSE = "assert_false";
	private static final String METHOD_NAME_TRUE = "assert_true";
	private static final String METHOD_NAME_EQUALS = "assert_equals";
	private static final String METHOD_NAME_DIFFERS = "assert_differs";
	private static final String METHOD_NAME_SUBRC = "assert_subrc";
	private static final String METHOD_NAME_FAIL = "fail";

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Write testable code", "#write-testable-code") };

	@Override
	public RuleID getID() { return RuleID.ASSERT_CLASS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Replaces ASSERT statements (in product code) with static calls to an assert class to make the error cases testable, too."; }

	@Override
	public String getHintsAndRestrictions() { return "Note that the class name must be adjusted to the respective application (CX_..._ASSERT). The class should at least implement the methods called in the example below."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 11); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isActiveByDefault() { return false; } // by default, deactivate this Rule, because the assert class name must first be adjusted to a class of the respective application 
		
	@Override
   public String getExample() {
      return "" 
      	+ LINE_SEP + "CLASS cl_product_code IMPLEMENTATION."
			+ LINE_SEP + "  METHOD use_assert_class." 
			+ LINE_SEP + "    ASSERT lo_instance IS BOUND." 
			+ LINE_SEP + "    ASSERT is_any_structure-component IS NOT BOUND." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT is_parameters-component_name IS INITIAL." 
			+ LINE_SEP + "    ASSERT io_any_instance IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT sy-subrc = 0. \" item must exist" 
			+ LINE_SEP + "    ASSERT sy-subrc = 4." 
			+ LINE_SEP + "    ASSERT sy-subrc = get_expected_subrc_value( param_a = 'abc' " 
			+ LINE_SEP + "                                                param_b = 'def' )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT lv_was_initialized = abap_true." 
			+ LINE_SEP + "    ASSERT mv_is_valid = abap_false." 
			+ LINE_SEP + "    ASSERT line_exists( lts_table[ iv_param_a = 1 " 
			+ LINE_SEP + "                                   iv_param_b = 'abc' ] ) = abap_true." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT ms_data-item_type = if_any_interface=>co_any_item_type." 
			+ LINE_SEP + "    ASSERT lv_timestamp(7) = lts_table[ 1 ]-start_timestamp(7)." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT lo_any_item_instance->ms_data-item_category <> if_any_interface=>co_any_item_category." 
			+ LINE_SEP + "    ASSERT sy-subrc <> 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT 1 = 2." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSERT <ls_any_field_symbol> IS ASSIGNED." 
			+ LINE_SEP + "    ASSERT <ls_other_field_symbol> IS NOT ASSIGNED." 
			+ LINE_SEP + "    ASSERT is_any_structure IS NOT INITIAL OR is_other_structure IS NOT INITIAL." 
			+ LINE_SEP + "    ASSERT <ls_row>-item_key <= ms_parameters-last_item_key." 
			+ LINE_SEP + "    ASSERT lv_quantity <= 100." 
			+ LINE_SEP + "    ASSERT abs( <ls_any_field_symbol>-sum_quantity ) > 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" chains can only be processed if they are first unchained" 
			+ LINE_SEP + "    ASSERT: sy-subrc = 0," 
			+ LINE_SEP + "            io_instance IS BOUND," 
			+ LINE_SEP + "            iv_is_valid = abap_false." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP 
			+ LINE_SEP  + "\" example implementation of the assert class, using simplified CL_ABAP_UNIT_ASSERT method signatures:"
			+ LINE_SEP + "CLASS cx_xyz_assert DEFINITION PUBLIC"
			+ LINE_SEP + "      INHERITING FROM cx_no_check FINAL"
			+ LINE_SEP + "      CREATE PUBLIC."
			+ LINE_SEP + ""
			+ LINE_SEP + "  PUBLIC SECTION."
			+ LINE_SEP + "    METHODS constructor"
			+ LINE_SEP + "      IMPORTING previous LIKE previous OPTIONAL."
			+ LINE_SEP + ""
			+ LINE_SEP + "    CLASS-METHODS assert_equals"
			+ LINE_SEP + "      IMPORTING VALUE(act) TYPE any"
			+ LINE_SEP + "                VALUE(exp) TYPE any."
			+ LINE_SEP + ""
			+ LINE_SEP + "    CLASS-METHODS assert_bound"
			+ LINE_SEP + "      IMPORTING VALUE(act) TYPE any."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" ..."
			+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP + ""
			+ LINE_SEP + "CLASS cx_xyz_assert IMPLEMENTATION."
			+ LINE_SEP + "  METHOD constructor."
			+ LINE_SEP + "    super->constructor( previous = previous )."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + ""
			+ LINE_SEP + "  METHOD assert_equals."
			+ LINE_SEP + "    IF act <> exp."
			+ LINE_SEP + "      RAISE EXCEPTION NEW cx_xyz_assert( )."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + ""
			+ LINE_SEP + "  METHOD assert_bound."
			+ LINE_SEP + "    IF act IS NOT BOUND."
			+ LINE_SEP + "      RAISE EXCEPTION NEW cx_xyz_assert( )."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + ""
			+ LINE_SEP + "  \" ..."
			+ LINE_SEP + "ENDCLASS.";
	}

	final ConfigTextValue configAssertClassName = new ConfigTextValue(this, "AssertClassName", "Assert class name:", "cx_assert", ConfigTextType.ABAP_CLASS);
	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain ASSERT: chains (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));

	private final ConfigValue[] configValues = new ConfigValue[] { configAssertClassName, configProcessChains };
 
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AssertClassRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	private String getAssertClassCall(String methodName) {
		return configAssertClassName.getValue() + "=>" + methodName;
	}
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.firstCodeTokenIsKeyword("ASSERT"))
			return false;
		
		ArrayList<Command> unchainedCommands = unchain(code, command, configProcessChains.getValue());
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return false;
		
		for (Command unchainedCommand : unchainedCommands) {
			if (executeOnUnchained(code, unchainedCommand)) {
				code.addRuleUse(this, unchainedCommand);
			}
		}
		return false; // addRuleUse() was already called above
	}
	
	
	private boolean executeOnUnchained(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// exclude cases with optional additions ID or FIELDS:
		// ASSERT [ [ID group [SUBKEY sub]] [FIELDS val1 val2 ...] CONDITION ] log_exp. 
		Token assertToken = command.getFirstCodeToken();
		if (assertToken == null || !assertToken.isKeyword("ASSERT"))
			return false;
		Token next = assertToken.getNextCodeSibling(); 
		if (next.isAnyKeyword("ID", "FIELDS"))
			return false;

		// in case of "ASSERT CONDITION ..." without other additions, simply remove the (optional) 'CONDITION' keyword
		// (at this point, we are already sure that the ASSERT can be replaced, otherwise an exception will be raised)
		if (next.isKeyword("CONDITION"))
			next.removeFromCommand();
		
		Token period = assertToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ".");

		if (assertToken.getNext().matchesOnSiblings(false, TokenSearch.ANY_TERM, "IS", "BOUND|NOT BOUND|INITIAL|NOT INITIAL", ".")
		 || assertToken.getNext().matchesOnSiblings(false, "NOT", TokenSearch.ANY_TERM, "IS", "BOUND|NOT BOUND|INITIAL|NOT INITIAL", ".")) {
			useAssertBoundOrInitial(assertToken, period);
		} else if (assertToken.getNext().matchesOnSiblings(false, "sy-subrc", "=", TokenSearch.ANY_TERM, ".")) {
			useAssertSubrc(assertToken, period);
		} else if (assertToken.getNext().matchesOnSiblings(false, TokenSearch.ANY_TERM, "=|<>", TokenSearch.ANY_TERM, ".")) {
			useAssertEqualsOrDiffers(command, assertToken, period);
		} else {
			useXsdBool(assertToken, period);
		}

		command.invalidateMemoryAccessType();
		return true;
	}

	private void useAssertBoundOrInitial(Token assertToken, Token period) throws UnexpectedSyntaxBeforeChanges, IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Token notToken = assertToken.getNext().isKeyword("NOT") ? assertToken.getNext() : null;

		Term term;
		Term attribute;
		try {
			term = Term.createSimple((notToken != null && notToken.getNext() != null) ? notToken.getNextSibling() : assertToken.getNextSibling());
			attribute = Term.createForTokenRange(term.getNextSibling(), period.getPrev());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		String methodName;
		if (attribute.firstToken.matchesOnSiblings(false, "IS", "BOUND"))
			methodName = (notToken == null) ? METHOD_NAME_BOUND : METHOD_NAME_NOT_BOUND;
		else if (attribute.firstToken.matchesOnSiblings(false, "IS", "NOT", "BOUND"))
			methodName = METHOD_NAME_NOT_BOUND;
		else if (attribute.firstToken.matchesOnSiblings(false, "IS", "INITIAL"))
			methodName = (notToken == null) ? METHOD_NAME_INITIAL : METHOD_NAME_NOT_INITIAL;
		else if (attribute.firstToken.matchesOnSiblings(false, "IS", "NOT", "INITIAL"))
			methodName = METHOD_NAME_NOT_INITIAL;
		else
			throw new UnexpectedSyntaxBeforeChanges(this, attribute.firstToken, "Expected IS [NOT] BOUND/INITIAL, but found " + attribute.toErrorLogString());

		Token methodCall = Token.createForAbap(0, assertToken.spacesLeft, getAssertClassCall(methodName), TokenType.IDENTIFIER, assertToken.sourceLineNum);
		term.firstToken.insertLeftSibling(methodCall, true);
		methodCall.appendParenthesesUpTo(term.getNext(), false);

		assertToken.removeFromCommand(true);
		if (notToken != null)
			notToken.removeFromCommand(true);
		attribute.removeFromCommand(false);

		methodCall.lineBreaks = assertToken.lineBreaks;
	}

	private void useAssertSubrc(Token assertToken, Token period) throws UnexpectedSyntaxBeforeChanges, IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Token subrcToken = assertToken.getNext();
		Token comparisonOp = subrcToken.getNext();
		Term term;
		try {
			term = Term.createSimple(comparisonOp.getNext());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		Token methodCall = Token.createForAbap(0, assertToken.spacesLeft, getAssertClassCall(METHOD_NAME_SUBRC), TokenType.IDENTIFIER, assertToken.sourceLineNum);
		term.firstToken.insertLeftSibling(methodCall, true);
		methodCall.appendParenthesesUpTo(period, true);

		if (term.isSingleToken() && term.firstToken.textEquals("0"))
			term.removeFromCommand(true);

		assertToken.removeFromCommand(true);
		subrcToken.removeFromCommand(true);
		comparisonOp.removeFromCommand(true);

		methodCall.lineBreaks = assertToken.lineBreaks;
	}

	private void useAssertEqualsOrDiffers(Command command, Token assertToken, Token period)
			throws UnexpectedSyntaxBeforeChanges, IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Term term1;
		Token comparisonOp;
		Term term2;
		try {
			term1 = Term.createSimple(assertToken.getNext());
			comparisonOp = term1.getNext();
			term2 = Term.createSimple(comparisonOp.getNext());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		// for the special case "ASSERT 1 = 2." etc., use assert_fail
		boolean isSureToFail = term1.isSingleIntegerLiteral() && term2.isSingleIntegerLiteral() && !term1.firstToken.getText().equals(term2.firstToken.getText());
		boolean term2IsAbapBool = term2.isSingleIdentifier() && term2.firstToken.textEqualsAny(ABAP.ABAP_TRUE, ABAP.ABAP_FALSE);

		String methodName;
		if (isSureToFail)
			methodName = METHOD_NAME_FAIL;
		else if (term2IsAbapBool)
			methodName = term2.firstToken.textEquals(ABAP.ABAP_TRUE) ? METHOD_NAME_TRUE : METHOD_NAME_FALSE;
		else
			methodName = (comparisonOp.textEquals("=") ? METHOD_NAME_EQUALS : METHOD_NAME_DIFFERS);

		Token methodCall = Token.createForAbap(0, assertToken.spacesLeft, getAssertClassCall(methodName), TokenType.IDENTIFIER, assertToken.sourceLineNum);
		term1.firstToken.insertLeftSibling(methodCall, true);
		methodCall.appendParenthesesUpTo(period, true);

		if (isSureToFail) {
			term1.removeFromCommand(true);
			term2.removeFromCommand(true);

		} else if (term2IsAbapBool) {
			term2.removeFromCommand(true);

		} else {
			int oldIndentTerm1 = term1.firstToken.getStartIndexInLine();
			term1.firstToken.insertLeftSibling(Token.createForAbap(0, 1, "act", TokenType.IDENTIFIER, term1.firstToken.sourceLineNum), true);
			term1.firstToken.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, term1.firstToken.sourceLineNum), true);
			term1.firstToken.setWhitespace();
			if (!term1.isSingleToken())
				command.addIndent(term1.firstToken.getStartIndexInLine() - oldIndentTerm1, oldIndentTerm1, term1.firstToken.getNext(), term1.lastToken.getNext());

			int oldIndentTerm2 = term2.firstToken.getStartIndexInLine();
			term2.firstToken.insertLeftSibling(Token.createForAbap(1, methodCall.getNext().getStartIndexInLine(), "exp", TokenType.IDENTIFIER, term2.firstToken.sourceLineNum), true);
			term2.firstToken.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, term2.firstToken.sourceLineNum), true);
			term2.firstToken.setWhitespace();
			if (!term2.isSingleToken())
				command.addIndent(term2.firstToken.getStartIndexInLine() - oldIndentTerm2, oldIndentTerm2, term2.firstToken.getNext(), term2.lastToken.getNext());
		}

		assertToken.removeFromCommand(true);
		comparisonOp.removeFromCommand(false);

		methodCall.lineBreaks = assertToken.lineBreaks;
	}

	private void useXsdBool(Token assertToken, Token period) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Token xsdboolCall = Token.createForAbap(0, 1, "xsdbool", TokenType.IDENTIFIER, assertToken.sourceLineNum);
		assertToken.insertRightSibling(xsdboolCall, true);
		xsdboolCall.appendParenthesesUpTo(period, true);

		Token methodCall = Token.createForAbap(0, assertToken.spacesLeft, getAssertClassCall(METHOD_NAME_TRUE), TokenType.IDENTIFIER, assertToken.sourceLineNum);
		xsdboolCall.insertLeftSibling(methodCall, true);
		methodCall.appendParenthesesUpTo(period, true);

		assertToken.removeFromCommand();

		methodCall.lineBreaks = assertToken.lineBreaks;
	}
}
