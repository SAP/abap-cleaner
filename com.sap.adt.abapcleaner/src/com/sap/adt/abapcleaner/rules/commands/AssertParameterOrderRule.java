package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigTextType;
import com.sap.adt.abapcleaner.rulebase.ConfigTextValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rules.alignment.AlignParametersRule;
import com.sap.adt.abapcleaner.rules.alignment.AlignParametersRule.ContentType;

public class AssertParameterOrderRule extends RuleForCommands {
	private final static String abapUnitAssertClass = "cl_abap_unit_assert";
	
	private enum MethodGroup {
		COMPARISON,
		TEXT_NUM_TABLE, 
		RETURN_CODE;
	}
	
	private static class AssertMethod {
		public final MethodGroup methodGroup;
		public final String methodName;
		public final String methodCallIdentifierText;
		public final String[] expParamNames; 
		public final String[] actParamNames;
		
		public AssertMethod(MethodGroup methodGroup, String methodName, String expParamName, String actParamName) {
			this.methodGroup = methodGroup;
			this.methodName = methodName;
			this.methodCallIdentifierText = abapUnitAssertClass + "=>" + methodName + "(";
			expParamNames = new String[] { expParamName };
			actParamNames = new String[] { actParamName };
		}
		
		public AssertMethod(MethodGroup methodGroup, String methodName, String[] expParamNames, String[] actParamNames) {
			this.methodGroup = methodGroup;
			this.methodName = methodName; 
			this.methodCallIdentifierText = abapUnitAssertClass + "=>" + methodName + "(";
			this.expParamNames = expParamNames;
			this.actParamNames = actParamNames;
		}
	}

	private static ArrayList<AssertMethod> assertMethods;

	private static ArrayList<AssertMethod> initAssertMethods() {
		ArrayList<AssertMethod> assertMethods = new ArrayList<>();

		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_equals", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_equals_float", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_differs", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_char_cp", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_char_np", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assert_that", new String[] { "exp" }, new String[] { "act", "act_as_text" }));
		assertMethods.add(new AssertMethod(MethodGroup.COMPARISON, "assume_that", new String[] { "exp" }, new String[] { "act", "act_as_text" }));

		assertMethods.add(new AssertMethod(MethodGroup.TEXT_NUM_TABLE, "assert_number_between", new String[] { "lower", "upper" }, new String[] { "number" }));
		assertMethods.add(new AssertMethod(MethodGroup.TEXT_NUM_TABLE, "assert_text_matches", "pattern", "text"));
		assertMethods.add(new AssertMethod(MethodGroup.TEXT_NUM_TABLE, "assert_text_not_matches", "pattern", "text"));
		assertMethods.add(new AssertMethod(MethodGroup.TEXT_NUM_TABLE, "assert_table_contains", "line", "table"));
		assertMethods.add(new AssertMethod(MethodGroup.TEXT_NUM_TABLE, "assert_table_not_contains", "line", "table"));

		assertMethods.add(new AssertMethod(MethodGroup.RETURN_CODE, "assert_subrc", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.RETURN_CODE, "assert_return_code", "exp", "act"));
		assertMethods.add(new AssertMethod(MethodGroup.RETURN_CODE, "assume_return_code", "exp", "act"));

		return assertMethods;
	}
	// -------------------------------------------------------------------------
	
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };
	
	@Override
	public RuleID getID() { return RuleID.ASSERT_PARAMETER_ORDER; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Standardize assertion parameter order"; }

	@Override
	public String getDescription() { return "Rearranges parameters in CL_ABAP_UNIT_ASSERT calls to follow a standardized order for expected and actual values."; }

	@Override
	public String getHintsAndRestrictions() { return "If an assert class for product code exists with signatures similar to CL_ABAP_UNIT_ASSERT (see rule '" + AssertClassRule.displayName + "'), calls to this class can be processed, too."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 11, 5); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS } ; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD test_anything." 
			+ LINE_SEP + "    \" given"
			+ LINE_SEP + "    prepare_test_case( )."
			+ LINE_SEP
			+ LINE_SEP + "    \" when"
			+ LINE_SEP + "    call_method_under_test( )."
			+ LINE_SEP
			+ LINE_SEP + "    \" then"
			+ LINE_SEP + "    \" 1. ASSERT_EQUALS[_FLOAT], ASSERT_DIFFERS, ASSERT_CHAR_CP, ASSERT_CHAR_NP, ASSERT_THAT, ASSUME_THAT:"
			+ LINE_SEP + "    \" the signatures of these methods start with parameter ACT, but you may want calls to start with EXP:"
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( act = ms_data-item_type"
			+ LINE_SEP + "                                        exp = if_any_interface=>co_any_item_type )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( msg = 'unexpected value for component1!'" 
			+ LINE_SEP + "                                        exp = 'new value' \" comment on exp"
			+ LINE_SEP + "                                        act = lts_act_table[ 1 ]-component1 )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_differs( act  = lo_atc_item_instance->ms_data-item_category"
			+ LINE_SEP + "                                         exp  = if_any_interface=>co_any_item_category"
			+ LINE_SEP + "                                         msg  = 'unexpected item category'"
			+ LINE_SEP + "                                         quit = if_aunit_constants=>quit-no )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_differs( exp = 3 act = lines( lts_act_table ) )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_char_cp( act = lt_result[ 1 ] exp = |*;'-z2;*| )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" 2. ASSERT_NUMBER_BETWEEN, ASSERT_TEXT[_NOT]_MATCHES, ASSERT_TABLE[_NOT]_CONTAINS:"
			+ LINE_SEP + "    \" the signatures of these methods start with the parameters for the expectation (UPPER, LOWER;"
			+ LINE_SEP + "    \" PATTERN; LINE), followed by the actual value (NUMBER; TEXT; TABLE)"
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl."
			+ LINE_SEP + "                                                number = lv_result_value"
			+ LINE_SEP + "                                                upper  = 10"
			+ LINE_SEP + "                                                lower  = 5 )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text"
			+ LINE_SEP + "                                              pattern = lv_exp_message_pattern )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line"
			+ LINE_SEP + "                                                    table = lt_act_table"
			+ LINE_SEP + "                                                    msg   = 'deletion of line failed' )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" 3. ASSERT_SUBRC, ASSERT_RETURN_CODE, ASSUME_RETURN_CODE: parameter ACT has SY-SUBRC as its default value"
			+ LINE_SEP + "    \" these methods use SY-SUBRC as the default value for parameter SY-SUBRC; the signature starts with EXP"
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_subrc( quit = if_aunit_constants=>quit-no"
			+ LINE_SEP + "                                       exp  = 4 )."
			+ LINE_SEP + ""
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_return_code( exp = 4"
			+ LINE_SEP + "                                             act = lv_return_code )."
			+ LINE_SEP + "  ENDMETHOD.";
	}

	final ConfigInfoValue configInfoOrder = new ConfigInfoValue(this, "Ensure order of parameters for expectation and actual value:", ConfigInfoStyle.NORMAL);
	final ConfigEnumValue<AssertParameterOrder> configComparisonParamOrder = new ConfigEnumValue<AssertParameterOrder>(this, "ComparisonParamOrder", "Methods comparing values (ASSERT_EQUALS..., _DIFFERS, _THAT, _CHAR...)",
			new String[] { "expectation first (EXP)", "actual value first (ACT, ACT_AS_TEXT)", "keep as is" }, AssertParameterOrder.values(), AssertParameterOrder.EXP_FIRST);
	final ConfigEnumValue<AssertParameterOrder> configNumTextTableParamOrder = new ConfigEnumValue<AssertParameterOrder>(this, "NumTextTableParamOrder", "Methods checking numbers, text and tables (_NUMBER..., _TEXT_..., _TABLE_...)",
			new String[] { "expectation first (LOWER..UPPER, PATTERN, LINE)", "actual value first (NUMBER, TEXT, TABLE)", "keep as is" }, AssertParameterOrder.values(), AssertParameterOrder.EXP_FIRST);
	final ConfigEnumValue<AssertParameterOrder> configReturnCodeParamOrder = new ConfigEnumValue<AssertParameterOrder>(this, "ReturnCodeParamOrder", "Methods checking return code (ASSERT_SUBRC, _RETURN_CODE):",
			new String[] { "expectation first (EXP)", "actual value first (ACT)", "keep as is" }, AssertParameterOrder.values(), AssertParameterOrder.EXP_FIRST);
	final ConfigTextValue configAssertClassName = new ConfigTextValue(this, "AssertClassName", "Product code assert class name:", "", ConfigTextType.ABAP_CLASS, 30, "");

	private final ConfigValue[] configValues = new ConfigValue[] { configInfoOrder, configComparisonParamOrder, configNumTextTableParamOrder, configReturnCodeParamOrder, configAssertClassName };
 
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AssertParameterOrderRule(Profile profile) {
		super(profile);
		if (assertMethods == null)
			assertMethods = initAssertMethods();
		initializeConfiguration();
	}

	private AssertParameterOrder getConfigOfMethodGroup(MethodGroup methodGroup) {
		switch (methodGroup) {
			case COMPARISON:
				return AssertParameterOrder.forValue(configComparisonParamOrder.getValue());
			case TEXT_NUM_TABLE:
				return AssertParameterOrder.forValue(configNumTextTableParamOrder.getValue());
			case RETURN_CODE:
				return AssertParameterOrder.forValue(configReturnCodeParamOrder.getValue());
			default:
				throw new IllegalArgumentException();
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		String ownAssertClassName = configAssertClassName.getValue();
		String ownAssertClassCall = StringUtil.isNullOrEmpty(ownAssertClassName) ? "" : ownAssertClassName + "=>";
		
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null || !firstCode.isIdentifier())
			return false;
		if (!firstCode.textStartsWith("cl_abap_unit_assert=>") && (StringUtil.isNullOrEmpty(ownAssertClassName) || !firstCode.textStartsWith(ownAssertClassCall)))
			return false;

		for (AssertMethod assertMethod : assertMethods) {
			if (firstCode.textEquals(assertMethod.methodCallIdentifierText) 
					|| (!StringUtil.isNullOrEmpty(ownAssertClassCall) && firstCode.textEquals(ownAssertClassCall + assertMethod.methodName + "("))) { 
				AssertParameterOrder order = getConfigOfMethodGroup(assertMethod.methodGroup);
				if (order == AssertParameterOrder.KEEP_AS_IS) {
					return false;
				} else {
					return executeOn(code, command, assertMethod, order);
				}
			}
		}
		return false; // pro forma
	}
	
	private boolean executeOn(Code code, Command command, AssertMethod assertMethod, AssertParameterOrder order) throws UnexpectedSyntaxBeforeChanges, IntegrityBrokenException {
		AlignTable table = new AlignTable(AlignParametersRule.MAX_COLUMN_COUNT);
		ArrayList<Token> otherLineStarts = new ArrayList<>();
		Token firstCode = command.getFirstCodeToken();
		Token end = firstCode.getEndOfParamsOrComponentsList();
		
		try {
			AlignParametersRule.buildAlignTable(table, firstCode, end, ContentType.FUNCTIONAL_CALL_PARAMS, otherLineStarts);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, e);
		}

		AlignColumn paramNameCol = table.getColumn(AlignParametersRule.Columns.PARAMETER.getValue());
		
		// determine the line indices of the parameters for expected / actual values for this call
		int[] curExpLines = new int[assertMethod.expParamNames.length];
		int[] curActLines = new int[assertMethod.actParamNames.length];
		for (int i = 0; i < curExpLines.length; ++i)
			curExpLines[i] = -1;
		for (int i = 0; i < curActLines.length; ++i)
			curActLines[i] = -1;

		for (int line = 0; line < table.getLineCount(); ++line) {
			AlignCell paramNameCell = paramNameCol.getCellFromLine(line);
			if (paramNameCell == null)  // pro forma
				continue;
			String paramName = paramNameCell.getFirstToken().getText();

			int curExpIndex = 0;
			for (String expParamName : assertMethod.expParamNames) {
				if (AbapCult.stringEquals(paramName, expParamName, true)) 
					curExpLines[curExpIndex] = line;
				++curExpIndex;
			}

			int curActIndex = 0;
			for (String actParamName : assertMethod.actParamNames) {
				if (AbapCult.stringEquals(paramName, actParamName, true)) 
					curActLines[curActIndex] = line;
				++curActIndex;
			}
		}

		// determine the current line index of the parameters; the index of this int[] matches the configured order
		int[] curLines = new int[curExpLines.length + curActLines.length];
		if (order == AssertParameterOrder.EXP_FIRST) {
			System.arraycopy(curExpLines, 0, curLines, 0, curExpLines.length);
			System.arraycopy(curActLines, 0, curLines, curExpLines.length, curActLines.length);
		} else {
			System.arraycopy(curActLines, 0, curLines, 0, curActLines.length);
			System.arraycopy(curExpLines, 0, curLines, curActLines.length, curExpLines.length);
		} 
		
		// check whether the current order matches the configured order 
		boolean needsRearrange = false;
		int index = 0;
		for (int curLine : curLines) {
			if (curLine >= 0) {
				needsRearrange |= (curLine != index);
				++index;
			}
		}

		if (!needsRearrange)
			return false;
		
		// create a list of Terms in the configured order
		ArrayList<Term> terms = new ArrayList<>();
		for (int curLine : curLines) {
			if (curLine >= 0) {
				try {
					terms.add(createTermFromLine(table.getLine(curLine)));
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxBeforeChanges(this, e);
				}
			}
		}

		// remember the position of the closing parenthesis (which might change when Terms are removed) 
		// and whether the call is a one-liner (which implies that there cannot be any comments inside the parentheses)
		Token closingParens = firstCode.getNextSibling();
		int closingParensLineBreaks = closingParens.lineBreaks;
		int closingParensSpacesLeft = closingParens.spacesLeft;
		boolean isOneLiner = !command.containsLineBreaksBetween(firstCode, closingParens, false);

		// in the configured order, move the Terms to the start of the parameter list (after optional EXPORTING)
		Token writePos = table.getLine(0).getCell(AlignParametersRule.Columns.PARAMETER.getValue()).getFirstToken(); 

		boolean changed = false;
		for (Term term : terms) {
			if (term.firstToken == writePos) {
				// move writePos behind the Term
				writePos = term.lastToken.getNext();
			} else {
				term.removeFromCommand(true);
				term.firstToken.copyWhitespaceFrom(writePos);
				if (isOneLiner && !term.lastToken.isComment()) { // && pro forma
					writePos.setWhitespace();
				} else {
					writePos.setWhitespace(1, writePos.getStartIndexInLine());
				}
				writePos.insertLeftSibling(term);
				changed = true;
			}
			if (writePos == null)  // pro forma
				break;
			writePos = writePos.getNextWhileComment();
		}
		
		// move closing parenthesis to line end if it was beforehand
		if (!closingParens.getPrev().isComment()) {
			if (closingParensLineBreaks == 0)
				closingParensSpacesLeft = Math.max(closingParensSpacesLeft, 1);
			if (closingParens.setWhitespace(closingParensLineBreaks, closingParensSpacesLeft)) {
				changed = true;
			}
		}
		return changed;
	}

	private Term createTermFromLine(AlignLine line) throws UnexpectedSyntaxException {
		// the first Token in the line may be the optional EXPORTING keyword, therefore start from the parameter
		Token firstToken = line.getCell(AlignParametersRule.Columns.PARAMETER.getValue()).getFirstToken();
		Token lastToken = line.getLastToken();
		
		// expand the Token range to include pragmas before and comments at the end of the line
		while (firstToken.lineBreaks == 0 && firstToken.getPrev() != null && firstToken.getPrev().isPragma()) 
			firstToken = firstToken.getPrev();

		if (lastToken.getNext() != null && lastToken.getNext().isCommentAfterCode()) 
			lastToken = lastToken.getNext();

		return Term.createForTokenRange(firstToken, lastToken);
	}
}
