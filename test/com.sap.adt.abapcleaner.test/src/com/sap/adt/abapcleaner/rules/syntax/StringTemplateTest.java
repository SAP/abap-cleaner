package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class StringTemplateTest extends RuleTestBase {
	private StringTemplateRule rule;
	
	StringTemplateTest() {
		super(RuleID.STRING_TEMPLATE);
		rule = (StringTemplateRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.SHORTER_OR_EQUAL);
		rule.configAlwaysConvertLiterals.setValue(true);
		rule.configRequireOperandsOnSameLine.setValue(false);
		rule.configIgnoreMultiLineOperands.setValue(true);
		rule.configKeepControlCharsSeparate.setValue(true);
	}

	@Test
	void testMultipleLiteralsPerLine() {
		buildSrc("    lv_from = left && ` join ` && right && ` on `.");
		buildSrc("    ls_sel_params-low = '*' && ls_sel_params-low && '*'.");
		buildSrc("    out->write( `Name:` && ` ` && iv_first_name && ` ` && iv_last_name ).");
		buildSrc("    cl_abap_unit_assert=>assert_fail( msg = 'Expected:' && ` ` && iv_number && ',' && ` `");
		buildSrc("                                         && 'Actual:' && ` ` && lv_act ).");

		buildExp("    lv_from = |{ left } join { right } on |.");
		buildExp("    ls_sel_params-low = |*{ ls_sel_params-low }*|.");
		buildExp("    out->write( |Name: { iv_first_name } { iv_last_name }| ).");
		buildExp("    cl_abap_unit_assert=>assert_fail( msg = |Expected: { iv_number }, |");
		buildExp("                                         && |Actual: { lv_act }| ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testOneLiteralPerLine() {
		buildSrc("    lv_first_day_of_march = lv_year && '0301'.");
		buildSrc("    lv_formula = lv_var_1 && ` + ` && lv_var_2.");
		buildSrc("    lv_salutation = `Hello ` && lv_name.");
		buildSrc("    ls_struc-comp = ls_struc-comp && | { TEXT-004 } { lv_any }|.");

		buildExp("    lv_first_day_of_march = |{ lv_year }0301|.");
		buildExp("    lv_formula = |{ lv_var_1 } + { lv_var_2 }|.");
		buildExp("    lv_salutation = |Hello { lv_name }|.");
		buildExp("    ls_struc-comp = |{ ls_struc-comp } { TEXT-004 } { lv_any }|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOneLiteralPerLineChangedNoEmbedding() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.SHORTER);

		buildSrc("    lv_first_day_of_march = lv_year && '0301'.");
		buildSrc("    lv_formula = lv_var_1 && ` + ` && lv_var_2.");
		buildSrc("    lv_salutation = `Hello ` && lv_name.");

		buildExp("    lv_first_day_of_march = lv_year && |0301|.");
		buildExp("    lv_formula = lv_var_1 && | + | && lv_var_2.");
		buildExp("    lv_salutation = |Hello | && lv_name.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOneOrNoLiteralPerLineKept() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.SHORTER);
		rule.configAlwaysConvertLiterals.setValue(false);

		buildSrc("    lv_first_day_of_march = lv_year && '0301'.");
		buildSrc("    lv_formula = lv_var_1 && ` + ` && lv_var_2.");
		buildSrc("    lv_salutation = `Hello ` && lv_name.");
		buildSrc("    lv_date = lv_year && lv_month && lv_day.");
		buildSrc("    lv_fiscal_year_period = lv_fiscal_year && lv_period.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoLiteralInLineUnchanged() {
		buildSrc("    lv_date = lv_year && lv_month && lv_day.");
		buildSrc("    lv_fiscal_year_period = lv_fiscal_year && lv_period.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoLiteralInLineEmbedded() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.ALWAYS);

		buildSrc("    lv_date = lv_year && lv_month && lv_day.");
		buildSrc("    lv_fiscal_year_period = lv_fiscal_year && lv_period.");
		buildSrc("    result =    d1 && d2 && d4 && d5 && d6 && d7 && d8 && d9 && d10");
		buildSrc("             && c1 && c2 && c4 && c5 && c6 && c7 && c8 && c9 && c10.");

		buildExp("    lv_date = |{ lv_year }{ lv_month }{ lv_day }|.");
		buildExp("    lv_fiscal_year_period = |{ lv_fiscal_year }{ lv_period }|.");
		buildExp("    result =    |{ d1 }{ d2 }{ d4 }{ d5 }{ d6 }{ d7 }{ d8 }{ d9 }{ d10 }|");
		buildExp("             && |{ c1 }{ c2 }{ c4 }{ c5 }{ c6 }{ c7 }{ c8 }{ c9 }{ c10 }|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTrailingSpaces() {
		buildSrc("    lv_result1 = ' a   ' && ' +   ' && ' b  ' && ' =     ' && ' 10' && '  '.");
		buildSrc("    lv_result2 = ` a   ` && ` +   ` && ` b  ` && ` =     ` && ` 10` && `  `.");

		buildExp("    lv_result1 = | a + b = 10|.");
		buildExp("    lv_result2 = | a    +    b   =      10  |.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEscapeChars() {
		buildSrc("    lv_escape1 = 'To ''be''' && ` or ``not`` to be`.");
		buildSrc("    lv_escape2 = 'String templates must escape |' && ` as well as { and }.`.");
		buildSrc("    lv_escape3 = `abc 123 ``\\|{}` && |\\r\\n\\t| && '\\' && '''' && '|' && '{' && '}'.");

		buildExp("    lv_escape1 = |To 'be' or `not` to be|.");
		buildExp("    lv_escape2 = |String templates must escape \\| as well as \\{ and \\}.|.");
		buildExp("    lv_escape3 = |abc 123 `\\\\\\|\\{\\}| && |\\r\\n\\t| && |\\\\'\\|\\{\\}|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testConstructorExprAsFirstOperand() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.ALWAYS);
		rule.configIgnoreMultiLineOperands.setValue(false);

		buildSrc("    FINAL(sentence) = REDUCE string( INIT text = `` sep = ``");
		buildSrc("                                     FOR word IN switched_words");
		buildSrc("                                     NEXT text = text && sep && word");
		buildSrc("                                          sep  = ` ` ) && '.'.");

		buildExp("    FINAL(sentence) = |{ REDUCE string( INIT text = `` sep = ``");
		buildExp("                                        FOR word IN switched_words");
		buildExp("                                        NEXT text = |{ text }{ sep }{ word }|");
		buildExp("                                             sep  = ` ` ) }.|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmbedMultiLineOperands() {
		rule.configIgnoreMultiLineOperands.setValue(false);

		buildSrc("    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1");
		buildSrc("                                         THEN 'element'");
		buildSrc("                                         ELSE 'elements').");

		buildExp("    lv_text = |{ lv_count } { COND #( WHEN lv_count = 1");
		buildExp("                                      THEN 'element'");
		buildExp("                                      ELSE 'elements') }|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIgnoreMultiLineOperandsAtStartOrEnd() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.ALWAYS);

		buildSrc("    FINAL(sentence) = REDUCE string( INIT text = `` sep = ``");
		buildSrc("                                     FOR word IN switched_words");
		buildSrc("                                     NEXT text = text && sep && word");
		buildSrc("                                          sep  = ` ` ) && '.'.");
		buildSrc("    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1");
		buildSrc("                                         THEN 'element'");
		buildSrc("                                         ELSE 'elements').");

		buildExp("    FINAL(sentence) = REDUCE string( INIT text = `` sep = ``");
		buildExp("                                     FOR word IN switched_words");
		buildExp("                                     NEXT text = |{ text }{ sep }{ word }|");
		buildExp("                                          sep  = ` ` ) && |.|.");
		buildExp("    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1");
		buildExp("                                         THEN 'element'");
		buildExp("                                         ELSE 'elements').");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMergeStringTemplatesInclControlChars() {
		rule.configKeepControlCharsSeparate.setValue(false);

		buildSrc("    lv_result = |{ iv_any }:| && | TEXT { iv_other }| && | TEXT2|");
		buildSrc("             && | | && |  | && || && |     |");
		buildSrc("             && |\\r\\n| && |TEXT3| && |\\t|.");

		buildExp("    lv_result = |{ iv_any }: TEXT { iv_other } TEXT2|");
		buildExp("             && |        |");
		buildExp("             && |\\r\\nTEXT3\\t|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMergeStringTemplatesExclControlChars() {
		buildSrc("    lv_result = |{ iv_any }:| && | TEXT { iv_other }| && | TEXT2|");
		buildSrc("             && | | && |  | && || && |     |");
		buildSrc("             && |\\r\\n| && |TEXT3| && |\\t|.");

		buildExp("    lv_result = |{ iv_any }: TEXT { iv_other } TEXT2|");
		buildExp("             && |        |");
		buildExp("             && |\\r\\n| && |TEXT3| && |\\t|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSkipAbapSql() {
		buildSrc("      SELECT char1 && '_' && char2 AS group");
		buildSrc("        FROM demo_expressions");
		buildSrc("        ORDER BY group");
		buildSrc("        INTO TABLE @FINAL(lv_result).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMethodCallParameters() {
		buildSrc("    cl_any_class=>any_method( iv_any_param = |T1| && <ls_any_struc>->other_method( )-lv_any_value && |T2| ).");
		buildSrc("");
		buildSrc("    lo_any_instance->any_method( )->other_method( CONV #( lv_any_value+0(4) && '0' && lv_any_value+4(2) ) ).");
		buildSrc("");
		buildSrc("    CALL FUNCTION 'ANY_FUNCTION'");
		buildSrc("      EXPORTING lv_any_value   = CONV ty_any_type( <ls_any_struc>-comp(4) && <ls_other_struc>-comp+4(2) && '2' )");
		buildSrc("      IMPORTING lv_third_value = lv_fourth_value.");

		buildExp("    cl_any_class=>any_method( iv_any_param = |T1{ <ls_any_struc>->other_method( )-lv_any_value }T2| ).");
		buildExp("");
		buildExp("    lo_any_instance->any_method( )->other_method( CONV #( |{ lv_any_value+0(4) }0{ lv_any_value+4(2) }| ) ).");
		buildExp("");
		buildExp("    CALL FUNCTION 'ANY_FUNCTION'");
		buildExp("      EXPORTING lv_any_value   = CONV ty_any_type( |{ <ls_any_struc>-comp(4) }{ <ls_other_struc>-comp+4(2) }2| )");
		buildExp("      IMPORTING lv_third_value = lv_fourth_value.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableExpression() {
		buildSrc("    lts_any_table[ any_comp = lc_any_constant+0(6) && '2' ]-other_comp = 2.");

		buildExp("    lts_any_table[ any_comp = |{ lc_any_constant+0(6) }2| ]-other_comp = 2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueStatement() {
		buildSrc("    DATA(lt_any_table) = VALUE ty_any_type( ( any_comp   = TEXT-001 && | | && iv_any_param && |-| && lv_any_value");
		buildSrc("                                              other_comp = '2' && iv_any_param ) ).");

		buildExp("    DATA(lt_any_table) = VALUE ty_any_type( ( any_comp   = |{ TEXT-001 } { iv_any_param }-{ lv_any_value }|");
		buildExp("                                              other_comp = |2{ iv_any_param }| ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultipleStringTemplates() {
		buildSrc("    lv_any_value = |T1{ iv_any_param }T2| && |T3| && |T4|.");
		buildSrc("    lv_any_value = |T1| && |T2{ lv_other_value }T2|");
		buildSrc("                          && |T2{ iv_any_param }T2| && |T5|");
		buildSrc("                          && |T2{ if_any_interface=>gc_any_constant }T2| && |T6| &&");
		buildSrc("                   |T7| && |T2{ lv_thard_value }T2| ##NO_TEXT.");

		buildExp("    lv_any_value = |T1{ iv_any_param }T2T3T4|.");
		buildExp("    lv_any_value = |T1T2{ lv_other_value }T2|");
		buildExp("                       && |T2{ iv_any_param }T2T5|");
		buildExp("                       && |T2{ if_any_interface=>gc_any_constant }T2T6| &&");
		buildExp("                   |T7T2{ lv_thard_value }T2| ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmbeddedFunctionOneLiner() {
		buildSrc("    v1 = `T1` && meth1( v2 = meth2( v3 = v4 v5 = v6 ) v7 = v7 )");
		buildSrc("      && `T2` && meth1( v2 = meth2( v3 = v4 v5 = v6 ) v7 = v7 ).");
		buildSrc("    meth1( v1 = 'T1' && ` ` && 'T2' && meth2( v2 ) && 'T2' && ` ` && 'T3' ).");

		buildExp("    v1 = |T1{ meth1( v2 = meth2( v3 = v4 v5 = v6 ) v7 = v7 ) }|");
		buildExp("      && |T2{ meth1( v2 = meth2( v3 = v4 v5 = v6 ) v7 = v7 ) }|.");
		buildExp("    meth1( v1 = |T1 T2{ meth2( v2 ) }T2 T3| ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmbeddedExprWithFormatOptions() {
		buildSrc("            v1 = v1 && | { TEXT-007 } { v2 DATE = ENVIRONMENT }T1|.");
		buildSrc("            v1 = v1 && `T1` && v2 && |T2{ v3 CURRENCY = v4 NUMBER = ENVIRONMENT } { v4 }T3|.");
		buildSrc("            v1 = v1 && `T1` && v2 && |T2{ v3 CURRENCY = v4 NUMBER = ENVIRONMENT } { v4 }T3|.");

		buildExp("            v1 = |{ v1 } { TEXT-007 } { v2 DATE = ENVIRONMENT }T1|.");
		buildExp("            v1 = |{ v1 }T1{ v2 }T2{ v3 CURRENCY = v4 NUMBER = ENVIRONMENT } { v4 }T3|.");
		buildExp("            v1 = |{ v1 }T1{ v2 }T2{ v3 CURRENCY = v4 NUMBER = ENVIRONMENT } { v4 }T3|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOperandOnOwnLine() {
		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.ALWAYS);

		buildSrc("    lv_result = shift_left( val = |{ lv_value NUMBER = RAW }| &&");
		buildSrc("                                  repeat( val = `0` occ = lv_digits * 2 )  ).");

		buildExp("    lv_result = shift_left( val = |{ lv_value NUMBER = RAW }| &&");
		buildExp("                                  |{ repeat( val = `0` occ = lv_digits * 2 ) }|  ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSkipConcatenationInsideStringTemplate() {
		// ensure no string templates are created inside string templates, e.g. 
		//   any_method( |{ condense( lv_any ) && lv_other ALIGN = RIGHT }| ).
		// is better kept than changed to:  
		//   any_method( |{ |{ condense( lv_any) }{ lv_other }| ALIGN = RIGHT }| ).

		rule.configStringTemplateCondition.setEnumValue(StringTemplateCondition.ALWAYS);

		buildSrc("    lv_result = condense( lv_any ) && lv_other.");
		buildSrc("    any_method( |{ condense( lv_any ) && lv_other ALIGN = RIGHT }| ).");

		buildExp("    lv_result = |{ condense( lv_any ) }{ lv_other }|.");
		buildExp("    any_method( |{ condense( lv_any ) && lv_other ALIGN = RIGHT }| ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testConcatOpsAtLineEnd() {
		buildSrc("    rv_text = iv_date+0(4) && '-' &&");
		buildSrc("       iv_date+4(2) && '-' &&");
		buildSrc("       iv_date+6(2) && '-' &&");
		buildSrc("       iv_add.");

		buildExp("    rv_text = |{ iv_date+0(4) }-| &&");
		buildExp("       |{ iv_date+4(2) }-| &&");
		buildExp("       |{ iv_date+6(2) }-| &&");
		buildExp("       iv_add.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRequireOperandsOnSameLine() {
		rule.configRequireOperandsOnSameLine.setValue(true);

		buildSrc("    rv_example  = `3 + 5 = ` && `8` && `. `");
		buildSrc("               && `a + b = ` && c && `.`.");

		buildExp("    rv_example  = `3 + 5 = ` && `8` && `. `");
		buildExp("               && |a + b = { c }.|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotRequireOperandsOnSameLine() {
		buildSrc("    rv_example  = `3 + 5 = ` && `8` && `. `");
		buildSrc("               && `a + b = ` && c && `.`.");

		buildExp("    rv_example  = |3 + 5 = 8. |");
		buildExp("               && |a + b = { c }.|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
