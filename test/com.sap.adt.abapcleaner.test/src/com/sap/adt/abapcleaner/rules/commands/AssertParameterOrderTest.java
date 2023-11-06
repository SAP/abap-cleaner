package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class AssertParameterOrderTest extends RuleTestBase {
	private AssertParameterOrderRule rule;
	
	AssertParameterOrderTest() {
		super(RuleID.ASSERT_PARAMETER_ORDER);
		rule = (AssertParameterOrderRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configComparisonParamOrder.setEnumValue(AssertParameterOrder.EXP_FIRST);
		rule.configNumTextTableParamOrder.setEnumValue(AssertParameterOrder.EXP_FIRST);
		rule.configReturnCodeParamOrder.setEnumValue(AssertParameterOrder.EXP_FIRST);
		rule.configAssertClassName.setValue("cx_assert");
	}
	
	@Test
	void testChangeComparisonToExpFirst() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = ms_data-item_type");
		buildSrc("                                        exp = if_any_interface=>co_any_item_type ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_equals( msg = 'unexpected value for component1!'");
		buildSrc("                                        exp = 'new value' \" comment on exp");
		buildSrc("                                        act = lts_act_table[ 1 ]-component1 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_differs( act  = lo_atc_item_instance->ms_data-item_category");
		buildSrc("                                         exp  = if_any_interface=>co_any_item_category");
		buildSrc("                                         msg  = 'unexpected item category'");
		buildSrc("                                         quit = if_aunit_constants=>quit-no ).");

		buildExp("    cl_abap_unit_assert=>assert_equals( exp = if_any_interface=>co_any_item_type");
		buildExp("                                        act = ms_data-item_type ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_equals( exp = 'new value' \" comment on exp");
		buildExp("                                        act = lts_act_table[ 1 ]-component1");
		buildExp("                                        msg = 'unexpected value for component1!' ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_differs( exp  = if_any_interface=>co_any_item_category");
		buildExp("                                         act  = lo_atc_item_instance->ms_data-item_category");
		buildExp("                                         msg  = 'unexpected item category'");
		buildExp("                                         quit = if_aunit_constants=>quit-no ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentMovedBeforeClosingParenthesis() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = ms_data-item_type \" comment");
		buildSrc("                                        exp = if_any_interface=>co_any_item_type ).");

		buildExp("    cl_abap_unit_assert=>assert_equals( exp = if_any_interface=>co_any_item_type");
		buildExp("                                        act = ms_data-item_type \" comment");
		buildExp("                                        ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeComparisonToActFirst() {
		rule.configComparisonParamOrder.setEnumValue(AssertParameterOrder.ACT_FIRST);

		buildSrc("    cl_abap_unit_assert=>assert_equals( msg = 'unexpected value for component1!'");
		buildSrc("                                        ##ANY_PRAGMA exp = 'new value' \" comment on exp");
		buildSrc("                                        act = lts_act_table[ 1 ]-component1 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_differs( exp = 3 act = lines( lts_act_table ) ).");

		buildExp("    cl_abap_unit_assert=>assert_equals( act = lts_act_table[ 1 ]-component1");
		buildExp("                                        ##ANY_PRAGMA exp = 'new value' \" comment on exp");
		buildExp("                                        msg = 'unexpected value for component1!' ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_differs( act = lines( lts_act_table ) exp = 3 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testKeepComparison() {
		rule.configComparisonParamOrder.setEnumValue(AssertParameterOrder.KEEP_AS_IS);

		buildSrc("    cl_abap_unit_assert=>assert_equals( act = ms_data-item_type");
		buildSrc("                                        exp = if_any_interface=>co_any_item_type ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_equals( msg = 'unexpected value for component1!'");
		buildSrc("                                        exp = 'new value' \" comment on exp");
		buildSrc("                                        act = lts_act_table[ 1 ]-component1 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_differs( exp = 3 act = lines( lts_act_table ) ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_char_cp( act = lt_result[ 1 ] exp = |*;'-z2;*| ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeOwnClassComparisonToExpFirst() {
		rule.configAssertClassName.setValue("cx_assert");

		buildSrc("    cx_assert=>assert_equals( act = ms_data-item_type");
		buildSrc("                              exp = if_any_interface=>co_any_item_type ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_equals( msg = 'unexpected value for component1!'");
		buildSrc("                              exp = 'new value' \" comment on exp");
		buildSrc("                              act = lts_act_table[ 1 ]-component1 ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_differs( act  = lo_atc_item_instance->ms_data-item_category");
		buildSrc("                               exp  = if_any_interface=>co_any_item_category");
		buildSrc("                               msg  = 'unexpected item category'");
		buildSrc("                               quit = if_aunit_constants=>quit-no ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_char_cp( act = lt_result[ 1 ] exp = |*;'-z2;*| ).");

		buildExp("    cx_assert=>assert_equals( exp = if_any_interface=>co_any_item_type");
		buildExp("                              act = ms_data-item_type ).");
		buildExp("");
		buildExp("    cx_assert=>assert_equals( exp = 'new value' \" comment on exp");
		buildExp("                              act = lts_act_table[ 1 ]-component1");
		buildExp("                              msg = 'unexpected value for component1!' ).");
		buildExp("");
		buildExp("    cx_assert=>assert_differs( exp  = if_any_interface=>co_any_item_category");
		buildExp("                               act  = lo_atc_item_instance->ms_data-item_category");
		buildExp("                               msg  = 'unexpected item category'");
		buildExp("                               quit = if_aunit_constants=>quit-no ).");
		buildExp("");
		buildExp("    cx_assert=>assert_char_cp( exp = |*;'-z2;*| act = lt_result[ 1 ] ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownCallUnchanged() {
		rule.configAssertClassName.setValue("cx_assert");

		buildSrc("    cl_abap_unit_assert=>assert_unknown( act = ms_data-item_type");
		buildSrc("                                         exp = if_any_interface=>co_any_item_type ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_unknown( act = ms_data-item_type");
		buildSrc("                               exp = if_any_interface=>co_any_item_type ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_unknown_too( msg = 'unexpected value for component1!'");
		buildSrc("                                   exp = 'new value' \" comment on exp");
		buildSrc("                                   act = lts_act_table[ 1 ]-component1 ).");
		buildSrc("");
		buildSrc("    cx_assert=>assume_something( act  = lo_atc_item_instance->ms_data-item_category");
		buildSrc("                                 exp  = if_any_interface=>co_any_item_category");
		buildSrc("                                 msg  = 'unexpected item category'");
		buildSrc("                                 quit = if_aunit_constants=>quit-no ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeNumTextTableToExpFirst() {
		buildSrc("    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl.");
		buildSrc("                                                number = lv_result_value");
		buildSrc("                                                upper  = 10");
		buildSrc("                                                lower  = 5 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text");
		buildSrc("                                              pattern = lv_exp_message_pattern ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line");
		buildSrc("                                                    table = lt_act_table");
		buildSrc("                                                    msg   = 'deletion of line failed' ).");

		buildExp("    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl.");
		buildExp("                                                lower  = 5");
		buildExp("                                                upper  = 10");
		buildExp("                                                number = lv_result_value ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_text_matches( pattern = lv_exp_message_pattern");
		buildExp("                                              text    = lv_act_message_text ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line");
		buildExp("                                                    table = lt_act_table");
		buildExp("                                                    msg   = 'deletion of line failed' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeNumTextTableToActFirst() {
		rule.configNumTextTableParamOrder.setEnumValue(AssertParameterOrder.ACT_FIRST);

		buildSrc("    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl.");
		buildSrc("                                                number = lv_result_value");
		buildSrc("                                                upper  = 10");
		buildSrc("                                                lower  = 5 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text");
		buildSrc("                                              pattern = lv_exp_message_pattern ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line");
		buildSrc("                                                    table = lt_act_table");
		buildSrc("                                                    msg   = 'deletion of line failed' ).");

		buildExp("    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl.");
		buildExp("                                                number = lv_result_value");
		buildExp("                                                lower  = 5");
		buildExp("                                                upper  = 10 ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text");
		buildExp("                                              pattern = lv_exp_message_pattern ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_table_not_contains( table = lt_act_table");
		buildExp("                                                    line  = ls_deleted_table_line");
		buildExp("                                                    msg   = 'deletion of line failed' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepNumTextTable() {
		rule.configNumTextTableParamOrder.setEnumValue(AssertParameterOrder.KEEP_AS_IS);

		buildSrc("    cl_abap_unit_assert=>assert_number_between( \" must be between 5 and 10 incl.");
		buildSrc("                                                number = lv_result_value");
		buildSrc("                                                upper  = 10");
		buildSrc("                                                lower  = 5 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_text_matches( text    = lv_act_message_text");
		buildSrc("                                              pattern = lv_exp_message_pattern ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_table_not_contains( line  = ls_deleted_table_line");
		buildSrc("                                                    table = lt_act_table");
		buildSrc("                                                    msg   = 'deletion of line failed' ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeOwnClassNumTextTableToActFirst() {
		rule.configNumTextTableParamOrder.setEnumValue(AssertParameterOrder.ACT_FIRST);
		rule.configAssertClassName.setValue("cx_assert");

		buildSrc("    cx_assert=>assert_text_matches( pattern = lv_exp_message_pattern");
		buildSrc("                                    text    = lv_act_message_text ).");
		buildSrc("");
		buildSrc("    cx_assert=>assert_table_not_contains( line  = ls_deleted_table_line");
		buildSrc("                                          table = lt_act_table");
		buildSrc("                                          msg   = 'deletion of line failed' ).");

		buildExp("    cx_assert=>assert_text_matches( text    = lv_act_message_text");
		buildExp("                                    pattern = lv_exp_message_pattern ).");
		buildExp("");
		buildExp("    cx_assert=>assert_table_not_contains( table = lt_act_table");
		buildExp("                                          line  = ls_deleted_table_line");
		buildExp("                                          msg   = 'deletion of line failed' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testChangeReturnCodeToExpFirst() {
		buildSrc("    cl_abap_unit_assert=>assert_subrc( quit = if_aunit_constants=>quit-no");
		buildSrc("                                       exp  = 4 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assume_return_code( act = lv_return_code");
		buildSrc("                                             exp = 4 ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( exp  = 4");
		buildExp("                                       quit = if_aunit_constants=>quit-no ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assume_return_code( exp = 4");
		buildExp("                                             act = lv_return_code ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeReturnCodeToActFirst() {
		rule.configReturnCodeParamOrder.setEnumValue(AssertParameterOrder.ACT_FIRST);

		buildSrc("    cl_abap_unit_assert=>assert_subrc( quit = if_aunit_constants=>quit-no");
		buildSrc("                                       exp  = 4 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_return_code( exp = 4");
		buildSrc("                                             act = lv_return_code ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( exp  = 4");
		buildExp("                                       quit = if_aunit_constants=>quit-no ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_return_code( act = lv_return_code");
		buildExp("                                             exp = 4 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepReturnCode() {
		rule.configReturnCodeParamOrder.setEnumValue(AssertParameterOrder.KEEP_AS_IS);

		buildSrc("    cl_abap_unit_assert=>assert_subrc( quit = if_aunit_constants=>quit-no");
		buildSrc("                                       exp  = 4 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_return_code( exp = 4");
		buildSrc("                                             act = lv_return_code ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeOwnClassReturnCodeToExpFirst() {
		rule.configNumTextTableParamOrder.setEnumValue(AssertParameterOrder.EXP_FIRST);
		rule.configAssertClassName.setValue("cx_assert");

		buildSrc("    cx_assert=>assert_subrc( quit = if_aunit_constants=>quit-no");
		buildSrc("                             exp  = 4 ).");

		buildExp("    cx_assert=>assert_subrc( exp  = 4");
		buildExp("                             quit = if_aunit_constants=>quit-no ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeCallsWithExportingKeywordAndComments() {
		buildSrc("    cl_abap_unit_assert=>assert_equals(");
		buildSrc("      EXPORTING");
		buildSrc("        act = 1");
		buildSrc("        exp = lines( lt_result ) ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_differs( \" commment1");
		buildSrc("      EXPORTING \" comment2");
		buildSrc("        act = 1");
		buildSrc("        exp = lines( lt_result ) ).");

		buildExp("    cl_abap_unit_assert=>assert_equals(");
		buildExp("      EXPORTING");
		buildExp("        exp = lines( lt_result )");
		buildExp("        act = 1 ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_differs( \" commment1");
		buildExp("      EXPORTING \" comment2");
		buildExp("        exp = lines( lt_result ) ");
		buildExp("        act = 1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentInExpression() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = lv_act_value");
		buildSrc("                                        exp = 2 * \" comment");
		buildSrc("                                                  lv_any_value ).");

		buildExp("    cl_abap_unit_assert=>assert_equals( exp = 2 * \" comment");
		buildExp("                                                  lv_any_value");
		buildExp("                                        act = lv_act_value ).");

		testRule();
	}
}
