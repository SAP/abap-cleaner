package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ValueStatementTest extends RuleTestBase {
	private ValueStatementRule rule;
	
	ValueStatementTest() {
		super(RuleID.VALUE_STATEMENT);
		rule = (ValueStatementRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMoveIntegerLiterals.setValue(true);
		rule.configMoveFloatLiterals.setValue(true);
		rule.configMoveStringLiterals.setValue(true);
		rule.configMoveIdentifiers.setValue(true);
		rule.configMoveComplexExpressions.setValue(true); 
		rule.configMoveMethodCalls.setValue(false);
		rule.configSkipIfRowsCommentedOut.setValue(true);
	}
	
	@Test
	void testValueWithOneLinePerRow() {
		buildSrc("    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )");
		buildSrc("                        ( a = 2  b = 4  c = 8 )");
		buildSrc("                        ( a = 2  b = 5  c = 10 )  ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        ( b = 3  c = 6 )");
		buildExp("                        ( b = 4  c = 8 )");
		buildExp("                        ( b = 5  c = 10 )  ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithBaseAndArithmeticExprs() {
		buildSrc("    lts_data = VALUE #( BASE <ls_item>-data");
		buildSrc("                        ( number   = lc_any_number");
		buildSrc("                          qty      = <ls_item>-component-qty / 2");
		buildSrc("                          amount   = <ls_item>-component-amount_per_unit * <ls_item>-component-qty / 2");
		buildSrc("                          is_final = abap_false )");
		buildSrc("");
		buildSrc("                        ( number   = lc_other_number");
		buildSrc("                          qty      = <ls_item>-component-qty / 2");
		buildSrc("                          amount   = <ls_item>-component-amount_per_unit * <ls_item>-component-qty / 2");
		buildSrc("                          is_final = abap_true ) ).");

		buildExp("    lts_data = VALUE #( BASE <ls_item>-data");
		buildExp("                        qty      = <ls_item>-component-qty / 2");
		buildExp("                        amount   = <ls_item>-component-amount_per_unit * <ls_item>-component-qty / 2");
		buildExp("                        ( number   = lc_any_number");
		buildExp("                          is_final = abap_false )");
		buildExp("");
		buildExp("                        ( number   = lc_other_number");
		buildExp("                          is_final = abap_true ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testValueWithMethodCall() {
		// expect method calls to NOT be extracted, because such a method call may have side effects,
		// and if positioned outside the inner parentheses of individual rows, it would be evaluated only once

		buildSrc("    lts_data = VALUE #( ( item_key    = '20220010000101'");
		buildSrc("                          contract_id = get_next_contract_id( )");
		buildSrc("                          item_id     = get_next_item_id( iv_add = 100 )");
		buildSrc("                          guid        = '1' )");
		buildSrc("                        ( item_key    = '20220010000101'");
		buildSrc("                          contract_id = get_next_contract_id( )");
		buildSrc("                          item_id     = get_next_item_id( iv_add = 100 )");
		buildSrc("                          guid        = '2' ) ).");

		buildExp("    lts_data = VALUE #( item_key    = '20220010000101'");
		buildExp("                        ( contract_id = get_next_contract_id( )");
		buildExp("                          item_id     = get_next_item_id( iv_add = 100 )");
		buildExp("                          guid        = '1' )");
		buildExp("                        ( contract_id = get_next_contract_id( )");
		buildExp("                          item_id     = get_next_item_id( iv_add = 100 )");
		buildExp("                          guid        = '2' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithTableExprAndCallChain() {
		buildSrc("    lts_data_exp = VALUE #( ( item_key    = '20220010000101'");
		buildSrc("                              condition   = 'ABCD'");
		buildSrc("                              contract_id = lt_items[ 1 ]->get_contract_data( )->get_contract_id( )");
		buildSrc("                              item_type   = lt_items[ 1 ]->item_data-item_type");
		buildSrc("                              flag        = if_any_interface=>co_any_flag");
		buildSrc("                              guid        = '1')");
		buildSrc("                            ( item_key    = '20220010000101'");
		buildSrc("                              condition   = 'ABCD'");
		buildSrc("                              contract_id = lt_items[ 1 ]->get_contract_data( )->get_contract_id( )");
		buildSrc("                              item_type   = lt_items[ 1 ]->item_data-item_type");
		buildSrc("                              flag        = if_any_interface=>co_other_flag");
		buildSrc("                              extra_flag  = abap_true");
		buildSrc("                              guid        = '2' ) ).");

		buildExp("    lts_data_exp = VALUE #( item_key    = '20220010000101'");
		buildExp("                            condition   = 'ABCD'");
		buildExp("                            item_type   = lt_items[ 1 ]->item_data-item_type");
		buildExp("                            ( contract_id = lt_items[ 1 ]->get_contract_data( )->get_contract_id( )");
		buildExp("                              flag        = if_any_interface=>co_any_flag");
		buildExp("                              guid        = '1')");
		buildExp("                            ( contract_id = lt_items[ 1 ]->get_contract_data( )->get_contract_id( )");
		buildExp("                              flag        = if_any_interface=>co_other_flag");
		buildExp("                              extra_flag  = abap_true");
		buildExp("                              guid        = '2' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithLineEndComment() {
		buildSrc("    lt_table = VALUE #( ( qty    = 20 ");
		buildSrc("                          amount = 10 )");
		buildSrc("                        ( qty    = 30 \" comment");
		buildSrc("                          amount = 10 ) ).");

		buildExp("    lt_table = VALUE #( amount = 10");
		buildExp("                        ( qty    = 20 )");
		buildExp("                        ( qty    = 30 ) ). \" comment");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithCommentLinesInside() {
		buildSrc("    lts_data_exp = VALUE #( ( item_key    = '20220010000101'");
		buildSrc("*                              comment that cannot be moved");
		buildSrc("                              condition   = 'ABCD'");
		buildSrc("                              contract_id = lt_item[ 1 ]->contract_id");
		buildSrc("                              guid        = '1')");
		buildSrc("                            ( item_key    = '20220010000101'");
		buildSrc("                              \" comment that can be moved");
		buildSrc("                              condition   = 'ABCD'");
		buildSrc("                              contract_id = lt_item[ 1 ]->contract_id");
		buildSrc("                              guid        = '2' ) ).");

		buildExp("    lts_data_exp = VALUE #( item_key    = '20220010000101'");
		buildExp("                            condition   = 'ABCD'");
		buildExp("                            contract_id = lt_item[ 1 ]->contract_id");
		buildExp("                            (");
		buildExp("*                              comment that cannot be moved");
		buildExp("                              guid        = '1')");
		buildExp("                            ( \" comment that can be moved");
		buildExp("                              guid        = '2' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	// -------------------------------------------------------------------------
	// test configuration settings for move types
	
	@Test
	void testDontMoveIntegerLiterals() {
		rule.configMoveIntegerLiterals.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( b = '3.14'");
		buildExp("                        c = |abc|");
		buildExp("                        d = lv_data");
		buildExp("                        e = x[ 1 ] / 2");
		buildExp("                        ( a = 2  f = next_value( )  g = 1 )");
		buildExp("                        ( a = 2  f = next_value( )  g = 2 )");
		buildExp("                        ( a = 2  f = next_value( )  g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontMoveFloatLiterals() {
		rule.configMoveFloatLiterals.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        c = |abc|");
		buildExp("                        d = lv_data");
		buildExp("                        e = x[ 1 ] / 2");
		buildExp("                        ( b = '3.14'  f = next_value( )  g = 1 )");
		buildExp("                        ( b = '3.14'  f = next_value( )  g = 2 )");
		buildExp("                        ( b = '3.14'  f = next_value( )  g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontMoveStringLiterals() {
		rule.configMoveStringLiterals.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        b = '3.14'");
		buildExp("                        d = lv_data");
		buildExp("                        e = x[ 1 ] / 2");
		buildExp("                        ( c = |abc|  f = next_value( )  g = 1 )");
		buildExp("                        ( c = |abc|  f = next_value( )  g = 2 )");
		buildExp("                        ( c = |abc|  f = next_value( )  g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontMoveIdentifiers() {
		rule.configMoveIdentifiers.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        b = '3.14'");
		buildExp("                        c = |abc|");
		buildExp("                        e = x[ 1 ] / 2");
		buildExp("                        ( d = lv_data  f = next_value( )  g = 1 )");
		buildExp("                        ( d = lv_data  f = next_value( )  g = 2 )");
		buildExp("                        ( d = lv_data  f = next_value( )  g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontMoveComplexExpressions() {
		rule.configMoveComplexExpressions.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        b = '3.14'");
		buildExp("                        c = |abc|");
		buildExp("                        d = lv_data");
		buildExp("                        ( e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildExp("                        ( e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildExp("                        ( e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveMethodCalls() {
		rule.configMoveMethodCalls.setValue(true);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 1 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 2 )");
		buildSrc("                        ( a = 2  b = '3.14'  c = |abc|  d = lv_data  e = x[ 1 ] / 2  f = next_value( )  g = 3 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        b = '3.14'");
		buildExp("                        c = |abc|");
		buildExp("                        d = lv_data");
		buildExp("                        e = x[ 1 ] / 2");
		buildExp("                        f = next_value( )");
		buildExp("                        ( g = 1 )");
		buildExp("                        ( g = 2 )");
		buildExp("                        ( g = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testGroupKeySkipped() {
		// ensure that the group_key in 'FOR GROUP OF ... IN ... GROUP BY ( ... )' is skipped, 
		// i.e. extraction of the 'plant' parameter only happens after 'GROUP BY ( ... )' 
		// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenfor_groups_of.htm
		
		buildSrc("  itab = VALUE #( FOR GROUPS OF <fs> IN itab");
		buildSrc("                  GROUP BY ( plant = <fs>-plant )");
		buildSrc("                  ( plant    = <fs>-plant");
		buildSrc("                    itemcode = <fs>-itemcode1 )");
		buildSrc("                  ( plant    = <fs>-plant");
		buildSrc("                    itemcode = <fs>-itemcode2 ) ).");

		buildExp("  itab = VALUE #( FOR GROUPS OF <fs> IN itab");
		buildExp("                  GROUP BY ( plant = <fs>-plant )");
		buildExp("                  plant    = <fs>-plant");
		buildExp("                  ( itemcode = <fs>-itemcode1 )");
		buildExp("                  ( itemcode = <fs>-itemcode2 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentBeforeParenthesis() {
		buildSrc("    lt_any = VALUE #( ( a = 1");
		buildSrc("                        b = 'X' )");
		buildSrc("                      ( a = 2");
		buildSrc("                        b = 'X'");
		buildSrc("                        \" any comment");
		buildSrc("                        )");
		buildSrc("                      ( a = 3");
		buildSrc("                        b = 'Y' ) ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSkipIfRowsCommentedOut() {
		buildSrc("    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )");
		buildSrc("*                        ( a = 1  b = 4  c = 8 ) ");
		buildSrc("                        ( a = 2  b = 5  c = 10 )  ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSkipIfRowsCommentedOut() {
		rule.configSkipIfRowsCommentedOut.setValue(false);

		buildSrc("    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )");
		buildSrc("*                        ( a = 1  b = 4  c = 8 ) ");
		buildSrc("                        ( a = 2  b = 5  c = 10 )  ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        ( b = 3  c = 6 )");
		buildExp("*                        ( a = 1  b = 4  c = 8 ) ");
		buildExp("                        ( b = 5  c = 10 )  ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentedOutValueNotExtracted() {
		buildSrc("    lts_data = VALUE #( ( a = 2");
		buildSrc("*                          b = 3");
		buildSrc("                          c = 6 )");
		buildSrc("                        ( a = 2");
		buildSrc("                          b = 3");
		buildSrc("                          c = 8 ) ).");

		buildExp("    lts_data = VALUE #( a = 2");
		buildExp("                        (");
		buildExp("*                          b = 3");
		buildExp("                          c = 6 )");
		buildExp("                        ( b = 3");
		buildExp("                          c = 8 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testGroupingWithEmptyLines() {
		// ensure that grouping (i.e. the empty line above fifth_comp) is preserved when factoring out, 
		// while the empty line is NOT propagated to the following factored-out components;
		// also, ensure that an additional empty line is added above the first table row in such a case 
		
		buildSrc("    lt_any_table = VALUE #( ( any_comp    = 'T1'");
		buildSrc("                              other_comp  = 'T2'");
		buildSrc("                              third_comp  = '2'");
		buildSrc("                              fourth_comp = 'T3'");
		buildSrc("");
		buildSrc("                              fifth_comp  = 'T4'");
		buildSrc("                              comp_6      = 'T4'");
		buildSrc("                              comp_7      = 'T4' )");
		buildSrc("                            ( any_comp    = 'T1'");
		buildSrc("                              other_comp  = 'T2'");
		buildSrc("                              third_comp  = '3'");
		buildSrc("                              fourth_comp = 'T5'");
		buildSrc("");
		buildSrc("                              fifth_comp  = 'T4'");
		buildSrc("                              comp_6      = 'T4'");
		buildSrc("                              comp_7      = 'T4' ) ).");

		buildExp("    lt_any_table = VALUE #( any_comp    = 'T1'");
		buildExp("                            other_comp  = 'T2'");
		buildExp("");
		buildExp("                            fifth_comp  = 'T4'");
		buildExp("                            comp_6      = 'T4'");
		buildExp("                            comp_7      = 'T4'");
		buildExp("");
		buildExp("                            ( third_comp  = '2'");
		buildExp("                              fourth_comp = 'T3' )");
		buildExp("                            ( third_comp  = '3'");
		buildExp("                              fourth_comp = 'T5' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
