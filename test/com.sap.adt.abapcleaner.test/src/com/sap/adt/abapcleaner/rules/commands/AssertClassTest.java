package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AssertClassTest extends RuleTestBase {
	private AssertClassRule rule;
	
	AssertClassTest() {
		super(RuleID.ASSERT_CLASS);
		rule = (AssertClassRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAssertClassName.setValue("cx_any_assert");
	}
	
	@Test
	void testAssertBound() {
		buildSrc("    ASSERT lo_item IS BOUND.");
		buildSrc("    ASSERT is_structure-component IS NOT BOUND.");
		buildSrc("    ASSERT NOT is_structure-component IS BOUND.");

		buildExp("    cx_any_assert=>assert_bound( lo_item ).");
		buildExp("    cx_any_assert=>assert_not_bound( is_structure-component ).");
		buildExp("    cx_any_assert=>assert_not_bound( is_structure-component ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssertInitial() {
		buildSrc("    ASSERT is_structure-component IS INITIAL.");
		buildSrc("    ASSERT io_instance IS NOT INITIAL.");
		buildSrc("    ASSERT NOT io_instance IS INITIAL.");

		buildExp("    cx_any_assert=>assert_initial( is_structure-component ).");
		buildExp("    cx_any_assert=>assert_not_initial( io_instance ).");
		buildExp("    cx_any_assert=>assert_not_initial( io_instance ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssertSubrc() {
		buildSrc("    ASSERT sy-subrc = 0. \" comment");
		buildSrc("    ASSERT sy-subrc = 4.");
		buildSrc("    ASSERT sy-subrc = any_method_name( param_a = 'abc'");
		buildSrc("                                       param_b = 'def' ).");

		buildExp("    cx_any_assert=>assert_subrc( ). \" comment");
		buildExp("    cx_any_assert=>assert_subrc( 4 ).");
		buildExp("    cx_any_assert=>assert_subrc( any_method_name( param_a = 'abc'");
		buildExp("                                                  param_b = 'def' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssertTrueOrFalse() {
		buildSrc("    ASSERT lv_was_initialized = abap_true.");
		buildSrc("    ASSERT mv_is_valid = abap_false.");
		buildSrc("    ASSERT line_exists( lts_table[ iv_param_a = 1");
		buildSrc("                                   iv_param_b = 'abc' ] ) = abap_true.");

		buildExp("    cx_any_assert=>assert_true( lv_was_initialized ).");
		buildExp("    cx_any_assert=>assert_false( mv_is_valid ).");
		buildExp("    cx_any_assert=>assert_true( line_exists( lts_table[ iv_param_a = 1");
		buildExp("                                                        iv_param_b = 'abc' ] ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssertEquals() {
		buildSrc("    ASSERT ms_struc-component = if_any_interface=>co_any_constant.");
		buildSrc("    ASSERT lv_timestamp(7) = lts_any_table[ 1 ]-lv_timestamp(7).");
		buildSrc("    ASSERT lts_any_table[ 1 ]-timestamp(7) = lts_any_table[ 2 ]-timestamp(7).");

		buildExp("    cx_any_assert=>assert_equals( act = ms_struc-component");
		buildExp("                                  exp = if_any_interface=>co_any_constant ).");
		buildExp("    cx_any_assert=>assert_equals( act = lv_timestamp(7)");
		buildExp("                                  exp = lts_any_table[ 1 ]-lv_timestamp(7) ).");
		buildExp("    cx_any_assert=>assert_equals( act = lts_any_table[ 1 ]-timestamp(7)");
		buildExp("                                  exp = lts_any_table[ 2 ]-timestamp(7) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssertDiffers() {
		buildSrc("    ASSERT lo_instance->ms_data-item_category <> if_any_interface=>co_any_constant.");
		buildSrc("    ASSERT sy-subrc <> 0.");

		buildExp("    cx_any_assert=>assert_differs( act = lo_instance->ms_data-item_category");
		buildExp("                                   exp = if_any_interface=>co_any_constant ).");
		buildExp("    cx_any_assert=>assert_differs( act = sy-subrc");
		buildExp("                                   exp = 0 ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testImplicitFail() {
		buildSrc("    ASSERT 1 = 2.");
		buildSrc("    ASSERT 314159265 = 42.");

		buildExp("    cx_any_assert=>fail( ).");
		buildExp("    cx_any_assert=>fail( ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNoDedicatedMethod() {
		buildSrc("    ASSERT <ls_new_item> IS ASSIGNED.");
		buildSrc("    ASSERT <ls_item> IS NOT ASSIGNED.");
		buildSrc("    ASSERT is_any_struc IS NOT INITIAL OR is_other_struc IS NOT INITIAL.");
		buildSrc("    ASSERT <ls_item>-component <= ms_struc-component.");
		buildSrc("    ASSERT lv_quantity <= 100.");
		buildSrc("    ASSERT abs( <ls_item>-amount ) > 0.");

		buildExp("    cx_any_assert=>assert_true( xsdbool( <ls_new_item> IS ASSIGNED ) ).");
		buildExp("    cx_any_assert=>assert_true( xsdbool( <ls_item> IS NOT ASSIGNED ) ).");
		buildExp("    cx_any_assert=>assert_true( xsdbool( is_any_struc IS NOT INITIAL OR is_other_struc IS NOT INITIAL ) ).");
		buildExp("    cx_any_assert=>assert_true( xsdbool( <ls_item>-component <= ms_struc-component ) ).");
		buildExp("    cx_any_assert=>assert_true( xsdbool( lv_quantity <= 100 ) ).");
		buildExp("    cx_any_assert=>assert_true( xsdbool( abs( <ls_item>-amount ) > 0 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOtherAssertClassName() {
		rule.configAssertClassName.setValue("cx_other_assert");
		
		buildSrc("    ASSERT ls_item-source_item IS NOT BOUND.");
		buildSrc("    ASSERT NOT io_instance IS INITIAL.");
		buildSrc("    ASSERT sy-subrc = 0. \" comment");

		buildExp("    cx_other_assert=>assert_not_bound( ls_item-source_item ).");
		buildExp("    cx_other_assert=>assert_not_initial( io_instance ).");
		buildExp("    cx_other_assert=>assert_subrc( ). \" comment");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		buildSrc("    ASSERT: a = 1, b = 2.");
		buildSrc("    ASSERT NOT: lo_item IS BOUND,");
		buildSrc("                <ls_field> IS ASSIGNED.");
		buildSrc("    ASSERT lo_item IS : NOT BOUND, INITIAL.");
		
		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
