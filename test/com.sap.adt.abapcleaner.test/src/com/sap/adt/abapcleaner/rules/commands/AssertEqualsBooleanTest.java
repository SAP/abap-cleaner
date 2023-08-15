package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AssertEqualsBooleanTest extends RuleTestBase {
	private AssertEqualsBooleanRule rule;
	
	AssertEqualsBooleanTest() {
		super(RuleID.ASSERT_EQUALS_BOOLEAN);
		rule = (AssertEqualsBooleanRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRemoveActIfOnlyParameter.setValue(true);
	}
	
	@Test
	void testSimpleExpAct() {
		rule.configRemoveActIfOnlyParameter.setValue(true);

		buildSrc("    cl_abap_unit_assert=>assert_equals( exp = abap_false");
		buildSrc("                                        act = lv_value ).");

		buildExp("    cl_abap_unit_assert=>assert_false( lv_value ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleExpActKeepParamName() {
		rule.configRemoveActIfOnlyParameter.setValue(false);

		buildSrc("    cl_abap_unit_assert=>assert_equals( exp = abap_false");
		buildSrc("                                        act = lv_value ).");

		buildExp("    cl_abap_unit_assert=>assert_false( act = lv_value ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexActExp() {
		rule.configRemoveActIfOnlyParameter.setValue(true);

		buildSrc("    cl_abap_unit_assert=>assert_equals( act = mo_instance->is_valid( )");
		buildSrc("                                        exp = abap_true ).");

		buildExp("    cl_abap_unit_assert=>assert_true( mo_instance->is_valid( ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexActExpKeepParamName() {
		rule.configRemoveActIfOnlyParameter.setValue(false);

		buildSrc("    cl_abap_unit_assert=>assert_equals( act = mo_instance->is_valid( )");
		buildSrc("                                        exp = abap_true ).");

		buildExp("    cl_abap_unit_assert=>assert_true( act = mo_instance->is_valid( ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexActExpWithCommentOnAct() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = mo_instance->is_valid( ) \" comment on act");
		buildSrc("                                        exp = abap_true ).");

		buildExp("    cl_abap_unit_assert=>assert_true( act = mo_instance->is_valid( ) ). \" comment on act");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexActExpWithCommentOnExp() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = mo_instance->is_valid( )");
		buildSrc("                                        exp = abap_true    \" comment on exp");
		buildSrc("                                      ).");

		buildExp("    cl_abap_unit_assert=>assert_true( act = mo_instance->is_valid( ) ). \" comment on exp");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testExpActWithCommentOnExp() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( exp = abap_false  \" comment on exp");
		buildSrc("                                        act = lv_value ).");

		buildExp("    cl_abap_unit_assert=>assert_false( \" comment on exp");
		buildExp("                                       act = lv_value ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testWithMsgParameter() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = lv_value");
		buildSrc("                                        exp = abap_true");
		buildSrc("                                        msg = 'message' ).");

		buildExp("    cl_abap_unit_assert=>assert_true( act = lv_value");
		buildExp("                                      msg = 'message' ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMultiParamsWithLineEndComments() {
		buildSrc("    cl_abap_unit_assert=>assert_equals(");
		buildSrc("        act  = lv_value     \" comment on act");
		buildSrc("        exp  = abap_false   \" comment on exp");
		buildSrc("        msg  = 'message'    \" comment on msg");
		buildSrc("        quit = lv_quit ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_equals(");
		buildSrc("        act = lv_value     \" comment on act");
		buildSrc("        exp = abap_true");
		buildSrc("        msg = 'message' ). \" comment on msg");

		buildExp("    cl_abap_unit_assert=>assert_false(");
		buildExp("        act  = lv_value     \" comment on act");
		buildExp("        \" comment on exp");
		buildExp("        msg  = 'message'    \" comment on msg");
		buildExp("        quit = lv_quit ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_true(");
		buildExp("        act = lv_value     \" comment on act");
		buildExp("        msg = 'message' ). \" comment on msg");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
