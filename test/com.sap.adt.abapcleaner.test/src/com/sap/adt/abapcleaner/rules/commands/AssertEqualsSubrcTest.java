package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AssertEqualsSubrcTest extends RuleTestBase {
	private AssertEqualsSubrcRule rule;
	
	AssertEqualsSubrcTest() {
		super(RuleID.ASSERT_EQUALS_SUBRC);
		rule = (AssertEqualsSubrcRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRemoveExpEqualsZero.setValue(true);
	}
	
	@Test
	void testSimpleCaseRemoveExp0() {
		rule.configRemoveExpEqualsZero.setValue(true);
		
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc");
		buildSrc("                                        exp = 0 ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleCaseKeepExp0() {
		rule.configRemoveExpEqualsZero.setValue(false);
		
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc");
		buildSrc("                                        exp = 0 ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( exp = 0 ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testWithCommentRemoveExp0() {
		rule.configRemoveExpEqualsZero.setValue(true);

		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc \" comment");
		buildSrc("                                        exp = 0 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc");
		buildSrc("                                        exp = 0   \" comment");
		buildSrc("                                      ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( ). \" comment");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_subrc( ). \" comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testWithCommentKeepExp0() {
		rule.configRemoveExpEqualsZero.setValue(false);

		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc \" comment");
		buildSrc("                                        exp = 0 ).");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>assert_equals( act = sy-subrc");
		buildSrc("                                        exp = 0   \" comment");
		buildSrc("                                      ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( \" comment");
		buildExp("                                       exp = 0 ).");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>assert_subrc( exp = 0   \" comment");
		buildExp("                                      ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testExp4() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( exp = 4");
		buildSrc("                                        act = sy-subrc ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( exp = 4 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testFourParameters() {
		buildSrc("    cl_abap_unit_assert=>assert_equals( act  = sy-subrc");
		buildSrc("                                        exp  = 0");
		buildSrc("                                        msg  = 'message'");
		buildSrc("                                        quit = lv_quit ).");

		buildExp("    cl_abap_unit_assert=>assert_subrc( msg  = 'message'");
		buildExp("                                       quit = lv_quit ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
