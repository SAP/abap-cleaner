package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class LogicalOperatorPositionTest extends RuleTestBase {
	LogicalOperatorPositionTest() {
		super(RuleID.LOGICAL_OPERATOR_POSITION);
	}
	
	@Test
	void testIfWithMultipleOr() {
		buildSrc("    IF ls_data-item_type = if_any_interface=>co_any_item_type OR");
		buildSrc("       ls_data-start_date IS NOT INITIAL OR");
		buildSrc("       ls_data-end_date IS INITIAL OR");
		buildSrc("       ls_data-processing_method = if_any_interface=>co_any_method.");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF    ls_data-item_type          = if_any_interface=>co_any_item_type");
		buildExp("       OR ls_data-start_date        IS NOT INITIAL");
		buildExp("       OR ls_data-end_date          IS INITIAL");
		buildExp("       OR ls_data-processing_method  = if_any_interface=>co_any_method.");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIfWithOrInParentheses() {
		buildSrc("    IF  ( c IS NOT SUPPLIED OR");
		buildSrc("          b IS INITIAL ) AND");
		buildSrc("        ( d IS SUPPLIED OR");
		buildSrc("          b IS NOT INITIAL ).");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF     (    c IS NOT SUPPLIED");
		buildExp("             OR b IS INITIAL )");
		buildExp("       AND (    d IS SUPPLIED");
		buildExp("             OR b IS NOT INITIAL ).");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testXsdBoolWithMultipleOr() {
		buildSrc("    rv_result = xsdbool( lv_flag_a = abap_true OR");
		buildSrc("                         lv_flag_b = abap_true OR");
		buildSrc("                         lv_flag_c = abap_true ).");

		buildExp("    rv_result = xsdbool(    lv_flag_a = abap_true");
		buildExp("                         OR lv_flag_b = abap_true");
		buildExp("                         OR lv_flag_c = abap_true ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testXsdBoolInAssignment() {
		buildSrc("    DATA(lv_result) = xsdbool( lv_value_a > 0 AND");
		buildSrc("                               ( lv_quantity + lv_quantity_delta ) <= 0 AND");
		buildSrc("                               ( lv_quantity <> 0 OR lv_quantity_delta <> 0 ) ).");

		buildExp("    DATA(lv_result) = xsdbool(     lv_value_a > 0");
		buildExp("                               AND ( lv_quantity + lv_quantity_delta ) <= 0");
		buildExp("                               AND ( lv_quantity <> 0 OR lv_quantity_delta <> 0 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testChainUnchanged() {
		// expect commands that contain a chain colon to be skipped
		
		buildSrc("    CHECK iv_param > 0 AND");
		buildSrc("          iv_param < : iv_limit_a, iv_limit_b.");
		buildSrc("    CHECK: iv_param > 0 AND");
		buildSrc("           iv_param < iv_limit_a,");
		buildSrc("           iv_param > 0 OR");
		buildSrc("           iv_param < iv_limit_b.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
