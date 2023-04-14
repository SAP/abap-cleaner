package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignAssignmentsTest extends RuleTestBase {
	private AlignAssignmentsRule rule;
	
	AlignAssignmentsTest() {
		super(RuleID.ALIGN_ASSIGNMENTS);
		rule = (AlignAssignmentsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(true);
	}
	
	@Test
	void testFieldSymbolComponents() {
		buildSrc("    <ls_field_sym>-component = 1.");
		buildSrc("    <ls_field_sym>-comp2 = 'a'.");
		buildSrc("    <ls_field_sym>-c3 = VALUE #( ).");

		buildExp("    <ls_field_sym>-component = 1.");
		buildExp("    <ls_field_sym>-comp2     = 'a'.");
		buildExp("    <ls_field_sym>-c3        = VALUE #( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testStructureComponents() {
		buildSrc("    ls_any_struc-any_id = iv_any_id.");
		buildSrc("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
	
		buildExp("    ls_any_struc-any_id      = iv_any_id.");
		buildExp("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testStructureComponentsWithOffset() {
		buildSrc("    ls_any_struc-any_id = iv_any_id.");
		buildSrc("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
		buildSrc("    ls_any_struc-any_temp_id+0(2) = if_any_interface=>co_any_id_prefix.");
	
		buildExp("    ls_any_struc-any_id           = iv_any_id.");
		buildExp("    ls_any_struc-any_temp_id      = ls_any_struc-any_id.");
		buildExp("    ls_any_struc-any_temp_id+0(2) = if_any_interface=>co_any_id_prefix.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAttributeAccess() {
		buildSrc("    ro_instance->mv_any = iv_any.");
		buildSrc("    ro_instance->mv_other = iv_other.");

		buildExp("    ro_instance->mv_any   = iv_any.");
		buildExp("    ro_instance->mv_other = iv_other.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCalculationAssignmentOps() {
		buildSrc("    ls_struc-component += 1.");
		buildSrc("    ls_struc-comp2 = 'a'.");
		buildSrc("    ls_struc-start_date+6(2) = 31.");
		buildSrc("    ls_struc-c3 = VALUE #( ).");
		buildSrc("    ls_struc-comp4 -= 1.");

		buildExp("    ls_struc-component       += 1.");
		buildExp("    ls_struc-comp2            = 'a'.");
		buildExp("    ls_struc-start_date+6(2)  = 31.");
		buildExp("    ls_struc-c3               = VALUE #( ).");
		buildExp("    ls_struc-comp4           -= 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignAcrossCommentsAndEmptyLines() {
		buildSrc("    \" test alignment across comments and empty lines (depending on configuration):");
		buildSrc("    ls_struc-component += 1.");
		buildSrc("    \" comment");
		buildSrc("    ls_struc-comp2 = 'a'.");
		buildSrc("");
		buildSrc("    ls_struc-c3 = VALUE #( ).");
		buildSrc("");
		buildSrc("    \" comment");
		buildSrc("    ls_struc-comp4 -= 1.");

		buildExp("    \" test alignment across comments and empty lines (depending on configuration):");
		buildExp("    ls_struc-component += 1.");
		buildExp("    \" comment");
		buildExp("    ls_struc-comp2      = 'a'.");
		buildExp("");
		buildExp("    ls_struc-c3         = VALUE #( ).");
		buildExp("");
		buildExp("    \" comment");
		buildExp("    ls_struc-comp4     -= 1.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(true);
		
		buildSrc("    <ls_field_sym>-component = 1.");
		buildSrc("    <ls_field_sym>-comp2 = 'a'.");
		buildSrc("");
		buildSrc("    <ls_field_sym>-c3 = VALUE #( ).");

		buildExp("    <ls_field_sym>-component = 1.");
		buildExp("    <ls_field_sym>-comp2     = 'a'.");
		buildExp("");
		buildExp("    <ls_field_sym>-c3        = VALUE #( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNotAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(false);
		
		buildSrc("    <ls_field_sym>-component = 1.");
		buildSrc("");
		buildSrc("    <ls_field_sym>-comp2 = 'a'.");
		buildSrc("    <ls_field_sym>-c3 = VALUE #( ).");

		buildExp("    <ls_field_sym>-component = 1.");
		buildExp("");
		buildExp("    <ls_field_sym>-comp2 = 'a'.");
		buildExp("    <ls_field_sym>-c3    = VALUE #( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(true);
		
		buildSrc("    ls_any_struc-any_id = iv_any_id.");
		buildSrc("    \" comment line");
		buildSrc("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
	
		buildExp("    ls_any_struc-any_id      = iv_any_id.");
		buildExp("    \" comment line");
		buildExp("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNotAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(false);
		
		buildSrc("    ls_any_struc-any_id = iv_any_id.");
		buildSrc("    ls_any_struc-any_type = iv_any_type.");
		buildSrc("    \" comment line");
		buildSrc("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
	
		buildExp("    ls_any_struc-any_id   = iv_any_id.");
		buildExp("    ls_any_struc-any_type = iv_any_type.");
		buildExp("    \" comment line");
		buildExp("    ls_any_struc-any_temp_id = ls_any_struc-any_id.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
