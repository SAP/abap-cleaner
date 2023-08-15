package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class NotIsTest extends RuleTestBase {
	NotIsTest() {
		super(RuleID.NOT_IS);
	}
	
	@Test
	void testSimpleCases() {
		buildSrc("    IF NOT iv_param IS SUPPLIED.");
		buildSrc("      IF NOT <ls_data> IS ASSIGNED.");
		buildSrc("        IF NOT lo_object IS BOUND.");
		buildSrc("          IF NOT lo_object IS INSTANCE OF cl_any_class.");
		buildSrc("            \" do nothing");
		buildSrc("          ENDIF.");
		buildSrc("        ENDIF.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF iv_param IS NOT SUPPLIED.");
		buildExp("      IF <ls_data> IS NOT ASSIGNED.");
		buildExp("        IF lo_object IS NOT BOUND.");
		buildExp("          IF lo_object IS NOT INSTANCE OF cl_any_class.");
		buildExp("            \" do nothing");
		buildExp("          ENDIF.");
		buildExp("        ENDIF.");
		buildExp("      ENDIF.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMultiLine() {
		// expect parameter alignment to NOT be called from this rule - it is executed later in the RuleID order

		buildSrc("    IF NOT lts_table[ a = 1");
		buildSrc("                      b = 2");
		buildSrc("                      c = 3 ]-field IS INITIAL.");
		buildSrc("      \" do nothing");
		buildSrc("    ENDIF.");

		buildExp("    IF lts_table[ a = 1");
		buildExp("                      b = 2");
		buildExp("                      c = 3 ]-field IS NOT INITIAL.");
		buildExp("      \" do nothing");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfOr() {
		buildSrc("    IF NOT iv_param IS SUPPLIED");
		buildSrc("    OR NOT iv_other_param IS SUPPLIED.");
		buildSrc("      \" do nothing");
		buildSrc("    ENDIF.");

		buildExp("    IF    iv_param       IS NOT SUPPLIED");
		buildExp("       OR iv_other_param IS NOT SUPPLIED.");
		buildExp("      \" do nothing");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	
	@Test
	void testComplexIf() {
		buildSrc("    IF NOT <ls_data> IS ASSIGNED");
		buildSrc("    OR ( NOT lo_object IS BOUND");
		buildSrc("         AND NOT lo_object IS INSTANCE OF cl_any_class ).");
		buildSrc("      \" do nothing");
		buildSrc("    ENDIF.");

		buildExp("    IF    <ls_data> IS NOT ASSIGNED");
		buildExp("       OR (     lo_object IS NOT BOUND");
		buildExp("            AND lo_object IS NOT INSTANCE OF cl_any_class ).");
		buildExp("      \" do nothing");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	
	@Test
	void testInsideCondConstructor() {
		buildSrc("    cl_abap_unit_assert=>fail( msg = COND #( WHEN NOT mts_any_table IS INITIAL");
		buildSrc("                                                  OR ( NOT its_table IS SUPPLIED");
		buildSrc("                                                       AND NOT its_other_table IS SUPPLIED )");
		buildSrc("                                             THEN 'unexpected method call'");
		buildSrc("                                             ELSE 'unexpected input parameters' ) ).");

		buildExp("    cl_abap_unit_assert=>fail( msg = COND #( WHEN    mts_any_table IS NOT INITIAL");
		buildExp("                                                  OR (     its_table       IS NOT SUPPLIED");
		buildExp("                                                       AND its_other_table IS NOT SUPPLIED )");
		buildExp("                                             THEN 'unexpected method call'");
		buildExp("                                             ELSE 'unexpected input parameters' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	
	@Test
	void testLineStructureRemainsIntact() {
		// expect the line structure to be kept intact when NOT is moved
		
		buildSrc("    IF lo_instance->attr1 = if_any_interface=>gc_any_constant OR");
		buildSrc("       NOT lo_instance->attr2 IS INITIAL OR");
		buildSrc("       NOT lo_instance->attribute3 IS INITIAL OR");
		buildSrc("       NOT lo_instance->a4 IS INITIAL.");
		buildSrc("    ENDIF.");

		buildExp("    IF lo_instance->attr1       = if_any_interface=>gc_any_constant OR");
		buildExp("       lo_instance->attr2      IS NOT INITIAL OR");
		buildExp("       lo_instance->attribute3 IS NOT INITIAL OR");
		buildExp("       lo_instance->a4         IS NOT INITIAL.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testChainUnchanged() {
		// expect commands that contain a chain colon to be skipped

		buildSrc("    CHECK NOT iv_param IS : SUPPLIED, INITIAL.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentAfterCheck() {
		buildSrc("CHECK \" comment");
		buildSrc("      NOT lv_other IS INITIAL.");

		buildExp("CHECK \" comment");
		buildExp("      lv_other IS NOT INITIAL.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentBeforeNot() {
		buildSrc("CHECK NOT lv_any IS INITIAL AND \" comment");
		buildSrc("      NOT lv_other IS INITIAL.");

		buildExp("CHECK lv_any   IS NOT INITIAL AND \" comment");
		buildExp("      lv_other IS NOT INITIAL.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
