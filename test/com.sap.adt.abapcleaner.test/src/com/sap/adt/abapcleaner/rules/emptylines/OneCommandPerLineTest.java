package com.sap.adt.abapcleaner.rules.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class OneCommandPerLineTest extends RuleTestBase {
	private OneCommandPerLineRule rule;
	
	OneCommandPerLineTest() {
		super(RuleID.ONE_COMMAND_PER_LINE);
		rule = (OneCommandPerLineRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configKeepOneLinersAfterWhen.setValue(true);
		rule.configKeepMultiLinersAfterWhen.setValue(false);
	}

	@Test
	void testMoveDeclarationsAndAssignments() {
		buildSrc("    DATA lv_x TYPE i VALUE 1. DATA lv_y TYPE i VALUE 2. DATA lv_z TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    \" comment");
		buildSrc("    lv_x += 2.lv_y -= 4.lv_z *= 6.");

		buildExp("    DATA lv_x TYPE i VALUE 1.");
		buildExp("    DATA lv_y TYPE i VALUE 2.");
		buildExp("    DATA lv_z TYPE i VALUE 3.");
		buildExp("");
		buildExp("    \" comment");
		buildExp("    lv_x += 2.");
		buildExp("    lv_y -= 4.");
		buildExp("    lv_z *= 6.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepEmptyCommandAndPragmaLine() {
		buildSrc("    DATA lv_x TYPE i VALUE 1.. DATA lv_y TYPE i VALUE 2... DATA lv_z TYPE i VALUE 3. ##NEEDED");

		buildExp("    DATA lv_x TYPE i VALUE 1..");
		buildExp("    DATA lv_y TYPE i VALUE 2...");
		buildExp("    DATA lv_z TYPE i VALUE 3. ##NEEDED");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSkipTypesAfterInclude() {
		buildSrc("  TYPES:");
		buildSrc("    BEGIN OF ty_s_any,");
		buildSrc("      any_comp   TYPE i,");
		buildSrc("      other_comp TYPE string.");
		buildSrc("      INCLUDE TYPE ty_s_other. TYPES:");
		buildSrc("    END OF ty_s_any.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveNestedIfBlocks() {
		buildSrc("    IF lv_x > lv_y. IF lv_y < lv_z.");
		buildSrc("      lv_x = lv_z - lv_y.ELSE.lv_y = lv_z - lv_x.ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF lv_x > lv_y.");
		buildExp("      IF lv_y < lv_z.");
		buildExp("        lv_x = lv_z - lv_y.");
		buildExp("      ELSE.");
		buildExp("        lv_y = lv_z - lv_x.");
		buildExp("      ENDIF.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveNestedDoAndIfBlocks() {
		buildSrc("    DO 3 TIMES.lv_y *= 3.DO 5 TIMES.IF lv_x < lv_y + lv_z.lv_x += 2.lv_y -= 1.ELSE.lv_z *= 1.ENDIF.ENDDO.ENDDO.");

		buildExp("    DO 3 TIMES.");
		buildExp("      lv_y *= 3.");
		buildExp("      DO 5 TIMES.");
		buildExp("        IF lv_x < lv_y + lv_z.");
		buildExp("          lv_x += 2.");
		buildExp("          lv_y -= 1.");
		buildExp("        ELSE.");
		buildExp("          lv_z *= 1.");
		buildExp("        ENDIF.");
		buildExp("      ENDDO.");
		buildExp("    ENDDO.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepOneLinersAfterWhen() {
		buildSrc("    CASE lv_x.");
		buildSrc("      WHEN 1. lv_x += 1. lv_y += 1.");
		buildSrc("      WHEN 2. lv_y -= 1. lv_z -= 2.");
		buildSrc("      WHEN 3. lv_z *= 2. lv_x *= 3.");
		buildSrc("    ENDCASE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveOneLinersAfterWhen() {
		rule.configKeepOneLinersAfterWhen.setValue(false);

		buildSrc("    CASE lv_x.");
		buildSrc("      WHEN 1. lv_x += 1. lv_y += 1.");
		buildSrc("      WHEN 2. lv_y -= 1. lv_z -= 2.");
		buildSrc("      WHEN 3. lv_z *= 2. lv_x *= 3.");
		buildSrc("    ENDCASE.");

		buildExp("    CASE lv_x.");
		buildExp("      WHEN 1.");
		buildExp("        lv_x += 1.");
		buildExp("        lv_y += 1.");
		buildExp("      WHEN 2.");
		buildExp("        lv_y -= 1.");
		buildExp("        lv_z -= 2.");
		buildExp("      WHEN 3.");
		buildExp("        lv_z *= 2.");
		buildExp("        lv_x *= 3.");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepMultiLinersAfterWhen() {
		rule.configKeepMultiLinersAfterWhen.setValue(true);

		buildSrc("    CASE lv_x.");
		buildSrc("      WHEN 1. lv_x += 1. do_something( iv_value = lv_x");
		buildSrc("                                       iv_name  = 'X' ).");
		buildSrc("      WHEN 2. lv_y -= 1. do_something( iv_value = lv_y");
		buildSrc("                                       iv_name  = 'Y' ).");
		buildSrc("      WHEN 3. lv_z *= 2. do_something( iv_value = lv_z ).");
		buildSrc("        do_something_else( iv_value = lv_z ).");
		buildSrc("    ENDCASE.");

		buildExp("    CASE lv_x.");
		buildExp("      WHEN 1. lv_x += 1. do_something( iv_value = lv_x");
		buildExp("                                       iv_name  = 'X' ).");
		buildExp("      WHEN 2. lv_y -= 1. do_something( iv_value = lv_y");
		buildExp("                                       iv_name  = 'Y' ).");
		buildExp("      WHEN 3. lv_z *= 2. do_something( iv_value = lv_z ).");
		buildExp("        do_something_else( iv_value = lv_z ).");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveMultiLinersAfterWhen() {
		buildSrc("    CASE lv_x.");
		buildSrc("      WHEN 1. lv_x += 1. do_something( iv_value = lv_x");
		buildSrc("                                       iv_name  = 'X' ).");
		buildSrc("      WHEN 2. lv_y -= 1. do_something( iv_value = lv_y");
		buildSrc("                                       iv_name  = 'Y' ).");
		buildSrc("      WHEN 3. lv_z *= 2. do_something( iv_value = lv_z ).");
		buildSrc("        do_something_else( iv_value = lv_z ).");
		buildSrc("    ENDCASE.");

		buildExp("    CASE lv_x.");
		buildExp("      WHEN 1.");
		buildExp("        lv_x += 1.");
		buildExp("        do_something( iv_value = lv_x");
		buildExp("                      iv_name  = 'X' ).");
		buildExp("      WHEN 2.");
		buildExp("        lv_y -= 1.");
		buildExp("        do_something( iv_value = lv_y");
		buildExp("                      iv_name  = 'Y' ).");
		buildExp("      WHEN 3.");
		buildExp("        lv_z *= 2.");
		buildExp("        do_something( iv_value = lv_z ).");
		buildExp("        do_something_else( iv_value = lv_z ).");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
