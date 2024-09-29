package com.sap.adt.abapcleaner.rules.syntax;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class NeedlessParenthesesTest extends RuleTestBase {
	private NeedlessParenthesesRule rule;
	
	NeedlessParenthesesTest() {
		super(RuleID.NEEDLESS_PARENTHESES);
		rule = (NeedlessParenthesesRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRemoveAroundAll.setValue(true);
		rule.configRemoveAroundRelExpr.setValue(true);
		rule.configRemoveOrParenthesisAnd.setValue(true);
		rule.configRemoveAroundSameOp.setValue(true);
	}
	
	@Test
	void testCheckParensAroundAll() {
		rule.configRemoveAroundAll.setValue(true);

		buildSrc("    CHECK ( sy-subrc <> 0 ).");
		buildSrc("    CHECK ( lt_any_table IS NOT INITIAL ).");
		buildSrc("    CHECK ( lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED ).");

		buildExp("    CHECK sy-subrc <> 0.");
		buildExp("    CHECK lt_any_table IS NOT INITIAL.");
		buildExp("    CHECK lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWhileAndIfParensAroundAll() {
		rule.configRemoveAroundAll.setValue(true);

		buildSrc("    WHILE ( lv_continue = abap_true ).");
		buildSrc("      IF ( prepare_next_item( ) < 0 ).");
		buildSrc("        lv_continue = abap_false.");
		buildSrc("      ELSEIF ( iv_any_value >= 0 AND iv_other_value <= 0 ).");
		buildSrc("        process_item( ).");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE lv_continue = abap_true.");
		buildExp("      IF prepare_next_item( ) < 0.");
		buildExp("        lv_continue = abap_false.");
		buildExp("      ELSEIF iv_any_value >= 0 AND iv_other_value <= 0.");
		buildExp("        process_item( ).");
		buildExp("      ENDIF.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testParensAroundAllKept() {
		rule.configRemoveAroundAll.setValue(false);

		buildSrc("    CHECK ( sy-subrc <> 0 ).");
		buildSrc("    CHECK ( lt_any_table IS NOT INITIAL ).");
		buildSrc("    CHECK ( lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED ).");
		buildSrc("");
		buildSrc("    WHILE ( lv_continue = abap_true ).");
		buildSrc("      IF ( prepare_next_item( ) < 0 ).");
		buildSrc("        lv_continue = abap_false.");
		buildSrc("      ELSEIF ( iv_any_value >= 0 AND iv_other_value <= 0 ).");
		buildSrc("        process_item( ).");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveParensAroundRelExpr() {
		rule.configRemoveAroundRelExpr.setValue(true);

		buildSrc("    CHECK     ( lv_any_value <> lv_other_value )");
		buildSrc("          AND ( lv_any_value <> lv_third_value ).");
		buildSrc("");
		buildSrc("    lv_result = xsdbool( ( lv_any_value IS INITIAL ) AND ( lt_any_table IS NOT INITIAL ) ).");
		buildSrc("");
		buildSrc("    IF    ( iv_state = lc_invalid_state )");
		buildSrc("       OR ( iv_state = lc_obsolete_state ).");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		buildExp("    CHECK     lv_any_value <> lv_other_value");
		buildExp("          AND lv_any_value <> lv_third_value.");
		buildExp("");
		buildExp("    lv_result = xsdbool( lv_any_value IS INITIAL AND lt_any_table IS NOT INITIAL ).");
		buildExp("");
		buildExp("    IF    iv_state = lc_invalid_state");
		buildExp("       OR iv_state = lc_obsolete_state.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParensAroundRelExpr() {
		rule.configRemoveAroundRelExpr.setValue(false);

		buildSrc("    CHECK     ( lv_any_value <> lv_other_value )");
		buildSrc("          AND ( lv_any_value <> lv_third_value ).");
		buildSrc("");
		buildSrc("    lv_result = xsdbool( ( lv_any_value IS INITIAL ) AND ( lt_any_table IS NOT INITIAL ) ).");
		buildSrc("");
		buildSrc("    IF    ( iv_state = lc_invalid_state )");
		buildSrc("       OR ( iv_state = lc_obsolete_state ).");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveOrParenthesisAnd() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    IF    lv_event_year < lv_final_year");
		buildSrc("       OR (     lv_event_year  = lv_final_year");
		buildSrc("            AND lv_event_month < lv_final_month ).");
		buildSrc("      process_past_event( ).");
		buildSrc("");
		buildSrc("    ELSEIF    ( lv_any_amount < 0 AND lv_other_amount > 0 )");
		buildSrc("           OR ( lv_any_amount > 0 AND lv_other_amount < 0 ).");
		buildSrc("      process_different_signs( ).");
		buildSrc("");
		buildSrc("    ELSEIF sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_any_value IS INITIAL ) OR lv_stop = abap_true.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    LOOP AT lt_any_table INTO DATA(ls_struc)");
		buildSrc("         WHERE    ( comp1 IS INITIAL     AND comp2 = 'A' )");
		buildSrc("               OR ( comp1 IS NOT INITIAL AND comp2 = 'B' ).");
		buildSrc("    ENDLOOP.");

		buildExp("    IF    lv_event_year < lv_final_year");
		buildExp("       OR     lv_event_year  = lv_final_year");
		buildExp("          AND lv_event_month < lv_final_month.");
		buildExp("      process_past_event( ).");
		buildExp("");
		buildExp("    ELSEIF    lv_any_amount < 0 AND lv_other_amount > 0");
		buildExp("           OR lv_any_amount > 0 AND lv_other_amount < 0.");
		buildExp("      process_different_signs( ).");
		buildExp("");
		buildExp("    ELSEIF sy-subrc <> 0 OR sy-subrc = 0 AND lv_any_value IS INITIAL OR lv_stop = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    LOOP AT lt_any_table INTO DATA(ls_struc)");
		buildExp("         WHERE    comp1 IS INITIAL     AND comp2 = 'A'");
		buildExp("               OR comp1 IS NOT INITIAL AND comp2 = 'B'.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	@Test
	void testKeepOrParenthesisAnd() {
		rule.configRemoveOrParenthesisAnd.setValue(false);

		buildSrc("    IF    lv_event_year < lv_final_year");
		buildSrc("       OR (     lv_event_year  = lv_final_year");
		buildSrc("            AND lv_event_month < lv_final_month ).");
		buildSrc("      process_past_event( ).");
		buildSrc("");
		buildSrc("    ELSEIF    ( lv_any_amount < 0 AND lv_other_amount > 0 )");
		buildSrc("           OR ( lv_any_amount > 0 AND lv_other_amount < 0 ).");
		buildSrc("      process_different_signs( ).");
		buildSrc("");
		buildSrc("    ELSEIF sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_any_value IS INITIAL ) OR lv_stop = abap_true.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    LOOP AT lt_any_table INTO DATA(ls_struc)");
		buildSrc("         WHERE    ( comp1 IS INITIAL     AND comp2 = 'A' )");
		buildSrc("               OR ( comp1 IS NOT INITIAL AND comp2 = 'B' ).");
		buildSrc("    ENDLOOP.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveParensAroundSameOp() {
		rule.configRemoveAroundSameOp.setValue(true);

		buildSrc("    IF    (    lt_table1  IS INITIAL");
		buildSrc("            OR lt_table2  IS INITIAL )");
		buildSrc("       OR (    lv_amount1 <= 0");
		buildSrc("            OR lv_amount2 <= 0 ).");
		buildSrc("      RETURN.");
		buildSrc("");
		buildSrc("    ELSEIF     ( lv_year  >= iv_min_year  AND lv_year  <= iv_max_year )");
		buildSrc("           AND ( iv_count >= iv_min_count AND iv_count <= iv_max_count ).");
		buildSrc("      process_match( ).");
		buildSrc("    ENDIF.");

		buildExp("    IF    lt_table1  IS INITIAL");
		buildExp("       OR lt_table2  IS INITIAL");
		buildExp("       OR lv_amount1 <= 0");
		buildExp("       OR lv_amount2 <= 0.");
		buildExp("      RETURN.");
		buildExp("");
		buildExp("    ELSEIF     lv_year  >= iv_min_year  AND lv_year  <= iv_max_year");
		buildExp("           AND iv_count >= iv_min_count AND iv_count <= iv_max_count.");
		buildExp("      process_match( ).");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParensAroundSameOp() {
		rule.configRemoveAroundSameOp.setValue(false);

		buildSrc("    IF    (    lt_table1  IS INITIAL");
		buildSrc("            OR lt_table2  IS INITIAL )");
		buildSrc("       OR (    lv_amount1 <= 0");
		buildSrc("            OR lv_amount2 <= 0 ).");
		buildSrc("      RETURN.");
		buildSrc("");
		buildSrc("    ELSEIF     ( lv_year  >= iv_min_year  AND lv_year  <= iv_max_year )");
		buildSrc("           AND ( iv_count >= iv_min_count AND iv_count <= iv_max_count ).");
		buildSrc("      process_match( ).");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParensAroundNot() {
		buildSrc("    IF     ( NOT lv_any_value IS INITIAL )");
		buildSrc("       AND ( NOT lv_any_value  = 'T1' ).");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF    ( NOT sy-subrc = 0 )");
		buildSrc("       OR ( NOT <ls_any_struc> IS INITIAL ).");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParensAroundEquiv() {
		buildSrc("    IF       ( a < b )");
		buildSrc("       EQUIV ( b < c ).");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSkipUnsupportedLogicalExpressions() {
		buildSrc("    CHECK ( ( a IS NOT INITIAL ) ).");
		buildSrc("    IF ( ( ( a = b ) ) ).");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParensAroundForInWhere() {
		// ensure that the 'outer' parentheses of 'WHERE ( log_expr )' are kept 

		rule.configRemoveAroundRelExpr.setValue(true);

		buildSrc("    DATA(lt_value) = VALUE #( FOR wa IN messages");
		buildSrc("                              WHERE ( ( comp = 'A' ) AND ( comp2 = '1' ) )");
		buildSrc("                              ( wa ) ).");

		buildExp("    DATA(lt_value) = VALUE #( FOR wa IN messages");
		buildExp("                              WHERE ( comp = 'A' AND comp2 = '1' )");
		buildExp("                              ( wa ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	private void assertConfigRemoveOptionsEnabled() {
		assertTrue(rule.isConfigValueEnabled(rule.configRemoveAroundAll));
		assertTrue(rule.isConfigValueEnabled(rule.configRemoveAroundRelExpr));
		assertTrue(rule.isConfigValueEnabled(rule.configRemoveOrParenthesisAnd));
		assertTrue(rule.isConfigValueEnabled(rule.configRemoveAroundSameOp));
	}
	
	@Test
	void testConfigWarningShownForOrParenthesisAnd() {
		rule.configRemoveAroundAll.setValue(false);
		rule.configRemoveAroundRelExpr.setValue(false);
		rule.configRemoveOrParenthesisAnd.setValue(true);
		rule.configRemoveAroundSameOp.setValue(false);
		
		assertTrue(rule.isConfigValueEnabled(rule.configWarning));
		assertConfigRemoveOptionsEnabled();
	}
	
	@Test
	void testConfigWarningShownForSameOp() {
		rule.configRemoveAroundAll.setValue(false);
		rule.configRemoveAroundRelExpr.setValue(false);
		rule.configRemoveOrParenthesisAnd.setValue(false);
		rule.configRemoveAroundSameOp.setValue(true);
		
		assertTrue(rule.isConfigValueEnabled(rule.configWarning));
		assertConfigRemoveOptionsEnabled();
	}
	
	@Test
	void testConfigWarningHidden() {
		rule.configRemoveAroundAll.setValue(true);
		rule.configRemoveAroundRelExpr.setValue(true);
		rule.configRemoveOrParenthesisAnd.setValue(false);
		rule.configRemoveAroundSameOp.setValue(false);

		assertFalse(rule.isConfigValueEnabled(rule.configWarning));
		assertConfigRemoveOptionsEnabled();
	}

	@Test
	void testCommentsInsideExpression() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    CHECK    (     lt_table1  IS INITIAL \" comment");
		buildSrc("*                  comment");
		buildSrc("               AND lt_table2  IS INITIAL");
		buildSrc("             )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");

		buildExp("    CHECK        lt_table1  IS INITIAL \" comment");
		buildExp("*                  comment");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");

		testRule();
	}
	
	@Test
	void testCommentsAroundOpeningParenthesis() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    CHECK    \" comment");
		buildSrc("             (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    ( \" comment");
		buildSrc("                   lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    ( \" comment");
		buildSrc("*                  comment");
		buildSrc("                   lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");

		buildExp("    CHECK    \" comment");
		buildExp("                 lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK    \" comment");
		buildExp("                 lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK    \" comment");
		buildExp("*                  comment");
		buildExp("                 lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");

		testRule();
	}

	@Test
	void testCommentsAroundClosingParenthesis() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL \" comment");
		buildSrc("             )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL \" comment");
		buildSrc("*              comment");
		buildSrc("             )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL ) \" comment");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL ) \" comment");
		buildSrc("*            comment");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("*            comment");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");

		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL \" comment");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL \" comment");
		buildExp("*              comment");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL \" comment");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL \" comment");
		buildExp("*            comment");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("*            comment");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");

		testRule();
	}

	@Test
	void testCommentsAtEnd() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 \" comment");
		buildSrc("             ).");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ) \" comment");
		buildSrc("             .");
		buildSrc("");
		buildSrc("    CHECK    (     lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ). \" comment");

		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0 \" comment");
		buildExp("             .");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0 \" comment");
		buildExp("             .");
		buildExp("");
		buildExp("    CHECK        lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0. \" comment");

		testRule();
	}

	@Test
	void testAsteriskCommentAfterOpeningParens() {
		rule.configRemoveOrParenthesisAnd.setValue(true);

		buildSrc("    CHECK    \" comment 1");
		buildSrc("             (");
		buildSrc("* comment 2");
		buildSrc("                   lt_table1  IS INITIAL");
		buildSrc("               AND lt_table2  IS INITIAL )");
		buildSrc("          OR (     lv_amount1 <= 0");
		buildSrc("               AND lv_amount2 <= 0 ).");

		buildExp("    CHECK    \" comment 1");
		buildExp("* comment 2");
		buildExp("                 lt_table1  IS INITIAL");
		buildExp("             AND lt_table2  IS INITIAL");
		buildExp("          OR     lv_amount1 <= 0");
		buildExp("             AND lv_amount2 <= 0.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
