package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

class CheckOutsideLoopTest extends RuleTestBase {
	private CheckOutsideLoopRule rule;
	
	CheckOutsideLoopTest() {
		super(RuleID.CHECK_OUTSIDE_LOOP);
		rule = (CheckOutsideLoopRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS);
		rule.configNegationStyle.setEnumValue(NegationStyle.AVOID_INNER_NEGATIONS);
		rule.configConvertAbapFalseAndAbapTrue.setValue(true);
		rule.configAllowCheckAfterCheckpoints.setValue(true);
	}
	
	@Test
	void testCheckAtMethodStartUnchanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START);
		
		buildSrc("    \" CHECKs at earliest possible position");
		buildSrc("    CHECK its_table IS NOT INITIAL.");
		buildSrc("");
		buildSrc("    CHECK is_struc-item_id    IS NOT INITIAL");
		buildSrc("      AND is_struc-any_flag    = abap_false");
		buildSrc("      AND is_struc-other_flag  = abap_false");
		buildSrc("      AND is_struc-third_flag  = abap_true.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterCheckPointsKept() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START);
		rule.configAllowCheckAfterCheckpoints.setValue(true);
		
		buildSrc("    ASSERT its_table IS NOT INITIAL.");
		buildSrc("    LOG-POINT ID any_id.");
		buildSrc("    BREAK-POINT.");
		buildSrc("    CHECK its_table IS NOT INITIAL.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterLogPointChanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START);
		rule.configAllowCheckAfterCheckpoints.setValue(false);
		
		buildSrc("    LOG-POINT ID any_id.");
		buildSrc("    CHECK its_table IS NOT INITIAL.");

		buildExp("    LOG-POINT ID any_id.");
		buildExp("    IF its_table IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterAssertChanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START);
		rule.configAllowCheckAfterCheckpoints.setValue(false);
		
		buildSrc("    ASSERT iv_value > 0.");
		buildSrc("    CHECK its_table IS NOT INITIAL.");

		buildExp("    ASSERT iv_value > 0.");
		buildExp("    IF its_table IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterDeclarationsKept() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS);

		buildSrc("    DATA: lv_some_value    TYPE i,");
		buildSrc("          lv_another_value TYPE string.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("");
		buildSrc("    \" CHECKs only preceded by declarations");
		buildSrc("    CHECK a = abap_false AND b > 3 OR a = abap_true AND b <= 10.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterDeclarationsChanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START);

		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("");
		buildSrc("    CHECK a = abap_false AND b > 3 OR a = abap_true AND b <= 10.");

		buildExp("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildExp("");
		buildExp("    IF ( a = abap_true OR b <= 3 ) AND ( a = abap_false OR b > 10 ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterClearKept() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS_AND_CLEAR);

		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("    CLEAR ev_success.");
		buildSrc("");
		buildSrc("    \" CHECKs only preceded by declarations and CLEAR");
		buildSrc("    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterClearAndCheckpointsKept() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS_AND_CLEAR);

		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("    CLEAR ev_success.");
		buildSrc("");
		buildSrc("    BREAK-POINT.");
		buildSrc("    LOG-POINT ID any_id.");
		buildSrc("    ASSERT its_table IS NOT INITIAL.");
		buildSrc("");
		buildSrc("    \" CHECKs only preceded by declarations and CLEAR");
		buildSrc("    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterClearChanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS);

		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("    CLEAR ev_success.");
		buildSrc("");
		buildSrc("    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).");

		buildExp("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildExp("    CLEAR ev_success.");
		buildExp("");
		buildExp("    IF ( c IS SUPPLIED AND b IS NOT INITIAL ) OR ( d IS NOT SUPPLIED AND b IS INITIAL ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckAfterBreakPointChanged() {
		rule.configKeepCondition.setEnumValue(KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS);
		rule.configAllowCheckAfterCheckpoints.setValue(false);
		
		buildSrc("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildSrc("");
		buildSrc("    BREAK-POINT ID any_id.");
		buildSrc("    CHECK b IS NOT INITIAL.");

		buildExp("    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table.");
		buildExp("");
		buildExp("    BREAK-POINT ID any_id.");
		buildExp("    IF b IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testForceInnerNot() {
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    \" CHECKs inside the method");
		buildSrc("    CHECK line_exists( its_table[ 0 ] )");
		buildSrc("       OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ).");
		buildSrc("");
		buildSrc("    CHECK a = 5 AND b > 3 EQUIV c IS NOT SUPPLIED OR b IS INITIAL.");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    \" CHECKs inside the method");
		buildExp("    IF     NOT line_exists( its_table[ 0 ] )");
		buildExp("       AND ( lines( its_table ) <= 2 OR NOT line_exists( its_table[ 1 ] ) ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF a = 5 AND b > 3 NOT EQUIV c IS NOT SUPPLIED OR b IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAllowOuterNot() {
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    \" CHECKs inside the method");
		buildSrc("    CHECK line_exists( its_table[ 0 ] )");
		buildSrc("       OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ).");
		buildSrc("");
		buildSrc("    CHECK a = 5 AND b > 3 EQUIV c IS NOT SUPPLIED OR b IS INITIAL.");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    \" CHECKs inside the method");
		buildExp("    IF NOT (    line_exists( its_table[ 0 ] )");
		buildExp("             OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ) ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF a = 5 AND b > 3 NOT EQUIV c IS NOT SUPPLIED OR b IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNegatingInOperator() {
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    \" CHECKs with IN or NOT IN");
		buildSrc("    CHECK ls_result-component IN rt_table.");
		buildSrc("    CHECK lr_range IS INITIAL OR <gs_data>-component NOT IN lr_range.");
		buildSrc("    CHECK lo_instance->mv_num(2) IN cl_any_class=>gt_any_table.");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    \" CHECKs with IN or NOT IN");
		buildExp("    IF ls_result-component NOT IN rt_table.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF lr_range IS NOT INITIAL AND <gs_data>-component IN lr_range.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF lo_instance->mv_num(2) NOT IN cl_any_class=>gt_any_table.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNegatingBetweenOperator() {
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    \" CHECKs with BETWEEN or NOT BETWEEN");
		buildSrc("    CHECK ls_result-component BETWEEN rv_min AND rv_max.");
		buildSrc("    CHECK lr_range IS INITIAL OR <gs_data>-component NOT BETWEEN lv_min AND lv_max.");
		buildSrc("    CHECK lo_data->mv_num(2) BETWEEN cl_any_class=>get( )->get_max( iv_param = 1 ) AND cl_any_class=>get( )->get_max( iv_param = 2 ).");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    \" CHECKs with BETWEEN or NOT BETWEEN");
		buildExp("    IF ls_result-component NOT BETWEEN rv_min AND rv_max.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF lr_range IS NOT INITIAL AND <gs_data>-component BETWEEN lv_min AND lv_max.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF lo_data->mv_num(2) NOT BETWEEN cl_any_class=>get( )->get_max( iv_param = 1 ) AND cl_any_class=>get( )->get_max( iv_param = 2 ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNegatingNot() {
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    CHECK a = b NOT EQUIV c = d.");
		buildSrc("    CHECK NOT line_exists( its_table[ 1 ] ).");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    IF a = b EQUIV c = d.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF line_exists( its_table[ 1 ] ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAddingParenthesesForOpPriority() {
		// test operator priority: AND binds stronger than OR, therefore parentheses must be introduced
		// for the three main parts of the logical expression when negating it

		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    CHECK        ( a = abap_true OR b > 3 )");
		buildSrc("             AND ( a = abap_false OR b <= 10 )");
		buildSrc("          OR     ( c IS NOT SUPPLIED OR b IS INITIAL )");
		buildSrc("             AND ( d IS SUPPLIED OR b IS NOT INITIAL )");
		buildSrc("             AND line_exists( its_table[ 0 ] )");
		buildSrc("          OR     lines( its_table ) > 2");
		buildSrc("             AND line_exists( its_table[ 1 ] ).");

		buildExp("    lv_value = 1.");
		buildExp("");
		buildExp("    IF     (    ( a  = abap_false   AND b <= 3 )");
		buildExp("             OR ( a  = abap_true    AND b  > 10 ) )");
		buildExp("       AND (    ( c IS SUPPLIED     AND b IS NOT INITIAL )");
		buildExp("             OR ( d IS NOT SUPPLIED AND b IS INITIAL )");
		buildExp("             OR NOT line_exists( its_table[ 0 ] ) )");
		buildExp("       AND (        lines( its_table ) <= 2");
		buildExp("             OR NOT line_exists( its_table[ 1 ] ) ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testConvertAbapBool() {
		// expect '= abap_true' to be negated to '= abap_false' and vice versa, 
		// assuming that abap_undefined can NOT occur
		
		rule.configConvertAbapFalseAndAbapTrue.setValue(true);

		buildSrc("    lv_value = 1.");
		buildSrc("    CHECK a = abap_true AND b > 3");
		buildSrc("       OR a = abap_false AND b <= 10.");

		buildExp("    lv_value = 1.");
		buildExp("    IF     ( a = abap_false OR b <= 3 )");
		buildExp("       AND ( a = abap_true  OR b  > 10 ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDontConvertAbapBool() {
		// expect '= abap_true' to be negated to '<> abap_true' (rather than '= abap_false') and vice versa
		// assuming that abap_undefined may occur
		
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);
		rule.configConvertAbapFalseAndAbapTrue.setValue(false);

		buildSrc("    lv_value = 1.");
		buildSrc("    CHECK a = abap_true AND b > 3");
		buildSrc("       OR a = abap_false AND b <= 10.");

		buildExp("    lv_value = 1.");
		buildExp("    IF     ( a <> abap_true  OR b <= 3 )");
		buildExp("       AND ( a <> abap_false OR b  > 10 ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testChainsUnchanged() {
		// as of now, expect chains to be ignored 
		
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    CHECK: its_table_1 IS INITIAL,");
		buildSrc("           its_table_2 IS INITIAL.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCodeWithoutMethodContextUnchanged() {
		// Ensure the code is NOT changed if it is unclear whether or not the CHECK statement is inside a LOOP,
		// (because only a code snippet with no method context is processed).
		// Otherwise, the user may clean just the loop body (but not the LOOP statement), and a CHECK could then  
		// erroneously be changed into IF ... RETURN.
		
		buildSrc("    CHECK its_table_1 IS NOT INITIAL.");
		buildSrc("");
		buildSrc("    lv_value = 1.");
		buildSrc("");
		buildSrc("    CHECK its_table_2 IS INITIAL.");

		copyExpFromSrc();

		// do NOT put a method around this snippet
		
		testRule();
	}
	
	@Test
	void testNegateLogicalExpressionWithComment() {
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    a = 1.");
		buildSrc("    CHECK a = b AND a = c \" comment 1");
		buildSrc("       OR a = d AND a = e");
		buildSrc("* comment 2");
		buildSrc("* comment 3");
		buildSrc("       OR a = f.");

		buildExp("    a = 1.");
		buildExp("    IF     ( a <> b OR a <> c ) \" comment 1");
		buildExp("       AND ( a <> d OR a <> e )");
		buildExp("* comment 2");
		buildExp("* comment 3");
		buildExp("       AND a <> f.");
 		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testOuterNotForLogicalExpressionWithComment() {
		buildSrc("    a = 1.");
		buildSrc("    CHECK a = b AND a = c \" comment 1");
		buildSrc("       OR a = d AND a = e");
		buildSrc("* comment 2");
		buildSrc("* comment 3");
		buildSrc("       OR a = f.");

		buildExp("    a = 1.");
		buildExp("    IF NOT (    a = b AND a = c \" comment 1");
		buildExp("             OR a = d AND a = e");
		buildExp("* comment 2");
		buildExp("* comment 3");
		buildExp("             OR a = f ).");
 		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIdentifierUnchanged() {
		// expect 'check' to be categorized as an identifier and to remain unchanged 
		buildSrc("    a = 1.");
		buildSrc("    check = NEW #( ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckInSelectEndSelect() {
		// ensure SELECT ... ENDSELECT is correctly recognized as a loop (and therefore NOT processed by this rule)

		buildSrc("    SELECT * FROM any_table");
		buildSrc("      WHERE comp = '1'");
		buildSrc("      INTO @DATA(ls_data).");
		buildSrc("");
		buildSrc("      CHECK ls_data-comp2 IS INITIAL.");
		buildSrc("");
		buildSrc("      \" do something");
		buildSrc("    ENDSELECT.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
