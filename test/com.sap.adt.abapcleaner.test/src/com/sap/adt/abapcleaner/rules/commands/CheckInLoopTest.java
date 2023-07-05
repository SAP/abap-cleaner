package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

class CheckInLoopTest extends RuleTestBase {
	private CheckInLoopRule rule;
	
	CheckInLoopTest() {
		super(RuleID.CHECK_IN_LOOP);
		rule = (CheckInLoopRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configKeepCondition.setEnumValue(KeepCheckInLoopCondition.NEVER);
		rule.configNegationStyle.setEnumValue(NegationStyle.AVOID_INNER_NEGATIONS);
		rule.configConvertAbapFalseAndAbapTrue.setValue(true);
	}
	
	@Test
	void testCheckAtDoStart() {
		buildSrc("    DO 5 TIMES.");
		buildSrc("      CHECK its_table IS NOT INITIAL.");
		buildSrc("      lv_counter += 1.");
		buildSrc("    ENDDO.");

		buildExp("    DO 5 TIMES.");
		buildExp("      IF its_table IS INITIAL.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      lv_counter += 1.");
		buildExp("    ENDDO.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testReplaceChecksAtLoopStart() {
		rule.configKeepCondition.setEnumValue(KeepCheckInLoopCondition.NEVER);

		buildSrc("    LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("      \" the following CHECKs are considered to be at loop start (despite this comment)");
		buildSrc("      CHECK ls_row-min_id <> 0.");
		buildSrc("      CHECK ls_row-processed = abap_false.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT its_table INTO DATA(ls_row).");
		buildExp("      \" the following CHECKs are considered to be at loop start (despite this comment)");
		buildExp("      IF ls_row-min_id = 0.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      IF ls_row-processed = abap_true.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepChecksAtLoopStart() {
		rule.configKeepCondition.setEnumValue(KeepCheckInLoopCondition.KEEP_AT_LOOP_START);

		buildSrc("    LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("      \" the following CHECKs are considered to be at loop start (despite this comment)");
		buildSrc("      CHECK ls_row-min_id <> 0.");
		buildSrc("      CHECK ls_row-processed = abap_false.");
		buildSrc("    ENDLOOP.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testReplaceChecksInNestedLoops() {
		rule.configKeepCondition.setEnumValue(KeepCheckInLoopCondition.NEVER);

		buildSrc("    DO 5 TIMES.");
		buildSrc("      CHECK its_table IS NOT INITIAL.");
		buildSrc("");
		buildSrc("      LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("        CHECK ls_row-min_id <> 0.");
		buildSrc("      ENDLOOP.");
		buildSrc("");
		buildSrc("    ENDDO.");

		buildExp("    DO 5 TIMES.");
		buildExp("      IF its_table IS INITIAL.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      LOOP AT its_table INTO DATA(ls_row).");
		buildExp("        IF ls_row-min_id = 0.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("      ENDLOOP.");
		buildExp("");
		buildExp("    ENDDO.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepChecksInNestedLoops() {
		rule.configKeepCondition.setEnumValue(KeepCheckInLoopCondition.KEEP_AT_LOOP_START);

		buildSrc("    DO 5 TIMES.");
		buildSrc("      CHECK its_table IS NOT INITIAL.");
		buildSrc("");
		buildSrc("      LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("        CHECK ls_row-min_id <> 0.");
		buildSrc("      ENDLOOP.");
		buildSrc("");
		buildSrc("    ENDDO.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChecksInsideWhile() {
		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("");
		buildSrc("      \" these CHECKs are obviously NOT at loop start:");
		buildSrc("      CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL ).");
		buildSrc("");
		buildSrc("      CHECK a BETWEEN b AND c ");
		buildSrc("        AND d BETWEEN e AND f.");
		buildSrc("");
		buildSrc("      \" do something very important");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("");
		buildExp("      \" these CHECKs are obviously NOT at loop start:");
		buildExp("      IF ( c IS SUPPLIED AND b IS NOT INITIAL ) OR ( d IS NOT SUPPLIED AND b IS INITIAL ).");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      IF NOT (     a BETWEEN b AND c");
		buildExp("               AND d BETWEEN e AND f ).");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      \" do something very important");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckBetweenOpInsideWhile() {
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("");
		buildSrc("      \" this CHECK is obviously NOT at loop start:");
		buildSrc("      CHECK a BETWEEN b AND c ");
		buildSrc("        AND d BETWEEN e AND f.");
		buildSrc("");
		buildSrc("      \" do something very important");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("");
		buildExp("      \" this CHECK is obviously NOT at loop start:");
		buildExp("      IF    a NOT BETWEEN b AND c");
		buildExp("         OR d NOT BETWEEN e AND f.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      \" do something very important");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckBetweenOpOuterNot() {
		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("");
		buildSrc("      \" this CHECK is obviously NOT at loop start:");
		buildSrc("      CHECK a BETWEEN b AND c ");
		buildSrc("        AND d BETWEEN e AND f.");
		buildSrc("");
		buildSrc("      \" do something very important");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("");
		buildExp("      \" this CHECK is obviously NOT at loop start:");
		buildExp("      IF NOT (     a BETWEEN b AND c");
		buildExp("               AND d BETWEEN e AND f ).");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      \" do something very important");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckTableExprInsideWhile() {
		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("");
		buildSrc("      \" this CHECK is obviously NOT at loop start:");
		buildSrc("      CHECK line_exists( its_table[ 0 ] ) AND its_table[ 0 ]-processed = abap_true");
		buildSrc("        OR lines( its_table ) > 2  AND line_exists( its_table[ 1 ] ).");
		buildSrc("");
		buildSrc("      \" do something very important");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("");
		buildExp("      \" this CHECK is obviously NOT at loop start:");
		buildExp("      IF NOT (    line_exists( its_table[ 0 ] ) AND its_table[ 0 ]-processed = abap_true");
		buildExp("               OR lines( its_table ) > 2 AND line_exists( its_table[ 1 ] ) ).");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      \" do something very important");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testConvertAbapBool() {
		// expect '= abap_true' to be negated to '= abap_false' and vice versa, 
		// assuming that abap_undefined can NOT occur
		
		rule.configConvertAbapFalseAndAbapTrue.setValue(true);
		
		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("      CHECK its_table[ 0 ]-processed = abap_true");
		buildSrc("         OR its_table[ 1 ]-processed = abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("      IF     its_table[ 0 ]-processed = abap_false");
		buildExp("         AND its_table[ 1 ]-processed = abap_true.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      \" do something");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDontConvertAbapBool() {
		// expect '= abap_true' to be negated to '<> abap_true' (rather than '= abap_false') and vice versa
		// assuming that abap_undefined may occur

		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);
		rule.configConvertAbapFalseAndAbapTrue.setValue(false);
		
		buildSrc("    WHILE lv_id < ls_row-max_id.");
		buildSrc("      lv_id += 1.");
		buildSrc("      CHECK its_table[ 0 ]-processed = abap_true");
		buildSrc("         OR its_table[ 1 ]-processed = abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");
		
		buildExp("    WHILE lv_id < ls_row-max_id.");
		buildExp("      lv_id += 1.");
		buildExp("      IF     its_table[ 0 ]-processed <> abap_true");
		buildExp("         AND its_table[ 1 ]-processed <> abap_false.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      \" do something");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainsUnchanged() {
		buildSrc("    \" chains are currently ignored:");
		buildSrc("    DO 10 TIMES.");
		buildSrc("      CHECK: its_table_1 IS INITIAL,");
		buildSrc("             its_table_2 IS INITIAL.");
		buildSrc("    ENDDO.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckOutsideLoopUnchanged() {
		buildSrc("    \" outside the scope of this rule:");
		buildSrc("    CHECK its_input_table IS NOT INITIAL.");
		buildSrc("");
		buildSrc("    DO 5 TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckAtWhileStart() {
		// check negation if a closing parenthesis must be inserted before an existing closing parenthesis

		buildSrc("    WHILE a < 5.");
		buildSrc("      CHECK a > 1 AND ( b >= 10 OR b >= 5 AND c <= 5 ).");
		buildSrc("      a += 1.");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE a < 5.");
		buildExp("      IF a <= 1 OR ( b < 10 AND ( b < 5 OR c > 5 ) ).");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      a += 1.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testForceInnerNegation() {
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    WHILE a < 5.");
		buildSrc("      CHECK a = 1 AND b = 3 AND c = 5.");
		buildSrc("      a += 1.");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE a < 5.");
		buildExp("      IF a <> 1 OR b <> 3 OR c <> 5.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      a += 1.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIdentifierUnchanged() {
		// expect 'check' to be categorized as an identifier and to remain unchanged 
		buildSrc("    WHILE a < 5.");
		buildSrc("      check = NEW #( ).");
		buildSrc("      a += 1.");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckInSelectEndSelect() {
		// ensure SELECT ... ENDSELECT is correctly recognized as a loop 

		buildSrc("    SELECT * FROM any_table");
		buildSrc("      WHERE comp = '1'");
		buildSrc("      INTO @DATA(ls_data).");
		buildSrc("");
		buildSrc("      CHECK ls_data-comp2 IS INITIAL.");
		buildSrc("");
		buildSrc("      \" do something");
		buildSrc("    ENDSELECT.");

		buildExp("    SELECT * FROM any_table");
		buildExp("      WHERE comp = '1'");
		buildExp("      INTO @DATA(ls_data).");
		buildExp("");
		buildExp("      IF ls_data-comp2 IS NOT INITIAL.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      \" do something");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
