package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

class IfBlockAtLoopEndTest extends RuleTestBase {
	private IfBlockAtLoopEndRule rule;
	
	IfBlockAtLoopEndTest() {
		super(RuleID.IF_BLOCK_AT_LOOP_END);
		rule = (IfBlockAtLoopEndRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMinLineCount.setValue(10);
		rule.configMinLinePercentage.setValue(50);
		rule.configKeepExceptionLogicInIf.setValue(true);
		rule.configNegationStyle.setEnumValue(NegationStyle.AVOID_INNER_NEGATIONS);
		rule.configConvertAbapFalseAndAbapTrue.setValue(true);
		rule.configEnsureEmptyLineAfterEndIf.setValue(true);
	}
	
	@Test
	void testNestedIfsInNestedLoops() {
		buildSrc("    DATA lv_count TYPE i.");
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    WHILE lv_count > 0.");
		buildSrc("      lv_count -= 1.");
		buildSrc("      IF lv_count MOD 2 = 0.");
		buildSrc("        LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("          \" if the row was already processed, there is nothing to do.");
		buildSrc("          IF ls_row-processed = abap_false.");
		buildSrc("            IF ls_row-another_component = abap_true.");
		buildSrc("              \" if the row was not yet processed, there's so much to do!");
		buildSrc("              lv_value = 1.");
		buildSrc("              lv_value = 2.");
		buildSrc("              lv_value = 3.");
		buildSrc("              lv_value = 4.");
		buildSrc("              lv_value = 5.");
		buildSrc("              lv_value = 6.");
		buildSrc("              lv_value = 7.");
		buildSrc("              lv_value = 8.");
		buildSrc("              lv_value = 9.");
		buildSrc("              ls_row-processed = abap_true.");
		buildSrc("            ENDIF.");
		buildSrc("          ENDIF.");
		buildSrc("        ENDLOOP.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		buildExp("    DATA lv_count TYPE i.");
		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    WHILE lv_count > 0.");
		buildExp("      lv_count -= 1.");
		buildExp("      IF lv_count MOD 2 <> 0.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      LOOP AT its_table INTO DATA(ls_row).");
		buildExp("        \" if the row was already processed, there is nothing to do.");
		buildExp("        IF ls_row-processed = abap_true.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("");
		buildExp("        IF ls_row-another_component = abap_false.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("");
		buildExp("        \" if the row was not yet processed, there's so much to do!");
		buildExp("        lv_value = 1.");
		buildExp("        lv_value = 2.");
		buildExp("        lv_value = 3.");
		buildExp("        lv_value = 4.");
		buildExp("        lv_value = 5.");
		buildExp("        lv_value = 6.");
		buildExp("        lv_value = 7.");
		buildExp("        lv_value = 8.");
		buildExp("        lv_value = 9.");
		buildExp("        ls_row-processed = abap_true.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLongOuterIfAndShortInnerIfs() {
		buildSrc("    DATA lv_count TYPE i.");
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    WHILE lv_count > 0.");
		buildSrc("      lv_count -= 1.");
		buildSrc("      IF lv_count MOD 2 = 0.");
		buildSrc("        LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("          \" if the row was already processed, there is nothing to do.");
		buildSrc("          IF ls_row-processed = abap_false.");
		buildSrc("            \" if the row was not yet processed, there's so much to do!");
		buildSrc("            lv_value = 1.");
		buildSrc("            lv_value = 2.");
		buildSrc("            lv_value = 3.");
		buildSrc("            ls_row-processed = abap_true.");
		buildSrc("          ENDIF.");
		buildSrc("        ENDLOOP.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");
		buildSrc("");
		buildSrc("    \" pretend to be using the variables");
		buildSrc("    ev_value = lv_value.");

		buildExp("    DATA lv_count TYPE i.");
		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    WHILE lv_count > 0.");
		buildExp("      lv_count -= 1.");
		buildExp("      IF lv_count MOD 2 <> 0.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      LOOP AT its_table INTO DATA(ls_row).");
		buildExp("        \" if the row was already processed, there is nothing to do.");
		buildExp("        IF ls_row-processed = abap_false.");
		buildExp("          \" if the row was not yet processed, there's so much to do!");
		buildExp("          lv_value = 1.");
		buildExp("          lv_value = 2.");
		buildExp("          lv_value = 3.");
		buildExp("          ls_row-processed = abap_true.");
		buildExp("        ENDIF.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDWHILE.");
		buildExp("");
		buildExp("    \" pretend to be using the variables");
		buildExp("    ev_value = lv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNestedIfsInShortLoopsUnchanged() {
		buildSrc("    DATA lv_count TYPE i.");
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    WHILE lv_count > 0.");
		buildSrc("      lv_count -= 1.");
		buildSrc("      IF lv_count MOD 2 = 0.");
		buildSrc("        LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("          \" if the row was already processed, there is nothing to do.");
		buildSrc("          IF ls_row-processed = abap_false.");
		buildSrc("            IF ls_row-another_component = abap_true.");
		buildSrc("              ls_row-processed = abap_true.");
		buildSrc("            ENDIF.");
		buildSrc("          ENDIF.");
		buildSrc("        ENDLOOP.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDontConvertAbapBool() {
		// configure '= abap_true' to be converted to '<> abap_true' (rather than '= abap_false')
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);
		rule.configConvertAbapFalseAndAbapTrue.setValue(false);

		buildSrc("    WHILE lv_count > 0.");
		buildSrc("      lv_count -= 1.");
		buildSrc("      IF lv_count MOD 2 = 0.");
		buildSrc("        LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("          IF ls_row-processed = abap_false.");
		buildSrc("            IF ls_row-another_component = abap_true.");
		buildSrc("              lv_value = 1.");
		buildSrc("              lv_value = 2.");
		buildSrc("              lv_value = 3.");
		buildSrc("              ls_row-processed = abap_true.");
		buildSrc("            ENDIF.");
		buildSrc("          ENDIF.");
		buildSrc("        ENDLOOP.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE lv_count > 0.");
		buildExp("      lv_count -= 1.");
		buildExp("      IF lv_count MOD 2 <> 0.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      LOOP AT its_table INTO DATA(ls_row).");
		buildExp("        IF ls_row-processed <> abap_false.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("");
		buildExp("        IF ls_row-another_component <> abap_true.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("");
		buildExp("        lv_value = 1.");
		buildExp("        lv_value = 2.");
		buildExp("        lv_value = 3.");
		buildExp("        ls_row-processed = abap_true.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNoEmptyLineAfterIf() {
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);
		rule.configEnsureEmptyLineAfterEndIf.setValue(false);

		buildSrc("    WHILE lv_count > 0.");
		buildSrc("      lv_count -= 1.");
		buildSrc("      IF lv_count MOD 2 = 0.");
		buildSrc("        LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("          IF ls_row-processed = abap_false.");
		buildSrc("            IF ls_row-another_component = abap_true.");
		buildSrc("              lv_value = 1.");
		buildSrc("              lv_value = 2.");
		buildSrc("              lv_value = 3.");
		buildSrc("              ls_row-processed = abap_true.");
		buildSrc("            ENDIF.");
		buildSrc("          ENDIF.");
		buildSrc("        ENDLOOP.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE lv_count > 0.");
		buildExp("      lv_count -= 1.");
		buildExp("      IF lv_count MOD 2 <> 0.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("      LOOP AT its_table INTO DATA(ls_row).");
		buildExp("        IF ls_row-processed = abap_true.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("        IF ls_row-another_component = abap_false.");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("        lv_value = 1.");
		buildExp("        lv_value = 2.");
		buildExp("        lv_value = 3.");
		buildExp("        ls_row-processed = abap_true.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfBlockWithLowShare() {
		// expect the IF block to NOT be replaced, because its share of all lines in the loop is too low (6 / 13 < 50%)

		rule.configMinLineCount.setValue(1);
		rule.configMinLinePercentage.setValue(50);
		
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("      lv_value = 1.");
		buildSrc("      lv_value = 2.");
		buildSrc("      lv_value = 3.");
		buildSrc("      lv_value = 4.");
		buildSrc("      lv_value = 5.");
		buildSrc("      lv_value = 6.");
		buildSrc("      lv_value = 7.");
		buildSrc("");
		buildSrc("      IF ls_row-processed = abap_false.");
		buildSrc("        lv_value = 8.");
		buildSrc("        lv_value = 9.");
		buildSrc("        ls_row-processed = abap_true.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDLOOP.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepExceptionalLogicInIf() {
		// expect the IF block to NOT be replaced, because it contains a command (RETURN) that indicates exceptional logic

		rule.configMinLineCount.setValue(1);
		rule.configKeepExceptionLogicInIf.setValue(true);
		
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("      lv_value = 1.");
		buildSrc("");
		buildSrc("      IF something_special( ) = abap_true.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("        lv_value = 4.");
		buildSrc("        lv_value = 5.");
		buildSrc("        lv_value = 6.");
		buildSrc("        lv_value = 7.");
		buildSrc("        RETURN.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDLOOP.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testConvertIfWithExceptionalLogic() {
		// configure rule so that the IF block is even replaced if it contains a command (RETURN) that indicates exceptional logic 
		rule.configMinLineCount.setValue(1);
		rule.configKeepExceptionLogicInIf.setValue(false);
		
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    LOOP AT its_table INTO DATA(ls_row).");
		buildSrc("      lv_value = 1.");
		buildSrc("");
		buildSrc("      IF something_special( ) = abap_true.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("        lv_value = 4.");
		buildSrc("        lv_value = 5.");
		buildSrc("        lv_value = 6.");
		buildSrc("        lv_value = 7.");
		buildSrc("        RETURN.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDLOOP.");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    LOOP AT its_table INTO DATA(ls_row).");
		buildExp("      lv_value = 1.");
		buildExp("");
		buildExp("      IF something_special( ) = abap_false.");
		buildExp("        CONTINUE.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      lv_value = 2.");
		buildExp("      lv_value = 3.");
		buildExp("      lv_value = 4.");
		buildExp("      lv_value = 5.");
		buildExp("      lv_value = 6.");
		buildExp("      lv_value = 7.");
		buildExp("      RETURN.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
