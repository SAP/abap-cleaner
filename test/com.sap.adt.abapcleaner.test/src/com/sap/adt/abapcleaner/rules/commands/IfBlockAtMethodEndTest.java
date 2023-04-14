package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

class IfBlockAtMethodEndTest extends RuleTestBase {
	private IfBlockAtMethodEndRule rule;
	
	IfBlockAtMethodEndTest() {
		super(RuleID.IF_BLOCK_AT_METHOD_END);
		rule = (IfBlockAtMethodEndRule)getRule();
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
	void testLongIfBlockAtMethodEnd() {
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    \" if the item was already processed, there is nothing to do.");
		buildSrc("    IF mv_item_processed = abap_false.");
		buildSrc("      \" if the item was not yet processed, there's so much to do!");
		buildSrc("      lv_value = 1.");
		buildSrc("      lv_value = 2.");
		buildSrc("      lv_value = 3.");
		buildSrc("      lv_value = 4.");
		buildSrc("      lv_value = 5.");
		buildSrc("      lv_value = 6.");
		buildSrc("      lv_value = 7.");
		buildSrc("      lv_value = 8.");
		buildSrc("      lv_value = 9.");
		buildSrc("      mv_item_processed = abap_true.");
		buildSrc("    ENDIF.");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    \" if the item was already processed, there is nothing to do.");
		buildExp("    IF mv_item_processed = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    \" if the item was not yet processed, there's so much to do!");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");
		buildExp("    lv_value = 4.");
		buildExp("    lv_value = 5.");
		buildExp("    lv_value = 6.");
		buildExp("    lv_value = 7.");
		buildExp("    lv_value = 8.");
		buildExp("    lv_value = 9.");
		buildExp("    mv_item_processed = abap_true.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLongNestedIfBlocksAtMethodEnd() {
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    \" if the item was already processed, there is nothing to do.");
		buildSrc("    IF mv_item_processed_outer = abap_false.");
		buildSrc("      IF mv_item_processed_inner = abap_false.");
		buildSrc("        \" if the item was not yet processed, there's so much to do!");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("        lv_value = 4.");
		buildSrc("        lv_value = 5.");
		buildSrc("        lv_value = 6.");
		buildSrc("        lv_value = 7.");
		buildSrc("        lv_value = 8.");
		buildSrc("        lv_value = 9.");
		buildSrc("        mv_item_processed_outer = abap_true.");
		buildSrc("        mv_item_processed_inner = abap_true.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    \" if the item was already processed, there is nothing to do.");
		buildExp("    IF mv_item_processed_outer = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF mv_item_processed_inner = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    \" if the item was not yet processed, there's so much to do!");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");
		buildExp("    lv_value = 4.");
		buildExp("    lv_value = 5.");
		buildExp("    lv_value = 6.");
		buildExp("    lv_value = 7.");
		buildExp("    lv_value = 8.");
		buildExp("    lv_value = 9.");
		buildExp("    mv_item_processed_outer = abap_true.");
		buildExp("    mv_item_processed_inner = abap_true.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testShortIfBlockAtMethodEndUnchanged() {
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    \" if the item was already processed, there is nothing to do.");
		buildSrc("    IF mv_item_processed = abap_false.");
		buildSrc("      \" if the item was not yet processed, there's so much to do!");
		buildSrc("      lv_value = 1.");
		buildSrc("      lv_value = 2.");
		buildSrc("      lv_value = 3.");
		buildSrc("      mv_item_processed = abap_true.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDontConvertAbapBool() {
		// configure '= abap_true' to be converted to '<> abap_true' (rather than '= abap_false')
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);
		rule.configConvertAbapFalseAndAbapTrue.setValue(false);

		buildSrc("    IF mv_cond_a = abap_false AND mv_cond_b = abap_true.");
		buildSrc("      IF mv_cond_1 = abap_true OR mv_cond_2 = abap_false.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF mv_cond_a <> abap_false OR mv_cond_b <> abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF mv_cond_1 <> abap_true AND mv_cond_2 <> abap_false.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOuterNegation() {
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);

		buildSrc("    IF a = 1 AND b = 2.");
		buildSrc("      IF c IS INITIAL OR d IS INITIAL.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF NOT ( a = 1 AND b = 2 ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF NOT ( c IS INITIAL OR d IS INITIAL ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testInnerNegation() {
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);

		buildSrc("    IF a <> 1 AND b IS NOT INITIAL.");
		buildSrc("      IF c <> 3 OR <d> IS NOT ASSIGNED.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF a = 1 OR b IS INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF c = 3 AND <d> IS ASSIGNED.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testForceOuterNegation() {
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);
		rule.configNegationStyle.setEnumValue(NegationStyle.ALWAYS_WITH_AND_OR);

		buildSrc("    IF a <> 1 AND b IS NOT INITIAL.");
		buildSrc("      IF c <> 3 OR <d> IS NOT ASSIGNED.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF NOT ( a <> 1 AND b IS NOT INITIAL ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF NOT ( c <> 3 OR <d> IS NOT ASSIGNED ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testForceInnerNegation() {
		rule.configMinLineCount.setValue(3);
		rule.configMinLinePercentage.setValue(20);
		rule.configNegationStyle.setEnumValue(NegationStyle.NEVER);

		buildSrc("    IF a = 1 AND b = 2.");
		buildSrc("      IF c IS INITIAL OR d IS INITIAL.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF a <> 1 OR b <> 2.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF c IS NOT INITIAL AND d IS NOT INITIAL.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNoEmptyLineAfterIf() {
		rule.configMinLineCount.setValue(3);
		rule.configEnsureEmptyLineAfterEndIf.setValue(false);

		buildSrc("    IF mv_item_processed_outer = abap_false.");
		buildSrc("      IF mv_item_processed_inner = abap_false.");
		buildSrc("        lv_value = 1.");
		buildSrc("        lv_value = 2.");
		buildSrc("        lv_value = 3.");
		buildSrc("        mv_item_processed_outer = abap_true.");
		buildSrc("        mv_item_processed_inner = abap_true.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF mv_item_processed_outer = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    IF mv_item_processed_inner = abap_true.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");
		buildExp("    mv_item_processed_outer = abap_true.");
		buildExp("    mv_item_processed_inner = abap_true.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIfBlockWithLowShare() {
		// expect the IF block to NOT be replaced, because its share of all lines in the method is too low (< 50%)

		rule.configMinLineCount.setValue(1);
		rule.configMinLinePercentage.setValue(50);
		
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    lv_value = 1.");
		buildSrc("    lv_value = 2.");
		buildSrc("    lv_value = 3.");
		buildSrc("    lv_value = 4.");
		buildSrc("    lv_value = 5.");
		buildSrc("");
		buildSrc("    IF mv_item_processed = abap_false.");
		buildSrc("      lv_value = 6.");
		buildSrc("      lv_value = 7.");
		buildSrc("      mv_item_processed = abap_true.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepExceptionalLogicInIf() {
		// expect the IF block to NOT be replaced, because it contains a command (RAISE EXCEPTION) that indicates exceptional logic

		rule.configMinLineCount.setValue(1);
		rule.configKeepExceptionLogicInIf.setValue(true);

		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    IF something_wrong( ) = abap_true.");
		buildSrc("      lv_value = 1.");
		buildSrc("      lv_value = 2.");
		buildSrc("      lv_value = 3.");
		buildSrc("      RAISE EXCEPTION NEW cx_assert( ).");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testConvertIfWithExceptionalLogic() {
		// configure rule so that the IF block is even replaced if it contains a command (RAISE EXCEPTION) that indicates exceptional logic 
		rule.configMinLineCount.setValue(1);
		rule.configKeepExceptionLogicInIf.setValue(false);

		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    IF something_wrong( ) = abap_true.");
		buildSrc("      lv_value = 1.");
		buildSrc("      lv_value = 2.");
		buildSrc("      lv_value = 3.");
		buildSrc("      RAISE EXCEPTION NEW cx_assert( ).");
		buildSrc("    ENDIF.");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    IF something_wrong( ) = abap_false.");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    lv_value = 1.");
		buildExp("    lv_value = 2.");
		buildExp("    lv_value = 3.");
		buildExp("    RAISE EXCEPTION NEW cx_assert( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
