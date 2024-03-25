package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class CondenseTest extends RuleTestBase {
	private CondenseRule rule;
	
	CondenseTest() {
		super(RuleID.CONDENSE);
		rule = (CondenseRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configSpecifyValName.setValue(false);
	   rule.configSpecifyDel.setValue(false);
	   rule.configSpecifyFromForNoGaps.setValue(true);
		rule.configKeepParamsOnOneLine.setValue(false);
	}

	@Test
	void testSpecifyValName() {
		rule.configSpecifyValName.setValue(true);

		buildSrc("    \" comment");
		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    \" comment");
		buildExp("    lv_text_a = condense( val = lv_text_a ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyFromForNoGaps() {
		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( lv_text_a ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSpecifyFromForNoGaps() {
		rule.configSpecifyFromForNoGaps.setValue(false);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( lv_text_a ).");
		buildExp("    lv_text_b = condense( val = lv_text_b");
		buildExp("                          to  = `` ).");
		buildExp("    lv_text_c+5(7) = condense( lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyDel() {
		rule.configSpecifyDel.setValue(true);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a");
		buildExp("                          del = ` ` ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7)");
		buildExp("                               del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyDelButNotFrom() {
		rule.configSpecifyDel.setValue(true);
		rule.configSpecifyFromForNoGaps.setValue(false);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a");
		buildExp("                          del = ` ` ).");
		buildExp("    lv_text_b = condense( val = lv_text_b");
		buildExp("                          to  = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7)");
		buildExp("                               del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentsAndPragmasUnchanged() {
		buildSrc("    CONDENSE \" comment");
		buildSrc("             lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS ##ANY_PRAGMA.");
		buildSrc("    CONDENSE");
		buildSrc("* comment line");
		buildSrc("             lv_text_c+5(7).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParamsOnOneLine() {
		rule.configSpecifyDel.setValue(true);
		rule.configKeepParamsOnOneLine.setValue(true);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a del = ` ` ).");
		buildExp("    lv_text_b = condense( val = lv_text_b from = ` ` to = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7) del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}	