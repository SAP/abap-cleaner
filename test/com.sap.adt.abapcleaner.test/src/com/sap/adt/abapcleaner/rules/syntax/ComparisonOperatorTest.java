package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ComparisonOperatorTest extends RuleTestBase {
	ComparisonOperatorTest() {
		super(RuleID.COMPARISON_OPERATOR);
	}
	
	@Test
	void testSimpleCases() {
		buildSrc("    IF a EQ b OR c NE d.");
		buildSrc("      IF a LT c AND b GT d.");
		buildSrc("        IF a LE d AND c GE b.");
		buildSrc("          \" do something");
		buildSrc("        ENDIF.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF a = b OR c <> d.");
		buildExp("      IF a < c AND b > d.");
		buildExp("        IF a <= d AND c >= b.");
		buildExp("          \" do something");
		buildExp("        ENDIF.");
		buildExp("      ENDIF.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	
	@Test
	void testMultiLine() {
		// ensure that the indent of the second line is adjusted to the shortened = operator
		
		buildSrc("    IF a EQ b OR a LT 0");
		buildSrc("              OR b GT 0.");
		buildSrc("       \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF a = b OR a < 0");
		buildExp("             OR b > 0.");
		buildExp("       \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
