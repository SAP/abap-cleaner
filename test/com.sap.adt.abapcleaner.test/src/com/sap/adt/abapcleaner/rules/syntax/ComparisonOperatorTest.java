package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ComparisonOperatorTest extends RuleTestBase {
	private ComparisonOperatorRule rule;
	
	ComparisonOperatorTest() {
		super(RuleID.COMPARISON_OPERATOR);
		rule = (ComparisonOperatorRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configPreferredOperatorSet.setEnumValue(ComparisonOperatorType.SYMBOLIC);
		rule.configReplaceRegularOperators.setValue(true);
		rule.configReplaceObsoleteOperators.setValue(true);
	}
	
	@Test
	void testPreferSymbolicSimpleCases() {
		buildSrc("    IF a EQ b OR c NE d.");
		buildSrc("      IF a < c AND b > d");
		buildSrc("               AND b < e.");
		buildSrc("        IF a LE d AND c GE b.");
		buildSrc("          result = xsdbool( a <= d OR a GE b ).");
		buildSrc("        ENDIF.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF a = b OR c <> d.");
		buildExp("      IF a < c AND b > d");
		buildExp("               AND b < e.");
		buildExp("        IF a <= d AND c >= b.");
		buildExp("          result = xsdbool( a <= d OR a >= b ).");
		buildExp("        ENDIF.");
		buildExp("      ENDIF.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPreferTextualSimpleCases() {
		rule.configPreferredOperatorSet.setEnumValue(ComparisonOperatorType.TEXTUAL);

		buildSrc("    IF a EQ b OR c NE d.");
		buildSrc("      IF a < c AND b > d");
		buildSrc("               AND b < e.");
		buildSrc("        IF a LE d AND c GE b.");
		buildSrc("          result = xsdbool( a <= d OR a GE b ).");
		buildSrc("        ENDIF.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		buildExp("    IF a EQ b OR c NE d.");
		buildExp("      IF a LT c AND b GT d");
		buildExp("                AND b LT e.");
		buildExp("        IF a LE d AND c GE b.");
		buildExp("          result = xsdbool( a LE d OR a GE b ).");
		buildExp("        ENDIF.");
		buildExp("      ENDIF.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPreferSymbolicMultiLine() {
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
	
	@Test
	void testSelectOptionsOptionKept() {
		// ensure that in 'SELECT-OPTIONS selcrit FOR dobj DEFAULT val1 [TO val2] [OPTION opt] ....', 'opt' is NOT changed,  
		// because '... OPTION <>.' would be a syntax error ('opt' can be EQ, NE, GE, GT, LE, LT, CP, or NP)
		
		buildSrc("REPORT any_report.");
		buildSrc("  SELECT-OPTIONS s_any FOR dtab-field DEFAULT 'abc' OPTION NE.");

		copyExpFromSrc();
		
		testRule();
	}


	@Test
	void testObsoleteToSymbolic() {
		buildSrc("FORM any_form.");
		buildSrc("  IF a >< b AND b => c OR c =< d.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDFORM.");

		buildExp("FORM any_form.");
		buildExp("  IF a <> b AND b >= c OR c <= d.");
		buildExp("    RETURN.");
		buildExp("  ENDIF.");
		buildExp("ENDFORM.");

		testRule();
	}


	@Test
	void testObsoleteToTextual() {
		rule.configPreferredOperatorSet.setEnumValue(ComparisonOperatorType.TEXTUAL);

		buildSrc("FORM any_form.");
		buildSrc("  IF a >< b AND b => c OR c =< d.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDFORM.");

		buildExp("FORM any_form.");
		buildExp("  IF a NE b AND b GE c OR c LE d.");
		buildExp("    RETURN.");
		buildExp("  ENDIF.");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testKeepRegularOperators() {
		rule.configReplaceRegularOperators.setValue(false);

		buildSrc("    IF a EQ b OR c NE d.");
		buildSrc("      IF a < c AND b > d");
		buildSrc("               AND b < e.");
		buildSrc("        IF a LE d AND c GE b.");
		buildSrc("          result = xsdbool( a <= d OR a GE b ).");
		buildSrc("        ENDIF.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepObsoleteOperators() {
		rule.configReplaceObsoleteOperators.setValue(false);

		buildSrc("FORM any_form.");
		buildSrc("  IF a >< b AND b => c OR c =< d.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDFORM.");

		copyExpFromSrc();

		testRule();
	}
}
