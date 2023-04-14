package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EqualsSignChainTest extends RuleTestBase {
	private EqualsSignChainRule rule;
	
	EqualsSignChainTest() {
		super(RuleID.EQUALS_SIGN_CHAIN);
		rule = (EqualsSignChainRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRepeatIntegerLiterals.setValue(true);
		rule.configRepeatStringLiterals.setValue(true);
		rule.configRepeatSimpleIdentifiers.setValue(true);
	}
	
	@Test
	void testRepeatNumberLiteral() {
		rule.configRepeatIntegerLiterals.setValue(true);

		buildSrc("    \" assign a number literal");
		buildSrc("    a = b = 42. \" plain and simple");

		buildExp("    \" assign a number literal");
		buildExp("    b = 42. \" plain and simple");
		buildExp("    a = 42.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDontRepeatNumberLiteral() {
		rule.configRepeatIntegerLiterals.setValue(false);

		buildSrc("    a = b = 42.");

		buildExp("    b = 42.");
		buildExp("    a = b.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRepeatStringLiteral() {
		rule.configRepeatStringLiterals.setValue(true);

		buildSrc("    c = d = e = 'abc'. \" still quite simple");

		buildExp("    e = 'abc'. \" still quite simple");
		buildExp("    d = 'abc'.");
		buildExp("    c = 'abc'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDontRepeatStringLiteral() {
		rule.configRepeatStringLiterals.setValue(false);

		buildSrc("    c = d = e = 'abc'. \" still quite simple");

		buildExp("    e = 'abc'. \" still quite simple");
		buildExp("    d = e.");
		buildExp("    c = d.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRepeatSimpleIdentifier() {
		rule.configRepeatSimpleIdentifiers.setValue(true);

		buildSrc("    \" assign a simple variable");
		buildSrc("    f = g = h = iv_value.");

		buildExp("    \" assign a simple variable");
		buildExp("    h = iv_value.");
		buildExp("    g = iv_value.");
		buildExp("    f = iv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDontRepeatSimpleIdentifier() {
		rule.configRepeatSimpleIdentifiers.setValue(false);

		buildSrc("    \" assign a simple variable");
		buildSrc("    f = g = h = iv_value.");

		buildExp("    \" assign a simple variable");
		buildExp("    h = iv_value.");
		buildExp("    g = h.");
		buildExp("    f = g.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testFunctionalCall() {
		// ensure that functional method calls are never repeated, because they could have side effects
		
		rule.configRepeatSimpleIdentifiers.setValue(true);

		buildSrc("    f = g = h = next_value( ).");

		buildExp("    h = next_value( ).");
		buildExp("    g = h.");
		buildExp("    f = g.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexExpression() {
		buildSrc("    \" assign a complex expression that should not be repeated");
		buildSrc("    i = j = k = l = m = complex_expression( param_1 = 5");
		buildSrc("                                            param_2 = 'abc' \" comment on param_2");
		buildSrc("                                            param_3 = VALUE #( param_4 = 42  \" comment on param_4");
		buildSrc("                                                               param_5 = iv_param_5 ) ). \" final comment");

		buildExp("    \" assign a complex expression that should not be repeated");
		buildExp("    m = complex_expression( param_1 = 5");
		buildExp("                            param_2 = 'abc' \" comment on param_2");
		buildExp("                            param_3 = VALUE #( param_4 = 42  \" comment on param_4");
		buildExp("                                               param_5 = iv_param_5 ) ). \" final comment");
		buildExp("    l = m.");
		buildExp("    k = l.");
		buildExp("    j = k.");
		buildExp("    i = j.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		// expect commands with chain colon to remain unchanged
		
		buildSrc("    a = b = : next_value( ), next_value( ).");
		buildSrc("    c = d = : 1.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testThreeVariables() {
		rule.configRepeatSimpleIdentifiers.setValue(false);

		buildSrc("    a = b = c.");

		buildExp("    b = c.");
		buildExp("    a = b.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
