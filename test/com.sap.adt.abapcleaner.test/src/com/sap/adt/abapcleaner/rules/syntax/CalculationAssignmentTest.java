package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class CalculationAssignmentTest extends RuleTestBase {
	private CalculationAssignmentRule rule;
	
	CalculationAssignmentTest() {
		super(RuleID.CALCULATION_ASSIGNMENT);
		rule = (CalculationAssignmentRule)getRule();
	}
	
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configConvertMinusToMultiplication.setValue(true);
	   rule.configAllowMinusBeforeMultOrDiv.setValue(true);
	   rule.configAllowVariableAtEnd.setValue(true);
	}
	
	@Test
	void testOldAbapRelease() {
		// ensure that calculation assignment operators are not introduced if the code must compile against an ABAP Release prior to 7.54, 
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-754-assignments.htm
		
		setAbapReleaseOfCode("753");
		
		buildSrc("    lv_value = lv_value + 1.");
		buildSrc("    lv_value = lv_value - 1.");
		buildSrc("    lv_value = lv_value * lv_factor.");
		buildSrc("    lv_value = lv_value / lv_divisor.");
		buildSrc("    lv_value = - lv_value.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReleaseRestriction() {
		// ensure that calculation assignment operators are NOT introduced if the user applies an ABAP release restriction < 7.54  
		// (even if the code itself is compiled against the newest release)
		
		setReleaseRestrictionFromUI(753);
		
		buildSrc("    lv_value = lv_value + 1.");
		buildSrc("    lv_value = lv_value - 1.");
		buildSrc("    lv_value = lv_value * lv_factor.");
		buildSrc("    lv_value = lv_value / lv_divisor.");
		buildSrc("    lv_value = - lv_value.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleCases() {
		buildSrc("    lv_value = lv_value + 1.");
		buildSrc("    lv_value = lv_value - 1.");
		buildSrc("    lv_value = lv_value * lv_factor.");
		buildSrc("    lv_value = lv_value / lv_divisor.");
		buildSrc("    lv_value = - lv_value.");

		buildExp("    lv_value += 1.");
		buildExp("    lv_value -= 1.");
		buildExp("    lv_value *= lv_factor.");
		buildExp("    lv_value /= lv_divisor.");
		buildExp("    lv_value *= -1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleCasesWithMinus() {
		buildSrc("    lv_value = - lv_value + 1.");
		buildSrc("    lv_value = - lv_value - 1.");
		buildSrc("    lv_value = - lv_value * 2.");
		buildSrc("    lv_value = - lv_value / -3.");
		buildSrc("    lv_value = - lv_value * lv_factor.");
		buildSrc("    lv_value = - lv_value / lv_divisor.");

		buildExp("    lv_value = - lv_value + 1.");
		buildExp("    lv_value = - lv_value - 1.");
		buildExp("    lv_value *= -2.");
		buildExp("    lv_value /= 3.");
		buildExp("    lv_value *= - lv_factor.");
		buildExp("    lv_value /= - lv_divisor.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleCasesVariableAtEnd() {
		// expect only the + and * cases to be changed
		buildSrc("    lv_value = 1 + lv_value.");
		buildSrc("    lv_value = 1 - lv_value.");
		buildSrc("    lv_value = lv_factor * lv_value.");
		buildSrc("    lv_value = lv_dividend / lv_value.");

		buildExp("    lv_value += 1.");
		buildExp("    lv_value = 1 - lv_value.");
		buildExp("    lv_value *= lv_factor.");
		buildExp("    lv_value = lv_dividend / lv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testKeepSimpleCasesVariableAtEnd() {
		rule.configAllowVariableAtEnd.setValue(false);

		buildSrc("    lv_value = 1 + lv_value.");
		buildSrc("    lv_value = 1 - lv_value.");
		buildSrc("    lv_value = lv_factor * lv_value.");
		buildSrc("    lv_value = lv_dividend / lv_value.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleCasesVariableAtEndWithMinus() {
		buildSrc("    lv_value = - 5 * lv_value.");
		buildSrc("    lv_value = - 1 - lv_value.");
		buildSrc("    lv_value = - lv_factor * lv_value.");
		buildSrc("    lv_value = - lv_dividend / lv_value.");

		buildExp("    lv_value *= -5.");
		buildExp("    lv_value = - 1 - lv_value.");
		buildExp("    lv_value *= - lv_factor.");
		buildExp("    lv_value = - lv_dividend / lv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSingleTermInParentheses() {
		// ensure that parentheses which enclose a single term (e.g. a single literal, table expression, or constructor expression) are removed, 
		// because "lv_value *= ( -1 )." etc. would be a syntax error
		
		buildSrc("    lv_value = lv_value * ( -1 ).");
		buildSrc("    lv_value = lv_value / ( CONV i( '-1.1' ) ).");
		buildSrc("    lv_value = lv_value + ( lts_value[ 1 ] ).");
		buildSrc("    lv_value = lv_value - ( -1 \" comment");
		buildSrc("                          ).");

		buildExp("    lv_value *= -1.");
		buildExp("    lv_value /= CONV i( '-1.1' ).");
		buildExp("    lv_value += lts_value[ 1 ].");
		buildExp("    lv_value -= -1 \" comment");
		buildExp("                .");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSingleTermInParenthesesWithMinus() {
		// as opposed to "lv_value *= ( -1 )" in the previous test, "lv_value *= - ( -1 )." is NOT a syntax error
		
		buildSrc("    lv_value = - lv_value * ( -1 ).");
		buildSrc("    lv_value = - lv_value / ( CONV i( '-1.1' ) ).");

		buildExp("    lv_value *= - ( -1 ).");
		buildExp("    lv_value /= - ( CONV i( '-1.1' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSingleTermInParenthesesVariableAtEnd() {
		buildSrc("    lv_value = ( -1 ) * lv_value.");
		buildSrc("    lv_value = ( CONV i( '-1.1' ) ) / lv_value.");
		buildSrc("    lv_value = ( lts_value[ 1 ] ) + lv_value.");

		buildExp("    lv_value *= -1.");
		buildExp("    lv_value = ( CONV i( '-1.1' ) ) / lv_value.");
		buildExp("    lv_value += lts_value[ 1 ].");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testArithmeticExprInParentheses() {
		// parentheses that enclose an arithmetic expression with multiple terms can be kept for readability, 
		// and "lv_value *= ( 1 - 2 )." is NOT a syntax error, as opposed to the cases in testSingleTermInParentheses()
		
		buildSrc("    lv_value = lv_value * ( 1 - 2 ).");
		buildSrc("    lv_value = lv_value / ( CONV i( '-1.1' ) - 2 ).");
		buildSrc("    lv_value = lv_value + ( lts_value[ 1 ] - 1 ).");

		buildExp("    lv_value *= ( 1 - 2 ).");
		buildExp("    lv_value /= ( CONV i( '-1.1' ) - 2 ).");
		buildExp("    lv_value += ( lts_value[ 1 ] - 1 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testArithmeticExprInParenthesesWithMinus() {
		// parentheses that enclose an arithmetic expression with multiple terms can be kept for readability, 
		// and "lv_value *= ( 1 - 2 )." is NOT a syntax error, as opposed to the cases in testSingleTermInParentheses()
		
		buildSrc("    lv_value = - lv_value * ( 1 - 2 ).");
		buildSrc("    lv_value = - lv_value / ( CONV i( '-1.1' ) - 2 ).");
		buildSrc("    lv_value = - lv_value + ( lts_value[ 1 ] - 1 ).");

		buildExp("    lv_value *= - ( 1 - 2 ).");
		buildExp("    lv_value /= - ( CONV i( '-1.1' ) - 2 ).");
		buildExp("    lv_value = - lv_value + ( lts_value[ 1 ] - 1 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testArithmeticExprInParenthesesVarAtEnd() {
		buildSrc("    lv_value = ( 1 - 2 ) * lv_value.");
		buildSrc("    lv_value = ( CONV i( '-1.1' ) - 2 ) - lv_value.");
		buildSrc("    lv_value = ( lts_value[ 1 ] - 1 ) + lv_value.");

		buildExp("    lv_value *= ( 1 - 2 ).");
		buildExp("    lv_value = ( CONV i( '-1.1' ) - 2 ) - lv_value.");
		buildExp("    lv_value += ( lts_value[ 1 ] - 1 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testKeepArithmeticExprVariableAtEnd() {
		// complex cases with the variable at the end are not supported, even if they could be converted (like the last three lines): 
		buildSrc("    lv_value = 1 - 2 * lv_value.");
		buildSrc("    lv_value = 2 * lv_factor * lv_value.");
		buildSrc("    lv_value = CONV i( '-1.1' ) - 2 + lv_value.");
		buildSrc("    lv_value = lts_value[ 1 ] ** 2 + lv_value.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepSimpleCasesWithMinus() {
		rule.configAllowMinusBeforeMultOrDiv.setValue(false);
		
		buildSrc("    lv_value = - lv_value * lv_factor.");
		buildSrc("    lv_value = - lv_value / lv_divisor.");
		buildSrc("    lv_value = - lv_value * ( -1 ).");
		buildSrc("    lv_value = - lv_value / ( CONV i( '-1.1' ) ).");
		buildSrc("    lv_value = - lv_value * ( 1 - 2 ).");
		buildSrc("    lv_value = - lv_value / ( CONV i( '-1.1' ) - 2 ).");
		// also include cases with the variable at the end:
		buildSrc("    lv_value = - lv_factor * lv_value.");
		buildSrc("    lv_value = - ( -1 ) * lv_value.");
		buildSrc("    lv_value = - ( 1 - 2 ) * lv_value.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentToComponent() {
		buildSrc("    ls_struc-component = ls_struc-component + 1.");
		buildSrc("    <ls_field>-component = <ls_field>-component - 1.");
		buildSrc("    ls_struc-component = - ls_struc-component.");
		buildSrc("    <ls_field>-component = - <ls_field>-component.");

		buildExp("    ls_struc-component += 1.");
		buildExp("    <ls_field>-component -= 1.");
		buildExp("    ls_struc-component *= -1.");
		buildExp("    <ls_field>-component *= -1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentToComponentWithMinus() {
		buildSrc("    ls_struc-component = - ls_struc-component + 1.");
		buildSrc("    ls_struc-component = - ls_struc-component * 2.");
		buildSrc("    <ls_field>-component = - <ls_field>-component / 3.");

		buildExp("    ls_struc-component = - ls_struc-component + 1.");
		buildExp("    ls_struc-component *= -2.");
		buildExp("    <ls_field>-component /= -3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentToComponentAtEnd() {
		buildSrc("    ls_struc-component = 1 + ls_struc-component.");
		buildSrc("    <ls_field>-component = 1 - <ls_field>-component.");
		buildSrc("    <ls_field>-component = - 3 * <ls_field>-component.");

		buildExp("    ls_struc-component += 1.");
		buildExp("    <ls_field>-component = 1 - <ls_field>-component.");
		buildExp("    <ls_field>-component *= -3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentToSubstring() {
		buildSrc("    lv_date+4(2) = lv_date+4(2) - 1.");

		buildExp("    lv_date+4(2) -= 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexOperand() {
		buildSrc("    lv_value = lv_value + get_value( EXPORTING iv_value1 = lv_value \" comment 1");
		buildSrc("                                               iv_value2 = 'abc'    \" comment 2");
		buildSrc("                                               iv_value3 = VALUE #( a = 5");
		buildSrc("                                                                    b = 7 ) ). \" comment 3");

		buildExp("    lv_value += get_value( EXPORTING iv_value1 = lv_value \" comment 1");
		buildExp("                                     iv_value2 = 'abc'    \" comment 2");
		buildExp("                                     iv_value3 = VALUE #( a = 5");
		buildExp("                                                          b = 7 ) ). \" comment 3");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexOperandVariableAtEnd() {
		buildSrc("    lv_value = get_value( EXPORTING iv_value1 = lv_value \" comment 1");
		buildSrc("                                    iv_value2 = 'abc'    \" comment 2");
		buildSrc("                                    iv_value3 = VALUE #( a = 5");
		buildSrc("                                                         b = 7 ) ) + lv_value. \" comment 3");

		buildExp("    lv_value += get_value( EXPORTING iv_value1 = lv_value \" comment 1");
		buildExp("                                     iv_value2 = 'abc'    \" comment 2");
		buildExp("                                     iv_value3 = VALUE #( a = 5");
		buildExp("                                                          b = 7 ) ). \" comment 3");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexArithmeticeExpr() {
		buildSrc("    \" cases with longer arithmetic expressions are only allowed for addition:");
		buildSrc("    rv_total_amount = rv_total_amount + <ls_item>-first_amount + <ls_item>-second_amount + <ls_item>-third_amount.");
		buildSrc("    lv_value = lv_value + 1 - 3 ** ( 7 MOD 10 ) DIV 3.");

		buildExp("    \" cases with longer arithmetic expressions are only allowed for addition:");
		buildExp("    rv_total_amount += <ls_item>-first_amount + <ls_item>-second_amount + <ls_item>-third_amount.");
		buildExp("    lv_value += 1 - 3 ** ( 7 MOD 10 ) DIV 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNoChangeDueToDifferentVars() {
		buildSrc("    lv_value = iv_value + 1. \" identifiers do not match!");
		buildSrc("    lv_value = - iv_value. \" comment");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNoChangeDueToOperatorPriority() {
		buildSrc("    lv_value = lv_value * 5 + 3. \" due to operator priority this is NOT the same as lv_value *= 5 + 3");
		buildSrc("    lv_value = lv_value / 2 + 1. \" due to operator priority this is NOT the same as lv_value /= 2 + 1");
		buildSrc("    lv_value = - lv_value * 5 + 3.");
		buildSrc("    lv_value = - lv_value / 2 + 1.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNoChangeDueToExecutionOrder() {
		buildSrc("    lv_value = lv_value - 2 - 1. \" due to execution order (left-to-right), this is NOT the same as lv_value -= 2 - 1.");
		buildSrc("    lv_value = lv_value / 2 / 5. \" due to execution order (left-to-right), this is NOT the same as lv_value /= 2 / 5.");
		buildSrc("    lv_value = - lv_value / 2 / 5.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		// expect commands with chain colon to remain unchanged
		
		buildSrc("    lv_value = lv_value : + 1, + 2.");
		buildSrc("    lv_value = lv_value - : 1, 2.");
		buildSrc("    lv_value = lv_value * lv_factor :,.");
		buildSrc("    lv_value = - lv_value :.");
		buildSrc("    lv_value = - lv_value * 3 :.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testComment() {
		// expect the comment before the calculation operator to prevent the change
		
		buildSrc("    lv_value = lv_value \" comment on value");
		buildSrc("             + 1.       \" another comment");
		buildSrc("");
		buildSrc("    lv_value = lv_value \" comment on value");
		buildSrc("             - 1.");
		buildSrc("");
		buildSrc("    lv_value = - \" comment on value");
		buildSrc("               lv_value.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDoNotConvertMinusToMultiplication() {
		rule.configConvertMinusToMultiplication.setValue(false);

		buildSrc("    lv_value = - lv_value.");
		buildSrc("    ls_struc-component = - ls_struc-component.");
		buildSrc("    <ls_field>-component = - <ls_field>-component.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
