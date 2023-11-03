package com.sap.adt.abapcleaner.rulehelpers;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;


public class LogicalExpressionTest {
	private static final String abapRelease = ABAP.NEWEST_RELEASE; 
	
	void testLogicalExpression(String abapExpression, String expDebugString, String expNegatedExpression) {
		testLogicalExpression(NegationStyle.AVOID_INNER_NEGATIONS, abapExpression, expDebugString, expNegatedExpression);
	}
	
	void testLogicalExpression(NegationStyle negationStyle, String abapExpression, String expDebugString, String expNegatedExpression) {
		// get a valid ABAP statement by using the logical expression inside a 'CHECK ...' Command  
		String sourceCode = "CHECK " + abapExpression + ".";

		// parse the source code
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, abapRelease));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}

		// test the referential integrity of the parse result
		try {
			code.testReferentialIntegrity(true);
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code:" + e1.getMessage());
			return;
		} 
		
		// create the LogicalExpression
		Command command = code.firstCommand;
		LogicalExpression logicalExp = null;
		try {
			logicalExp = LogicalExpression.create(command.getFirstToken().getNext(), command.getLastToken().getPrev());
		} catch (UnexpectedSyntaxException e) {
			fail("Error creating logical expression:" + e.getMessage());
		}
		assertTrue(logicalExp.isSupported());
		assertEquals(command.getFirstToken().getNext(), logicalExp.getFirstToken());
		
		// check the debug text (and thereby operator precedence) 
		String debugString = logicalExp.toString();
		assertEquals(expDebugString, debugString);
		
		// check that a TreeAlign can be created
		TreeAlign tree = null;
		try {
			tree = TreeAlign.createFrom(logicalExp);
		} catch (UnexpectedSyntaxAfterChanges | UnexpectedSyntaxException e) {
			fail("Error creating tree align:" + e.getMessage());
		}
		assertNotNull(tree);
		
		// negate the logical expression
		try {
			logicalExp.negate(negationStyle, true);
		} catch (UnexpectedSyntaxAfterChanges | UnexpectedSyntaxException e) {
			fail("Error negating logical expression:" + e.getMessage());
		}

		// check the ABAP code of the negated logical expression 
		assertEquals("CHECK " + expNegatedExpression + ".", command.toString());
	}
	
	void testLogicalExpressionNotSupported(String abapExpression) {
		// get a valid ABAP statement by using the logical expression inside a 'CHECK ...' command  
		String sourceCode = "CHECK " + abapExpression + ".";

		// parse the source code
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, abapRelease));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}

		// test the referential integrity of the parse result
		try {
			code.testReferentialIntegrity(true);
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code:" + e1.getMessage());
			return;
		} 
		
		// create the LogicalExpression
		Command command = code.firstCommand;
		LogicalExpression logicalExp = null;
		try {
			logicalExp = LogicalExpression.create(command.getFirstToken().getNext(), command.getLastToken().getPrev());
		} catch (UnexpectedSyntaxException e) {
			fail("Error creating logical expression:" + e.getMessage());
		}
		
		// expect the expression to be not supported
		assertFalse(logicalExp.isSupported());

		// check the debug text (and thereby operator precedence) 
		String debugString = logicalExp.toString();
		assertTrue(debugString.startsWith("??:"));
}
	
	@Test
	void testComparison() {
		testLogicalExpression(
				"a = b", 
				"a !=! b", 
				"a <> b");
	}
	
	@Test
	void testComparisonInParentheses() {
		testLogicalExpression(
				"( a = b )", 
				"( a !=! b )", 
				"( a <> b )");
	}
	
	@Test
	void testComparisonWithoutNot() {
		// ensure that NegationStyle.ALWAYS_WITH_AND_OR has NO effect here
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"a = b", 
				"a !=! b", 
				"a <> b");
	}
	
	@Test
	void testComparisonOrComparisonWithNot() {
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"a = b OR a = c", 
				"{ a !=! b } !OR! { a !=! c }", 
				"NOT ( a = b OR a = c )");
	}
	
	@Test
	void testComparisonOrComparison() {
		testLogicalExpression(NegationStyle.NEVER,
				"a = b OR a = c", 
				"{ a !=! b } !OR! { a !=! c }", 
				"a <> b AND a <> c");
	}
	
	@Test
	void testAndOrAndWithNot() {
		testLogicalExpression(
				"a = b AND b = c OR a <> b AND b <> c", 
				"{ { a !=! b } !AND! { b !=! c } } !OR! { { a !<>! b } !AND! { b !<>! c } }",
				"NOT ( a = b AND b = c ) AND ( a = b OR b = c )");
	}
	
	@Test
	void testAndOrAnd() {
		testLogicalExpression(NegationStyle.NEVER,
				"a = b AND b = c OR a <> b AND b <> c", 
				"{ { a !=! b } !AND! { b !=! c } } !OR! { { a !<>! b } !AND! { b !<>! c } }",
				"( a <> b OR b <> c ) AND ( a = b OR b = c )");
	}
	
	@Test
	void testAndAndOrAndAndWithNot() {
		testLogicalExpression(
				"a < b AND b = c AND c < d OR a > b AND b = c AND c > d", 
				"{ { a !<! b } !AND! { b !=! c } !AND! { c !<! d } } !OR! { { a !>! b } !AND! { b !=! c } !AND! { c !>! d } }",
				"NOT ( a < b AND b = c AND c < d OR a > b AND b = c AND c > d )");
	}
	
	@Test
	void testAndAndOrAndAnd() {
		testLogicalExpression(NegationStyle.NEVER,
				"a < b AND b = c AND c < d OR a > b AND b = c AND c > d", 
				"{ { a !<! b } !AND! { b !=! c } !AND! { c !<! d } } !OR! { { a !>! b } !AND! { b !=! c } !AND! { c !>! d } }",
				"( a >= b OR b <> c OR c >= d ) AND ( a <= b OR b <> c OR c <= d )");
	}
	
	@Test
	void testPredicate() {
		testLogicalExpression(
				"a IS BOUND", 
				"a !IS! BOUND", 
				"a IS NOT BOUND");
	}
	
	@Test
	void testPredicateAndNotPredicate() {
		testLogicalExpression(
				"a IS BOUND OR b IS NOT INITIAL", 
				"{ a !IS! BOUND } !OR! { b !IS! NOT INITIAL }", 
				"a IS NOT BOUND AND b IS INITIAL");
	}
	
	@Test
	void testBetweenWithNot() {
		testLogicalExpression(
				"a BETWEEN b AND c OR d BETWEEN e AND f", 
				"{ a !BETWEEN! b AND c } !OR! { d !BETWEEN! e AND f }", 
				"NOT ( a BETWEEN b AND c OR d BETWEEN e AND f )");
	}
	
	@Test
	void testBetween() {
		testLogicalExpression(NegationStyle.NEVER,
				"a BETWEEN b AND c OR d BETWEEN e AND f", 
				"{ a !BETWEEN! b AND c } !OR! { d !BETWEEN! e AND f }", 
				"a NOT BETWEEN b AND c AND d NOT BETWEEN e AND f");
	}
	
	@Test
	void testNotBetweenWithNot() {
		// force negation with NOT ( ... ), although it is not advisable
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"a NOT BETWEEN b AND c AND d NOT BETWEEN e AND f", 
				"{ a NOT !BETWEEN! b AND c } !AND! { d NOT !BETWEEN! e AND f }", 
				"NOT ( a NOT BETWEEN b AND c AND d NOT BETWEEN e AND f )");
	}
	
	@Test
	void testNotBetween() {
		testLogicalExpression(
				"a NOT BETWEEN b AND c AND d NOT BETWEEN e AND f", 
				"{ a NOT !BETWEEN! b AND c } !AND! { d NOT !BETWEEN! e AND f }", 
				"a BETWEEN b AND c OR d BETWEEN e AND f");
	}
	
	@Test
	void testEquiv() {
		testLogicalExpression(
				"a = b EQUIV c = d", 
				"{ a !=! b } !EQUIV! { c !=! d }", 
				"a = b NOT EQUIV c = d");
	}
	
	@Test
	void testEquivWithoutNot() {
		// ensure that NegationStyle.ALWAYS_WITH_AND_OR has NO effect here
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"a = b EQUIV c = d", 
				"{ a !=! b } !EQUIV! { c !=! d }", 
				"a = b NOT EQUIV c = d");
	}
	
	@Test
	void testNotEquiv() {
		testLogicalExpression(
				"a = b NOT EQUIV c = d", 
				"{ a !=! b NOT } !EQUIV! { c !=! d }", // TODO: bracketing of "NOT" is not ideal here 
				"a = b EQUIV c = d");
	}
	
	@Test
	void testPredicateFunction() {
		testLogicalExpression(
				"line_exists( lts_table[ 1 ] )", 
				"!line_exists( )!", 
				"NOT line_exists( lts_table[ 1 ] )");
	}
	
	@Test
	void testMultiplePredicateFunctions() {
		testLogicalExpression(
				"line_exists( lts_table[ 1 ] ) AND NOT line_exists( lts_table[ 2 ] )", 
				"{ !line_exists( )! } !AND! { !NOT! { !line_exists( )! } }", 
				"NOT line_exists( lts_table[ 1 ] ) OR line_exists( lts_table[ 2 ] )");
	}
	
	@Test
	void testNotPredicateFunction() {
		testLogicalExpression(
				"NOT line_exists( lts_table[ 1 ] )", 
				"!NOT! { !line_exists( )! }", 
				"line_exists( lts_table[ 1 ] )");
	}
	
	@Test
	void testNotPredicateFunctionWithoutNot() {
		// ensure that NegationStyle.AVOID_INNER_NEGATIONS has NO effect here
		testLogicalExpression(NegationStyle.AVOID_INNER_NEGATIONS,
				"NOT line_exists( lts_table[ 1 ] )", 
				"!NOT! { !line_exists( )! }", 
				"line_exists( lts_table[ 1 ] )");
	}
	
	@Test
	void testAbapBoolWithNot() {
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"a = abap_true AND b = abap_false", 
				"{ a !=! abap_true } !AND! { b !=! abap_false }", 
				"NOT ( a = abap_true AND b = abap_false )");
	}
	
	@Test
	void testAbapBoolWithNotAndParens() {
		// ensure that NOT ( ... ) adds additional parentheses here, 
		// because the starting "(" and the final ")" are NOT a pair
		testLogicalExpression(NegationStyle.ALWAYS_WITH_AND_OR,
				"( a = abap_true ) AND ( b = abap_false )", 
				"{ ( a !=! abap_true ) } !AND! { ( b !=! abap_false ) }", 
				"NOT ( ( a = abap_true ) AND ( b = abap_false ) )");
	}
	
	@Test
	void testAbapBool() {
		testLogicalExpression(
				"a = abap_true AND b = abap_false", 
				"{ a !=! abap_true } !AND! { b !=! abap_false }", 
				"a = abap_false OR b = abap_true");
	}
	
	@Test
	void testSymbolicComparison() {
		testLogicalExpression(
				"a LT b OR a LE b OR a EQ b OR a GE b OR a GT b", 
				"{ a !LT! b } !OR! { a !LE! b } !OR! { a !EQ! b } !OR! { a !GE! b } !OR! { a !GT! b }", 
				"a >= b AND a > b AND a <> b AND a < b AND a <= b");
	}
	
	@Test
	void testPredicativeMethodCall() {
		testLogicalExpression(
				"is_valid( a )", 
				"!is_valid( )!", 
				"NOT is_valid( a )");
	}
	
	@Test
	void testPredicativeMethodCallWithNot() {
		testLogicalExpression(
				"NOT is_valid( a = 1 )", 
				"!NOT! { !is_valid( )! }", 
				"is_valid( a = 1 )");
	}
	
	@Test
	void testPredicativeMethodCalls1Not() {
		testLogicalExpression(
				"is_valid( a ) OR NOT is_valid( b ) AND is_valid( c )", 
				"{ !is_valid( )! } !OR! { { !NOT! { !is_valid( )! } } !AND! { !is_valid( )! } }", 
				"NOT is_valid( a ) AND ( is_valid( b ) OR NOT is_valid( c ) )");
	}
	
	@Test
	void testPredicativeMethodCalls2Not() {
		testLogicalExpression(
				"NOT is_valid( a ) OR is_valid( b ) AND NOT is_valid( c )", 
				"{ !NOT! { !is_valid( )! } } !OR! { { !is_valid( )! } !AND! { !NOT! { !is_valid( )! } } }", 
				"is_valid( a ) AND ( NOT is_valid( b ) OR is_valid( c ) )");
	}
	
	@Test
	void testPredicativeMethodCalls3Not() {
		testLogicalExpression(
				"NOT is_valid( a ) OR NOT is_valid( b ) AND NOT is_valid( c )", 
				"{ !NOT! { !is_valid( )! } } !OR! { { !NOT! { !is_valid( )! } } !AND! { !NOT! { !is_valid( )! } } }", 
				"is_valid( a ) AND ( is_valid( b ) OR is_valid( c ) )");
	}
	
	@Test
	void testAndOpenOrAndCloseWithNot() {
		testLogicalExpression(
				"a = 1 AND ( b = 1 OR b = 2 AND c = 3 )", 
				"{ a !=! 1 } !AND! { ( { b !=! 1 } !OR! { { b !=! 2 } !AND! { c !=! 3 } } ) }", 
				"NOT ( a = 1 AND ( b = 1 OR b = 2 AND c = 3 ) )");
	}

	@Test
	void testAndOpenOrAndClose() {
		// ensure negation works if a closing parenthesis must be inserted before an existing closing parenthesis
		testLogicalExpression(NegationStyle.NEVER,
				"a = 1 AND ( b = 1 OR b = 2 AND c = 3 )", 
				"{ a !=! 1 } !AND! { ( { b !=! 1 } !OR! { { b !=! 2 } !AND! { c !=! 3 } } ) }", 
				"a <> 1 OR ( b <> 1 AND ( b <> 2 OR c <> 3 ) )");
	}

	@Test
	void testAndOpenAndOrCloseWithNot() {
		testLogicalExpression(
				"a = 1 AND ( b = 2 AND c = 3 OR b = 1 )", 
				"{ a !=! 1 } !AND! { ( { { b !=! 2 } !AND! { c !=! 3 } } !OR! { b !=! 1 } ) }", 
				"NOT ( a = 1 AND ( b = 2 AND c = 3 OR b = 1 ) )");
	}

	@Test
	void testAndOpenAndOrClose() {
		// ensure negation works if an opening parenthesis must be inserted after an existing opening parenthesis
		testLogicalExpression(NegationStyle.NEVER,
				"a = 1 AND ( b = 2 AND c = 3 OR b = 1 )", 
				"{ a !=! 1 } !AND! { ( { { b !=! 2 } !AND! { c !=! 3 } } !OR! { b !=! 1 } ) }", 
				"a <> 1 OR ( ( b <> 2 OR c <> 3 ) AND b <> 1 )");
	}

	@Test
	void testOuterNotForSubExpression() {
		// expect NOT ( ... ) for the first sub-expression, but inner negation for the second one
		testLogicalExpression(
				"a = 1 AND b = 1 AND c = 1 OR a <> 1 AND b <> 1 AND c <> 1", 
				"{ { a !=! 1 } !AND! { b !=! 1 } !AND! { c !=! 1 } } !OR! { { a !<>! 1 } !AND! { b !<>! 1 } !AND! { c !<>! 1 } }", 
				"NOT ( a = 1 AND b = 1 AND c = 1 ) AND ( a = 1 OR b = 1 OR c = 1 )");
	}

	@Test
	void testOuterNotForDiverseSubExpression() {
		// expect inner negation for the first sub-expression, but NOT ( ... ) for the second one 
		testLogicalExpression(
				"a <> 1 AND b IS NOT SUPPLIED AND NOT line_exists( itab[ 1 ] ) OR a = 1 AND b IS INITIAL AND line_exists( itab[ 2 ] )", 
				"{ { a !<>! 1 } !AND! { b !IS! NOT SUPPLIED } !AND! { !NOT! { !line_exists( )! } } } !OR! { { a !=! 1 } !AND! { b !IS! INITIAL } !AND! { !line_exists( )! } }", 
				"( a = 1 OR b IS SUPPLIED OR line_exists( itab[ 1 ] ) ) AND NOT ( a = 1 AND b IS INITIAL AND line_exists( itab[ 2 ] ) )");
	}

	@Test
	void testOuterNotForSubExpressions2And4() {
		// expect NOT ( ... ) for the 2nd and 4th sub-expressions, but inner negation for the 1st and 3rd one
		testLogicalExpression(
				"( a <> 1 OR b <> 1 ) AND ( c = 1 OR d = 1 ) AND ( e <> f OR g <> h ) AND ( i = 2 OR j = 3 )", 
				"{ ( { a !<>! 1 } !OR! { b !<>! 1 } ) } !AND! { ( { c !=! 1 } !OR! { d !=! 1 } ) } !AND! { ( { e !<>! f } !OR! { g !<>! h } ) } !AND! { ( { i !=! 2 } !OR! { j !=! 3 } ) }", 
				"( a = 1 AND b = 1 ) OR NOT ( c = 1 OR d = 1 ) OR ( e = f AND g = h ) OR NOT ( i = 2 OR j = 3 )");
	}

	@Test
	void testOuterNotForSubExpressions1And4() {
		// expect NOT ( ... ) for the 1st and 4th sub-expressions, but inner negation for the 2nd and 3rd one
		testLogicalExpression(
				"( a = 1 OR b = 1 ) AND ( c <> 1 OR d <> 1 ) AND ( e <> f OR g <> h ) AND ( i = 2 OR j = 3 )", 
				"{ ( { a !=! 1 } !OR! { b !=! 1 } ) } !AND! { ( { c !<>! 1 } !OR! { d !<>! 1 } ) } !AND! { ( { e !<>! f } !OR! { g !<>! h } ) } !AND! { ( { i !=! 2 } !OR! { j !=! 3 } ) }", 
				"NOT ( a = 1 OR b = 1 ) OR ( c = 1 AND d = 1 ) OR ( e = f AND g = h ) OR NOT ( i = 2 OR j = 3 )");
	}

}
