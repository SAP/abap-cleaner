package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class AbapTest {
	@Test
	void testIsAbapUpperCaseKeywordTrue() {
		assertTrue(ABAP.isAbapUpperCaseKeyword("types"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("TYPES"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("types:"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("TYPES:"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("data"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("DATA"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("data("));
		assertTrue(ABAP.isAbapUpperCaseKeyword("DATA("));
		assertTrue(ABAP.isAbapUpperCaseKeyword("final("));
		assertTrue(ABAP.isAbapUpperCaseKeyword("FINAL("));
		assertTrue(ABAP.isAbapUpperCaseKeyword("move-corresponding"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("MOVE-CORRESPONDING"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("FIELD-SYMBOLS"));
		assertTrue(ABAP.isAbapUpperCaseKeyword("CLASS-DATA"));
	}

	@Test
	void testIsAbapUpperCaseKeywordFalse() {
		assertFalse(ABAP.isAbapUpperCaseKeyword(""));
		assertFalse(ABAP.isAbapUpperCaseKeyword("any"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("INT8"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("me"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("SPACE"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("string"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("TIMESTAMPL"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("xsdbool"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("sy"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("TABLE_LINE"));

		assertFalse(ABAP.isAbapUpperCaseKeyword("HAVE"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("have"));
	}

	@Test
	void testIsAbapLowerCaseKeywordTrue() {
		assertTrue(ABAP.isAbapLowerCaseKeyword("any"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("INT8"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("me"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("SPACE"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("string"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("TIMESTAMPL"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("xsdbool"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("sy-subrc"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("SY-SUBRC"));
		assertTrue(ABAP.isAbapLowerCaseKeyword("TABLE_LINE"));
	}
	
	@Test
	void testIsAbapLowerCaseKeywordFalse() {
		assertFalse(ABAP.isAbapLowerCaseKeyword(""));
		assertFalse(ABAP.isAbapLowerCaseKeyword("types"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("TYPES"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("types:"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("TYPES:"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("data"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("DATA"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("data("));
		assertFalse(ABAP.isAbapLowerCaseKeyword("DATA("));
		assertFalse(ABAP.isAbapLowerCaseKeyword("final("));
		assertFalse(ABAP.isAbapLowerCaseKeyword("FINAL("));
		assertFalse(ABAP.isAbapLowerCaseKeyword("move-corresponding"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("MOVE-CORRESPONDING"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("FIELD-SYMBOLS"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("CLASS-DATA"));

		assertFalse(ABAP.isAbapLowerCaseKeyword("HAVE"));
		assertFalse(ABAP.isAbapLowerCaseKeyword("have"));
	}

	@Test
	void testIsHexValueTrue() {
		assertTrue(ABAP.isHexValue("00"));
		assertTrue(ABAP.isHexValue("FF"));
		assertTrue(ABAP.isHexValue("0123456789ABCDEF"));
		assertTrue(ABAP.isHexValue("0A1B2C3D4E5F"));
	}


	@Test
	void testIsHexValueFalse() {
		assertFalse(ABAP.isHexValue(null));
		assertFalse(ABAP.isHexValue("0"));   // uneven number of chars
		assertFalse(ABAP.isHexValue("FFF")); // uneven number of chars
		assertFalse(ABAP.isHexValue("GH"));  // beyond 0-9 A-F
		assertFalse(ABAP.isHexValue("ZZZ"));
		assertFalse(ABAP.isHexValue("ab")); // lower case 
		assertFalse(ABAP.isHexValue("ff"));
		assertFalse(ABAP.isHexValue("\0\t\n\r"));
	}

	@Test
	void testConsistsOfDigitsOnlyTrue() {
		assertTrue(ABAP.consistsOfDigitsOnly(null));
		assertTrue(ABAP.consistsOfDigitsOnly("0"));
		assertTrue(ABAP.consistsOfDigitsOnly("42"));
		assertTrue(ABAP.consistsOfDigitsOnly("000"));
		assertTrue(ABAP.consistsOfDigitsOnly("0123456789"));
	}


	@Test
	void testConsistsOfDigitsOnlyFalse() {
		assertFalse(ABAP.consistsOfDigitsOnly("3.1415"));
		assertFalse(ABAP.consistsOfDigitsOnly("3,1415"));
		assertFalse(ABAP.consistsOfDigitsOnly("1E9"));
		assertFalse(ABAP.consistsOfDigitsOnly("xyz"));
		assertFalse(ABAP.consistsOfDigitsOnly("'7'"));
	}
	
	@Test
	void testConsistsOfDigitsOnlyWithNoLeading0True() {
		assertTrue(ABAP.consistsOfDigitsOnlyWithNoLeading0("0"));
		assertTrue(ABAP.consistsOfDigitsOnlyWithNoLeading0("42"));
		assertTrue(ABAP.consistsOfDigitsOnlyWithNoLeading0("1234567890"));
	}


	@Test
	void testConsistsOfDigitsOnlyWithNoLeading0False() {
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0(null));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("3.1415"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("3,1415"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("1E9"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("xyz"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("'7'"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("00"));
		assertFalse(ABAP.consistsOfDigitsOnlyWithNoLeading0("007"));
	}
	
	@Test
	void testIsIntegerTrue() {
		assertTrue(ABAP.isInteger("0"));
		assertTrue(ABAP.isInteger("-42"));
		assertTrue(ABAP.isInteger("-1234567"));
		assertTrue(ABAP.isInteger("31415"));
	}

	@Test
	void testIsIntegerFalse() {
		assertFalse(ABAP.isInteger("--0"));
		assertFalse(ABAP.isInteger("+5"));
		assertFalse(ABAP.isInteger("05"));
		assertFalse(ABAP.isInteger("AABBCC"));
	}

	@Test
	void testTextualComparisonOperator() {
		assertEquals("LT", ABAP.getTextualComparisonOperator("<"));
		assertEquals("LE", ABAP.getTextualComparisonOperator("<="));
		assertEquals("EQ", ABAP.getTextualComparisonOperator("="));
		assertEquals("GE", ABAP.getTextualComparisonOperator(">="));
		assertEquals("GT", ABAP.getTextualComparisonOperator(">"));
		assertEquals("NE", ABAP.getTextualComparisonOperator("<>"));
	}

	@Test
	void testNegateComparisonOperator() {
		assertEquals(">=", ABAP.negateComparisonOperator("<"));
		assertEquals(">=", ABAP.negateComparisonOperator("LT"));
		assertEquals(">=", ABAP.negateComparisonOperator("lt"));
		
		assertEquals(">", ABAP.negateComparisonOperator("<="));
		assertEquals(">", ABAP.negateComparisonOperator("LE"));
		assertEquals(">", ABAP.negateComparisonOperator("le"));
		
		assertEquals("<>", ABAP.negateComparisonOperator("="));
		assertEquals("<>", ABAP.negateComparisonOperator("EQ"));
		assertEquals("<>", ABAP.negateComparisonOperator("eq"));

		assertEquals("<", ABAP.negateComparisonOperator(">="));
		assertEquals("<", ABAP.negateComparisonOperator("GE"));
		assertEquals("<", ABAP.negateComparisonOperator("ge"));
		
		assertEquals("<=", ABAP.negateComparisonOperator(">"));
		assertEquals("<=", ABAP.negateComparisonOperator("GT"));
		assertEquals("<=", ABAP.negateComparisonOperator("gt"));
		
		assertEquals("=", ABAP.negateComparisonOperator("<>"));
		assertEquals("=", ABAP.negateComparisonOperator("NE"));
		assertEquals("=", ABAP.negateComparisonOperator("ne"));
		
		assertEquals("CN", ABAP.negateComparisonOperator("CO"));
		assertEquals("CO", ABAP.negateComparisonOperator("CN"));
		
		assertEquals("NA", ABAP.negateComparisonOperator("CA"));
		assertEquals("CA", ABAP.negateComparisonOperator("NA"));
		
		assertEquals("NS", ABAP.negateComparisonOperator("CS"));
		assertEquals("CS", ABAP.negateComparisonOperator("NS"));
		
		assertEquals("NP", ABAP.negateComparisonOperator("CP"));
		assertEquals("CP", ABAP.negateComparisonOperator("NP"));

		assertEquals("BYTE-CN", ABAP.negateComparisonOperator("BYTE-CO"));
		assertEquals("BYTE-CO", ABAP.negateComparisonOperator("BYTE-CN"));
		
		assertEquals("BYTE-NA", ABAP.negateComparisonOperator("BYTE-CA"));
		assertEquals("BYTE-CA", ABAP.negateComparisonOperator("BYTE-NA"));
		
		assertEquals("BYTE-NS", ABAP.negateComparisonOperator("BYTE-CS"));
		assertEquals("BYTE-CS", ABAP.negateComparisonOperator("BYTE-NS"));

		String[] errorOperators = new String[] { "IN", "BETWEEN", "in", "between", "no-operator" };
		for (String errorOperator : errorOperators) {
			try {
				ABAP.negateComparisonOperator(errorOperator);
				fail("expected operator " + errorOperator + " to raise an exception!"); 
			} catch (IndexOutOfBoundsException ex) {
			}
		}
	}
	
	@Test
	void testReleaseNumberOfReleaseNames() {
		// make sure that getReleaseNumber successfully converts all known release names
		for (String releaseRestrictionName : ABAP.RELEASE_RESTRICTION_NAMES) {
			assertTrue(ABAP.getReleaseRestrictionNumber(releaseRestrictionName ) > 0);
		}
		assertEquals(ABAP.NO_RELEASE_RESTRICTION, ABAP.getReleaseRestrictionNumber(ABAP.NO_RELEASE_RESTRICTION_NAME));
	}
	
	@Test
	void testIsCharAllowedForTypeNames() {
		// deliberately omitting '/' (NAMESPACE_SIGN), which can occur in type names
		String neverAllowed = "!\"'()+,.:;=@[\\]^`{|}~";

		// first char
		assertTrue(ABAP.isCharAllowedForTypeNames("a", 0, true, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("A", 0, true, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("z", 0, true, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("Z", 0, true, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("_", 0, true, true));
		
		assertFalse(ABAP.isCharAllowedForTypeNames("0", 0, true, true));
		assertFalse(ABAP.isCharAllowedForTypeNames("9", 0, true, true));
		// in ABAP cleaner, type names may contain => 
		assertFalse(ABAP.isCharAllowedForTypeNames("=", 0, true, true));
		assertFalse(ABAP.isCharAllowedForTypeNames(">", 0, true, true));

		assertFalse(ABAP.isCharAllowedForTypeNames("!", 0, true, true));
		assertFalse(ABAP.isCharAllowedForTypeNames("?", 0, true, true));

		// '%' is only allowed as first char if followed by '_'
		assertTrue(ABAP.isCharAllowedForTypeNames("%_nn", 0, true, true));
		assertFalse(ABAP.isCharAllowedForTypeNames("%nn", 0, true, true));

		for (int i = 0; i < neverAllowed.length(); ++i) {
			assertFalse(ABAP.isCharAllowedForTypeNames(neverAllowed, i, true, false));
		}

		// non-first char
		assertTrue(ABAP.isCharAllowedForTypeNames("a", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("A", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("z", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("Z", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("_", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("0", 0, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("9", 0, false, true));
		// in ABAP cleaner, type names may contain => 
		assertTrue(ABAP.isCharAllowedForTypeNames("a=>b", 1, false, true));
		assertTrue(ABAP.isCharAllowedForTypeNames("a=>b", 2, false, true));

		assertFalse(ABAP.isCharAllowedForTypeNames("!", 0, false, true));
		assertFalse(ABAP.isCharAllowedForTypeNames("?", 0, false, true));

		for (int i = 0; i < neverAllowed.length(); ++i) {
			assertFalse(ABAP.isCharAllowedForTypeNames(neverAllowed, i, false, false));
		}
	}
	
	@Test
	void testIsCharAllowedForTypeNamesOutsideOO() {
		// deliberately omitting '/' (NAMESPACE_SIGN), which can occur in type names
		String neverAllowed = "!\"'()+,.:;=@[\\]^`{|}~"; 

		// first char
		assertTrue(ABAP.isCharAllowedForTypeNames("%_nn", 0, true, true));

		assertTrue(ABAP.isCharAllowedForTypeNames("%", 0, true, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("$", 0, true, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("*", 0, true, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("?", 0, true, false));
		
		assertTrue(ABAP.isCharAllowedForTypeNames("&", 0, true, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("<", 0, true, false));
		assertFalse(ABAP.isCharAllowedForTypeNames("#", 0, true, false));
		assertTrue(ABAP.isCharAllowedForTypeNames(">", 0, true, false));
		
		for (int i = 0; i < neverAllowed.length(); ++i) {
			assertFalse(ABAP.isCharAllowedForTypeNames(neverAllowed, i, true, false));
		}
		
		// non-first char
		assertTrue(ABAP.isCharAllowedForTypeNames("a%", 1, false, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("a$", 1, false, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("a*", 1, false, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("a?", 1, false, false));

		assertFalse(ABAP.isCharAllowedForTypeNames("a&", 1, false, false));
		assertFalse(ABAP.isCharAllowedForTypeNames("a<", 1, false, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("a#", 1, false, false));
		assertTrue(ABAP.isCharAllowedForTypeNames("a>", 1, false, false));
		assertFalse(ABAP.isCharAllowedForTypeNames("a>b", 1, false, false));

		for (int i = 0; i < neverAllowed.length(); ++i) {
			assertFalse(ABAP.isCharAllowedForTypeNames(neverAllowed, i, false, false));
		}
	}
	
	@Test
	void testIsCharCommonForKeywords() {
		// first char
		assertTrue(ABAP.isCharCommonForKeywords('a', true));
		assertTrue(ABAP.isCharCommonForKeywords('A', true));
		assertTrue(ABAP.isCharCommonForKeywords('z', true));
		assertTrue(ABAP.isCharCommonForKeywords('Z', true));
		// others are not allowed
		assertFalse(ABAP.isCharCommonForKeywords('-', true));
		assertFalse(ABAP.isCharCommonForKeywords('0', true));
		assertFalse(ABAP.isCharCommonForKeywords('9', true));
		assertFalse(ABAP.isCharCommonForKeywords('_', true));
		assertFalse(ABAP.isCharCommonForKeywords('!', true));
		assertFalse(ABAP.isCharCommonForKeywords('?', true));
		assertFalse(ABAP.isCharCommonForKeywords('{', true));

		// non-first char
		assertTrue(ABAP.isCharCommonForKeywords('a', false));
		assertTrue(ABAP.isCharCommonForKeywords('A', false));
		assertTrue(ABAP.isCharCommonForKeywords('z', false));
		assertTrue(ABAP.isCharCommonForKeywords('Z', false));
		assertTrue(ABAP.isCharCommonForKeywords('-', false));
		// 0-9 are allowed, but not common
		assertFalse(ABAP.isCharCommonForKeywords('0', false));
		assertFalse(ABAP.isCharCommonForKeywords('9', false));
		// others are not allowed
		assertFalse(ABAP.isCharCommonForKeywords('_', false));
		assertFalse(ABAP.isCharCommonForKeywords('!', false));
		assertFalse(ABAP.isCharCommonForKeywords('?', false));
	}
	
	@Test
	void testIsCharAllowedForAnyKeyword() {
		// first char
		assertTrue(ABAP.isCharAllowedForAnyKeyword('a', true));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('A', true));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('z', true));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('Z', true));

		assertFalse(ABAP.isCharAllowedForAnyKeyword('-', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('0', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('9', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('_', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('!', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('?', true));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('{', true));

		// non-first char
		assertTrue(ABAP.isCharAllowedForAnyKeyword('a', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('A', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('z', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('Z', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('-', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('0', false));
		assertTrue(ABAP.isCharAllowedForAnyKeyword('9', false));

		assertFalse(ABAP.isCharAllowedForAnyKeyword('_', false));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('!', false));
		assertFalse(ABAP.isCharAllowedForAnyKeyword('?', false));
	}
	
	@Test
	void testIsCharAllowedForIntLiterals() {
		// first char
		assertTrue(ABAP.isCharAllowedForIntLiterals('0', true));
		assertTrue(ABAP.isCharAllowedForIntLiterals('9', true));
		assertTrue(ABAP.isCharAllowedForIntLiterals('-', true));

		assertFalse(ABAP.isCharAllowedForIntLiterals('a', true));
		assertFalse(ABAP.isCharAllowedForIntLiterals('Z', true));
		assertFalse(ABAP.isCharAllowedForIntLiterals('_', true));
		assertFalse(ABAP.isCharAllowedForIntLiterals('.', true));
		assertFalse(ABAP.isCharAllowedForIntLiterals(',', true));

		// non-first char
		assertTrue(ABAP.isCharAllowedForIntLiterals('0', false));
		assertTrue(ABAP.isCharAllowedForIntLiterals('9', false));

		assertFalse(ABAP.isCharAllowedForIntLiterals('-', false));
		assertFalse(ABAP.isCharAllowedForIntLiterals('a', false));
		assertFalse(ABAP.isCharAllowedForIntLiterals('Z', false));
		assertFalse(ABAP.isCharAllowedForIntLiterals('_', false));
		assertFalse(ABAP.isCharAllowedForIntLiterals('.', false));
		assertFalse(ABAP.isCharAllowedForIntLiterals(',', false));
	}

	@Test
	void testReadTillEndOfTypeName() {
		assertEquals(null, ABAP.readTillEndOfTypeName(null, 0, true));
		assertEquals(null, ABAP.readTillEndOfTypeName("", 0, true));
		assertEquals(null, ABAP.readTillEndOfTypeName("TYPE ", 5, true));
		assertEquals("any_type", ABAP.readTillEndOfTypeName("TYPE any_type", 5, true));
		assertEquals("any_type", ABAP.readTillEndOfTypeName("TYPE any_type ", 5, true));
		assertEquals("other_type", ABAP.readTillEndOfTypeName("TYPE other_type.", 5, true));
		assertEquals("if_any_interface=>ty_any_type", ABAP.readTillEndOfTypeName("TYPE if_any_interface=>ty_any_type.", 5, true));
	}
	
	@Test
	void testIsComparisonOperator() {
		assertTrue(ABAP.isComparisonOperator("lt"));
		assertTrue(ABAP.isComparisonOperator("LT"));

		assertTrue(ABAP.isComparisonOperator("ca"));
		assertTrue(ABAP.isComparisonOperator("CA"));

		assertTrue(ABAP.isComparisonOperator("byte-CA"));
		assertTrue(ABAP.isComparisonOperator("BYTE-CA"));
	}

	@Test
	void testParameterNull() {
		// cover null-case branches that are never covered otherwise in tests
		assertFalse(ABAP.isAbapKeywordCollocation(null));
		assertFalse(ABAP.isAbapKeywordCollocation(""));

		assertFalse(ABAP.isAbapKeywordCollocationStart(null));
		assertFalse(ABAP.isAbapKeywordCollocationStart(""));

		assertFalse(ABAP.isInteger(null));
		
		assertFalse(ABAP.isNumeric(null));
		
		assertFalse(ABAP.mayBeVariableName(null, false, true));
		assertFalse(ABAP.mayBeVariableName(null, true, true));
		assertFalse(ABAP.mayBeVariableName("", true, true));
		assertFalse(ABAP.mayBeVariableName("", false, true));

		assertEquals(null, ABAP.readTillEndOfVariableName(null, 0, false, true));
		assertEquals(null, ABAP.readTillEndOfVariableName(null, 0, true, true));
	}

	@Test
	void testDefaultResult() {
		assertEquals("<=", ABAP.getSymbolicComparisonOperator("<="));
		assertEquals(">", ABAP.getSymbolicComparisonOperator(">"));

		assertEquals("LE", ABAP.getTextualComparisonOperator("LE"));
		assertEquals("GT", ABAP.getTextualComparisonOperator("GT"));
	}
	
	@Test
	void testIsNumeric() {
		// dot allowed / not allowed
		assertTrue(ABAP.isNumeric("3.14", true, true));
		assertFalse(ABAP.isNumeric("3.14", true, false));

		// multiple dots
		assertFalse(ABAP.isNumeric("3.141.592", true, true));
		assertFalse(ABAP.isNumeric("3.141.592", true, false));

		// minus at start
		assertTrue(ABAP.isNumeric("-1", true, true));
		assertTrue(ABAP.isNumeric("-1", false, true));
		
		// minus at end allowed / not allowed
		assertTrue(ABAP.isNumeric("1-", true, true));
		assertFalse(ABAP.isNumeric("1-", false, true));

		// minus at start and end
		assertFalse(ABAP.isNumeric("-1-", true, true));
		assertFalse(ABAP.isNumeric("-1-", false, true));
		assertFalse(ABAP.isNumeric("-3.14-", true, true));
		assertFalse(ABAP.isNumeric("-3.14-", false, true));
	}
	
	@Test
	void testIsPositionAndLength() {
		assertFalse(ABAP.isPositionAndLength(null));
		assertFalse(ABAP.isPositionAndLength("1(2"));
		assertFalse(ABAP.isPositionAndLength("1(23"));
		assertFalse(ABAP.isPositionAndLength("1(23)4"));
		assertFalse(ABAP.isPositionAndLength("1)23"));
		assertFalse(ABAP.isPositionAndLength("-1(2)"));
		assertFalse(ABAP.isPositionAndLength("1(-2)"));
		assertFalse(ABAP.isPositionAndLength("(1)"));
		assertFalse(ABAP.isPositionAndLength("(10)"));

		assertTrue(ABAP.isPositionAndLength("0(0)"));
		assertTrue(ABAP.isPositionAndLength("0(1)"));
		assertTrue(ABAP.isPositionAndLength("1(0)"));
		assertTrue(ABAP.isPositionAndLength("1(1)"));
		assertTrue(ABAP.isPositionAndLength("10(10)"));
		// leading zeros are allowed, too: 
		assertTrue(ABAP.isPositionAndLength("01(2)"));
		assertTrue(ABAP.isPositionAndLength("1(02)"));
		assertTrue(ABAP.isPositionAndLength("001(002)"));
	}
	
	@Test
	void testToVariableName() {
		assertEquals("", ABAP.toVariableName(null));
		assertEquals("", ABAP.toVariableName(""));
		assertEquals("abc", ABAP.toVariableName("ABC"));
		
		// expect namespaces to be kept 
		assertEquals("/abcd/efghi", ABAP.toVariableName("/abcd/efghi"));
		assertEquals("/abcdefg/hijk", ABAP.toVariableName("/abcdefg/hijk"));
		
		// expect non-allowed chars to be replaced with '_'
		assertEquals("_ab_cd_", ABAP.toVariableName("1ab+cd!"));
		
		// expect variable name to end after 30 chars
		assertEquals("a123456789b123456789c123456789", ABAP.toVariableName("a123456789b123456789c123456789d123456789"));
	}
	
	
	@Test
	void testGetReleaseRestrictionNumber() {
		assertEquals(ABAP.NO_RELEASE_RESTRICTION, ABAP.getReleaseRestrictionNumber(ABAP.NO_RELEASE_RESTRICTION_NAME));

		assertEquals(750, ABAP.getReleaseRestrictionNumber("7.50"));
		assertEquals(757, ABAP.getReleaseRestrictionNumber("7.57"));

		assertEquals(-1, ABAP.getReleaseRestrictionNumber("7,50"));
		assertEquals(-1, ABAP.getReleaseRestrictionNumber("abc"));
		assertEquals(-1, ABAP.getReleaseRestrictionNumber(""));
	}

	@Test
	void testGetReleaseRestrictionName() {
		assertEquals(ABAP.NO_RELEASE_RESTRICTION_NAME, ABAP.getReleaseRestrictionName(ABAP.NO_RELEASE_RESTRICTION));
		assertEquals(ABAP.NO_RELEASE_RESTRICTION_NAME, ABAP.getReleaseRestrictionName(-1));

		assertEquals("7.50", ABAP.getReleaseRestrictionName(750));
		assertEquals("7.57", ABAP.getReleaseRestrictionName(757));
	}
	
	@Test
	void testIsFieldSymbol() {
		assertFalse(ABAP.isFieldSymbol(null));
		assertFalse(ABAP.isFieldSymbol(""));
		assertFalse(ABAP.isFieldSymbol("<"));
		assertFalse(ABAP.isFieldSymbol(">"));
		assertFalse(ABAP.isFieldSymbol("<<"));
		assertFalse(ABAP.isFieldSymbol(">>"));

		assertTrue(ABAP.isFieldSymbol("<>")); // this really is a valid field symbol...
		assertTrue(ABAP.isFieldSymbol("<any>")); // this really is a valid field symbol...
	}

	void testInferFileName(String expFileName, String... lines) {
		String code = StringUtil.join(ABAP.LINE_SEPARATOR, lines);
		assertEquals(expFileName, ABAP.inferFileNameFromCode(code));
	}
	
	@Test
	void testInferFileNameFromCode() {
		testInferFileName(null, 
				"*&---------------------------------------------------------------------*", 
				"*& Report ",
				"*&---------------------------------------------------------------------*");

		testInferFileName("ANY_REPORT", 
				"*&---------------------------------------------------------------------*", 
				"*& Report ANY_REPORT",
				"*&---------------------------------------------------------------------*");

		testInferFileName("any_class", 
				"CLASS any_class DEFINITION", "  PUBLIC", "  FINAL", "  CREATE PUBLIC.");

		testInferFileName("any_class", 
				"\" comment", 
				"\" more comment", 
				"CLASS any_class DEFINITION", "  PUBLIC", "  FINAL", "  CREATE PUBLIC.");

		testInferFileName("ANY_AU_INCL_ANY", 
				"*&---------------------------------------------------------------------*", 
				"*& Include          ANY_AU_INCL_ANY.", 
				"*&---------------------------------------------------------------------*");

		testInferFileName("ANY_MODULE", 
				"*&---------------------------------------------------------------------*", 
				"*& Modulpool         ANY_MODULE                                        *", 
				"*&                                                                     *",
				"*&---------------------------------------------------------------------*"
				);

		testInferFileName("ANY_INCLUDE", 
				"************************************************************************", 
				"***INCLUDE ANY_INCLUDE", 
				"************************************************************************");
	}
	
	@Test
	void testMayBeVariableName() {
		// all of the following are VALID usages of namespaces, i.e. they could be used as
		// identifiers for variable, method, structure, component names etc. 
		assertTrue(ABAP.mayBeVariableName("/abc/def", false, true));
		assertTrue(ABAP.mayBeVariableName("/123/", false, true));
		assertTrue(ABAP.mayBeVariableName("/123/abc", false, true));
		assertTrue(ABAP.mayBeVariableName("/123456890/abc", false, true));
		assertTrue(ABAP.mayBeVariableName("/1234568901234567890123456789/", false, true));
		assertTrue(ABAP.mayBeVariableName("a/abc/a/abcedfg/", false, true));
		assertTrue(ABAP.mayBeVariableName("abcd/123/a/12345/", false, true));
		assertTrue(ABAP.mayBeVariableName("/123//456/", false, true));
		assertTrue(ABAP.mayBeVariableName("/___/", false, true));
		assertTrue(ABAP.mayBeVariableName("/___/_/___/", false, true));
		assertTrue(ABAP.mayBeVariableName("_/123//___//abc/", false, true));

		// all of the following are INVALID usages of namespaces (see the rules in ABAP.mayBeVariableName()):
		assertFalse(ABAP.mayBeVariableName("/12/", false, true)); // namespace /12/ too short
		assertFalse(ABAP.mayBeVariableName("/ab/", false, true)); // namespace /ab/ too short
		assertFalse(ABAP.mayBeVariableName("/abc/def/gh/", false, true)); // namespace /gh/ too short
		assertFalse(ABAP.mayBeVariableName("/abc/def/ghi", false, true)); // final / not closed
		assertFalse(ABAP.mayBeVariableName("/abc//def//ghi", false, true)); // final / not closed
		assertFalse(ABAP.mayBeVariableName("/12345689012345678901234567890/", false, true)); // too long
	}

	@Test
	void testMayBeVariableNameForFieldSymbol() {
		assertTrue(ABAP.mayBeVariableName("<>", true, true)); 
		assertTrue(ABAP.mayBeVariableName("<_>", true, true)); 
		assertTrue(ABAP.mayBeVariableName("<a>", true, true)); 
		assertTrue(ABAP.mayBeVariableName("<ls_any>", true, true)); 
		assertTrue(ABAP.mayBeVariableName("<ls_any>", true, true)); 
		assertTrue(ABAP.mayBeVariableName("<1234567890123456789012345678>", true, true)); 

		// all of the following are INVALID as field symbols
		assertFalse(ABAP.mayBeVariableName("<a", true, true)); 
		assertFalse(ABAP.mayBeVariableName("a>", true, true)); 
		assertFalse(ABAP.mayBeVariableName("<a<", true, true)); 
		assertFalse(ABAP.mayBeVariableName(">a>", true, true)); 
		assertFalse(ABAP.mayBeVariableName("<a<b>", true, true)); 
		assertFalse(ABAP.mayBeVariableName("<a>b>", true, true)); 
		assertFalse(ABAP.mayBeVariableName("a<b", true, true)); 
		assertFalse(ABAP.mayBeVariableName("a>b", true, true)); 
		assertFalse(ABAP.mayBeVariableName("</>", true, true)); // '/' not allowed in FIELD-SYMBOLS
		assertFalse(ABAP.mayBeVariableName("<a/>", true, true)); // '/' not allowed in FIELD-SYMBOLS
	}

	@Test 
	void testUnescapeCharLiteral() {
		assertNull(ABAP.unescapeCharLiteral(""));
		assertNull(ABAP.unescapeCharLiteral("'"));
		
		assertEquals("'abc", ABAP.unescapeCharLiteral("'''abc'"));
		assertEquals("abc'", ABAP.unescapeCharLiteral("'abc'''"));
		assertEquals("`abc|", ABAP.unescapeCharLiteral("'`abc|'"));

		assertEquals("`abc", ABAP.unescapeCharLiteral("```abc`"));
		assertEquals("abc`", ABAP.unescapeCharLiteral("`abc```"));
		assertEquals("'abc|{}", ABAP.unescapeCharLiteral("`'abc|{}`"));

		assertEquals("|abc{}", ABAP.unescapeCharLiteral("|\\|abc\\{\\}|"));
		assertEquals("a\tb\rc\n", ABAP.unescapeCharLiteral("|a\\tb\\rc\\n|"));
		assertEquals("`'abc", ABAP.unescapeCharLiteral("|`'abc|"));
	}

	@Test 
	void testToFieldLiteral() {
		assertEquals("'abc'", ABAP.toTextFieldLiteral("abc"));
		assertEquals("'''abc'", ABAP.toTextFieldLiteral("'abc"));
		assertEquals("'a''bc'", ABAP.toTextFieldLiteral("a'bc"));
		assertEquals("'''abc'''", ABAP.toTextFieldLiteral("'abc'"));
		assertEquals("'`abc|{}'", ABAP.toTextFieldLiteral("`abc|{}"));
	}

	@Test
	void testSplitIdentifierEmpty() {
		assertEquals(0, ABAP.splitIdentifier(null, true, true).size());
		assertEquals(0, ABAP.splitIdentifier("", true, true).size());
	}

	@Test
	void testNegateIntegerErr() {
		assertNull(ABAP.negateInteger(""));
		assertNull(ABAP.negateInteger("'3.1415'"));
		assertNull(ABAP.negateInteger("xyz"));
	}
	
	@Test
	void testIsAbapUpperCaseKeyword() {
		assertTrue(ABAP.isAbapUpperCaseKeyword("CONDENSE"));
		assertFalse(ABAP.isAbapUpperCaseKeyword("condense("));
		assertFalse(ABAP.isAbapUpperCaseKeyword("CONDENSE("));
	}
}
