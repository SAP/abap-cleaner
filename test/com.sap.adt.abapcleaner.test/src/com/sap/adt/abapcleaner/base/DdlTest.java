package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;

public class DdlTest {
	@Test
	void testParameterNull() {
		assertFalse(DDL.isNumeric(null, true));
		assertFalse(DDL.isComparisonOperator(null));
		assertFalse(DDL.isDdlKeyword(null));
		assertFalse(DDL.isBuiltInDdlFunction(null));
		assertFalse(DDL.isCharAllowedForIdentifier(null, 0, false));
		assertFalse(DDL.isCharAllowedForIdentifier("abc", 3, false)); // pos too high
	}
	
	@Test
	void testIsNumeric() {
		// dot allowed / not allowed
		assertTrue(DDL.isNumeric("3.14", true));
		assertFalse(DDL.isNumeric("3.14", false));

		// multiple dots
		assertFalse(DDL.isNumeric("3.141.592", true));
		assertFalse(DDL.isNumeric("3.141.592", false));

		// minus at start / end
		assertTrue(DDL.isNumeric("-1", true));
		assertFalse(DDL.isNumeric("1-", true));

		// minus at start and end
		assertFalse(DDL.isNumeric("-1-", true));
		assertFalse(DDL.isNumeric("-3.14-", true));
	}

	@Test
	void testIsComparisonOperator() {
		assertTrue(DDL.isComparisonOperator("<"));
		assertTrue(DDL.isComparisonOperator("<="));
		assertTrue(DDL.isComparisonOperator("="));
		assertTrue(DDL.isComparisonOperator(">="));
		assertTrue(DDL.isComparisonOperator(">"));
		assertTrue(DDL.isComparisonOperator("<>"));
		assertTrue(DDL.isComparisonOperator("!=")); // "!=" is not listed in the documentation, but it works (unlike "==")

		assertFalse(DDL.isComparisonOperator("LT"));
		assertFalse(DDL.isComparisonOperator("EQ"));
	}

	@Test
	void testIsKeyword() {
		assertTrue(DDL.isDdlKeyword("define"));
		assertTrue(DDL.isDdlKeyword("DEFINE"));
		assertTrue(DDL.isDdlKeyword("Define"));
		assertTrue(DDL.isDdlKeyword("DeFiNe"));
		
		assertTrue(DDL.isDdlKeyword("association"));
		assertTrue(DDL.isDdlKeyword("projection"));
		assertTrue(DDL.isDdlKeyword("ABSTRACT"));
		assertTrue(DDL.isDdlKeyword("Returns"));
		assertTrue(DDL.isDdlKeyword("descending"));
		assertTrue(DDL.isDdlKeyword("INCLUDE"));

		assertFalse(DDL.isDdlKeyword("MOVE-CORRESPONDING"));
		assertFalse(DDL.isDdlKeyword("report"));
		assertFalse(DDL.isDdlKeyword("Class"));
	}

	@Test
	void testIsBuiltInFunction() {
		assertTrue(DDL.isBuiltInDdlFunction("coalesce"));
		assertTrue(DDL.isBuiltInDdlFunction("COALESCE"));
		assertTrue(DDL.isBuiltInDdlFunction("Coalesce"));
		assertTrue(DDL.isBuiltInDdlFunction("cOaLesCe"));
		
		assertTrue(DDL.isBuiltInDdlFunction("concat"));
		assertTrue(DDL.isBuiltInDdlFunction("curr_to_decfloat_amount"));
		assertTrue(DDL.isBuiltInDdlFunction("dats_days_between"));
		assertTrue(DDL.isBuiltInDdlFunction("DIVISION"));
		assertTrue(DDL.isBuiltInDdlFunction("Left"));
		assertTrue(DDL.isBuiltInDdlFunction("RiGhT"));
		assertTrue(DDL.isBuiltInDdlFunction("SUBSTRING"));
		assertTrue(DDL.isBuiltInDdlFunction("fiscal_calendar_shift"));

		assertFalse(DDL.isBuiltInDdlFunction("lines"));
		assertFalse(DDL.isBuiltInDdlFunction("XSDBOOL"));
		assertFalse(DDL.isBuiltInDdlFunction("Count_Any_Not_Of"));
	}
	
	@Test
	void testIsCharAllowedForAnyKeyword() {
		// as any (first or non-first) char
		assertTrue(DDL.isCharAllowedForAnyKeyword('a'));
		assertTrue(DDL.isCharAllowedForAnyKeyword('z'));
		assertTrue(DDL.isCharAllowedForAnyKeyword('A'));
		assertTrue(DDL.isCharAllowedForAnyKeyword('Z'));
		assertTrue(DDL.isCharAllowedForAnyKeyword('_'));
		
		assertFalse(DDL.isCharAllowedForAnyKeyword('$')); // only allowed as first char
		assertFalse(DDL.isCharAllowedForAnyKeyword('0'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('9'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('<'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('>'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('!'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('\0'));
		assertFalse(DDL.isCharAllowedForAnyKeyword('\u00a0'));

		// as first char
		assertTrue(DDL.isCharAllowedForAnyKeyword('a', true));
		assertTrue(DDL.isCharAllowedForAnyKeyword('Z', true));
		assertTrue(DDL.isCharAllowedForAnyKeyword('_', true));
		assertTrue(DDL.isCharAllowedForAnyKeyword('$', true));
		
		assertFalse(DDL.isCharAllowedForAnyKeyword('5', true));
		assertFalse(DDL.isCharAllowedForAnyKeyword('=', true));

		// as non-first char
		assertTrue(DDL.isCharAllowedForAnyKeyword('a', false));
		assertTrue(DDL.isCharAllowedForAnyKeyword('Z', false));
		assertTrue(DDL.isCharAllowedForAnyKeyword('_', false));
		assertFalse(DDL.isCharAllowedForAnyKeyword('$', false));

		assertFalse(DDL.isCharAllowedForAnyKeyword('5', false));
		assertFalse(DDL.isCharAllowedForAnyKeyword('=', false));
	}
	
	@Test
	void testIsCharAllowedForIdentifier() {
		// as first char
		assertTrue(DDL.isCharAllowedForIdentifier("m?", 0, true));
		assertTrue(DDL.isCharAllowedForIdentifier("N,", 0, true));
		assertTrue(DDL.isCharAllowedForIdentifier("_;", 0, true));
		assertTrue(DDL.isCharAllowedForIdentifier("$:", 0, true));
		assertTrue(DDL.isCharAllowedForIdentifier("#+", 0, true));
		assertTrue(DDL.isCharAllowedForIdentifier("@-", 0, true));
		
		assertFalse(DDL.isCharAllowedForIdentifier("0*", 0, true));
		assertFalse(DDL.isCharAllowedForIdentifier("9/", 0, true));
		assertFalse(DDL.isCharAllowedForIdentifier("9\0", 1, true));
		assertFalse(DDL.isCharAllowedForIdentifier("9\u00a0", 1, true));

		// as non-first char
		assertTrue(DDL.isCharAllowedForIdentifier("a!", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("z?", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("A.", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("Z,", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("_;", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("0*", 0, false));
		assertTrue(DDL.isCharAllowedForIdentifier("9/", 0, false));
		
		assertFalse(DDL.isCharAllowedForIdentifier("$:", 0, false));
		assertFalse(DDL.isCharAllowedForIdentifier("#+", 0, false));
		assertFalse(DDL.isCharAllowedForIdentifier("@-", 0, false));
		
		assertFalse(DDL.isCharAllowedForIdentifier("a!", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("z?", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("A.", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("Z,", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("_;", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("$:", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("#+", 1, false));
		assertFalse(DDL.isCharAllowedForIdentifier("@-", 1, false));
	}
	
	private void assertKnownCollocation(String keywordSequence, String parentFunction, Language language) {
		ArrayList<String> keywords = new ArrayList<String>();
		boolean expTrue = false;
		boolean expFalse = false;
		int mainIndex = -1;
		int index = 0;
		for (String keyword : StringUtil.split(keywordSequence, ' ', false)) {
			if (keyword.startsWith("+")) {
				expTrue = true;
				mainIndex = index;
				keywords.add(keyword.substring(1));
			} else if (keyword.startsWith("-")) {
				expFalse = true;
				mainIndex = index;
				keywords.add(keyword.substring(1));
			} else {
				keywords.add(keyword);
			}
			++index;
		}
		if (expTrue == expFalse) 
			fail(); // the supplied keywordSequence must contain exactly one "+" OR exactly one "-"
		
		assertEquals(expTrue, DDL.isKnownCollocation(keywords, mainIndex, parentFunction, language));
	}

	@Test
	void testIsKnownCollocation() {
		// known collocations
		assertKnownCollocation("as +select from", null, Language.DDL);
		assertKnownCollocation("root custom +entity", null, Language.DDL);
		assertKnownCollocation("as parent +child hierarchy", null, Language.DDL);
		assertKnownCollocation("+min (", null, Language.DDL);
		assertKnownCollocation("multiple +parents allowed", "hierarchy", Language.DDL);

		// unknown collocations
		assertKnownCollocation("-multiple parents allowed", null, Language.DDL); // parent function must be "hierarchy"
		assertKnownCollocation("-multiple parents allowed", "other_function", Language.DDL); // parent function unknown
		assertKnownCollocation("provider -contract unknown_keyword", null, Language.DDL); // unknown third keyword
		assertKnownCollocation("provider -contract", null, Language.DDL); // third keyword missing
		assertKnownCollocation("-root", null, Language.DDL); // "root" never appears stand-alone
		assertKnownCollocation("by -method", null, Language.DDL); // "implemented" missing
	}
}
