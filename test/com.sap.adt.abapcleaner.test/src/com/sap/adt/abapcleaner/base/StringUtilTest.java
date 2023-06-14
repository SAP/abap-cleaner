package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

class StringUtilTest {
	private void assertStringArrayEquals(String[] exp, String[] act) {
		assertEquals(exp.length, act.length);
		for (int i = 0; i < exp.length; ++i)
			assertEquals(exp[i], act[i]);
	}

	@Test
	void testInstrCount() {
		assertEquals(StringUtil.instrCount(null, ' '), 0);
		assertEquals(StringUtil.instrCount("", ' '), 0);
		assertEquals(StringUtil.instrCount("a, b, c", ','), 2);
		assertEquals(StringUtil.instrCount("a\r\nb\r\nc\r\n", '\n'), 3);
	}
	
	@Test
	void testSpaceCountAtStartOf() {
		assertEquals(0, StringUtil.spaceCountAtStartOf(null));
		assertEquals(0, StringUtil.spaceCountAtStartOf(""));
		assertEquals(0, StringUtil.spaceCountAtStartOf("abc"));
		
		assertEquals(1, StringUtil.spaceCountAtStartOf(" abc"));
		assertEquals(5, StringUtil.spaceCountAtStartOf("     abc"));
		
		// expect tabs to be counted like spaces
		assertEquals(1, StringUtil.spaceCountAtStartOf("\tabc"));
		assertEquals(3, StringUtil.spaceCountAtStartOf("\t\t\tabc"));
		assertEquals(5, StringUtil.spaceCountAtStartOf("\t  \t abc"));
	}
	
	void testSplit(String text, String separator, boolean removeEmptyEntries, String... expResults) {
		String[] actResults = StringUtil.split(text, separator, removeEmptyEntries);
		assertStringArrayEquals(expResults, actResults);
	}
	
	@Test
	void testSplitWithStringSeparator() {
		// null is expected only if text is null
		assertEquals(null, StringUtil.split(null, " ", true));
		assertEquals(null, StringUtil.split(null, " ", false));
		assertEquals(0, StringUtil.split("", " ", true).length);
		assertEquals(1, StringUtil.split("", " ", false).length);
		
		// expected values are now at the END of the following calls (starting from fourth parameter):
		testSplit("", " ", false, "");

		testSplit("a|bb|c", "|", false, "a", "bb", "c");
		testSplit("a|bb|c", "|", true, "a", "bb", "c");
		testSplit("a\r\nbb\r\nc", "\r\n", false, "a", "bb", "c");
		testSplit("a\r\nbb\r\nc", "\r\n", true, "a", "bb", "c");

		testSplit("|a||bb|", "|", false, "", "a", "", "bb", "");
		testSplit("|a||bb|", "|", true, "a", "bb");
		testSplit("\r\na\r\n\r\nbb\r\n", "\r\n", false, "", "a", "", "bb", "");
		testSplit("\r\na\r\n\r\nbb\r\n", "\r\n", true, "a", "bb");
	}

	void testSplit(String text, char separator, boolean removeEmptyEntries, String... expResults) {
		String[] actResults = StringUtil.split(text, separator, removeEmptyEntries);
		assertStringArrayEquals(expResults, actResults);
	}
	
	@Test
	void testSplitWithCharSeparator() {
		// null is expected only if text is null
		assertEquals(null, StringUtil.split(null, ' ', true));
		assertEquals(null, StringUtil.split(null, ' ', false));

		assertEquals(0, StringUtil.split("", ' ', true).length);
		assertEquals(1, StringUtil.split("", ' ', false).length);

		// expected values are now at the END of the following calls (starting from fourth parameter):
		testSplit("", ' ', false, "");

		testSplit("a|bb|c", '|', false, "a", "bb", "c");
		testSplit("a|bb|c", '|', true, "a", "bb", "c");

		testSplit("|a||bb|", '|', false, "", "a", "", "bb", "");
		testSplit("|a||bb|", '|', true, "a", "bb");
	}

	void testSplit(String text, char[] separators, boolean removeEmptyEntries, String... expResults) {
		String[] actResults = StringUtil.split(text, separators, removeEmptyEntries);
		assertStringArrayEquals(expResults, actResults);
	}
	
	@Test
	void testSplitWithCharArraySeparator() {
		char[] separators = new char[] { ' ', '|' };
		
		// null is expected only if text is null
		assertEquals(null, StringUtil.split(null, separators, true));
		assertEquals(null, StringUtil.split(null, separators, false));
		assertEquals(0, StringUtil.split("", separators, true).length);
		assertEquals(1, StringUtil.split("", separators, false).length);
		
		// expected values are now at the END of the following calls (starting from fourth parameter):
		testSplit("", separators, false, "");

		testSplit("a bb|c", separators, false, "a", "bb", "c");
		testSplit("a bb|c", separators, true, "a", "bb", "c");

		testSplit(" a |bb ", separators, false, "", "a", "", "bb", "");
		testSplit("|a| bb|", separators, false, "", "a", "", "bb", "");
		testSplit(" a |bb ", separators, true, "a", "bb");
		testSplit("|a| bb|", separators, true, "a", "bb");
	}

	@Test
	void testStartsWith() {
		// case-sensitive tests
		assertTrue(StringUtil.startsWith("Abc", "", false));
		assertTrue(StringUtil.startsWith("Abc", "A", false));
		assertTrue(StringUtil.startsWith("Abc", "Abc", false));
		
		assertFalse(StringUtil.startsWith("Abc", "a", false));
		assertFalse(StringUtil.startsWith("Abc", " ", false));
		assertFalse(StringUtil.startsWith("Abc", "Abcd", false));

		// tests ignoring case
		assertTrue(StringUtil.startsWith("Abc", "", true));
		assertTrue(StringUtil.startsWith("Abc", "A", true));
		assertTrue(StringUtil.startsWith("Abc", "a", true));
		assertTrue(StringUtil.startsWith("Abc", "Abc", true));
		assertTrue(StringUtil.startsWith("Abc", "aBC", true));
		assertTrue(StringUtil.startsWith("Abc", "ABC", true));
		
		assertFalse(StringUtil.startsWith("Abc", " ", true));
		assertFalse(StringUtil.startsWith("Abc", "Abcd", true));
	}

	@Test
	void testEndsWith() {
		// case-sensitive tests
		assertTrue(StringUtil.endsWith("abC", "", false));
		assertTrue(StringUtil.endsWith("abC", "C", false));
		assertTrue(StringUtil.endsWith("abC", "abC", false));
		
		assertFalse(StringUtil.endsWith("abC", "c", false));
		assertFalse(StringUtil.endsWith("abC", " ", false));
		assertFalse(StringUtil.endsWith("abC", " abC", false));

		// tests ignoring case
		assertTrue(StringUtil.endsWith("abC", "", true));
		assertTrue(StringUtil.endsWith("abC", "C", true));
		assertTrue(StringUtil.endsWith("abC", "c", true));
		assertTrue(StringUtil.endsWith("abC", "abC", true));
		assertTrue(StringUtil.endsWith("abC", "ABc", true));
		assertTrue(StringUtil.endsWith("abC", "ABC", true));
		
		assertFalse(StringUtil.endsWith("abC", " ", true));
		assertFalse(StringUtil.endsWith("abC", " abC", true));
	}
	
	@Test
	void testSuffixCount() {
		assertEquals(0, StringUtil.suffixCount(null, null, false));
		assertEquals(0, StringUtil.suffixCount("", "", false));
		assertEquals(0, StringUtil.suffixCount(null, "a", false));
		assertEquals(0, StringUtil.suffixCount("", "a", false));
		assertEquals(0, StringUtil.suffixCount("a", null, false));
		assertEquals(0, StringUtil.suffixCount("a", "", false));

		assertEquals(4, StringUtil.suffixCount("    ", " ", false));
		assertEquals(2, StringUtil.suffixCount("    ", "  ", false));
		assertEquals(1, StringUtil.suffixCount("    ", "    ", false));
		assertEquals(0, StringUtil.suffixCount("    ", "     ", false));

		// case-sensitive tests
		assertEquals(0, StringUtil.suffixCount("A", "++", false));
		assertEquals(1, StringUtil.suffixCount("A++", "++", false));
		assertEquals(1, StringUtil.suffixCount("A+++", "++", false));
		assertEquals(2, StringUtil.suffixCount("A++++", "++", false));
		assertEquals(0, StringUtil.suffixCount("ABCbc", "BC", false));
		assertEquals(1, StringUtil.suffixCount("AbcBC", "BC", false));
		assertEquals(2, StringUtil.suffixCount("ABCBC", "BC", false));

		// tests ignoring case
		assertEquals(2, StringUtil.suffixCount("ABCbc", "BC", true));
		assertEquals(2, StringUtil.suffixCount("AbcBC", "BC", true));
		assertEquals(2, StringUtil.suffixCount("ABCBC", "BC", true));
	}
	
	@Test
	void testRepeatChar() {
		assertEquals("", StringUtil.repeatChar('a', 0));
		assertEquals("a", StringUtil.repeatChar('a', 1));
		assertEquals("aaa", StringUtil.repeatChar('a', 3));
	}
	
	@Test
	void testTrimStart() {
		assertEquals(null, StringUtil.trimStart(null));
		
		assertEquals("", StringUtil.trimStart(""));
		assertEquals("", StringUtil.trimStart("  "));
		assertEquals("abc", StringUtil.trimStart("abc"));
		assertEquals("abc", StringUtil.trimStart(" abc"));
		assertEquals("abc ", StringUtil.trimStart("   abc "));
		assertEquals("abc\t", StringUtil.trimStart("\t  \t abc\t"));
	}
	
	@Test
	void testTrimStartWithChar() {
		assertEquals(null, StringUtil.trimStart(null, '-'));
		
		assertEquals("", StringUtil.trimStart("", '-'));
		assertEquals("", StringUtil.trimStart("--", '-'));
		assertEquals("abc", StringUtil.trimStart("abc", '-'));
		assertEquals("abc", StringUtil.trimStart("-abc", '-'));
		assertEquals("abc-", StringUtil.trimStart("---abc-", '-'));
		assertEquals("=--=abc-", StringUtil.trimStart("--=--=abc-", '-'));
	}

	@Test
	void testTrimEnd() {
		assertEquals(null, StringUtil.trimEnd(null));
		
		assertEquals("", StringUtil.trimEnd(""));
		assertEquals("", StringUtil.trimEnd("  "));
		assertEquals("abc", StringUtil.trimEnd("abc"));
		assertEquals("abc", StringUtil.trimEnd("abc "));
		assertEquals(" abc", StringUtil.trimEnd(" abc   "));
		assertEquals("\tabc", StringUtil.trimEnd("\tabc\t  \t "));
	}
	
	@Test
	void testTrimEndWithChar() {
		assertEquals(null, StringUtil.trimEnd(null, '-'));
		
		assertEquals("", StringUtil.trimEnd("", '-'));
		assertEquals("", StringUtil.trimEnd("--", '-'));
		assertEquals("abc", StringUtil.trimEnd("abc", '-'));
		assertEquals("abc", StringUtil.trimEnd("abc-", '-'));
		assertEquals(" abc", StringUtil.trimEnd(" abc---", '-'));
		assertEquals("-abc=--=", StringUtil.trimEnd("-abc=--=--", '-'));
	}
	
	@Test
	void testTrimEndWithText() {
		assertEquals(null, StringUtil.trimEnd(null, "++"));
		
		assertEquals("", StringUtil.trimEnd("", "++"));
		assertEquals("", StringUtil.trimEnd("++++", "++"));
		assertEquals("abc", StringUtil.trimEnd("abc", "++"));
		assertEquals("abc", StringUtil.trimEnd("abc++", "++"));
		assertEquals("++abc", StringUtil.trimEnd("++abc++++++", "++"));
		assertEquals("++abc+", StringUtil.trimEnd("++abc+++++", "++"));
		assertEquals("++abc==--", StringUtil.trimEnd("++abc==--++++", "++"));
	}
	
	@Test
	void testIsNullOrEmpty() {
		assertTrue(StringUtil.isNullOrEmpty(null));
		assertTrue(StringUtil.isNullOrEmpty(""));

		assertFalse(StringUtil.isNullOrEmpty(" "));
		assertFalse(StringUtil.isNullOrEmpty("\t"));
		assertFalse(StringUtil.isNullOrEmpty("a"));
		assertFalse(StringUtil.isNullOrEmpty("abc"));
	}
	
	@Test
	void testJoin() {
		String[] strings = new String[] { "a", "bb", "c" };
		String[] stringsWithNull = new String[] { "a", null, "b" };
		
		assertEquals(null, StringUtil.join(null, null));
		assertEquals("abbc", StringUtil.join(null, strings));
		assertEquals("a bb c", StringUtil.join(" ", strings));
		assertEquals("a|bb|c", StringUtil.join("|", strings));
		assertEquals("a, bb, c", StringUtil.join(", ", strings));

		assertEquals("a  b", StringUtil.join(" ", stringsWithNull));
		assertEquals("a||b", StringUtil.join("|", stringsWithNull));
		assertEquals("a, , b", StringUtil.join(", ", stringsWithNull));
}
	
	@Test
	void testIndexOfAny() {
		char[] matchChars = new char[] { ' ', '|' };
		
		assertEquals(-1, StringUtil.indexOfAny(null, matchChars, 0));
		assertEquals(-1, StringUtil.indexOfAny("", matchChars, 0));

		// startIndex = 0
		assertEquals(0, StringUtil.indexOfAny(" a b c ", matchChars, 0));
		assertEquals(0, StringUtil.indexOfAny("|a b|c ", matchChars, 0));
		assertEquals(2, StringUtil.indexOfAny("a, b, c", matchChars, 0));
		assertEquals(2, StringUtil.indexOfAny("a,|b, c", matchChars, 0));
		assertEquals(3, StringUtil.indexOfAny("abc ", matchChars, 0));
		assertEquals(3, StringUtil.indexOfAny("abc|", matchChars, 0));
		assertEquals(-1, StringUtil.indexOfAny("abc", matchChars, 0));

		// startIndex > 0
		assertEquals(2, StringUtil.indexOfAny(" a b|c ", matchChars, 2));
		assertEquals(4, StringUtil.indexOfAny(" a b|c ", matchChars, 3));
		assertEquals(6, StringUtil.indexOfAny(" a b|c ", matchChars, 6));
		assertEquals(-1, StringUtil.indexOfAny(" a b|c ", matchChars, 7));
	}
	
	@Test
	void testIndexOfAnyWithSkip() {
		char[] matchChars = new char[] { '.', ',' };
		String[] skipTexts = new String[] { "(.)", "(,)" };
		
		assertEquals(-1, StringUtil.indexOfAny(null, matchChars, 0, skipTexts));
		assertEquals(-1, StringUtil.indexOfAny("", matchChars, 0, skipTexts));

		// startIndex = 0
		assertEquals(0, StringUtil.indexOfAny(".abc", matchChars, 0, skipTexts));
		assertEquals(0, StringUtil.indexOfAny(",abc", matchChars, 0, skipTexts));
		assertEquals(4, StringUtil.indexOfAny("(.)a.b,c", matchChars, 0, skipTexts));
		assertEquals(4, StringUtil.indexOfAny("(,)a,b.c", matchChars, 0, skipTexts));
		assertEquals(9, StringUtil.indexOfAny("(.)(,)abc.", matchChars, 0, skipTexts));
		assertEquals(9, StringUtil.indexOfAny("(.)(,)abc,", matchChars, 0, skipTexts));
		assertEquals(-1, StringUtil.indexOfAny("(.)(,)abc(.)", matchChars, 0, skipTexts));

		// startIndex > 0
		assertEquals(4, StringUtil.indexOfAny("(,)a,b.c", matchChars, 3, skipTexts));
		assertEquals(4, StringUtil.indexOfAny("(,)a,b.c", matchChars, 4, skipTexts));
		assertEquals(6, StringUtil.indexOfAny("(,)a,b.c", matchChars, 6, skipTexts));
		assertEquals(-1, StringUtil.indexOfAny("(,)a,b.c", matchChars, 7, skipTexts));
	}
	
	@Test
	void testArrayListToStringArray() {
		ArrayList<String> arrayList = new ArrayList<String>();
		arrayList.add("");
		arrayList.add("a");
		arrayList.add("bb");

		assertEquals(null, StringUtil.toStringArray(null));
		assertEquals(null, StringUtil.toStringArray(new ArrayList<String>()));
		assertStringArrayEquals(new String[] { "", "a", "bb" }, StringUtil.toStringArray(arrayList));
	}
	
	@Test
	void testListToStringArray() {
		List<String> emptyArrayList = new ArrayList<String>();
		List<String> arrayList = new ArrayList<String>();
		arrayList.add("");
		arrayList.add("a");
		arrayList.add("bb");

		assertEquals(null, StringUtil.toStringArray(null));
		assertEquals(null, StringUtil.toStringArray(emptyArrayList));
		assertStringArrayEquals(new String[] { "", "a", "bb" }, StringUtil.toStringArray(arrayList));
	}
	
	@Test
	void testEqualsCheckingForNull() {
		assertTrue(StringUtil.equalsCheckingForNull(null, null));
		assertTrue(StringUtil.equalsCheckingForNull("", ""));
		assertTrue(StringUtil.equalsCheckingForNull("a", "a"));

		assertFalse(StringUtil.equalsCheckingForNull(null, ""));
		assertFalse(StringUtil.equalsCheckingForNull("", null));
		assertFalse(StringUtil.equalsCheckingForNull("a", "A"));
		assertFalse(StringUtil.equalsCheckingForNull("a", "b"));
	}
	
	@Test
	void testEqualsIgnoreCaseCheckingForNull() {
		assertTrue(StringUtil.equalsIgnoreCaseCheckingForNull(null, null));
		assertTrue(StringUtil.equalsIgnoreCaseCheckingForNull("", ""));
		assertTrue(StringUtil.equalsIgnoreCaseCheckingForNull("a", "a"));
		assertTrue(StringUtil.equalsIgnoreCaseCheckingForNull("a", "A"));

		assertFalse(StringUtil.equalsIgnoreCaseCheckingForNull(null, ""));
		assertFalse(StringUtil.equalsIgnoreCaseCheckingForNull("", null));
		assertFalse(StringUtil.equalsIgnoreCaseCheckingForNull("a", "b"));
	}
	
	@Test
	void testInferLineSeparator() {
		assertEquals(System.lineSeparator(), StringUtil.inferLineSeparator(null));
		assertEquals(System.lineSeparator(), StringUtil.inferLineSeparator(""));
		assertEquals(System.lineSeparator(), StringUtil.inferLineSeparator("abc"));
		
		assertEquals("\n", StringUtil.inferLineSeparator("\n"));
		assertEquals("\n", StringUtil.inferLineSeparator("abc\ndef\n"));
		assertEquals("\r\n", StringUtil.inferLineSeparator("\r\n"));
		assertEquals("\r\n", StringUtil.inferLineSeparator("abc\r\ndef\r\n"));
	}
	
	@Test
	void testGetEscapeText() {
		assertEquals(null, StringUtil.getEscapeText(null));
		assertEquals("", StringUtil.getEscapeText(""));
		
		assertEquals("abc", StringUtil.getEscapeText("abc"));
		assertEquals("a\\\\bc", StringUtil.getEscapeText("a\\bc"));
		assertEquals("a\\\"bc", StringUtil.getEscapeText("a\"bc"));
		assertEquals("a\\\"bc\\\"", StringUtil.getEscapeText("a\"bc\""));
	}
	
	@Test
	void testGetUnescapedText() {
		assertEquals(null, StringUtil.getUnescapedText(null));
		assertEquals("", StringUtil.getUnescapedText(""));
		
		assertEquals("abc", StringUtil.getUnescapedText("abc"));
		assertEquals("a\\bc", StringUtil.getUnescapedText("a\\\\bc"));
		assertEquals("a\"bc", StringUtil.getUnescapedText("a\\\"bc"));
		assertEquals("a\"bc\"", StringUtil.getUnescapedText("a\\\"bc\\\""));
		assertEquals("abc\\", StringUtil.getUnescapedText("abc\\"));
	}
	
	@Test
	void testReadTillEndOfAllowedChars() {
		assertEquals(null, StringUtil.readTillEndOfAllowedChars(null, 0, "abc"));
		assertEquals(null, StringUtil.readTillEndOfAllowedChars("", 0, "abc"));
		assertEquals(null, StringUtil.readTillEndOfAllowedChars("", 1, "abc"));
		assertEquals(null, StringUtil.readTillEndOfAllowedChars("abc", 4, "abc"));

		assertEquals("", StringUtil.readTillEndOfAllowedChars("def", 0, "abc"));
		assertEquals("a", StringUtil.readTillEndOfAllowedChars("a", 0, "abc"));
		assertEquals("abbc", StringUtil.readTillEndOfAllowedChars("abbc", 0, "abc"));
		assertEquals("cba", StringUtil.readTillEndOfAllowedChars("cbadef", 0, "abc"));
		assertEquals("cba", StringUtil.readTillEndOfAllowedChars("cba def", 0, "abc"));

		assertEquals("aaa", StringUtil.readTillEndOfAllowedChars("xyz aaa", 4, "abc"));
		assertEquals("cba", StringUtil.readTillEndOfAllowedChars("xyz cba", 4, "abc"));
		assertEquals("abc", StringUtil.readTillEndOfAllowedChars("xyz abc ", 4, "abc"));
	}
	
	@Test
	void testFindFirstNonSpace() {
		assertEquals(0, StringUtil.findFirstNonSpace(null));
		assertEquals(0, StringUtil.findFirstNonSpace(""));
		assertEquals(3, StringUtil.findFirstNonSpace("   "));
		assertEquals(3, StringUtil.findFirstNonSpace("   abc"));
	}
	
	@Test
	void testFindFirstNonSpaceWithStart() {
		assertEquals(0, StringUtil.findFirstNonSpace(null, 0));
		assertEquals(0, StringUtil.findFirstNonSpace("", 0));
		assertEquals(0, StringUtil.findFirstNonSpace("", 1));
		assertEquals(3, StringUtil.findFirstNonSpace("   ", 2));
		assertEquals(9, StringUtil.findFirstNonSpace("   abc   def", 6));
	}
	
	@Test 
	void testRemoveSuffix() {
		assertEquals("test", StringUtil.removeSuffix("test.txt", ".txt", false));
		assertEquals("test", StringUtil.removeSuffix("test.txt", ".txt", true));
		assertEquals("test.txt", StringUtil.removeSuffix("test.txt", ".abc", true));

		assertEquals("test.txt", StringUtil.removeSuffix("test.txt", ".TXT", false));
		assertEquals("test", StringUtil.removeSuffix("test.txt", ".TXT", true));
	}
	
	@Test 
	void testRemovePrefix() {
		assertEquals("test", StringUtil.removePrefix("lv_test", "lv_", false));
		assertEquals("test", StringUtil.removePrefix("lv_test", "lv_", true));
		assertEquals("lv_test", StringUtil.removePrefix("lv_test", "mv_", true));

		assertEquals("lv_test", StringUtil.removePrefix("lv_test", "LV_", false));
		assertEquals("test", StringUtil.removePrefix("lv_test", "LV_", true));
	}
	
	@Test 
	void testRemoveSuffixRecursively() {
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt", ".txt", false));
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt", ".txt", true));
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt.txt", ".txt", false));
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt.txt", ".txt", true));
		assertEquals("test.txt", StringUtil.removeSuffixRecursively("test.txt", ".abc", true));

		assertEquals("test.txt.txt", StringUtil.removeSuffixRecursively("test.txt.txt", ".TXT", false));
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt", ".TXT", true));
		assertEquals("test", StringUtil.removeSuffixRecursively("test.txt.txt", ".TXT", true));
	}
	
	@Test 
	void testremovePrefixRecursivelyRecursively() {
		assertEquals("test", StringUtil.removePrefixRecursively("lv_test", "lv_", false));
		assertEquals("test", StringUtil.removePrefixRecursively("lv_test", "lv_", true));
		assertEquals("test", StringUtil.removePrefixRecursively("lv_lv_test", "lv_", false));
		assertEquals("test", StringUtil.removePrefixRecursively("lv_lv_test", "lv_", true));
		assertEquals("lv_test", StringUtil.removePrefixRecursively("lv_test", "mv_", true));

		assertEquals("lv_test", StringUtil.removePrefixRecursively("lv_test", "LV_", false));
		assertEquals("test", StringUtil.removePrefixRecursively("lv_test", "LV_", true));
		assertEquals("test", StringUtil.removePrefixRecursively("lv_lv_test", "LV_", true));
	}
}
