package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;


public class AbabCultTest {
	@Test
	void testStringEquals() {
		assertTrue(AbapCult.stringEquals("abc", "abc", false));
		assertFalse(AbapCult.stringEquals("abc", "ABC", false));
		assertFalse(AbapCult.stringEquals("abc", "Abc", false));

		assertTrue(AbapCult.stringEquals("abc", "abc", true));
		assertTrue(AbapCult.stringEquals("abc", "ABC", true));
		assertTrue(AbapCult.stringEquals("abc", "Abc", true));
}
	@Test
	void testStringEqualsAny() {
		assertTrue(AbapCult.stringEqualsAny(false, "abc", "123", "abc", "def", "GHI"));
		assertFalse(AbapCult.stringEqualsAny(false, "abc", "123", "ABC", "def", "GHI"));
		assertFalse(AbapCult.stringEqualsAny(false, "abc", "123", "Abc", "def", "GHI"));

		assertTrue(AbapCult.stringEqualsAny(true, "abc", "123", "abc", "def", "GHI"));
		assertTrue(AbapCult.stringEqualsAny(true, "abc", "123", "ABC", "def", "GHI"));
		assertTrue(AbapCult.stringEqualsAny(true, "abc", "123", "Abc", "def", "GHI"));
	}

	@Test
	void testIndexOf() {
		assertEquals(2, AbapCult.indexOf("abcdefg abcdefg", "cde", 0, false));
		assertEquals(10, AbapCult.indexOf("abcdefg abcdefg", "cde", 3, false));
		assertEquals(-1, AbapCult.indexOf("abcdefg abcdefg", "CDE", 0, false));
		assertEquals(-1, AbapCult.indexOf("abcdefg abcdefg", "CDE", 3, false));
		assertEquals(-1, AbapCult.indexOf("abcdefg abcdefg", "Cde", 0, false));
		assertEquals(-1, AbapCult.indexOf("abcdefg abcdefg", "Cde", 3, false));

		assertEquals(2, AbapCult.indexOf("abcdefg abcdefg", "cde", 0, true));
		assertEquals(10, AbapCult.indexOf("abcdefg abcdefg", "cde", 3, true));
		assertEquals(2, AbapCult.indexOf("abcdefg abcdefg", "CDE", 0, true));
		assertEquals(10, AbapCult.indexOf("abcdefg abcdefg", "CDE", 3, true));
		assertEquals(2, AbapCult.indexOf("abcdefg abcdefg", "Cde", 0, true));
		assertEquals(10, AbapCult.indexOf("abcdefg abcdefg", "Cde", 3, true));
	}

	@Test
	void testLastIndexOf() {
		assertEquals(10, AbapCult.lastIndexOf("abcdefg abcdefg", "cde", 15, false));
		assertEquals(2, AbapCult.lastIndexOf("abcdefg abcdefg", "cde", 8, false));
		assertEquals(-1, AbapCult.lastIndexOf("abcdefg abcdefg", "CDE", 15, false));
		assertEquals(-1, AbapCult.lastIndexOf("abcdefg abcdefg", "CDE", 8, false));
		assertEquals(-1, AbapCult.lastIndexOf("abcdefg abcdefg", "Cde", 15, false));
		assertEquals(-1, AbapCult.lastIndexOf("abcdefg abcdefg", "Cde", 8, false));

		assertEquals(10, AbapCult.lastIndexOf("abcdefg abcdefg", "cde", 15, true));
		assertEquals(2, AbapCult.lastIndexOf("abcdefg abcdefg", "cde", 8, true));
		assertEquals(10, AbapCult.lastIndexOf("abcdefg abcdefg", "CDE", 15, true));
		assertEquals(2, AbapCult.lastIndexOf("abcdefg abcdefg", "CDE", 8, true));
		assertEquals(10, AbapCult.lastIndexOf("abcdefg abcdefg", "Cde", 15, true));
		assertEquals(2, AbapCult.lastIndexOf("abcdefg abcdefg", "Cde", 8, true));
	}
	
	@Test
	void testStringContainsAt() {
		assertFalse(AbapCult.stringContainsAt(null, 0, "abc", false));
		assertFalse(AbapCult.stringContainsAt(null, 0, "abc", true));
		assertFalse(AbapCult.stringContainsAt("abc", 0, null, false));
		assertFalse(AbapCult.stringContainsAt("abc", 0, null, true));

		assertFalse(AbapCult.stringContainsAt("abcd", 0, "bc", false));
		assertTrue(AbapCult.stringContainsAt("abcd", 1, "bc", false));
		assertFalse(AbapCult.stringContainsAt("abcd", 2, "bc", false));
		assertFalse(AbapCult.stringContainsAt("abcd", 5, "bc", false));

		assertFalse(AbapCult.stringContainsAt("abcd", 1, "BC", false));
		assertFalse(AbapCult.stringContainsAt("abcd", 1, "bC", false));
		assertTrue(AbapCult.stringContainsAt("abcd", 1, "BC", true));
		assertTrue(AbapCult.stringContainsAt("abcd", 1, "bC", true));
	}
	
	@Test
	void testToLower() {
		assertEquals(null, AbapCult.toLower(null));
		assertEquals("abc", AbapCult.toLower("abc"));
		assertEquals("abc", AbapCult.toLower("ABC"));
		assertEquals("abc", AbapCult.toLower("Abc"));
	}
	
	@Test
	void testToUpper() {
		assertEquals(null, AbapCult.toUpper(null));
		assertEquals("ABC", AbapCult.toUpper("abc"));
		assertEquals("ABC", AbapCult.toUpper("ABC"));
		assertEquals("ABC", AbapCult.toUpper("Abc"));
	}
	
	@Test
	void testStringStartsWithAny() {
		assertTrue(AbapCult.stringStartsWithAny("abc", "a", "d", "e"));
		assertTrue(AbapCult.stringStartsWithAny("abc", "E", "D", "A"));

		assertFalse(AbapCult.stringStartsWithAny("abc", "b", "c", "d"));
		assertFalse(AbapCult.stringStartsWithAny("abc", "D", "C", "B"));
	}
	
	@Test
	void testStringStartsWith() {
		// expect the method to NOT be case-sensitive
		assertTrue(AbapCult.stringStartsWith("abc", "a"));
		assertTrue(AbapCult.stringStartsWith("abc", "A"));

		assertFalse(AbapCult.stringStartsWith("abc", "c"));
		assertFalse(AbapCult.stringStartsWith("abc", "C"));
	}
	
	@Test
	void testStringStartsWithCaseSensitive() {
		assertTrue(AbapCult.stringStartsWith("abc", "a", false));

		assertFalse(AbapCult.stringStartsWith("abc", "A", false));
		assertFalse(AbapCult.stringStartsWith("abc", "c", false));
		assertFalse(AbapCult.stringStartsWith("abc", "C", false));
	}
	
	@Test
	void testStringEndsWithAny() {
		assertTrue(AbapCult.stringEndsWithAny("abc", "c", "d", "e"));
		assertTrue(AbapCult.stringEndsWithAny("abc", "E", "D", "C"));

		assertFalse(AbapCult.stringEndsWithAny("abc", "a", "b", "d"));
		assertFalse(AbapCult.stringEndsWithAny("abc", "D", "B", "A"));
	}

	@Test
	void testStringEndsWith() {
		// expect the method to NOT be case-sensitive
		assertTrue(AbapCult.stringEndsWith("abc", "c"));
		assertTrue(AbapCult.stringEndsWith("abc", "C"));

		assertFalse(AbapCult.stringEndsWith("abc", "a"));
		assertFalse(AbapCult.stringEndsWith("abc", "A"));
	}

	@Test
	void testStringEndsWithCaseSensitive() {
		assertTrue(AbapCult.stringEndsWith("abc", "c", false));

		assertFalse(AbapCult.stringEndsWith("abc", "C", false));
		assertFalse(AbapCult.stringEndsWith("abc", "a", false));
		assertFalse(AbapCult.stringEndsWith("abc", "A", false));
	}
}
