package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDateTime;

import org.junit.jupiter.api.Test;

public class CultTest {
	@Test
	void testFormat() {
		assertEquals("0", Cult.format(0));
		assertEquals("1", Cult.format(1));
		assertEquals("42", Cult.format(42));
	}

	@Test
	void testFromMillisec() {
		assertEquals("0 ms", Cult.fromMillisec(0));
		assertEquals("1 ms", Cult.fromMillisec(1));
		assertEquals("42 ms", Cult.fromMillisec(42));
	}
	
	@Test
	void testFormatDouble() {
		// the decimal separator is NOT tested, as it is culture-dependent
		assertTrue(Cult.format(3.1415926535, 2).endsWith("14"));
		assertTrue(Cult.format(3.1415926535, 5).endsWith("14159"));
		assertTrue(Cult.format(3.1415926535, 8).endsWith("14159265"));
	}

	@Test
	void testGetReverseDate() {
		LocalDateTime dateTime = LocalDateTime.of(2022, 3, 14, 15, 9, 26);
		assertEquals("20220314", Cult.getReverseDateTime(dateTime, false));
		assertEquals("20220314_150926", Cult.getReverseDateTime(dateTime, true));
	}

	@Test
	void testGetPaddedString() {
		assertEquals("5", Cult.getPaddedString(5, 0, ' '));
		assertEquals("5", Cult.getPaddedString(5, 1, ' '));
		assertEquals(" 5", Cult.getPaddedString(5, 2, ' '));
		assertEquals("  5", Cult.getPaddedString(5, 3, ' '));

		assertEquals("42", Cult.getPaddedString(42, 1, '_'));
		assertEquals("_42", Cult.getPaddedString(42, 3, '_'));
		assertEquals("___42", Cult.getPaddedString(42, 5, '_'));
	}
}
