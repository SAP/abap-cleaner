package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class CleanupResultTest {
	@Test
	void testCreateForRange() {
		final String code = "DO 5 TIMES.\r\n  a += 1.\r\nENDDO.";
		CleanupResult cleanupResult = CleanupResult.createForRange(code, 0, 0, 0, 13);
		
		assertTrue(cleanupResult.hasCleanedCode());
		assertEquals(code, cleanupResult.getCleanedCode());
		assertTrue(cleanupResult.hasLineSelection());
		assertFalse(cleanupResult.hasErrorMessage());
		assertEquals("DO 5 TIMES.\r\n", cleanupResult.getSelectedText());
	}

	@Test
	void testCreateWithoutRange() {
		final String code = "DATA a TYPE i.";
		CleanupResult cleanupResult = CleanupResult.createWithoutRange(code);
		
		assertTrue(cleanupResult.hasCleanedCode());
		assertEquals(code, cleanupResult.getCleanedCode());
		assertFalse(cleanupResult.hasLineSelection());
		assertFalse(cleanupResult.hasErrorMessage());
		assertEquals("", cleanupResult.getSelectedText());
	}

	@Test
	void testCreateError() {
		CleanupResult cleanupResult = CleanupResult.createError("message");
		
		assertFalse(cleanupResult.hasCleanedCode());
		assertEquals(null, cleanupResult.getCleanedCode());
		assertFalse(cleanupResult.hasLineSelection());
		assertTrue(cleanupResult.hasErrorMessage());
		assertEquals("", cleanupResult.getSelectedText());
	}
}
