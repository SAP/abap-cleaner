package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class ProfileDirTest {
	@Test
	void testDefaultShortName() {
		assertEquals("team A", ProfileDir.getDefaultShortName(0));
		assertEquals("team B", ProfileDir.getDefaultShortName(1));
		assertEquals("team C", ProfileDir.getDefaultShortName(2));
	}

	@Test
	void testStandardizeShortName() {
		assertEquals("team A", ProfileDir.standardizeShortName("  team A  "));
		assertEquals("team. A", ProfileDir.standardizeShortName("team: A"));
	}

	@Test
	void testAttributes() {
		String anyName = "team A";
		String anyDir = "C:\\any\\dir\\";
		ProfileDir profileDir = new ProfileDir(anyName, anyDir);
		assertEquals(anyName, profileDir.shortName);
		assertEquals(anyDir, profileDir.readOnlyDir);
	}
}
