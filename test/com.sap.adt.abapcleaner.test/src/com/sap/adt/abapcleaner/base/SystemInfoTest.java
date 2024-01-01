package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class SystemInfoTest {
	@Test
	void testOperatingSystemKnown() {
		assertNotEquals(OperatingSystem.UNKNOWN, SystemInfo.getOperatingSystem());
	}

	@Test
	void testOperatingSystemConsistent() {
		OperatingSystem os1 = SystemInfo.getOperatingSystem();
		OperatingSystem os2 = SystemInfo.getOperatingSystem();
		assertEquals(os1, os2);
	}

	@Test
	void testButtonOrderConsistent() {
		boolean okBeforeCancel1 = SystemInfo.putOKBeforeCancel();
		boolean okBeforeCancel2 = SystemInfo.putOKBeforeCancel();
		assertEquals(okBeforeCancel1, okBeforeCancel2);
	}
}
