package com.sap.adt.abapcleaner.rules.declarations;

public enum LocalDeclarationOrder {
   METHOD_START_KEEP_ORDER,
   METHOD_START_CHANGE_ORDER,
   ENCLOSING_BLOCK_CHANGE_ORDER;

	public int getValue() { return this.ordinal(); }

	public static LocalDeclarationOrder forValue(int value) {
		return values()[value];
	}
}
