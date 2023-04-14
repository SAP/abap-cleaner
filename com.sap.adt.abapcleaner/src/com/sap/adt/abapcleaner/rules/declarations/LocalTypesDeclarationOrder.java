package com.sap.adt.abapcleaner.rules.declarations;

public enum LocalTypesDeclarationOrder {
   METHOD_START_KEEP_ORDER;

	public int getValue() { return this.ordinal(); }

	public static LocalTypesDeclarationOrder forValue(int value) {
		return values()[value];
	}
}
