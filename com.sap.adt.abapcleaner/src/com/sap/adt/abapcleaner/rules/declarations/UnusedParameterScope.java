package com.sap.adt.abapcleaner.rules.declarations;

public enum UnusedParameterScope {
	ALL_METHODS,
	NON_INTERFACE_METHODS,
	PROTECTED_OR_PRIVATE,
	PRIVATE_ONLY,
	NEVER;

	public int getValue() { return this.ordinal(); }
	
	public static UnusedParameterScope forValue(int value) {
		return values()[value];
	}
}
