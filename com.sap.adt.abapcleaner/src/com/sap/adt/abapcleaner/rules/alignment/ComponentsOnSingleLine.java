package com.sap.adt.abapcleaner.rules.alignment;

public enum ComponentsOnSingleLine {
	NEVER, 
	IF_BELOW_MAX_LINE_LENGTH, 
	ALWAYS;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static ComponentsOnSingleLine forValue(int value) { 
		return values()[value]; 
	}
}