package com.sap.adt.abapcleaner.rules.emptylines;

public enum AddLineBetweenDefTypesStyle {
	NEVER,
	ADD_IGNORE_STATIC,
	ADD_CONSIDER_STATIC;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static AddLineBetweenDefTypesStyle forValue(int value) { 
		return values()[value]; 
	}
}
