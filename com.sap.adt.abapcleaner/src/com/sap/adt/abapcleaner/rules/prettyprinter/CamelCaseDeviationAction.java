package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum CamelCaseDeviationAction {
	CHANGE_IF_KNOWN,
	CHANGE_IF_APPROVED,
	NONE;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static CamelCaseDeviationAction forValue(int value) { 
		return values()[value]; 
	}
}
