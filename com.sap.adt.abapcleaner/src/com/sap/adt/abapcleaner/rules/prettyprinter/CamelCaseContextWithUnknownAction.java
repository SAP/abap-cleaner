package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum CamelCaseContextWithUnknownAction {
	CHANGE_ALL_KNOWN,
	CHANGE_ALL_APPROVED,
	CHANGE_SURE_ONLY,
	CHANGE_NONE;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static CamelCaseContextWithUnknownAction forValue(int value) { 
		return values()[value]; 
	}
}
