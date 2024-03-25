package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum CamelCaseContextAllKnownAction {
	CHANGE_ALL_KNOWN,
	CHANGE_ALL_APPROVED,
	CHANGE_SURE_ONLY;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static CamelCaseContextAllKnownAction forValue(int value) { 
		return values()[value]; 
	}
}
