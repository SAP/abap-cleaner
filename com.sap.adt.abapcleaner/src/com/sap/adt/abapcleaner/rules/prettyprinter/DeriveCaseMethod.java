package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum DeriveCaseMethod {
	NONE,
	FROM_FIRST_TOKEN,
	FROM_MAJORITY;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static DeriveCaseMethod forValue(int value) { 
		return values()[value]; 
	}

}
