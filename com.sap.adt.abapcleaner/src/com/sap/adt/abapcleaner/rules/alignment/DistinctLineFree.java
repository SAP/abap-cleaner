package com.sap.adt.abapcleaner.rules.alignment;

public enum DistinctLineFree {
	ALWAYS, 
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DistinctLineFree forValue(int value) { 
		return values()[value]; 
	}
}
