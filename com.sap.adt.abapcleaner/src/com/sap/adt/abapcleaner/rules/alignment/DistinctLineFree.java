package com.sap.adt.abapcleaner.rules.alignment;

public enum DistinctLineFree {
	ALWAYS, 
	NEVER,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DistinctLineFree forValue(int value) { 
		return values()[value]; 
	}
}
