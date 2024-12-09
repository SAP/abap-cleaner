package com.sap.adt.abapcleaner.rules.alignment;

public enum DistinctLineCatch {
	ALWAYS, 
	NEVER,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DistinctLineCatch forValue(int value) { 
		return values()[value]; 
	}
}
