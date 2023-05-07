package com.sap.adt.abapcleaner.rules.alignment;

public enum DistinctLineClear {
	ALWAYS, 
	ONLY_WITH_ADDITIONS, 
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DistinctLineClear forValue(int value) { 
		return values()[value]; 
	}
}
