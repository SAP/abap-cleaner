package com.sap.adt.abapcleaner.rules.alignment;

public enum DistinctLineClear {
	ALWAYS, 
	ONLY_WITH_ADDITIONS, 
	NEVER,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DistinctLineClear forValue(int value) { 
		return values()[value]; 
	}
}
