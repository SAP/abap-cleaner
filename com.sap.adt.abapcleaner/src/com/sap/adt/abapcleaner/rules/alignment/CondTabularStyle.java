package com.sap.adt.abapcleaner.rules.alignment;

public enum CondTabularStyle {
	CREATE,
	KEEP,
	SPLIT;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static CondTabularStyle forValue(int value) { 
		return values()[value]; 
	}
}
