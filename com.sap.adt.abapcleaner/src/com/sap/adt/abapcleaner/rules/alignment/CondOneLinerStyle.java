package com.sap.adt.abapcleaner.rules.alignment;

public enum CondOneLinerStyle {
	CREATE, 
	KEEP, 
	SPLIT;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static CondOneLinerStyle forValue(int value) { 
		return values()[value]; 
	}
}
