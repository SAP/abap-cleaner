package com.sap.adt.abapcleaner.rules.alignment;

public enum CondValueIndent {
	ADD_0,
	ADD_2,
	ADD_5,
	ADD_7;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static CondValueIndent forValue(int value) { 
		return values()[value]; 
	}
}
