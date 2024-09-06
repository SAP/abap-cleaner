package com.sap.adt.abapcleaner.rules.ddl.position;

public enum DdlLineBreakWithoutNever {
	ALWAYS,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlLineBreakWithoutNever forValue(int value) { 
		return values()[value]; 
	}

}
