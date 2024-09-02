package com.sap.adt.abapcleaner.rules.ddl.position;

public enum DdlLineBreak {
	ALWAYS,
	KEEP_AS_IS,
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlLineBreak forValue(int value) { 
		return values()[value]; 
	}

}
