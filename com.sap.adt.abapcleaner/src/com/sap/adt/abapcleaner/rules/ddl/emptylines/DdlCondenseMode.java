package com.sap.adt.abapcleaner.rules.ddl.emptylines;

public enum DdlCondenseMode {
	ALWAYS,
	IF_ONLY_ONE_LINERS,
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlCondenseMode forValue(int value) { 
		return values()[value]; 
	}

}
