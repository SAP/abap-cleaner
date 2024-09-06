package com.sap.adt.abapcleaner.rules.ddl.position;

public enum DdlConditionLineBreak {
	ALWAYS,
	IF_MULTI_LINE_FOUND,
	KEEP_AS_IS,
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlConditionLineBreak forValue(int value) { 
		return values()[value]; 
	}

}
