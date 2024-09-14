package com.sap.adt.abapcleaner.rules.ddl.alignment;

public enum DdlNextAfterParensPos {
	CONTINUE,
	LINE_START,
	LINE_START_PLUS_2,
	SOURCE_NAME,
	SOURCE_NAME_PLUS_2,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlNextAfterParensPos forValue(int value) { 
		return values()[value]; 
	}
}
