package com.sap.adt.abapcleaner.rules.ddl.alignment;

public enum DdlSourceParamPos {
	CONTINUE,
	LINE_START_PLUS_2,
	LINE_START_PLUS_4,
	SOURCE_NAME_PLUS_2,
	SOURCE_NAME_PLUS_4,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlSourceParamPos forValue(int value) { 
		return values()[value]; 
	}
}
