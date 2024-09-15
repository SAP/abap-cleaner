package com.sap.adt.abapcleaner.rules.ddl.alignment;

public enum DdlNameListPos {
	CONTINUE,
	LINE_START_PLUS_2,
	LINE_START_PLUS_4,
	VIEW_NAME_PLUS_2,
	VIEW_NAME_PLUS_4,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlNameListPos forValue(int value) { 
		return values()[value]; 
	}
}
