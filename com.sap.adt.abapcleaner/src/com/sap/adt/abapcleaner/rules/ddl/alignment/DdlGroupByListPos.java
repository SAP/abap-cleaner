package com.sap.adt.abapcleaner.rules.ddl.alignment;

public enum DdlGroupByListPos {
	CONTINUE,
	LINE_START_PLUS_2,
	LINE_START_PLUS_4,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlGroupByListPos forValue(int value) { 
		return values()[value]; 
	}
}
