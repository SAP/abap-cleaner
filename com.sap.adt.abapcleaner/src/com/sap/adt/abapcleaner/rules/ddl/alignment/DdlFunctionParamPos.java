package com.sap.adt.abapcleaner.rules.ddl.alignment;

public enum DdlFunctionParamPos {
	CONTINUE,
	FUNCTION_NAME_PLUS_2,
	FUNCTION_NAME_PLUS_4,
	KEEP_AS_IS;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static DdlFunctionParamPos forValue(int value) { 
		return values()[value]; 
	}
}
