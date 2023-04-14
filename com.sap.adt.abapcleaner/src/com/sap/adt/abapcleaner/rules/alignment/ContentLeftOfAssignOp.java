package com.sap.adt.abapcleaner.rules.alignment;

public enum ContentLeftOfAssignOp {
	NEVER, 
	TO_KEEP_MAX_LINE_LENGTH, 
	ALWAYS;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static ContentLeftOfAssignOp forValue(int value) { 
		return values()[value]; 
	}
}