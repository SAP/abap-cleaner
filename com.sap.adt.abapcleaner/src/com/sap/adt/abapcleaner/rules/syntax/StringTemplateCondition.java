package com.sap.adt.abapcleaner.rules.syntax;

public enum StringTemplateCondition {
	SHORTER,
	SHORTER_OR_EQUAL,
	ALWAYS;

	public int getValue() { 
		return this.ordinal(); 
	}

	public static StringTemplateCondition forValue(int value) { 
		return values()[value]; 
	}
}
