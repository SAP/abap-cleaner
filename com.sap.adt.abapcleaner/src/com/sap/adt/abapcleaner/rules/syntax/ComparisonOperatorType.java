package com.sap.adt.abapcleaner.rules.syntax;

public enum ComparisonOperatorType {
	SYMBOLIC,
	TEXTUAL;
	
	public int getValue() { return this.ordinal(); }
	
	public static ComparisonOperatorType forValue(int value) {
		return values()[value];
	}
}
