package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum CaseStyle {
	UNCHANGED, 
	LOWER_CASE, 
	UPPER_CASE;

	public int getValue() { return this.ordinal(); }

	public static CaseStyle forValue(int value) { return values()[value]; }
}