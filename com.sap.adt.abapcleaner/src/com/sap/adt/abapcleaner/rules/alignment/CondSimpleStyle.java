package com.sap.adt.abapcleaner.rules.alignment;

public enum CondSimpleStyle {
	VERTICAL_LAYOUT,
	LIKE_COMPLEX;
	
	public int getValue() { 
		return this.ordinal();
	}

	public static CondSimpleStyle forValue(int value) { 
		return values()[value]; 
	}
}
