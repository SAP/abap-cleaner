package com.sap.adt.abapcleaner.gui;

public enum ColorProfile {
	ADT,
	CLASSIC;

	public int getValue() { return this.ordinal(); }
	
	public static ColorProfile forValue(int value) {
		return values()[value];
	}

 	public static ColorProfile  getDefault() {
		return ColorProfile.ADT;
 	}
}
