package com.sap.adt.abapcleaner.rules.ddl.emptylines;

public enum DdlCondenseMode {
	ALWAYS,
	IF_ONLY_ONE_LINERS,
	IF_ONLY_DETACHED_ONE_LINERS,
	NEVER;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public boolean risksLossOfVisualGrouping() {
		return (this == DdlCondenseMode.ALWAYS || this == DdlCondenseMode.IF_ONLY_ONE_LINERS);
	}
	public static DdlCondenseMode forValue(int value) { 
		return values()[value]; 
	}

}
