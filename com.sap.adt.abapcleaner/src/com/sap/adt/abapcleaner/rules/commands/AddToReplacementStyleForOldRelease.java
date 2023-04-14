package com.sap.adt.abapcleaner.rules.commands;

public enum AddToReplacementStyleForOldRelease {
	KEEP,
	REPLACE_WITHOUT_ASSIGNMENT_OP;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static AddToReplacementStyleForOldRelease forValue(int value) { 
		return values()[value]; 
	}

}
