package com.sap.adt.abapcleaner.rules.declarations;

public enum EmptySectionsAction {
	REMOVE_PROTECTED_OF_FINAL_CLASS,
	REMOVE_ANY_FROM_NON_EMPTY_CLASS,
	REMOVE_ANY;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static EmptySectionsAction forValue(int value) { 
		return values()[value]; 
	}
}
