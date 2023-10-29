package com.sap.adt.abapcleaner.rules.declarations;

public enum ClassDefOneLinerAction {
	CREATE_ON_SAME_LINE, 
	CREATE_ON_NEXT_LINE, 
	KEEP, 
	SPLIT;
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static ClassDefOneLinerAction forValue(int value) { 
		return values()[value]; 
	}
}
