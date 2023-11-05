package com.sap.adt.abapcleaner.rules.commands;

public enum AssertParameterOrder {
	EXP_FIRST,
	ACT_FIRST, 
	KEEP_AS_IS;
	
	public int getValue() { return this.ordinal(); }

	public static AssertParameterOrder forValue(int value) {
		return values()[value];
	}
}
