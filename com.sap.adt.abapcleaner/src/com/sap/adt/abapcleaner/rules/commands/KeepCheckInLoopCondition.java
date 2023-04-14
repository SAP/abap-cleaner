package com.sap.adt.abapcleaner.rules.commands;

public enum KeepCheckInLoopCondition  {
   NEVER,
   KEEP_AT_LOOP_START;

	public int getValue() { return this.ordinal(); }

	public static KeepCheckInLoopCondition forValue(int value) {
		return values()[value];
	}
}