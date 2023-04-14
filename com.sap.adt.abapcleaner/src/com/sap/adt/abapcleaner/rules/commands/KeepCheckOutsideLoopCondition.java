package com.sap.adt.abapcleaner.rules.commands;

public enum KeepCheckOutsideLoopCondition  {
   NEVER,
   KEEP_AT_METHOD_START,
   KEEP_AFTER_DECLARATIONS,
   KEEP_AFTER_DECLARATIONS_AND_CLEAR;

	public int getValue() { return this.ordinal(); }

	public static KeepCheckOutsideLoopCondition forValue(int value) {
		return values()[value];
	}
}