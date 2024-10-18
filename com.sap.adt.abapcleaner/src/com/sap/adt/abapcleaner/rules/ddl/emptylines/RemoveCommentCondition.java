package com.sap.adt.abapcleaner.rules.ddl.emptylines;

public enum RemoveCommentCondition {
	ALWAYS,
	IF_VALUE_HAS_CONTENT,
	NEVER;

	public int getValue() { return this.ordinal(); }

	public static RemoveCommentCondition forValue(int value) { return values()[value]; }
}
