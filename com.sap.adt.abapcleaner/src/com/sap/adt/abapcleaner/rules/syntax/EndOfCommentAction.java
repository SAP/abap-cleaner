package com.sap.adt.abapcleaner.rules.syntax;

public enum EndOfCommentAction {
	KEEP,
	REMOVE_REDUNDANT,
	REMOVE_ALL;

	public int getValue() { return this.ordinal(); }

	public static EndOfCommentAction forValue(int value) {
		return values()[value];
	}
}
