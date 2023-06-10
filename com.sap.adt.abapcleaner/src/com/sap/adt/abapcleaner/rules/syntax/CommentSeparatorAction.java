package com.sap.adt.abapcleaner.rules.syntax;

public enum CommentSeparatorAction  {
   KEEP,
   CONVERT_TO_HYPHEN,
   CONVERT_TO_EQUALS,
   DELETE;

	public int getValue() { return this.ordinal(); }

	public static CommentSeparatorAction forValue(int value) {
		return values()[value];
	}
}