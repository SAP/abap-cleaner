package com.sap.adt.abapcleaner.rules.syntax;

public enum CommentSeparatorMeasure  {
   KEEP,
   CONVERT_TO_HYPHEN,
   CONVERT_TO_EQUALS,
   DELETE;

	public int getValue() { return this.ordinal(); }

	public static CommentSeparatorMeasure forValue(int value) {
		return values()[value];
	}
}