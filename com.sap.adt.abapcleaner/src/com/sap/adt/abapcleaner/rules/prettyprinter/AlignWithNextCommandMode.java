package com.sap.adt.abapcleaner.rules.prettyprinter;

public enum AlignWithNextCommandMode {
	ALWAYS, 
	IF_BLANK_LINE_ABOVE, 
	NEVER;

	public int getValue() { return this.ordinal(); }

	public static AlignWithNextCommandMode forValue(int value) {
		return values()[value];
	}
}