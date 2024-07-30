package com.sap.adt.abapcleaner.rulehelpers;

public enum DdlAnnotationSortOrder {
	BY_FIRST_ELEM,
	BY_TWO_ELEMS,
	BY_ALL_ELEMS,
	KEEP;

	public int getValue() { return this.ordinal(); }

	public static DdlAnnotationSortOrder forValue(int value) {
		return values()[value];
	}
}
