package com.sap.adt.abapcleaner.rulehelpers;

public enum DdlAnnotationEmptyLines {
	ALWAYS(true, true),
	FOR_MULTI_LINE_OR_NEW_FIRST_ELEM(true, true),
	FOR_NEW_FIRST_ELEM(true, false),
	NEVER(false, false), 
	KEEP_AS_IS(false, false);

	public final boolean separateByFirstElement;
	public final boolean separateMultiLiners;
	
	private DdlAnnotationEmptyLines(boolean separateByFirstElement, boolean separateMultiLiners) {
		this.separateByFirstElement = separateByFirstElement;
		this.separateMultiLiners = separateMultiLiners;
	}
	
	public int getValue() { return this.ordinal(); }

	public static DdlAnnotationEmptyLines forValue(int value) {
		return values()[value];
	}
}
