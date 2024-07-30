package com.sap.adt.abapcleaner.rulehelpers;

public enum DdlAnnotationNestingDepth {
	LEVEL_2(1),
	LEVEL_3(2),
	LEVEL_4(3),
	LEVEL_5(4),
	LEVEL_6(5),
	LEVEL_7(6),
	NEVER(999), 
	FILL_UP_EXISTING(true, true),
	KEEP_AS_IS(true, false);

	/* 0-based value */
	private int minDepth;

	private boolean useExisting;
	private boolean fillUpExisting;
	
	public int getMinDepth() { return minDepth; }
	public boolean getUseExisting() { return useExisting; }
	public boolean getFillUpExisting() { return fillUpExisting; }

	private DdlAnnotationNestingDepth(int minDepth) {
		this.minDepth = minDepth;
		this.useExisting = false;
		this.fillUpExisting = true;
	}
	
	private DdlAnnotationNestingDepth(boolean useExisting, boolean fillUpExisting) {
		this.minDepth = 0;
		this.useExisting = useExisting;
		this.fillUpExisting = fillUpExisting;
	}
	
	public int getValue() { return this.ordinal(); }

	public static DdlAnnotationNestingDepth forValue(int value) {
		return values()[value];
	}
}
