package com.sap.adt.abapcleaner.parser;

public enum StressTestType {
	NONE("none"),
	LINE_END_COMMENT("line-end comment"),
	COMMENT_LINE("comment line"),
	PRAGMA("pragma"),
	COLON("chain colon");
	
	public final String description;
	
	private StressTestType(String description) {
		this.description = description;
	}
	
	public static StressTestType[] getAll() {
		return new StressTestType[] { StressTestType.LINE_END_COMMENT, StressTestType.COMMENT_LINE, StressTestType.PRAGMA, StressTestType.COLON };
	}
}
