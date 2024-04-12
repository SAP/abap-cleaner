package com.sap.adt.abapcleaner.parser;

public enum ChainElementAction {
	DELETE,
	COMMENT_OUT_WITH_ASTERISK,
	COMMENT_OUT_WITH_QUOT,
	ADD_TODO_COMMENT,
	ADD_PRAGMA_NEEDED,
	IGNORE;

	public int getValue() { return this.ordinal(); }
	
	public static ChainElementAction forValue(int value) {
		return values()[value];
	}
}
