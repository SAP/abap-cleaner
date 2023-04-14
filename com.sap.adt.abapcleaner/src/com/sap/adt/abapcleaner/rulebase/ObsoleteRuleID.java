package com.sap.adt.abapcleaner.rulebase;

public enum ObsoleteRuleID {
	SPACE_BEFORE_COMMENT_SIGN, 
	SPACE_AFTER_COMMENT_SIGN;
	
	public static final int SIZE = java.lang.Integer.SIZE;

	public int getValue() { return this.ordinal(); }

	public static ObsoleteRuleID forValue(int value) {
		return values()[value];
	}
}
