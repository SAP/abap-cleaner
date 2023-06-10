package com.sap.adt.abapcleaner.rules.declarations;

public enum UnusedVariableAction  {
   DELETE,
   COMMENT_OUT_WITH_ASTERISK,
   COMMENT_OUT_WITH_QUOT,
   ADD_TODO_COMMENT,
   IGNORE;

	public int getValue() { return this.ordinal(); }

	public static UnusedVariableAction forValue(int value) {
		return values()[value];
	}
}