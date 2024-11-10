package com.sap.adt.abapcleaner.rulehelpers;

public enum TriState {
	UNKNOWN,
	FALSE,
	TRUE;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static TriState forValue(int value) {
      return values()[value];
   }
}
