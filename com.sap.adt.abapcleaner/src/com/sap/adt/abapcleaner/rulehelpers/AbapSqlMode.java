package com.sap.adt.abapcleaner.rulehelpers;

public enum AbapSqlMode {
	STRICT,
	NOT_STRICT,
	UNCERTAIN;

	public static final int length = values().length;
   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static AbapSqlMode forValue(int value) {
      return values()[value];
   }	
}
