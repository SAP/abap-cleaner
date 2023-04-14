package com.sap.adt.abapcleaner.rules.alignment;

public enum MethodsSequenceAlignment {
	NEVER,
	ONE_LINERS,
	ALL_TABULAR;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static MethodsSequenceAlignment forValue(int value) {
      return values()[value];
   }	
}
