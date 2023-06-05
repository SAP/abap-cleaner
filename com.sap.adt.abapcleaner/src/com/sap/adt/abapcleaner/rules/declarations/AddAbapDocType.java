package com.sap.adt.abapcleaner.rules.declarations;

public enum AddAbapDocType {
	ALWAYS,
	ALWAYS_AS_NON_SYNCHRONIZED,
	ONLY_FOR_NON_SYNCHRONIZED,
	NEVER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static AddAbapDocType forValue(int value) {
      return values()[value];
   }
}
