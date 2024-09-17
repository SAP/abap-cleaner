package com.sap.adt.abapcleaner.rules.ddl.emptylines;

public enum DdlEmptyLineType {
   ALWAYS_AT_LEAST_ONE,
   ALWAYS_EXACTLY_ONE,
   KEEP_AS_IS,
   NEVER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static DdlEmptyLineType forValue(int value) {
      return values()[value];
   }
}