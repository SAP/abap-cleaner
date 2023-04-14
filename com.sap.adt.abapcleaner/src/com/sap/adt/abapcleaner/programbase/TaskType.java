package com.sap.adt.abapcleaner.programbase;

public enum TaskType  {
   NONE,
   PARSER,
   CLEANER,
   COMPARER,
   INTEGRITY_TEST;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static TaskType forValue(int value) {
      return values()[value];
   }
}