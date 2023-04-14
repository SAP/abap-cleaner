package com.sap.adt.abapcleaner.rulehelpers;

public enum ChangeType {
   ALWAYS,
   KEEP_AS_IS,
   NEVER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ChangeType forValue(int value) {
      return values()[value];
   }
}