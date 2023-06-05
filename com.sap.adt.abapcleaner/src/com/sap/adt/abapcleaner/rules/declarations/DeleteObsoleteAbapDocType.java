package com.sap.adt.abapcleaner.rules.declarations;

public enum DeleteObsoleteAbapDocType {
   ALWAYS,
   IF_DESCRIPTION_EMPTY,
   NEVER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static DeleteObsoleteAbapDocType forValue(int value) {
      return values()[value];
   }
}