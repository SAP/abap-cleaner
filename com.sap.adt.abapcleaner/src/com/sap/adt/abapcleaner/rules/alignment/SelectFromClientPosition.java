package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectFromClientPosition {
   CONTINUE,
   KEEP_AS_IS,
   OWN_LINE_AFTER_JOINS,
   OWN_LINE;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static SelectFromClientPosition forValue(int value) {
      return values()[value];
   }	
}
