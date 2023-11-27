package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectFromOnPosition {
   CONTINUE,
   BELOW_JOIN,
   BELOW_JOIN_PLUS_2,
   BELOW_JOIN_WORD_2;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static SelectFromOnPosition forValue(int value) {
      return values()[value];
   }	
}
