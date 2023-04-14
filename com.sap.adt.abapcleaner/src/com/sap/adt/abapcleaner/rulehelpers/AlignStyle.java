package com.sap.adt.abapcleaner.rulehelpers;

public enum AlignStyle  {
   DO_NOT_ALIGN,
   LEFT_ALIGN,
   RIGHT_ALIGN;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static AlignStyle forValue(int value) {
      return values()[value];
   }
}