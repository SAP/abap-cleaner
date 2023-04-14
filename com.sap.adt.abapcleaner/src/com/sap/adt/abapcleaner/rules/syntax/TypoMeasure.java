package com.sap.adt.abapcleaner.rules.syntax;

public enum TypoMeasure {
	CHANGE_DIRECTLY,
	ADD_TODO_COMMENT,
	KEEP_UNCHANGED;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static TypoMeasure forValue(int value) {
      return values()[value];
   }
}