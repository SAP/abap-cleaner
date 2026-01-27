package com.sap.adt.abapcleaner.rules.alignment;

public enum ContainingPosition {
	UNCHANGED,
	CONTINUE,
	CONTINUE_AND_BREAK,
	BELOW_KEYWORD_PLUS_2,
	BELOW_IDENTIFIER_PLUS_2,
	BELOW_TYPE,
	BELOW_ANY_STRUCTURE;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ContainingPosition forValue(int value) {
      return values()[value];
   }
}
