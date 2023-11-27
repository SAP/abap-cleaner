package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectListLayout {
	MULTI_LINE,
	ONE_LINE,
	DERIVE,
   KEEP_AS_IS; 

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static SelectListLayout forValue(int value) {
      return values()[value];
   }	
}
