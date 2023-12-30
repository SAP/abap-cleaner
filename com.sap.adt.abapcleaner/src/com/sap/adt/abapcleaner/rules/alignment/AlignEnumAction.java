package com.sap.adt.abapcleaner.rules.alignment;

public enum AlignEnumAction {
	ALIGN_NAME_AND_VALUE,
	ALIGN_NAME_ONLY;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static AlignEnumAction forValue(int value) {
      return values()[value];
   }
}
