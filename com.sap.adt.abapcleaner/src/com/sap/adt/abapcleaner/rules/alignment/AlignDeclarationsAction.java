package com.sap.adt.abapcleaner.rules.alignment;

public enum AlignDeclarationsAction {
	ALIGN_NAME_TYPE_LENGTH_ETC,
	ALIGN_NAME_AND_TYPE,
	ALIGN_NAME_ONLY;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static AlignDeclarationsAction forValue(int value) {
      return values()[value];
   }
}
