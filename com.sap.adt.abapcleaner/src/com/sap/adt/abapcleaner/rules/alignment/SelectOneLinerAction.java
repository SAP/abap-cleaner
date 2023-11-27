package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectOneLinerAction {
   CREATE,
   KEEP_EXISTING,
   ALWAYS_SPLIT;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static SelectOneLinerAction forValue(int value) {
      return values()[value];
   }	
}
