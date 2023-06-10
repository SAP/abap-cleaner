package com.sap.adt.abapcleaner.rules.alignment;

public enum MethodsOneLinerAction {
   CREATE,
   KEEP_EXISTING,
   SAME_AS_MULTI_LINERS;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static MethodsOneLinerAction forValue(int value) {
      return values()[value];
   }	
}
