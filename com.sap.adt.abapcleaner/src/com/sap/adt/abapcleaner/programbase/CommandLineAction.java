package com.sap.adt.abapcleaner.programbase;

public enum CommandLineAction {
	SHOW_HELP,
	SHOW_VERSION,
	CLEANUP;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static CommandLineAction forValue(int value) {
      return values()[value];
   }
}
