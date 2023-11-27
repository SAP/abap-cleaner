package com.sap.adt.abapcleaner.rulebase;

public enum ConfigInfoStyle {
	NORMAL,
   HEADING, 
   WARNING;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ConfigInfoStyle forValue(int value) {
      return values()[value];
   }	
}
