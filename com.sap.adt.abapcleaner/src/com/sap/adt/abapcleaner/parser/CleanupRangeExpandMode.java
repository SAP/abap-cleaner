package com.sap.adt.abapcleaner.parser;

public enum CleanupRangeExpandMode {
	FULL_STATEMENT("Current statement"),
	FULL_METHOD("Current method etc."),
	FULL_CLASS("Current class"),
	FULL_DOCUMENT("Entire code document");

   public static final int SIZE = java.lang.Integer.SIZE;

   public final String displayText;
   
   private CleanupRangeExpandMode(String displayText) {
   	this.displayText = displayText;	
   }

   public int getValue() {
      return this.ordinal();
   }

   public static CleanupRangeExpandMode forValue(int value) throws IllegalArgumentException {
   	 if (value < 0 || value >= values().length)
   		 throw new IllegalArgumentException();
  		 return values()[value];
   }

 	public static CleanupRangeExpandMode getDefault() {
		return CleanupRangeExpandMode.FULL_METHOD;
 	}
}
