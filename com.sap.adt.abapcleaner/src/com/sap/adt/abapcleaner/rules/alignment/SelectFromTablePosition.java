package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectFromTablePosition {
   CONTINUE(-1),
   BELOW_PLUS_0(0),
   BELOW_PLUS_2(2);

	private final int addIndent;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   private SelectFromTablePosition(int addIndent) {
   	this.addIndent = addIndent;
   }

   public int getAddIndent() {
   	return addIndent;
   }

   public int getValue() {
      return this.ordinal();
   }

   public static SelectFromTablePosition forValue(int value) {
      return values()[value];
   }	
}
