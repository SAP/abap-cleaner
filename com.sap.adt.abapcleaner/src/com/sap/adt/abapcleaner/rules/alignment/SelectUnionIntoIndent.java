package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectUnionIntoIndent {
   PLUS_0(0),
   PLUS_2(2),
   PLUS_4(4),
   PLUS_7(7);

	private final int addIndent;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   private SelectUnionIntoIndent(int addIndent) {
   	this.addIndent = addIndent;
   }

   public int getAddIndent() {
   	return addIndent;
   }

   public int getValue() {
      return this.ordinal();
   }

   public static SelectUnionIntoIndent forValue(int value) {
      return values()[value];
   }	
}
