package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectUnionIndent {
   PLUS_0(0),
   PLUS_2(2),
   PLUS_4(4),
   PLUS_7(7);

	private final int addIndent;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   private SelectUnionIndent(int addIndent) {
   	this.addIndent = addIndent;
   }

   public int getAddIndent() {
   	return addIndent;
   }

   public int getValue() {
      return this.ordinal();
   }

   public static SelectUnionIndent forValue(int value) {
      return values()[value];
   }	
}
