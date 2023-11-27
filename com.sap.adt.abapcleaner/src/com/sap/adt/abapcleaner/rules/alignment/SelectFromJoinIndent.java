package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectFromJoinIndent {
   PLUS_0(0),
   PLUS_2(2),
   PLUS_4(4);

	private final int addIndent;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   private SelectFromJoinIndent(int addIndent) {
   	this.addIndent = addIndent;
   }

   public int getAddIndent() {
   	return addIndent;
   }

   public int getValue() {
      return this.ordinal();
   }

   public static SelectFromJoinIndent forValue(int value) {
      return values()[value];
   }	
}
