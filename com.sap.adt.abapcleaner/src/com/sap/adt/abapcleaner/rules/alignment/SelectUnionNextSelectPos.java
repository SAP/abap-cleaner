package com.sap.adt.abapcleaner.rules.alignment;

public enum SelectUnionNextSelectPos {
	CONTINUE(-1),
	CONTINUE_OR_BELOW_SECOND(-1),
   BELOW_PLUS_0(0),
   BELOW_PLUS_2(2),
   BELOW_PLUS_4(4),
   BELOW_SECOND_WORD(-1);

	private final int addIndent;
	
   public static final int SIZE = java.lang.Integer.SIZE;

   private SelectUnionNextSelectPos(int addIndent) {
   	this.addIndent = addIndent;
   }

   public int getAddIndent() {
   	return addIndent;
   }

   public int getValue() {
      return this.ordinal();
   }

   public static SelectUnionNextSelectPos forValue(int value) {
      return values()[value];
   }	
}
