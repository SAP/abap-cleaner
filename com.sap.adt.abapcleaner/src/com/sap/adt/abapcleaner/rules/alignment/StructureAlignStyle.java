package com.sap.adt.abapcleaner.rules.alignment;

public enum StructureAlignStyle {
	/** align across all nesting levels of a structure */
	ACROSS_LEVELS,
	/** align each nesting level of a structure independently (like Pretty Printer), resuming alignment after an inner structure ends */
	PER_LEVEL,
	/** align each section independently, starting alignment anew after each BEGIN OF or END OF */
	PER_SECTION;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static StructureAlignStyle forValue(int value) {
      return values()[value];
   }	
}
