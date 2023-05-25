package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration for the type of change (indent change, inner space change, upper/lower case change, text or line change) 
 * which was detected for a {@link HighlightBit} when comparing two versions of a code line.</p> 
 */
enum HighlightBitType  {
   INDENT_CHANGE,
   INNER_SPACE_CHANGE,
   CASE_CHANGE,
   TEXT_CHANGE;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() { return this.ordinal(); }

   public static HighlightBitType forValue(int value) {
      return values()[value];
   }
}