package com.sap.adt.abapcleaner.rulehelpers;

public enum RelationalExpressionType  {
   NONE,
   COMPARISON, // e.g. "a < b"
   PREDICATE_EXPRESSION, // e.g. "... IS NOT BOUND"
   PREDICATE_FUNCTION; // e.g. line_exists( ... ), matches(), contains(), contains_any_of(), contains_any_not_of()

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static RelationalExpressionType forValue(int value) {
      return values()[value];
   }
}