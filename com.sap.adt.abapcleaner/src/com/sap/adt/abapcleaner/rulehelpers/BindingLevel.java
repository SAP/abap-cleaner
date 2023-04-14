package com.sap.adt.abapcleaner.rulehelpers;

/** 
 sorted from least binding (EQUIV) to most binding (COMPARISON_OR_PREDICATE)
*/
enum BindingLevel  {
   UNKNOWN,
   EQUIV,
   OR,
   AND,
   NOT,
   COMPARISON_OR_PREDICATE; // comparison operator, predicate operator IS, predicate function line_exists() / matches() / contains[..]()

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static BindingLevel forValue(int value) {
      return values()[value];
   }
}