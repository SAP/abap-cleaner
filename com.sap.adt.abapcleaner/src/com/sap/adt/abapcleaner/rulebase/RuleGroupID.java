package com.sap.adt.abapcleaner.rulebase;

public enum RuleGroupID  {
   EMPTY_LINES,
   SPACES,
   DECLARATIONS,
   SYNTAX,
   COMMANDS,
   PRETTY_PRINTER,
   ALIGNMENT, 
   
   DDL_ANNOTATIONS,
   DDL_POSITION,
   DDL_SPACES,
   DDL_ALIGNMENT;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static RuleGroupID forValue(int value) {
      return values()[value];
   }
}