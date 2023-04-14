package com.sap.adt.abapcleaner.rulebase;

public enum RuleSource  {
   ABAP_STYLE_GUIDE,
   CODE_PAL_FOR_ABAP,
   ABAP_KEYWORD_DOCU,
   ABAP_CLEANER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static RuleSource forValue(int value) {
      return values()[value];
   }
}