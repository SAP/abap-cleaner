package com.sap.adt.abapcleaner.rulehelpers;

public enum CommentIdentifierMode  {
   CODE_ONLY,
   NON_LINE_START_COMMENTS, // comment lines that start with " in ABAP, or any comment line in DDL that does NOT have // at line-start
   LINE_START_COMMENTS,     // comment lines as you would get them when commenting out multiple lines of code (i.e. starting with * in ABAP or having // at line-start in DDL)
   ALL_COMMENT_LINES;       // all comment lines

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static CommentIdentifierMode forValue(int value) {
      return values()[value];
   }
}