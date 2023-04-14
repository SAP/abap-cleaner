package com.sap.adt.abapcleaner.rulehelpers;

public enum CommentIdentifierMode  {
   ABAP_CODE_ONLY,
   QUOT_MARK_COMMENTS_ONLY,
   ASTERISK_COMMENTS,
   ALL_COMMENT_LINES;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static CommentIdentifierMode forValue(int value) {
      return values()[value];
   }
}