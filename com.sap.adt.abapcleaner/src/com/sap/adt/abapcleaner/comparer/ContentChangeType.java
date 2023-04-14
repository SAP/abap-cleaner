package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration indicating whether in a {@link DiffLine},</p>  
 * <ul>
 * <li>text content has not changed ({@link ContentChangeType#NONE})</li>
 * <li>text content has changed ({@link ContentChangeType#CONTENT_CHANGED})</li>
 * </ul>
 * <p>The enumeration is also used to express the minimum level of change that shall be highlighted in the display.</p>
 * <p>See also {@link IndentChangeType}, {@link InnerSpaceChangeType} and {@link CaseChangeType}.</p> 
 */
public enum ContentChangeType  {
   NOT_APPLICABLE,
   NONE,
   CONTENT_CHANGED,
   NEVER;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ContentChangeType forValue(int value) {
      return values()[value];
   }
}