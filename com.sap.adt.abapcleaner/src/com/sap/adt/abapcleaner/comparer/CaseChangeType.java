package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration indicating whether in a {@link DiffLine},</p>  
 * <ul>
 * <li>upper/lower case has not changed ({@link CaseChangeType#NONE})</li>
 * <li>upper/lower case has changed ({@link CaseChangeType#CASE_CHANGED}), but text content has not changed</li>  
 * <li>text content has changed beyond changes to upper/lower case ({@link CaseChangeType#CONTENT_CHANGED})</li>
 * </ul>
 * <p>The enumeration is also used to express the minimum level of change that shall be highlighted in the display.</p>
 * <p>See also {@link IndentChangeType}, {@link InnerSpaceChangeType} and {@link ContentChangeType}.</p> 
 */
public enum CaseChangeType  {
   NOT_APPLICABLE,
   NONE,
   CASE_CHANGED,
   CONTENT_CHANGED;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static CaseChangeType forValue(int value) {
      return values()[value];
   }
}