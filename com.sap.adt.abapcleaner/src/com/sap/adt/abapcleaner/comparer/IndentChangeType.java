package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration indicating whether in a {@link DiffLine},</p>  
 * <ul>
 * <li>indentation at line start has not changed ({@link IndentChangeType#NONE})</li>
 * <li>indentation at line start has changed ({@link IndentChangeType#INDENT_CHANGED}), but text content has not changed</li>  
 * <li>text content has changed beyond changes to whitespace ({@link IndentChangeType#CONTENT_CHANGED})</li>
 * </ul>
 * <p>The enumeration is also used to express the minimum level of change that shall be highlighted in the display.</p>
 * <p>Note that the space <i>between</i> {@link Token}s in a {@link DiffLine} is handled by {@link InnerSpaceChangeType}.</p> 
 * <p>See also {@link InnerSpaceChangeType}, {@link CaseChangeType} and {@link ContentChangeType}.</p> 
 */
public enum IndentChangeType  {
   NOT_APPLICABLE,
   NONE,
   INDENT_CHANGED,
   CONTENT_CHANGED;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static IndentChangeType forValue(int value) {
      return values()[value];
   }
}