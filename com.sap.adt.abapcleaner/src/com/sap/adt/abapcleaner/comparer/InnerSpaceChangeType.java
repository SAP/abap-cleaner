package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration indicating whether in a {@link DiffLine},</p>  
 * <ul>
 * <li>space between {@link Token}s has not changed ({@link InnerSpaceChangeType#NONE})</li>
 * <li>space between {@link Token}s has changed ({@link InnerSpaceChangeType#INNER_SPACE_CHANGED}), but text content has not changed</li>  
 * <li>text content has changed beyond changes to whitespace ({@link InnerSpaceChangeType#CONTENT_CHANGED})</li>
 * </ul>
 * <p>The enumeration is also used to express the minimum level of change that shall be highlighted in the display.</p>
 * <p>Note that the space <i>before</i> the first {@link Token} in a {@link DiffLine} is handled by {@link IndentChangeType}.</p> 
 * <p>See also {@link IndentChangeType}, {@link CaseChangeType} and {@link ContentChangeType}.</p> 
 */
public enum InnerSpaceChangeType  {
   NOT_APPLICABLE,
   NONE,
   INNER_SPACE_CHANGED,
   CONTENT_CHANGED;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() { return this.ordinal(); }

   public static InnerSpaceChangeType forValue(int value) {
      return values()[value];
   }
}