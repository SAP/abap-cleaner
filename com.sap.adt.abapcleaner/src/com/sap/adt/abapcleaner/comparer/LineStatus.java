package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Enumeration expressing for a {@link DiffLine}, whether</p>
 * <ul>
 * <li>the original line remains unchanged ({@link LineStatus#EQUAL})</li>
 * <li>the text of the 'left' line was changed into the text of the 'right' line ({@link LineStatus#CHANGED})</li>
 * <li>the 'left' line was deleted, with no matching line on the 'right' side ({@link LineStatus#LEFT_DELETED})</li>   
 * <li>the 'right' line was added, with no matching line on the 'left' side ({@link LineStatus#RIGHT_ADDED})</li>   
 * <li>the 'left' line was deleted, while another, unrelated 'right' line was added in its place ({@link LineStatus#LEFT_DELETED_RIGHT_ADDED})</li>
 * </ul>   
 */
public enum LineStatus  {
   EQUAL,
   CHANGED,
   LEFT_DELETED,
   RIGHT_ADDED,
   LEFT_DELETED_RIGHT_ADDED;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static LineStatus forValue(int value) {
      return values()[value];
   }
}