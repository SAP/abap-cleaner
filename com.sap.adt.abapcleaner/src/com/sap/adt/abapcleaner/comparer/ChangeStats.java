package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Contains statistical information on the number of changes of various types
 * that were found (on {@link DiffLine} level) when comparing the two text versions of a {@link DiffDoc}.</p> 
 */
public class ChangeStats {
   public final int indentCount;
   public final int innerSpaceCount;
   public final int caseCount;
   public final int contentCount;

   public int getTotalCount() { return indentCount + innerSpaceCount + caseCount + contentCount; }
   
   public static ChangeStats createEmpty() {
   	return new ChangeStats(0, 0, 0, 0);
   }
   
   public static ChangeStats create(int indentCount, int innerSpaceCount, int caseCount, int contentCount) {
   	return new ChangeStats(indentCount, innerSpaceCount, caseCount, contentCount);
   }
   
   private ChangeStats(int indentCount, int innerSpaceCount, int caseCount, int contentCount) {
      this.indentCount = indentCount;
      this.innerSpaceCount = innerSpaceCount;
      this.caseCount = caseCount;
      this.contentCount = contentCount;
   }
}