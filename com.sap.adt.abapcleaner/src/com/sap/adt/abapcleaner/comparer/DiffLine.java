package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.StringUtil;

/**
 * <p>Represents the juxtaposition ('diff view') of two different versions of a single text line in a document ({@link DiffDoc}).
 * The two versions of the line are referred to as the {@link #leftLine} (old state) and {@link #rightLine} (new state).</p>
 * 
 * <p>As shown by the {@link #status}, the line may be unchanged, or it may have been deleted, added, replaced or its text changed.     
 * For the latter case ({@link LineStatus#CHANGED}), attributes {@link #indentChange}, {@link #innerSpaceChange} and {@link #caseChange}
 * specify the exact type(s) of change(s).</p> 
 */
public class DiffLine {
   public int index;
   public final DisplayLine leftLine;
   public final DisplayLine rightLine;
   public final LineStatus status;

   final IndentChangeType indentChange;
   final InnerSpaceChangeType innerSpaceChange;
   final CaseChangeType caseChange;

   final DisplayLine getDisplayLine(DisplaySide displaySide) {
      return (displaySide == DisplaySide.LEFT) ? leftLine : rightLine;
   }


   DiffLine(int index, DisplayLine leftLine, DisplayLine rightLine, LineStatus status, IndentChangeType indentChange, InnerSpaceChangeType whitespaceChange, CaseChangeType caseChange) {
      this.index = index;
      this.leftLine = leftLine;
      this.rightLine = rightLine;
      this.status = status;
      this.indentChange = indentChange;
      this.innerSpaceChange = whitespaceChange;
      this.caseChange = caseChange;
   }

   final boolean highlight(IndentChangeType minIndentChange, InnerSpaceChangeType minWhitespaceChange, CaseChangeType minCaseChange) {
      if (status == LineStatus.EQUAL)
         return false;
      else if (status == LineStatus.CHANGED)
         return (indentChange.getValue() >= minIndentChange.getValue() || innerSpaceChange.getValue() >= minWhitespaceChange.getValue() || caseChange.getValue() >= minCaseChange.getValue());
      else if (status == LineStatus.LEFT_DELETED && StringUtil.isNullOrEmpty(leftLine.getText()))
         return (IndentChangeType.INDENT_CHANGED.getValue() >= minIndentChange.getValue());
      else
         return true;
   }

   final boolean isEmptyWithNoChanges() {
      return (status == LineStatus.EQUAL && leftLine != null && leftLine.isEmpty() && rightLine != null && rightLine.isEmpty());
   }
   
   final boolean isEmpty() {
      return (leftLine == null || leftLine.isEmpty()) && (rightLine == null || rightLine.isEmpty());
   }
   
   final boolean containsSourceLineNum(int sourceLineNum) {
   	return (leftLine != null && leftLine.parentCommand.containsSourceLineNum(sourceLineNum))
   	    || (rightLine != null && rightLine.parentCommand.containsSourceLineNum(sourceLineNum));
   }
 }