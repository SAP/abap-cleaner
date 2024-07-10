package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Represents the juxtaposition ('diff view') of two different versions of a single text line in a document ({@link DiffDoc}).
 * The two versions of the line are referred to as the {@link #leftLine} (old state) and {@link #rightLine} (new state).</p>
 * 
 * <p>As shown by the {@link #status}, the line may be unchanged, or it may have been deleted, added, replaced or its text changed.     
 * For the latter case ({@link LineStatus#CHANGED}), attribute {@link #changeTypes} specifies the exact type(s) of change(s).</p> 
 */
public class DiffLine {
   public int index;
   public final DisplayLine leftLine;
   public final DisplayLine rightLine;
   public final LineStatus status;

   final ChangeTypes changeTypes;

   final DisplayLine getDisplayLine(DisplaySide displaySide) {
      return (displaySide == DisplaySide.LEFT) ? leftLine : rightLine;
   }


   DiffLine(int index, DisplayLine leftLine, DisplayLine rightLine, LineStatus status, ChangeTypes changeTypes) {
      this.index = index;
      this.leftLine = leftLine;
      this.rightLine = rightLine;
      this.status = status;
      this.changeTypes = changeTypes;
   }

   final boolean matchesChangeTypesFilter(ChangeTypes filter) {
      if (status == LineStatus.EQUAL) 
         return false;
      else if (status == LineStatus.CHANGED) 
         return changeTypes.matchesFilter(filter);
      else  
         return filter.textOrLineChanges; 
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

   @Override
	public String toString() { // for debugging
		switch(status) {
			case EQUAL:
				return "== " + leftLine.toString();
			case CHANGED:
				return "<> " + leftLine.toString() + System.lineSeparator() + "   " + rightLine.toString();
			case LEFT_DELETED:
				return "-- " + leftLine.toString();
			case RIGHT_ADDED:
				return "++ " + rightLine.toString();
			case LEFT_DELETED_RIGHT_ADDED:
				return "-  " + leftLine.toString() + System.lineSeparator() + " + " + rightLine.toString();
			default:
				return "";
		}
	}
 }