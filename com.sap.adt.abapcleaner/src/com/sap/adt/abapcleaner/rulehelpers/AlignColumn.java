package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.parser.*;
import java.lang.IllegalStateException;

public class AlignColumn {
	private final AlignTable parentTable;
	private final int index;
	private int cellCount;
	public boolean rightAlign;
	private boolean forceLineBreakAfter;
	private boolean overrideWidthWith1;
	private int minimumWidth;
	/** the forced indent of the column, to which the basicIndent is added **/
	private int forceIndent = -1;

	// variables that are updated on .addCell() and may be recalculated with .recalculate()
	private boolean isValid;
	private int maxMonoLineWidth;
	private int maxMultiLineWidth;

	// the effective indent is only determined in AlignTable.align()
	private int effectiveIndent;
	
	final int getIndex() { return index; }

	public final int getCellCount() { return cellCount; }

	public final boolean isEmpty() { return (cellCount == 0); }

	public final boolean getAreAllCellsOccupied() { return (cellCount == parentTable.getLineCount()); }

	public final boolean getForceLineBreakAfter() { return forceLineBreakAfter; }

	public final int getForceIndent() { return forceIndent; }

	public final int getEffectiveIndent() { return effectiveIndent; }
	
	public final AlignCell getCellFromLine(int lineIndex) {
		return parentTable.getLine(lineIndex).getCell(index);
	}

	AlignColumn(AlignTable parentTable, int index) {
		this.parentTable = parentTable;
		this.index = index;
		isValid = true;
	}

	final void addCell(AlignCell cell) {
		cell.parentColumn = this;
		++cellCount;
		if (overrideWidthWith1) {
			maxMonoLineWidth = 1;
			maxMultiLineWidth = 1;
		} else {
			maxMonoLineWidth = Math.max(maxMonoLineWidth, cell.getMonoLineWidth());
			maxMultiLineWidth = Math.max(maxMultiLineWidth, cell.getMultiLineWidth());
		}
	}

	final void removeCell(AlignCell cell) {
		--cellCount;
		invalidate();
	}

	final void invalidate() {
		isValid = false;
	}

	public final void clear() {
		clearStats();
		for (AlignLine line : parentTable.getLines())
			line.clearCell(index);
		isValid = true;
	}

	private void clearStats() {
		maxMonoLineWidth = 0;
		maxMultiLineWidth = 0;
		cellCount = 0;
	}

	final void recalculate() {
		clearStats();
		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			if (cell != null)
				addCell(cell);
		}
		isValid = true;
	}

	final int getMaxMonoLineWidthWithSpaceLeft() {
		if (!isValid)
			recalculate();
		return isEmpty() ? 0 : Math.max(minimumWidth, maxMonoLineWidth) + 1;
	}

	final int getMaxMultiLineWidthWithSpaceLeft() {
		if (!isValid)
			recalculate();
		return isEmpty() ? 0 : Math.max(minimumWidth, maxMultiLineWidth) + 1;
	}

	/**
	 * Appends all Tokens and Terms in this column to those of the respective previous occupied column
	 * (which means that no separate horizontal space will be reserved for them); 
	 * returns true if whitespace was changed
	 */
	public final boolean joinIntoPreviousColumns() throws UnexpectedSyntaxException {
		if (index == 0)
			throw new IllegalStateException("This method cannot be called on the first column!");

		boolean changed = false;
		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			if (cell == null)
				continue;

			for (int i = index - 1; i >= 0; --i) {
				AlignCell destCell = line.getCell(i);
				if (destCell != null) {
					if (!cell.getFirstToken().isFirstTokenInLine()) {
						if (cell.getFirstToken().setWhitespace()) {
							changed = true;
						}
					}
					Term joinedTerm = Term.createForTokenRange(destCell.getFirstToken(), cell.getLastToken());
					line.setCell(i, new AlignCellTerm(joinedTerm), true);
					parentTable.getColumn(i).invalidate();
					break;
				} else if (i == 0) {
					line.setCell(0, cell);
					parentTable.getColumn(0).invalidate();
					break;
				}
			}
			line.clearCell(index);
		}
		clearStats();
		return changed;
	}
	
	public void setForceLineBreakAfter(boolean overrideWidthWith1) {
		this.forceLineBreakAfter = true;
		this.overrideWidthWith1 = overrideWidthWith1;
		invalidate();
	}

	/**
	 * forces the indent of this column to be basicIndent + value
	 * @param value
	 */
	public void setForceIndent(int value) {
		forceIndent = value;
		invalidate();
	}
	
	public void setEffectiveIndent(int value) {
		effectiveIndent = value;
	}
	
	public boolean hasAnyLineBreakBefore() {
		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			if (cell == null)
				continue;

			if (cell.getFirstToken().lineBreaks > 0)
				return true;
		}
		return false;
	}
	
	public boolean removeLineBreaksBefore() {
		boolean success = true;
		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			if (cell == null)
				continue;

			Token firstToken = cell.getFirstToken();
			if (firstToken.lineBreaks == 0)
				continue;

			Token prevToken = firstToken.getPrev();
			if (prevToken != null && prevToken.isComment())
				success = false;
			else 
				firstToken.setWhitespace();
		}
		return success;
	}

	public final int getLineIndexOfLastNonEmptyCell() {
		for (int lineIndex = parentTable.getLineCount() - 1; lineIndex >= 0; --lineIndex) {
			AlignCell cell = parentTable.getLine(lineIndex).getCell(index);
			if (cell != null)
				return lineIndex;
		}
		return -1;
	}

	public final AlignCell getLastNonEmptyCell() {
		for (int lineIndex = parentTable.getLineCount() - 1; lineIndex >= 0; --lineIndex) {
			AlignCell cell = parentTable.getLine(lineIndex).getCell(index);
			if (cell != null)
				return cell;
		}
		return null;
	}
	
	public final void setMinimumWidth(int minimumWidth) {
		this.minimumWidth = minimumWidth;
	}
	
	public final boolean isPreviousColumnFixedWidth() {
		if (index == 0)
			return false;
		
		int minWidth = -1;
		int maxWidth = -1;
		for (AlignLine line : parentTable.getLines()) {
			if (line.getCell(index) == null)
				continue;
			AlignCell prevCell = line.getCell(index - 1);
			if (prevCell == null)
				return false;
			int width = prevCell.getMultiLineWidth();
			minWidth = (minWidth < 0) ? width : Math.min(minWidth, width);  
			maxWidth = (maxWidth < 0) ? width : Math.max(maxWidth, width);  
		}
		return (minWidth >= 0 && minWidth == maxWidth);
	}
}
