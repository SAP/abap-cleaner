package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.parser.*;
import java.lang.IllegalStateException;
import java.util.ArrayList;

public class AlignColumn {
	private final AlignTable parentTable;
	private final int index;
	private int cellCount;
	public boolean rightAlign;
	private boolean forceLineBreakAfter;
	private boolean overrideWidthWith1;
	private int minimumWidth;
	/** the AlignColumn on which the forcedIndentOffset is based, using its effective indent; 
	 * if this is null, basicIndent will be used as the offset**/
	private AlignColumn forceIndentBaseColumn;
	/** the forced indent offset, which is added to the effective indent of the forceIndentBaseColumn 
	 * (or if that is null, to the basicIndent) **/
	private int forceIndentOffset = -1;

	// variables that are updated on .addCell() and may be recalculated with .recalculate()
	private boolean isValid;
	private int maxMonoLineWidth;
	private int maxMultiLineWidth;

	// the effective indent is only determined in AlignTable.align()
	private int effectiveIndent;
	
	final int getIndex() { return index; }

	// cellCount can be used without recalculation, even if isValid == false
	public final int getCellCount() { return cellCount; }

	public final boolean isEmpty() { return (cellCount == 0); }

	public final boolean getAreAllCellsOccupied() { return (cellCount == parentTable.getLineCount()); }

	public final boolean getForceLineBreakAfter() { return forceLineBreakAfter; }

	public final AlignColumn getForceIndentBaseColumn() { return forceIndentBaseColumn; }

	public final int getForceIndentOffset() { return forceIndentOffset; }

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

	public final Command[] joinIntoPreviousColumns(boolean condenseSpaceBetweenCells) throws UnexpectedSyntaxException {
		return joinIntoPreviousColumns(condenseSpaceBetweenCells, 1, false);
	}
	
	public final Command[] joinIntoPreviousColumns(boolean condenseSpaceBetweenCells, boolean removeLineBreaksBetweenCells) throws UnexpectedSyntaxException {
		return joinIntoPreviousColumns(condenseSpaceBetweenCells, 1, removeLineBreaksBetweenCells);
	}

	/**
	 * Appends all Tokens and Terms in this column to those of the respective previous occupied column
	 * (which means that no separate horizontal space will be reserved for them); 
	 * returns true if whitespace was changed
	 */
	public final Command[] joinIntoPreviousColumns(boolean condenseSpaceBetweenCells, int condensedSpacesLeft, boolean removeLineBreaksBetweenCells) throws UnexpectedSyntaxException {
		if (index == 0)
			throw new IllegalStateException("This method cannot be called on the first column!");

		ArrayList<Command> changedCommands = new ArrayList<Command>();
		Command lastChangedCommand = null;

		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			if (cell == null)
				continue;

			// remove the right-hand cell first, so AlignCell.setCell can determine .canAlignToMonoLine correctly
			line.clearCell(index);

			for (int i = index - 1; i >= 0; --i) {
				AlignCell destCell = line.getCell(i);
				if (destCell != null) {
					Token firstInCell = cell.getFirstToken();
					if (condenseSpaceBetweenCells && (removeLineBreaksBetweenCells || !firstInCell.isFirstTokenInLine())) {
						boolean changed = false;
						Token prevToken = firstInCell.getPrev();
						if (prevToken == null || !prevToken.isComment()) {
							if (cell.setWhitespace(0, condensedSpacesLeft, true, false, null)) {
								changed = true;
							}
						} else {
							Token prevCode = firstInCell.getPrevCodeToken();
							if (cell.setWhitespace(1, prevCode.getEndIndexInLine() + 1, true, false, null)) {
								changed = true;
							}
						}
						if (changed) {
							Command command = firstInCell.getParentCommand();
							if (command != lastChangedCommand) {
								changedCommands.add(command);
								lastChangedCommand = command;
							}
						}
					}
					// create a joined Term, but preserve special settings of the destination cell (additionalIndent, overrideTextWidth)
					Term joinedTerm = Term.createForTokenRange(destCell.getFirstToken(), cell.getLastToken());
					AlignCellTerm alignCellTerm = AlignCellTerm.createSpecial(joinedTerm, destCell.additionalIndent, (destCell.overrideTextWidth == 1)); 
					line.overwriteCell(i, alignCellTerm);
					
					parentTable.getColumn(i).invalidate();
					break;
				} else if (i == 0) {
					line.setCell(0, cell);
					parentTable.getColumn(0).invalidate();
					break;
				}
			}
		}
		clearStats();
		return changedCommands.toArray(new Command[0]);
	}
	
	public void setForceLineBreakAfter(boolean overrideWidthWith1) {
		this.forceLineBreakAfter = true;
		this.overrideWidthWith1 = overrideWidthWith1;
		invalidate();
	}

	public void setForceLineBreakAfter(boolean overrideWidthWith1, int offsetForNextColumn) {
		this.forceLineBreakAfter = true;
		this.overrideWidthWith1 = overrideWidthWith1;
		parentTable.getColumn(index + 1).setForceIndent(this, offsetForNextColumn);
		invalidate();
	}

	/**
	 * forces the indent of this column to be basicIndent + offset
	 * @param value
	 */
	public void setForceIndent(int offset) {
		forceIndentBaseColumn = null;
		forceIndentOffset = offset;
		invalidate();
	}
	
	/**
	 * forces the indent of this column to be the effective indent of the supplied baseColumn + offset
	 * @param value
	 */
	public void setForceIndent(AlignColumn baseColumn, int offset) {
		forceIndentBaseColumn = baseColumn;
		forceIndentOffset = offset;
		invalidate();
	}
	
	/**
	 * forces the indent of this column to be the effective indent of the supplied baseColumn + offset
	 * @param value
	 */
	public void setForceIndent(int baseColumnIndex, int offset) {
		forceIndentBaseColumn = parentTable.getColumn(baseColumnIndex);
		forceIndentOffset = offset;
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

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (AlignLine line : parentTable.getLines()) {
			AlignCell cell = line.getCell(this);
			sb.append((cell == null) ? "null" : cell.getSimplifiedText());
			sb.append(System.lineSeparator());
		}
		
		return sb.toString();
	}

}
