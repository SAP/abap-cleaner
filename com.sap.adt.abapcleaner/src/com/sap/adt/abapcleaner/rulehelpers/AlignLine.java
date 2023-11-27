package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.*;

public class AlignLine {
	private AlignTable parentTable;
	private AlignCell[] cells;

	private Token overlengthLineBreakToken = null;
	private Token overlengthFallbackToken1 = null;
	private Token overlengthFallbackToken2 = null;
	
	/** the Token (e.g. VALUE) within this cell before which a line break can be introduced if otherwise, line length would be exceeded */
	Token getOverlengthLineBreakToken() { return overlengthLineBreakToken; }
	
	/** the Token (e.g. TYPE) within this cell with which the overlengthLineBreakToken shall be aligned */
	Token getOverlengthFallbackToken1() { return overlengthFallbackToken1; }
	
	/** the Token (e.g. TYPE) within this cell with which the overlengthLineBreakToken shall be aligned (extreme cases) */
	Token getOverlengthFallbackToken2() { return overlengthFallbackToken2; }
	
	@Override
	public String toString() {
		return getSimplifiedText();
	}

	public final AlignCell getCell(AlignColumn column) {
		return cells[column.getIndex()];
	}

	public final AlignCell getCell(int index) {
		return cells[index];
	}

	AlignLine(AlignTable parentTable) {
		this.parentTable = parentTable;
		cells = new AlignCell[parentTable.maxColumnCount];
	}

	public final void setCell(int index, AlignCell cell) {
		setCell(index, cell, false);
	}
	public final void overwriteCell(int index, AlignCell cell) {
		setCell(index, cell, true);
	}
	private final void setCell(int index, AlignCell cell, boolean allowOverwrite) {
		if (index < 0 || index >= cells.length)
			throw new IndexOutOfBoundsException("index");
		else if (!allowOverwrite && cells[index] != null)
			throw new IllegalArgumentException("Cell at this index was already set!");

		if (cell == null)
			throw new NullPointerException("cell");

		// remove an existing cell first to decrease AlignColumn.cellCount and invalidate its statistics
		if (cells[index] != null)
			parentTable.getColumn(index).removeCell(cells[index]);
		
		cells[index] = cell;
		parentTable.getColumn(index).addCell(cell);

		if (cell.hasInnerComment()) {
			parentTable.canAlignToMonoLine = false;
		} else {
			for (int i = getLastCellIndex() - 1; i >= 0; --i) {
				if (cells[i] != null && cells[i].hasCommentAtEnd()) {
					parentTable.canAlignToMonoLine = false;
				} 
			}
		}
	}

	public final void clearCell(int index) {
		if (cells[index] != null) {
			parentTable.getColumn(index).removeCell(cells[index]);
			cells[index] = null;
		}
	}

	public final Token getFirstToken() {
		for (AlignCell cell : cells) {
			if (cell != null)
				return cell.getFirstToken();
		}
		return null;
	}

	public final int getLastCellIndex() {
		for (int i = cells.length - 1; i >= 0; --i) {
			AlignCell cell = cells[i];
			if (cell != null)
				return i;
		}
		return -1;
	}

	public final Token getLastToken() {
		int lastCellIndex = getLastCellIndex();
		return (lastCellIndex >= 0) ? cells[lastCellIndex].getLastToken() : null;
	}

	public final String getSimplifiedText() {
		StringBuilder sb = new StringBuilder();
		for (int column = 0; column < parentTable.maxColumnCount; ++ column) {
			if (column > 0)
				sb.append("|");
			AlignCell cell = cells[column];
			if (cell != null)
				sb.append(cell.getSimplifiedText(" "));
		}
		return sb.toString();
	}

	public final AlignCell getNextNonEmptyCellAfter(int index) {
		for (int i = index + 1; i < cells.length; ++i) {
			if (cells[i] != null)
				return cells[i];
		}
		return null;
	}

	public final boolean containsComments() {
		Token token = getFirstToken();
		Token lastToken = getLastToken();
		while (token != null) {
			if (token.isComment())
				return true;
			if (token == lastToken)
				break;
			token = token.getNext();
		};
		return false;
	}

	public final boolean containsInnerLineBreaks() {
		Token token = getFirstToken();
		Token lastToken = getLastToken();
		if (token == lastToken)
			return false;
		
		token = token.getNext();
		while (token != null) {
			if (token.lineBreaks > 0)
				return true;
			if (token == lastToken)
				break;
			token = token.getNext();
		};
		return false;
	}
	
	/**
	 * adds the supplied width to the (last line of the) last cell by overriding its text width, if needed
	 * @param addWidth - the number of extra characters 
	 */
	public final void addWidthToEnd(int addWidth) {
		int lastCellIndex = getLastCellIndex();
		if (lastCellIndex < 0) 
			return;
		AlignCell cell = cells[lastCellIndex];
		// do not add width if the cell explicitly overrides the text width of its content
		if (cell.overridesTextWidth())
			return;
		int lastLineWidth = cell.getEndIndexInLastLine() - cell.getStartIndexInFirstLine();
		int maxLineWidth = cell.getMaxEndIndexInAnyLine() - cell.getStartIndexInFirstLine();
		if (lastLineWidth + addWidth > maxLineWidth) {
			cell.setOverrideTextWidth(lastLineWidth + addWidth);
		}
	}

	/**
	 * sets tokens used to introduce an extra line break if line length would be exceeded otherwise
	 * @param overlengthLineBreakToken - the Token before which a line break may be introduced if needed
	 * @param overlengthFallbackToken1 - an earlier Token with which the overlengthLineBreakToken shall be aligned 
	 * @param overlengthFallbackToken2 - an even earlier Token for alignment (may be null)
	 */
	public void setOverlengthLineBreakToken(Token overlengthLineBreakToken, Token overlengthFallbackToken1, Token overlengthFallbackToken2) {
		this.overlengthLineBreakToken = overlengthLineBreakToken;
		this.overlengthFallbackToken1 = overlengthFallbackToken1;
		this.overlengthFallbackToken2 = overlengthFallbackToken2;
	}
}