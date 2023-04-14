package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.*;

public class AlignLine {
	private AlignTable parentTable;
	private AlignCell[] cells;
	private String simplifiedText;

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
	public final void setCell(int index, AlignCell cell, boolean allowOverwrite) {
		if (index < 0 || index >= cells.length)
			throw new IndexOutOfBoundsException("index");
		else if (!allowOverwrite && cells[index] != null)
			throw new IllegalArgumentException("Cell at this index was already set!");

		if (cell == null)
			throw new NullPointerException("cell");

		cells[index] = cell;
		parentTable.getColumn(index).addCell(cell);

		if (cell.hasCommentAtAnyLineEnd())
			parentTable.canAlignToMonoLine = false;
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
		if (simplifiedText != null)
			return simplifiedText;
		StringBuilder text = new StringBuilder();
		for (AlignCell cell : cells) {
			if (cell != null)
				text.append(cell.getSimplifiedText());
		}
		simplifiedText = text.toString();
		return simplifiedText;
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
		int lastLineWidth = cell.getEndIndexInLastLine() - cell.getStartIndexInFirstLine();
		int maxLineWidth = cell.getMaxEndIndexInAnyLine() - cell.getStartIndexInFirstLine();
		if (lastLineWidth + addWidth > maxLineWidth) {
			cell.setOverrideTextWidth(lastLineWidth + addWidth);
		}
	}
}