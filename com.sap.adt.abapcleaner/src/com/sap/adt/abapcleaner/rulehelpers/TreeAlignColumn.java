package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.*;
import java.util.*;

/**
 * In the context of aligning a logical expression, a TreeAlignColumn represents AlignCells that shall be horizontally aligned.
 * As long as the cells in the next line of the logical expression have the same "signature" of consecutive TreeAlignColumns,
 * these cells will be added to the already created TreeAlignColumns (potentially skipping unused columns that are reserved for opening parentheses, NOT keywords etc.)
 * Once the "signature" of the cells in the current line deviates from that of the cells in the previous line(s),
 * new TreeAlignColumns will be instantiated and appended to the last matching TreeAlignColumn.
 */
class TreeAlignColumn {
	private static boolean columnTypeClosesBracket(TreeAlignColumnType columnType) {
		switch (columnType) {
			case CLOSE_BRACKET_FOR_BOOL_OP:
			case CLOSE_BRACKET_FOR_NOT:
			case CLOSE_BRACKET_FOR_REL:
				return true;
			default:
				return false;
		}
	}

	private static boolean columnTypeOpensBracket(TreeAlignColumnType columnType) {
		switch (columnType) {
			case OPEN_BRACKET_FOR_BOOL_OP:
			case OPEN_BRACKET_FOR_NOT:
			case OPEN_BRACKET_FOR_REL:
				return true;
			default:
				return false;
		}
	}

	private static boolean columnTypeBelongsToBoolOp(TreeAlignColumnType columnType) {
		switch (columnType) {
			case OPEN_BRACKET_FOR_BOOL_OP:
			case BOOL_OPERATOR:
			case CLOSE_BRACKET_FOR_BOOL_OP:
				return true;
			default:
				return false;
		}
	}

	// ----------------------------------------------------------------------

	private ArrayList<AlignCell> cells = new ArrayList<AlignCell>();
	final TreeAlignColumn prev;
	private TreeAlignColumn next;
	private int indexFromLineStart;
	final int brackLevel;
	final TreeAlignColumnType columnType;
	int indentForNextColumn;
	boolean rightAlign;
	boolean doNotAlign;
	private int branchCount;

	private int maxMonoLineWidth;
	private int maxMultiLineWidth;

	final TreeAlignColumn getNext() { return next; }

	final int getBranchCount() { return branchCount; }

	final boolean isEmpty() { return cells.isEmpty(); }

	final int getCellCount() { return cells.size(); }

	final AlignCell getCell(int index) {
		return cells.get(index);
	}

	final java.lang.Iterable<AlignCell> getCells() { return cells; }

	final boolean getOpensBracket() { return columnTypeOpensBracket(columnType); }

	final boolean getClosesBracket() { return columnTypeClosesBracket(columnType); }

	final int getMaxMonoLineWidth() { return isEmpty() ? 0 : maxMonoLineWidth; }

	final int getMaxMultiLineWidth() { return isEmpty() ? 0 : maxMultiLineWidth; }

	final int brackLevelForNextColumn(TreeAlignColumnType nextColumnType) {
		return brackLevel + (getOpensBracket() ? 1 : 0) - (columnTypeClosesBracket(nextColumnType) ? 1 : 0);
	}

	final boolean getBelongsToBoolOp() { return columnTypeBelongsToBoolOp(columnType); }

	TreeAlignColumn(TreeAlignColumnType columnType, TreeAlignColumn previousColumn) {
		prev = previousColumn;
		if (previousColumn != null) {
			++previousColumn.branchCount;
			previousColumn.next = this;
		}

		indexFromLineStart = (previousColumn == null) ? 0 : previousColumn.indexFromLineStart + 1;
		this.columnType = columnType;
		// .rightAlign will only be set in TreeAlign.align()

		brackLevel = (previousColumn == null) ? 0 : previousColumn.brackLevelForNextColumn(columnType);
	}

	final void add(AlignCell cell) {
		cells.add(cell);
		updateWidth(cell);
	}

	final void insertFirst(AlignCell cell) {
		cells.add(0, cell);
		updateWidth(cell);
	}

	private void updateWidth(AlignCell cell) {
		maxMonoLineWidth = Math.max(maxMonoLineWidth, cell.getMonoLineWidth());
		maxMultiLineWidth = Math.max(maxMultiLineWidth, cell.getMultiLineWidth());
	}

	private String getSimplifiedText() { return getSimplifiedText(" | "); }
	private String getSimplifiedText(String separator) {
		StringBuilder text = new StringBuilder();
		for (AlignCell cell : cells) {
			if (text.length() > 0)
				text.append(separator);
			text.append(cell.getSimplifiedText());
		}
		return text.toString();
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();

		result.append("lv " + String.valueOf(brackLevel));
		result.append(" (idx " + Cult.getPaddedString(indexFromLineStart, 2, '0') + "): "); // minimum 2 digits for the number of lines to be aligned
		result.append(StringUtil.repeatChar(' ', 3 * brackLevel));
		result.append(columnType.toString() + ": ");
		result.append(isEmpty() ? "{empty}" : getSimplifiedText());
		if (next == null)
			result.append(" ------ branch end");
		else if (branchCount > 1)
			result.append(" <<< " + Cult.format(branchCount) + " branches");
		return result.toString();
	}

	final boolean isFirstNonEmptyAfterBranch() {
		if (isEmpty())
			return false;
		TreeAlignColumn column = prev;
		while (column != null) {
			if (column.branchCount > 1)
				return true;
			if (!column.isEmpty())
				return false;
			column = column.prev;
		}
		return true;
	}
}