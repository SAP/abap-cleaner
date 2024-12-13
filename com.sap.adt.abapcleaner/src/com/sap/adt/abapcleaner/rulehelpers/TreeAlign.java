package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

import java.util.*;

public class TreeAlign {
	private final ArrayList<TreeAlignColumn> allColumns = new ArrayList<TreeAlignColumn>();

	private final LogicalExpression logicalExpression;
	private final boolean isInOOContext;
	private TreeAlignColumn curCol; // the current column during build
	private HashSet<Token> lastTokensOfAlignStopCells = new HashSet<Token>();

	private TreeAlign(LogicalExpression logicalExpression) {
		this.logicalExpression = logicalExpression;
		this.isInOOContext = logicalExpression.getFirstToken().getParentCommand().isInOOContext();
	}

	public static TreeAlign createFrom(LogicalExpression logicalExpression) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxException {
		if (logicalExpression == null)
			throw new NullPointerException("logicalExpression");

		TreeAlign result = new TreeAlign(logicalExpression);
		logicalExpression.toTreeAlign(result);
		return result;
	}

	final void add(TreeAlignColumnType columnType, Token token) {
		add(columnType, (token == null) ? null : new AlignCellToken(token));
	}

	final void add(TreeAlignColumnType columnType, Token firstToken, Token lastToken) throws UnexpectedSyntaxException {
		add(columnType, (firstToken == null) ? null : new AlignCellTerm(Term.createForTokenRange(firstToken, lastToken)));
	}

	private void add(TreeAlignColumnType columnType, AlignCell cell) {
		// if the first token of the cell is in a new line, however, the previous Token is a stand-alone AND / OR / EQUIV / ( 
		// on its own line, then continue right of that previous Token
		boolean continuesLine = (cell != null && cell.getFirstToken().lineBreaks == 0);
		if (cell != null && !continuesLine) {
			Token prev = cell.getFirstToken().getPrevNonCommentToken();
			if (prev.isFirstTokenInLine() && (prev.isAnyKeyword("AND", "OR", "EQUIV") || prev.textEquals("("))) {
				continuesLine = true;
			} 
		}
		
		if (cell == null || continuesLine || cell.getFirstToken() == logicalExpression.getFirstToken()) {
			TreeAlignColumn nextCol = (curCol == null) ? null : curCol.getNext();
			boolean nextColHasContent = (nextCol != null && !nextCol.isEmpty());
			boolean newCellHasContent = (cell != null);
			if (nextCol == null || nextCol.columnType != columnType || nextCol.getOpensBracket() && (nextColHasContent != newCellHasContent)) {
				// structural mismatch - start a new column (i.e. a new, independently aligned 'branch' of the tree)
				curCol = new TreeAlignColumn(columnType, curCol);
				allColumns.add(curCol);
			} else {
				// structural match with the next column of previous line - continue matching
				curCol = nextCol;
			}
		} else {
			// find first column with matching brackLevel and columnType
			int brackLevel = (curCol == null) ? 0 : curCol.brackLevelForNextColumn(columnType);
			TreeAlignColumn match = null;
			TreeAlignColumn partialMatch = null;
			while (curCol != null) {
				// (partial) match found?
				if (curCol.brackLevel == brackLevel && !(curCol.columnType == TreeAlignColumnType.BOOL_OPERATOR && columnType == TreeAlignColumnType.OPEN_BRACKET_FOR_BOOL_OP)) {
					partialMatch = curCol;
					if (curCol.columnType == columnType)
						match = curCol;
					// continue searching for an even earlier column
				}
				// do not go beyond the Boolean operator's opening bracket (unless it is nested deeper or another Boolean operator is to be added)
				if (curCol.columnType == TreeAlignColumnType.OPEN_BRACKET_FOR_BOOL_OP) {
					if (curCol.brackLevel < brackLevel) {
						break;
					} else if (curCol.brackLevel == brackLevel && columnType != TreeAlignColumnType.BOOL_OPERATOR && columnType != TreeAlignColumnType.OPEN_BRACKET_FOR_BOOL_OP) {
						break;
					}
				}
				curCol = curCol.prev;
			}
			if (match != null) {
				curCol = match;
			} else {
				if (partialMatch != null && partialMatch.prev != null) {
					curCol = partialMatch.prev;
				}
				curCol = new TreeAlignColumn(columnType, curCol);
				allColumns.add(curCol);
			}
		}
		if (cell != null) {
			curCol.add(cell);
		}
	}

	public final boolean align(Token keyword, AlignStyle keywordAlignStyle, boolean rightAlignComparisonOps, boolean keepMultiline, boolean onlyAlignSameObjects,
			int maxInnerSpaces, boolean attachParentheses) {
		boolean changed = false;

		// determine the leftmost indent in all content to align
		int leftMostIndent = Integer.MAX_VALUE;
		for (TreeAlignColumn column : allColumns) {
			for (AlignCell cell : column.getCells()) {
				Token firstToken = cell.getFirstToken();
				if (firstToken.lineBreaks > 0)
					leftMostIndent = Math.min(leftMostIndent, firstToken.spacesLeft);
			}
		}

		// align first EQUIV / AND / OR column with the keyword "IF", "ELSEIF", "CHECK", "WHILE" or "WHERE" (right-aligned),
		// unless that keyword is further right than the following lines (e.g. with WHERE at line end)
		int basicIndent;
		if (keyword.isLastTokenInLineExceptComment() && keyword.getStartIndexInLine() > leftMostIndent) {
			basicIndent = leftMostIndent;
			// move basicIndent to the right if it is too far left: 
			// - if basicIndent is further right than the first code Token inside the parent parenthesis, use this Token's indent
			if (keyword.getParent() != null) {
				Token firstCodeChild = keyword.getParent().getFirstCodeChild();
				if (firstCodeChild != null && basicIndent < firstCodeChild.getStartIndexInLine()) {
					basicIndent = firstCodeChild.getStartIndexInLine();
				}
			}
			// - move basicIndent at least 2 blanks behind the indent of the command
			basicIndent = Math.max(basicIndent, keyword.getParentCommand().getFirstToken().spacesLeft + ABAP.INDENT_STEP);
			
		} else if ((keywordAlignStyle != AlignStyle.DO_NOT_ALIGN) && allColumns.size() >= 2 && allColumns.get(0).columnType == TreeAlignColumnType.OPEN_BRACKET_FOR_BOOL_OP
				&& allColumns.get(0).isEmpty() && allColumns.get(1).columnType == TreeAlignColumnType.BOOL_OPERATOR) {
			allColumns.get(1).insertFirst(new AlignCellToken(keyword));
			allColumns.get(1).rightAlign = (keywordAlignStyle == AlignStyle.RIGHT_ALIGN);
			basicIndent = keyword.getStartIndexInLine();
		} else {
			basicIndent = keyword.getEndIndexInLine() + 1;
		}

		// left- or right-align comparison operators, depending on configuration
		for (TreeAlignColumn column : allColumns) {
			if (column.columnType == TreeAlignColumnType.REL_OPERATOR)
				column.rightAlign = rightAlignComparisonOps;
		}

		// do not align closing brackets and Boolean operators at line end
		for (int i = allColumns.size() - 1; i >= 0; --i) {
			TreeAlignColumn column = allColumns.get(i);
			if (column.getNext() == null) {
				while (column != null && column.getBranchCount() <= 1 && (!column.isFirstNonEmptyAfterBranch() || column.columnType == TreeAlignColumnType.BOOL_OPERATOR) 
						&& (column.isEmpty() || column.getCellCount() == 1 || column.getClosesBracket() || column.columnType == TreeAlignColumnType.BOOL_OPERATOR)) {
					column.doNotAlign = true;
					column = column.prev;
				}
			}
		}

		// optionally stop aligning if relational expressions in consecutive lines work on different objects
		if (onlyAlignSameObjects) {
			for (TreeAlignColumn column : allColumns) {
				if (column.columnType == TreeAlignColumnType.REL_TERM1 || column.columnType == TreeAlignColumnType.REL_FUNCTION) {
					int cellCount = column.getCellCount();
					for (int i = 0; i < cellCount; ++i) {
						AlignCell prevCell = (i > 0) ? column.getCell(i - 1) : null;
						AlignCell cell = column.getCell(i);
						AlignCell nextCell = (i + 1 < cellCount) ? column.getCell(i + 1) : null;
						if ((prevCell == null || !cell.startsWithSameObjectAs(prevCell, isInOOContext)) && (nextCell == null || !cell.startsWithSameObjectAs(nextCell, isInOOContext)))
							stopAligningLineOf(cell);
					}
				}
			}
		}

		boolean isAfterAttachedOpeningParens = false;
		for (TreeAlignColumn column : allColumns) {
			int columnWidth = keepMultiline ? column.getMaxMultiLineWidth() : column.getMaxMonoLineWidth();

			// calculate the column indent (even if the column is empty, because this information will be used by the next column)
			int columnIndent;
			if (column.prev == null) {
				// start a fresh sequence of columns
				isAfterAttachedOpeningParens = false;
				columnIndent = basicIndent;
			} else {
				// continue a sequence
				columnIndent = column.prev.indentForNextColumn; // might still be decreased below for attached ")"
			}
			
			// calculate minimum spaces left - usually 1, but 0 in case of a preceding attached "(", or if this is an attached ")"
			int minSpacesLeft = isAfterAttachedOpeningParens ? 0 : 1;
			if (attachParentheses && column.getClosesBracket() && columnWidth == 1 && column.prev != null) {
				--columnIndent;
				minSpacesLeft = 0;
			}

			// determine for the next column(s):
			// - the number of spaces right of this column, and 
			// - whether this column is an attached "("
			int spacesRight;
			if (columnWidth == 0) {
				// column is not 'materialized'
				spacesRight = 0;
				// keep isAfterAttachedOpeningParens unchanged
				
			} else if (attachParentheses && column.getOpensBracket() && columnWidth == 1) {
				// for DDL or DCL, parentheses can be attached to the next column, depending on configuration
				spacesRight = 0;
				isAfterAttachedOpeningParens = true;
			
			} else {
				spacesRight = 1;
				isAfterAttachedOpeningParens = false;
			}
			
			column.indentForNextColumn = columnIndent + columnWidth + spacesRight;

			// propagate stopped alignment from the previous columns
			for (AlignCell cell : column.getCells()) {
				if (isAlignmentStoppedForLineOf(cell)) {
					stopAligningLineOf(cell);
				}
			}

			for (AlignCell cell : column.getCells()) {
				int currentIndent = cell.getStartIndexInFirstLine();
				int newIndent = columnIndent;
				if (column.rightAlign) {
					int cellWidth = keepMultiline ? cell.getMultiLineWidth() : cell.getMonoLineWidth();
					newIndent += (columnWidth - cellWidth); 
				}

				Token firstToken = cell.getFirstToken();
				int spacesLeft = firstToken.spacesLeft + (newIndent - currentIndent);
				if (columnIndent > 0) {
					spacesLeft = Math.max(spacesLeft, minSpacesLeft);
				}
				if (firstToken.lineBreaks == 0) {
					if (column.doNotAlign || (onlyAlignSameObjects && isAlignmentStoppedForLineOf(cell))) {
						spacesLeft = minSpacesLeft;
					} else if (spacesLeft > maxInnerSpaces && firstToken != keyword.getNext()) {
						// stop aligning if maximum number of inner spaces would be exceeded (except at line start)
						spacesLeft = minSpacesLeft;
						stopAligningLineOf(cell);
					}
				}
				if (cell.setWhitespace(firstToken.lineBreaks, spacesLeft, keepMultiline, true, null)) {
					changed = true;
				}
			}
		}

		return changed;
	}

	private void stopAligningLineOf(AlignCell cell) {
		Token lastToken = cell.getLastToken();
		if (!lastTokensOfAlignStopCells.contains(lastToken))
			lastTokensOfAlignStopCells.add(lastToken);
	}

	private boolean isAlignmentStoppedForLineOf(AlignCell cell) {
		Token firstToken = cell.getFirstToken();
		if (firstToken.isFirstTokenInLine() || firstToken.getPrev() == null)
			return false;
		return lastTokensOfAlignStopCells.contains(firstToken.getPrev());
	}
}
