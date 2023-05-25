package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * <p>Allows its client to navigate over a {@link DiffDoc}, i.e. over the 'diff view' of 
 * existing code (left) / cleaned code (right), by</p>
 * <ul>
 * <li>moving the current line ({@link #curLine}), including moving to the next change of a certain type</li> 
 * <li>selecting multiple lines (from {@link #selStartLine} to {@link #curLine})</li>
 * <li>searching the text on one or both sides of the {@link DiffDoc} and moving to the next match</li>
 * <li>getting statistical information on the current selection ({@link #getRuleStats(Profile)})</li>
 * <li>blocking or unblocking ABAP cleaner {@link Rule}s for the current selection ({@link #setBlockRuleInSelection(RuleID, boolean) setBlockRuleInSelection(...)})
 * <li>re-processing the parsing and cleaning of the current selection of code lines ({@link #reprocessSelection(Profile, int, String) reprocessSelection(...)})
 * </ul>
 * <p>The client may use {@link #setHighlight(boolean, boolean, boolean, boolean) setHighlight(...)} 
 * to specify which types of changes shall be highlighted ({@link #isLineHighlighted(DiffLine)}); 
 * navigation to the next change (and optionally, text search) is then filtered by these settings.</p>
 */
public class DiffNavigator {
	private Code code;
	private DiffDoc diffDoc;

	private int curLine;
	private int selStartLine; // may be equal, lower, or higher than curLine, or -1
	private String searchText;
	private int curSearchPos;
	private DisplaySide curSearchSide = DisplaySide.values()[0];

	private ChangeTypes highlight = ChangeTypes.createAllChanges();

	private boolean isInSearchMode;

	public final int getCurLine() { return curLine; }

	public final int getSelStartLine() { return selStartLine; }

	public final int getLineCount() { return (diffDoc == null) ? 0 : diffDoc.getLineCount(); }

	public final boolean isEmpty() { return (getLineCount() == 0); }

	public final int getLineNumberDigits() { return String.valueOf(getLineCount()).length(); }

	public final boolean areLinesSelected() { return (selStartLine != curLine); }

	public final int getSelectionLineMin() { return Math.min(curLine, selStartLine); }

	public final int getSelectionLineMax() { return Math.max(curLine, selStartLine); }

	private int validLineIndex(int lineIndex) {
		return Math.min(Math.max(lineIndex, 0), Math.max(diffDoc.getLineCount() - 1, 0));
	}

	public final DiffLine getLine(int lineIndex) {
		return (lineIndex < 0 || lineIndex >= getLineCount()) ? null : diffDoc.getLine(lineIndex);
	}

	public final boolean isInSearchMode() { return isInSearchMode; }

	public final String getSearchText() { return searchText; }

	public final String getCodeToString() { return (code == null) ? null : code.toString(); }

	public final ChangeStats getChangeStats() { return (diffDoc == null) ? null : diffDoc.getChangeStats(); }

	// -------------------------------------------------------------------------
	
	public static DiffNavigator create() {
		return new DiffNavigator();
	}
	
	/**
	 * 
	 * 
	 * @param code
	 * @param diffDoc
	 * @param setToTopLineIndex       -1 to keep current position, 0 to set to start
	 * @param setToCurLineIndex       -1 to keep current position, 0 to set to start
	 * @param setSelectionToStartLine -1 to keep current position, 0 to set to start
	 */
	public final void refreshCode(Code code, DiffDoc diffDoc, int setToTopLineIndex, int setToCurLineIndex, int setSelectionToStartLine) {
		if (code != null)
			this.code = code;
		if (diffDoc != null)
			this.diffDoc = diffDoc;

		if (setToCurLineIndex >= 0)
			curLine = validLineIndex(setToCurLineIndex);
		curSearchPos = 0;
		if (setSelectionToStartLine >= 0)
			selStartLine = validLineIndex(setSelectionToStartLine);
	}

	public final String reprocessSelection(Profile profile, int releaseRestriction, String sourceName) {
		int startLine = getSelectionLineMin();
		int lastLine = getSelectionLineMax();

		// if nothing but empty lines between methods / classes are selected (including empty lines that were deleted or added), 
		// then expand the selection to include the adjacent 'ENDMETHOD ... METHOD' or 'ENDCLASS ... CLASS'
		if (diffDoc.getLastNonEmptyLineInRange(startLine - 1, lastLine) < startLine) {
			Command testCommand = diffDoc.getCommandAt(startLine);
			if (testCommand.getParent() == null || testCommand.getParent().getParent() == null) { 
				startLine = diffDoc.getLastNonEmptyLineInRange(0, startLine);
				lastLine = diffDoc.getFirstNonEmptyLineInRange(lastLine, diffDoc.getLineCount());
			}
		}
		
		// if the last line is an empty line in both displays, it is not intuitive that this belongs to the next command
		lastLine = diffDoc.getLastNonEmptyOrChangedLineInRange(startLine, lastLine);
		
		// expand the line range if the start/end command was removed from the code (e.g. because of unused variables), 
		// because the parent/prev/next/prevSibling/nextSibling attributes of such commands were set to null
		Command startCommand = diffDoc.getCommandAt(startLine);
		while (startCommand.wasRemovedFromCode() && startLine > 0) {
			--startLine;
			startCommand = diffDoc.getCommandAt(startLine);
		}
		Command lastCommand = diffDoc.getCommandAt(lastLine);
		while(lastCommand.wasRemovedFromCode() && lastLine + 1 < diffDoc.getLineCount()) {
			++lastLine;
			lastCommand = diffDoc.getCommandAt(lastLine);
		}

		// widen the selection to include the complete original command in case that was split into multiple commands (e.g. with DeclarationChainRule)
		if (startCommand.originalCommand != null) {
			while (startCommand.getPrev() != null && startCommand.originalCommand == startCommand.getPrev().originalCommand)
				startCommand = startCommand.getPrev();
		}
		if (lastCommand.originalCommand != null) {
			while (lastCommand.getNext() != null && lastCommand.originalCommand == lastCommand.getNext().originalCommand)
				lastCommand = lastCommand.getNext();
		}
		
		int sourceLineOfSelStart = startCommand.getSourceLineNumStart();
		int sourceLineOfSelEnd = lastCommand.getSourceLineNumStart();
		
		// move the start command to possibly have a parent, but no grandparent (i.e., assuming CLASS ... METHOD ..., move to METHOD level)
		while (startCommand.getParent() != null && startCommand.getParent().getParent() != null) 
			startCommand = startCommand.getParent();
		// if the parent is a PUBLIC / PROTECTED / PRIVATE SECTION, move the start command there (otherwise, Command.isInClassDefinition() won't work in reprocessing)
		while (startCommand.getParent() != null && startCommand.getParent().isDeclarationSectionStart()) 
			startCommand = startCommand.getParent();
		// if the code snippet contains METHOD with no CLASS around it, move to METHOD level (= in this case, top level)
		while (startCommand.getParent() != null && startCommand.getParent().isMethodFunctionOrFormStart()) 
			startCommand = startCommand.getParent();
		// similarly, move the last command to possibly have a parent, but no grandparent (i.e., assuming CLASS ... METHOD ..., move to ENDMETHOD level)
		while (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().getParent() != null)
			lastCommand = lastCommand.getParent().getNextSibling();
		// if the code snippet contains METHOD with no CLASS around it, move to ENDMETHOD level (= in this case, top level)
		while (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().isMethodFunctionOrFormStart())
			lastCommand = lastCommand.getParent().getNextSibling();

		// ensure start command and last command are on the same level and have the same parent (if any):
		// if the start command is a class start, move the last command down to class end
		if (startCommand.getParent() == null && lastCommand != null && lastCommand.getParent() != null)
			lastCommand = lastCommand.getParent().getNextSibling();
		// if the last command is a class end, move the start command up to class start
		if (lastCommand != null && lastCommand.getParent() == null && startCommand.getParent() != null)
			startCommand = startCommand.getParent();
		// if start command and last command are inside different classes, move both to class level
		if (startCommand.getParent() != null && lastCommand != null && lastCommand.getParent() != null && startCommand.getParent() != lastCommand.getParent()) {
			startCommand = startCommand.getParent();
			lastCommand = lastCommand.getParent().getNextSibling();
		}
		
		// move from PRIVATE SECTION etc. to CLASS start / end
		while (startCommand.getClosesLevel() && startCommand.getPrevSibling() != null) 
			startCommand = startCommand.getPrevSibling();
		while (lastCommand != null && lastCommand.getOpensLevel() && lastCommand.getNextSibling() != null)
			lastCommand = lastCommand.getNextSibling();

		if (lastCommand == null) 
			lastCommand = code.lastCommand;
		startLine = diffDoc.findFirstLineOfCommand(startCommand, DisplaySide.RIGHT);
		if (startCommand.originalCommand != startCommand) {
			int originalStartLine = diffDoc.findFirstLineOfCommand(startCommand.originalCommand, DisplaySide.LEFT);
			if (originalStartLine >= 0 && originalStartLine < startLine)
				startLine = originalStartLine;
		}
		lastLine = diffDoc.findLastLineOfCommand(lastCommand, DisplaySide.RIGHT);
		if (lastCommand.originalCommand != lastCommand) {
			int originalLastLine = diffDoc.findLastLineOfCommand(lastCommand.originalCommand, DisplaySide.LEFT);
			if (originalLastLine >= 0 && originalLastLine > lastLine)
				lastLine = originalLastLine;
		}
		int sourceTextStart = startCommand.getSourceTextStart();
		int sourceTextEnd = lastCommand.getSourceTextEnd();

		// TODO: this is wrong if the first Token's lineBreaks were changed - we need the original sourceLineNumStart!
		int lineNumOffset = startCommand.getSourceLineNumStart() - startCommand.getSourceLineBreaksBefore();
		String codePartText = code.codeText.substring(sourceTextStart, sourceTextEnd);

		CleanupRange fullCleanupRange = code.getCleanupRange();
		// partCleanupRange works with the regular line numbers, i.e. we do NOT subtract the lineNumOffset 
		CleanupRange partCleanupRange = (fullCleanupRange != null) ? CleanupRange.create(fullCleanupRange.startLine, fullCleanupRange.endLine, false) : null;
		ParseParams parseParams = ParseParams.createForReprocessing(sourceName, codePartText, code.abapRelease, partCleanupRange, lineNumOffset, sourceTextStart, code);
		Job partJob = Job.createForSingleCodeDocument(parseParams, CleanupParams.createForProfile(profile, false, releaseRestriction));
		partJob.run();
		Task result = partJob.getResult();
		if (!result.getSuccess()) {
			String errorMsg = result.getErrorMessage();
			return (errorMsg != null) ? errorMsg : "";
		}

		// merge parts into full code and diffDoc
		try {
			code.replacePart(startCommand, lastCommand, result.getResultingCode());
		} catch (IntegrityBrokenException e) {
			e.addToLog();
			return e.getMessage();
		}
		DiffDoc diffDocPart = result.getResultingDiffDoc();
		diffDoc.replacePart(startLine, lastLine, diffDocPart);

		// move selection to cover the same commands in the changed code
		int newSelStartLine = diffDoc.findFirstLineOfSourceLine(sourceLineOfSelStart);
		int newCurLine = diffDoc.findLastLineOfSourceLine(sourceLineOfSelEnd);
		if (newSelStartLine >= 0 && newCurLine >= 0) {
			selStartLine = diffDoc.getFirstNonEmptyOrChangedLineInRange(newSelStartLine, newCurLine);
			curLine = newCurLine; 
		}

		return null;
	}

	public final void setSearchMode(boolean searchMode) {
		isInSearchMode = searchMode;
		// if search mode is started, reset the search text; otherwise keep it, so F3 can still be used
		if (searchMode)
			searchText = "";
	}

	public final void setCurLine(int newLine, boolean clearSelection) {
		curLine = validLineIndex(newLine);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void clearSearchPos() {
		curSearchPos = -1;
	}

	public final boolean moveToNextScreenWithChanges(int lastVisibleLine) {
		int newLine = diffDoc.getLineOfNextChangedCommand(validLineIndex(lastVisibleLine), true, highlight);
		if (newLine < 0)
			return false;
		curLine = newLine;
		selStartLine = curLine;
		return true;
	}

	public final boolean moveToPrevScreenWithChanges(int firstVisibleLine) {
		int newLine = diffDoc.getLineOfNextChangedCommand(validLineIndex(firstVisibleLine), false, highlight);
		if (newLine < 0)
			return false;
		curLine = newLine;
		selStartLine = curLine;
		return true;
	}

	public final void moveToFirstLine(boolean clearSelection) {
		curLine = 0;
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void moveToLastLine(boolean clearSelection) {
		curLine = diffDoc.getLineCount() - 1;
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void selectAll() {
		selStartLine = 0;
		curLine = diffDoc.getLineCount() - 1;
	}

	public final boolean moveToNextChange() {
		int newLine = diffDoc.getLineOfNextChangedCommand(curLine, true, highlight);
		if (newLine < 0)
			return false;

		curLine = newLine;
		selStartLine = curLine;
		return true;
	}

	public final boolean moveToPrevChange() {
		int newLine = diffDoc.getLineOfNextChangedCommand(curLine, false, highlight);
		if (newLine < 0)
			return false;

		curLine = newLine;
		selStartLine = curLine;
		return true;
	}

	public final void moveToNextPage(int visibleLineCount, boolean clearSelection) {
		curLine = validLineIndex(curLine + visibleLineCount);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void moveToPrevPage(int visibleLineCount, boolean clearSelection) {
		curLine = validLineIndex(curLine - visibleLineCount);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void moveToNextLine(boolean clearSelection) {
		curLine = validLineIndex(curLine + 1);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void moveToPrevLine(boolean clearSelection) {
		curLine = validLineIndex(curLine - 1);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final void moveToLine(int lineNum, boolean clearSelection) {
		curLine = validLineIndex(lineNum);
		if (clearSelection)
			selStartLine = curLine;
	}

	public final boolean moveToLineInDoc(int lineInDoc, DisplaySide displaySide) {
		int diffLineIndex = diffDoc.findLineIndexOfLineNum(lineInDoc, displaySide); // 0-based
		if (diffLineIndex < 0)
			return false;
		curLine = validLineIndex(diffLineIndex);
		selStartLine = curLine;
		return true;
	}

	public final String getSelectedText(DisplaySide displaySide, String lineSeparator) {
		return diffDoc.getTextOfLineRange(getSelectionLineMin(), getSelectionLineMax(), displaySide, lineSeparator);
	}

	public final void removeLastCharFromSearchText() {
		searchText = StringUtil.isNullOrEmpty(searchText) ? "" : searchText.substring(0, searchText.length() - 1);
	}

	public final void addCharToSearchText(char c) {
		searchText += c;
	}

	public final boolean search(boolean findNext, boolean mayExtendCurrentSelection, boolean searchLeft, boolean searchRight, boolean searchChangedLinesOnly, boolean matchCase,
			boolean wholeWord) {
		boolean ignoreCase = !matchCase;

		if (StringUtil.isNullOrEmpty(searchText))
			return false;

		DisplaySide testSide = curSearchSide;
		int testLine = curLine;
		int start = curSearchPos + (mayExtendCurrentSelection ? 0 : (findNext ? 1 : -1));

		if (findNext) {
			while (testLine < diffDoc.getLineCount()) {
				DiffLine diffLine = diffDoc.getLine(testLine);
				DisplayLine line = diffLine.getDisplayLine(testSide);
				String text = (line == null) ? null : line.getText();
				boolean sideMatch = (testSide == DisplaySide.LEFT) ? searchLeft : searchRight;
				boolean lineStatusMatch = searchChangedLinesOnly ? diffLine.matchesChangeTypesFilter(highlight) : true;
				if (line != null && lineStatusMatch && sideMatch && start < text.length()) {
					int foundPos = AbapCult.indexOf(text, searchText, (start >= 0) ? start : 0, ignoreCase);
					if (foundPos >= 0 && (!wholeWord || isStartOfWholeWord(text, foundPos))) {
						curSearchSide = testSide;
						curLine = testLine;
						curSearchPos = foundPos;
						selStartLine = curLine;
						return true;
					}
				}
				if (testSide == DisplaySide.LEFT)
					testSide = DisplaySide.RIGHT;
				else {
					testSide = DisplaySide.LEFT;
					++testLine;
				}
				start = 0;
			}
		} else {
			while (testLine >= 0) {
				DiffLine diffLine = diffDoc.getLine(testLine);
				DisplayLine line = diffLine.getDisplayLine(testSide);
				String text = (line == null) ? null : line.getText();
				boolean sideMatch = (testSide == DisplaySide.LEFT) ? searchLeft : searchRight;
				boolean lineStatusMatch = searchChangedLinesOnly ? diffLine.matchesChangeTypesFilter(highlight) : true;
				if (line != null && lineStatusMatch && sideMatch) {
					int foundPos = AbapCult.lastIndexOf(text, searchText, (start >= 0 ? start : text.length()), ignoreCase);
					if (foundPos >= 0 && (!wholeWord || isStartOfWholeWord(text, foundPos))) {
						curSearchSide = testSide;
						curLine = testLine;
						curSearchPos = foundPos;
						selStartLine = curLine;
						return true;
					}
				}
				if (testSide == DisplaySide.RIGHT)
					testSide = DisplaySide.LEFT;
				else {
					testSide = DisplaySide.RIGHT;
					--testLine;
				}
				start = -1;
			}
		}
		return false;
	}

	public final boolean isStartOfWholeWord(String text, int pos) {
		if (pos < 0)
			return false;
		if (pos == 0)
			return true;
		else
			return !Character.isLetterOrDigit(text.charAt(pos - 1));
	}

	public final RuleStats[] getRuleStats(Profile profile) {
		return diffDoc.getRuleStatsOfLineRange(profile, getSelectionLineMin(), getSelectionLineMax());
	}

	public final void setBlockRuleInSelection(RuleID ruleID, boolean blocked) {
		int startLine = getSelectionLineMin();
		int lastLine = getSelectionLineMax();
		
		// if the last line is an empty line in both displays, it is not intuitive that this belongs to the next command
		lastLine = diffDoc.getLastNonEmptyOrChangedLineInRange(startLine, lastLine);

		Command lastCommand = null;
		for (int i = startLine; i <= lastLine; ++i) {
			DiffLine line = diffDoc.getLine(i);
			DisplayLine displayLine = (line.rightLine != null) ? line.rightLine : line.leftLine;
			Command command = displayLine.parentCommand;
			if (command != null && command != lastCommand) {
				command.getChangeControl().setBlockedRule(ruleID, blocked);
				lastCommand = command;
			}
		}
	}

	public final void setHighlight(ChangeTypes highlight) {
		this.highlight = highlight;
	}

	public final boolean showAddedAndDeletedLines() { 
		return highlight.textOrLineChanges; // or .anyChanges(), but that is lees intuitive 
	}

	public final boolean showAnyChanges() { 
		return highlight.anyChanges(); // or .anyChanges(), but that is lees intuitive 
	}

	public final boolean isLineHighlighted(DiffLine diffLine) {
		return diffLine.matchesChangeTypesFilter(highlight);
	}

	public final boolean isLineBitHighlighted(HighlightBit bit) {
		switch (bit.type) {
			case TEXT_CHANGE:
				return highlight.textOrLineChanges;
			case INDENT_CHANGE:
				return highlight.indentChanges;
			case INNER_SPACE_CHANGE:
				return highlight.innerSpaceChanges;
			case CASE_CHANGE:
				return highlight.caseChanges;
			default:
				throw new IndexOutOfBoundsException("unexpected HighlightBitType!");
		}
	}

	public final boolean isSearchMatchHighlighted(int lineIndex, int pos, DisplaySide displaySide) {
		return (lineIndex == curLine && pos == curSearchPos && curSearchSide == displaySide);
	}
	
	public final Command getCommandAt(int lineIndex) {
		return diffDoc.getCommandAt(lineIndex);
	}
}
