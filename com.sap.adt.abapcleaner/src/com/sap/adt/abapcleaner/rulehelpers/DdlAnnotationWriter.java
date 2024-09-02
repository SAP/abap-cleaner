package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public abstract class DdlAnnotationWriter {
	abstract protected void addToken(int lineBreaks, int spacesLeft, String text, Language language) throws UnexpectedSyntaxException;
	abstract protected void addToLastTokenText(String text);
	protected void endAnnotation(Command originalCommand) { }
	protected void finishCommandBuild() throws ParseException { }
	abstract public String toText();

	// -------------------------------------------------------------------------

	private final DdlAnnotationLayout layout;
	protected Token originalValueToken;
	protected Command originalCommand;
	
	private int[] startPosOfLevel;
	private int writtenIndexInLine;

	private boolean isFirstCommand;
	private boolean isNewCommand; 
	private boolean isBetweenMultiLineCommands; 
	private boolean lastCommandWasMultiLine;
	private boolean isInCommand;

	private int nextLineBreaks;
	private int nextSpacesLeft; 
	private boolean lastWasElemName;

	// -------------------------------------------------------------------------
	
	public DdlAnnotationWriter(DdlAnnotationLayout layout, int maxLevel) {
		this.layout = layout;

		startPosOfLevel = new int[maxLevel  + 1];
		nextLineBreaks = layout.initialLineBreaks;
		nextSpacesLeft = layout.basicIndent; 
		isFirstCommand = true;
	}

	public void startCommand(boolean commandWillBeMultiLine) {
		isNewCommand = true;
		isBetweenMultiLineCommands = (lastCommandWasMultiLine || commandWillBeMultiLine);
		lastCommandWasMultiLine = commandWillBeMultiLine;
	}

	public void startAnnotation(Token originalValueToken, int originalLineBreaks, boolean startsNewMainElem) {
		this.originalValueToken = originalValueToken;
		this.originalCommand = originalValueToken.getParentCommand();
		
		if (isFirstCommand) {
			// do not put an extra empty line above the very first annotation in the scope 
			isFirstCommand = false;
			
		} else if (isNewCommand) {
			if (layout.emptyLines == DdlAnnotationEmptyLines.ALWAYS) {
				++nextLineBreaks;
			
			} else if (layout.emptyLines == DdlAnnotationEmptyLines.NEVER) {
				// keep nextLineBreaks unchanged

			} else if (layout.emptyLines == DdlAnnotationEmptyLines.KEEP_AS_IS) {
				if (originalLineBreaks > 1) { // originalValueToken.getFirstTokenInLine().lineBreaks > 1
					++nextLineBreaks;
				}

			} else { // .FOR_MULTI_LINE_OR_NEW_FIRST_ELEM, .FOR_NEW_FIRST_ELEM 
				if (startsNewMainElem && layout.emptyLines.separateByFirstElement || isBetweenMultiLineCommands && layout.emptyLines.separateMultiLiners) {
					++nextLineBreaks;
				}
			} 
		}
		isNewCommand = false;
		isBetweenMultiLineCommands = false;
	}

	private int addToken(int addSpacesLeft, String text, int spacesRight) throws UnexpectedSyntaxException  {
		return addToken(addSpacesLeft, text, spacesRight, false);
	}
	
	private int addToken(int addSpacesLeft, String text, int spacesRight, boolean isElemName) throws UnexpectedSyntaxException {
		if (isElemName && lastWasElemName && nextSpacesLeft == 0 && addSpacesLeft == 0) {
			addToLastTokenText(text);
			writtenIndexInLine += text.length();
		} else {
			addToken(nextLineBreaks, nextSpacesLeft + addSpacesLeft, text, originalCommand.getLanguage());
			isInCommand = true;
			if (nextLineBreaks > 0) 
				writtenIndexInLine = 0;
			writtenIndexInLine += nextSpacesLeft + text.length();
		}
		
		nextLineBreaks = 0;
		nextSpacesLeft = spacesRight;
		lastWasElemName = isElemName || text.equals(DDL.ANNOTATION_SIGN_STRING);
		return writtenIndexInLine;
	}
	
	public int openBracket() throws UnexpectedSyntaxException {
		return addToken(0, DDL.BRACKET_OPEN_STRING, layout.spacesInsideBrackets);
	}
	
	public int openBrace() throws UnexpectedSyntaxException {
		return addToken(0, DDL.BRACE_OPEN_STRING, layout.spacesInsideBraces);
	}

	public int addAnnotationSign() throws UnexpectedSyntaxException {
		return addToken(0, DDL.ANNOTATION_SIGN_STRING, 0);
	}
	
	public void startLevel(int level) {
		// remember the current indentation for this level
		startPosOfLevel[level] = isInCommand ? (nextLineBreaks > 0 ? nextSpacesLeft : writtenIndexInLine + nextSpacesLeft) : 0;
	}
	
	public int addAnnoElemName(String elemName, int columnWidth) throws UnexpectedSyntaxException {
		int spacesRight = 0;
		if (layout.alignValues && columnWidth > elemName.length()) 
			spacesRight = columnWidth - elemName.length();
		
		return addToken(0, elemName, spacesRight, true);
	}
	
	public int addDot() throws UnexpectedSyntaxException {
		return addToken(layout.spacesBeforeDot, DDL.DOT_SIGN_STRING, layout.spacesAfterDot, true);
	}
	
	public int addColon() throws UnexpectedSyntaxException {
		// if nextSpacesLeft were set to align columns, attach the colon and move the extra spaces behind the colon
		int spacesRight = nextSpacesLeft + layout.spacesAfterColon;
		nextSpacesLeft = 0;
		
		return addToken(layout.spacesBeforeColon, DDL.COLON_SIGN_STRING, spacesRight);
	}
	
	public int addValue(String value, int valueColumnWidth) throws UnexpectedSyntaxException {
		int spacesRight = 0;
		if (layout.alignTablesInArrays && valueColumnWidth > value.length()) 
			spacesRight = valueColumnWidth - value.length();

		return addToken(0, value, spacesRight);
	}
	
	public int addComma(int newLevel, boolean continueOnLine) throws UnexpectedSyntaxException {
		// if nextSpacesLeft were set to align value columns, attach the comma and move the extra spaces behind the comma
		int spacesRight = continueOnLine ? nextSpacesLeft + layout.spacesAfterComma : 0;
		nextSpacesLeft = 0;
		
		int result = addToken(layout.spacesBeforeComma, DDL.COMMA_SIGN_STRING, spacesRight);
		if (!continueOnLine)
			addLineFeed(startPosOfLevel[newLevel]);
		return result;
	}
	
	public int closeBracket() throws UnexpectedSyntaxException {
		return addToken(layout.spacesInsideBrackets, DDL.BRACKET_CLOSE_STRING, 0);
	}
	
	public int closeBrace() throws UnexpectedSyntaxException {
		return addToken(layout.spacesInsideBraces, DDL.BRACE_CLOSE_STRING, 0);
	}
	
	public void finishCommand() throws UnexpectedSyntaxException, ParseException {
		finishCommandBuild();
		isInCommand = false;
		lastWasElemName = false;
		originalCommand = null;
		addLineFeed(layout.basicIndent);
	}

	private void addLineFeed(int newIndent) {
		nextLineBreaks = 1;
		nextSpacesLeft = newIndent;
		lastWasElemName = false;
	}
	
	@Override
	public String toString() { // for debugging
		return toText();
	}
}
