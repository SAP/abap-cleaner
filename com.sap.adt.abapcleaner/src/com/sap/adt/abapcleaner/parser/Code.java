package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.programbase.*;

import java.util.*;

/**
 * <p>Represents an ABAP code document, which is returned as the result of {@link #parse(IProgress, ParseParams)} 
 * and may consist of anything between a single code line up to tens of thousands of code lines with several classes etc.</p> 
 * 
 * <p>Such a Code consists of a sequence of {@link Command}s, 
 * starting from {@link #firstCommand} and ending with {@link #lastCommand}.</p>
*/
public class Code {
	public final String sourceName;
	public final String codeText;

	/** the ABAP release against which this code must compile (e.g. "757" for release 7.57; null if unknown), 
	 * see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews.htm">ABAP - Release News</a> */
	public final String abapRelease;

	/** the line range to clean; null if the whole code shall be cleaned */
	public CleanupRange cleanupRange;
	
	public CleanupRange getCleanupRange() { return cleanupRange; }
	public boolean hasCleanupRange() { return (cleanupRange != null); }
	
	private int indentOfFirstCommand = -1;

	final int getIndentOfFirstCommand() { return indentOfFirstCommand; }

	public Command firstCommand;
	public Command lastCommand;

	// (re)calculated in finishBuild():
	public int commandCount;
	private int classStartCount;
	// private int methodStartCount;
	private int methodFunctionOrFormStartCount;
	private int methodFunctionFormOrEventBlockStartCount;
	private boolean hasIntroductoryStatement;

	/** returns true if this Code starts with CLASS-POOL, FUNCTION-POOL, INTERFACE-POOL, PROGRAM, REPORT, or TYPE-POOL. */
	final boolean hasIntroductoryStatement() { return hasIntroductoryStatement; }

	private HashMap<Integer, ChangeControl> changeControlOfSourceLineStart;

	/** returns true if this Code contains classes (i.e. CLASS statements) */
	final boolean hasClassStart() { return (classStartCount > 0); }

	public final boolean hasMethodFunctionOrFormStart() { return (methodFunctionOrFormStartCount > 0); }

	public final boolean hasMethodFunctionFormOrEventBlockStart() { return (methodFunctionFormOrEventBlockStartCount > 0); }

	// ----------------------------------------------------------------------

	public static Code parse(IProgress progress, ParseParams parseParams) throws ParseException {
		Code code = new Code(parseParams.sourceName, parseParams.codeText, parseParams.abapRelease, parseParams.cleanupRange, parseParams.surroundingCode);
		Parser parser = Parser.create(parseParams.codeText);
		parser.parse(progress, code, parseParams.lineNumOffset, parseParams.surroundingTextOffset);
		
		code.expandCleanupRange(parseParams.cleanupRangeExpandMode); 

		return code;
	}

	static Code createEmptyForTests() {
		Code code = new Code("test", "", ABAP.NEWEST_RELEASE, null, null);
		return code;
	}

	private Code(String sourceName, String codeText, String abapRelease, CleanupRange cleanupRange, Code surroundingCode) {
		this.sourceName = sourceName;
		this.codeText = codeText;
		this.abapRelease = abapRelease;
		this.cleanupRange = cleanupRange;
		this.changeControlOfSourceLineStart = (surroundingCode != null && surroundingCode.changeControlOfSourceLineStart != null) ? surroundingCode.changeControlOfSourceLineStart : new HashMap<Integer, ChangeControl>();
	}

	final void appendCommand(Command newCommand) {
		if (firstCommand == null)
			firstCommand = newCommand;
		lastCommand = newCommand;
		++commandCount;
	}

	final void finishBuild() {
		int indentOfFirstComment = -1;

		Command command = firstCommand;
		commandCount = 0;
		while (command != null) {
			if (indentOfFirstComment < 0 && command.isQuotMarkCommentLine())
				indentOfFirstComment = command.firstToken.spacesLeft;
			if (indentOfFirstCommand < 0 && !command.isCommentLine() && !command.isEmpty()) {
				indentOfFirstCommand = command.firstToken.spacesLeft;
				// ensure that a method is indented at least 2 chars (esp. when reprocessing a selection)
				if (command.isMethodStart()) 
					indentOfFirstCommand  = Math.max(indentOfFirstCommand,  2);
			}

			++commandCount;
			if (command.isClassStart())
				++classStartCount;
			// else if (command.IsMethodStart)
			//    ++methodStartCount;

			// do NOT attach with else if:
			if (command.isMethodFunctionOrFormStart())
				++methodFunctionOrFormStartCount;

			if (command.isMethodFunctionFormOrEventBlockStart())
				++methodFunctionFormOrEventBlockStartCount;

			if (command.isIntroductoryStatement())
				hasIntroductoryStatement = true;

			command = command.getNext();
		}
		if (indentOfFirstCommand < 0)
			indentOfFirstCommand = Math.max(indentOfFirstComment, 0);
	}

	@Override
	public String toString() {
		return toString(ABAP.LINE_SEPARATOR);
	}
	
	public String toString(String lineSeparator) {
		StringBuilder result = new StringBuilder();
		Command command = firstCommand;
		while (command != null) {
			result.append(command.toString(lineSeparator));
			command = command.getNext();
		}
		return result.toString();
	}

	public final ArrayList<DisplayLine> toDisplayLines() {
		return toDisplayLines(0);
	}
	public final ArrayList<DisplayLine> toDisplayLines(int indexOffset) {
		ArrayList<DisplayLine> lines = new ArrayList<DisplayLine>();

		int index = indexOffset;
		StringBuilder line = new StringBuilder();
		ArrayList<TextBit> textBits = new ArrayList<TextBit>();

		Command command = firstCommand;
		while (command != null) {

			Token token = command.firstToken;
			while (token != null) {
				if (token.lineBreaks > 0) {
					// add last line (compiled from previous Tokens in this Command)
					if (token != command.firstToken) {
						lines.add(DisplayLine.create(command, line.toString(), index++, condenseTextBits(textBits)));
						line.setLength(0);
						textBits.clear();
					}

					// add further empty lines, if applicable
					for (int i = 1; i < token.lineBreaks; ++i)
						lines.add(DisplayLine.create(command, "", index++));
				}

				// add spaces; no TextBits are required for this
				if (token.spacesLeft > 0)
					line.append(StringUtil.repeatChar(' ', token.spacesLeft));

				// add the Token's text, and the TextBits for coloring this text
				int startIndex = line.length();
				line.append(token.text);
				textBits.addAll(Arrays.asList(token.toTextBits(startIndex)));

				token = token.getNext();
			}
			// add the last line
			if (line.length() > 0 && (command.getNext() == null || command.getNext().getFirstTokenLineBreaks() > 0)) {
				lines.add(DisplayLine.create(command, line.toString(), index++, condenseTextBits(textBits)));
				line.setLength(0);
				textBits.clear();
			}

			command = command.getNext();
		}

		return lines;
	}

	private ArrayList<TextBit> condenseTextBits(ArrayList<TextBit> textBits) {
		if (textBits.isEmpty())
			return null;

		// condense attached TextBits
		ArrayList<TextBit> condensedResult = new ArrayList<TextBit>();
		TextBit curBit = null;
		for (TextBit textBit : textBits) {
			if (curBit == null) {
				// create first TextBit, starting from position 0
				curBit = TextBit.create(0, textBit.getEnd(), textBit.type);
				condensedResult.add(curBit);
			} else if (curBit.type == textBit.type) {
				// extend current TextBit, since it has the same type
				curBit.length = textBit.getEnd() - curBit.start;
			} else {
				// create new TextBit
				curBit = TextBit.createFromModel(textBit);
				condensedResult.add(curBit);
			}
		}
		return condensedResult;
	}

	public final int getTotalTokenCount() {
		int result = 0;
		Command command = firstCommand;
		while (command != null) {
			result += command.tokenCount;
			command = command.getNext();
		}
		return result;
	}

	public final String compareWithSource(String sourceCode, int maxReportLineCount) {
		if (sourceCode == null)
			throw new NullPointerException("sourceCode");

		String recompiledCodeText = this.toString();
		if (sourceCode.equals(recompiledCodeText))
			return null;

		// in some places, a Tab is found; both SAP GUI and ADT display this as a single space
		sourceCode = Tokenizer.removeTabs(sourceCode);
		if (sourceCode.equals(recompiledCodeText))
			return null;

		StringBuilder result = new StringBuilder();
		String sourceLineSep = StringUtil.inferLineSeparator(sourceCode);
		String[] sourceLines = StringUtil.split(sourceCode, sourceLineSep, false);
		String[] recompiledLines = StringUtil.split(recompiledCodeText, ABAP.LINE_SEPARATOR, false);
		// ignore empty lines at the end
		int sourceLineCount = sourceLines.length;
		while (sourceLineCount > 0 && StringUtil.isNullOrEmpty(sourceLines[sourceLineCount - 1]))
			--sourceLineCount;
		int recompiledLineCount = recompiledLines.length;
		while (recompiledLineCount > 0 && StringUtil.isNullOrEmpty(recompiledLines[recompiledLineCount - 1]))
			--recompiledLineCount;

		int minLineCount = Math.min(sourceLineCount, recompiledLineCount);
		int findingCount = 0;
		for (int i = 0; i < minLineCount; ++i) {
			if (!sourceLines[i].equals(recompiledLines[i])) {
				result.append((result.length() == 0) ? "Parse result differs in line " : ", ");
				result.append(AbapCult.ToString(i + 1));
				++findingCount;
				if (findingCount >= maxReportLineCount && maxReportLineCount >= 0)
					break;
			}
		}
		if (sourceLineCount != recompiledLineCount) {
			if (result.length() > 0)
				result.append("; ");
			result.append("line count mismatch: " + AbapCult.ToString(sourceLineCount) + " versus " + AbapCult.ToString(recompiledLineCount));
		}
		return (result.length() == 0) ? null : result.toString();
	}

	public final void addRuleUses(Rule rule, Command[] commands) {
		if (commands != null) {
			for (Command command : commands)
				addRuleUse(rule, command);
		}
	}

	public final void addRuleUses(Rule rule, ArrayList<Command> commands) {
		if (commands != null) {
			for (Command command : commands)
				addRuleUse(rule, command);
		}
	}

	public final void addRuleUses(Rule rule, Section section) {
		if (section != null) {
			Command command = section.firstCommand;
			while (command != null) {
				addRuleUse(rule, command);
				if (command == section.lastCommand)
					break;
				command = command.getNext();
			} 
		}
	}

	public final void addRuleUse(Rule rule, Command command) {
		addRuleUse(rule, command, null);
	}
	public final void addRuleUse(Rule rule, Command command, Token token) {
		if (command == null)
			throw new NullPointerException("command");
		if (rule == null)
			throw new NullPointerException("rule");

		RuleID ruleID = rule.getID();
		if (!command.getChangeControl().wasRuleUsed(ruleID))
			command.getChangeControl().setUsedRule(ruleID);
	}

	public final void testReferentialIntegrity(boolean deep) throws IntegrityBrokenException {
		testReferentialIntegrity(deep, null);
	}
	public final void testReferentialIntegrity(boolean deep, IProgress progress) throws IntegrityBrokenException {
		check(firstCommand == null || lastCommand != null);
		check((firstCommand == null) == (commandCount == 0));
		check(firstCommand == null || firstCommand.getPrev() == null);
		check(firstCommand == null || firstCommand.getPrevSibling() == null);
		check(lastCommand == null || lastCommand.getNext() == null);
		check(lastCommand == null || lastCommand.getNextSibling() == null);

		if (!deep)
			return;
		
		if (progress != null)
			progress.report(TaskType.INTEGRITY_TEST, 0.0);

		Command command = firstCommand;
		int count = 0;
		while (command != null) {
			check(command.getParentCode() == this);
			command.testReferentialIntegrity(true, true);

			if (progress != null && progress.isCancellationPending())
				return;
			++count;
			if (progress != null)
				progress.report(TaskType.INTEGRITY_TEST, count / (double) commandCount);

			command = command.getNext();
		}
	}

	private void check(boolean value) throws IntegrityBrokenException {
		if (!value) {
			throw new IntegrityBrokenException(this, "Failed referential integrity test on Code level!");
		}
	}

	public final void checkSyntax(boolean afterCleanup) throws IntegrityBrokenException {
		final int maxErrorLinesToReport = 10;
		
		TokenTypeRefinerRnd rndParser = Program.getRndParser();
		if (rndParser == null)
			return;

		StringBuilder sbLine = new StringBuilder();
		int errorCommandCount = 0;
		String firstErrorDetails = null;
		
		Command command = firstCommand;
		while (command != null) {
			if (command.isInCleanupRange()) {
				RndParseResult rndParseResult;
				try {
					rndParseResult = rndParser.getRndParseResult(command);
				} catch (ParseException e) {
					throw new IntegrityBrokenException(command, e.getMessage());
				}
				if (rndParseResult != null) {
					int allowedErrorCount = afterCleanup ? command.getErrorTokenCountBeforeCleanup() : 0;
					if (rndParseResult.errorTokenCount > allowedErrorCount) {
						++errorCommandCount;
						if (errorCommandCount > 1)
							sbLine.append(", ");
						sbLine.append(Cult.format(command.getSourceLineNumStart()));
						if (firstErrorDetails == null)
							firstErrorDetails = rndParseResult.getFirstErrorDetails();
						if (errorCommandCount >= maxErrorLinesToReport) {
							break;
						}
					}
				}
			}
			command = command.getNext();
		}
		if (errorCommandCount > 0) {
			String message = (errorCommandCount == 1) ? "line " : "lines ";
			message += sbLine.toString() + ": syntax error" + (afterCleanup ? " after cleanup" : "");
			if (!StringUtil.isNullOrEmpty(firstErrorDetails)) {
				if (errorCommandCount > 1)
					message += "; first error";
				message += " " + firstErrorDetails; // "at token '...' (line # + #, col #)"
			}
			throw new IntegrityBrokenException(this, message);
		}
	}
	
	final ChangeControl getChangeControl(int sourceTextStart, int sourceTextEnd) {
		if (changeControlOfSourceLineStart.containsKey(sourceTextStart)) 
			return changeControlOfSourceLineStart.get(sourceTextStart);
		ChangeControl newChangeControl = new ChangeControl(sourceTextStart, sourceTextEnd);
		changeControlOfSourceLineStart.put(sourceTextStart, newChangeControl);
		return newChangeControl;
	}

	public final void clearUsedRules() {
		Command command = firstCommand;
		while (command != null) {
			command.getChangeControl().clearUsedRules();
			command = command.getNext();
		}
	}

	public final void replacePart(Command startCommand, Command lastCommand, Code codePart) throws IntegrityBrokenException {
		Command command = codePart.firstCommand;
		while (command != null) {
			command.setParentCode(this);
			command = command.getNext();
		}

		Command newCommand = codePart.firstCommand;
		while (newCommand != null) {
			newCommand.setParent(startCommand.getParent());
			newCommand = newCommand.getNextSibling();
		}
		if (startCommand.getParent() != null && startCommand.getParent().getFirstChild() == startCommand)
			startCommand.getParent().setFirstChild(codePart.firstCommand);
		if (lastCommand.getParent() != null && lastCommand.getParent().getLastChild() == lastCommand)
			lastCommand.getParent().setLastChild(codePart.lastCommand);

		codePart.firstCommand.setPrev(startCommand.getPrev());
		codePart.firstCommand.setPrevSibling(startCommand.getPrevSibling());
		if (codePart.firstCommand.getPrev() != null)
			codePart.firstCommand.getPrev().setNext(codePart.firstCommand);
		if (codePart.firstCommand.getPrevSibling() != null)
			codePart.firstCommand.getPrevSibling().setNextSibling(codePart.firstCommand);

		codePart.lastCommand.setNext(lastCommand.getNext());
		codePart.lastCommand.setNextSibling(lastCommand.getNextSibling());
		if (codePart.lastCommand.getNext() != null)
			codePart.lastCommand.getNext().setPrev(codePart.lastCommand);
		if (codePart.lastCommand.getNextSibling() != null)
			codePart.lastCommand.getNextSibling().setPrevSibling(codePart.lastCommand);

		if (startCommand == firstCommand)
			firstCommand = codePart.firstCommand;
		if (lastCommand == this.lastCommand)
			this.lastCommand = codePart.lastCommand;

		finishBuild();

		testReferentialIntegrity(true, null);
	}

	public final void setBlockedRule(RuleID ruleID, boolean blocked) {
		Command command = firstCommand;
		while (command != null) {
			command.getChangeControl().setBlockedRule(ruleID, true);
			command = command.getNext();
		}
	}

	final Command findFirstCommandInCleanupRange() {
		Command command = firstCommand;
		while (command != null) {
			if (command.isInCleanupRange())
				return command;
			command = command.getNext();
		}
		return null;
	}

	final Command findLastCommandInCleanupRange() {
		Command command = lastCommand;
		while (command != null) {
			if (command.isInCleanupRange())
				return command;
			command = command.getPrev();
		}
		return null;
	}

	public CleanupResult toCleanupResult(String lineSeparator) {
		if (!hasCleanupRange())
			return CleanupResult.createWithoutRange(this.toString(lineSeparator));
			
		int startLine = -1;
		int endLine = -1;
		int startPos = -1;
		int endPos = 0;
		
		int pos = 0;
		int line = 0;
		Command command = firstCommand;

		// determine the char and line range of the selection that was cleaned up 
		while (command != null) {
			int commandLength = command.toString(lineSeparator).length();

			// since Rules like IfBlockAtLineEndRule / IfBlockAtMethodEndRule may change the sequence of Commands, 
			// it is not guaranteed that the Commands that are in the cleanup range are still consecutive; 
			// we therefore simply look out for the first and the last Command in that range, ignoring gaps 
			if (command.isInCleanupRange()) {
				if (startLine < 0) {
					// do not include the initial line breaks in the selection 
					int firstTokenLineBreaks = command.getFirstTokenLineBreaks();
					startLine = line + firstTokenLineBreaks;
					startPos = pos + lineSeparator.length() * firstTokenLineBreaks;
				}
				endLine = line;
				endPos = pos + commandLength;
			}

			line += command.getLineBreakSum();
			pos += commandLength;
			command = command.getNext();
		}

		return CleanupResult.createForRange(toString(lineSeparator), startLine, endLine, startPos, endPos - startPos);
	}
	
	public void expandCleanupRange(CleanupRangeExpandMode mode) {
		if (cleanupRange == null || !cleanupRange.expandRange) 
			return;

		if (mode == CleanupRangeExpandMode.FULL_DOCUMENT) {
			cleanupRange = null;
			return;
		}

		Command startCommand = findFirstCommandInCleanupRange();
		if (startCommand == null) {
			// no chance to expand the cleanup range
			cleanupRange = CleanupRange.create(cleanupRange.startLine, cleanupRange.endLine, false);
			return;
		}			

		Command lastCommand = findLastCommandInCleanupRange(); 

		if (mode == CleanupRangeExpandMode.FULL_METHOD || mode == CleanupRangeExpandMode.FULL_CLASS) { 
			// if the selection begins with ENDMETHOD etc., move the start to the corresponding METHOD; analogously for (END)INTERFACE
			if (startCommand.isMethodFunctionOrFormEnd() || startCommand.isInterfaceEnd())
				startCommand = startCommand.getPrevSibling();
			// if the selection ends with METHOD etc., move the end to the corresponding ENDMETHOD; analogously for (END)INTERFACE
			if (lastCommand != null && (lastCommand.isMethodFunctionOrFormStart() || lastCommand.isInterfaceStart()))
				lastCommand = lastCommand.getNextSibling();
			
			// expand to full methods 
			// move the start and last command to possibly have a parent, but no grandparent (i.e., assuming CLASS ... METHOD ..., move to METHOD level)
			while (startCommand.getParent() != null && startCommand.getParent().getParent() != null) 
				startCommand = startCommand.getParent();
			while (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().getParent() != null)
				lastCommand = lastCommand.getParent().getNextSibling();

			// if the code snippet contains METHOD with no CLASS around it, move to METHOD level (= in this case, top level)
			if (startCommand.getParent() != null && (startCommand.getParent().isMethodFunctionOrFormStart())) 
				startCommand = startCommand.getParent();
			if (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().isMethodFunctionOrFormStart())
				lastCommand = lastCommand.getParent().getNextSibling();

			// if the code snippet is part of a PRIVATE|PROTECTED|PUBLIC declaration SECTION, move to ...SECTION level
			// (note that these ...SECTIONs are siblings of CLASS ... DEFINITION, not its children);
			// similarly, move to the start of sections in reports: AT SELECTION-SCREEN, START-OF-SELECTION etc.;
			if (startCommand.getParent() != null && startCommand.getParent().isReportOrDeclarationSectionStart()) 
				startCommand = startCommand.getParent();
			// likewise, move the last Command to the end of such a section
			if (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().isReportOrDeclarationSectionStart()) { 
				lastCommand = lastCommand.getParent().getNextSibling(); // may be null
				// do NOT include the next SECTION statement itself, unless we expand to full class level below
				if (lastCommand != null && mode != CleanupRangeExpandMode.FULL_CLASS)
					lastCommand = lastCommand.getPrev(); 
			} else if (lastCommand != null && lastCommand.isReportOrDeclarationSectionStart()) { // in case the lastCommand is the SECTION declaration itself 
				lastCommand = lastCommand.getNextSibling(); // may be null
				// do NOT include the next SECTION statement itself, unless we expand to full class level below
				if (lastCommand != null && mode != CleanupRangeExpandMode.FULL_CLASS)
					lastCommand = lastCommand.getPrev(); 
			}

			// note that startCommand may now belong to a different method (even in a different class) than lastCommand! 
			
			// if the code snippet is part of an INTERFACE declaration, move to INTERFACE level
			if (startCommand.getParent() != null && startCommand.getParent().isInterfaceStart()) 
				startCommand = startCommand.getParent();
			if (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().isInterfaceStart()) 
				lastCommand = lastCommand.getParent().getNextSibling(); 
			
			if (mode == CleanupRangeExpandMode.FULL_CLASS) {
				// expand to class level
				if (startCommand.getParent() != null && startCommand.getParent().isClassStart()) 
					startCommand = startCommand.getParent();
				if (lastCommand != null && lastCommand.getParent() != null && lastCommand.getParent().isClassStart())
					lastCommand = lastCommand.getParent().getNextSibling();

				// move from PRIVATE SECTION etc. to CLASS start / end
				while (startCommand.getClosesLevel() && startCommand.getPrevSibling() != null) 
					startCommand = startCommand.getPrevSibling();
				while (lastCommand != null && lastCommand.getOpensLevel() && lastCommand.getNextSibling() != null)
					lastCommand = lastCommand.getNextSibling();
				// note that startCommand may now belong to a different class than lastCommand 
			}
			
			/*
			// for classes and methods, include possible comment lines that directly precede startCommand
			if (startCommand.isMethodFunctionOrFormStart() || startCommand.isClassStart()) {
				while (startCommand.getFirstTokenLineBreaks() == 1 && startCommand.getPrev() != null && startCommand.getPrev().isCommentLine())
					startCommand = startCommand.getPrev();
			}
			*/
		}

		if (lastCommand == null) 
			lastCommand = this.lastCommand;
		
		// modify the CleanupRange and prevent further expansion
		int startLine = startCommand.getSourceLineNumStart() - 1; // sourceLineNumStart is 1-based, but startLine is 0-based
		int endLine = lastCommand.getSourceLineNumEnd() - 1 + 1; // endLine must be the 0-based line after the Command  
		cleanupRange = CleanupRange.create(startLine, endLine, false);
	}
	
	public int getLineCountInCleanupRange() {
		int firstLine = Integer.MAX_VALUE;
		int lastLine = -1;
		if (hasCleanupRange()) {
			Command command = firstCommand;
			while (command != null) {
				if (command.isInCleanupRange()) {
					firstLine = Math.min(firstLine, command.getSourceLineNumStart());
					lastLine = Math.max(lastLine, command.getSourceLineNumEnd());
				}
				command = command.getNext();
			}
		} else if (firstCommand != null && lastCommand != null) {
			lastLine = lastCommand.getSourceLineNumEnd();
			firstLine = firstCommand.getSourceLineNumStart();
		}
		if (firstLine == Integer.MAX_VALUE)
			return 0;
		return lastLine - firstLine + 1;
	}
	
	/** returns the first Command in this Code that matches the pattern that is hard-coded in {@link Command#matchesPattern()} */
	public Command getFirstPatternMatch() {
		Command command = firstCommand;
		while (command != null) {
			if (command.matchesPattern()) {
				return command;
			}
			command = command.getNext();
		}
		return null;
	} 

	public final boolean insertStressTestTokentAt(int tokenIndex, StressTestType stressTestType) throws IntegrityBrokenException {
		TokenTypeRefinerRnd rndParser = Program.getRndParser();
		
		Command command = firstCommand;
		boolean found = false;
		while (command != null) {
			if (command.insertStressTestTokenAt(tokenIndex, stressTestType)) {
				found = true;

				// in case syntax errors were introduced (e.g. due to wrong pragma positions), update the number of 
				// erroneous tokens before cleanup; e.g., if there is a comment, pragma (or even line break) between 
				// the keywords "OPEN" and "CURSOR", RND Parser will show all commas in the SELECT field list as erroneous 
				if (rndParser != null) {
					RndParseResult rndParseResult;
					try {
						rndParseResult = rndParser.getRndParseResult(command);
					} catch (ParseException e) {
						throw new IntegrityBrokenException(command, e.getMessage());
					}
					if (rndParseResult != null) {
						command.setErrorStateBeforeCleanup(rndParseResult.errorTokenCount);
					}
				}
			}
			command = command.getNext();
		}
		return found;
	}
}
