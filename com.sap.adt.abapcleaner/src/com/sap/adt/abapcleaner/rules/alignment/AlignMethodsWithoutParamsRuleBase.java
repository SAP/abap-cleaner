package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

public abstract class AlignMethodsWithoutParamsRuleBase extends AlignDeclarationSectionRuleBase {
   private enum CommonColumns  {
      KEYWORD,
      METHOD_NAME,
   	FOR_TESTING_OR_REDEFINITION;

		public int getValue() { return this.ordinal(); }
	}

	protected abstract int getMaxColumnCount();
	protected abstract Token buildTableLine(AlignTable table, Token token, boolean isChain) throws UnexpectedSyntaxException;
	
	private static final String[] changeTypeSelection = new String[] { "always", "keep as is", "never" };
	final ConfigEnumValue<ChangeType> configContinueAfterKeyword = new ConfigEnumValue<ChangeType>(this, "ContinueAfterKeyword", "Continue line after METHODS", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS, ChangeType.ALWAYS, LocalDate.of(2023,  3,  7));
	final ConfigEnumValue<ChangeType> configContinueAfterMethodName = new ConfigEnumValue<ChangeType>(this, "ContinueAfterMethodName", "Continue line after method name", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS, ChangeType.ALWAYS, LocalDate.of(2023,  3,  7));
	 
	public AlignMethodsWithoutParamsRuleBase(Profile profile) {
		super(profile);
	}

	@Override
	protected void executeOn(Code code, Command startCommand, Command endCommand, boolean isChain, int pass) throws UnexpectedSyntaxBeforeChanges {
		AlignTable table = new AlignTable(getMaxColumnCount());

		Command command = startCommand;
		if (command.containsInnerCommentLine())
			return;
		Token token = command.getFirstToken();

		int firstLineBreaks = token.lineBreaks;
		int basicIndent = token.getStartIndexInLine();

		try {
			do {
				// skip comment lines
				if (isChain) {
					while (token != null && token.isCommentLine())
						token = token.getNext();
					if (token == null)
						break;
				} else {
					while (command != endCommand && skipCommand(command))
						command = command.getNext();
					if (command == endCommand)
						break;
					commandForErrorMsg = command;
					token = command.getFirstToken();
				}

				token = buildTableLine(table, token, isChain);
				if (token == null) 
					return;
				
				// move to next method declaration
				if (isChain) {
					token = token.getNext();
				} else {
					command = command.getNext();
					if (command == endCommand)
						break;
					if (command.containsInnerCommentLine())
						return;
					token = command.getFirstToken();
				}
			} while (token != null);

			joinColumns(code, table, isChain);
			
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		if (table.getLineCount() > 0) {
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, false);
			code.addRuleUses(this, changedCommands);
		}
	}

	protected void joinColumns(Code code, AlignTable table, boolean isChain) throws UnexpectedSyntaxException {
		// insert line breaks depending on rule configuration
		for (int colIndex = CommonColumns.KEYWORD.getValue(); colIndex <= CommonColumns.METHOD_NAME.getValue(); ++colIndex) {
			AlignColumn column = table.getColumn(colIndex);
			if (column.isEmpty())
				continue;

			// determine the configuration for this column
			ChangeType continueLine;
			if (colIndex == CommonColumns.KEYWORD.getValue()) {
				continueLine = ChangeType.forValue(configContinueAfterKeyword.getValue());
			} else { // CommonColumns.METHOD_NAME
				continueLine = ChangeType.forValue(configContinueAfterMethodName.getValue());
			}
			
			// if configured, remove or insert line breaks
			boolean forceLineBreak = changeLineBreaksAfterColumn(code, table, colIndex, continueLine);
			
			// if configuration demands to break after this column, or if KEEP_AS_IS is configured and current code always breaks,
			// configure the AlignColumn to force a line break (required by AlignTable.align() method) 
			if (forceLineBreak) {
				column.setForceLineBreakAfter(forceLineBreak);
				// if there is a line break after 'METHODS method_name' (and we're not in a chain), FOR TESTING or REDEFINITION  
				// is indented below METHODS (NOT below method_name, unless we break after METHODS, too)
				if (colIndex == CommonColumns.METHOD_NAME.getValue() && !isChain) {
					AlignColumn forTestingCol = table.getColumn(CommonColumns.FOR_TESTING_OR_REDEFINITION.getValue()); 
					if (table.getColumn(CommonColumns.KEYWORD.getValue()).getForceLineBreakAfter()) {
						forTestingCol.setForceIndent(CommonColumns.METHOD_NAME.getValue(), ABAP.INDENT_STEP);
					} else {
						forTestingCol.setForceIndent(CommonColumns.KEYWORD.getValue(), ABAP.INDENT_STEP);
					}
				}
			}

			if (continueLine != ChangeType.ALWAYS) {
				table.overrideWidthIfColumnIsFollowedByLineBreaks(column);
			}
		}

	}

	private boolean changeLineBreaksAfterColumn(Code code, AlignTable table, int colIndex, ChangeType continueLine) {
		boolean forceLineBreak = (continueLine != ChangeType.ALWAYS);
		
		for (AlignLine line : table.getLines()) {
			if (line.getCell(colIndex) == null)
				continue;
			
			AlignCell nextCell = line.getNextNonEmptyCellAfter(colIndex);
			if (nextCell == null) 
				continue;
			
			Token nextToken = nextCell.getFirstToken();
			if (continueLine == ChangeType.ALWAYS) {
				if (nextToken.lineBreaks > 0 && !nextToken.getPrev().isComment()) { 
					nextToken.setWhitespace();
					code.addRuleUse(this, nextToken.getParentCommand());
				}
			
			} else if (continueLine == ChangeType.KEEP_AS_IS) {
				if (nextToken.lineBreaks == 0)
					forceLineBreak = false;
					
			} else if (continueLine == ChangeType.NEVER) {
				if (nextToken.lineBreaks == 0) { 
					nextToken.setWhitespace(1,  nextToken.getStartIndexInLine());
					code.addRuleUse(this, nextToken.getParentCommand());
				}
			}
		}
		return forceLineBreak;
	}
}
