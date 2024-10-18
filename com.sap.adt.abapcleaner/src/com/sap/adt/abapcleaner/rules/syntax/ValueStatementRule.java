package com.sap.adt.abapcleaner.rules.syntax;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;
import com.sap.adt.abapcleaner.rules.spaces.ClosingBracketsPositionRule;

import java.time.LocalDate;
import java.util.*;

public class ValueStatementRule extends RuleForTokens {
	// see "Short Form for Structured Line Types":
	// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenvalue_constructor_params_lspc.htm

	private enum Columns {
		PARAMETER, 
		ASSIGNMENT_OP, 
		EXPRESSION, 
		COMMENT;

		public int getValue() { return this.ordinal(); }
	}

	private static final int MAX_COLUMN_COUNT = 4;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.VALUE_STATEMENT; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Shorten VALUE statements"; }

	@Override
	public String getDescription() { return "Shortens VALUE statements for internal tables by factoring out assignments that are identical in all lines."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 15); }

	@Override
	public RuleReference[] getReferences() { return references; }

	// getRequiredAbapRelease() not required, as 'shortening' was immediately available with the VALUE constructor operator in ABAP release 7.40 
	
	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD shorten_value_statements." 
			+ LINE_SEP + "    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )" 
			+ LINE_SEP + "                        ( a = 2  b = 4  c = 8 ) " 
			+ LINE_SEP + "                        ( a = 2  b = 5  c = 10 )  )." 
			+ LINE_SEP 
			+ LINE_SEP + "    lts_data_exp = VALUE #( ( item_key       = '20220010000101'" 
			+ LINE_SEP + "                              item_type      = lt_item[ 1 ]->item_data-item_type" 
			+ LINE_SEP + "                              contract_id    = lt_item[ 1 ]->get_contract_id( )" 
			+ LINE_SEP + "                              contrac_type   = 'ABCD'" 
			+ LINE_SEP + "                              category       = 'AB'" 
			+ LINE_SEP + "                              flag           = if_any_interface=>co_any_flag" 
			+ LINE_SEP + "                              is_defined     = 'Y'" 
			+ LINE_SEP + "                              gjahr          = 2022" 
			+ LINE_SEP + "                              poper          = '001'" 
			+ LINE_SEP + "                              amount         = 6000" 
			+ LINE_SEP + "                              currency       = if_any_interface=>co_any_currency" 
			+ LINE_SEP + "                              account        = '12345'" 
			+ LINE_SEP + "                              guid           = '1')" 
			+ LINE_SEP + "                            ( item_key       = '20220010000101'" 
			+ LINE_SEP + "                              item_type      = lt_item[ 1 ]->item_data-item_type" 
			+ LINE_SEP + "                              contract_id    = lt_item[ 1 ]->get_contract_id( )" 
			+ LINE_SEP + "                              contract_type  = 'ABCD'" 
			+ LINE_SEP + "                              category       = 'CD'" 
			+ LINE_SEP + "                              flag           = if_any_interface=>co_other_flag" 
			+ LINE_SEP + "                              is_optional    = abap_true" 
			+ LINE_SEP + "                              is_defined     = 'Y'" 
			+ LINE_SEP + "                              gjahr          = 2022" 
			+ LINE_SEP + "                              poper          = '001'" 
			+ LINE_SEP + "                              amount         = -6000" 
			+ LINE_SEP + "                              currency       = if_any_interface=>co_any_currency" 
			+ LINE_SEP + "                              account        = '67890'" 
			+ LINE_SEP + "                              guid           = '2' ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" statements that contain commented-out table rows should be skipped," 
			+ LINE_SEP + "    \" because the commented-out content can not be considered" 
			+ LINE_SEP + "    lts_data = VALUE #( ( a = 2  b = 3  c = 6 )" 
			+ LINE_SEP + "*                        ( a = 1  b = 4  c = 8 ) " 
			+ LINE_SEP + "                        ( a = 2  b = 5  c = 10 )  )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" by contrast, statements with commented-out assignments can be processed," 
			+ LINE_SEP + "    \" because the parameters in those assignments will never be moved to the top:"
			+ LINE_SEP + "    lts_data = VALUE #( ( a = 2" 
			+ LINE_SEP + "*                          b = 3" 
			+ LINE_SEP + "                          c = 6 )" 
			+ LINE_SEP + "                        ( a = 2" 
			+ LINE_SEP + "                          b = 3" 
			+ LINE_SEP + "                          c = 8 ) )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configMoveIntegerLiterals = new ConfigBoolValue(this, "MoveIntegerLiterals", "Move assignments with integer literals", true);
	final ConfigBoolValue configMoveFloatLiterals = new ConfigBoolValue(this, "MoveFloatLiterals", "Move assignments with '...' numeric literals", true);
	final ConfigBoolValue configMoveStringLiterals = new ConfigBoolValue(this, "MoveStringLiterals", "Move assignments with string literals", true);
	final ConfigBoolValue configMoveIdentifiers = new ConfigBoolValue(this, "MoveIdentifiers", "Move assignments with identifiers / constants", true);
	final ConfigBoolValue configMoveComplexExpressions = new ConfigBoolValue(this, "MoveComplexExpressions", "Move assignments with complex expressions", true); 
	final ConfigBoolValue configMoveMethodCalls = new ConfigBoolValue(this, "MoveMethodCalls", "Move assignments with method calls (WARNING: could change behavior if methods with side effects are called!)", false);
	final ConfigBoolValue configSkipIfRowsCommentedOut = new ConfigBoolValue(this, "SkipIfRowsCommentedOut", "Skip statements that contain commented-out rows", true, true, LocalDate.of(2024, 10, 11));

	private final ConfigValue[] configValues = new ConfigValue[] { configMoveIntegerLiterals, configMoveFloatLiterals, configMoveStringLiterals, configMoveIdentifiers, configMoveComplexExpressions, configMoveMethodCalls, configSkipIfRowsCommentedOut };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private ClosingBracketsPositionRule closeBracketsAtLineEndRule = null;

	public ValueStatementRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// find the next "VALUE ...( ... (" statement with inner parentheses
		Token nextCode = token.getNextCodeSibling();
		if (!token.isKeyword("VALUE") || !nextCode.getOpensLevel() || !nextCode.hasChildren() || nextCode.isLiteral()
				|| !nextCode.getNext().matchesOnSiblings(true, TokenSearch.ASTERISK, "("))
			return false;

		// skip the statement if it contains commented-out table(!) rows
		Token parentToken = nextCode;
		if (configSkipIfRowsCommentedOut.getValue()) {
			Token test = parentToken.getFirstChild();
			while (test != null) {
				if (test.isAsteriskCommentLine()) {
					String commentText = test.getText();
					int parensOpenPos = commentText.indexOf('(');
					if (parensOpenPos > 0) {
						String[] asterisksAndParens = StringUtil.split(commentText.substring(0, parensOpenPos), ' ', true);
						// return if the opening "(" is only preceded by *** (any number) and spaces 
						if (asterisksAndParens.length == 1) {
							return false;
						}
					}
				}
				test = test.getNextSibling();
			}
		}

		// determine minimum indentation for parameters (in case putting them behind the opening bracket would exceed line length):
		// if the result of the method call, table expression etc. is assigned, the parameters must remain right of the assignment operator
		AlignTable commonAssignmentsTable;
		try {
			commonAssignmentsTable = createTableFromAssignmentSequence(parentToken);
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		Token nextParent = commonAssignmentsTable.endToken;

		HashMap<String, AlignLine> commonAssignments = null;
		ArrayList<AlignTable> rows = new ArrayList<AlignTable>();

		while (nextParent != null && nextParent.textEquals("(")) {
			// skip the group_key in 'FOR GROUP OF ... IN ... GROUP BY ( ... )'
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenfor_groups_of.htm
			Token prev = nextParent.getPrevCodeSibling();
			Token prevPrev = (prev == null) ? null : prev.getPrevCodeSibling();
			if (prevPrev != null && prevPrev.isKeyword("GROUP") && prev.isKeyword("BY")) {
				nextParent = nextParent.getNextSibling();
				while (nextParent != null && !nextParent.textEquals("(")) {
					nextParent = nextParent.getNextSibling();
				}
				continue;
			}
			
			AlignTable newRow;
			try {
				newRow = createTableFromAssignmentSequence(nextParent);
			} catch (UnexpectedSyntaxException ex) {
				throw new UnexpectedSyntaxBeforeChanges(this, ex);
			}

			// ensure that there is no third nesting level
			if (newRow.endToken != null && newRow.endToken.textEquals("("))
				return false;

			// if any of the "rows" is empty, there can be nothing to be shortened
			if (newRow.isEmpty())
				return false;

			HashMap<String, AlignLine> assignmentsInRow = newRow.getLineDictionary();
			if (commonAssignments == null) {
				// filter the assignments in the first row, depending on the configuration of this Rule
				commonAssignments = new HashMap<String, AlignLine>();
				for (Map.Entry<String, AlignLine> kvp : assignmentsInRow.entrySet()) {
					Term expression = ((AlignCellTerm) kvp.getValue().getCell(Columns.EXPRESSION.getValue())).getTerm();
					if (expression.isSingleIntegerLiteral()) {
						if (!configMoveIntegerLiterals.getValue())
							continue;
					} else if (expression.isSingleFloatLiteral()) {
						if (!configMoveFloatLiterals.getValue())
							continue;
					} else if (expression.isSingleStringLiteral()) {
						if (!configMoveStringLiterals.getValue())
							continue;
					} else if (expression.isSingleIdentifier()) {
						if (!configMoveIdentifiers.getValue())
							continue;
					} else if (!expression.isSingleToken()) {
						if (expression.mayHaveSideEffects() && !configMoveMethodCalls.getValue())
							continue;
						if (!configMoveComplexExpressions.getValue())
							continue;
					}
					commonAssignments.put(kvp.getKey(), kvp.getValue());
				}
			} else {
				// identify the assignments that are (still) common to all the rows
				HashMap<String, AlignLine> remainingAssignments = new HashMap<String, AlignLine>();
				for (String key : commonAssignments.keySet()) {
					if (assignmentsInRow.containsKey(key)) {
						remainingAssignments.put(key, commonAssignments.get(key));
					}
				}
				commonAssignments = remainingAssignments;
			}
			// if no common assignments remain, this rule cannot be applied to this VALUE statement
			if (commonAssignments.isEmpty())
				return false;

			rows.add(newRow);

			// find the beginning of the next row, potentially skipping further first-level assignments and comments
			nextParent = newRow.endToken;
			while (nextParent != null && !nextParent.textEquals("(")) {
				nextParent = nextParent.getNextSibling();
			}
		}

		if (rows.size() < 2 || commonAssignments.isEmpty())
			return false;

		// now "factor out" all common assignments, using the "order of appearance" from the first row
		AlignTable firstRow = rows.get(0);
		// move lines before the "(" of the first row; this "(" will have to be at the beginning of a line,
		// since we are sure by now that there are assignments to be factored out; this way, we are also safe to move line-end comments before the "("
		Token insertBeforeToken = firstRow.parentToken;
		int indent = insertBeforeToken.getStartIndexInLine();
		if (insertBeforeToken.lineBreaks == 0)
			insertBeforeToken.setWhitespace(1, indent);

		int lineBreaksBeforeAdjust = -1;
		int nextLineBreaksBeforeAdjust = -1;
		boolean hasEmptyLine = false;
		for (AlignLine line : firstRow.getLines()) {
			String simplifiedText = line.getSimplifiedText();
			if (!commonAssignments.containsKey(simplifiedText)) {
				lineBreaksBeforeAdjust = -1;
				continue;
			}

			Term lineToMove = getLineToMove(line);
			// remember the number of line breaks above the next assignment before the adjustment
			nextLineBreaksBeforeAdjust = setWhitespaceAfter(lineToMove);
			lineToMove.removeFromCommand(false);

			Token identifier = lineToMove.firstToken;
			if (insertBeforeToken.getPrev().getOpensLevel()) {
				identifier.setWhitespace();
			} else if (identifier.lineBreaks == 0) { // insertBefore.getPrev() could be a comment or part of a BASE section etc.
				identifier.setWhitespace(1, indent);
			} else if (identifier.lineBreaks > 0) {
				// if multiple assignments are 'factored out', ensure that an empty line above the first assignment 
				// is not propagated to all subsequent assignments  
				int lineBreaks = (lineBreaksBeforeAdjust < 0) ? identifier.lineBreaks : Math.max(lineBreaksBeforeAdjust, 1);
				identifier.setWhitespace(lineBreaks, indent);
				hasEmptyLine |= (lineBreaks > 1);
			}
			insertBeforeToken.insertLeftSibling(lineToMove);
			
			for (int rowIndex = 1; rowIndex < rows.size(); ++rowIndex) {
				AlignTable row = rows.get(rowIndex);
				AlignLine furtherLine = row.getLineBySimplifiedText(simplifiedText); // line must exist, otherwise this entry would have been removed from commonAssignments

				Term lineToRemove = getLineToMove(furtherLine);
				setWhitespaceAfter(lineToRemove);
				lineToRemove.removeFromCommand(false);
			}
			lineBreaksBeforeAdjust = nextLineBreaksBeforeAdjust;
		}
		// if the 'factored out' assignments contained at least one empty line, add another empty line before the first table row 
		if (hasEmptyLine) {
			insertBeforeToken.setLineBreaks(Math.max(insertBeforeToken.lineBreaks, 2));
		}
		
		// move closing brackets to line end, since .setWhitespaceAfter() does not move them if they are preceded by a comment 
		if (closeBracketsAtLineEndRule == null)
			closeBracketsAtLineEndRule = (ClosingBracketsPositionRule) parentProfile.getRule(RuleID.CLOSING_BRACKETS_POSITION);
		closeBracketsAtLineEndRule.executeOn(code, command, false, releaseRestriction);

		command.invalidateMemoryAccessType();
		
		return true;
	}

	private Term getLineToMove(AlignLine line) throws UnexpectedSyntaxAfterChanges {
		try {
			return Term.createForTokenRange(line.getFirstToken(), line.getLastToken());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxAfterChanges(this, ex);
		}
	}

	private int setWhitespaceAfter(Term lineToMove) {
		Token next = lineToMove.getNext();
		if (next.closesLevel() && next.lineBreaks == 0) {
			Token prev = lineToMove.getPrev();
			if (prev.isComment()) {
				// do NOT move the closing bracket to the previous line, this must be done by ClosingBracketsPositionRule
				next.copyWhitespaceFrom(lineToMove.firstToken);
			} else {
				// ensure that an attached closing bracket is detached - e.g. in the case of "iv_param = 'abc')",
				// because the remaining parameter may not end with '
				next.spacesLeft = Math.max(next.spacesLeft, 1);
			}
			return -1;
		} else if (!next.closesLevel() && !next.isAsteriskCommentLine()) {
			int nextLineBreaksBeforeAdjust = next.lineBreaks;
			next.copyWhitespaceFrom(lineToMove.firstToken);
			return nextLineBreaksBeforeAdjust;
		} else {
			return -1;
		}
	}
	
	private AlignTable createTableFromAssignmentSequence(Token parentToken) throws UnexpectedSyntaxException {
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		table.parentToken = parentToken;

		// find assignments within the parentheses, stopping at "(" which opens a new row
		Token token = parentToken.getNext();
		while (token != null && token != parentToken.getNextSibling() && !token.textEquals("(")) {
			if (token.isAssignmentOperator() && token.getPrev().isIdentifier() && token.getNext() != null && !token.getNext().isComment()) {
				Token parameter = token.getPrev();
				Token assignmentOp = token;
				Term expression = Term.createArithmetic(assignmentOp.getNext());

				AlignLine line = table.addLine();
				line.setCell(Columns.PARAMETER.getValue(), new AlignCellToken(parameter));
				line.setCell(Columns.ASSIGNMENT_OP.getValue(), new AlignCellToken(assignmentOp));
				line.setCell(Columns.EXPRESSION.getValue(), new AlignCellTerm(expression));

				token = expression.getNext();
				if (token.isCommentAfterCode()) {
					line.setCell(Columns.COMMENT.getValue(), new AlignCellToken(token));
					token = token.getNext();
				}

			} else if (token.getNextSibling() != null) {
				token = token.getNextSibling();

			} else {
				// make sure table.endToken is not null, but points to the closing parentheses, esp. if there is a comment line before it
				token = token.getNext();
				break;
			}
		}

		table.endToken = token;
		return table;
	}
}
