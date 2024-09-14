package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rules.ddl.alignment.DdlNextAfterParensPos;
import com.sap.adt.abapcleaner.rules.ddl.alignment.DdlSourceParamPos;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlPositionDefineRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public abstract class RuleForDdlAlignParameters extends RuleForDdlCommands {
	protected enum Columns {
		PARAMETER,
		ASSIGNMENT_OP,
		EXPRESSION;
		public int getValue() { return this.ordinal(); }
	}
	protected static final int MAX_COLUMN_COUNT = 3;

	protected RuleForDdlAlignParameters(Profile profile) {
		super(profile);
	}

	// settings used from other rules
	protected int getParamsIndent() {
		return ((DdlPositionDefineRule)parentProfile.getRule(RuleID.DDL_POSITION_DEFINE)).getParamsIndent(); 
	}
	private ChangeType getSpaceBeforeColon() { 
		return ((DdlSpacesAroundSignsRule)parentProfile.getRule(RuleID.DDL_SPACES_AROUND_SIGNS)).getSpaceBeforeColon(); 
	}
	private ChangeType getSpaceAfterColon() { 
		return ((DdlSpacesAroundSignsRule)parentProfile.getRule(RuleID.DDL_SPACES_AROUND_SIGNS)).getSpaceAfterColon(); 
	}
	private ChangeType getSpaceInsideParens() { 
		return ((DdlSpacesAroundBracketsRule)parentProfile.getRule(RuleID.DDL_SPACES_AROUND_BRACKETS)).getSpacesInsideFuncParens(); 
	}

	protected boolean moveOpeningParenthesis(Token openingParenthesis) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;

		// move the opening parenthesis behind the view name
		if (openingParenthesis.lineBreaks > 0) {
			// if there is a comment in the way, handle at least the most frequent case of a line-end comment
			Token prev = openingParenthesis.getPrev();
			Token firstChild = openingParenthesis.getFirstChild();

			if (prev.isComment() && prev.getPrev().isCode() && firstChild != null) {
				Token comment = prev;
				comment.removeFromCommand();
				firstChild.copyWhitespaceFrom(openingParenthesis);
				firstChild.insertLeftSibling(comment);
				openingParenthesis.setWhitespace(0, 0);
				changed = true;
			
			} else if (!prev.isComment()) {
				if (firstChild.lineBreaks == 0)
					firstChild.copyWhitespaceFrom(openingParenthesis);
				openingParenthesis.setWhitespace(0, 0);
				changed = true;
			}
		}
		return changed;
	}
	
	private boolean moveClosingParenthesis(Token closingParenthesis, int fallbackIndent) throws UnexpectedSyntaxAfterChanges {
		int lineBreaks;
		int spacesLeft;
		if (closingParenthesis.getPrev().isComment()) {
			lineBreaks = closingParenthesis.lineBreaks;
			spacesLeft = fallbackIndent;
		} else {
			lineBreaks = 0;
			spacesLeft = 0;
			ChangeType spaceInsideParens = getSpaceInsideParens();
			if (spaceInsideParens == ChangeType.ALWAYS || spaceInsideParens == ChangeType.KEEP_AS_IS && !closingParenthesis.isAttached()) {
				spacesLeft = 1;
			}
		}
		return closingParenthesis.setWhitespace(lineBreaks, spacesLeft);
	}

	protected boolean moveTokenAfterClosingParens(Token openingParenthesis, DdlNextAfterParensPos nextPos) {
		Token viewOrFunctionName = openingParenthesis.getPrevCodeSibling();
		Token closingParenthesis = openingParenthesis.getNextSibling();
		Token next = closingParenthesis.getNext();
		if (next == null || next.isKeyword("END"))  // "CASE ... END" is handled elsewhere 
			return false;

		// determine first line breaks and indent
		int lineBreaks = 1;
		int spacesLeft;
		if (nextPos == DdlNextAfterParensPos.CONTINUE) {
			lineBreaks = 0;
			spacesLeft = 1;

		} else if (nextPos == DdlNextAfterParensPos.LINE_START) {
			spacesLeft = openingParenthesis.getFirstTokenInLine().spacesLeft;
		} else if (nextPos == DdlNextAfterParensPos.LINE_START_PLUS_2) {
			spacesLeft = openingParenthesis.getFirstTokenInLine().spacesLeft + 2;
			
		} else if (nextPos == DdlNextAfterParensPos.SOURCE_NAME) {
			spacesLeft = viewOrFunctionName.getStartIndexInLine();
		} else if (nextPos == DdlNextAfterParensPos.SOURCE_NAME_PLUS_2) {
			spacesLeft = viewOrFunctionName.getStartIndexInLine() + 2;
		
		} else { // DdlAsAliasPos.KEEP_AS_IS
			lineBreaks = Math.min(next.lineBreaks, 1);
			spacesLeft = next.spacesLeft;
		}

		return next.setWhitespace(lineBreaks, spacesLeft);
	}

	protected boolean alignParameters(Code code, Token openingParenthesis, String assignmentOpText, DdlSourceParamPos parameterPos, boolean alignAssignmentOps, boolean alignActualParams) throws UnexpectedSyntaxAfterChanges {
		Token viewOrFunctionName = openingParenthesis.getPrevCodeSibling();
		Token firstInnerToken = openingParenthesis.getFirstChild();
		Token closingParenthesis = openingParenthesis.getNextSibling();
		
		if (firstInnerToken == null)
			return false;
		
		// determine first line breaks and indent
		int firstLineBreaks = 1;
		int indent;
		if (parameterPos == DdlSourceParamPos.CONTINUE) {
			firstLineBreaks = 0;
			indent = openingParenthesis.getEndIndexInLine();
			ChangeType spaceInsideParens = getSpaceInsideParens();
			if (spaceInsideParens == ChangeType.ALWAYS || spaceInsideParens == ChangeType.KEEP_AS_IS && !firstInnerToken.isAttached()) {
				++indent;
			}
			
		} else if (parameterPos == DdlSourceParamPos.LINE_START_PLUS_2) {
			indent = openingParenthesis.getFirstTokenInLine().spacesLeft + 2;
		} else if (parameterPos == DdlSourceParamPos.LINE_START_PLUS_4) {
			indent = openingParenthesis.getFirstTokenInLine().spacesLeft + 4;
			
		} else if (parameterPos == DdlSourceParamPos.SOURCE_NAME_PLUS_2) {
			indent = viewOrFunctionName.getStartIndexInLine() + 2;
		} else if (parameterPos == DdlSourceParamPos.SOURCE_NAME_PLUS_4) {
			indent = viewOrFunctionName.getStartIndexInLine() + 4;
		
		} else { // DdlSourceParamPos.KEEP_AS_IS
			firstLineBreaks = Math.min(firstInnerToken.lineBreaks, 1);
			indent = firstInnerToken.getStartIndexInLine();
		}
		
		// build the align table
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		ArrayList<Token> otherLineStarts = new ArrayList<>();
		try {
			buildAlignTable(table, openingParenthesis, closingParenthesis, assignmentOpText, otherLineStarts);
		} catch (UnexpectedSyntaxException e) {
			return false;
		}
		if (table.isEmpty())
			return false;
		
		boolean changed = false;

		// move the closing parenthesis behind the last assignment
		int fallbackIndent = Math.min(indent, openingParenthesis.getStartIndexInLine());
		changed |= moveClosingParenthesis(closingParenthesis, fallbackIndent);
		
		// join columns as configured
		boolean assignOpsAreColons = assignmentOpText.equals(DDL.COLON_SIGN_STRING);
		joinColumns(code, table, alignAssignmentOps, alignActualParams, assignOpsAreColons);

		// align the table
		changed |= (table.align(indent, firstLineBreaks, true) != null);
		
		// align comments between parameters
		for (Token otherLineStart : otherLineStarts) {
			changed |= otherLineStart.setWhitespace(otherLineStart.lineBreaks, indent);
		}
		return changed;
	}

	private void buildAlignTable(AlignTable table, Token parentToken, Token end, String assignmentOpText, ArrayList<Token> otherLineStarts) throws UnexpectedSyntaxException {
		Token token = parentToken.getFirstChild(); // parentToken is the opening parenthesis 
		if (token == null)
			return;
		
		// find assignments within the siblings inside the parentheses or brackets
		while (token != null && token != end && token.getNext() != null) {
			Token next = token.getNext();
			if (token.isIdentifier() && next != null && next.textEquals(assignmentOpText)
					&& next.getNext() != null && !next.getNext().isComment()) {

				// identify the parts of the assignment "parameter = term"
				Token parameter = token;
				Token assignmentOp = parameter.getNextCodeSibling();
				Token exprStart = assignmentOp.getNext();
				Token exprLast = exprStart.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",");
				if (exprLast == null)
					exprLast = end.getPrevCodeToken();
				Term expression = Term.createForTokenRange(exprStart, exprLast);
				
				// build the next table line from the identified parts of the assignment
				AlignLine line = table.addLine();
				line.setCell(Columns.PARAMETER.getValue(), new AlignCellToken(parameter));
				line.setCell(Columns.ASSIGNMENT_OP.getValue(), new AlignCellToken(assignmentOp));
				line.setCell(Columns.EXPRESSION.getValue(), new AlignCellTerm(expression));
				
				token = expression.getNext();

			} else {
				if (token.isComment() && token.lineBreaks > 0)
					otherLineStarts.add(token);
				token = token.getNextSibling();
			}
		}
	}
	
	protected void joinColumns(Code code, AlignTable table, boolean alignAssignmentOps, boolean alignExpressions, boolean assignOpsAreColons) throws UnexpectedSyntaxAfterChanges {
		if (!alignAssignmentOps) {
			// do not create a dedicated column for the assignment operators ":" or "=>"
			
			// determine number of spaces before the assignment operator
			int spacesBeforeAssignOp = 1;
			if (assignOpsAreColons) {
				spacesBeforeAssignOp = (getSpaceBeforeColon() == ChangeType.ALWAYS) ? 1 : 0;
			}

			// join assignment operators into the parameter column
			try {
				Command[] changedCommands = table.getColumn(Columns.ASSIGNMENT_OP.getValue()).joinIntoPreviousColumns(true, spacesBeforeAssignOp, true);
				code.addRuleUses(this, changedCommands);
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e); 
			}
		}

		if (!alignExpressions) {
			// do not create a dedicated column for the actual parameters / type identifiers

			// determine number of spaces after the assignment operator
			int spacesAfterAssignOp = 1;
			if (assignOpsAreColons) {
				spacesAfterAssignOp = (getSpaceAfterColon() == ChangeType.NEVER) ? 0 : 1;
			}

			// join actual parameters / type identifier into the previous column
			try {
				Command[] changedCommands = table.getColumn(Columns.EXPRESSION.getValue()).joinIntoPreviousColumns(true, spacesAfterAssignOp, true);
				code.addRuleUses(this, changedCommands);
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e); 
			}
		}
	}
}
