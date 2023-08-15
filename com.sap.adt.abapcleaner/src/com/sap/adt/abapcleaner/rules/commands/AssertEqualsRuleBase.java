package com.sap.adt.abapcleaner.rules.commands;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.spaces.ClosingBracketsPositionRule;

public abstract class AssertEqualsRuleBase extends RuleForCommands {
	private String getStaticAssertClassCall(String methodName) {
		return "cl_abap_unit_assert=>" + methodName + "(";
	}

	private ClosingBracketsPositionRule closeBracketsAtLineEndRule = null;

	protected boolean performAdditionalCleanUp(Token assertCall) throws UnexpectedSyntaxAfterChanges {
		return false;
	}

	public AssertEqualsRuleBase(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	protected final boolean changeAssert(Code code, Command command, int releaseRestriction, String oldMethodName, String paramName, String paramValue, String newMethodName) throws UnexpectedSyntaxAfterChanges {
		// example call: oldMethodName = "assert_equals", paramName = "exp", paramValue = "abap_true", newMethodName = "assert_true"

		Token token = command.getFirstCodeToken();
		if (token == null)
			return false;
		if (!token.matchesDeep(false, getStaticAssertClassCall(oldMethodName), TokenSearch.ASTERISK, paramName, "=", paramValue))
			return false;

		Token assertCall = token;
		Token paramToken = token.getLastTokenDeep(true, TokenSearch.ASTERISK, paramName);
		Token assignmentOp = paramToken.getNext();
		Token boolValue = assignmentOp.getNext();
		Token next = boolValue.getNext();

		// change assert method
		assertCall.setText(getStaticAssertClassCall(newMethodName), true);

		// prepare to move the following Token (i.e. the line-end comment, the next parameter, or the closing parenthesis) 
		// to the position of paramToken - or to the previous line, if it is a line-end comment
		boolean paramContinuedLine = (paramToken.lineBreaks == 0);
		boolean nextContinuedLine = (next.lineBreaks == 0);
		boolean mayRemoveClosingBracket = (boolValue.getNextCodeSibling() == null);

		// remove the "(paramName) = ..." assignment
		paramToken.removeFromCommand();
		assignmentOp.removeFromCommand();
		boolValue.removeFromCommand();

		// move the following Token (see above)
		if (paramContinuedLine) {
			next.copyWhitespaceFrom(paramToken);
		} else if (next.isComment() && nextContinuedLine && !next.getPrev().isComment()) {
			// move the line-end comment behind the previous line
			next.setWhitespace();
		}		

		if (performAdditionalCleanUp(assertCall))
			mayRemoveClosingBracket = true;

		// if applicable, move closing bracket on the previous line
		if (mayRemoveClosingBracket) {
			// "(paramName) = ..." was the last parameter
			if (closeBracketsAtLineEndRule == null)
				closeBracketsAtLineEndRule = (ClosingBracketsPositionRule) parentProfile.getRule(RuleID.CLOSING_BRACKETS_POSITION);
			closeBracketsAtLineEndRule.executeOn(code, command, false, releaseRestriction);
		}

		return true;
	}

}