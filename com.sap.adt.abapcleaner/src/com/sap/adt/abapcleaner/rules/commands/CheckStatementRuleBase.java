package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;
import com.sap.adt.abapcleaner.rules.alignment.AlignLogicalExpressionsRule;

public abstract class CheckStatementRuleBase extends Rule {
   protected final ConfigBoolValue configConvertAbapFalseAndAbapTrue = new ConfigBoolValue(this, "ConvertAbapFalseAndAbapTrue", "Convert abap_false <-> abap_true (assuming abap_undefined is never used)", true);
   protected final ConfigEnumValue<NegationStyle> configNegationStyle = new ConfigEnumValue<NegationStyle>(this, "NegationStyle", NegationStyle.description, NegationStyle.selectionTexts, NegationStyle.values(), NegationStyle.AVOID_INNER_NEGATIONS, NegationStyle.NEVER, LocalDate.of(2022, 10, 17));

	public CheckStatementRuleBase(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	/**
	 * transforms "CHECK {logical expression}." into "IF {negated logical expression}. CONTINUE/RETURN. ENDIF."
	 * @throws IntegrityBrokenException 
	 */
	protected final boolean executeOn(Code code, Command command, boolean isInLoop, boolean processChains, NegationStyle negationStyle, boolean convertAbapFalseAndAbapTrue, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		ArrayList<Command> unchainedCommands = unchain(code, command, processChains);
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return false;
		
		for (Command unchainedCommand : unchainedCommands) {
			if (executeOnUnchained(code, unchainedCommand, isInLoop, negationStyle, convertAbapFalseAndAbapTrue, releaseRestriction)) {
				code.addRuleUse(this, unchainedCommand);
			}
		}
		return false; // addRuleUse() was already called above
	}

	private final boolean executeOnUnchained(Code code, Command command, boolean isInLoop, NegationStyle negationStyle, boolean convertAbapFalseAndAbapTrue, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		Token period = command.getLastNonCommentToken();
		int indent = firstToken.getStartIndexInLine();

		// read the logical expression to check whether it can be processed
		LogicalExpression logicalExpression;
		try {
			logicalExpression = LogicalExpression.create(firstToken.getNext(), period.getPrev());
			if (!logicalExpression.isSupported())
				return false;
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}

		try {
			// negate the logical expression of the CHECK
			logicalExpression.negate(negationStyle, convertAbapFalseAndAbapTrue);

			Command originalCommand = (command.originalCommand != null) ? command.originalCommand : command;

			firstToken.setText("IF", true);
			command.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
			command.originalCommand = originalCommand;

			Command endifCommand = Command.create(Token.createForAbap(1, firstToken.getStartIndexInLine(), "ENDIF", TokenType.KEYWORD, period.sourceLineNum), originalCommand);
			endifCommand.getFirstToken().addNext(Token.createForAbap(0, 0, ".", TokenType.PERIOD, period.sourceLineNum));
			endifCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
			command.insertRightSibling(endifCommand);

			Command returnCommand = Command.create(Token.createForAbap(1, indent + 2, isInLoop ? "CONTINUE" : "RETURN", TokenType.KEYWORD, period.sourceLineNum), originalCommand);
			returnCommand.getFirstToken().addNext(Token.createForAbap(0, 0, ".", TokenType.PERIOD, period.sourceLineNum));
			returnCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
			command.insertFirstChild(returnCommand);

			// execute AlignLogicalExpressionsRule on the logical expression in the IF command (unless the Rule is blocked for this Command)
			((AlignLogicalExpressionsRule) parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, command, firstToken, command.getLastNonCommentToken(),
					true, releaseRestriction);

		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxAfterChanges(this, ex)).addToLog();
			return false;
		} catch (ParseException ex) {
			(new UnexpectedSyntaxAfterChanges(this, null)).addToLog();
			return false;
		}
		return true;
	}
}
