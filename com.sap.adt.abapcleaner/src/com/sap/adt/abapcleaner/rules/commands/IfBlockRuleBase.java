package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;
import com.sap.adt.abapcleaner.rules.alignment.AlignLogicalExpressionsRule;

public abstract class IfBlockRuleBase extends Rule {
	protected final ConfigIntValue configMinLineCount = new ConfigIntValue(this, "MinLineCount", "Replace IF blocks with at least", "lines", 2, 10, 100);
	protected final ConfigIntValue configMinLinePercentage = new ConfigIntValue(this, "MinLinePercentage", "and a share of at least", "% of all lines", 0, 50, 100, 0, LocalDate.of(2021, 12, 14));
	protected final ConfigBoolValue configKeepExceptionLogicInIf = new ConfigBoolValue(this, "KeepExceptionalLogicInIf", "Keep IF blocks that contain exceptional logic (RAISE, MESSAGE, RETURN, EXIT)", true, false, LocalDate.of(2021, 12, 17));
	// TODO: use common configuration for the following (because it is found in several Rules)?
   protected final ConfigEnumValue<NegationStyle> configNegationStyle = new ConfigEnumValue<NegationStyle>(this, "NegationStyle", NegationStyle.description, NegationStyle.selectionTexts, NegationStyle.values(), NegationStyle.AVOID_INNER_NEGATIONS, NegationStyle.NEVER, LocalDate.of(2022, 10, 17));
	protected final ConfigBoolValue configConvertAbapFalseAndAbapTrue = new ConfigBoolValue(this, "ConvertAbapFalseAndAbapTrue", "Convert abap_false <-> abap_true (assuming abap_undefined is never used)", true);
	protected final ConfigBoolValue configEnsureEmptyLineAfterEndIf = new ConfigBoolValue(this, "EnsureEmptyLineAfterEndIf", "Add empty line after ENDIF", true);

	public IfBlockRuleBase(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	protected final boolean executeOn(Code code, Command ifCommand, boolean isInLoop, NegationStyle negationStyle, boolean convertAbapFalseAndAbapTrue, boolean ensureEmptyLineAfterEndIf, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Command endIfCommand = ifCommand.getNextSibling();

		if (!ifCommand.hasChildren())
			return false; // the IF block is empty

		// read the logical expression to check whether it can be processed
		Token firstToken = ifCommand.getFirstToken();
		Token period = ifCommand.getLastNonCommentToken();
		int indent = firstToken.getStartIndexInLine();
		LogicalExpression logicalExpression;
		try {
			logicalExpression = LogicalExpression.create(firstToken.getNext(), period.getPrev());
			if (!logicalExpression.isSupported())
				return false;
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}

		// move the whole IF body after the ENDIF and ensure there is an empty line above it
		Section moveSection;
		try {
			moveSection = Section.create(ifCommand.getNext(), endIfCommand.getPrev());
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}
		try {
			moveSection.removeFromCode();
			endIfCommand.insertRightSibling(moveSection);
			if (ensureEmptyLineAfterEndIf)
				moveSection.firstCommand.getFirstToken().lineBreaks = Math.max(moveSection.firstCommand.getFirstTokenLineBreaks(), 2);
			moveSection.addIndent(-ABAP.INDENT_STEP);

			// transform "IF <logical expression>." into "IF <negated logical expression>. CONTINUE/RETURN. ENDIF."
			logicalExpression.negate(negationStyle, convertAbapFalseAndAbapTrue);
			endIfCommand.getFirstToken().lineBreaks = 1;

			Command originalCommand = (ifCommand.originalCommand != null) ? ifCommand.originalCommand : ifCommand;
			ifCommand.originalCommand = originalCommand;

			Command continueOrReturn = Command.create(Token.createForAbap(1, indent + 2, isInLoop ? "CONTINUE" : "RETURN", TokenType.KEYWORD, period.sourceLineNum), originalCommand);
			continueOrReturn.getFirstToken().addNext(Token.createForAbap(0, 0, ".", TokenType.PERIOD, period.sourceLineNum));
			try {
				continueOrReturn.finishBuild(ifCommand.getSourceTextStart(), ifCommand.getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, originalCommand, "parse error in newly created command");
			}
			ifCommand.insertFirstChild(continueOrReturn);

			// execute AlignLogicalExpressionsRule on the logical expression in the IF command (unless the Rule is blocked for this Command)
			((AlignLogicalExpressionsRule) parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, ifCommand, firstToken, ifCommand.getLastNonCommentToken(),
					true, releaseRestriction);

		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxAfterChanges(this, ex)).addToLog();
			return false;
		}

		code.addRuleUse(this, ifCommand);
		return true;
	}
	
	/**
	 * Returns true if the direct child commands of the supplied IF block contain a command that indicates exceptional logic 
	 * (RAISE, MESSAGE, RETURN, EXIT) rather than normal logic 
	 * @param ifCommand - the IF statement that starts the block to be analyzed  
	 * @return
	 */
	protected boolean blockContainsExceptionalLogic(Command ifCommand) {
		Command command = ifCommand.getFirstChild();
		while (command != null) {
			if (command.firstCodeTokenIsAnyKeyword("RAISE", "MESSAGE", "RETURN", "EXIT"))
				return true;
			command = command.getNextNonCommentSibling();
		}
		return false;
	}
}
