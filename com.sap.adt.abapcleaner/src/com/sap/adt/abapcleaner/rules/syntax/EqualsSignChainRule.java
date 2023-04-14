package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class EqualsSignChainRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't chain assignments", "#dont-chain-assignments"), 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Equals Sign Chaining", "equals-sign-chaining.md") };

	@Override
	public RuleID getID() { return RuleID.EQUALS_SIGN_CHAIN; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Resolve equals sign chain into several commands"; }

	@Override
	public String getDescription() { return "Resolves assignments to multiple variables with assignment operator chaining (a = b = c = 1.) into several commands."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 6); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD equals_sign_chaining." 
			+ LINE_SEP + "    \" assign a number literal" 
			+ LINE_SEP + "    a = b = 42. \" plain and simple" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" assign a string literal" 
			+ LINE_SEP + "    c = d = e = 'abc'. \" still quite simple" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" assign a simple variable" 
			+ LINE_SEP + "    f = g = h = iv_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" assign a complex expression that should not be repeated" 
			+ LINE_SEP + "    i = j = k = l = m = complex_expression( iv_param_1 = 5" 
			+ LINE_SEP + "                                            iv_param_2 = 'abc' " 
			+ LINE_SEP + "                                            iv_param_3 = VALUE #( iv_param_4 = 42  " 
			+ LINE_SEP + "                                                                  iv_param_5 = iv_value ) ). " 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configRepeatIntegerLiterals = new ConfigBoolValue(this, "RepeatIntegerLiterals", "Repeat integer literals", true);
	final ConfigBoolValue configRepeatStringLiterals = new ConfigBoolValue(this, "RepeatStringLiterals", "Repeat string literals", true);
	final ConfigBoolValue configRepeatSimpleIdentifiers = new ConfigBoolValue(this, "RepeatSimpleIdentifiers", "Repeat simple identifiers", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configRepeatIntegerLiterals, configRepeatStringLiterals, configRepeatSimpleIdentifiers };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EqualsSignChainRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (command.containsChainColon())
			return false;

		// find out whether the command is an assignment with "Equals Sign Chaining" and set assignmentOp to the rightmost "equals sign"
		Token firstToken = command.getFirstToken();
		Token assignmentOp = firstToken.getLastTokenOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_IDENTIFIER, "=");
		if (assignmentOp == null)
			return false;
		int assignmentCount = 2;
		while (assignmentOp.getNext().matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=")) {
			++assignmentCount;
			assignmentOp = assignmentOp.getNext().getLastTokenOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=");
		}
		if (!assignmentOp.getNext().matchesOnSiblings(false, TokenSearch.ANY_ARITHMETIC_EXPRESSION, "."))
			return false;

		boolean repeatIntegerLiterals = configRepeatIntegerLiterals.getValue();
		boolean repeatStringLiterals = configRepeatStringLiterals.getValue();
		boolean repeatSimpleIdentifiers = configRepeatSimpleIdentifiers.getValue();

		Command originalCommand = (command.originalCommand != null) ? command.originalCommand : command;
		command.originalCommand = originalCommand;

		// all resulting Commands get the same .spacesLeft; only the first will get the current .lineBreaks (possibly > 1), all others get 1 lineBreak only
		int lineBreaks = firstToken.lineBreaks;
		int spacesLeft = firstToken.spacesLeft;

		// get a possible comment at the end of the line
		Token commentAfterPeriod = command.getLastNonCommentToken().getNext();
		if (commentAfterPeriod != null && !commentAfterPeriod.isCommentAfterCode())
			commentAfterPeriod = null;

		// if the final Term is a simple literal or identifier, it will be copied to all Commands
		Term copyTerm = null;

		// start with the last assignment and create a new Command (above the current Command) from it;
		// then continue with the assignment before that etc.
		Token identifier = assignmentOp.getPrev();
		for (int i = 0; i < assignmentCount - 1; ++i) {
			// create a new Command (which will be added above the current Command) with a new . sign
			// (the Command is built from right to left)
			Token newPeriod = Token.createForAbap(0, 0, ".", TokenType.PERIOD, identifier.sourceLineNum);
			Command newCommand = Command.create(newPeriod, originalCommand);

			// if applicable, move the line-end comment to this (first) newly created Command
			if (i == 0 && commentAfterPeriod != null) {
				commentAfterPeriod.removeFromCommand();
				newPeriod.insertRightSibling(commentAfterPeriod);
			}

			// move the Term to the new Command
			Term moveTerm;
			try {
				moveTerm = Term.createArithmetic(identifier.getNext().getNext()); // may as well be a mere identifier
			} catch (UnexpectedSyntaxException ex) {
				throw new UnexpectedSyntaxAfterChanges(this, ex);
			}
			int oldIndexInLine = moveTerm.firstToken.getStartIndexInLine();
			moveTerm.removeFromCommand(true);
			moveTerm.firstToken.setWhitespace();
			newPeriod.insertLeftSibling(moveTerm);

			// in the first loop cycle, decide whether the Term is simple and can be copied to all other Commands
			if (i == 0 && ((repeatIntegerLiterals && moveTerm.isSingleIntegerLiteral()) 
					|| (repeatStringLiterals && moveTerm.isSingleStringLiteral())
					|| (repeatSimpleIdentifiers && moveTerm.isSingleIdentifier()))) {
				copyTerm = moveTerm;
			}
			
			// move the assignment operator (=) to the new Command
			Token moveAssignmentOp = identifier.getNext();
			moveAssignmentOp.removeFromCommand();
			moveAssignmentOp.setWhitespace();
			moveTerm.firstToken.insertLeftSibling(moveAssignmentOp);

			// create a copy of the current identifier and add it at the beginning of the new Command
			Token copiedIdentifier = Token.createForAbap(lineBreaks, spacesLeft, identifier.getText(), TokenType.IDENTIFIER, identifier.sourceLineNum);
			moveAssignmentOp.insertLeftSibling(copiedIdentifier);

			// adjust the indent in case the Term consist of multiple lines
			int newIndexInLine = moveTerm.firstToken.getStartIndexInLine();
			newCommand.addIndent(newIndexInLine - oldIndexInLine, oldIndexInLine, moveTerm.firstToken);
			try {
				newCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, command, "parse error in extracted command");
			}

			// insert the new Command before the current one
			command.insertLeftSibling(newCommand);
			code.addRuleUse(this, newCommand);

			// move to the previous identifier (only the last item can be a Term, so .getPrev().getPrev() should lead there)
			identifier = identifier.getPrev().getPrev();
			if (!identifier.isIdentifier())
				throw new UnexpectedSyntaxAfterChanges(this, identifier, "Expected an identifier, but found " + identifier.getTypeAndTextForErrorMessage() + "!");

			// if the final Term is simple, use it in all Commands (rather than writing d = 42. c = d. b = c. a = b.)
			if (copyTerm != null) {
				Token obsoleteIdentifier = identifier.getNext().getNext();
				Token copyOfSimpleTerm = Token.createForAbap(0, 1, copyTerm.firstToken.getText(), copyTerm.firstToken.sourceLineNum);
				obsoleteIdentifier.insertLeftSibling(copyOfSimpleTerm);
				obsoleteIdentifier.removeFromCommand();
			}

			lineBreaks = 1;
		}
		firstToken.lineBreaks = 1;
		command.invalidateMemoryAccessType();
		return true;
	}
}
