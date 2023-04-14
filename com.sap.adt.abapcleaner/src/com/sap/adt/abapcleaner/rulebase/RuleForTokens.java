package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;

/**
 * base class for a Rule that is potentially applied to several Tokens within a Command (but does not need special provisions or context information)
 */
public abstract class RuleForTokens extends Rule {
	protected boolean skipCommand(Command command) {
		return false;
	}

	protected abstract boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges;

	protected RuleForTokens(Profile profile) {
		super(profile);
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// remember the next Command in case the rule removes the current Command from the Code
			Command nextCommand = command.getNext();

			if (!isCommandBlocked(command) && !skipCommand(command)) {
				try {
					Token token = command.getFirstToken();
					while (token != null) {
						// remember the next Token in case the rule removes the current Token from the Command
						// (more sophisticated cases can't extend RuleForTokens but require own logic)
						Token nextToken = token.getNext();
						if (executeOn(code, command, token, releaseRestriction))
							code.addRuleUse(this, command);
						
						if (command.wasRemovedFromCode())
							break;
						token = token.wasRemovedFromCommand() ? nextToken : token.getNext();
					}
				} catch (UnexpectedSyntaxBeforeChanges ex) {
					// log the error and continue with next command
					ex.addToLog();
				}
			}

			command = command.wasRemovedFromCode() ? nextCommand : command.getNext();
		}
	}
}
