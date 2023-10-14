package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;

/**
 * base class for a Rule that is applied once per Command at maximum (and does not need special provisions or context information)
 */
public abstract class RuleForCommands extends Rule {
	protected abstract boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges;
	
	protected boolean skipDeclarationsInsideBeginOf() { return false; }
	
	protected RuleForCommands(Profile profile) {
		super(profile);
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		boolean skipInsideBeginOf = skipDeclarationsInsideBeginOf();
		int blockLevel = 0;
		Command command = code.firstCommand;

		while (command != null) {
			commandForErrorMsg = command;

			// get the next Command now, in case the current command is removed from the code
			Command nextCommand = command.getNext();
			int blockLevelDiff = command.getBlockLevelDiff();
			
			if (!isCommandBlocked(command) && (!skipInsideBeginOf || blockLevel == 0)) {
				try {
					if (executeOn(code, command, releaseRestriction)) {
						code.addRuleUse(this, command);
					}
				} catch (UnexpectedSyntaxBeforeChanges ex) {
					// log the error and continue with next command
					ex.addToLog();
				}
			}

			blockLevel += blockLevelDiff;
			command = nextCommand;
		}
	}
}
