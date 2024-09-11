package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;

public abstract class RuleForDdlCommands extends Rule {
	protected abstract boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public Language[] getSupportedLanguages() { return ddlOnly; }

	protected final String[] changeTypeSelection = new String[] { "Always", "Keep as is", "Never" };

	protected RuleForDdlCommands(Profile profile) {
		super(profile);
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// get the next Command now, in case the current command is removed from the code
			Command nextCommand = command.getNext();
			
			if (command.isDdl() && !isCommandBlocked(command)) {
				try {
					if (executeOn(code, command, releaseRestriction)) {
						code.addRuleUse(this, command);
					}
				} catch (UnexpectedSyntaxBeforeChanges ex) {
					// log the error and continue with next command
					ex.addToLog();
				}
			}

			command = nextCommand;
		}
	}
}
