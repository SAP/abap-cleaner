package com.sap.adt.abapcleaner.rules.alignment;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;

/**
 * base class for a Rule that works on a section of consecutive declaration commands (including a declaration chain command)
 */
public abstract class AlignDeclarationSectionRuleBase extends Rule {
	protected int getNumberOfPasses() { return 1; }
	
	protected boolean getAlignAcrossEmptyLinesDefault() { return true; }
	protected boolean getAlignAcrossCommentLinesDefault() { return true; }
	
	protected abstract boolean isMatchForFirstCommand(Command command, int pass);

	protected abstract boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass);

	protected abstract void executeOn(Code code, Command startCommand, Command endCommand, boolean isChain, int pass) throws UnexpectedSyntaxBeforeChanges;

	protected final ConfigBoolValue configAlignAcrossEmptyLines = new ConfigBoolValue(this, "AlignAcrossEmptyLines", "Align across empty lines", getAlignAcrossEmptyLinesDefault());
	protected final ConfigBoolValue configAlignAcrossCommentLines = new ConfigBoolValue(this, "AlignAcrossCommentLines", "Align across comment lines", getAlignAcrossCommentLinesDefault());

	protected AlignDeclarationSectionRuleBase(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		boolean alignAcrossEmptyLines = configAlignAcrossEmptyLines.getValue();
		boolean alignAcrossCommentLines = configAlignAcrossCommentLines.getValue();
		int numberOfPasses = getNumberOfPasses();
		
		// do one or two passes
		for (int pass = 0; pass < numberOfPasses; ++pass) {
			Command command = code.firstCommand;
			
			while (command != null) {
				commandForErrorMsg = command;
				if (isCommandBlocked(command)) {
					command = command.getNext();
					continue;
				}
				if (!isMatchForFirstCommand(command, pass)) {
					command = command.getNext();
					continue;
				}
	
				// move behind the sequence of declaration lines with the same keyword
				Command startCommand = command;
				String keywordOfFirstCommand = startCommand.getFirstToken().getText();
				boolean startCommandIsChain = startCommand.isSimpleChain();
				int commandCount = 0;
				boolean startCommandOpensBlock = (startCommand.getBlockLevelDiff() > 0);
				if (startCommandIsChain && !startCommandOpensBlock) {
					// chain declarations are treated as one sequence (except when they open a BEGIN OF block)
					commandCount = 1;
					command = command.getNext();
				} else {
					boolean commandBelongsToBlock = false;
					int blockLevel = command.getBlockLevelDiff();
					do {
						++commandCount;
	
						command = command.getNext();
						if (command == null)
							break;
						if (isCommandBlocked(command))
							break;
						
						// if a BEGIN OF block is started, process the Command sequence up to this point first  
						int curBlockLevelDiff = command.getBlockLevelDiff();
						if (blockLevel == 0 && curBlockLevelDiff > 0)
							break;
						blockLevel += curBlockLevelDiff;
	
						commandBelongsToBlock = false;
						if (blockLevel > 0) {
							// inside BEGIN OF blocks, always continue until the block is closed with END OF (irrespective of empty lines or comment lines)
							commandBelongsToBlock = true;
						} else if (blockLevel == 0 && curBlockLevelDiff < 0) {
							// when the outermost BEGIN OF block is closed, include this Command, but do not continue any further
							++commandCount;
							command = command.getNext();
							break;
						} else {
							if (!alignAcrossEmptyLines && command.getFirstTokenLineBreaks() > 1)
								break;
							if (!alignAcrossCommentLines && command.getFirstToken().isCommentLine())
								break;
						}
					} while (skipCommand(command) || commandBelongsToBlock 
							|| isMatchForFurtherCommand(command, keywordOfFirstCommand, pass));
				}
	
				if (startCommandIsChain || commandCount > 0) {
					try {
						executeOn(code, startCommand, command, startCommandIsChain, pass);
					} catch (UnexpectedSyntaxBeforeChanges ex) {
						// log and continue with next Command
						ex.addToLog();
					}
				}
			}
		}
	}
	
	protected boolean skipCommand(Command command) {
		return command.isCommentLine() || command.isPeriodOnly();
	}

}
