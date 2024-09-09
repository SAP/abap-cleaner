package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlLineBreak;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlLineBreakWithoutNever;

public abstract class RuleForDdlPosition extends Rule {
	protected final int MAX_INDENT = 80;
	protected final String[] lineBreakSelection = new String[] { "Always", "Keep as is", "Never" };
	protected final String[] lineBreakSelectionWithoutNever = new String[] { "Always", "Keep as is" };

	protected void prepare(Code code) { }
	protected abstract boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException;

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_POSITION; }

	@Override
	public Language[] getSupportedLanguages() { return ddlOnly; }
	
	protected RuleForDdlPosition(Profile profile) {
		super(profile);
	}

	@Override
	protected void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// DdlPositionJoinRule needs to determine in advance whether the view contains multi-line ON conditions
		prepare(code);
		
		Command command = code.firstCommand;

		while (command != null) {
			if (command.isDdl() && !command.isCommentLine() && !isCommandBlocked(command)) {
				try {
					if (executeOn(code, command)) {
						code.addRuleUse(this, command);
					}
				} catch (UnexpectedSyntaxBeforeChanges e) {
				}
			}			
			command = command.getNext();
		}
	}
	
	protected DdlLineBreak getLineBreak(DdlLineBreakWithoutNever keywordsLineBreak) {
		switch (keywordsLineBreak) {
			case ALWAYS:
				return DdlLineBreak.ALWAYS;
			case KEEP_AS_IS:
				return DdlLineBreak.KEEP_AS_IS;
			default: // pro forma
				return DdlLineBreak.ALWAYS;
		}
	}

	protected boolean breakBefore(Token token, DdlLineBreak lineBreak, boolean emptyLineIfBreaking, int indent) {
		if (lineBreak == DdlLineBreak.ALWAYS) {
			// continue below
			
		} else if (lineBreak == DdlLineBreak.KEEP_AS_IS) {
			if (token.lineBreaks == 0) {
				return false;
			} // otherwise continue below to apply the indent
			
		} else if (lineBreak == DdlLineBreak.NEVER) {
			Token prev = token.getPrev();
			if (prev == null) { // e.g. before the opening or closing brace of the select list, which is a distinct Command
				Command prevCommand = token.getParentCommand().getPrev();
				if (prevCommand != null) { // pro forma
					prev = prevCommand.getLastToken(); 
				}
			}
			if (prev == null) { // pro forma
				return false;
			} else if (!prev.isComment()) {
				return token.setWhitespace();
			} // otherwise continue below
		}

		// adjust the indent of the supplied Token (or the attached non-annotation comments above it) 
		int lineBreaksMin = emptyLineIfBreaking ? 2 : 1;
		if (token.getPrev() == null && token.getParentCommand().isFirstCommandInCode()) // e.g. for DEFINE
			lineBreaksMin = 0;
		int lineBreaksMax = 2;
		setWhitespaceInclAttachedComments(token, lineBreaksMin, lineBreaksMax, indent, false);
		return false; // .addRuleUse() was already called above for all involved commands
	}
	
	protected boolean condense(Token start, Token last) {
		int indent = start.getFirstTokenInLine().spacesLeft;
		return start.condenseUpTo(last, DDL.MAX_LINE_LENGTH, indent, true);
	}

	protected Command setNewIndent(Command command, int indent) {
		Code code = command.getParentCode();
		
		// ensure a line break before the first Token
		Token firstToken = command.getFirstToken();
		int addIndent = indent - firstToken.spacesLeft;
		if (firstToken.lineBreaks == 0) {
			int startIndex = firstToken.getStartIndexInLine();
			addIndent = indent - startIndex;
			firstToken.setWhitespace(1, startIndex);
			code.addRuleUse(this, command);
		}

		boolean isMultiLineComment = command.startsMultiLineDdlComment();
		Command changeCommand = command;
		do {
			// move the entire Command (which may e.g. be a multi-line annotation)
			if (changeCommand.addIndent(addIndent, 0))
				changeCommand.getParentCode().addRuleUse(this, changeCommand);

			// continue for multi-line comments, using the same addIndent value for all lines
			if (!isMultiLineComment || changeCommand.endsMultiLineDdlComment()) 
				break;
			changeCommand = changeCommand.getNext();
		} while (changeCommand != null);

		// return the last Command of a multi-line comment (or the Command that was supplied)
		return changeCommand;
	}

	protected void setWhitespaceInclAttachedComments(Token token, int lineBreaksMin, int lineBreaksMax, int indent, boolean includeDdlAnnoComments) {
		ArrayList<Command> changedCommands = token.setWhitespaceInclAttachedComments(lineBreaksMin, lineBreaksMax, indent, includeDdlAnnoComments);
		if (changedCommands != null) {
			for (Command changedCommand : changedCommands) {
				changedCommand.getParentCode().addRuleUse(this, changedCommand);
			}
		}
	}
}
