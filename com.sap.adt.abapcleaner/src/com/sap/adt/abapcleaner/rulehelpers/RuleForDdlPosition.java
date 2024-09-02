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

public abstract class RuleForDdlPosition extends Rule {
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
	
	protected boolean breakBefore(Token token, DdlLineBreak lineBreak, boolean emptyLineIfBreaking, int indent) {
		if (lineBreak == DdlLineBreak.KEEP_AS_IS)
			return false;

		if (lineBreak == DdlLineBreak.ALWAYS) {
			// move the supplied Token (or the attached non-annotation comments above it) to the next line
			int lineBreaks = emptyLineIfBreaking ? 2 : 1;
			lineBreaks = Math.max(token.lineBreaks, lineBreaks);
			setWhitespaceInclAttachedComments(token, lineBreaks, indent, false);
			return false; // .addRuleUse() was already called above
		}
		
		if (lineBreak == DdlLineBreak.NEVER) {
			Token prev = token.getPrev();
			if (prev != null && !prev.isComment()) { 
				return token.setWhitespace();
			}
		}

		return false;
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

	protected void setWhitespaceInclAttachedComments(Token token, int lineBreaks, int indent, boolean includeDdlAnnoComments) {
		ArrayList<Command> changedCommands = token.setWhitespaceInclAttachedComments(lineBreaks, indent, includeDdlAnnoComments);
		if (changedCommands != null) {
			for (Command changedCommand : changedCommands) {
				changedCommand.getParentCode().addRuleUse(this, changedCommand);
			}
		}
	}
}
