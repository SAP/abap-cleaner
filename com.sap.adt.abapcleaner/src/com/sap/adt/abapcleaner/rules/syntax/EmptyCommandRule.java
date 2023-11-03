package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class EmptyCommandRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.EMPTY_COMMAND; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Remove empty commands"; }

	@Override
	public String getDescription() { return "Removes commands that only consist of . or : , . but keeps comments which these commands may contain."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 5, 1); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD remove_empty_commands." 
			+ LINE_SEP + "    \" although this method contains syntactically correct ABAP code," 
			+ LINE_SEP + "    \" we hope you'll never see anything like this in real life!" 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_primary TYPE i..." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_primary = 2." 
			+ LINE_SEP + "    . . ." 
			+ LINE_SEP + "    lv_primary = 3." 
			+ LINE_SEP 
			+ LINE_SEP + "    . lv_primary = 5." 
			+ LINE_SEP + "    .. lv_primary = 7.." 
			+ LINE_SEP + "    :,. lv_primary = 11." 
			+ LINE_SEP 
			+ LINE_SEP + "    ::::. lv_primary = 13. ::::." // compilable syntax error 
			+ LINE_SEP + "    .:.:,.:,.lv_primary = 17.:.:,.:,." 
			+ LINE_SEP + "    ... lv_primary = 19... lv_primary = 23... lv_primary = 29..." 
			+ LINE_SEP 
			+ LINE_SEP + "    : \" comment 1" 
			+ LINE_SEP + "*   comment 2" 
			+ LINE_SEP + "    , \" comment 3" 
			+ LINE_SEP + "    . \" comment 4" 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public EmptyCommandRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (command.isCommentLine())
			return false;

		Token token = command.getFirstToken();
		while (token != null) {
			if (!token.isChainColon() && !token.isCommaOrPeriod())
				return false;
			token = token.getNextNonCommentToken();
		}
		
		// if the next Command continues the line of this Command, 
		// then copy line breaks from this Command (if any are found) to the first token of the next Command
		if (command.getNext() != null && command.getNext().getFirstTokenLineBreaks() == 0) {
			Token firstFollowing = command.getNext().getFirstToken();
			Token last = command.getLastToken(); // this can't be a comment
			while (last != null && !last.isComment()) {
				if (last.lineBreaks > firstFollowing.lineBreaks)
					firstFollowing.copyWhitespaceFrom(last);
				last = last.getPrev();
			}
		}
		
		// if the first Token of this Command continues the line of the previous Command and is followed by a line-end comment, 
		// then attach that comment to the previous Command 
		Token firstToken = command.getFirstToken();
		Token firstComment = firstToken.getNext();
		if (firstComment != null && firstComment.isCommentAfterCode() && firstToken.lineBreaks == 0) {
			Command prevCommand = command.getPrev();
			if (prevCommand != null && !prevCommand.getLastToken().isComment()) {
				int newLineBreaks = firstToken.lineBreaks;
				int newSpacesLeft = firstToken.spacesLeft;
				firstComment.removeFromCommand();
				firstToken.setWhitespace(1, command.getIndent());
				firstComment.setWhitespace(newLineBreaks, newSpacesLeft);
				firstComment.ensureWhitespace(); // at least one space
				prevCommand.getLastToken().insertRightSibling(firstComment);
			}
		}

		// split out all other comments from this Command into new Commands 
		token = command.getFirstToken(); // the first token must be a non-comment
		while (token != null) {
			if (token.getNext() != null && token.getNext().isComment())
				command.splitOutCommentLinesAfter(token);
			token = token.getNextNonCommentToken();
		}
		
		// delete the remaining Command, which now only consists of : , .
		try {
			command.removeFromCode();
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxAfterChanges(this, ex);
		}

		return true;
	}
}
