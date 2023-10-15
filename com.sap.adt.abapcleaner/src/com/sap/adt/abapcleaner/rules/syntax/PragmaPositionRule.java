package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class PragmaPositionRule extends Rule {
   private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Pragmas", "abenpragma.htm") };

	@Override
	public RuleID getID() { return RuleID.PRAGMA_POSITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Move pragmas to correct position"; }

	@Override
	public String getDescription() { return "Moves pragmas to the correct position at the end of the line, but before the period or comma. Pragmas at line start are correctly positioned but can optionally be moved, too."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 22); }

	@Override
	public RuleReference[] getReferences() { return references; }

	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.INSET,
																					RuleID.ALIGN_METHODS_DECLARATION,
																					RuleID.ALIGN_METHODS_FOR_TESTING,
																					RuleID.ALIGN_METHODS_REDEFINITION,
																					RuleID.ALIGN_ALIASES_FOR,
																					RuleID.ALIGN_DECLARATIONS,
																					RuleID.ALIGN_ASSIGNMENTS,
																					RuleID.ALIGN_WITH_SECOND_WORD,
																					RuleID.ALIGN_PARAMETERS, 
																					RuleID.ALIGN_LOGICAL_EXPRESSIONS,
																					RuleID.ALIGN_COND_EXPRESSIONS }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD move_pragmas_to_correct_pos." 
			+ LINE_SEP + "    \" pragmas at line end must be placed before the comma or period;" 
			+ LINE_SEP + "    \" they are also correct at line start, but you may prefer them at line end" 
			+ LINE_SEP + "    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. ##NO_TEXT" 
			+ LINE_SEP + "    ##NO_TEXT CONSTANTS lc_other_key TYPE ty_key VALUE 'def'." 
			+ LINE_SEP
			+ LINE_SEP + "    DATA: a ##NEEDED TYPE string, \" comment" 
			+ LINE_SEP + "          b TYPE string. ##NEEDED" 
			+ LINE_SEP
			+ LINE_SEP + "    \" a pragma before the chain colon is effective for all parts of the chain;" 
			+ LINE_SEP + "    \" however, it cannot be moved if the line continues behind the colon" 
			+ LINE_SEP + "    ##NEEDED DATA:" 
			+ LINE_SEP + "          c TYPE string," 
			+ LINE_SEP + "          d TYPE string." 
			+ LINE_SEP + "    ##NEEDED DATA: e TYPE string," 
			+ LINE_SEP + "          f TYPE string." 
			+ LINE_SEP
			+ LINE_SEP + "    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. ##ENH_OK" 
			+ LINE_SEP
			+ LINE_SEP + "    \" unlike pseudo comments, ##NEEDED should not be placed inside the empty block:"
			+ LINE_SEP + "    DO 5 TIMES."
			+ LINE_SEP + "      ##NEEDED" 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP
			+ LINE_SEP + "    \" the same is true for ##NO_HANDLER, which belongs to the CATCH statement:"
			+ LINE_SEP + "    TRY." 
			+ LINE_SEP + "      GET BADI lo_any_badi." 
			+ LINE_SEP + "    CATCH cx_badi_not_implemented. \" comment" 
			+ LINE_SEP + "      ##NO_HANDLER" 
			+ LINE_SEP + "    ENDTRY." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	// assuming that a pragma at line start was deliberately put to that position, this option is inactive by default 
	final ConfigBoolValue configMovePragmaFromLineStartToEnd = new ConfigBoolValue(this, "MovePragmaFromLineStartToEnd", "Move (correctly positioned) pragmas from line start to line end", false);

	private final ConfigValue[] configValues = new ConfigBoolValue[] { configMovePragmaFromLineStartToEnd };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public PragmaPositionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	public static boolean pragmaBelongsToPrevCommand(String pragmaName, Command command) {
		Command prevCommand = command.getPrevNonCommentCommand();
		if (prevCommand == null)
			return false;
		Command nextCommand = command.getNextNonCommentCommand();
		if (nextCommand != prevCommand.getNextSibling())
			return false;

		return (pragmaName.equals("NO_HANDLER") && prevCommand.firstCodeTokenIsKeyword("CATCH") 
				|| pragmaName.equals("NEEDED") && prevCommand.firstCodeTokenIsAnyKeyword("DO", "LOOP", "SELECT", "TRY", "WHILE", "FORM", "FUNCTION", "METHOD", "MODULE")
				|| pragmaName.equals("NEEDED") && nextCommand.firstCodeTokenIsKeyword("ENDIF"));
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

			if (!isCommandBlocked(command)) {
				Token token = command.getFirstToken();
				while (token != null) {
					// always remember the next Token now, because the rule may move the current Token; 
					// since Tokens are moved to the correct position, it does not harm if they are processed again later
					Token nextToken = token.getNext();
					if (executeOn(code, command, token, releaseRestriction))
						code.addRuleUse(this, command);
					if (command.wasRemovedFromCode())
						break;
					token = nextToken;
				}
			}

			command = command.wasRemovedFromCode() ? nextCommand : command.getNext();
		}
	}

	private boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException  {
		if (!token.isPragma())
			return false;

		// return if the pragma is already correctly positioned
		if (command.isPragmaLine()) {
			// move the pragma only
			// - if its Command continues the line of the previous Command, e.g. 'DATA a TYPE i. ##NEEDED',
			//   where ##NEEDED is a Command of its own
			// - in special cases of pragmas in empty blocks (CATCH, DO, LOOP, TRY, ...),
			//   where it is clear that it belongs to the previous Command
			String pragmaText = StringUtil.removePrefix(token.getText(), ABAP.PRAGMA_SIGN, false);
			String pragmaName = ABAP.getPragmaWithoutParameters(pragmaText);
			if (command.getFirstTokenLineBreaks() == 0 || pragmaBelongsToPrevCommand(pragmaName, command)) {
				return moveToPrevCommand(code, command, token);
			} else {
				return false;
			}
			
		} else if (token.isFirstTokenInLine()) {
			// the pragma position is correct; move it only if configuration demands to move pragmas at line start, too
			if (configMovePragmaFromLineStartToEnd.getValue() && !token.isLastTokenInLineExceptComment()) {
				return moveToLineEnd(token);
			} else {
				return false;
			}
			
		} else if (token.isLastTokenInLineExceptComment()) {
			// move the the pragma if there is a comma or period before the pragma
			if (token.getPrev().isCommaOrPeriod()) {
				return moveBeforePrevToken(token);
			} else {
				return false;
			}
			
		} else {
			// move the pragma, unless the line only continues with more pragmas and a comma/period/colon (and potentially a comment)  
			Token next = token.getNext();
			// move behind the last pragma in a possible sequence of pragmas on the same line  
			while (next.isPragma()) {
				next = next.getNext();
				if (next == null || next.lineBreaks > 0)
					return false;
			}
			if ((next.isCommaOrPeriod() || next.isChainColon()) && next.isLastTokenInLineExceptComment()) {
				return false;
			} else if (next.isCommentAfterCode()) {
				return false;
			} else {
				return moveToLineEnd(token);
			}
		}
	}

	private boolean moveToPrevCommand(Code code, Command command, Token token) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		// move the pragma to the end of the previous Command
		Command prevCommand = command.getPrevNonCommentCommand();
		Token insertBefore = prevCommand.getLastNonCommentToken();
		if (token.isOnlyTokenInCommand()) {
			try {
				command.removeFromCode();
			} catch(UnexpectedSyntaxException ex) {
				throw new UnexpectedSyntaxAfterChanges(this,  ex);
			}
		} else {
			token.removeFromCommand(false, true);
			if (token.getNext() != null) {
				token.getNext().copyWhitespaceFrom(token);
			}
		}
		token.setWhitespace();
		insertBefore.insertLeftSibling(token);
		code.addRuleUse(this, prevCommand);
		
		// if the pragma continued the line of the previous Command, and a comment is now left in the original Command, 
		// then move that comment as a line-end comment to the previous Command, too. Example:
		//   DATA a TYPE string. ##NEEDED " comment
		// is merged into one single Command:
		//   DATA a TYPE string ##NEEDED. " comment
		if (!command.wasRemovedFromCode() && insertBefore.isLastTokenInCommand()) {
			Token newFirstToken = command.getFirstToken();
			if (newFirstToken.isOnlyTokenInCommand() && newFirstToken.isComment() && newFirstToken.lineBreaks == 0) {
				try {
					command.removeFromCode();
				} catch(UnexpectedSyntaxException ex) {
					throw new UnexpectedSyntaxAfterChanges(this,  ex);
				}
				insertBefore.insertRightSibling(newFirstToken);
			}
		}
		return true;
	}
	
	private boolean moveToLineEnd(Token token) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		Token insertAfter = findLeftSiblingOfTargetPos(token);
		if (insertAfter == null)
			return false;
		token.removeFromCommand();
		token.setWhitespace();
		insertAfter.insertNext(token);
		return true;
	}

	private boolean moveBeforePrevToken(Token token) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// move the pragma before the comma or period
		Token insertBefore = token.getPrev();
		token.removeFromCommand();
		if (insertBefore.lineBreaks == 0) {
			token.setWhitespace();
		} else {
			token.copyWhitespaceFrom(insertBefore);
			insertBefore.setWhitespace(0, 0);
		}
		insertBefore.insertLeftSibling(token);
		return true;
	}

	/**
	 * returns the left sibling of the correct target position for the pragma at the end of the same line, 
	 * or null if there is no correct position 
	 * @param pragma
	 * @return
	 */
	private Token findLeftSiblingOfTargetPos(Token pragma) {
		Token token = pragma;
		// move to the last Token in the line
		while (token.getNext() != null && token.getNext().lineBreaks == 0) {
			token = token.getNext();
			// do NOT move the pragma behind a chain colon, because a pragma before the colon is valid for all chained commands;
			// however, continue if the chain colon is at line end, because then the pragma will end up before the colon 
			if ((token.isChainColon() || token.isComma()) && !token.isLastTokenInLineExceptComment())
				return null;
		}
		
		// move back to the left sibling of the target position for the pragma
		if (token.isCommentAfterCode())
			token = token.getPrev();
		if (token.isCommaOrPeriod() || token.isChainColon())
			token = token.getPrev();
		
		// return null if the pragma is already in the correct position
		return (token == pragma) ? null : token;
	}
}