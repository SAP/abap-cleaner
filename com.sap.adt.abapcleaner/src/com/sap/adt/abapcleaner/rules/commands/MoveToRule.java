package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class MoveToRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs"), 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid obsolete language elements", "#avoid-obsolete-language-elements"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Obsolete Assignments: MOVE", "abapmove_obs.htm") };

	@Override
	public RuleID getID() { return RuleID.MOVE_TO; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace obsolete MOVE ... TO with ="; }

	@Override
	public String getDescription() {
		return "Replaces obsolete MOVE ... TO and MOVE ... ?TO statements with the more general assignment operators = and ?=.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 2, 19); }

	@Override
	public RuleReference[] getReferences() { return references; }

	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_ASSIGNMENTS }; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_obsolete_move_to." 
			+ LINE_SEP + "    MOVE 1 TO ev_result." 
			+ LINE_SEP + "    MOVE 'ab' TO es_result-text." 
			+ LINE_SEP + "    MOVE '12' TO ev_start+4(2)."
			+ LINE_SEP + ""
			+ LINE_SEP + "    MOVE get_next_date( iv_year   = iv_year" 
			+ LINE_SEP + "                        iv_period = iv_period ) TO ev_date." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    MOVE lo_source ?TO lo_dest."
			+ LINE_SEP + ""
			+ LINE_SEP + "    MOVE EXACT source TO dest."
			+ LINE_SEP + ""
			+ LINE_SEP + "    MOVE:"
			+ LINE_SEP + "      \" some comment"
			+ LINE_SEP + "      1 TO ev_value,"
			+ LINE_SEP + "      '2023' TO ev_start(4),"
			+ LINE_SEP + ""
			+ LINE_SEP + "      \" another comment"
			+ LINE_SEP + "      EXACT iv_data TO ev_data,"
			+ LINE_SEP + "      io_instance ?TO eo_instance."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" with the MOVE chain, get_next_value( ) is also called 3 times,"
			+ LINE_SEP + "    \" but without the chain, that's much clearer:"
			+ LINE_SEP + "    MOVE get_next_value( ) TO: ev_any, ev_other, ev_third." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain MOVE: chains (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));

	private final ConfigValue[] configValues = new ConfigBoolValue[] { configProcessChains };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public MoveToRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.firstCodeTokenIsKeyword("MOVE"))
			return false;
		
		ArrayList<Command> unchainedCommands = unchain(code, command, configProcessChains.getValue());
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return false;
		
		for (Command unchainedCommand : unchainedCommands) {
   		if (executeOnNonChain(code, unchainedCommand)) {
   			code.addRuleUse(this, unchainedCommand);
   		}
   	} 
		return false; // addRuleUse() was already called above
	}
	
	private boolean executeOnNonChain(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// for the syntax of the obsolete MOVE ... TO statement, see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmove_obs.htm
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(true, "MOVE", TokenSearch.makeOptional("EXACT"), TokenSearch.ANY_TERM, "TO|?TO", TokenSearch.ANY_IDENTIFIER, "."))
			return false;

		// determine involved tokens: MOVE, EXACT, (term), TO or ?TO, (destination variable)
		Token moveToken = firstToken;
		Token termStart = moveToken.getNextCodeToken();

		Token exactToken = null;
		if (termStart.isKeyword("EXACT")) {
			exactToken = termStart;
			termStart = exactToken.getNextCodeToken();
		} 

		Term term;
		try {
			term = Term.createSimple(termStart);
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		int termPos = term.firstToken.getStartIndexInLine();
		
		Token toToken = term.getNextCodeToken();
		Token destVariable = toToken.getNextCodeToken();

		// insert the destination variable and the assignment operator ("=" or "?=") before "MOVE"
		destVariable.removeFromCommand();
		destVariable.copyWhitespaceFrom(moveToken);
		moveToken.insertLeftSibling(destVariable);
		moveToken.setWhitespace();
		
		String assignmentOp = toToken.textEquals("?TO") ? "?=" : "=";
		moveToken.insertLeftSibling(Token.createForAbap(0, 1, assignmentOp, TokenType.ASSIGNMENT_OP, moveToken.sourceLineNum));

		// if applicable, transform "EXACT source" into the lossless operator "EXACT #( source )"
		if (exactToken != null) {
			term.firstToken.insertParenthesesUpTo(toToken, "#(", ")");
		}

		// remove "MOVE" and "TO"/"?TO" (but keep "EXACT", if applicable) 
		moveToken.removeFromCommand();
		toToken.removeFromCommand();

		term.addIndent(term.firstToken.getStartIndexInLine() - termPos);

		command.invalidateMemoryAccessType();
		return true;
	}
}
