package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.declarations.ChainOfOneRule;
import com.sap.adt.abapcleaner.rules.declarations.ChainRule;

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
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Process MOVE: chains", true, false, LocalDate.of(2023, 6, 5));

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
		
	   if (!command.containsChainColon()) {
	   	return executeOnNonChain(code, command);
	   
	   } else if (configProcessChains.getValue() && command.containsChainColon()) {
	   	// check whether all parts of the chain match the expected pattern
	   	boolean isChainOfOne = true;
	   	if (command.isSimpleChain()) {
	   		// expect "MOVE: [EXACT] <term1> TO <identifier1>, [EXACT] <term2> TO <identifier2>, ..."
		   	Token checkToken = command.getFirstToken().getNextCodeSibling().getNextCodeSibling();
		   	while (checkToken != null) {
		   		Token lastToken = checkToken.getLastTokenOnSiblings(true, TokenSearch.makeOptional("EXACT"), TokenSearch.ANY_TERM, "TO|?TO", TokenSearch.ANY_IDENTIFIER, ".|,");
		   		if (lastToken == null || !lastToken.isCommaOrPeriod())
		   			return false;
		   		if (lastToken.isComma())
		   			isChainOfOne = false;
		   		checkToken = lastToken.getNextCodeSibling();
		   	}
	   	} else {
	   		// expect "MOVE [EXACT] <term1> TO: <identifier1>, <identifier2>, ...."
	   		Token chainColon = command.getFirstToken().getLastTokenOnSiblings(true, "MOVE", TokenSearch.makeOptional("EXACT"), TokenSearch.ANY_TERM, "TO|?TO", ":");
	   		if (chainColon == null)
	   			return false;
	   		Token checkToken = chainColon;
	   		while (checkToken != null) {
	   			checkToken = checkToken.getNextCodeSibling();
	   			if (checkToken == null)
	   				break;
	   			if (!checkToken.isIdentifier())
	   				return false;
	   			checkToken = checkToken.getNextCodeSibling();
	   			if (checkToken == null || !checkToken.isCommaOrPeriod())
	   				return false;
	   			if (checkToken.isComma())
	   				isChainOfOne = false;
	   		}
	   	}
	   	
	   	// unchain MOVE: into multiple commands
	   	Command prevCommand = command.getPrev();
	   	Command endCommand = command.getNext();
	   	boolean unchained;
	   	if (isChainOfOne) {
	   		unchained = ((ChainOfOneRule)parentProfile.getRule(RuleID.CHAIN_OF_ONE)).executeOn(code, command, false);
	   	} else {
		   	unchained = ((ChainRule)parentProfile.getRule(RuleID.DECLARATION_CHAIN)).executeOn(code, command, false);
	   	}
	   	if (!unchained)
	   		return false;

	   	// process unchained MOVE commands
	   	Command changeCommand = (prevCommand == null) ? code.firstCommand : prevCommand.getNext();
	   	while (changeCommand != endCommand) {
	   		if (changeCommand.isCommentLine() || executeOnNonChain(code, changeCommand)) {
	   			code.addRuleUse(this, changeCommand);
	   		}
	   		changeCommand = changeCommand.getNext();
	   	} 
	   	return true;

	   } else {
	   	return false;
	   }
	}
	
	private boolean executeOnNonChain(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// for the syntax of the obsolete MOVE ... TO statement, see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmove_obs.htm
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(true, "MOVE", TokenSearch.makeOptional("EXACT"), TokenSearch.ANY_TERM, "TO|?TO", TokenSearch.ANY_IDENTIFIER, "."))
			return false;

		// determine involved tokens: MOVE, EXACT, (term), TO or ?TO, (destination variable)
		Token moveToken = firstToken;
		Token termStart = moveToken.getNext();

		Token exactToken = null;
		if (termStart.isKeyword("EXACT")) {
			exactToken = termStart;
			termStart = exactToken.getNext();
		} 

		Term term;
		try {
			term = Term.createSimple(termStart);
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		int termPos = term.firstToken.getStartIndexInLine();
		
		Token toToken = term.getNext();
		Token destVariable = toToken.getNext();

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
