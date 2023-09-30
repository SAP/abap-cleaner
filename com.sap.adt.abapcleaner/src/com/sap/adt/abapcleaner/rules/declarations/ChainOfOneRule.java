package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;

public class ChainOfOneRule extends RuleForCommands {
	// for the structure of DATA statements, see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapdata.htm

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.CHAIN_OF_ONE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Simplify a chain with one element"; }

	@Override
	public String getDescription() { return "Simplifies chains that consist of one element only by removing the : sign."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_DECLARATIONS } ; }

   @Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS simplify_chain_of_one DEFINITION." 
 			+ LINE_SEP + "  PUBLIC SECTION." 
 			+ LINE_SEP + "    CONSTANTS: any_constant TYPE i VALUE 42."
 			+ LINE_SEP + "    METHODS:" 
 			+ LINE_SEP + "      simplify_chain_of_one." 
 			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP + "" 
 			+ LINE_SEP + "CLASS simplify_chain_of_one IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD simplify_chain_of_one." 
			+ LINE_SEP + "    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA: \" comment on declaration" 
			+ LINE_SEP + "      lo_item TYPE REF TO cl_item ##NEEDED." 
			+ LINE_SEP + "    DATA: lth_hash_table TYPE ty_th_hash_table. " 
			+ LINE_SEP 
			+ LINE_SEP + "    FIELD-SYMBOLS: <ls_field_symbol> LIKE LINE OF its_table." 
			+ LINE_SEP + "    FIELD-SYMBOLS:" 
			+ LINE_SEP + "         <ls_data> TYPE ty_any_data." 
			+ LINE_SEP 
			+ LINE_SEP + "    CHECK: its_table IS NOT INITIAL." 
			+ LINE_SEP + "    CLEAR: ev_result." 
			+ LINE_SEP + "    ev_result := 1." 
			+ LINE_SEP 
			+ LINE_SEP + "    CALL METHOD : \" comment on method call" 
			+ LINE_SEP + "      any_method( )." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS."; 
   }

   final ConfigBoolValue configExecuteOnClassDefinitionSections = new ConfigBoolValue(this, "ExecuteOnClassDefinitionSections", "Execute on declarations in CLASS ... DEFINITION sections", true, false, LocalDate.of(2022, 4, 9));
   final ConfigBoolValue configExecuteOnLocalDeclarations = new ConfigBoolValue(this, "ExecuteOnLocalDeclarations", "Execute on declarations in methods etc.", true);
   final ConfigBoolValue configExecuteOnNonDeclarations = new ConfigBoolValue(this, "ExecuteOnNonDeclarations", "Execute on non-declaration commands", true, false, LocalDate.of(2022, 4, 9));

   private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnClassDefinitionSections, configExecuteOnLocalDeclarations, configExecuteOnNonDeclarations };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ChainOfOneRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override 
	protected boolean skipDeclarationsInsideBeginOf() { 
		// skip this Command if it is a declaration inside a BEGIN OF block which was opened by a previous Command
		return true; 
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!command.containsChainColon())
			return false;

		// ensure the rule is configured to be executed on this command
		boolean execute;
		if (command.isInClassDefinition()) {
			execute = command.isDeclaration() || command.isDeclarationInclude() || command.isDeclarationInClassDef() ? configExecuteOnClassDefinitionSections.getValue() : false;
		} else {
			execute = command.isDeclaration() || command.isDeclarationInclude() ? configExecuteOnLocalDeclarations.getValue() : configExecuteOnNonDeclarations.getValue();
		}
		if (execute) {
			return executeOn(code, command, true);
		} else { 
			return false;
		}
	}
	
	public boolean executeOn(Code code, Command command, boolean addRuleUse) throws UnexpectedSyntaxAfterChanges {
		// ensure this is a "chain of one", i.e. there is no comma in the command; 
		// we expect the chain sign to be top-level (i.e. not inside parentheses etc.); this is ensured by Token.addNext()
		Token chainSign = command.getFirstToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ABAP.COLON_SIGN_STRING);
		if (chainSign == null || chainSign == command.getFirstToken())
			return false;
		Token token = chainSign.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ABAP.COMMA_SIGN_STRING);
		if (token != null)
			return false;

		// exclude the special case "TYPES: END OF ..." after an "INCLUDE TYPE ..." line, because removing the colon here 
		// would cause poor layout the next time PrettyPrinter is called - PrettyPrinter will produce:
		//    TYPES: BEGIN OF ty_s_struc,
		//             comp TYPE i.
		//             INCLUDE TYPE any_include.
		//  TYPES  END OF ty_s_struc.
		if (command.getFirstToken().matchesOnSiblings(true,  "TYPES", ":", "END", "OF")) {
			Command prevNonComment = command.getPrevNonCommentCommand();
			if (prevNonComment != null && prevNonComment.firstCodeTokenIsKeyword("INCLUDE"))
				return false;
		}

		// exclude the special case "TYPES: BEGIN OF ..." directly followed by an "INCLUDE TYPE ..." line, because removing 
		// the colon here would cause poor layout the next time PrettyPrinter is called - PrettyPrinter would produce:
	   //  TYPES BEGIN OF ty_s_struc.
		//  INCLUDE TYPE any_include.
		//  TYPES: comp TYPE i,
		//         END OF ty_s_struc.
		if (command.getFirstToken().matchesOnSiblings(true,  "TYPES", ":", "BEGIN", "OF")) {
			Command nextNonComment = command.getNextNonCommentCommand();
			if (nextNonComment != null && nextNonComment.firstCodeTokenIsKeyword("INCLUDE"))
				return false;
		}

		// remove the (first) colon
		Token firstToken = command.getFirstToken();
		Token prevNonComment = chainSign.getPrevNonCommentToken();
		Token nextToken = chainSign.getNext();
		Token nextNonComment = chainSign.getNextNonCommentToken();
		int oldIndentOfNextNonComment = nextNonComment.getStartIndexInLine();
		boolean wasColonAfterInitialKeywords = firstToken.isKeyword() && (prevNonComment == firstToken.getLastTokenOfKeywordCollocation()); 

		chainSign.removeFromCommand();

		// if the chain colon was directly after the first keyword (typically, a declaration keyword) or keyword collocation (like CALL METHOD) ...  
		if (wasColonAfterInitialKeywords) {
			// move comment lines (or an initial line-end comment behind the keyword or the chain sign) into a new command
			ArrayList<Command> newCommentCommands = command.splitOutCommentLinesAfter(prevNonComment); 
			if (!newCommentCommands.isEmpty() && addRuleUse) 
				code.addRuleUses(this, newCommentCommands);
			// remove the line break before the next token
			if (nextNonComment.lineBreaks > 0) // even if the next Token is "BEGIN OF", because a single "TYPES: BEGIN OF." command will be followed by more "TYPES ..." commands 
				nextNonComment.setWhitespace();
			// ensure there is at least one space separating the next Token from the previous one (e.g. "DATA:identifier")
			if (nextNonComment.lineBreaks == 0 && nextNonComment.spacesLeft == 0)
				nextNonComment.spacesLeft = 1;
		} else {
			if (nextToken.lineBreaks == 0 && nextToken.spacesLeft == 0)
				nextToken.spacesLeft = 1;
		}

		// adjust following lines 
		command.addIndent(nextNonComment.getStartIndexInLine() - oldIndentOfNextNonComment, oldIndentOfNextNonComment, nextNonComment);

		// remove all further colons from the command (see comment inside the method) 
		command.removeAllFurtherChainColons();

		return true;
	}
}