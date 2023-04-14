package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignDeclarationsRule extends AlignDeclarationSectionRuleBase {
   // for the structure of DATA statements, see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapdata.htm

   private enum Columns  {
      KEYWORD,
      IDENTIFIER,
      TYPE,
      LENGTH,
      DECIMALS,
      VALUE,
      READ_ONLY,
      LINE_END_COMMENT;

		public int getValue() { return this.ordinal(); }
	}

	private static final int MAX_COLUMN_COUNT = 8;

	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_CLEANER),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't align type clauses", "#dont-align-type-clauses", true) };

   private class TableStart {
   	public final int firstLineBreaks;
   	public final int basicIndent;
   	
   	private TableStart(int firstLineBreaks, int basicIndent) {
   		this.firstLineBreaks = firstLineBreaks;
   		this.basicIndent = basicIndent;
   	}
   }
   
   private class Layout {
   	public int additionalIndent;
   }
   
	@Override
	public RuleID getID() { return RuleID.ALIGN_DECLARATIONS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align declarations"; }

	@Override
	public String getDescription() { return "Aligns both chains and consecutive declaration lines of CONSTANTS, DATA, FIELD-SYMBOLS, and TYPES."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 8); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
      return  LINE_SEP + "  METHOD align_declarations." 
				+ LINE_SEP + "    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200," 
				+ LINE_SEP + "      lc_other_price TYPE ty_price VALUE 1000," 
				+ LINE_SEP + "               lc_num_contract_change TYPE ty_sequence_number VALUE    1," 
				+ LINE_SEP + "          lc_start_date TYPE ty_start_date VALUE '20220312'." 
				+ LINE_SEP 
				+ LINE_SEP + "    \" if only a single declaration (or a very small ratio of them) has a VALUE, a comment, etc.," 
				+ LINE_SEP + "    \" no extra column is created for it" 
				+ LINE_SEP + "    DATA lth_any_table TYPE        ty_th_hash_table_type. \" only line with a comment" 
				+ LINE_SEP + "    DATA lo_contract TYPE REF TO cl_contract  ##NEEDED." 
				+ LINE_SEP + "    DATA      ls_item_data  TYPE if_any_interface=>ty_s_item." 
				+ LINE_SEP + "    DATA lv_was_saved TYPE abap_bool VALUE abap_false ##NO_TEXT." 
				+ LINE_SEP 
				+ LINE_SEP + "    FIELD-SYMBOLS: " 
				+ LINE_SEP + "         <ls_data> TYPE ty_s_data, \" first comment line" 
				+ LINE_SEP + "      <ls_amount> LIKE LINE OF its_amount," 
				+ LINE_SEP + "    <ls_contract> TYPE ty_s_contract, \" second comment line" 
				+ LINE_SEP + "   <ls_param> LIKE LINE OF mt_parameter." 
				+ LINE_SEP 
				+ LINE_SEP + "    TYPES:"
				+ LINE_SEP + "      BEGIN OF ty_s_outer,"
				+ LINE_SEP + "      alpha TYPE i,"
				+ LINE_SEP + "      BEGIN OF ty_s_inner,"
				+ LINE_SEP + "      one TYPE i,"
				+ LINE_SEP + "      two TYPE i,"
				+ LINE_SEP + "      three TYPE i,"
				+ LINE_SEP + "      END OF ty_s_inner,"
				+ LINE_SEP + "      beta TYPE i,"
				+ LINE_SEP + "      gamma TYPE i,"
				+ LINE_SEP + "      END OF ty_s_outer."
				+ LINE_SEP 
				+ LINE_SEP + "    \" alignment across comments and empty lines (depending on configuration):" 
				+ LINE_SEP + "    DATA lv_value TYPE i." 
				+ LINE_SEP + "    \" comment" 
				+ LINE_SEP + "    DATA lv_long_variable_name TYPE string." 
				+ LINE_SEP 
				+ LINE_SEP + "    DATA lts_sorted_table LIKE its_table." 
				+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configExecuteOnClassDefinitionSections = new ConfigBoolValue(this, "ExecuteOnClassDefinitionSections", "Execute on CLASS ... DEFINITION sections", true);
	final ConfigIntValue configFillPercentageToJustifyOwnColumn = new ConfigIntValue(this, "FillPercentageToJustifyOwnColumn", "Fill Ratio to justify own column", "%", 1, 20, 100);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnClassDefinitionSections, configAlignAcrossEmptyLines, configAlignAcrossCommentLines, configFillPercentageToJustifyOwnColumn };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignDeclarationsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean isMatchForFirstCommand(Command command, int pass) {
		return command.isDeclaration() && (configExecuteOnClassDefinitionSections.getValue() || !command.isInClassDefinition());
	}

	@Override
	protected boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass) {
		if (command.getFirstToken().isAnyKeyword(keywordOfFirstCommand))
			return command.getFirstToken().getNext() != null && !command.getFirstToken().getNext().isChainColon();
		else if (AbapCult.stringEquals(keywordOfFirstCommand, "TYPES", true) && command.isDeclarationInclude())
			return true;
		else
			return false;
	}

	@Override
	protected void executeOn(Code code, Command startCommand, Command endCommand, boolean startCommandIsChain, int pass) throws UnexpectedSyntaxBeforeChanges {
		// if (startCommand.containsInnerCommentLine())
		// 	return;

		boolean includeKeywordInTable = includeKeywordInTable(startCommand, endCommand);

		// determine the basic indent of the sequence and whether to start with line breaks
		TableStart tableStart = determineTableStart(startCommand, includeKeywordInTable);

		// build the table
		AlignTable table;
		try {
			table = buildTable(startCommand, endCommand, includeKeywordInTable);
			if (table == null)
				return;
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		// align the table
		if (table.getLineCount() > 0) {
			Command[] changedCommands = table.align(tableStart.basicIndent, tableStart.firstLineBreaks, false);
			code.addRuleUses(this, changedCommands);
		}
		
		alignInnerCommentLines(startCommand, endCommand);
	}

	private boolean includeKeywordInTable(Command startCommand, Command endCommand) {
		// determine
		// - whether there is a chain in the Command range
		// - whether any of the declarations continues directly (i.e. without line break) behind the keyword
		//   (esp. "TYPES ..." or "TYPES: ...") or whether all Commands have line breaks after the keyword; 
		boolean hasAnyChain = false;
		boolean continuesBehindAnyKeyword = false;
		Command command = startCommand;
		String keywordOfFirstCommand = startCommand.getFirstToken().getText();
		while (command != endCommand) {
			if (!command.isCommentLine() && command.getFirstToken().isAnyKeyword(keywordOfFirstCommand)) {
				Token identifier = command.getFirstToken().getNextNonCommentSibling();
				if (identifier.isChainColon()) {
					identifier = identifier.getNextNonCommentSibling();
					hasAnyChain = true;
				}
				if (identifier.lineBreaks == 0) {
					continuesBehindAnyKeyword = true;
				}
			}
			command = command.getNext();
		}
		return continuesBehindAnyKeyword || !hasAnyChain;
	}
	
	private TableStart determineTableStart(Command startCommand, boolean includeKeywordInTable) {
		Token firstToken = startCommand.getFirstToken();
		int firstLineBreaks;
		int basicIndent;
		if (includeKeywordInTable) {
			// the first keyword of each Command will be considered in the table, too
			firstLineBreaks = firstToken.lineBreaks;
		   basicIndent = firstToken.getStartIndexInLine();
		} else {
			// the first keyword of each Commands will NOT be considered in the table, i.e. alignment is done for the 
			// content AFTER the first keyword, possibly keeping a "KEYWORD:<line break>" scenario; 
			// this branch is selected if there is a line break after each keyword, and at least one chain is involved 
			if (startCommand.isSimpleChain()) {
				Token colon = firstToken.getNextNonCommentToken();
				Token offsetToken = colon.getNextNonCommentToken();
				firstLineBreaks = offsetToken.lineBreaks;
				if (firstLineBreaks > 0) {
					basicIndent = firstToken.getStartIndexInLine() + ABAP.INDENT_STEP;
				} else {
					// currently impossible to be here, but in case logic is changed later
					basicIndent = colon.getEndIndexInLine() + 1; // not "offsetToken.getStartIndexInLine()", as this is often weirdly indented
				}
			} else {
				Token offsetToken = firstToken.getNextNonCommentToken();
				firstLineBreaks = offsetToken.lineBreaks;
				basicIndent = offsetToken.getStartIndexInLine();
			}
		}
		return new TableStart(firstLineBreaks, basicIndent);
	}

	private AlignTable buildTable(Command startCommand, Command endCommand, boolean includeKeywordInTable) throws UnexpectedSyntaxException {
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);

		// for lines within (possibly nested) BEGIN OF ... END OF blocks, an additionalIndent is used
		Command command = startCommand;
		Token token = command.getFirstToken();
		Layout layout = new Layout();
		
		do {
			// token is now 
			// - either the very first token of a new declaration command (chain or non-chain) - i.e. the declaration keyword DATA, TYPES, etc.
			// - or the first Token after the , in a declaration chain (i.e. an identifier, "BEGIN OF", "END OF" etc.)
			
			if (command.isSimpleChain()) {
				// skip comment lines between chain elements 
				while (token != null && token.isCommentLine()) {
					token = token.getNext();
				}
				if (token == null)
					break;
			} else {
				// skip Commands that are whole comment lines 
				while (command.isCommentLine() && command != endCommand)
					command = command.getNext();
				if (command == endCommand)
					break;
				commandForErrorMsg = command;
				token = command.getFirstToken();
			}

			AlignLine line = table.addLine();

			// keyword DATA, FIELD-SYMBOLS or TYPES, possibly with ":"
			if (token.isAnyKeyword(Command.declarationKeywords)) { 
				token = readDeclarationKeyword(line, token, includeKeywordInTable);
			}

			// (structure) declaration line
			if (token.matchesOnSiblings(true, "BEGIN|END", "OF") || token.matchesOnSiblings(true, "INCLUDE", "TYPE|STRUCTURE")) {
				token = readStructureDeclaration(line, token, layout);
			} else {
				token = readDeclarationLine(line, token, layout.additionalIndent);
			}
			if (token == null)
				return null;

			// comment at line end (except in BEGIN OF / END OF / INCLUDE TYPE ... lines, where the line-end comment was already skipped on purpose)
			token = readCommentAtLineEnd(line, token);

			token = token.getNext();
			if (command.isSimpleChain() && token != null) {
				// continue
			} else {
				command = command.getNext();
				if (command == endCommand)
					break;
				if (command.containsInnerCommentLine())
					return null;
				commandForErrorMsg = command;
				token = command.getFirstToken();
			}
		} while (true);
		
		joinSparselyFilledColumns(table);
		
		return table;
	}

	private Token readDeclarationKeyword(AlignLine line, Token token, boolean includeKeywordInTable) throws UnexpectedSyntaxException {
		// the keywords (and the colons following them, if any) are included in the table in cases where 
		// the remaining content shall be aligned behind (not below) the keywords
		Command command = token.getParentCommand();
		if (includeKeywordInTable) {
			AlignCell newCell;
			if (command.isSimpleChain()) {
				Token colon = token.getNextNonCommentSibling();
				newCell = new AlignCellTerm(Term.createForTokenRange(token, colon));
			} else {
				newCell = new AlignCellToken(token);
			}
			line.setCell(Columns.KEYWORD.getValue(), newCell);
		}
		// move behind the token (and the chain colon, if any)
		if (command.isSimpleChain())
			token = token.getNextNonCommentToken(); 
		token = token.getNextNonCommentToken();
		return token;
	}

	private Token readStructureDeclaration(AlignLine line, Token token, Layout layout) {
		if (token.matchesOnSiblings(true, "END", "OF"))
			layout.additionalIndent -= 2;
		
		// treat the first keyword like an identifier, so it is aligned like the other component names; 
		// however, override the text width to be just 1 character wide (i.e. don't move the TYPE column in the component lines because of this) 
		AlignCell newCell = AlignCellToken.createSpecial(token, layout.additionalIndent, true);
		line.setCell(Columns.IDENTIFIER.getValue(), newCell);
		
		if (token.matchesOnSiblings(true, "BEGIN", "OF"))
			layout.additionalIndent += 2;
		
		// skip the rest of the line, including a line-end comment
		token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
		if (token == null)
			return null;
		if (token.getNext() != null && token.getNext().isCommentAfterCode())
			token = token.getNext();
		return token;
	}

	private Token readDeclarationLine(AlignLine line, Token token, int additionalIndent) throws UnexpectedSyntaxException {
		// identifier; may have the form "var(len)", therefore consider it as a Term
		Term identifier = Term.createSimple(token);
		AlignCell newCell = AlignCellTerm.createSpecial(identifier, additionalIndent, false);
		line.setCell(Columns.IDENTIFIER.getValue(), newCell);

		token = identifier.getNext();
		if (token.isCommaOrPeriod()) // e.g., "DATA: lv_text(100), ..." uses the default type c  
			return token;
		if (!token.isAnyKeyword("TYPE", "LIKE"))
			return null;
		
		// TYPE|LIKE [{LINE OF}|{RANGE OF}|{REF TO}|{{STANDARD|SORTED|HASHED} TABLE OF [REF TO]}] <identifier>
		// (not specifically considered here, but probably irrelevant, as they are skipped): [tabkeys], [INITIAL SIZE n] [WITH HEADER LINE]
		Token typeStart = token;
		Token typeEnd = token;
		while (typeEnd != null && !typeEnd.isAnyKeyword("LENGTH", "DECIMALS", "VALUE", "READ-ONLY") && !typeEnd.textEqualsAny(".", ",")) {
			// do not align table declarations with "WITH ... KEY ..." sections, because they usually should not be put on a single line
			if (typeEnd.isKeyword("WITH"))
				return null;
			typeEnd = typeEnd.getNextSibling();
		}
		if (typeEnd == null)
			return null;
		Term typeInfo = Term.createForTokenRange(typeStart, typeEnd.getPrev());
		line.setCell(Columns.TYPE.getValue(), new AlignCellTerm(typeInfo));
		token = typeEnd;

		// the remaining components only appear in DATA and TYPES, not with FIELD-SYMBOLS
		if (token.isCommaOrPeriod())
			return token;

		// [LENGTH <identifier>]
		Token lengthInfoLast = token.getLastTokenOnSiblings(true, "LENGTH", TokenSearch.ANY_IDENTIFIER_OR_LITERAL);
		if (lengthInfoLast != null) {
			line.setCell(Columns.LENGTH.getValue(), new AlignCellTerm(Term.createForTokenRange(token, lengthInfoLast)));
			token = lengthInfoLast.getNext();
		}

		// [DECIMALS <identifier>]
		Token decimalsInfoLast = token.getLastTokenOnSiblings(true, "DECIMALS", TokenSearch.ANY_IDENTIFIER_OR_LITERAL);
		if (decimalsInfoLast != null) {
			line.setCell(Columns.DECIMALS.getValue(), new AlignCellTerm(Term.createForTokenRange(token, decimalsInfoLast)));
			token = decimalsInfoLast.getNext();
		}

		// [VALUE <term>|{IS INITIAL}]
		if (token.isKeyword("VALUE")) {
			Token valueLast = token.getLastTokenOnSiblings(true, "VALUE", "IS", "INITIAL");
			if (valueLast == null) {
				Term valueTerm = Term.createSimple(token.getNext());
				valueLast = valueTerm.lastToken;
			}
			if (valueLast == null)
				return null;
			line.setCell(Columns.VALUE.getValue(), new AlignCellTerm(Term.createForTokenRange(token, valueLast)));
			token = valueLast.getNext();
		}

		// [READ-ONLY]
		if (token.isKeyword("READ-ONLY")) {
			line.setCell(Columns.READ_ONLY.getValue(), new AlignCellToken(token));
			token = token.getNext();
		}

		// . or , (may be preceded by a pragma)
		token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
		if (token == null)
			return null;

		return token;
	}

	private Token readCommentAtLineEnd(AlignLine line, Token token) {
		if (token.getNext() != null && token.getNext().isCommentAfterCode()) {
			token = token.getNext();
			line.setCell(Columns.LINE_END_COMMENT.getValue(), new AlignCellToken(token));
		}
		return token;
	}

	private void joinSparselyFilledColumns(AlignTable table) throws UnexpectedSyntaxException {
		// decide whether rarely used columns should really be aligned (i.e. whether horizontal space should be reserved for them):
		// if only one single line (or less than 20% of all lines) have content in this column, then join the content into a previous column

		double fillRatioToJustifyOwnColumn = configFillPercentageToJustifyOwnColumn.getValue() / 100.0;

		for (int i = Columns.LINE_END_COMMENT.getValue(); i >= Columns.LENGTH.getValue(); --i) {
			AlignColumn testColumn = table.getColumn(i);
			if (testColumn.isEmpty())
				continue;
			if (testColumn.getCellCount() <= 1 || testColumn.getCellCount() < (int) (table.getLineCount() * fillRatioToJustifyOwnColumn))
				testColumn.joinIntoPreviousColumns();
		}
	}

	private void alignInnerCommentLines(Command startCommand, Command endCommand) {
		Command command = startCommand;
		int blockLevel = 0;
		while (command != endCommand) {
			blockLevel += command.getBlockLevelDiff();
			Token token = command.getFirstToken();
			while (token != null) {
				if (token.isQuotMarkCommentLine()) {
					Token nextCode = token.getNextNonCommentToken();
					if (nextCode != null) {
						// align with next code, but if next code reads "END OF", add an indent step
						token.spacesLeft = nextCode.spacesLeft + (nextCode.matchesOnSiblings(true, "END", "OF") ? ABAP.INDENT_STEP : 0);
					} else if (blockLevel > 0) {
						// inside a BEGIN OF block, align with the next executable code line
						Command nextCodeCommand = command.getNextNonCommentCommand();
						if (nextCodeCommand != null)
							token.spacesLeft = nextCodeCommand.getFirstToken().spacesLeft;
					}
				}
				token = token.getNext();
			}
			command = command.getNext();
		}
	}
}
