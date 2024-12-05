package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignMethodsDeclarationRule extends AlignDeclarationSectionRuleBase {
   // for the structure of METHODS statements, see https://syntax.abaplint.org/#/abap/statement/MethodDef

   private enum Columns  {
      KEYWORD,
      METHOD_NAME,
      ACCESS,
      PARAMETER_NAME,
      TYPE_OR_LIKE,
      DEFAULT_OR_OPTIONAL;

		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 6;

	private static final String[] accessOrExceptionKeywords = new String[] { "IMPORTING", "EXPORTING", "CHANGING", "RETURNING", "RAISING", "EXCEPTIONS" };

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_METHODS_DECLARATION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align METHODS declarations"; }

	@Override
	public String getDescription() { return "Aligns METHODS and CLASS-METHODS declarations."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 5, 24); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
      return  LINE_SEP + "CLASS cl_any_class DEFINITION FINAL." 
				+ LINE_SEP + "  PUBLIC SECTION." 
				+ LINE_SEP + "    CLASS-METHODS any_method" 
				+ LINE_SEP + "      IMPORTING" 
				+ LINE_SEP + "        !iv_any_param TYPE i OPTIONAL" 
				+ LINE_SEP + "        !iv_other_param TYPE string DEFAULT 'abc'" 
				+ LINE_SEP + "      EXPORTING" 
				+ LINE_SEP + "        !ev_any_result TYPE i" 
				+ LINE_SEP + "        !ev_other_result TYPE string" 
				+ LINE_SEP + "      RAISING" 
				+ LINE_SEP + "        !cx_any_exception."  
				+ LINE_SEP 
				+ LINE_SEP + "    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL" 
				+ LINE_SEP + "                                   !iv_other_param TYPE string DEFAULT 'abc'" 
				+ LINE_SEP + "                         EXPORTING !ev_any_result TYPE i" 
				+ LINE_SEP + "                                   !ev_other_result TYPE string" 
				+ LINE_SEP + "                         RAISING !cx_any_exception."  
				+ LINE_SEP 
				+ LINE_SEP + "    METHODS set_value IMPORTING !iv_new_value TYPE i." 
				+ LINE_SEP + "    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i." 
				+ LINE_SEP + "    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i." 
				+ LINE_SEP + "    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i" 
				+ LINE_SEP + "                         EXPORTING ev_any_result TYPE i" 
				+ LINE_SEP + "                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table" 
				+ LINE_SEP + "                         RAISING cx_any_exception."  
				+ LINE_SEP 
				+ LINE_SEP + "    METHODS get_max_value" 
				+ LINE_SEP + "      RETURNING" 
				+ LINE_SEP + "        VALUE(rv_result) TYPE i." 
				+ LINE_SEP 
				+ LINE_SEP + "    METHODS:" 
				+ LINE_SEP + "      any_chained_method" 
				+ LINE_SEP + "        IMPORTING" 
				+ LINE_SEP + "          !iv_any_param TYPE i OPTIONAL" 
				+ LINE_SEP + "          !iv_other_param TYPE string DEFAULT 'abc'" 
				+ LINE_SEP + "        RAISING" 
				+ LINE_SEP + "          !cx_any_exception,"  
				+ LINE_SEP + "      other_chained_method" 
				+ LINE_SEP + "        IMPORTING" 
				+ LINE_SEP + "          !it_source_table TYPE ty_tt_any OPTIONAL" 
				+ LINE_SEP + "          !iv_name TYPE string" 
				+ LINE_SEP + "        CHANGING" 
				+ LINE_SEP + "          !cts_result_table TYPE ty_ts_any." 
				+ LINE_SEP 
				+ LINE_SEP + "    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i," 
				+ LINE_SEP + "      get_current_value_chained RETURNING VALUE(rv_result) TYPE i," 
				+ LINE_SEP + "      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i," 
				+ LINE_SEP + "      get_max_value_chained" 
				+ LINE_SEP + "        RETURNING" 
				+ LINE_SEP + "          VALUE(rv_result) TYPE i." 
				+ LINE_SEP + "ENDCLASS.";
   }

	private static final String[] changeTypeSelection = new String[] { "always", "keep as is", "never" };
	private static final String[] handleOneLinersSelection = new String[] { "create if possible", "keep existing", "same as multi-liners" };
	private static final String[] alignConsecutiveSelection = new String[] { "never", "one-liners", "one-liners and tabular layout" };
	
	final ConfigEnumValue<ChangeType> configContinueAfterKeyword = new ConfigEnumValue<ChangeType>(this, "ContinueAfterKeyword", "Continue line after [CLASS-]METHODS", changeTypeSelection, ChangeType.values(), ChangeType.KEEP_AS_IS);
	final ConfigEnumValue<ChangeType> configContinueAfterMethodName = new ConfigEnumValue<ChangeType>(this, "ContinueAfterMethodName", "Continue line after method name", changeTypeSelection, ChangeType.values(), ChangeType.KEEP_AS_IS);
	final ConfigEnumValue<ChangeType> configContinueAfterAccess = new ConfigEnumValue<ChangeType>(this, "ContinueAfterAccess", "Continue line after IMPORTING etc.", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);
	final ConfigIntValue configFillPercentageToJustifyOwnColumn = new ConfigIntValue(this, "FillPercentageToJustifyOwnColumn", "Fill ratio to justify own column for DEFAULT / OPTIONAL", "%", 1, 40, 100);
	final ConfigEnumValue<MethodsOneLinerAction> configHandleOneLiners = new ConfigEnumValue<MethodsOneLinerAction>(this, "HandleOneLiners", "Handling of (potential) one-liners", handleOneLinersSelection, MethodsOneLinerAction.values(), MethodsOneLinerAction.KEEP_EXISTING);
	final ConfigEnumValue<MethodsSequenceAlignment> configAlignConsecutive = new ConfigEnumValue<MethodsSequenceAlignment>(this, "AlignConsecutive", "Align consecutive declarations", alignConsecutiveSelection, MethodsSequenceAlignment.values(), MethodsSequenceAlignment.ONE_LINERS, MethodsSequenceAlignment.NEVER, LocalDate.of(2022, 6, 5));
	final ConfigBoolValue configSeparateWithEmptyLine = new ConfigBoolValue(this, "SeparateWithEmptyLine", "Separate multi-line declarations with empty lines", true, false, LocalDate.of(2022, 6, 5));

	private final ConfigValue[] configValues = new ConfigValue[] { configContinueAfterKeyword, configContinueAfterMethodName, configContinueAfterAccess, configFillPercentageToJustifyOwnColumn, configHandleOneLiners, configAlignConsecutive , configAlignAcrossEmptyLines, configAlignAcrossCommentLines, configSeparateWithEmptyLine };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private MethodsSequenceAlignment getAlignConsecutive() { return MethodsSequenceAlignment.forValue(configAlignConsecutive.getValue()); }
	
	public AlignMethodsDeclarationRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected int getNumberOfPasses() {
		return (getAlignConsecutive() == MethodsSequenceAlignment.NEVER) ? 1 : 2;
	}
	
	@Override
	protected boolean getAlignAcrossCommentLinesDefault() { 
		return false; 
	}

	@Override
	protected boolean isMatchForFirstCommand(Command command, int pass) {
		return command.firstCodeTokenIsAnyKeyword("METHODS", "CLASS-METHODS") && !command.isRapHandlerMethodDef();
	}

	@Override
	protected boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass) {
		// only in the second pass, allow for several consecutive declarations (or declaration chains)
		return pass > 0 && command.firstCodeTokenIsKeyword(keywordOfFirstCommand) && isMatchForFirstCommand(command, pass);
	}

	@Override
	protected void executeOn(Code code, Command startCommand, Command endCommand, boolean startCommandIsChain, int pass) throws UnexpectedSyntaxBeforeChanges {
		AlignTable table = null;
		
		// in the first pass, create an AlignTable for each declaration; 
		// in the second pass, build all consecutive (one-liner or tabular) declarations (regardless of whether they are 
		// part of a chain) into one AlignTable, until a declaration is found that does not fulfill isDeclarationSuitedForSecondPass()
		boolean allowMultipleDeclarationsInTable = (pass > 0);

		if (allowMultipleDeclarationsInTable)
			table = new AlignTable(MAX_COLUMN_COUNT);
		
		Command command = startCommand;
		while (command != endCommand) {
			if (skipCommand(command) || !command.firstCodeTokenIsAnyKeyword("METHODS", "CLASS-METHODS")) {
				command = command.getNext();
				continue;
			}

			if (!allowMultipleDeclarationsInTable)
				table = new AlignTable(MAX_COLUMN_COUNT);

			// create the KEYWORD cell from the [CLASS-]METHODS Token and, if applicable, the chain colon 
			Token token = command.getFirstCodeToken();
			token = addMethodsKeyword(token, table);

			// create the METHOD_NAME cell  
			token = token.getNextCodeSibling();
			token = addMethodName(token, table);

			// read Tokens of this Command and add them to the corresponding cells of the current AlignLine
			token = token.getNextCodeSibling();
			boolean isMethodForEvent = false;
			while (token != null) {
				if (token.isAnyKeyword(accessOrExceptionKeywords)) {
					token = addAccessOrException(token, table, isMethodForEvent);
					
				// because of "FINAL REDEFINITION, check "FOR TESTING|FINAL REDEFINITION|..." BEFORE the next block on "ABSTRACT|FINAL|..."!
				} else if (token.isKeyword() && token.matchesOnSiblings(true, "FOR TESTING|FINAL REDEFINITION|REDEFINITION")) { 
					token = addForTestingOrRedefinition(token, table);
					
				} else if (token.isKeyword() && token.matchesOnSiblings(true, "ABSTRACT|FINAL|DEFAULT IGNORE|DEFAULT FAIL|AMDP OPTION|FOR EVENT")) {
					token = addOptionalKeywords(token, table);
					if (token.matchesOnSiblings(true, "FOR EVENT")) {
						isMethodForEvent = true;
					}
					
				} else if (token.isAnyKeyword("TYPE", "LIKE")) {
					token = addParameter(token, table);
					
				} else if (token.isAnyKeyword("OPTIONAL", "DEFAULT")) {
					token = addOptionalOrDefault(token, table);

				} else if (token.isCommaOrPeriod()) {
					// if the table is 'full', align it (possibly after removing the last declaration) and start a new AlignTable
					table = finishTableIfFull(code, token, table, allowMultipleDeclarationsInTable);

					token = token.getNextCodeSibling();
					if (token == null)
						break;

					isMethodForEvent = false;
					table.addLine();
					token = addMethodName(token, table);
				}
				token = token.getNextCodeSibling();
			}

			command = command.getNext();
		}
		
		// align remaining entries
		if (allowMultipleDeclarationsInTable && !table.isEmpty()) {
			executeOn(code, startCommand, table, allowMultipleDeclarationsInTable);
		}

		alignInnerCommentLines(code, startCommand, endCommand);
	}

	private Token addMethodsKeyword(Token token, AlignTable table) throws UnexpectedSyntaxBeforeChanges {
		Token nextToken = token.getNextCodeSibling();
		AlignCell keywordCell;
		if (nextToken != null && nextToken.isChainColon()) {
			Token chainColon = nextToken;
			try {
				keywordCell = new AlignCellTerm(Term.createForTokenRange(token, chainColon));
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxBeforeChanges(this, e);
			}
			token = chainColon;
		} else {
			keywordCell = new AlignCellToken(token);
		}
		AlignLine line = table.addLine();
		line.setCell(Columns.KEYWORD.getValue(), keywordCell);
		return token;
	}

	private Token addMethodName(Token token, AlignTable table) {
		table.getLastLine().setCell(Columns.METHOD_NAME.getValue(), new AlignCellToken(token));
		return token;
	}

	private Token addAccessOrException(Token token, AlignTable table, boolean isMethodForEvent) {
		// if an access keyword or parameter was already found, create a new table line
		AlignLine line = getLineWithFreeCell(table, Columns.ACCESS);
		line.setCell(Columns.ACCESS.getValue(), new AlignCellToken(token));

		// in some cases, a simple list of parameter/exception names follows without 'TYPE', so we add non-comment Tokens 
		// that follow the keyword as the 'parameter' names: 
		// - EXCEPTIONS + names of non-class-based exceptions  
		// - RAISING + class names of class-based exceptions
		// - IMPORTING + parameter names [+ 'sender'], if this is a "METHODS ... FOR EVENT ..." declaration
		if (token.isAnyKeyword("EXCEPTIONS", "RAISING") || (isMethodForEvent && token.isKeyword("IMPORTING"))) {
			do {
				token = token.getNextCodeSibling();
				// if multiple exception / exception class / parameter names are found in the same line, only enter the first one 
				if (line.getCell(Columns.PARAMETER_NAME.getValue()) == null) {
					// like PrettyPrinter, we do NOT make the width of the PARAMETER_NAME column depend on these exception / 
					// exception class / parameter names; therefore, override text width with 1:
					AlignCell exceptionName = AlignCellToken.createSpecial(token, 0, true);
					line.setCell(Columns.PARAMETER_NAME.getValue(), exceptionName);
				}
				// in case of "RAISING ... RESUMABLE(exc1)", move to the closing parenthesis
				if (token.getOpensLevel()) 
					token = token.getNextCodeSibling();

				Token testToken = token.getNextCodeSibling();
				if (testToken == null || !testToken.isIdentifier())
					break;
				if (testToken.lineBreaks > 0)
					line = table.addLine();
			} while(true);
		}
		return token;
	}
	
	private AlignLine getLineWithFreeCell(AlignTable table, Columns column) {
		// if the supplied column (or a column after it) was already entered to the current line, create a new table line;
		// otherwise continue on the current one
		AlignLine line = table.getLastLine();
		for (int columnIndex = column.getValue(); columnIndex < MAX_COLUMN_COUNT; ++columnIndex) {
			if (line.getCell(columnIndex) != null) {
				return table.addLine();
			}
		}
		return line;
	}

	private Token addForTestingOrRedefinition(Token token, AlignTable table) {
		AlignLine line = getLineWithFreeCell(table, Columns.ACCESS);

		// "FOR TESTING" and "[FINAL] REDEFINITION" are always aligned (whether or not they were preceded by a line break)
		// with the "IMPORTING" etc. keywords of other method declarations (but overriding their width with 1)
		AlignCell forTestingKeyword = AlignCellToken.createSpecial(token, 0, true);
		line.setCell(Columns.ACCESS.getValue(), forTestingKeyword);

		if (token.matchesOnSiblings(true, "FINAL", "REDEFINITION")) {
			// ensure that we continue behind "FINAL REDEFINITION"
			token = token.getNextCodeSibling(); // "REDEFINITION"
			
		} else if (token.matchesOnSiblings(true, "FOR", "TESTING")) {
			// if "FOR TESTING" is directly followed by "RAISING" (with no comment in between), then "RAISING" is 
			// NOT put to the ACCESS column (otherwise it would end up below "FOR TESTING"), but rather always put 
			// directly behind it (same for the first exception class) 
			token = token.getNextCodeSibling();
			Token nextToken = token.getNextSibling(); // do NOT use getNextCodeSibling
			if (nextToken != null && nextToken.isKeyword("RAISING")) {
				nextToken.setWhitespace();
				token = nextToken; // prevent this "RAISING" from being processed again in the above "if" branch
				nextToken = token.getNextSibling();
				if (nextToken != null && nextToken.isIdentifier()) {
					nextToken.setWhitespace();
					token = nextToken;
				}
			}
		} 
		return token;
	}

	private Token addOptionalKeywords(Token token, AlignTable table) {
		// if these (optional) keywords were put to an own line, align them with "IMPORTING" etc., but override their length with 1;
		// this may later be changed by the dedicated AlignMethodsForTestingRule etc.; keywords that are put on the same line as the method name 
		// (like ABSTRACT, FINAL, DEFAULT IGNORE, DEFAULT FAIL, AMDP OPTION [READ-ONLY] [CDS SESSION CLIENT clnt|CURRENT], 
		// FOR TESTING, FOR EVENT evt OF {class|intf}) are simply skipped, leaving them in their position
		if (token.lineBreaks > 0) { 
			AlignLine line = getLineWithFreeCell(table, Columns.ACCESS);
			AlignCell optionalKeyword = AlignCellToken.createSpecial(token, 0, true);
			line.setCell(Columns.ACCESS.getValue(), optionalKeyword);
		}
		return token;
	}

	private Token addParameter(Token typeOrLikeToken, AlignTable table) throws UnexpectedSyntaxBeforeChanges {
		AlignLine line = getLineWithFreeCell(table, Columns.PARAMETER_NAME);
		
		// the parameter name is the term that precedes TYPE or LIKE
		AlignCell parameterNameCell;
		Token parameterName = typeOrLikeToken.getPrevCodeSibling();
		if (parameterName != null && parameterName.textStartsWith(")")) { // VALUE(...) or REFERENCE(...)
			try {
				parameterNameCell = new AlignCellTerm(Term.createForTokenRange(parameterName.getPrevCodeSibling(), parameterName));
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxBeforeChanges(this, e);
			}
		} else {
			parameterNameCell = new AlignCellToken(parameterName);
		}
		line.setCell(Columns.PARAMETER_NAME.getValue(), parameterNameCell);
		line.setCell(Columns.TYPE_OR_LIKE.getValue(), new AlignCellToken(typeOrLikeToken));
		return typeOrLikeToken;
	}

	private Token addOptionalOrDefault(Token token, AlignTable table) throws UnexpectedSyntaxBeforeChanges {
		// replace the TYPE_OR_LIKE cell with everything up to this keyword
		AlignLine line = table.getLastLine();
		AlignCell oldTypeCell = line.getCell(Columns.TYPE_OR_LIKE.getValue());
		if (oldTypeCell != null) {
			AlignCellTerm newTypeCell;
			try {
				newTypeCell = new AlignCellTerm(Term.createForTokenRange(oldTypeCell.getFirstToken(), token.getPrevNonCommentSibling()));
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxBeforeChanges(this, e);
			}
			line.overwriteCell(Columns.TYPE_OR_LIKE.getValue(), newTypeCell);
		}
		line.setCell(Columns.DEFAULT_OR_OPTIONAL.getValue(), new AlignCellToken(token));
		return token;
	}
	
	private AlignTable finishTableIfFull(Code code, Token token, AlignTable table, boolean allowMultipleDeclarationsInTable) throws UnexpectedSyntaxBeforeChanges {
		// determine whether the table is 'full' (and therefore must be aligned)
		if (allowMultipleDeclarationsInTable) {
			// find the beginning of the declaration that was last added to the table
			int methodNameLineIndex = table.getColumn(Columns.METHOD_NAME.getValue()).getLineIndexOfLastNonEmptyCell();

			// determine whether the last declaration matches the configuration for alignment of consecutive 
			// one-liners / tabular style declarations; if so, continue building further declarations into the same table 
			if (isDeclarationSuitedForMultiDeclarations(table, methodNameLineIndex, token)) 
				return table;

			// remove the last method declaration, as it is NOT suited for multi-alignment in the second pass
			while (!table.isEmpty() && table.getLineCount() > methodNameLineIndex)
				table.removeLastLine();
		}

		// align the (remaining lines of the) table and start a new table
		if (table.getLineCount() > 0) {
			executeOn(code, token.getParentCommand(), table, allowMultipleDeclarationsInTable);
		}
		return new AlignTable(MAX_COLUMN_COUNT);
	}
	
	private boolean isDeclarationSuitedForMultiDeclarations(AlignTable table, int methodNameLineIndex, Token endToken) {
		// determines whether the last method declaration (starting from the method name cell) matches the configuration  
		// for common alignment of one-liners / tabular style declarations

		if (methodNameLineIndex < 0)
			return false;
		
		if (getAlignConsecutive() == MethodsSequenceAlignment.ONE_LINERS) {
			// check whether there is a line break between the method name and the end of the declaration (endToken);
			// in non-chains, even start the check at the METHODS or CLASS-METHODS keyword (while chains may begin with 'METHODS: " comment<br>') 
			Token startToken = table.getLine(methodNameLineIndex).getCell(Columns.METHOD_NAME.getValue()).getFirstToken();
			if (startToken.getPrevCodeSibling().isAnyKeyword("METHODS", "CLASS-METHODS")) // with no colon : in between!
				startToken = startToken.getPrevCodeSibling();

			Command command = startToken.getParentCommand();
			if (command.containsLineBreaksBetween(startToken, endToken, true))
				return false;

			// one-liners that only consist of a method name only are also included; however, .executeOn() will override 
			// their method names' text width with 1 to ensure that they do NOT influence other columns
			
			return true;
			
		} else if (getAlignConsecutive() == MethodsSequenceAlignment.ALL_TABULAR) {
			// allow one-liners as well as declarations in 'tabular' layout (or chains of those), i.e. ensure that there are no line breaks...
			// - after the keywords METHODS / CLASS-METHODS (except when they are followed by a chain colon)
			// - after the method names, or 
			// - after the access keywords IMPORTING, EXPORTING etc.  
			final Columns[] columnsToCheck = new Columns[] { Columns.KEYWORD, Columns.METHOD_NAME, Columns.ACCESS };
			
			for (int lineIndex = methodNameLineIndex; lineIndex < table.getLineCount(); ++lineIndex) {
				AlignLine line = table.getLine(lineIndex);
				for (Columns column : columnsToCheck) {
					AlignCell cell = line.getCell(column.getValue());
					if (cell == null) 
						continue;
					Token nextToken = cell.getLastToken().getNextNonCommentSibling();
					if (nextToken != null && nextToken.lineBreaks > 0 && !cell.getLastToken().isChainColon()) {
						// line break found - this is no one-liner or 'tabular' layout
						// or, as an exception, continue if (lineIndex == methodNameLineIndex && ChangeType.forValue(configContinueAfterKeyword.getValue()) == ChangeType.NEVER)?
						return false;
					}
				}
			}
			return true;
		
		} else { // MethodsSequenceAlignment.NEVER
			return false;
		}
	}

	private void executeOn(Code code, Command startCommand, AlignTable table, boolean isTableOfOneLinersOrTabular) throws UnexpectedSyntaxBeforeChanges {
		if (table.isEmpty())
			return;

		// determine indent
		int basicIndent = startCommand.getIndent();
		if (table.getColumn(Columns.KEYWORD.getValue()).isEmpty()) 
			basicIndent += ABAP.INDENT_STEP;
		int firstLineBreaks = table.getFirstToken().lineBreaks;
		
		// determine whether the table starts or ends in the middle of a declaration chain
		boolean tableContainsPartialChains = false;
		if (table.getLine(0).getCell(Columns.KEYWORD.getValue()) == null) {
			tableContainsPartialChains = true;
		} else if (table.getLastLine().getLastToken().matchesOnSiblings(true, TokenSearch.ASTERISK, ABAP.COLON_SIGN_STRING)) {
			tableContainsPartialChains = true;
		}

		// insert line breaks depending on rule configuration
		changeLineBreaks(code, table, isTableOfOneLinersOrTabular && !tableContainsPartialChains, basicIndent);

		// decide whether the DEFAULT or OPTIONAL column should really be aligned (i.e. whether horizontal space should be reserved for them):
		// if only one single line (or less than 20% of all lines) have content in this column, then join the content into a previous column
		try {
			AlignColumn defOrOptColumn = table.getColumn(Columns.DEFAULT_OR_OPTIONAL.getValue());
			if (!defOrOptColumn.isEmpty()) {
				double fillRatioToJustifyOwnColumn = configFillPercentageToJustifyOwnColumn.getValue() / 100.0;
				if (defOrOptColumn.getCellCount() <= 1 || defOrOptColumn.getCellCount() < (int) (table.getLineCount() * fillRatioToJustifyOwnColumn)) {
					Command[] changedCommands = defOrOptColumn.joinIntoPreviousColumns(true);
					code.addRuleUses(this, changedCommands);
				}
			}
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		if (isTableOfOneLinersOrTabular) {
			// methods without parameters should not contribute to the column width of method names  
			for (AlignLine line : table.getLines()) {
				if (line.getLastCellIndex() == Columns.METHOD_NAME.getValue()) {
					line.getCell(Columns.METHOD_NAME.getValue()).setOverrideTextWidth(1);
				}
			}
		}
		
		// align the table
		int minimumLineCount = isTableOfOneLinersOrTabular ? 2 : 1; 
		if (table.getLineCount() >= minimumLineCount) {
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, false);
			code.addRuleUses(this, changedCommands);
		}

		// separate multi-line declarations with empty lines above and below
		if (configSeparateWithEmptyLine.getValue()) {
			separateWithEmptyLine(code, table);
		}
	}
	
	private void changeLineBreaks(Code code, AlignTable table, boolean alwaysContinueLine, int basicIndent) {
 		boolean isPotentialOneLiner = (table.getLineCount() == 1 && !table.getLine(0).containsComments());
		boolean isCurrentOneLiner = isPotentialOneLiner && !table.getLine(0).containsInnerLineBreaks(); 
		MethodsOneLinerAction oneLinerAction = MethodsOneLinerAction.forValue(configHandleOneLiners.getValue());
		
		// insert line breaks depending on rule configuration
		for (int colIndex = Columns.KEYWORD.getValue(); colIndex <= Columns.ACCESS.getValue(); ++colIndex) {
			AlignColumn column = table.getColumn(colIndex);
			if (column.isEmpty())
				continue;

			// determine the configuration for this column
			ChangeType continueLine;
			if (alwaysContinueLine) {
				continueLine = (colIndex == Columns.KEYWORD.getValue()) ? ChangeType.KEEP_AS_IS : ChangeType.ALWAYS;
			} else if (isPotentialOneLiner && oneLinerAction == MethodsOneLinerAction.CREATE) {
				continueLine = ChangeType.ALWAYS;
			} else if (isCurrentOneLiner && oneLinerAction == MethodsOneLinerAction.KEEP_EXISTING) {
				continueLine = ChangeType.KEEP_AS_IS;
			} else {
				// for multi-liners, or if the one-liner doesn't get special treatment, use regular configuration  
				if (colIndex == Columns.KEYWORD.getValue()) {
					continueLine = ChangeType.forValue(configContinueAfterKeyword.getValue());
				} else if (colIndex == Columns.METHOD_NAME.getValue()) {
					continueLine = ChangeType.forValue(configContinueAfterMethodName.getValue());
				} else { // Columns.ACCESS
					continueLine = ChangeType.forValue(configContinueAfterAccess.getValue());
				}
			}

			// if configured, remove or insert line breaks
			boolean forceLineBreak = changeLineBreaksAfterColumn(code, table, colIndex, continueLine);
			
			// if configuration demands to break after this column, or if KEEP_AS_IS is configured and current code always breaks, 
			// configure the AlignColumn to force a line break (required by AlignTable.align() method) 
			if (forceLineBreak) {
				column.setForceLineBreakAfter(forceLineBreak);
				// if there is a line break after 'METHODS method_name', the ACCESS parameters (IMPORTING etc.) are indented below 
				// METHODS (NOT below method_name, unless we break after METHODS, too) 
				if (colIndex == Columns.METHOD_NAME.getValue()) {
					AlignColumn accessCol = table.getColumn(Columns.ACCESS.getValue()); 
					if (table.getColumn(Columns.KEYWORD.getValue()).getForceLineBreakAfter()) {
						accessCol.setForceIndent(Columns.METHOD_NAME.getValue(), ABAP.INDENT_STEP);
					} else {
						accessCol.setForceIndent(Columns.KEYWORD.getValue(), ABAP.INDENT_STEP);
					}
				}
			}

			if (continueLine != ChangeType.ALWAYS) {
				table.overrideWidthIfColumnIsFollowedByLineBreaks(column);
			}
		}
	}

	private boolean changeLineBreaksAfterColumn(Code code, AlignTable table, int colIndex, ChangeType continueLine) {
		boolean forceLineBreak = (continueLine != ChangeType.ALWAYS);
		for (AlignLine line : table.getLines()) {
			if (line.getCell(colIndex) == null)
				continue;
			
			AlignCell nextCell = line.getNextNonEmptyCellAfter(colIndex);
			if (nextCell == null) 
				continue;
			
			Token nextToken = nextCell.getFirstToken();
			if (continueLine == ChangeType.ALWAYS) {
				if (nextToken.lineBreaks > 0 && !nextToken.getPrev().isComment()) { 
					nextToken.setWhitespace();
					code.addRuleUse(this, nextToken.getParentCommand());
				}
			
			} else if (continueLine == ChangeType.KEEP_AS_IS) {
				if (nextToken.lineBreaks == 0)
					forceLineBreak = false;
					
			} else if (continueLine == ChangeType.NEVER) {
				if (nextToken.lineBreaks == 0) { 
					nextToken.setWhitespace(1,  nextToken.getStartIndexInLine());
					code.addRuleUse(this, nextToken.getParentCommand());
				}
			}
		}
		return forceLineBreak;
	}

	private void separateWithEmptyLine(Code code, AlignTable table) {
		AlignColumn methodNameColumn = table.getColumn(Columns.METHOD_NAME.getValue());
		
		for (AlignLine line : table.getLines()) {
			AlignCell methodNameCell = line.getCell(methodNameColumn); 
			if (methodNameCell == null)
				continue;
			Token methodName = methodNameCell.getFirstToken();
			Command command = methodName.getParentCommand();
			Token endToken = methodName.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			
			// skip this method declaration if it is a one-liner, a method FOR TESTING, or a method [FINAL] REDEFINITION
			if (!maySeparateWithEmptyLines(command, methodName, endToken))
				continue;
			
			// for multi-liners, ensure an empty line above the declaration; note that we could be at the start of the  
			// Command or inside a chain, and the line must be inserted above any comments that may be attached above 
			// this declaration
			Token changeToken = null;
			if (line.getFirstToken() == command.getFirstToken()) {
				changeToken = command.getStartOfAttachedComments().getFirstToken();
				if (changeToken.getParentCommand().getPrevSibling() == null)
					changeToken = null;
			} else {
				changeToken = line.getFirstToken().getStartOfAttachedComments(); 
			}
			if (changeToken != null && changeToken.lineBreaks <= 1) {
				changeToken.lineBreaks = 2;
				code.addRuleUse(this, command);
			}
			
			// also ensure an empty line below the multi-line declaration
			changeToken = null;
			if (endToken != null && endToken.isComma()) {
				changeToken = endToken.getNextNonCommentSibling().getStartOfAttachedComments();
			} else if (command.getNextSibling() != null) {
				changeToken = command.getNextSibling().getFirstToken();
			}
			if (changeToken != null && changeToken.lineBreaks <= 1) {
				changeToken.lineBreaks = 2;
				code.addRuleUse(this, command);
			}
		}
	}

	private boolean maySeparateWithEmptyLines(Command command, Token methodName, Token endToken) {
		// one-liners do NOT require separation by empty lines
		if (!command.containsLineBreaksBetween(methodName, endToken, false)) 
			return false;

		// determine whether the declaration is FOR TESTING or [FINAL] REDEFINITION; this can NOT be done using 
		// token.matchesOnSiblings(...), because this may search beyond endToken
		Token token = methodName;
		while (token != null && token != endToken) {
			if (token.matchesOnSiblings(true, "FOR TESTING|REDEFINITION")) 
				return false;
			token = token.getNextNonCommentSibling();
		}
		return true;
	}

	private void alignInnerCommentLines(Code code, Command startCommand, Command endCommand) {
		Command command;
		command = startCommand;
		while (command != endCommand) {
			Token token = command.getFirstToken();
			while (token != null) {
				if (token.isQuotMarkCommentLine()) {
					Token nextCode = token.getNextNonCommentToken();
					if (nextCode != null && token.spacesLeft != nextCode.spacesLeft) {
						token.spacesLeft = nextCode.spacesLeft;
						code.addRuleUse(this, command);
					}
				}
				token = token.getNext();
			}
			command = command.getNext();
		}
	}

}
