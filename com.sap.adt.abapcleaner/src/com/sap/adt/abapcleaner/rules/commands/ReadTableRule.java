package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.ABAP.SyField;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.LogicalExpression;
import com.sap.adt.abapcleaner.rulehelpers.ReadTableCommand;
import com.sap.adt.abapcleaner.rulehelpers.ReadTableResultType;
import com.sap.adt.abapcleaner.rulehelpers.ReadTableSelectType;
import com.sap.adt.abapcleaner.rulehelpers.RelationalExpressionType;
import com.sap.adt.abapcleaner.rulehelpers.SyFieldAnalyzer;
import com.sap.adt.abapcleaner.rules.alignment.AlignParametersRule;

public class ReadTableRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Prefer LINE_EXISTS or LINE_INDEX to READ TABLE or LOOP AT", "prefer-line-exists.md") };

	@Override
	public RuleID getID() { return RuleID.READ_TABLE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace READ TABLE with table expression"; }

	@Override
	public String getDescription() { return "Replaces suitable cases of READ TABLE with table expressions, introducing ASSIGN, line_exists( ), or line_index( )."; }

	@Override
	public String getHintsAndRestrictions() { return "Some variants cannot be replaced, esp. if 'FROM wa', 'INTO wa', 'REFERENCE INTO dref' or 'BINARY SEARCH' is used, if multiple SY- fields are evaluated afterwards, or the table is returned by a function or constructor expression."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 3, 8); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_PARAMETERS } ; }

	// table expressions were introduced with ABAP 7.40, and with them, the built-in functions line_exists( ) and line_index( )
	public int getRequiredAbapRelease() { return ABAP.REQUIRED_RELEASE_740; }

	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD read_table_to_table_expression." 
			+ LINE_SEP + "    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN." 
			+ LINE_SEP + "    CHECK sy-subrc <> 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ"
			+ LINE_SEP + "    IF sy-subrc EQ 0." 
			+ LINE_SEP + "      RETURN." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" note how verbose READ TABLE is compared to a table expression!" 
			+ LINE_SEP + "    READ TABLE its_any WITH TABLE KEY keyname" 
			+ LINE_SEP + "                       COMPONENTS comp1 = iv_value1" 
			+ LINE_SEP + "                                  comp2 = iv_value2" 
			+ LINE_SEP + "                       TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    IF sy-subrc <> 0." 
			+ LINE_SEP + "      RETURN." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    DATA(lv_line_index) = sy-tabix." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" ----------------------------------------------------------------------------------------------" 
			+ LINE_SEP + "    \" the following variants of READ TABLE can NOT be automatically replaced with table expressions:" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" BINARY SEARCH is not available with table expressions, so replacing this might lead to a full table scan" 
			+ LINE_SEP + "    READ TABLE it_any WITH KEY comp1 = iv_value1 BINARY SEARCH TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    CHECK sy-subrc = 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" FROM is not available with table expressions"
			+ LINE_SEP + "    READ TABLE its_any FROM is_any TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    CHECK sy-subrc = 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" replacing INTO or REFERENCE INTO would require catching the exception CX_SY_TAB_LINE_NOT_FOUND: While"
			+ LINE_SEP + "    \" VALUE #( itab[ ... ] OPTIONAL ) and REF #( itab[ ... ] OPTIONAL ) work without exception handling, their" 
			+ LINE_SEP + "    \" behavior differs from READ TABLE if the table line is NOT found: In this case, READ TABLE preserves the old"
			+ LINE_SEP + "    \" value of the variable, while the VALUE #( ) and REF #( ) constructors would initialize the variable"
			+ LINE_SEP + "    READ TABLE its_any WITH KEY comp1 = 'abc' INTO DATA(ls_any)." 
			+ LINE_SEP + "    READ TABLE its_any WITH KEY comp1 = 'def' REFERENCE INTO lr_any." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" tables retrieved via functional call or constructor expression are not supported by table expressions"
			+ LINE_SEP + "    READ TABLE get_table( ) WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    lv_line_index = sy-tabix." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" READ TABLE is kept if SY-SUBRC is checked against a value other than 0, used multiple times, or passed on"
			+ LINE_SEP + "    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    CHECK sy-subrc = 4." 
			+ LINE_SEP + "    any_method( iv_subrc = sy-subrc )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" this would be equivalent to cl_abap_unit_assert=>assert_true( xsdbool( line_exists( its_any[ ... ] ) ) )"
			+ LINE_SEP + "    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS." 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_subrc( exp = 0 act = sy-subrc )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" comments in some places of the READ TABLE statement prevent the automated replacement"
			+ LINE_SEP + "    READ TABLE its_any \" very important comment which must never get lost"
			+ LINE_SEP + "          WITH TABLE KEY comp1 = iv_value1 TRANSPORTING NO FIELDS."
			+ LINE_SEP + "    CHECK sy-subrc = 0." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configReplaceWithAssign = new ConfigBoolValue(this, "ReplaceWithAssign", "Replace with ASSIGN itab[ ... ]", true);
	final ConfigBoolValue configReplaceWithLineExists = new ConfigBoolValue(this, "ReplaceWithLineExists", "Replace with line_exists( itab[ ... ] )", true);
	final ConfigBoolValue configReplaceWithLineIndex = new ConfigBoolValue(this, "ReplaceWithLineIndex", "Replace with line_index( itab[ ... ] )", true);
	final ConfigBoolValue configUseComponentsKeyword = new ConfigBoolValue(this, "UseComponentsKeyword", "Keep optional keyword COMPONENTS after KEY name", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configReplaceWithAssign, configReplaceWithLineExists, configReplaceWithLineIndex, configUseComponentsKeyword };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ReadTableRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean skipOutsideMethodFunctionOrForm() {
		// ensure that we are inside a METHOD, FUNCTION or FORM, otherwise (esp. if the standalone version gets a small 
		// code snippet), we cannot be sure that the statement is not indeed followed by an evaluation of SY-TFILL or SY-TLENG
		return true; 
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null || !firstCode.matchesOnSiblings(true, "READ", "TABLE"))
			return false;

		boolean replaceWithAssign = configReplaceWithAssign.getValue();
		boolean replaceWithLineExists = configReplaceWithLineExists.getValue();
		boolean replaceWithLineIndex = configReplaceWithLineIndex.getValue();
		if (!replaceWithAssign && !replaceWithLineExists && !replaceWithLineIndex)
			return false;

		ReadTableCommand readTable;
		try {
			readTable = ReadTableCommand.create(command);
		} catch (UnexpectedSyntaxException e) {
			// the compiler allows for some READ TABLE variants that are simply not supported here 
			return false;  
		}
		ReadTableSelectType selectType = readTable.getSelectType();
		ReadTableResultType resultType = readTable.getResultType();

		// ----------------------------------------------------------------------
		// exclude cases that cannot be transformed for various reasons

		// exclude cases where the rule is deactivated for the result type 
		if (resultType == ReadTableResultType.ASSIGNING_FS && !replaceWithAssign)
			return false;
		if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS && !replaceWithLineExists && !replaceWithLineIndex)
			return false;

		// exclude cases in which the itab is returned by a function or a constructor expression, because those are not allowed in table expressions
		if (readTable.getTable().firstToken.isAnyKeyword(ABAP.constructorOperators) || readTable.getTable().firstToken.startsFunctionalMethodCall(false)) 
			return false;

		// exclude cases of READ TABLE ... FROM wa, for which there is no equivalent in table expressions; getWorkArea() is only checked for completeness 
		if (selectType == ReadTableSelectType.TABLE_KEY_FROM_WA || readTable.getWorkArea() != null)
			return false;
		
		// exclude cases of READ TABLE ... INTO wa, as well as READ TABLE ... REFERENCE INTO dref, because the resulting table  
		// expression 'wa = itab[ ... ]' or 'dref = REF #( itab[ ... ] )' would require exception handling for CX_SY_TAB_LINE_NOT_FOUND; 
		// also, table expressions would have no equivalent for transport_options with specified components in COMPARING and TRANSPORTING.
		// getCompareOptions() and getTransportOptions() are only checked for completeness 
		if (resultType == ReadTableResultType.INTO_WA || readTable.getCompareOptions() != null || readTable.getTransportOptions() != null)
			return false;
		
		// exclude cases of READ TABLE ... REFERENCE INTO dref, because the resulting table expression 'dref = REF #( itab[ ... ] )' 
		// would require exception handling for CX_SY_TAB_LINE_NOT_FOUND 
		if (resultType == ReadTableResultType.REFERENCE_INTO_DREF)
			return false;
		
		// exclude cases with BINARY SEARCH, because there is no equivalent for this in table expressions; and since ABAP cleaner cannot 
		// determine the table type or the primary key, it doesn't know whether without BINARY SEARCH, a table expression would result in 
		// a table scan and thus a performance issue.
		if (readTable.hasBinarySearchKeywords())
			return false;

		// exclude cases of READ TABLE ... WITH KEY ... COMPONENTS ... ASSIGNING (i.e. with a free key and an explicitly specified key name), 
		// because in table expressions, no explicit key can be specified for optimizing searches with a free key, and unlike line_exists and 
		// line_index, ASSIGN would then require that the specified components exactly match a table key, which ABAP cleaner cannot check.
		if (selectType == ReadTableSelectType.FREE_KEY && readTable.getKeyName() != null && resultType == ReadTableResultType.ASSIGNING_FS)
			return false;

		// exclude cases where SY-TFILL or SY-TLENG are evaluated, which are only filled by READ TABLE, but not by a table expression 
		ArrayList<Command> commandsReadingSyTFill = SyFieldAnalyzer.getSyFieldReadersFor(SyField.TFILL, command);
		if (commandsReadingSyTFill.size() > 0)
			return false;
		ArrayList<Command> commandsReadingSyTLeng = SyFieldAnalyzer.getSyFieldReadersFor(SyField.TLENG, command);
		if (commandsReadingSyTLeng.size() > 0)
			return false;

		// exclude cases where both (or none of) SY-SUBRC and SY-TABIX are evaluated after TRANSPORTING NO FIELDS, or either field is evaluated multiple times
		ArrayList<Command> commandsReadingSySubrc = SyFieldAnalyzer.getSyFieldReadersFor(SyField.SUBRC, command);
		int commandCountReadingSySubrc = commandsReadingSySubrc.size(); 
		ArrayList<Command> commandsReadingSyTabix = SyFieldAnalyzer.getSyFieldReadersFor(SyField.TABIX, command);
		int commandCountReadingSyTabix = commandsReadingSyTabix.size();
		Command commandReadingSyField = null;
		if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS) {
			// only proceed if exactly one evaluation of either SY-SUBRC or SY-TABIX was found
			if (commandCountReadingSySubrc + commandCountReadingSyTabix != 1) 
				return false;
			// only proceed if the SY- field evaluation is the next non-comment Command (otherwise, other logic could be executed on the table in between) 
			commandReadingSyField = (commandCountReadingSySubrc > 0) ? commandsReadingSySubrc.get(0) : commandsReadingSyTabix.get(0);
			if (commandReadingSyField != command.getNextNonCommentCommand())
				return false;
			// do NOT proceed if the SY- field evaluation is in a WHILE command etc., which could evaluate the SY- field multiple times after loop cycles
			if (commandReadingSyField.startsLoop()) { 
				return false;
			}
		}

		// exclude cases depending on usage of SY-SUBRC or SY-TABIX
		try {
			if (skipDueToSySubrcUsage(commandsReadingSySubrc, resultType) || skipDueToSyTabixUsage(commandsReadingSyTabix, resultType)) {
				return false;
			}
		} catch (UnexpectedSyntaxException e) {
			return false;
		}

		// exclude cases in which comments or pragmas would get lost
		// TODO: instead of 'return false', (optionally?) append those comments and pragmas to the destination Command?
		ArrayList<Token> commentOrPragmaOutsideOfTerms = readTable.getCommentOrPragmaOutsideOfTerms();
		if (commentOrPragmaOutsideOfTerms.size() == 1 && commentOrPragmaOutsideOfTerms.get(0).isCommentAfterPeriod()) {
			// allow cases with one final comment after the period, which can be kept (ASSIGN ...) or transferred, 
			// if the target Command does not yet end with a comment 
			if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS && commandReadingSyField.getLastToken().isComment()) {
				return false;
			}
		} else if (commentOrPragmaOutsideOfTerms.size() > 0) {
			return false;
		}
		
		// ----------------------------------------------------------------------
		// replace READ TABLE with a table expression

		Command changedCommand = null;
		if (replaceWithAssign && resultType == ReadTableResultType.ASSIGNING_FS) {
			if (replaceWithAssign(readTable, command)) {
				changedCommand = command;
			}
			
		} else if (replaceWithLineExists && resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS && commandCountReadingSySubrc == 1) {
			if (replaceWithLineExists(readTable, command, commandsReadingSySubrc.get(0))) {
				changedCommand = commandsReadingSySubrc.get(0);
			}
			
		} else if (replaceWithLineIndex && resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS && commandCountReadingSyTabix == 1) {
			if (replaceWithLineIndex(readTable, command, commandsReadingSyTabix.get(0))) {
				changedCommand = commandsReadingSyTabix.get(0);
			}
		}

		if (changedCommand == null) {
			return false;
			
		} else if (!readTable.wereAllRelevantTermsMoved()) {
			// a relevant part of the READ TABLE Command still belongs to the now-deleted Command 
			throw new UnexpectedSyntaxAfterChanges(this, changedCommand, "Unexpected syntax of READ TABLE: Not all relevant terms moved to new command.");
			
		} else {
			code.addRuleUse(this, changedCommand);
			// always align the components in the table expression
			AlignParametersRule alignParamRule = (AlignParametersRule)parentProfile.getRule(RuleID.ALIGN_PARAMETERS);
			alignParamRule.executeOn(code, changedCommand);
			return true;
		}
	}

	private boolean skipDueToSySubrcUsage(ArrayList<Command> commandsReadingSySubrc, ReadTableResultType resultType) throws UnexpectedSyntaxException {
		// exclude cases where SY-SUBRC is 
		// - used multiple times inside the supplied Command (only relevant for READ TABLE ... TRANSPORTING NO FIELDS), 
		// - compared to anything other than 0, or 
		// - used outside of a LogicalExpression

		// TODO: also consider CL_ABAP_UNIT_ASSERT=>ASSERT_SUBRC( ), =>ASSERT_RETURN_CODE( ), possibly with parameters!
		// They are already included in commandsReadingSySubrc. 

		// after READ TABLE ... ASSIGNING, we can have multiple commands that access SY-SUBRC; 
		// after READ TABLE ... TRANSPORTING NO FIELDS, it was already checked that there is only one such command 
		for (Command commandReadingSySubrc : commandsReadingSySubrc) {
			if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS && isSyFieldUsedTwiceIn(commandReadingSySubrc, SyField.SUBRC))
				return true;
			
			// check whether SY-SUBRC is used inside a logical expression
			ArrayList<LogicalExpression> comparisonsToSySubrc;
			try {
				comparisonsToSySubrc = findComparisonsToSySubrc(commandReadingSySubrc);
			} catch (UnexpectedSyntaxException e) {
				return true;
			}
			
			if (comparisonsToSySubrc.size() > 0) {
				// check whether SY-SUBRC is compared to anything other than 0
				for (LogicalExpression comparisonToSySubrc : comparisonsToSySubrc) {
					if (comparisonToSySubrc.getRelationalExpressionType() == RelationalExpressionType.PREDICATE_EXPRESSION) {
						// 'sy-subrc IS [NOT] INITIAL'. Note that in case of 'NOT sy-subrc IS INITIAL', the negation 'NOT ...'  
						// is an outer LogicalExpression, which can simply be kept unchanged: comparisonToSySubrc is then the 
						// inner LogicalExpression 'sy-subrc IS INITIAL' and will be changed to 'line_exists( )', 
						// so together with the outer LogicalExpression, we get 'NOT line_exists( )'
						Term relTerm2 = comparisonToSySubrc.getTerm2(); 
						// ensure that relTerm2 either reads 'INITIAL' or 'NOT INITIAL'
						if (relTerm2 != null && relTerm2.firstToken.textEquals("INITIAL") && relTerm2.firstToken == relTerm2.lastToken) {
							continue;
						} else if (relTerm2 != null && relTerm2.firstToken.textEquals("NOT") && relTerm2.lastToken.textEquals("INITIAL") && relTerm2.firstToken.getNextCodeSibling() == relTerm2.lastToken) {
							continue;
						} else {
							// unexpected case
							return true;
						}

					} else if (comparisonToSySubrc.getRelationalExpressionType() == RelationalExpressionType.COMPARISON) {
						Term compareTerm1 = comparisonToSySubrc.getTerm1();
						Term compareTerm2 = comparisonToSySubrc.getTerm2();
						Token comparisonOp = comparisonToSySubrc.getOperator();
						Term otherTerm = (compareTerm1.isSingleToken() && compareTerm1.firstToken.textEqualsAny(SyField.SUBRC.syField, SyField.SUBRC.systField)) ? compareTerm2 : compareTerm1;
						if (!otherTerm.isSingleToken() || !otherTerm.firstToken.textEquals("0") || !comparisonOp.textEqualsAny("=", "<>", "EQ", "NE")) {
							// skip READ TABLE command, because this comparison cannot be adjusted
							return true;
						}
					}
				}

			} else if (commandReadingSySubrc.getFirstToken().textEqualsAny("cl_abap_unit_assert=>assert_subrc(", "cl_abap_unit_assert=>assert_return_code(")) {
				if (commandReadingSySubrc.getFirstToken().hasChildren()) {
					// for now, skip all cases with parameters (even 'exp = 0' and 'act = sy-subrc');
					// alternatively, parameters could be analyzed with AlignParametersRule.getFunctionalCallParams(commandReadingSySubrc.getFirstToken())
					// to ensure SY-SUBRC is checked against 0
					return true;
					
				} else if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS) {
					// skip empty cases too, since 'cl_abap_unit_assert=>assert_true( xsdbool( line_exists( lts_any[ ... ] ) ) )' is rather complex itself
					return true;
				}

			} else if (resultType == ReadTableResultType.TRANSPORTING_NO_FIELDS) {
				// skip READ TABLE command, because SY-SUBRC is not used inside a LogicalExpression, so it cannot be adjusted
				// (e.g. if it is used in an assignment, as a parameter etc.)
				return true;
			}
		}
		return false;
	}

	private boolean skipDueToSyTabixUsage(ArrayList<Command> commandsReadingSyTabix, ReadTableResultType resultType) {
		// exclude cases where SY-TABIX is used multiple times inside the supplied Command (only relevant for READ TABLE ... TRANSPORTING NO FIELDS), 
		if (resultType != ReadTableResultType.TRANSPORTING_NO_FIELDS)
			return false;

		// after READ TABLE ... ASSIGNING, we can have multiple commands that access SY-TABIX; 
		// after READ TABLE ... TRANSPORTING NO FIELDS, it was already checked that there is only one such command 
		for (Command commandReadingSyTabix : commandsReadingSyTabix) {
			if (isSyFieldUsedTwiceIn(commandReadingSyTabix, SyField.TABIX)) {
				return true;
			}
		}
		return false;
	}

	private boolean isSyFieldUsedTwiceIn(Command commandReadingSyField, SyField syField) {
		Token token = commandReadingSyField.getFirstToken();
		
		// check whether SY-SUBRC is used twice inside the Command
		int useCount = 0;
		while (token != null) {
			if (token.isIdentifier() && token.textEqualsAny(syField.syField, syField.systField)) {
				++useCount;
				if (useCount > 1) {
					return true;
				}
			}
			token = token.getNextCodeToken();
		}
		return false;
	}

	private ArrayList<LogicalExpression> findComparisonsToSySubrc(Command command) throws UnexpectedSyntaxException {
		ArrayList<LogicalExpression> comparisonsToSySubrc = new ArrayList<>();

		Token token = command.getFirstToken();
		while (token != null) {
			Token lastInLogExpr = token.getLastTokenOfLogicalExpression();
			// already advance token now, so we can easily continue the loop
			token = token.getNextCodeToken();
			if (lastInLogExpr == null) 
				continue;

			// check whether SY-SUBRC is used in this logical expression (if not, it may be used in a later one!)
			LogicalExpression logExpr;
			logExpr = LogicalExpression.create(token, lastInLogExpr);
			LogicalExpression comparisonToSySubrc = logExpr.findComparisonWithAny(SyField.SUBRC.syField, SyField.SUBRC.systField);
			if (comparisonToSySubrc != null) {
				comparisonsToSySubrc.add(comparisonToSySubrc);
			}
		}
		return comparisonsToSySubrc;
	}

	private boolean replaceWithAssign(ReadTableCommand readTable, Command command) throws UnexpectedSyntaxAfterChanges {
		Token period = command.getLastCodeToken();
		
		// all tokens up to (but excluding) the period are first removed from the Command, so they can afterwards be inserted
		// again in the required order (note that they must then NOT be removed again)
		if (!removeAllTokensUpTo(period, command)) {
			return false;
		}
		
		// ASSIGN
		int sourceLineNum = period.sourceLineNum;
		period.insertLeftSibling(Token.createForAbap(period.lineBreaks, period.spacesLeft, "ASSIGN", sourceLineNum));
		period.setWhitespace(0, 0);

		insertTableExpressionLeftOf(period, readTable, false);

		// TO <fs> / TO FIELD-SYMBOL(<fs>)
		period.insertLeftSibling(Token.createForAbap(0, 1, "TO", sourceLineNum));
		Term resultVariable = readTable.getResultVariable();
		resultVariable.firstToken.setWhitespace();
		period.insertLeftSibling(resultVariable);

		// [CASTING] [ELSE UNASSIGN]
		Term assignOptions = readTable.getAssignOptions(); 
		if (assignOptions != null) {
			assignOptions.firstToken.setWhitespace();
			period.insertLeftSibling(assignOptions);
		}

		// if command ends with a final comment (esp. "#EC CI_...), this was preserved above
		
		return true;
	}
	
	private boolean removeAllTokensUpTo(Token period, Command command) throws IntegrityBrokenException {
		Term allTokens;
		period.copyWhitespaceFrom(command.getFirstCodeToken());
		try {
			allTokens = Term.createForTokenRange(command.getFirstToken(), period.getPrev());
		} catch (UnexpectedSyntaxException e) {
			return false;
		}
		allTokens.removeFromCommand(true);
		return true;
	}
	
	private boolean replaceWithLineExists(ReadTableCommand readTable, Command command, Command commandReadingSySubrc) throws UnexpectedSyntaxAfterChanges {
		LogicalExpression comparisonToSySubrc;
		Token startToken;
		Term comparisonTerm;
		boolean introduceNot = false;
		
		try {
			ArrayList<LogicalExpression> comparisonsToSySubrc = findComparisonsToSySubrc(commandReadingSySubrc);
			if (comparisonsToSySubrc.size() != 1) {
				return false;
			}
			comparisonToSySubrc = comparisonsToSySubrc.get(0);

			// get the comparison Term (except for possible enclosing parentheses)
			startToken = comparisonToSySubrc.getFirstTokenExceptOpeningParenthesis();
			Token lastToken = comparisonToSySubrc.getLastTokenExceptClosingParenthesis();
			comparisonTerm = Term.createForTokenRange(startToken, lastToken);

			if (comparisonToSySubrc.getRelationalExpressionType() == RelationalExpressionType.PREDICATE_EXPRESSION) {
				// skipDueToSySubrcUsage() already ensured that getCompareTerm2() either reads 'INITIAL' or 'NOT INITIAL'
				introduceNot = comparisonToSySubrc.getTerm2().firstToken.textEquals("NOT");

			} else { // RelationalExpressionType.COMPARISON
				// sy-subrc = 0 / sy-subrc <> 0 / 0 = sy-subrc etc.
				Token comparisonOp = comparisonToSySubrc.getOperator();
				introduceNot = comparisonOp.textEqualsAny("<>", "NE");
			}
		} catch (UnexpectedSyntaxException e) {
			return false;
		}

		// remove the READ TABLE command from the code
		removeReadTableCommand(command);

		// add '[NOT] line_exists( itab[ ... ] )' on the left of the comparison Term
		// - NOT 
		if (introduceNot) {
			Token notKeyword = Token.createForAbap(0, 1, "NOT", startToken.sourceLineNum);
			startToken.insertLeftSibling(notKeyword);
		}
		
		// - itab[ ... ]
		Term tableExpr = insertTableExpressionLeftOf(startToken, readTable, true);
		
		// - surround itab[ ... ] with line_exists( )
		tableExpr.firstToken.insertParenthesesUpTo(startToken, "line_exists(", ")");

		// remove the comparison Term
		comparisonTerm.removeFromCommand(false);
		
		// move the final comment, if any (esp. #EC CI_... comment)
		moveFinalComment(command, commandReadingSySubrc);

		return true;
	}

	private boolean replaceWithLineIndex(ReadTableCommand readTable, Command command, Command commandReadingSyTabix) throws UnexpectedSyntaxAfterChanges {
		// find the usage of SY-TABIX or SYST-TABIX inside the Command (it was already ensured that there is only one usage)
		Token token = commandReadingSyTabix.getFirstToken();
		Token syTabixToken = null;
		while (token != null) {
			if (token.isIdentifier() && token.textEqualsAny(SyField.TABIX.syField, SyField.TABIX.systField)) {
				syTabixToken = token;
				break;
			}
			token = token.getNextCodeToken();
		}
		
		// remove the READ TABLE command from the code
		removeReadTableCommand(command);

		// add 'line_index( itab[ ... ] )' on the left of the SY-TABIX Token
		// - itab[ ... ]
		Term tableExpr = insertTableExpressionLeftOf(syTabixToken, readTable, true);
		
		// - surround itab[ ... ] with line_index( )
		tableExpr.firstToken.insertParenthesesUpTo(syTabixToken, "line_index(", ")");

		// remove the SY-TABIX Token
		syTabixToken.removeFromCommand(false);

		// move the final comment, if any (esp. #EC CI_... comment)
		moveFinalComment(command, commandReadingSyTabix);

		return true;
	}

	private void removeReadTableCommand(Command command) throws UnexpectedSyntaxAfterChanges {
		// transfer line breaks from the READ TABLE command to the next Command (which may be a comment!)
		Command nextCommand = command.getNext();
		if (nextCommand.getFirstTokenLineBreaks() < command.getFirstTokenLineBreaks()) {
			nextCommand.getFirstToken().setLineBreaks(command.getFirstTokenLineBreaks());
			nextCommand.getParentCode().addRuleUse(this, nextCommand);
		}

		// remove the READ TABLE command from the code
		try {
			command.removeFromCode();
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}

	private Term insertTableExpressionLeftOf(Token anchor, ReadTableCommand readTable, boolean removeTermsFirst) throws UnexpectedSyntaxAfterChanges {
		ReadTableSelectType selectType = readTable.getSelectType();
		int sourceLineNum = anchor.sourceLineNum;

		// itab
		Term table = readTable.getTable();
		table.firstToken.setWhitespace();
		String tableName = table.lastToken.getText(); 
		if (tableName.endsWith("[]"))
			table.lastToken.setText(tableName.substring(0, tableName.length() - "[]".length()), false);
		anchor.insertLeftSibling(table);

		// ...[ ]
		Token openingBracket = Token.createForAbap(0, 0, "[", sourceLineNum);
		Token closingBracket = Token.createForAbap(0, 1, "]", sourceLineNum);
		anchor.insertLeftSibling(openingBracket, false, true);
		anchor.insertLeftSibling(closingBracket, false, true);
		Token writeAfter = openingBracket;
		
		Term keyName = readTable.getKeyName();
		boolean wasKeyNameSpecified = false;
		if (keyName != null) { 
			// KEY keyname  
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abentable_exp_itab_line.htm
			// the key name can even be specified for ReadTableSelectType.FREE_KEY, because
			// - cases for ASSIGN were already excluded (here it is not possible to specify an explicit table key for  
			//    optimizing searches using secondary keys, whereas this is possible in READ TABLE)
			// - for line_exists( ) and line_index( ), a key name can be provided even if not all table key components 
			//   are specified, or additional components are provided
			Token keyKeyword = Token.createForAbap(0, 1, "KEY", sourceLineNum);
			writeAfter.insertNext(keyKeyword);
			if (removeTermsFirst)
				keyName.removeFromCommand(true);
			keyKeyword.insertNext(keyName);
			wasKeyNameSpecified = true;
			writeAfter = keyName.lastToken;
		} 

		if (selectType == ReadTableSelectType.INDEX) {
			if (keyName != null) {
				// INDEX 
				Token indexKeyword = Token.createForAbap(0, 1, "INDEX", sourceLineNum);
				writeAfter.insertNext(indexKeyword);
				writeAfter = indexKeyword;
			}
			// idx
			Term indexNum = readTable.getIndexNum();
			if (removeTermsFirst)
				indexNum.removeFromCommand(true);
			indexNum.firstToken.setWhitespace();
			writeAfter.insertNext(indexNum);

		} else {
			Term componentAssignments = readTable.getComponentAssignments();
			int lineBreaksBeforeAssignments = wasKeyNameSpecified ? 1 : -1;
			int defaultIndent = openingBracket.getEndIndexInLine() + 1;
			if (wasKeyNameSpecified && selectType == ReadTableSelectType.TABLE_KEY && componentAssignments != null 
					&& configUseComponentsKeyword.getValue()) {
				// COMPONENTS: use a lineBreak if the READ TABLE Command had one
				Token oldComponentsKeyword = readTable.getComponentsKeyword();
				int lineBreaks = (oldComponentsKeyword != null && oldComponentsKeyword.lineBreaks > 0) ? 1 : 0;
				int spacesLeft = (lineBreaks == 0) ? 1 : defaultIndent;
				Token componentsKeyword = Token.createForAbap(lineBreaks, spacesLeft, "COMPONENTS", sourceLineNum);
				writeAfter.insertNext(componentsKeyword);
				writeAfter = componentsKeyword;
				if (lineBreaks == 1) {
					lineBreaksBeforeAssignments = 0;
				}
			}			
			// {comp_name1|(name1)} = operand1 {comp_name2|(name2)} = operand2 ... 
			if (componentAssignments != null) {
				if (removeTermsFirst)
					componentAssignments.removeFromCommand(true);
				if (lineBreaksBeforeAssignments < 0) {
					lineBreaksBeforeAssignments = (componentAssignments.firstToken.lineBreaks == 0) ? 0 : 1; 
				} 
				Token assignmentsStart = componentAssignments.firstToken;
				if (assignmentsStart.textEquals("=")) {
					// in the special case of 'WITH KEY = operator1', insert a 'table_line' Token
					assignmentsStart.setWhitespace();
					Token tableLineToken = Token.createForAbap(0, 1, "table_line", sourceLineNum);
					writeAfter.insertNext(tableLineToken);
					writeAfter = tableLineToken;
					assignmentsStart = tableLineToken;
				}
				if (lineBreaksBeforeAssignments == 0) {
					assignmentsStart.setWhitespace();
				} else if (lineBreaksBeforeAssignments == 1) {
					assignmentsStart.setWhitespace(1, defaultIndent);
				}
				writeAfter.insertNext(componentAssignments);
			}
		}
		
		try {
			return Term.createForTokenRange(table.firstToken, closingBracket);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}

	private void moveFinalComment(Command command, Command commandReadingSySubrc) throws UnexpectedSyntaxAfterChanges {
		// moves the comment from the end of the READ TABLE command to the target command,
		// esp. pseudo comments "#EC CI_SORTSEQ, "#EC CI_STDSEQ, and "#EC CI_HASHSEQ, which suppress performance findings 
		// on (possible) sequential reads on sorted / standard / hashed tables for both READ TABLE and table expressions)
		if (command.getLastToken().isComment() && !commandReadingSySubrc.getLastToken().isComment()) {
			Token finalComment = command.getLastToken();
			finalComment.removeFromCommand(false, true);
			finalComment.setWhitespace();
			commandReadingSySubrc.getLastToken().insertRightSibling(finalComment);
		}
	}
}