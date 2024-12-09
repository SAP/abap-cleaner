package com.sap.adt.abapcleaner.rules.alignment;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

import java.time.LocalDate;
import java.util.*;

public class AlignParametersRule extends RuleForCommands {
	public static final String DISPLAY_NAME = "Align parameters and components";
	public static final String OPTION_NAME_KEEP_OTHER_ONE_LINERS = "Keep other one-liners";
	
	public enum Columns {
		// LET ... IN expressions
		LET_KEYWORD, 
		LET_PARAMETER, 
		LET_ASSIGNMENT_OP, 
		LET_EXPRESSION,
		LET_IN_KEYWORD, // only used if "IN" is at the beginning of a line (otherwise, "IN" simply stays after the last LET_EXPRESSION)
		// keywords like EXPORTING, IMPORTING, ..., COMPONENTS
		KEYWORD, 
		// assignments
		PARAMETER, 
		ASSIGNMENT_OP, 
		EXPRESSION;
		
		public int getValue() { return this.ordinal(); }
	}
	public static final int MAX_COLUMN_COUNT = 9;

	public enum ContentType {
		FUNCTIONAL_CALL_PARAMS, 
		PROCEDURAL_CALL_PARAMS, 
		TABLE_KEY, 
		GROUP_KEY,
		CONSTRUCTOR_EXPR,
		ROW_IN_VALUE_OR_NEW_CONSTRUCTOR;
		
		// public int getValue() { return this.ordinal(); }
	}

   private final static RuleReference[] references = new RuleReference[] {
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Align parameters", "#align-parameters"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Keep parameters behind the call", "#keep-parameters-behind-the-call"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "If you break, indent parameters under the call", "#if-you-break-indent-parameters-under-the-call"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Line-break multiple parameters", "#line-break-multiple-parameters"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Stick to a reasonable line length", "#stick-to-a-reasonable-line-length"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Indent and snap to tab", "#indent-and-snap-to-tab"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Indent in-line declarations like method calls", "#indent-in-line-declarations-like-method-calls"),
   };

   private class TableStart {
   	public final int startIndent;
   	public final boolean continueOnSameLine;
   	public final boolean forceTableToNextLine;
   	public final int earlyIndent;
   	
   	private TableStart(int startIndent, boolean continueOnSameLine, boolean forceTableToNextLine, int earlyIndent) {
   		this.startIndent = startIndent;
   		this.continueOnSameLine = continueOnSameLine;
   		this.forceTableToNextLine = forceTableToNextLine;
   		this.earlyIndent = earlyIndent;
   	}
   }
   
	@Override
	public RuleID getID() { return RuleID.ALIGN_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return DISPLAY_NAME; }

	@Override
	public String getDescription() {
		return "Aligns parameter assignments in method calls, as well as component assignments in VALUE expressions, table expressions, table keys etc.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_LOGICAL_EXPRESSIONS } ; }

	@Override
	public boolean isEssential() { return true; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_parameters." 
			+ LINE_SEP + "    lts_table = cl_any_class=>create_table(" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        iv_contract_id = lo_contract1->ms_data-contract_id" 
			+ LINE_SEP + "        iv_contract_type = if_any_interface=>co_any_contract_type" 
			+ LINE_SEP + "        iv_item_type = lo_item->get_item_data( )-item_type )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" commented-out parameters are aligned, too:" 
			+ LINE_SEP + "    lts_other_table = cl_other_class=>create_table(" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        iv_item_key = '12345'" 
			+ LINE_SEP + "        iv_category = 'ABC'" 
			+ LINE_SEP + "*        iv_size = 100'" 
			+ LINE_SEP + "        iv_name = 'ANY_NAME'" 
			+ LINE_SEP + "        iv_qty = 8 )." 
			+ LINE_SEP 
			+ LINE_SEP + "    CALL METHOD procedural_call_example" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        iv_contract_id = lo_contract1->ms_data-contract_id" 
			+ LINE_SEP + "        iv_item_key = '13579'" 
			+ LINE_SEP + "      IMPORTING" 
			+ LINE_SEP + "        ev_category = lv_any_category" 
			+ LINE_SEP + "        ev_item_type = lv_any_item_type" 
			+ LINE_SEP + "      CHANGING" 
			+ LINE_SEP + "        cv_qty = lv_quantity." 
			+ LINE_SEP 
			+ LINE_SEP + "    ets_table = VALUE #( date = gc_any_date" 
			+ LINE_SEP + "                           id = gc_any_id" 
			+ LINE_SEP 
			+ LINE_SEP + "                           ( item_name = 'ANY'" 
			+ LINE_SEP + "*                      size = 'M'" 
			+ LINE_SEP + "                              quantity = 1 )" 
			+ LINE_SEP + "                             ( item_name = 'OTHER'" 
			+ LINE_SEP + "                                quantity = 2" 
			+ LINE_SEP + "                                reference_id = '12345' )" 
			+ LINE_SEP + "                                   ( item_name = 'THIRD'" 
			+ LINE_SEP + "                                  quantity = 3 ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ets_fulfillment = VALUE #( event_date = gc_any_event_date" 
			+ LINE_SEP + "                                amount = gc_any_amount" 
			+ LINE_SEP + "                               ( fulfillment_number = lc_fulfill_num_1  qty = lc_fulfill_qty_1 )" 
			+ LINE_SEP + "                                 ( fulfillment_number = lc_fulfill_num_2  qty = lc_fulfill_qty_2 )" 
			+ LINE_SEP + "                                     ( fulfillment_number = lc_fulfill_num_3  qty = lc_fulfill_qty_3 ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" tabular style improves readability, therefore some overlength may be tolerated here (max. line length B):" 
			+ LINE_SEP + "    mts_table = VALUE #( ( item_key = '20220030000101'  event_date = '20220301'  total_qty = '30'  qty_unit = 'DAY'  amount = '1000.00'  currency = 'EUR' )" 
			+ LINE_SEP + "                         ( item_key = '20220040000101'  event_date = '20220401'  total_qty = '30'  qty_unit = 'DAY'  amount = '1500.00'  currency = 'EUR' )" 
			+ LINE_SEP + "                         ( item_key = '20220050000101'  event_date = '20220501'  total_qty = '30'  qty_unit = 'DAY'  amount = '2000.00'  currency = 'EUR' ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    READ TABLE lt_any_table_name ASSIGNING <ls_table_row>" 
			+ LINE_SEP + "         WITH KEY field1 = ls_any_structure-field1" 
			+ LINE_SEP + "                  fld2 = ls_any_structure-fld2" 
			+ LINE_SEP + "                  long_field_name3 = ls_any_structure-long_field_name_3." 
			+ LINE_SEP 
			+ LINE_SEP + "    result = VALUE #( BASE result ( id = 1 name = 'abc' ) )."
			+ LINE_SEP 
			+ LINE_SEP + "    LOOP AT lt_table ASSIGNING <fs>" 
			+ LINE_SEP + "         GROUP BY ( key1 = <fs>-any_component key2 = get_value( <fs>-other_component )" 
			+ LINE_SEP + "                     indx = GROUP INDEX count = GROUP SIZE )" 
			+ LINE_SEP + "         ASSIGNING FIELD-SYMBOL(<group>)." 
			+ LINE_SEP 
			+ LINE_SEP + "      cl_demo_output=>write( |{ <group>-indx } { <group>-key1 } { <group>-key2 } { <group>-count }| )." 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length A (normal)", "", 80, 120, ABAP.MAX_LINE_LENGTH);
	final ConfigIntValue configMaxLineLengthForSingleLine = new ConfigIntValue(this, "MaxLineLengthForSingleLine", "Maximum line length B (for tabular style)", "", 80, 160, ABAP.MAX_LINE_LENGTH);
	final ConfigIntValue configMaxParamCountBehindProceduralCall = new ConfigIntValue(this, "MaxParamCountBehindProceduralCall", "Procedural call: continue behind the call for up to", "parameters", 0, 0, 100);
	final ConfigIntValue configMaxParamCountBehindFunctionalCall = new ConfigIntValue(this, "MaxParamCountBehindFunctionalCall", "Functional call: continue behind the call for up to", "parameters", 0, 100, 100);
	final ConfigBoolValue configPutProceduralCallKeywordsOnOwnLine = new ConfigBoolValue(this, "PutProceduralCallKeywordsOnOwnLine", "Procedural call: put keywords (EXPORTING etc.) on own line", false);
	final ConfigBoolValue configPutFunctionalCallKeywordsOnOwnLine = new ConfigBoolValue(this, "PutFunctionalCallKeywordsOnOwnLine", "Functional call: put keywords (EXPORTING etc.) on own line", false);
	final ConfigBoolValue configAlignAssignments = new ConfigBoolValue(this, "AlignAssignments", "Align assignments", true, true, LocalDate.of(2023, 3, 3));
	final ConfigBoolValue configAlignAcrossTableRows = new ConfigBoolValue(this, "AlignAcrossTableRows", "Align assignments across rows of table constructors", true, false, LocalDate.of(2023, 6, 9));
	final ConfigEnumValue<ComponentsOnSingleLine> configKeepComponentsOnSingleLine = new ConfigEnumValue<ComponentsOnSingleLine>(this, "KeepParametersOnSingleLine", "Table rows: Keep multiple components on single line",
																												new String[] { "never", "if maximum line length B is observed", "always" }, ComponentsOnSingleLine.values(), ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
	final ConfigEnumValue<ComponentsOnSingleLine> configKeepOtherOneLiners = new ConfigEnumValue<ComponentsOnSingleLine>(this, "KeepOtherOneLiners", OPTION_NAME_KEEP_OTHER_ONE_LINERS,
																												new String[] { "never", "if maximum line length A is observed", "always" }, ComponentsOnSingleLine.values(), ComponentsOnSingleLine.NEVER, ComponentsOnSingleLine.NEVER, LocalDate.of(2024, 1, 1));
	final ConfigEnumValue<ContentLeftOfAssignOp> configAllowContentLeftOfAssignOp = new ConfigEnumValue<ContentLeftOfAssignOp>(this, "AllowContentLeftOfAssignOp", "Allow line starts left of assignment operator",
																															new String[] { "never", "only to keep maximum line length", "always" }, ContentLeftOfAssignOp.values(), ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH, ContentLeftOfAssignOp.NEVER, LocalDate.of(2022, 3, 19));

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configMaxLineLengthForSingleLine, configMaxParamCountBehindProceduralCall, configMaxParamCountBehindFunctionalCall, 
																						configPutProceduralCallKeywordsOnOwnLine, configPutFunctionalCallKeywordsOnOwnLine, configAlignAssignments, configAlignAcrossTableRows, 
																						configKeepComponentsOnSingleLine, configKeepOtherOneLiners, configAllowContentLeftOfAssignOp };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private ContentLeftOfAssignOp getConfigAllowContentLeftOfAssignOp() {
		return ContentLeftOfAssignOp.forValue(configAllowContentLeftOfAssignOp.getValue());
	}

	public AlignParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	public boolean executeOn(Code code, Command command) throws UnexpectedSyntaxAfterChanges {
		return executeOn(code, command, ABAP.NO_RELEASE_RESTRICTION);
	}
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (command.isCommentLine())
			return false;
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null)
			return false;

		boolean changed = false;

		// note that cases processed here should be skipped in the AlignWithSecondWordRule 
		// (exception: RAISE ... MESSAGE ... EXPORTING ..., where the MESSAGE section benefits from AlignWithSecondWordRule)
		
		// align CALL METHOD|FUNCTION|BADI without parentheses (a call with parentheses will simply be handled by the "all other cases" section below)
		// also align CREATE OBJECT, if it was not replaced by a NEW constructor by the CreateObjectRule, 
		// and RECEIVE RESULTS FROM FUNCTION func [KEEPING TASK] (with IMPORTING, TABLES, CHANGING, EXCEPTIONS)
		Token period = command.getLastNonCommentToken();
		if (firstCode.matchesOnSiblings(true, "CALL", "METHOD|FUNCTION|BADI")
		 || firstCode.matchesOnSiblings(true, "CREATE", "OBJECT")
		 || firstCode.matchesOnSiblings(true, "RECEIVE", "RESULTS", "FROM", "FUNCTION")) {
			// CALL FUNCTION has a lot of potential parameters, so we just search the siblings(!) for the first keyword. 
			// In case of CALL METHOD identifier( ... ), this is deliberately NOT found among the siblings 
			// (see ABAP Reference: "CALL FUNCTION - Quick reference")
			Token firstKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "EXPORTING|IMPORTING|TABLES|CHANGING|RECEIVING|EXCEPTIONS");
			if (firstKeyword != null && firstKeyword.getParent() == null) { // otherwise, continue below
				Token parentToken = firstKeyword.getPrevCodeSibling();
				int baseIndent = command.getFirstToken().getStartIndexInLine(); 
				if (alignParams(code, command, parentToken, period, baseIndent, baseIndent, ContentType.PROCEDURAL_CALL_PARAMS)) {
					changed = true;
				}
			} 
		}

		// align parameters of "RAISE [RESUMABLE] EXCEPTION TYPE cx_class ... EXPORTING ...", "RAISE SHORTDUMP TYPE cx_class ... EXPORTING ..."
		// and "RAISE EVENT evt [EXPORTING p1 = a1 p2 = a2 ...]";
		// also processes cases with USING MESSAGE or MESSAGE ..., but only aligns starting from the EXPORTING keyword
		if (firstCode.matchesOnSiblings(true, "RAISE", TokenSearch.makeOptional("RESUMABLE"), "EXCEPTION", "TYPE", TokenSearch.ANY_IDENTIFIER, TokenSearch.ASTERISK, "EXPORTING")
		 || firstCode.matchesOnSiblings(true, "RAISE", "SHORTDUMP", "TYPE", TokenSearch.ANY_IDENTIFIER, TokenSearch.ASTERISK, "EXPORTING")
		 || firstCode.matchesOnSiblings(true, "RAISE", "EVENT", TokenSearch.ANY_IDENTIFIER, "EXPORTING")) {
			// (see ABAP Reference: "RAISE EXCEPTION" and "RAISE SHORTDUMP")
			Token firstKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "EXPORTING");
			if (firstKeyword != null) {
				Token parentToken = firstKeyword.getPrevCodeSibling();
				int baseIndent = determineBaseIndentForRaise(command, parentToken);
				if (alignParams(code, command, parentToken, period, baseIndent, baseIndent, ContentType.PROCEDURAL_CALL_PARAMS)) {
					changed = true;
				}
			}
		}

		// align READ|DELETE TABLE ... WITH [TABLE] KEY ...
		if (firstCode.matchesOnSiblings(true, "READ|DELETE", "TABLE")) {
			Token identifier = getFirstComponentOfTableKey(firstCode);
			if (identifier != null) {
				// find the end of the component assignment sequence
				// for BINARY SEARCH, REFERENCE INTO, TRANSPORTING NO FIELDS, but because of .getLastTokenOnSiblings, we keep it simple
				Token end = identifier.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "BINARY|INTO|ASSIGNING|REFERENCE|TRANSPORTING|.");
				if (end != null && identifier != end) {
					int baseIndent = (identifier.lineBreaks > 0) ? identifier.getStartIndexInLine() : identifier.getPrev().getEndIndexInLine() + 1;
					if (alignParams(code, command, identifier.getPrev(), end, baseIndent, baseIndent, ContentType.TABLE_KEY)) {
						changed = true;
					}
				}
			}
		}

		// align assignments in parentheses (including parentheses as parts of the above cases!)
		// if line starts are allowed left of an assignment operator to preserve maximum line length, perform 2 passes,
		// because we can't predict in advance what happens to inner constructs, and the first pass may prove too radical 
		// (e.g. if inner lists had to be broken into multiple lines even with the minimum indent); this can then be revised  
		// in the second pass that moves the construct back from the 'minimum indent' to the 'base indent' or even behind the call
		ContentLeftOfAssignOp contentLeftOfAssignOp = getConfigAllowContentLeftOfAssignOp();
		int passCount = (contentLeftOfAssignOp == ContentLeftOfAssignOp.NEVER) ? 1 : 2;
		for (int pass = 0; pass < passCount; ++pass) {
			HashSet<Token> allRowsAlignedForParent = new HashSet<>();
			boolean alignAcrossTableRows = configAlignAcrossTableRows.getValue();

			Token token = command.getFirstToken();
			while (token != null) {
				Token end = token.getEndOfParamsOrComponentsList(); 
				if (end != null) {
					ContentType contentType = determineContentType(token);

					// determine the base indent and the minimum indent (which may be left of an assignment operator); 
					// one of these indents is used in case putting parameters etc. behind the opening bracket would exceed line length
					int baseIndent = determineBaseIndent(token, contentType);
					int minimumIndent = determineMinimumIndent(token, contentType);
					
					// depending on configuration, base indent or minimum indent may be enforced
					if (contentLeftOfAssignOp == ContentLeftOfAssignOp.ALWAYS) {
						baseIndent = minimumIndent;
					} else if (contentLeftOfAssignOp == ContentLeftOfAssignOp.NEVER) {
						minimumIndent = baseIndent;
					} else if (contentLeftOfAssignOp == ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH) {
						// keep different values for base indent and minimum indent to decide depending on content width  
					}
		
					if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR && allRowsAlignedForParent.contains(token.getParent())) {
						// skip this row, because it was already aligned with the first row
					} else if (alignParams(code, command, token, end, baseIndent, minimumIndent, contentType)) {
						changed = true;
					}
					
					// ensure that further table rows of this constructor will be skipped 
					if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR && alignAcrossTableRows) {
						allRowsAlignedForParent.add(token.getParent());
					}
				}
				token = token.getNext();
			}
			// only do a second pass if something was changed - otherwise the second pass will not change anything, either 
			if (!changed)
				break;
		}
		return changed;
	}

	private Token getFirstComponentOfTableKey(Token firstCode) {
		Token keyToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "WITH", TokenSearch.makeOptional("TABLE"), "KEY");
		if (keyToken == null)
			return null;

		// find the first identifier that is followed by a "="
		Token identifier = keyToken.getNextCodeSibling();
		if (identifier.textEquals("=")) // "WITH KEY = dobj [BINARY SEARCH]"
			return null;
		
		Token next = identifier.getNextCodeSibling();
		if (next == null)
			return null;
		
		if (!next.textEquals("=")) {
			next = next.getNextCodeSibling();
			if (next != null && next.isKeyword("COMPONENTS"))
				next = next.getNextCodeSibling();
			if (next == null)
				return null;
			identifier = next;
		}
		
		next = identifier.getNextCodeSibling();
		if (next == null || !next.textEquals("=")) // "WITH KEY dobj [BINARY SEARCH]"
			return null;
		
		return identifier;
	}

	private ContentType determineContentType(Token token) {
		if (token.textEquals("(")) { 
			// stand-alone parenthesis, as opposed to "method_name(" or "type_name(" or "#(" 
			return token.opensGroupKey() ? ContentType.GROUP_KEY : ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR;
		}
		
		Token prev = token.getPrevCodeSibling();
		if (prev != null && prev.isKeyword("METHOD")) {
			// e.g. 'CALL METHOD any_method( ... )'  
			return ContentType.PROCEDURAL_CALL_PARAMS;

		} else if (prev != null && prev.isAnyKeyword("NEW", "VALUE")) {
			// e.g. 'result = VALUE ty_s_any( ... )', 'param = VALUE #( ... )' 
			return ContentType.CONSTRUCTOR_EXPR;

		} else {
			// e.g. 'result = any_method( ... )', 'SELECT ... FROM I_AnyView( P_AnyParam = ... )'
			return ContentType.FUNCTIONAL_CALL_PARAMS;
		}
	}

	private int determineBaseIndentForRaise(Command command, Token parentToken) {
		// by default, use the indent of the Command's first Token
		int baseIndent = command.getFirstToken().getStartIndexInLine(); 
		
		// if before the EXPORTING section, there is any keyword at line start (esp. MESSAGE), make baseIndent relative to that keyword 
		// (esp. for cases of RAISE ... MESSAGE ... which may have been processed by the AlignWithSecondWordRule) - example:
		// RAISE EXCEPTION TYPE cx_any_exception
		//       MESSAGE ID ... NUMBER ... 
		//               WITH ... 
		//       EXPORTING ... 
		Token token = command.getFirstToken().getNextCodeSibling();
		while (token != null && token != parentToken && (!token.isKeyword() || token.lineBreaks == 0)) {
			token = token.getNextCodeSibling();
		}
		if (token != null && token.isKeyword() && token.lineBreaks > 0) {
			// determineTableStart will put EXPORTING at the position baseIndent + ABAP.INDENT_STEP, therefore subtract it for now: 
			baseIndent = token.spacesLeft - ABAP.INDENT_STEP; 
		}
		
		return baseIndent;
	}

	private int determineBaseIndent(Token token, ContentType contentType) {
		if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR || contentType == ContentType.GROUP_KEY) {
			// rows in a VALUE or NEW constructor expression were already moved when the outer VALUE or NEW constructor itself was processed 
			return token.getStartIndexInLine();
		} else { 
			// parameters in a functional call or a constructor expression:
			// move to the beginning of the call chain, while that is on the same line; for example,    
			// move to the "lo_factory..." Token in "lv_result = lo_factory=>get( )->get_utility( )->any_method( iv_param = ..."
			Token testToken = token;
			while (!testToken.isFirstTokenInLine() && testToken.closesLevel() && testToken.getPrev() != null && testToken.getPrev().getOpensLevel()) {
				testToken = testToken.getPrev();
			}
			// move to the beginning of CALL METHOD 
			while (!testToken.isFirstTokenInLine() && testToken.getPrev() != null && testToken.getPrev().isAnyKeyword("CALL", "METHOD")) {
				testToken = testToken.getPrev();
			}
			return testToken.getStartIndexInLine();
		}
	}

	private int determineMinimumIndent(Token token, ContentType contentType) {
		if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR || contentType == ContentType.GROUP_KEY) {
			// rows in a VALUE or NEW constructor expression were already moved when the outer VALUE or NEW constructor itself was processed 
			return token.getStartIndexInLine();
		} else {
			// return the start index of the first token in same line; however, do not go to parent level, e.g. in case of 
			// "ls_struc = VALUE #( inner_struc = VALUE #( component = '1' ...", keep "component" right of "inner_struc"
			Token testToken = token;
			while (testToken.lineBreaks == 0 && testToken.getPrev() != null && testToken.getPrev() != token.getParent())
				testToken = testToken.getPrev();
			
			// however, if the start of the line assigns to a very short variable (e.g. "exp = ..." or "act = ..."), 
			// move the minimum indent right of the assignment operator, since otherwise, the layout looks awkward 
			if (testToken.isIdentifier() && testToken.getTextLength() <= 4) {
				Token assignOp = testToken.getNext();
				if (assignOp != null && assignOp.isAssignmentOperator() && assignOp.lineBreaks == 0 && assignOp.spacesLeft <= 2) {
					Token next = assignOp.getNext();
					if (next != null && next.lineBreaks == 0 && next.spacesLeft <= 1) {
						testToken = next;
					}
				}
			}

			return testToken.getStartIndexInLine();
		}
	}

	public final boolean alignParams(Code code, Command command, Token parentToken, Token endToken, int baseIndent, int minimumIndent, ContentType contentType) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;

		// -------------------------------------------------------------------
		// 1. prepare alignment 

		// build an AlignTable and a list of 'otherLineStarts' (i.e. comment lines, inner parentheses)
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		ArrayList<Token> parentTokens = new ArrayList<Token>();
		ArrayList<Token> otherLineStarts = new ArrayList<Token>();
		boolean alignAcrossTableRows = configAlignAcrossTableRows.getValue();
		try {
			// usually, we only consider the range from parentToken to endToken; however, for table constructors, the components  
			// of ALL rows ( ... ) ( ... ) can be added to the same AlignTable; in this case, parentToken is only the start of the FIRST row
			Token curParent = parentToken;
			Token curEnd = endToken;
			do {
				parentTokens.add(curParent);
				buildAlignTable(table, curParent, curEnd, contentType, otherLineStarts);
				setForcedLineBreaks(table, contentType);

				// in case of a table constructor, search the parentheses for the next row
				if (contentType != ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR || !alignAcrossTableRows)
					break;
				Token token = findStartOfNextRow(curEnd);
				if (token == null)
					break;
				
				// enhance the AlignTable with the next row
				curParent = token;
				curEnd = token.getNextSibling();
			} while(true);
			
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}

		// anything to align?
		if (table.isEmpty() && otherLineStarts.size() == 0) {
			return false;
		}

		// if configured, join assignment operators and expressions into previous columns, thus preventing the alignment
		// of assignments, or removing existing alignments
		if (!configAlignAssignments.getValue() ) {
			Columns[] columnsToJoinIntoPrevious = new Columns[] { Columns.LET_ASSIGNMENT_OP, Columns.LET_EXPRESSION, Columns.ASSIGNMENT_OP, Columns.EXPRESSION };
			try {
				for (Columns columnToJoin : columnsToJoinIntoPrevious) {
					Command[] changedCommands = table.getColumn(columnToJoin.getValue()).joinIntoPreviousColumns(true); 
					changed |= (changedCommands != null) && (changedCommands.length > 0);
				}
			} catch (UnexpectedSyntaxException ex) {
				if (changed)
					throw new UnexpectedSyntaxAfterChanges(this, ex);
				else 
					(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
				return changed;
			}
		}

		// determine the table start (startIndent and continueOnSameLine) 
		TableStart tableStart = determineTableStart(parentToken, baseIndent, minimumIndent, contentType, table, otherLineStarts);
		
		// determine whether to keep the whole content of the parentheses / brackets on one line
		
		boolean keepOnSingleLine = false;
		Token parentTokenOfFirstLine = null;
		if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR && alignAcrossTableRows) {
			// for rows that can be kept on one line, remove the corresponding assignments from the AlignTable
			for (Token curParent : parentTokens) {
				if (determineKeepOnSingleLine(curParent, curParent.getNextSibling(), tableStart.earlyIndent, true)) {
					table.removeAllLinesOfParent(curParent);
				} else if (parentTokenOfFirstLine == null) {
					// remember the parentToken of the first line that remains in the AlignTable (if any)
					parentTokenOfFirstLine = curParent;
				}
			}
		} else {
			// only the range parentToken ... endToken is included in the AlignTable; determine whether it can be kept on one line
			boolean forTabularStyle = (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR || isTableOfSingleComponents(parentToken, endToken)); 
			keepOnSingleLine = determineKeepOnSingleLine(parentToken, endToken, tableStart.earlyIndent, forTabularStyle);
		}
		if (parentTokenOfFirstLine == null) {
			parentTokenOfFirstLine = parentToken;
		}
		
		// -------------------------------------------------------------------
		// 2. perform alignment changes 

		// a) align the table - except when it contains multiple components in one line, or when its components and values  
		// may be manually aligned across multiple table rows (even with one component only), as in
		//     VALUE #( ( id =   1 )
		//              ( id = 100 ) ).
		boolean skipAlign = keepOnSingleLine && (table.getLineCount() > 1 || contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR);
		Command[] changedCommands = null;
		if (!table.isEmpty() && !skipAlign) {
			int startLineBreaks;
			if (tableStart.forceTableToNextLine) {
				startLineBreaks = 1;
			} else if (tableStart.continueOnSameLine && table.getFirstToken() == parentTokenOfFirstLine.getNext()) {
				startLineBreaks = 0;
			} else if (tableStart.continueOnSameLine && table.getFirstToken().lineBreaks == 0 && table.getFirstTokenColumnIndex() != Columns.PARAMETER.getValue()) {
				// e.g., keep COMPONENTS on the same line in "its_any[ KEY seckey COMPONENTS comp1 = ..." and only break for "comp2 = ..."
				startLineBreaks = 0;
			} else {
				startLineBreaks = 1;
			}

			changedCommands = table.align(tableStart.startIndent, startLineBreaks, true);
			if (changedCommands != null && changedCommands.length > 0) { // changedCommands can only contain this current command
				changed = true;
			}
		}

		// prepare alignment of asterisk comment lines
		AlignColumn letInKeywordColumn = table.getColumn(Columns.LET_IN_KEYWORD.getValue());
		AlignColumn parameterColumn = table.getColumn(Columns.PARAMETER.getValue());
		AlignColumn assignmentOpColumn = table.getColumn(Columns.ASSIGNMENT_OP.getValue());
		AlignColumn expressionColumn = table.getColumn(Columns.EXPRESSION.getValue());
		int parameterIndent = parameterColumn.getEffectiveIndent();
		int assignmentOpIndent = assignmentOpColumn.getEffectiveIndent();
		int expressionIndent = expressionColumn.getEffectiveIndent();
		boolean mayAlignAsteriskComments = !table.isEmpty()  
													&& !parameterColumn.isEmpty() && parameterIndent > 0
													&& !assignmentOpColumn.isEmpty() && assignmentOpIndent > 0
													&& !expressionColumn.isEmpty() && expressionIndent > 0;
													
		// b) align other lines that were found inside the parentheses or brackets: " comment lines, inner parentheses, assignments in * comment lines
		// If there is a LET ... IN expression, align with the parameters (which could be right of the .startIndent of the "IN" keyword)
		int otherLineIndent = letInKeywordColumn.isEmpty() ? tableStart.startIndent : parameterIndent; 
		for (Token other : otherLineStarts) {		
			if (other.textEquals("(")) { // stand-alone parenthesis, as opposed to "method_name(" or "type_name(" or "#("
				if (moveRowInValueOrNewConstructor(other, otherLineIndent, keepOnSingleLine)) {
					changed = true;
				}
			} else if (mayAlignAsteriskComments && other.isAsteriskCommentLine()) {
				// align commented-out assignments, too (if 'effective indent' positions could be retrieved from the AlignTable)  
				if (alignAsteriskComment(other, parameterIndent, assignmentOpIndent)) {
					changed = true;
				}
			} else if (!other.isAsteriskCommentLine()) { //  other.isQuotMarkComment() && other.isCommentLine()
				int curIndent = other.getStartIndexInLine();
				if (curIndent != otherLineIndent) {
					Token addIndentEnd = other.getNextSibling();
					// in case of WHERE ( ... ), add indent to the whole clause
					if (other.isKeyword("WHERE") && addIndentEnd.textEquals("(")) {
						addIndentEnd = addIndentEnd.getNextSibling().getNextSibling();
					} else if (other.textEquals(")")) {
						// closing parenthesis found at line start; this can happen if ClosingBracketsPositionRule is deactivated 
						// or if there is a comment line before the closing parenthesis
						Token next = other.getNext();
						if (next != null && next.lineBreaks == 0 && next.textEquals(")")) {
							// keep multiple closing parentheses where they are, because in this coding style, their line start position
							// usually depends on the line start of the opening section that corresponds to the last closing parenthesis 
							continue;
						}
						// move the (single) closing parenthesis, but not the rest of the Command
						addIndentEnd = next;
					}
					command.addIndent(otherLineIndent - curIndent, curIndent, other, addIndentEnd, true);
					changed = true;
				}
				// if applicable, adjust the line breaks of the first 'otherLine', e.g. moving it behind the parent token 
				if (other == parentToken.getNext()) {
					if (tableStart.continueOnSameLine && other.lineBreaks > 0) {
						if (other.setWhitespace()) {
							changed = true;
						}
					} else if (!tableStart.continueOnSameLine && other.lineBreaks == 0) {
						if (other.setWhitespace(1, otherLineIndent)) {
							changed = true;
						}
					}
				}
			}
		}
		
		return changed;
	}

	private Token findStartOfNextRow(Token endOfRow) throws UnexpectedSyntaxException {
		// find next table row by skipping any assignments 'component = arithmetic_expression' between rows
		Token token = endOfRow.getNextCodeSibling();
		while (token != null) {
			// start of next table row found?
			if (token.textEquals("("))
				return token;

			// skip the component name
			if (!token.isIdentifier())
				return null;
			token = token.getNextCodeSibling();
			
			// skip the assignment operator
			if (token == null || !token.isAssignmentOperator())
				return null;
			token = token.getNextCodeSibling();
			
			// skip the right-hand-side expression
			if (token == null)
				return null;
			Term expression = Term.createArithmetic(token);
			token = expression.lastToken.getNextCodeSibling();
		}
		
		return null;
	}

	public static HashMap<String, Term> getFunctionalCallParams(Token parentToken) throws UnexpectedSyntaxException {
		HashMap<String, Term> exprOfParam = new HashMap<>();
		AlignTable table = new AlignTable(AlignParametersRule.MAX_COLUMN_COUNT);
		ArrayList<Token> otherLineStarts = new ArrayList<Token>();
		buildAlignTable(table, parentToken, parentToken.getNextSibling(), ContentType.FUNCTIONAL_CALL_PARAMS, otherLineStarts);
		for (AlignLine line : table.getLines()) {
			AlignCell paramCell = line.getCell(Columns.PARAMETER.getValue());
			String paramName = paramCell.getFirstToken().getText().toUpperCase();
			AlignCellTerm exprCell = (AlignCellTerm)line.getCell(Columns.EXPRESSION.getValue());
			exprOfParam.put(paramName, exprCell.getTerm());
		}
		return exprOfParam;
	}
	
	public static void buildAlignTable(AlignTable table, Token parentToken, Token end, ContentType contentType, ArrayList<Token> otherLineStarts) throws UnexpectedSyntaxException {
		table.getColumn(Columns.ASSIGNMENT_OP.getValue()).rightAlign = true; // if both = and ?= appear, align the "=" and make the "?" stand out

		Token token = parentToken.getNext(); // parentToken is the opening parenthesis or bracket
		if (token == null)
			return;
		
		// consider the special case of a functional call with only an expression, but no actual parameter specified, e.g. 
		// any_method( VALUE #( ... ) ).
		if (contentType == ContentType.FUNCTIONAL_CALL_PARAMS && Term.isFirstTokenAllowed(token)) {
			try {
				Term onlyExpression = Term.createArithmetic(token);
				if (onlyExpression.lastToken != null && onlyExpression.lastToken.getNextCodeToken() == end) {
					AlignLine line = table.addLine();
					line.setCell(Columns.EXPRESSION.getValue(), new AlignCellTerm(onlyExpression));
					return;
				}
			} catch (UnexpectedSyntaxException ex) {
				// ignore error
			}
		}
		
		boolean isReceiveResults = (contentType == ContentType.PROCEDURAL_CALL_PARAMS && parentToken.getParentCommand().firstCodeTokenIsKeyword("RECEIVE"));
		
		// find assignments within the siblings inside the parentheses or brackets
		boolean isInLetExpression = false;
		boolean continueLastLine = false;
		boolean tableEndsWithOtherLine = false; 
		boolean isInExceptions = false;

		while (token != null && token != end && token.getNext() != null) {
			if ((token.isIdentifier() || token.isKeyword("OTHERS"))
					&& (token.getPrevCodeSibling() == null || !token.getPrevCodeSibling().isKeyword("FOR")) // exclude conditional iteration "FOR var = rhs [THEN expr] UNTIL|WHILE ..." 
					&& token.getNext() != null && token.getNext().isAssignmentOperator() 
					&& token.getNext().getNext() != null && !token.getNext().getNext().isComment()) {

				// identify the parts of the assignment "parameter = term" (and possibly the keyword like "EXPORTING" preceding it)   
				Token parameter = token;
				Token keyword = parameter.getPrevCodeToken();
				if (keyword != null) {
					if (contentType == ContentType.GROUP_KEY) {
						keyword = null; 
					} else if (!keyword.isKeyword() || keyword != parameter.getPrevCodeSibling()) {
						keyword = null; 
					} else if (keyword.isAnyKeyword("EXPORTING", "IMPORTING", "TABLES", "CHANGING", "RECEIVING", "EXCEPTIONS")) {
						isInExceptions = keyword.isKeyword("EXCEPTIONS");
						// keep keyword 
					} else if (keyword.isKeyword("COMPONENTS")) { // for "KEY ... COMPONENTS c1 = ... c2 = ..." 
						// keep keyword
					} else if (keyword.isKeyword("LET")) { // for LET expressions, e.g. "VALUE #( LET a = 1 IN ... )"
						isInLetExpression = true;
					} else {
						keyword = null;
					}
				}
				Token assignmentOp = parameter.getNextCodeSibling(); // there may be a comment line between EXPORTING and the first parameter
				Token exprStart = assignmentOp.getNext();
				Term expression;
				if (contentType == ContentType.GROUP_KEY && exprStart.matchesOnSiblings(true, "GROUP", "SIZE|INDEX")) {
					// LOOP AT ... GROUP BY ( key1 = dobj1 key2 = dobj2 ... [gs = GROUP SIZE] [gi = GROUP INDEX] ) ...
					expression = Term.createForTokenRange(exprStart, exprStart.getNextCodeSibling());
				} else {
					expression = Term.createArithmetic(assignmentOp.getNext());
				}
				
				// for "RECEIVE RESULTS FROM FUNCTION func" consider the MESSAGE addition after two special exceptions, 
				// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapreceive_para.htm:
				// EXCEPTIONS [exc1 = n1 exc2 = n2 ...] 
				//            [system_failure = ns [MESSAGE smess]] 
            //            [communication_failure = nc [MESSAGE cmess]]  
				if (isReceiveResults && isInExceptions && parameter.textEqualsAny("system_failure", "communication_failure")) {
					Token next = expression.lastToken.getNextCodeSibling();
					if (next != null && next.isKeyword("MESSAGE")) {
						Token messageIdentifier = next.getNextCodeSibling();
						if (messageIdentifier != null) {
							// extend the expression to contain MESSAGE smess / cmess:
							expression = Term.createForTokenRange(assignmentOp.getNext(), messageIdentifier);
						}
					}
				}
				
				// build the next table line from the identified parts of the assignment
				AlignLine line = continueLastLine ? table.getLastLine() : table.addLine();
				if (keyword != null) {
					line.setCell((isInLetExpression ? Columns.LET_KEYWORD : Columns.KEYWORD).getValue(), new AlignCellToken(keyword));
				}
				line.setCell((isInLetExpression ? Columns.LET_PARAMETER : Columns.PARAMETER).getValue(), new AlignCellToken(parameter));
				line.setCell((isInLetExpression ? Columns.LET_ASSIGNMENT_OP : Columns.ASSIGNMENT_OP).getValue(), new AlignCellToken(assignmentOp));
				line.setCell((isInLetExpression ? Columns.LET_EXPRESSION : Columns.EXPRESSION).getValue(), new AlignCellTerm(expression));
				continueLastLine = false;
				tableEndsWithOtherLine = false;
				
				if (!otherLineStarts.isEmpty() && otherLineStarts.get(otherLineStarts.size() - 1) == keyword)
					otherLineStarts.remove(otherLineStarts.size() - 1);

				token = expression.getNext();

			} else if (isInLetExpression && token.isKeyword("IN")) {
				isInLetExpression = false;
				if (token.lineBreaks == 0) {
					// "IN" at line end is not entered to the AlignTable, so it remains at line end
				} else {
					// "IN" at line start is put to the keyword column of the following content (NOT the LET_KEYWORD column)
					AlignLine line = table.addLine();
					line.setCell(Columns.LET_IN_KEYWORD.getValue(), new AlignCellToken(token));
					// set a higher text width to match the "LET" keyword width 
					line.addWidthToEnd("LET".length() - "IN".length());
					continueLastLine = true;
					tableEndsWithOtherLine = false;
				}
				
				token = token.getNext();
				
			} else if (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR && (token.isIdentifier() || token.isLiteral())) {
				// in VALUE or NEW constructors, rows may not contain assignments, but only  
				// - the expression, e.g. "lt_char = VALUE #( ( 'A' ) ( 'B' ) ( lv_char ) )" (for tables without structured line type)
				// - a variable for a whole structure, e.g. "lt_table = VALUE #( ( ls_struc ) ( comp = 1 ) )"
				// Note that only the second case can be mixed with other lines that contain assignments, and if alignment across
				// table rows is activated, 'LS_STRUC' should be aligned with the component 'COMP', not with the value '1'; therefore,  
				// the .PARAMETER column is used in all non-assignment cases, not the .EXPRESSION column 
				Term expression = Term.createArithmetic(token);
				AlignLine line = continueLastLine ? table.getLastLine() : table.addLine();
				line.setCell(Columns.PARAMETER.getValue(), AlignCellTerm.createSpecial(expression, 0, true));
				continueLastLine = false;
				tableEndsWithOtherLine = false;
			
				token = expression.getNext();

			} else {
				if (token == parentToken.getNext() || token.lineBreaks > 0) {
					if  (token.isPseudoCommentAfterCode()) {
						// avoid moving pseudo comments to the next line if they refer to the line of the parentToken 
					} else if (token.isCommentAfterCode() && contentType != ContentType.CONSTRUCTOR_EXPR && contentType != ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR && contentType != ContentType.GROUP_KEY) {
						// line-end comments after method calls usually do not refer to the parameters; therefore, keep them in their place, too
					} else {
						// store 'other line starts'; in case this is a keyword like "EXPORTING", this may be removed again later
						otherLineStarts.add(token); 
						tableEndsWithOtherLine = true;
					}
				} else if (token.textEquals("(") && token.lineBreaks == 0 && token.getPrevCodeSibling() != null && token.getPrevCodeSibling().isKeyword("WHERE")) {
					// skip this case of a logical expression in parentheses after WHERE, because it is NOT a table row:
					// 'VALUE type( FOR ... IN ... WHERE ( log_exp ) ... ).' 
				} else if (token.textEquals("(")) {
					// always store the start of rows in VALUE or NEW constructors (even if they do not yet start a line) 
					otherLineStarts.add(token); 
					tableEndsWithOtherLine = true;
				}
				continueLastLine = false;
				token = token.getNextSibling();
			}
		}

		if (!tableEndsWithOtherLine) {
			// consider the space needed for closing brackets and anything attached to them (except comments) by adding the 
			// required width to the end of the last table line. Example: in "lts_table[ param = ... ]-component.", the length 
			// of "]-component." must be considered for total line length
			int addWidth = 0;
			token = end;
			while (token != null && token.lineBreaks == 0 && !token.isComment() && (token.closesLevel() || token.spacesLeft == 0)) {
				addWidth += token.spacesLeft + token.getTextLength();
				token = token.getNext();
			}
			if (addWidth > 0 && table.getLineCount() > 0) {
				table.getLastLine().addWidthToEnd(addWidth); 
			}
		}
	}
	
	private void setForcedLineBreaks(AlignTable table, ContentType contentType) {
		boolean hasLetExpression = !table.getColumn(Columns.LET_KEYWORD.getValue()).isEmpty();

		if (hasLetExpression) {
			table.getColumn(Columns.LET_EXPRESSION.getValue()).setForceLineBreakAfter(false);
			// the LET_IN_KEYWORD column is only filled if "IN" is at the beginning of a line; 
			// however, even if it is NOT filled, it will forced the indent back to "basicIndent + 0"
			table.getColumn(Columns.LET_IN_KEYWORD.getValue()).setForceIndent(0);
			
		} else {
			// if configured, setup the AlignTable to force a line break after EXPORTING, IMPORTING etc. keywords
			// however, do not force a line break if the table continues behind a call
			int maxParamCount = 0;
			if (contentType == ContentType.PROCEDURAL_CALL_PARAMS) 
				maxParamCount = (configMaxParamCountBehindProceduralCall.getValue() > 0 ? 1 : 0); // alternative: maxParamCount = config...getValue();
			else if (contentType == ContentType.FUNCTIONAL_CALL_PARAMS) 
				maxParamCount = (configMaxParamCountBehindFunctionalCall.getValue() > 0 ? 1 : 0); // alternative: maxParamCount = config...getValue();

			AlignColumn keywordColumn = table.getColumn(Columns.KEYWORD.getValue());
			if (!keywordColumn.isEmpty() && forceLineBreakAfterKeywords(contentType) && table.getLineCount() > maxParamCount) {
				keywordColumn.setForceLineBreakAfter(true);
			}
		}
	}

	private boolean forceLineBreakAfterKeywords(ContentType contentType) {
		return (contentType == ContentType.PROCEDURAL_CALL_PARAMS && configPutProceduralCallKeywordsOnOwnLine.getValue()) 
    		 || (contentType == ContentType.FUNCTIONAL_CALL_PARAMS && configPutFunctionalCallKeywordsOnOwnLine.getValue());
	}

	private TableStart determineTableStart(Token parentToken, int baseIndent, int minimumIndent, ContentType contentType, AlignTable table, ArrayList<Token> otherLineStarts) {
		AlignColumn keywordColumn = table.getColumn(Columns.KEYWORD.getValue());
		boolean hasKeywords = !keywordColumn.isEmpty();
		int addIndent = (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR || contentType == ContentType.GROUP_KEY || hasKeywords) 
						  ? ABAP.INDENT_STEP : 2 * ABAP.INDENT_STEP; 

		// determine whether to continue on the same line after parentToken (this may be revised later if there is not enough space)
		// note that continueOnSameLine may refer to the table (if it starts directly after parentToken) or to otherLineStarts
		boolean continueOnSameLine;
		Token firstTokenInTable = table.getFirstToken();
		if ((contentType == ContentType.CONSTRUCTOR_EXPR) || (contentType == ContentType.ROW_IN_VALUE_OR_NEW_CONSTRUCTOR) || (contentType == ContentType.GROUP_KEY)) {
			continueOnSameLine = true;
		} else if (firstTokenInTable != null && parentToken.getNext() == firstTokenInTable) {
			continueOnSameLine = true;
		} else {
			continueOnSameLine = (parentToken.getNext().lineBreaks == 0);
		}
		
		// by default, indent directly behind the parentToken
		int startInParentheses = parentToken.getEndIndexInLine() + 1;
		int startIndent = startInParentheses;
		// exceptions: in some cases, indent at the current position of the first assignment:
		// - table expression with KEY, e.g. "lts_defitem_src[ KEY latest_defitem COMPONENTS condition_type = ..."
		//   (however, if line length is exceeded, move 'COMPONENTS ...' to the next line) 
		// - FILTER #( ... WHERE component = ... ) 
		if (firstTokenInTable != null) {
			if ((!continueOnSameLine && firstTokenInTable.getPrev().isKeyword("WHERE")) || firstTokenInTable.isKeyword("COMPONENTS"))
				startIndent = firstTokenInTable.getStartIndexInLine();
		}

		// determine the total width of the table, as well as the rows in a VALUE or NEW constructor
		int tableWidth = table.getTotalMultiLineWidth();
		for (Token token : otherLineStarts) {
			if (token.textEquals("(")) {
				try {
					Term term = Term.createForTokenRange(token, token.getNextSibling());
					int rowWidth = term.getCurrentWidth(false);
					// if the row is on one line, only consider its length if it has a chance of remaining on one line 
					if (!term.isOnSingleLine() || minimumIndent + rowWidth <= configMaxLineLengthForSingleLine.getValue())
						tableWidth = Math.max(tableWidth, rowWidth);
				} catch (UnexpectedSyntaxException e) {
				}
			}
		}

		// determine whether base indent can be used, or even minimum indent is required (if they are different)
		int earlyIndent = ((baseIndent + addIndent + tableWidth <= configMaxLineLength.getValue()) ? baseIndent : minimumIndent) + addIndent;

		// move assignments to the left if line length is exceeded // TODO: add configuration to prevent/allow/enforce this (possibly depending on main keyword)
		boolean forceTableToNextLine = false;
		if (startIndent + tableWidth > configMaxLineLength.getValue()) {
			if (startInParentheses + tableWidth <= configMaxLineLength.getValue()) {
				// even if the table does not directly start after the parentToken, move the table to the next line, 
				// e.g. in the above example "lts_defitem_src[ KEY latest_defitem COMPONENTS condition_type = ..."
				forceTableToNextLine = true;
				startIndent = startInParentheses;
			} else {
				// move the assignments to the earliest possible position (if this is an improvement)
				if (earlyIndent < startIndent) {
					continueOnSameLine = false;
					startIndent = earlyIndent;
				}
				// if the line length is still exceeded, force a line break after EXPORTING, IMPORTING etc., if applicable 
				if (hasKeywords && startIndent + tableWidth > configMaxLineLength.getValue()) {
					keywordColumn.setForceLineBreakAfter(true);
				}
			}
		}

		// depending on configuration, move to the next line and the minimum indent once a certain number of parameters is exceeded
		if (contentType == ContentType.PROCEDURAL_CALL_PARAMS) {
			if (table.getLineCount() > configMaxParamCountBehindProceduralCall.getValue()) {
				continueOnSameLine = false;
				startIndent = earlyIndent;
			}
		} else if (contentType == ContentType.FUNCTIONAL_CALL_PARAMS) {
			if (table.getLineCount() > configMaxParamCountBehindFunctionalCall.getValue()) { 
				continueOnSameLine = false;
				startIndent = earlyIndent;
			}
		}

		return new TableStart(startIndent, continueOnSameLine, forceTableToNextLine, earlyIndent);
	}

	private boolean determineKeepOnSingleLine(Token parentToken, Token end, int earlyIndent, boolean forTabularStyle) {
		// if there is already a line break, return false
		Token testToken = parentToken.getNext();
		while (testToken != null && testToken != end) {
			if (testToken.lineBreaks > 0) {
				return false;
			}
			testToken = testToken.getNext();
		}

		// decide whether to keep everything on one line
		ComponentsOnSingleLine keepOnSingleLine = forTabularStyle ? ComponentsOnSingleLine.forValue(configKeepComponentsOnSingleLine.getValue())
																					 : ComponentsOnSingleLine.forValue(configKeepOtherOneLiners.getValue());
		switch (keepOnSingleLine) {
			case ALWAYS:
				return true;
			case IF_BELOW_MAX_LINE_LENGTH:
				int maxLineLength = forTabularStyle ? configMaxLineLengthForSingleLine.getValue() : configMaxLineLength.getValue();
				return (earlyIndent + (end.getEndIndexInLine() - parentToken.getStartIndexInLine()) <= maxLineLength);
			case NEVER:
				return false;
			default:
				throw new IllegalArgumentException();
		}
	}

	private boolean isTableOfSingleComponents(Token parentToken, Token end) {
		// returns true for a table with single components like VALUE #( ( 1 ) ( 2 ) ( 3 ) ) or 
		// VALUE #( ( lo_any ) ( lo_other ) ( lo_third ) ), which may be kept on one line, although it is not a table row 
		
		Token testToken = parentToken.getNext();
		while (testToken != null) {
			if (!testToken.textEqualsAny("(", ")")) {
				return false;
			} if (testToken.hasChildren() && testToken.getFirstCodeChild() != testToken.getLastCodeChild()) {
				return false;
			}
			testToken = testToken.getNextSibling();
		}
		return true;
	}
	
	private boolean moveRowInValueOrNewConstructor(Token openingToken, int baseIndent, boolean keepOnSingleLine) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		int oldStartIndex = openingToken.getStartIndexInLine();
		boolean changed = false;
		if (openingToken.lineBreaks > 0) {
			if (openingToken.getPrev() == openingToken.getParent() && openingToken.getParent().getEndIndexInLine() + 1 == baseIndent) {
				// move first row back directly behind the VALUE or NEW constructor start
				changed = openingToken.setWhitespace(0, 1);
			} else if (openingToken.spacesLeft != baseIndent) {
				openingToken.spacesLeft = baseIndent;
				changed = true;
			}

		} else {
			int spacesLeft = baseIndent - openingToken.getPrev().getEndIndexInLine();
			if (spacesLeft > 0) {
				changed = openingToken.setWhitespace(0, spacesLeft);
			} else if (keepOnSingleLine) { 
				changed = openingToken.setWhitespace(0, 1);
			} else { 
				changed = openingToken.setWhitespace(1, baseIndent);
			}
		}
		
		// if the parenthesis spans multiple lines, add (or reduce) their indentation, too
		if (changed) {
			int newStartIndex = openingToken.getStartIndexInLine();
			openingToken.getParentCommand().addIndent(newStartIndex - oldStartIndex, oldStartIndex, openingToken.getNext(), openingToken.getNextSibling());
		}
		
		return changed;
	}

	private boolean alignAsteriskComment(Token comment, int parameterIndent, int assignmentOpIndent) {
		String[] bits = comment.getBitsOfCommentedOutAssignment();
		if (bits == null)
			return false;

		// align the commented-out assignment
		StringBuilder sb = new StringBuilder();
		sb.append(bits[0]); // any number of asterisks, by which the remaining text will be additionally shifted to the right

		sb.append(StringUtil.repeatChar(' ', parameterIndent));
		sb.append(bits[1]); // parameter name 
		
		int spaces = assignmentOpIndent - parameterIndent - bits[1].length();
		sb.append(StringUtil.repeatChar(' ', Math.max(spaces, 1)));
		sb.append(bits[2]); // =
		
		if (!StringUtil.isNullOrEmpty(bits[3]))
			sb.append(' ').append(bits[3]); // right-hand side term
		
		return comment.setText(sb.toString(), false);
	}
}
