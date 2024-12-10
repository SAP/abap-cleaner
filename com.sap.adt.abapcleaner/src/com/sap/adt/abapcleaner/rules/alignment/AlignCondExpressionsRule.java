package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellToken;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;

public class AlignCondExpressionsRule extends RuleForCommands {
	private enum Columns {
		LET_EXPR,
		OPERAND,
		WHEN, 
		CONDITION, 
		THEN, 
		THEN_VALUE,
		ELSE,
		ELSE_VALUE;
		
		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 8;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_COND_EXPRESSIONS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align conditional expressions"; }

	@Override
	public String getDescription() {
		return "Aligns conditional expressions with the constructor operators COND #( WHEN ... THEN ... ELSE ... ) and SWTICH #( operand WHEN ... THEN ... ELSE ... ).";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 5, 27); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return null; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_cond_expressions." 
			+ LINE_SEP + "    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ev_sign = COND #( WHEN iv_negative = abap_true" 
			+ LINE_SEP + "                        THEN -1" 
			+ LINE_SEP + "                          ELSE 1 )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN" 
			+ LINE_SEP + "                          io_object->get_sub_object( )->get_number( )" 
			+ LINE_SEP + "                        ELSE" 
			+ LINE_SEP + "                           if_object_boundaries=>co_maximum_number )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw" 
			+ LINE_SEP + "                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh" 
			+ LINE_SEP + "                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2" 
			+ LINE_SEP + "                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0" 
			+ LINE_SEP + "                                      THEN   ls_amounts-waers" 
			+ LINE_SEP + "                                    WHEN ls_amounts-betrh <> 0" 
			+ LINE_SEP + "                                       THEN  ls_amounts-hwaer" 
			+ LINE_SEP + "                                    WHEN ls_amounts-betr2 <> 0" 
			+ LINE_SEP + "                                      THEN   ls_amounts-hwae2" 
			+ LINE_SEP + "                                     WHEN ls_amounts-betr3 <> 0" 
			+ LINE_SEP + "                                        THEN ls_amounts-hwae3" 
			+ LINE_SEP + "                                    ELSE    gc_default_currency_code )." 
			+ LINE_SEP 
			+ LINE_SEP + "    display_time_info( COND #( LET t = '120000' IN"
			+ LINE_SEP + "                             WHEN sy-timlo < t"
			+ LINE_SEP + "                               THEN |{ sy-timlo TIME = ISO } AM|"
			+ LINE_SEP + "                              WHEN sy-timlo > t AND sy-timlo < '240000'"
			+ LINE_SEP + "                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|"
			+ LINE_SEP + "                               WHEN sy-timlo = t"
			+ LINE_SEP + "                                 THEN |High Noon|"
			+ LINE_SEP + "                                ELSE THROW cx_cant_be( ) ) )."
			+ LINE_SEP 
			+ LINE_SEP + "    ev_num = SWITCH #( ev_num WHEN 999 THEN 0 ELSE ( ev_num + 1 ) )."
			+ LINE_SEP 
			+ LINE_SEP + "    out->write( SWITCH string( sy-index"
			+ LINE_SEP + "                               WHEN 1 THEN 'one'"
			+ LINE_SEP + "                               WHEN 2 THEN 'two'"
			+ LINE_SEP + "                               WHEN 3 THEN 'three' ELSE THROW cx_overflow( ) ) )."
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length for one-liners and tabular cases:", "", MIN_LINE_LENGTH_ABAP, DEFAULT_LINE_LENGTH_ABAP, ABAP.MAX_LINE_LENGTH);

	final ConfigEnumValue<CondOneLinerStyle> configOneLinerStyle = new ConfigEnumValue<CondOneLinerStyle>(this, "OneLinerStyle", "One-liners:",
																								new String[] { "create if possible", "keep as is", "always split to multi-line" }, CondOneLinerStyle.values(), CondOneLinerStyle.KEEP);
	final ConfigEnumValue<CondSimpleStyle> configSimpleStyle = new ConfigEnumValue<CondSimpleStyle>(this, "SimpleStyle", "Simple cases (1x WHEN):",
																								new String[] { "put WHEN/THEN/ELSE below each other", "layout like complex cases" }, CondSimpleStyle.values(), CondSimpleStyle.VERTICAL_LAYOUT);
	final ConfigEnumValue<CondTabularStyle> configTabularStyle = new ConfigEnumValue<CondTabularStyle>(this, "TabularStyle", "Tabular cases (multiple WHEN):",
																								new String[] { "put WHEN... THEN ... on one line if possible", "keep as is", "always split to multi-line" }, CondTabularStyle.values(), CondTabularStyle.CREATE);
	
	final ConfigBoolValue configGapAfterElse = new ConfigBoolValue(this, "GapAfterElse", "Tabular cases: Align ELSE value with THEN values", true);
	final ConfigBoolValue configThenOnWhenLine = new ConfigBoolValue(this, "ThenOnWhenLine", "Complex cases: Put THEN at the end of WHEN line", true);
	final ConfigBoolValue configContinueAfterElse = new ConfigBoolValue(this, "ContinueAfterElse", "Complex cases: Continue after ELSE", false);
	final ConfigEnumValue<CondValueIndent> configValueIndent = new ConfigEnumValue<CondValueIndent>(this, "ValueInset", "Complex cases: Indentation of values:",
																								new String[] { "below WHEN", "below WHEN + 2 spaces", "below WHEN + 5 spaces", "below WHEN + 7 spaces" }, CondValueIndent.values(), CondValueIndent.ADD_2);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configOneLinerStyle, configSimpleStyle, configTabularStyle, configGapAfterElse, configThenOnWhenLine, configContinueAfterElse, configValueIndent };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private CondOneLinerStyle getConfigOneLinerStyle() {
		return CondOneLinerStyle.forValue(configOneLinerStyle.getValue());
	}

	private CondSimpleStyle getConfigSimpleStyle() {
		return CondSimpleStyle.forValue(configSimpleStyle.getValue());
	}

	private CondTabularStyle getConfigTabularStyle() {
		return CondTabularStyle.forValue(configTabularStyle.getValue());
	}

	private CondValueIndent getConfigValueIndent() {
		return CondValueIndent.forValue(configValueIndent.getValue());
	}

	public AlignCondExpressionsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (command.isInClassDefinition())
			return false;
		
		boolean changed = false;
		Token token = command.getFirstCodeToken();
		while (token != null) {
			if (token.isAnyKeyword("COND", "SWITCH")) {
				boolean isSwitch = token.isKeyword("SWITCH");
				token = token.getNextCodeSibling();
				if (token.getOpensLevel()) {
					Command[] changedCommands = executeOn(code, command, token, isSwitch);
					if (changedCommands != null && changedCommands.length > 0)
						changed = true;
				}
			}
			token = token.getNextCodeToken();
		}

		return changed; 
	}
	
	private Command[] executeOn(Code code, Command command, Token parentToken, boolean isSwitch) throws UnexpectedSyntaxAfterChanges { 
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		
		Token token = parentToken.getFirstCodeChild();

		int basicIndent = token.getStartIndexInLine();
		int firstLineBreaks = token.lineBreaks;

		AlignLine line = table.addLine();
		
		// read initial LET ... IN
		if (token.isKeyword("LET")) {
			Term letTerm = readTermUntil(token, true, "IN");
			line.setCell(Columns.LET_EXPR.getValue(), new AlignCellTerm(letTerm));
			token = letTerm.lastToken.getNextCodeSibling();
		}
		// for SWITCH expressions, read the operand
		if (isSwitch && token != null) {
			Term operand = readTermUntil(token, false, "WHEN");
			line.setCell(Columns.OPERAND.getValue(), new AlignCellTerm(operand));
			token = operand.lastToken.getNextCodeSibling();
		}

		int whenCount = 0;
		Token elseToken = null;
		Term elseValue = null;

		while (token != null) {
			if (token.isKeyword("WHEN")) {
				if (whenCount > 0) {
					line = table.addLine();
				}
				++whenCount;
				
				line.setCell(Columns.WHEN.getValue(), new AlignCellToken(token));

				// read the WHEN condition and set the CONDITION cell
				Term condTerm = readTermUntil(token.getNextCodeSibling(), false, "THEN");
				line.setCell(Columns.CONDITION.getValue(), new AlignCellTerm(condTerm));

				// set the THEN cell
				token = condTerm.lastToken.getNextCodeSibling();
				line.setCell(Columns.THEN.getValue(), new AlignCellToken(token));

				// read the THEN value and set the VALUE cell
				Term thenValue = readTermUntil(token.getNextCodeSibling(), false, "WHEN", "ELSE", "");
				line.setCell(Columns.THEN_VALUE.getValue(), new AlignCellTerm(thenValue));
				token = thenValue.lastToken;

			} else if (token.isKeyword("ELSE")) {
				elseToken = token;
				line.setCell(Columns.ELSE.getValue(), new AlignCellToken(elseToken));
				
				// read the ELSE value
				elseValue = readTermUntil(token.getNextCodeSibling(), false, "");
				line.setCell(Columns.ELSE_VALUE.getValue(), new AlignCellTerm(elseValue));
				token = elseValue.lastToken;
			}
			token = token.getNextCodeSibling();
		}
		
		AlignColumn letExprColumn = table.getColumn(Columns.LET_EXPR.getValue());
		AlignColumn operandColumn = table.getColumn(Columns.OPERAND.getValue());
		AlignColumn whenColumn = table.getColumn(Columns.WHEN.getValue());
		AlignColumn conditionColumn = table.getColumn(Columns.CONDITION.getValue());
		AlignColumn thenColumn = table.getColumn(Columns.THEN.getValue());
		AlignColumn thenValueColumn = table.getColumn(Columns.THEN_VALUE.getValue());
		AlignColumn elseColumn = table.getColumn(Columns.ELSE.getValue());
		AlignColumn elseValueColumn = table.getColumn(Columns.ELSE_VALUE.getValue());
		
		// check whether a one-liner can and shall be created or kept 
		if (whenCount == 1) {
			boolean alignAsOneLiner = false;
			boolean fitsIntoOneLine = basicIndent + table.getTotalMultiLineWidth() <= configMaxLineLength.getValue(); 

			CondOneLinerStyle oneLinerStyle = getConfigOneLinerStyle();
			if (oneLinerStyle == CondOneLinerStyle.CREATE) {
				alignAsOneLiner = fitsIntoOneLine && (!command.containsCommentBetween(table.getFirstToken(), parentToken.getNextSibling()));
			} else if (oneLinerStyle == CondOneLinerStyle.KEEP) {
				alignAsOneLiner = fitsIntoOneLine && (!command.containsLineBreaksBetween(parentToken.getNextNonCommentToken(), parentToken.getNextSibling(), false));
			}
			if (alignAsOneLiner) {
				return table.align(basicIndent, firstLineBreaks, true);
			}
		}

		// for all NON-one-liners, force and line break after LET ... IN and the SWITCH operand, 
		// and move the ELSE line below WHEN lines
		if (!letExprColumn.isEmpty()) {
			letExprColumn.setForceLineBreakAfter(true);
			operandColumn.setForceIndent(0);
		}
		if (!operandColumn.isEmpty()) {
			operandColumn.setForceLineBreakAfter(true);
			whenColumn.setForceIndent(0);
		}
		thenValueColumn.setForceLineBreakAfter(false);
		elseColumn.setForceIndent(0);

		// check whether simple layout can and shall be created
		CondSimpleStyle simpleStyle = getConfigSimpleStyle(); 
		if (simpleStyle == CondSimpleStyle.VERTICAL_LAYOUT && whenCount == 1) {
			conditionColumn.setForceLineBreakAfter(false);
			thenColumn.setForceIndent(0);
			elseValueColumn.removeLineBreaksBefore();
			return table.align(basicIndent, firstLineBreaks, true);
		}

		// check whether tabular layout can and shall be created
		if (whenCount > 1) {
			boolean alignToTabularStyle = false;
			CondTabularStyle tabularStyle = getConfigTabularStyle();
			if (tabularStyle  == CondTabularStyle.CREATE) {
				alignToTabularStyle = (basicIndent + table.getTotalMultiLineWidth() <= configMaxLineLength.getValue());
			} else if (tabularStyle == CondTabularStyle.KEEP) {
				alignToTabularStyle = (!thenColumn.hasAnyLineBreakBefore() && !thenValueColumn.hasAnyLineBreakBefore());
			}
			if (alignToTabularStyle) {
				if (configGapAfterElse.getValue() && elseToken != null) {
					line.clearCell(Columns.ELSE.getValue());
					line.clearCell(Columns.ELSE_VALUE.getValue());
					line = table.addLine();
					line.setCell(Columns.WHEN.getValue(), new AlignCellToken(elseToken));
					line.setCell(Columns.THEN_VALUE.getValue(), new AlignCellTerm(elseValue));
				}
				return table.align(basicIndent, firstLineBreaks, true);
			}
		}

		// create complex layout
		// - determine indent of values (which may include 'THEN')
		CondValueIndent thenIndent = getConfigValueIndent();
		int valueIndentAdd = 0;
		if (thenIndent == CondValueIndent.ADD_2)
			valueIndentAdd = 2;
		else if (thenIndent == CondValueIndent.ADD_5)
			valueIndentAdd = 5;
		else if (thenIndent == CondValueIndent.ADD_7)
			valueIndentAdd = 7;

		// - position of THEN and indent of THEN values
		if (configThenOnWhenLine.getValue()) {
			thenColumn.removeLineBreaksBefore();
			try {
				Command[] changedCommands = thenColumn.joinIntoPreviousColumns(true, true);
				code.addRuleUses(this, changedCommands);
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
			conditionColumn.setForceLineBreakAfter(false);
			thenValueColumn.setForceIndent(valueIndentAdd);
		} else {
			conditionColumn.setForceLineBreakAfter(false);
			thenColumn.setForceIndent(valueIndentAdd);
		}
		
		// - position of ELSE and indent of ELSE value
		boolean continueAfterElse = configContinueAfterElse.getValue();
		if (continueAfterElse) {
			elseValueColumn.removeLineBreaksBefore();
			if (thenIndent == CondValueIndent.ADD_7) {
				AlignCell elseValueCell = elseValueColumn.getLastNonEmptyCell();
				if (elseValueCell != null) {
					elseValueCell.setAdditionalIndent(2);
				}
			}
		} else {
			elseColumn.setForceLineBreakAfter(false);
			elseValueColumn.setForceIndent(valueIndentAdd);
		}

		return table.align(basicIndent, firstLineBreaks, true);
	}
	
	private Term readTermUntil(Token start, boolean includeEndKeyword, String... endKeywords) throws UnexpectedSyntaxAfterChanges {
		Token firstToken = start;
		Token lastToken = firstToken;
		
		Token token = firstToken;
		while (token != null && !token.isAnyKeyword(endKeywords)) {
			lastToken = token;
			token = token.getNextCodeSibling();
			if (includeEndKeyword && token != null)
				lastToken = token;
		}
		
		try {
			return Term.createForTokenRange(firstToken, lastToken);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}
}
