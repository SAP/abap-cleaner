package com.sap.adt.abapcleaner.rules.ddl.position;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlPosition;

public class DdlPositionClausesRule extends RuleForDdlPosition {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_CLAUSES; }

	@Override
	public String getDisplayName() { return "Break before WHERE clause etc."; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation of keywords that start the clauses WHERE, GROUP BY, HAVING, as well as EXCEPT, INTERSECT and UNION."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField          as AnyKey,"
				+ LINE_SEP + "      sum(AnyAlias.AnyNumericField) as AnySum,"
				+ LINE_SEP + "      AnyAlias.UnitField            as Unit"
				+ LINE_SEP + "}"
				+ LINE_SEP + ""
				+ LINE_SEP + "where AnyAlias.AnyConditionField = 'X'"
				+ LINE_SEP + "  and AnyAlias.AnyCategory       = 'A' group"
				+ LINE_SEP + "by AnyAlias.AnyKeyField,"
				+ LINE_SEP + "   AnyAlias.AnyNumericField,"
				+ LINE_SEP + "   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100"
				+ LINE_SEP + ""
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_OtherEntity as OtherAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key OtherAlias.OtherKeyField          as AnyKey,"
				+ LINE_SEP + "      sum(OtherAlias.OtherNumericField) as AnySum,"
				+ LINE_SEP + "      OtherAlias.OtherUnitField         as Unit"
				+ LINE_SEP + "} where OtherAlias.OtherCategory = 'A'"
				+ LINE_SEP + "     or OtherAlias.OtherCategory = 'B'"
				+ LINE_SEP + ""
				+ LINE_SEP + "group by OtherAlias.OtherKeyField,"
				+ LINE_SEP + "         OtherAlias.OtherNumericField,"
				+ LINE_SEP + "         OtherAlias.OtherUnitField"
				+ LINE_SEP + "";
	}
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeWhereEtc = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeWhereEtc", "Break before WHERE / GROUP BY / HAVING:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configWhereEtcIndent = new ConfigIntValue(this, "WhereEtcIndent", "Indent if breaking:", "", 0, 0, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeUnionEtc = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeUnionEtc", "Break before EXCEPT / INTERSECT / UNION:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configUnionEtcIndent = new ConfigIntValue(this, "UnionEtcIndent", "Indent if breaking:", "", 0, 0, MAX_INDENT);
	
	final ConfigInfoValue configInfoBreakBeforeSelect = new ConfigInfoValue(this, "The position of SELECT FROM after a UNION etc. can be changed with the rule '" + DdlPositionSelectRule.defaultDisplayName + "'", ConfigInfoStyle.NORMAL);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configBreakBeforeWhereEtc, configWhereEtcIndent, configBreakBeforeUnionEtc, configUnionEtcIndent, configInfoBreakBeforeSelect };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionClausesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (command.getParent() != null)
			return false;
		
		Token token = command.getFirstCodeToken();
		if (token == null) // pro forma
			return false;

		if (token.isAnyKeyword("WHERE", "HAVING")) {
			Token lastKeyword = token;
			Token clauseEnd = null; // Command.canAddToDdl() starts a new Command after the WHERE / HAVING clause
			DdlLineBreak lineBreak = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeWhereEtc.getValue()));

			return executeOn(command, token, lastKeyword, clauseEnd, lineBreak, configWhereEtcIndent.getValue(), false);

		} else if (token.matchesOnSiblings(true, "GROUP", "BY")) {
			Token lastKeyword = token.getNextCodeSibling(); // BY
			Token clauseEnd = null; // Command.canAddToDdl() starts a new Command after the GROUP BY clause
			DdlLineBreak lineBreak = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeWhereEtc.getValue()));
			
			return executeOn(command, token, lastKeyword, clauseEnd, lineBreak, configWhereEtcIndent.getValue(), false);
			
		} else if (token.startsDdlUnionEtc()) {
			Token lastKeyword = token.matchesOnSiblings(true, "UNION", "ALL") ? token.getNextCodeSibling() : token;
			Token clauseEnd = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "SELECT");
			DdlLineBreak lineBreak = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeUnionEtc.getValue()));
			
			Command prevCommand = command.getPrevNonCommentCommand();
			boolean isAfterSelectList = (token.getPrevCodeSibling() == null && prevCommand != null && prevCommand.firstCodeTokenTextEqualsAny(DDL.BRACE_CLOSE_STRING));
			boolean enforceEmptyLine = !isAfterSelectList;

			return executeOn(command, token, lastKeyword, clauseEnd, lineBreak, configUnionEtcIndent.getValue(), enforceEmptyLine);
		
		} else {
			return false;
		}
	}
	
	private boolean executeOn(Command command, Token clauseStart, Token lastKeyword, Token clauseEnd, DdlLineBreak lineBreak, int newIndent, boolean enforceEmptyLine) {
		boolean changed = false;
		int oldLastKeywordIndex = lastKeyword.getStartIndexInLine();
		
		// break before clause, as configured
		changed |= breakBefore(clauseStart, lineBreak, enforceEmptyLine, newIndent);
		
		// condense keyword collocations such as 'GROUP BY' and 'UNION ALL'
		if (lastKeyword != clauseStart) 
			changed |= condense(clauseStart, lastKeyword);


		// move the rest of the clause along with how the last keyword was moved horizontally
		int addIndent = lastKeyword.getStartIndexInLine() - oldLastKeywordIndex;
		if (addIndent != 0) 
			changed |= command.addIndent(addIndent, 0, lastKeyword.getNext(), clauseEnd);

		return changed;
	}
}
