package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlPositionJoinRule;

public class DdlAlignDataSourcesRule extends RuleForDdlCommands {
	private enum Columns {
		KEYWORDS,
		DATA_SOURCE,
		AS_ALIAS,
		ON_CONDITION;
		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 4;

	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_DATA_SOURCES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align JOINs and ASSOCIATIONs"; }

	@Override
	public String getDescription() { return "Aligns the data sources, aliases and ON conditions of SELECT FROM, JOINs and ASSOCIATIONs."; }

	@Override
	public String getHintsAndRestrictions() { return "Whether to break ON conditions to the next line is configured in '" + DdlPositionJoinRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 16); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "// Depending on configuration in '" + DdlPositionJoinRule.displayName + "',"
				+ LINE_SEP + "// ON conditions were either moved to the next line ..."
				+ LINE_SEP + "define view I_AnyView"
				+ LINE_SEP + "  as select from I_AnySource as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer join I_OtherSource as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.AnyField = OtherAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName"
				+ LINE_SEP + "      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField"
				+ LINE_SEP + "      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'"
				+ LINE_SEP + "                                           P_OtherParam : 42 ) as FourthAlias"
				+ LINE_SEP + "      on AnyAlias.AnyField = FourthAlias.OtherField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_FifthSource as _FifthAlias"
				+ LINE_SEP + "     on _FifthAlias.AnyField = AnyAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..1] to I_SixthSourceWithLongName as _SixthAlias"
				+ LINE_SEP + "     on  _SixthAlias.AnyField   = AnyAlias.AnyField"
				+ LINE_SEP + "     and _SixthAlias.OtherField = AnyAlias.OtherField"
				+ LINE_SEP + "     and _SixthAlias.ThirdField = ThirdAlias.ThirdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyField,"
				+ LINE_SEP + "      OtherAlias.OtherField,"
				+ LINE_SEP + "      ThirdAlias.ThirdField,"
				+ LINE_SEP + "      FourthAlias.FourthField"
				+ LINE_SEP + "}"
				+ LINE_SEP + ""
				+ LINE_SEP + "// ... or the ON conditions continue after the alias:"
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_AnySource2 as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField"
				+ LINE_SEP + "                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'"
				+ LINE_SEP + "                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField"
				+ LINE_SEP + "                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField"
				+ LINE_SEP + "                                                                  and _SixthAlias.ThirdField = ThirdAlias.ThirdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyField,"
				+ LINE_SEP + "      OtherAlias.OtherField,"
				+ LINE_SEP + "      ThirdAlias.ThirdField,"
				+ LINE_SEP + "      FourthAlias.FourthField"
				+ LINE_SEP + "}";
	}
	
	final ConfigBoolValue configAlignDataSources = new ConfigBoolValue(this, "AlignDataSources", "Align data sources", true);
	final ConfigBoolValue configAlignAliases = new ConfigBoolValue(this, "AlignAliases", "Align aliases", true);
	final ConfigBoolValue configAlignOnConditions = new ConfigBoolValue(this, "AlignOnConditions", "Align ON conditions that continue on the same line", true);
	final ConfigBoolValue configAlignAssociationsWithJoins = new ConfigBoolValue(this, "AlignAssociationsWithJoins", "Align ASSOCIATIONs together with JOINs", false);
	final ConfigBoolValue configConsiderAllParamAssignLines = new ConfigBoolValue(this, "ConsiderAllParamAssignLines", "Consider all lines of multi-line parameter assignments", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignDataSources, configAlignAliases, configAlignOnConditions, configAlignAssociationsWithJoins, configConsiderAllParamAssignLines };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignDataSourcesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		if (command.getParent() != null)
			return false;
		
		Token dataSource = command.getDdlFromDataSource();
		if (dataSource == null) // pro forma 
			return false;

		// build and align table of SELECT and JOIN Commands
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		try {
			buildTable(table, command, dataSource, false);
			command = command.getNextNonCommentSibling();
			while (command != null) {
				Token joinTarget = command.getDdlJoinTarget();
				if (joinTarget == null) 
					break;
				buildTable(table, command, joinTarget, true);
				command = command.getNextNonCommentCommand();
			}
			// unless ASSOCIATIONs shall be aligned with JOINs, ...
			if (!configAlignAssociationsWithJoins.getValue()) {
				// align the table of SELECT FROM and JOIN Commands 
				alignTable(code, table);
				table = new AlignTable(MAX_COLUMN_COUNT);
			}
		} catch (UnexpectedSyntaxException e) {
			// exceptions are only thrown before the alignment is done, therefore no need to throw an UnexpectedSyntaxAfterChanges exception
			return false;
		}
		
		// build and align table of ASSOCIATION Commands (possibly adding them to the table of SELECT FROM and JOIN Commands)
		try {
			while (command != null) {
				Token associationTarget = command.getDdlAssociationTarget();
				if (associationTarget == null) 
					break;
				buildTable(table, command, associationTarget, true);
				command = command.getNextNonCommentCommand();
			}
			alignTable(code, table);
			
		} catch (UnexpectedSyntaxException e) {
			// exceptions are only thrown before the alignment is done, therefore no need to throw an UnexpectedSyntaxAfterChanges exception
			return false;
		}
		
		// code.addRuleUse() was already called in alignTable()
		return false;
	}
	
	private void buildTable(AlignTable table, Command command, Token dataSource, boolean hasOnCondition) throws UnexpectedSyntaxException {
		AlignLine line = table.addLine();
		Token firstInLine = dataSource.getFirstTokenInLine();
		
		// fill keywords cell, e.g. "AS SELECT FROM", "INNER JOIN", "ASSOCIATION [0..*] TO"
		if (firstInLine != dataSource) {
			Token keywordsLast = dataSource.getPrevCodeSibling();
			Term keywordsTerm = Term.createForTokenRange(firstInLine, keywordsLast);
			line.setCell(Columns.KEYWORDS.getValue(), new AlignCellTerm(keywordsTerm));
		}
		
		Token aliasName = dataSource.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "AS", TokenSearch.ANY_IDENTIFIER);
		Token asToken = (aliasName != null) ? aliasName.getPrevCodeSibling() : null;
		
		Token onToken = null;
		if (hasOnCondition) {
			Token searchStart = (aliasName != null) ? aliasName : dataSource;
			onToken = searchStart.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "ON");
		}
		
		// determine the last Token of the data source, which could be the closing ")" of parameter assignments
		Token dataSourceLast;
		if (asToken != null) 
			dataSourceLast = asToken.getPrevCodeSibling();
		else if (onToken != null) 
			dataSourceLast = onToken.getPrevCodeSibling();
		else 
			dataSourceLast = command.getLastCodeToken();
			
		// fill data source cell, including parameter assignments ( P_Any : P_Any ... )
		Term dataSourceTerm = Term.createForTokenRange(dataSource, dataSourceLast);
		AlignCell dataSourceCell = new AlignCellTerm(dataSourceTerm);
		line.setCell(Columns.DATA_SOURCE.getValue(), dataSourceCell);
		
		// fill alias cell
		if (asToken != null) {
			Term aliasTerm = Term.createForTokenRange(asToken, aliasName);
			line.setCell(Columns.AS_ALIAS.getValue(), new AlignCellTerm(aliasTerm));
		}
		
		// fill condition cell, unless "ON" was moved to the next line
		if (onToken != null && onToken.lineBreaks == 0) {
			Term conditionTerm = Term.createForTokenRange(onToken, command.getLastCodeToken());
			line.setCell(Columns.ON_CONDITION.getValue(), new AlignCellTerm(conditionTerm));
		}
	}
	
	private void alignTable(Code code, AlignTable table) throws UnexpectedSyntaxException {
		AlignColumn keywordColumn = table.getColumn(Columns.KEYWORDS.getValue());
		if (keywordColumn.isEmpty())
			return;

		// line starts of the KEYWORDS column are already indented correctly, but their indent may deliberately differ,  
		// esp. between [AS] SELECT FROM and [INNER etc.] JOIN; we must therefore set "additional indent" to ensure this 
		// indentation is kept when aligning the table
		int minIndent = Integer.MAX_VALUE;
		for (int line = 0; line < table.getLineCount(); ++line) {
			AlignCell cell = keywordColumn.getCellFromLine(line);
			if (cell != null) {
				Token firstToken = cell.getFirstToken();
				minIndent = Math.min(minIndent, firstToken.spacesLeft);
			}
		}
		for (int line = 0; line < table.getLineCount(); ++line) {
			AlignCell cell = keywordColumn.getCellFromLine(line);
			if (cell != null) {
				Token firstToken = cell.getFirstToken();
				cell.setAdditionalIndent(firstToken.spacesLeft - minIndent);
			}
		}

		// join columns if they shall not be aligned
		Columns columnWithDataSources = Columns.DATA_SOURCE;
		if (!configAlignDataSources.getValue()) { 
			table.getColumn(Columns.DATA_SOURCE.getValue()).joinIntoPreviousColumns(true);
			columnWithDataSources = Columns.KEYWORDS;
		}

		if (!configAlignAliases.getValue()) 
			table.getColumn(Columns.AS_ALIAS.getValue()).joinIntoPreviousColumns(true);

		if (!configAlignOnConditions.getValue()) 
			table.getColumn(Columns.ON_CONDITION.getValue()).joinIntoPreviousColumns(true);

		// if configured, only consider the length of the last parameter assignment line
		// (this can only be done now, because the data source may have been joined into the KEYWORDS column above)
		if (!configConsiderAllParamAssignLines.getValue() ) {
			AlignColumn dataSourceColumn = table.getColumn(columnWithDataSources.getValue());
			for (int line = 0; line < table.getLineCount(); ++line) {
				AlignCellTerm dataSourceCell = (AlignCellTerm)dataSourceColumn.getCellFromLine(line);
				if (!dataSourceCell.getTerm().isOnSingleLine()) {
					int condensedWidth = dataSourceCell.getLastToken().getEndIndexInLine() - dataSourceCell.getFirstToken().getStartIndexInLine();
					dataSourceCell.setOverrideTextWidth(condensedWidth);
				}
			}
		}

		// align the table
		Command[] changedCommands = table.align(minIndent, 1, true);
		code.addRuleUses(this, changedCommands);
	}
}
