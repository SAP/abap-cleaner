package com.sap.adt.abapcleaner.rules.ddl.position;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlPosition;

public class DdlPositionSelectRule extends RuleForDdlPosition {
	final static String defaultDisplayName = "Break before AS SELECT etc.";
	
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };
	
	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_SELECT; }

	@Override
	public String getDisplayName() { return defaultDisplayName; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation before entity name, WITH PARAMETERS, [AS] SELECT, AS PROJECTION ON and data source."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 2); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity C_AnyEntity as select"
				+ LINE_SEP + "from I_AnyEntity as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "      AnyAlias.AnyNonKeyField"
				+ LINE_SEP + "}"
				+ LINE_SEP + ""
				+ LINE_SEP + "union"
				+ LINE_SEP + "  all"
				+ LINE_SEP + "    select"
				+ LINE_SEP + "      from"
				+ LINE_SEP + "        I_ThirdEntity As ThirdAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key ThirdAlias.AnyKeyField,"
				+ LINE_SEP + "      ThirdAlias.AnyNonKeyField"
				+ LINE_SEP + "}"
				+ LINE_SEP + ""
				+ LINE_SEP + "except select"
				+ LINE_SEP + "from I_FourthEntity As FourthAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key FourthAlias.AnyKeyField,"
				+ LINE_SEP + "      FourthAlias.AnyNonKeyField"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeAsSelectFrom = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeAsSelectFrom", "Break before AS SELECT FROM:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configAsSelectFromIndent = new ConfigIntValue(this, "AsSelectFromIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeSelectFrom = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeSelectFrom", "Break before SELECT FROM after UNION etc.", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configSelectFromIndent = new ConfigIntValue(this, "SelectFromIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeAsProjectionOn = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeAsProjectionOn", "Break before AS PROJECTION ON:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configAsProjectionOnIndent = new ConfigIntValue(this, "AsProjectionOnIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeDataSource = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeDataSource", "Break before data source:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configDataSourceIndent = new ConfigIntValue(this, "DataSourceIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);

	private final ConfigValue[] configValues = new ConfigValue[] { configBreakBeforeAsSelectFrom, configAsSelectFromIndent, configBreakBeforeSelectFrom, configSelectFromIndent, configBreakBeforeAsProjectionOn, configAsProjectionOnIndent, configBreakBeforeDataSource, configDataSourceIndent };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionSelectRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		boolean changed = false;

		DdlLineBreak breakBeforeAsSelectFrom = DdlLineBreak.forValue(configBreakBeforeAsSelectFrom.getValue());
		DdlLineBreak breakBeforeSelectFrom = DdlLineBreak.forValue(configBreakBeforeSelectFrom.getValue());
		DdlLineBreak breakBeforeAsProjectionOn = DdlLineBreak.forValue(configBreakBeforeAsProjectionOn.getValue());
		
		Token token = command.getFirstCodeToken();
		
		while (token != null) {
			// (un)break before "AS SELECT [DISTINCT] FROM"
			Token fromToken = token.getLastTokenOnSiblings(true, "AS", "SELECT", TokenSearch.makeOptional("DISTINCT"), "FROM");
			if (breakBeforeAsSelectFrom != DdlLineBreak.KEEP_AS_IS && fromToken != null ) {
				// after own parameters, add an extra empty line
				boolean emptyLine = command.getClosesLevel() && token == command.getFirstCodeToken();
				changed |= breakBefore(token, breakBeforeAsSelectFrom, emptyLine, configAsSelectFromIndent.getValue());
				changed |= condense(token, fromToken);
				changed |= breakBeforeDataSource(fromToken.getNextCodeSibling());
				break; // nothing else to be expected in this Command
			} 
			
			// (un)break before "AS PROJECTION ON"
			Token onToken = token.getLastTokenOnSiblings(true, "AS", "PROJECTION", "ON");
			if (breakBeforeAsProjectionOn != DdlLineBreak.KEEP_AS_IS && onToken != null) {
				// after own parameters, add an extra empty line
				boolean emptyLine = command.getClosesLevel() && token == command.getFirstCodeToken();
				changed |= breakBefore(token, breakBeforeAsProjectionOn, emptyLine, configAsProjectionOnIndent.getValue());
				changed |= condense(token, onToken);
				changed |= breakBeforeDataSource(onToken.getNextCodeSibling());
				break; // nothing else to be expected in this Command
			}

			// (un)break before "SELECT [DISTINCT] FROM" after {EXCEPT|INTERSECT|UNION [ALL]} 
			Token selectToken = token.getLastTokenOnSiblings(true, "EXCEPT|INTERSECT|UNION", TokenSearch.makeOptional("ALL"), "SELECT");
			if (breakBeforeSelectFrom != DdlLineBreak.KEEP_AS_IS && selectToken != null ) {
				changed |= condense(token, selectToken.getPrevCodeSibling());
				changed |= breakBefore(selectToken, breakBeforeSelectFrom, false, configSelectFromIndent.getValue());
				
				Token fromToken2 = selectToken.getLastTokenOnSiblings(true, "SELECT", TokenSearch.makeOptional("DISTINCT"), "FROM");
				if (fromToken2 != null) {
					changed |= condense(selectToken, fromToken2);
					changed |= breakBeforeDataSource(fromToken2.getNextCodeSibling());
				}
				break; // nothing else to be expected in this Command
			} 
			
			token = token.getNextCodeSibling();
		}

		return changed;
	}
	
	private boolean breakBeforeDataSource(Token dataSourceToken) {
		// (un)break before data source
		DdlLineBreak breakBeforeDataSource = DdlLineBreak.forValue(configBreakBeforeDataSource.getValue());
		if (breakBeforeDataSource != DdlLineBreak.KEEP_AS_IS && dataSourceToken != null) {
			return breakBefore(dataSourceToken, breakBeforeDataSource, false, configDataSourceIndent.getValue());
		}
		return false;
	}
}
