package com.sap.adt.abapcleaner.rules.ddl.position;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class DdlPositionAssociationRule extends RuleForDdlPositionJoinOrAssociation {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_ASSOCIATION; }

	@Override
	public String getDisplayName() { return "Break before ASSOCIATIONs"; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation of ASSOCIATIONs."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 8); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField"
				+ LINE_SEP + "                                                  and _Fourth.CondField   = 'X'"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association of many"
				+ LINE_SEP + "  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField"
				+ LINE_SEP + "                                  and $projection.SubIdField = _Fifth.SubIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.IdField,"
				+ LINE_SEP + "  key OtherAlias.SubIdField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField,"
				+ LINE_SEP + "      OtherAlias.OtherNonKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      _Fourth,"
				+ LINE_SEP + "      _Fifth"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeKeywords = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeKeywords", "Break before keywords:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configKeywordsIndent = new ConfigIntValue(this, "KeywordsIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeDataSource = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeDataSource", "Break before data source:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configDataSourceIndent = new ConfigIntValue(this, "DataSourceIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlConditionLineBreak> configBreakBeforeCondition = new ConfigEnumValue<DdlConditionLineBreak>(this, "BreakBeforeCondition", "Break before condition:", conditionLineBreakSelection, DdlConditionLineBreak.values(), DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
	final ConfigIntValue configConditionIndent = new ConfigIntValue(this, "ConditionIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeFilter = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeFilter", "Break before default filter:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configFilterIndent = new ConfigIntValue(this, "FilterIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	private final ConfigValue[] configValues = new ConfigValue[] { 
			configBreakBeforeKeywords, configKeywordsIndent,
			configBreakBeforeDataSource, configDataSourceIndent,
			configBreakBeforeCondition, configConditionIndent,
			configBreakBeforeFilter, configFilterIndent
	};
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionAssociationRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token firstCode) {
		if (!firstCode.startsDdlAssociation())
			return false;
		
		// read configuration
		boolean isJoin = false;
		boolean isFirst = (associationCount == 0);
		++associationCount;

		DdlLineBreak breakBeforeKeywords = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeKeywords.getValue()));
		int indentKeywords = configKeywordsIndent.getValue();
		DdlLineBreak breakBeforeDataSource = DdlLineBreak.forValue(configBreakBeforeDataSource.getValue());
		int indentDataSource = configDataSourceIndent.getValue();
		DdlLineBreak breakBeforeCondition = getLineBreakForCondition(DdlConditionLineBreak.forValue(configBreakBeforeCondition.getValue()), isJoin);
		int indentCondition = configConditionIndent.getValue();
		DdlLineBreak breakBeforeFilter = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeFilter.getValue()));
		int indentFilter = configFilterIndent.getValue();

		return executeOn(code, command, firstCode, isJoin, isFirst, breakBeforeKeywords, indentKeywords, breakBeforeDataSource, indentDataSource, breakBeforeCondition, indentCondition, breakBeforeFilter, indentFilter);
	}
}
