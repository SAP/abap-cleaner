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

public class DdlPositionJoinRule extends RuleForDdlPositionJoinOrAssociation {
	public final static String displayName = "Break before JOINs";
	
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_JOIN; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation of JOINs."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  left outer to one"
				+ LINE_SEP + "  join I_ThirdEntity as ThirdAlias"
				+ LINE_SEP + "  on  AnyAlias.IdField      = ThirdAlias.IdField"
				+ LINE_SEP + "  and OtherAlias.SubIdField = ThirdAlias.SubIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..1] to I_FourthEntity as _Fourth"
				+ LINE_SEP + "    on  $projection.IdField = _Fourth.IdField"
				+ LINE_SEP + "    and _Fourth.CondField   = 'X'"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.IdField,"
				+ LINE_SEP + "  key ThirdAlias.SubIdField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField,"
				+ LINE_SEP + "      ThirdAlias.OtherNonKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      _Fourth"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeKeywords = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeKeywords", "Break before keywords:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configKeywordsIndent = new ConfigIntValue(this, "KeywordsIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeDataSource = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeDataSource", "Break before data source:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configDataSourceIndent = new ConfigIntValue(this, "DataSourceIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlConditionLineBreak> configBreakBeforeCondition = new ConfigEnumValue<DdlConditionLineBreak>(this, "BreakBeforeCondition", "Break before condition:", conditionLineBreakSelection, DdlConditionLineBreak.values(), DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
	final ConfigIntValue configConditionIndent = new ConfigIntValue(this, "ConditionIndent", "Indent if breaking:", "", 0, 6, MAX_INDENT);
	
	private final ConfigValue[] configValues = new ConfigValue[] { 
			configBreakBeforeKeywords, configKeywordsIndent,
			configBreakBeforeDataSource, configDataSourceIndent,
			configBreakBeforeCondition, configConditionIndent
	};
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionJoinRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token firstCode) {
		if (!firstCode.startsDdlJoin()) 
			return false;

		// read configuration
		boolean isJoin = true;
		boolean isFirst = (joinCount == 0);
		++joinCount;

		DdlLineBreak breakBeforeKeywords = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeKeywords.getValue()));
		int indentKeywords = configKeywordsIndent.getValue();
		DdlLineBreak breakBeforeDataSource = DdlLineBreak.forValue(configBreakBeforeDataSource.getValue());
		int indentDataSource = configDataSourceIndent.getValue();
		DdlLineBreak breakBeforeCondition = getLineBreakForCondition(DdlConditionLineBreak.forValue(configBreakBeforeCondition.getValue()), isJoin);
		int indentCondition = configConditionIndent.getValue();
		DdlLineBreak breakBeforeFilter = DdlLineBreak.KEEP_AS_IS;
		int indentFilter = -1;

		return executeOn(code, command, firstCode, isJoin, isFirst, breakBeforeKeywords, indentKeywords, breakBeforeDataSource, indentDataSource, breakBeforeCondition, indentCondition, breakBeforeFilter, indentFilter);
	}
}
