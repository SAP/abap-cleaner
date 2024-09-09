package com.sap.adt.abapcleaner.rules.ddl.position;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
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

public class DdlPositionBracesRule extends RuleForDdlPosition {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_BRACES; }

	@Override
	public String getDisplayName() { return "Break before select list braces"; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation of braces around select lists."; }

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
				+ LINE_SEP + "  {"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "      AnyAlias.AnyNonKeyField"
				+ LINE_SEP + "      }"
				+ LINE_SEP + ""
				+ LINE_SEP + "union all "
				+ LINE_SEP + "  select from I_OtherEntity As OtherAlias {"
				+ LINE_SEP + "  key OtherAlias.AnyKeyField,"
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + "  }"
				+ LINE_SEP + ""
				+ LINE_SEP + "except"
				+ LINE_SEP + "  select from I_ThirdEntity As ThirdAlias"
				+ LINE_SEP + "  {"
				+ LINE_SEP + "  key ThirdAlias.AnyKeyField,"
				+ LINE_SEP + "      ThirdAlias.AnyNonKeyField }";
	}
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeOpeningBrace = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeOpeningBrace", "Break before opening brace { of select list:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configOpeningBraceIndent = new ConfigIntValue(this, "OpeningBraceIndent", "Indent if breaking:", "", 0, 0, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeClosingBrace = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeClosingBrace", "Break before closing brace } of select list:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configClosingBraceIndent = new ConfigIntValue(this, "ClosingBraceIndent", "Indent if breaking:", "", 0, 0, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeFrom = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeFrom", "Break before FROM (in syntax without braces):", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configFromIndent = new ConfigIntValue(this, "FromIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configBreakBeforeOpeningBrace, configOpeningBraceIndent, configBreakBeforeClosingBrace, configClosingBraceIndent, configBreakBeforeFrom, configFromIndent};
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionBracesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (command.getParent() != null)
			return false;
		
		if (command.getOpensLevel()) {
			Token braceOpen = command.getLastCodeToken();
			if (braceOpen.textEquals(DDL.BRACE_OPEN_STRING)) {
				DdlLineBreak lineBreak = DdlLineBreak.forValue(configBreakBeforeOpeningBrace.getValue());
				return breakBefore(braceOpen, lineBreak, false, configOpeningBraceIndent.getValue());
			}
		}

		if (command.getClosesLevel()) {
			Token braceClose = command.getFirstCodeToken();
			if (braceClose.textEquals(DDL.BRACE_CLOSE_STRING)) {  
				DdlLineBreak lineBreak = DdlLineBreak.forValue(configBreakBeforeClosingBrace.getValue());
				return breakBefore(braceClose, lineBreak, false, configClosingBraceIndent.getValue());
				
			} else if (braceClose.isKeyword("FROM")) { 
				// old syntax without braces: 'SELECT [DISTINCT] select_list FROM data_source [association1 association2 ...] [clauses]'
				DdlLineBreak lineBreak = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeFrom.getValue()));
				return breakBefore(braceClose, lineBreak, false, configFromIndent.getValue());
			}
		}

		return false;
	}
}
