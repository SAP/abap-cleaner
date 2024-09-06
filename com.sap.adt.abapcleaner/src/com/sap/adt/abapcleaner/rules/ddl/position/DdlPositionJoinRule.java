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
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlPosition;

public class DdlPositionJoinRule extends RuleForDdlPosition {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_JOIN; }

	@Override
	public String getDisplayName() { return "Break before JOINs and ASSOCIATIONs"; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation before JOINs and ASSOCIATIONs."; }

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
				+ LINE_SEP + "  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField"
				+ LINE_SEP + "                                                  and _Fourth.CondField   = 'X'"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association of many"
				+ LINE_SEP + "  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField"
				+ LINE_SEP + "                                  and $projection.SubIdField = _Fifth.SubIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.IdField,"
				+ LINE_SEP + "  key ThirdAlias.SubIdField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField,"
				+ LINE_SEP + "      ThirdAlias.OtherNonKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      _Fourth,"
				+ LINE_SEP + "      _Fifth"
				+ LINE_SEP + "}";
	}
	
	private final String[] lineBreakSelection = new String[] { "Always", "Keep as is", "Never" };
	private final String[] lineBreakSelectionWithoutNever = new String[] { "Always", "Keep as is" };
	private final String[] conditionLineBreakSelection = new String[] { "Always", "If view contains multi-line condition", "Keep as is", "Never" };

	// configuration for JOIN
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeJoinKeywords = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeJoinKeywords", "Break before JOIN keywords:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configJoinKeywordsIndent = new ConfigIntValue(this, "JoinKeywordsIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeJoinDataSource = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeJoinDataSource", "Break before JOIN data source:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configJoinDataSourceIndent = new ConfigIntValue(this, "JoinDataSourceIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlConditionLineBreak> configBreakBeforeJoinCondition = new ConfigEnumValue<DdlConditionLineBreak>(this, "BreakBeforeJoinCondition", "Break before JOIN condition:", conditionLineBreakSelection, DdlConditionLineBreak.values(), DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
	final ConfigIntValue configJoinConditionIndent = new ConfigIntValue(this, "JoinConditionIndent", "Indent if breaking:", "", 0, 6, MAX_INDENT);
	
	// configuration for ASSOCIATION
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeAssociationKeywords = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeAssociationKeywords", "Break before ASSOCIATION keywords:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configAssociationKeywordsIndent = new ConfigIntValue(this, "AssociationKeywordsIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeAssociationDataSource = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeAssociationDataSource", "Break before ASSOCIATION data source:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configAssociationDataSourceIndent = new ConfigIntValue(this, "AssociationDataSourceIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlConditionLineBreak> configBreakBeforeAssociationCondition = new ConfigEnumValue<DdlConditionLineBreak>(this, "BreakBeforeAssociationCondition", "Break before ASSOCIATION condition:", conditionLineBreakSelection, DdlConditionLineBreak.values(), DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
	final ConfigIntValue configAssociationConditionIndent = new ConfigIntValue(this, "AssociationConditionIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeAssociationFilter = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeAssociationFilter", "Break before ASSOCIATION filter:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configAssociationFilterIndent = new ConfigIntValue(this, "AssociationFilterIndent", "Indent if breaking:", "", 0, 4, MAX_INDENT);
	
	private final ConfigValue[] configValues = new ConfigValue[] { 
			configBreakBeforeJoinKeywords, configJoinKeywordsIndent,
			configBreakBeforeJoinDataSource, configJoinDataSourceIndent,
			configBreakBeforeJoinCondition, configJoinConditionIndent,
			configBreakBeforeAssociationKeywords, configAssociationKeywordsIndent,
			configBreakBeforeAssociationDataSource, configAssociationDataSourceIndent,
			configBreakBeforeAssociationCondition, configAssociationConditionIndent,
			configBreakBeforeAssociationFilter, configAssociationFilterIndent
	};
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlPositionJoinRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	private boolean viewContainsMultiLineOnCond;
	private int joinCount;
	private int associationCount;
	
	@Override
	protected void prepare(Code code) {
		Command command = code.firstCommand;

		// reset the JOIN / ASSOCIATION count; the first JOIN / ASSOCIATION will always get an empty line above
		joinCount = 0;
		associationCount = 0;

		// determine whether the view contains a multi-line ON condition anywhere
		viewContainsMultiLineOnCond = false;
		while (command != null) {
			Token firstCode = command.getFirstCodeToken();
			if (command.isDdl() && !command.isCommentLine() && firstCode != null) {
				if (firstCode.startsDdlJoin() || firstCode.startsDdlAssociation()) {
					Token onToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "ON");
					if (onToken != null) {
						Token lastTokenOfLogExpr = onToken.getLastTokenOfDdlLogicalExpression();
						if (command.containsLineBreaksBetween(onToken, lastTokenOfLogExpr, false)) {
							viewContainsMultiLineOnCond = true;
							break;
						}
					}
				}
			}			
			command = command.getNextSibling(); // JOINs and ASSOCIATIONs are only found on top level
		}
	}
	
	private DdlLineBreak getLineBreakForCondition(DdlConditionLineBreak conditionLineBreak) {
		switch (conditionLineBreak) {
			case ALWAYS:
				return DdlLineBreak.ALWAYS;
			case IF_MULTI_LINE_FOUND:
				return viewContainsMultiLineOnCond ? DdlLineBreak.ALWAYS : DdlLineBreak.NEVER;
			case KEEP_AS_IS:
				return DdlLineBreak.KEEP_AS_IS;
			case NEVER:
				return DdlLineBreak.NEVER;
			default: // pro forma
				return DdlLineBreak.ALWAYS;
		}
	}

	private DdlLineBreak getLineBreak(DdlLineBreakWithoutNever keywordsLineBreak) {
		switch (keywordsLineBreak) {
			case ALWAYS:
				return DdlLineBreak.ALWAYS;
			case KEEP_AS_IS:
				return DdlLineBreak.KEEP_AS_IS;
			default: // pro forma
				return DdlLineBreak.ALWAYS;
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		boolean changed = false;

		if (command.getParent() != null)
			return false;

		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) { // pro forma
			return false;
		} else if (firstCode.textEquals(DDL.BRACE_OPEN_STRING)) {
			// reset JOIN / ASSOCIATION count in case the select list is followed by a UNION etc.
			joinCount = 0;
			associationCount = 0;
			return false;
		}

		boolean isJoin;
		boolean isFirst;
		DdlLineBreak breakBeforeKeywords;
		int indentKeywords;
		DdlLineBreak breakBeforeDataSource;
		int indentDataSource;
		DdlLineBreak breakBeforeCondition;
		int indentCondition;
		DdlLineBreak breakBeforeFilter;
		int indentFilter;
		
		if (firstCode.startsDdlJoin()) {
			// read configuration for JOIN
			isJoin = true;
			isFirst = (joinCount == 0);
			++joinCount;
			
			breakBeforeKeywords = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeJoinKeywords.getValue()));
			indentKeywords = configJoinKeywordsIndent.getValue();
			breakBeforeDataSource = DdlLineBreak.forValue(configBreakBeforeJoinDataSource.getValue());
			indentDataSource = configJoinDataSourceIndent.getValue();
			breakBeforeCondition = getLineBreakForCondition(DdlConditionLineBreak.forValue(configBreakBeforeJoinCondition.getValue()));
			indentCondition = configJoinConditionIndent.getValue();
			breakBeforeFilter = DdlLineBreak.KEEP_AS_IS;
			indentFilter = -1;
			
		} else if (firstCode.startsDdlAssociation()) {			
			// read configuration for ASSOCIATION
			isJoin = false;
			isFirst = (associationCount == 0);
			++associationCount;
			
			breakBeforeKeywords = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeAssociationKeywords.getValue()));
			indentKeywords = configAssociationKeywordsIndent.getValue();
			breakBeforeDataSource = DdlLineBreak.forValue(configBreakBeforeAssociationDataSource.getValue());
			indentDataSource = configAssociationDataSourceIndent.getValue();
			breakBeforeCondition = getLineBreakForCondition(DdlConditionLineBreak.forValue(configBreakBeforeAssociationCondition.getValue()));
			indentCondition = configAssociationConditionIndent.getValue();
			breakBeforeFilter = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeAssociationFilter.getValue()));
			indentFilter = configAssociationFilterIndent.getValue();
		
		} else {
			return false;
		}
		
		// find the data source by skipping all JOIN / ASSOCIATION keywords and (cardinality) brackets, 
		// e.g. "EXACT ONE TO EXACT ONE JOIN" or "ASSOCIATION [1..1] TO"
		Token dataSourceToken = firstCode;
		while (dataSourceToken.isKeyword() || dataSourceToken.textEqualsAny(DDL.BRACKET_OPEN_STRING, DDL.BRACKET_CLOSE_STRING)) {
			dataSourceToken = dataSourceToken.getNextCodeSibling();
			if (dataSourceToken == null) { // pro forma
				return false;
			}
		}
		Token lastKeyword = dataSourceToken.getPrevCodeSibling();

		// find the JOIN condition (optional) or the ASSOCIATION condition (mandatory) 
		Token onToken = dataSourceToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "ON");
		int oldOnTokenStartIndex = (onToken == null) ? 0 : onToken.getStartIndexInLine();

		// find the ASSOCIATION filter (optional)
		Token filterKeywordsFirst = null;
		Token filterKeywordsLast = null;
		int oldFilterTokenStartIndex = 0;
		if (!isJoin && onToken != null) { // "onToken != null" pro forma 
			filterKeywordsLast = onToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "WITH", "DEFAULT", "FILTER");
			if (filterKeywordsLast != null) {
				oldFilterTokenStartIndex = filterKeywordsLast.getStartIndexInLine(); 
				filterKeywordsFirst = filterKeywordsLast.getPrevCodeSibling().getPrevCodeSibling();
			}
		}
		
		// break before JOIN / ASSOCIATION keywords, e.g. "EXACT ONE TO EXACT ONE JOIN" or "ASSOCIATION [1..1] TO"
		changed |= breakBefore(firstCode, breakBeforeKeywords, isFirst, indentKeywords);
		changed |= condense(firstCode, lastKeyword);
		
		// break before JOIN / ASSOCIATION data source
		changed |= breakBefore(dataSourceToken, breakBeforeDataSource, false, indentDataSource);
		
		// break before JOIN / ASSOCIATION condition
		if (onToken != null) {
			changed |= breakBefore(onToken, breakBeforeCondition, false, indentCondition);
			
			// move the rest of the JOIN / ASSOCIATION condition like the ON keyword was moved
			int addIndent = onToken.getStartIndexInLine() - oldOnTokenStartIndex;
			changed |= command.addIndent(addIndent, 0, onToken.getNext(), filterKeywordsFirst, false);
		}
		
		// break before ASSOCIATION filter
		if (filterKeywordsFirst != null) {
			changed |= breakBefore(filterKeywordsFirst, breakBeforeFilter, false, indentFilter);
			changed |= condense(filterKeywordsFirst, filterKeywordsLast);

			// move the rest of the filter condition like the FILTER keyword was moved
			int addIndent = filterKeywordsLast.getStartIndexInLine() - oldFilterTokenStartIndex;
			changed |= command.addIndent(addIndent, 0, filterKeywordsLast.getNext(), null, false);
		}
		
		return changed;
	}
}
