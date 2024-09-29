package com.sap.adt.abapcleaner.rules.ddl.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;

public class DdlEmptyLinesBetweenSectionsRule extends RuleForDdlCommands {
	@Override
	public RuleID getID() { return RuleID.DDL_EMPTY_LINES_BETWEEN; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Standardize empty lines between sections"; }

	@Override
	public String getDescription() { return "Standardizes empty lines between the sections for entity annotations, parameters, joins, associations, etc."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 16); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@EndUserText.label: 'Any Description'"
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    P_AnyParam:   any_parameter_type,"
				+ LINE_SEP + "    P_OtherParam: other_type"
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + "    inner join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + "    left outer to one join I_ThirdEntity as ThirdAlias"
				+ LINE_SEP + "      on  AnyAlias.IdField    = ThirdAlias.IdField"
				+ LINE_SEP + "      and AnyAlias.SubIdField = ThirdAlias.SubIdField"
				+ LINE_SEP + "  // comment on association"
				+ LINE_SEP + "  association [0..*] to I_FourthEntity as _FourthAlias"
				+ LINE_SEP + "    on AnyAlias.IdField = _FourthAlias.IdField"
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "  key AnyAlias.OtherKeyField,"
				+ LINE_SEP + "      @Annotation.subAnno: 'value'"
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + "}"
				+ LINE_SEP + "where AnyNonKeyField > 10"
				+ LINE_SEP + ""
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_AnyEntity2 as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_OtherEntity2 as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "  // comment on association"
				+ LINE_SEP + "  association [0..*] to I_ThirdEntity2 as _ThirdAlias"
				+ LINE_SEP + "    on  AnyAlias.IdField    = _ThirdAlias.IdField"
				+ LINE_SEP + "    and AnyAlias.SubIdField = _ThirdAlias.SubIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_FourthEntity2 as _FourthAlias"
				+ LINE_SEP + "    on AnyAlias.IdField = _FourthAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "  key AnyAlias.OtherKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "}"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "where AnyNonKeyField > 10"
				+ LINE_SEP + ""
				+ LINE_SEP + ""
				+ LINE_SEP + "";
	}
	
	private static final String[] emptyLineTypeSelection = new String[] { "always at least one", "always exactly one", "keep as is", "never" };
	
	final ConfigEnumValue<DdlEmptyLineType> configBetweenEntityAnnosAndDefine = new ConfigEnumValue<DdlEmptyLineType>(this, "BetweenEntityAnnosAndDefine", "Empty line between entity annotations and DEFINE etc.:", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
	final ConfigEnumValue<DdlEmptyLineType> configBetweenParametersAndAsSelect = new ConfigEnumValue<DdlEmptyLineType>(this, "BetweenParametersAndAsSelect", "Empty line between parameters and AS SELECT FROM:", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
	final ConfigEnumValue<DdlEmptyLineType> configBetweenSelectFromAndJoins = new ConfigEnumValue<DdlEmptyLineType>(this, "BetweenSelectFromAndJoins", "Empty line between SELECT FROM and JOINs:", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_EXACTLY_ONE);
	final ConfigEnumValue<DdlEmptyLineType> configBetweenJoinsAndAssociations = new ConfigEnumValue<DdlEmptyLineType>(this, "BetweenJoinsAndAssociations", "Empty line between JOINs and ASSOCIATIONs:", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

	final ConfigEnumValue<DdlEmptyLineType> configBeforeSelectListStart = new ConfigEnumValue<DdlEmptyLineType>(this, "BeforeSelectListStart", "Empty line before select list start '{':", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
	final ConfigEnumValue<DdlEmptyLineType> configAfterSelectListStart = new ConfigEnumValue<DdlEmptyLineType>(this, "AfterSelectListStart", "Empty line after select list start '{':", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.NEVER);
	final ConfigEnumValue<DdlEmptyLineType> configBeforeSelectListEnd = new ConfigEnumValue<DdlEmptyLineType>(this, "BeforeSelectListEnd", "Empty line before select list end '}':", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.NEVER);
	final ConfigEnumValue<DdlEmptyLineType> configAfterSelectListEnd = new ConfigEnumValue<DdlEmptyLineType>(this, "AfterSelectListEnd", "Empty line after select list end '}':", emptyLineTypeSelection, DdlEmptyLineType.values(), DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

	final ConfigBoolValue configRemoveAtDocumentEnd = new ConfigBoolValue(this, "RemoveAtDocumentEnd", "Remove empty lines at document end", true);
   final ConfigIntValue configMaxConsecutiveEmptyLines = new ConfigIntValue(this, "MaxConsecutiveEmptyLines", "Maximum number of consecutive empty lines:", "", 1, 2, 99);

	private final ConfigValue[] configValues = new ConfigValue[] { configBetweenEntityAnnosAndDefine, configBetweenParametersAndAsSelect, configBetweenSelectFromAndJoins, configBetweenJoinsAndAssociations,
			configBeforeSelectListStart, configAfterSelectListStart, configBeforeSelectListEnd, configAfterSelectListEnd,
			configRemoveAtDocumentEnd, configMaxConsecutiveEmptyLines };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlEmptyLinesBetweenSectionsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// remove empty lines at document end
		if (command.representsEmptyLinesAtCodeEnd() && configRemoveAtDocumentEnd.getValue()) {
			try {
				command.removeFromCode();
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
			return true;
		}
		
		boolean changed = false;
		
		// remove multiple lines within the Command
		int maxLineBreaks = configMaxConsecutiveEmptyLines.getValue() + 1;
		Token token = command.getFirstToken();
		while (token != null) {
			if (token.lineBreaks > maxLineBreaks) {
				token.lineBreaks = maxLineBreaks;
				changed = true;
			}
			token = token.getNext();
		}

		// the remaining actions one apply on non-comment Commands that are (directly or indirectly) preceded by another non-comment Command 
		if (command.isCommentLine())
			return changed;
		Command prevNonComment = command.getPrevNonCommentCommand();
		if (prevNonComment == null)
			return changed;
		
		if (command.startsDdlOrDclDefinition()) {
			if (prevNonComment.isDdlAnnotation()) { // pro forma
				// empty line between annotations and DEFINE etc.
				standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBetweenEntityAnnosAndDefine.getValue()));
			}

		}  else if (command.getClosesLevel() && (command.getPrevSibling().startsDdlEntityParameters() || command.getPrevSibling().startsDdlSelectListBeforeFrom())) {
			// empty line between parameters and AS SELECT - or, for DDIC-based views, between select list and "FROM data source"
			standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBetweenParametersAndAsSelect.getValue()));

		} else if (command.startsDdlJoin()) {
			// empty line between AS SELECT FROM and JOINs
			if (prevNonComment.startsDdlFromClause()) {
				standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBetweenSelectFromAndJoins.getValue()));
			}
		
		} else if (command.startsDdlAssociation()) {
			if (prevNonComment.startsDdlFromClause()) {
				// empty line between AS SELECT FROM and JOINs (here reused for ASSOCIATIONs)
				standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBetweenSelectFromAndJoins.getValue()));
			} else if (prevNonComment.startsDdlJoin()) {
				// empty line between JOINs and ASSOCIATIONs
				standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBetweenJoinsAndAssociations.getValue()));
			} 
		
		} else if (command.startsDdlSelectListWithBrace()) {
			// empty line before select list start '{'
			standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBeforeSelectListStart.getValue()));

			// empty line after select list start '{'
			if (command.hasChildren()) { // pro forma; the first child could also be a non-attached comment
				standardizeEmptyLineBefore(code, command.getFirstChild(), DdlEmptyLineType.forValue(configAfterSelectListStart.getValue()));
			}

		} else if (command.endsDdlSelectListWithBrace()) {
			// empty line before select list end '}'
			standardizeEmptyLineBefore(code, command, DdlEmptyLineType.forValue(configBeforeSelectListEnd.getValue()));

			// empty line after select list end '}'
			if (command.getNext() != null) { // the next Command could also be a non-attached comment
				standardizeEmptyLineBefore(code, command.getNext(), DdlEmptyLineType.forValue(configAfterSelectListEnd.getValue()));
			}
		}

		return changed;
	}

	private void standardizeEmptyLineBefore(Code code, Command command, DdlEmptyLineType putEmptyLineBefore) {
		if (putEmptyLineBefore == DdlEmptyLineType.KEEP_AS_IS)
			return;
		
		Command changeCommand = command.getStartOfAttachedComments();
		Token firstToken = changeCommand.getFirstToken();
		if (firstToken.lineBreaks == 0)
			return;
		
		int newLineBreaks;
		if (putEmptyLineBefore == DdlEmptyLineType.ALWAYS_AT_LEAST_ONE)
			newLineBreaks = Math.max(firstToken.lineBreaks, 2);
		else if (putEmptyLineBefore == DdlEmptyLineType.ALWAYS_EXACTLY_ONE)
			newLineBreaks = 2;
		else // (putEmptyLineBefore == DdlEmptyLineType.NEVER)
			newLineBreaks = 1;
		
		if (firstToken.setLineBreaks(newLineBreaks)) {
			code.addRuleUse(this, changeCommand);
		}
	}
}
