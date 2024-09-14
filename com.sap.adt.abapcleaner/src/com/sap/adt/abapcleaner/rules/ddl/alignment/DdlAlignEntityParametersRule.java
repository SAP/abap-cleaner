package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellToken;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlAlignParameters;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlPositionDefineRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public class DdlAlignEntityParametersRule extends RuleForDdlAlignParameters {
	private static final RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	private enum Columns {
		PARAMETER, 
		ASSIGNMENT_OP, 
		EXPRESSION;
		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 3;

	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_ENTITY_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align view parameters"; }

	@Override
	public String getDescription() { return "Aligns the parameter definitions of a view."; }

	public String getHintsAndRestrictions() { return "Parameter indent is configured in '" + DdlPositionDefineRule.displayName + "'. Space around colons (if colons are not aligned) is configured in '" + DdlSpacesAroundSignsRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 13); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public Language[] getSupportedLanguages() { return ddlOnly; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@EndUserText.label: 'Any Description'"
				+ LINE_SEP + ""
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    @AnalyticsDetails.query.variableSequence: 30"
				+ LINE_SEP + "    P_AnyParam: any_parameter_type,"
				+ LINE_SEP + ""
				+ LINE_SEP + "    // comment on OtherParam"
				+ LINE_SEP + "    @AnalyticsDetails.query.variableSequence: 50"
				+ LINE_SEP + "    P_OtherParam :           other_type,"
				+ LINE_SEP + ""
				+ LINE_SEP + "    @AnalyticsDetails.query.variableSequence: 60"
				+ LINE_SEP + "    @Consumption.defaultValue: 'NN'"
				+ LINE_SEP + "    P_ThirdParameter   : third_type,"
				+ LINE_SEP + ""
				+ LINE_SEP + "    @Consumption.hidden: true"
				+ LINE_SEP + "    P_FourthParam:fourth_type"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + "}";
	}
	
	final ConfigBoolValue configAlignColons = new ConfigBoolValue(this, "AlignColons", "Align colons in own column", true);
	final ConfigBoolValue configAlignTypes = new ConfigBoolValue(this, "AlignTypes", "Align types in own column", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignColons, configAlignTypes };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignEntityParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// this rule only runs when it is called for the first Command, iterating itself over the remaining Commands
		if (command != code.firstCommand)
			return false;
		
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);

		command = code.firstCommand;
		while (command != null) {
			if (command.isDdlParametersElement()) {
				// if one of the parameter Commands is blocked for alignment, cancel alignment entirely,
				// because every Command would be needed to reliably determine column widths
				if (isCommandBlocked(command)) 
					return false;

				try {
					addToTable(command, table);
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxBeforeChanges(this, e);
				}
			}
			command = command.getNextNonCommentCommand();
		}
		
		if (table.isEmpty()) 
			return false;

		joinColumns(code, table, configAlignColons.getValue(), configAlignTypes.getValue(), DDL.COLON_SIGN_STRING);
		
		// align the table
		int paramsIndent = getParamsIndent();
		Command[] changedCommands = table.align(paramsIndent, 1, true);
		for (Command changedCommand : changedCommands) {
			code.addRuleUse(this, changedCommand);
		}
		return false;
	}
	
	private void addToTable(Command command, AlignTable table) throws UnexpectedSyntaxException {
		// identify parameter name, colon and typing
		Token parameterName = command.getFirstCodeToken();
		Token colon = parameterName.getNextCodeSibling();
		if (colon == null || !colon.textEquals(DDL.COLON_SIGN_STRING))
			throw new UnexpectedSyntaxException("Colon expected in this position");

		Token typeToken = colon.getNextCodeSibling();
		Token lastToken = command.getLastCodeToken();
		Term typeTerm = Term.createForTokenRange(typeToken, lastToken);
		
		// build the next table line 
		AlignLine line = table.addLine();
		line.setCell(Columns.PARAMETER.getValue(), new AlignCellToken(parameterName));
		line.setCell(Columns.ASSIGNMENT_OP.getValue(), new AlignCellToken(colon));
		line.setCell(Columns.EXPRESSION.getValue(), new AlignCellTerm(typeTerm));
	}
}
