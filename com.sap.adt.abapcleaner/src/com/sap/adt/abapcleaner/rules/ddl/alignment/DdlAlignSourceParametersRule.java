package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlAlignParameters;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public class DdlAlignSourceParametersRule extends RuleForDdlAlignParameters {
	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_SOURCE_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align source parameters"; }

	@Override
	public String getDescription() { return "Aligns the parameters supplied to a data source."; }

	public String getHintsAndRestrictions() { return "Space inside parentheses is configured in '" + DdlSpacesAroundBracketsRule.displayName + "'. Space around colons (if colons are not aligned) is configured in '" + DdlSpacesAroundSignsRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 13); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity I_AnyEntity"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    P_AnyParam       : any_parameter_type,"
				+ LINE_SEP + "    P_OtherParam     : other_type,"
				+ LINE_SEP + "    P_ThirdParameter : third_type,"
				+ LINE_SEP + "    P_FourthParam    : fourth_type"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from I_OtherEntity"
				+ LINE_SEP + "  ("
				+ LINE_SEP + "   P_AnyParam : $parameters.P_AnyParam,"
				+ LINE_SEP + "     P_OtherParam    :   $parameters.P_OtherParam,"
				+ LINE_SEP + "    // comment"
				+ LINE_SEP + " P_ThirdParameter        :"
				+ LINE_SEP + "    $parameters.P_ThirdParameter,"
				+ LINE_SEP + "  P_FourthParam"
				+ LINE_SEP + "  : $parameters.P_FourthParam "
				+ LINE_SEP + " )"
				+ LINE_SEP + "   as OtherAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_ThirdEntity"
				+ LINE_SEP + "          (P_AnyParam  :  $parameters.P_AnyParam,"
				+ LINE_SEP + "//        P_OtherParam : $parameters.P_OtherParam,"
				+ LINE_SEP + "//        P_ThirdParam:$parameters.P_ThirdParameter,"
				+ LINE_SEP + "          P_FourthParameter: $parameters.P_FourthParam)"
				+ LINE_SEP + "          as ThirdAlias"
				+ LINE_SEP + "      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField"
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key OtherAlias.AnyKeyField,"
				+ LINE_SEP + "      ThirdAlias.AnyNonKeyField"
				+ LINE_SEP + "}"
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_FourthEntity( P_AnyParam   :"
				+ LINE_SEP + "         $parameters.P_AnyParam, P_OtherParam        :"
				+ LINE_SEP + "         $parameters.P_OtherParam, P_ThirdParameter :"
				+ LINE_SEP + "         $parameters.P_ThirdParameter, P_FourthParam:"
				+ LINE_SEP + "         $parameters.P_FourthParam  ) as FourthAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key FourthAlias.AnyKeyField,"
				+ LINE_SEP + "      FourthAlias.AnyNonKeyField"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlSourceParamPos> configParameterPos = new ConfigEnumValue<DdlSourceParamPos>(this, "ParameterPos", "Position of parameters",
			new String[] { "continue after (", "below line start + 2", "below line start + 4", "below view name + 2", "below view name + 4", "below first parameter as is" }, DdlSourceParamPos.values(), DdlSourceParamPos.SOURCE_NAME_PLUS_2);
	final ConfigBoolValue configAlignAssignmentOps = new ConfigBoolValue(this, "AlignAssignmentOps", "Align colons in own column", true);
	final ConfigBoolValue configAlignActualParams = new ConfigBoolValue(this, "AlignActualParams", "Align actual parameters in own column", true);
	final ConfigEnumValue<DdlNextAfterParensPos> configAsAliasPos = new ConfigEnumValue<DdlNextAfterParensPos>(this, "AsAliasPos", "Position of AS <alias>",
			new String[] { "continue after )", "below line start", "below line start + 2", "below view name", "below view name + 2", "keep as is" }, DdlNextAfterParensPos.values(), DdlNextAfterParensPos.CONTINUE);

	private final ConfigValue[] configValues = new ConfigValue[] { configParameterPos, configAlignAssignmentOps, configAlignActualParams, configAsAliasPos };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignSourceParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		
		// nothing to do inside parameter lists or element lists
		if (command.getParent() != null)
			return false;
		
		DdlSourceParamPos sourceParamPos = DdlSourceParamPos.forValue(configParameterPos.getValue());
		boolean alignAssignmentOps = configAlignAssignmentOps.getValue();
		boolean alignActualParams = configAlignActualParams.getValue();
		DdlNextAfterParensPos asAliasPos = DdlNextAfterParensPos.forValue(configAsAliasPos.getValue());
		
		Token token = command.getFirstToken();
		while (token != null) {
			// "as select from view( ... )", "as projection on view( ... )", 
			// "union all select from view( ... )", "left outer to many join view( ... )"  etc.
			token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "from|join|on", TokenSearch.ANY_IDENTIFIER, "(");
			if (token == null) 
				break;

			changed |= moveOpeningParenthesis(token);
			changed |= alignParameters(code, token, DDL.COLON_SIGN_STRING, sourceParamPos, alignAssignmentOps, alignActualParams);
			changed |= moveTokenAfterClosingParens(token, asAliasPos);
			
			token = token.getNextSibling();
		}
		
		return changed;
	}
}
