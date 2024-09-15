package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
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

public class DdlAlignFunctionParametersRule extends RuleForDdlAlignParameters {
	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_FUNCTION_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align function parameters after =>"; }

	@Override
	public String getDescription() { return "Aligns the parameters of special built-in functions for currency and unit conversion, and of analytical scalar functions."; }

	public String getHintsAndRestrictions() { return "Space inside parentheses is configured in '" + DdlSpacesAroundBracketsRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 14); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity I_AnyEntity"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    P_DisplayCurrency     : vdm_v_display_currency,"
				+ LINE_SEP + "    P_ExchangeRateType    : kurst,"
				+ LINE_SEP + "    P_ExchangeRateDate    : vdm_v_exchange_rate_date,"
				+ LINE_SEP + "    P_DisplayUnit         : meins"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from I_AnySource as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_OtherSource as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.AnyKeyField = OtherAlias.AnyKeyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association to I_ThirdSource  as _PreviousPeriod"
				+ LINE_SEP + "    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift("
				+ LINE_SEP + "                                                               fiscal_variant => $projection.FiscalYearVariant,"
				+ LINE_SEP + "                                                               base => $projection.FiscalYearPeriod,"
				+ LINE_SEP + "                                                               base_level => CALENDAR_FISCAL_LEVEL.#period, shift => abap.int2'-1'"
				+ LINE_SEP + "                                                                )"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "  key AnyAlias.FiscalYearVariant,"
				+ LINE_SEP + "  key AnyAlias.FiscalYearPeriod,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // special built-in functions"
				+ LINE_SEP + "      currency_conversion("
				+ LINE_SEP + "       amount => AnyAlias.Amount,"
				+ LINE_SEP + "        source_currency =>AnyAlias.Currency,"
				+ LINE_SEP + "     target_currency =>$parameters.P_DisplayCurrency,"
				+ LINE_SEP + "      exchange_rate_type=> $parameters.P_ExchangeRateType,"
				+ LINE_SEP + "        exchange_rate_date=>"
				+ LINE_SEP + "          $parameters.P_ExchangeRateDate"
				+ LINE_SEP + "//    round   =>'X',"
				+ LINE_SEP + "// error_handling => 'SET_TO_NULL'"
				+ LINE_SEP + "    ) as AmountInDisplayCurrency,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      unit_conversion"
				+ LINE_SEP + "       (quantity => AnyAlias.Quantity, source_unit =>"
				+ LINE_SEP + "        AnyAlias.QuantityUnit, target_unit=> $parameters.P_DisplayUnit)"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // analytical scalar functions"
				+ LINE_SEP + "      power(base=> power(base=> OtherAlias.AnyNumericValue,"
				+ LINE_SEP + "      exponent => 3), exponent => 2) as ValueCubeSquare,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      ratio_of( portion  =>"
				+ LINE_SEP + "(  OtherAlias.TotalAmount"
				+ LINE_SEP + " - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,"
				+ LINE_SEP + "total =>OtherAlias.TotalAmount) as AmountRatio,"
				+ LINE_SEP + ""
				+ LINE_SEP + "     _PreviousPeriod"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlFunctionParamPos> configParameterPos = new ConfigEnumValue<DdlFunctionParamPos>(this, "ParameterPos", "Position of parameters",
			new String[] { "continue after (", "below function name + 2", "below function name + 4", "below first parameter as is" }, DdlFunctionParamPos.values(), DdlFunctionParamPos.CONTINUE);
	final ConfigBoolValue configAlignAssignmentOps = new ConfigBoolValue(this, "AlignAssignmentOps", "Align => in own column", true);
	final ConfigBoolValue configAlignActualParams = new ConfigBoolValue(this, "AlignActualParams", "Align actual parameters in own column", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configParameterPos, configAlignAssignmentOps, configAlignActualParams };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignFunctionParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	private DdlSourceParamPos translateParamPos(DdlFunctionParamPos functionParamPos) {
		switch (functionParamPos) {
			case CONTINUE:
				return DdlSourceParamPos.CONTINUE;
			case FUNCTION_NAME_PLUS_2:
				return DdlSourceParamPos.SOURCE_NAME_PLUS_2;
			case FUNCTION_NAME_PLUS_4:
				return DdlSourceParamPos.SOURCE_NAME_PLUS_4;
			case KEEP_AS_IS:
				return DdlSourceParamPos.KEEP_AS_IS;
			default: // pro forma
				return DdlSourceParamPos.CONTINUE;
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		
		// translate the configured parameter position into DdlSourceParamPos (of which DdlFunctionParamPos is a subset), as required by the parent class
		DdlSourceParamPos paramPos = translateParamPos(DdlFunctionParamPos.forValue(configParameterPos.getValue()));
		boolean alignAssignmentOps = configAlignAssignmentOps.getValue();
		boolean alignActualParams = configAlignActualParams.getValue();

		HashSet<Token> alignedParentTokens = new HashSet<>();
		
		Token token = command.getFirstToken();
		while (token != null) {
			Token parentToken = token.getParent();
			
			// built-in functions currency_conversion(...), replace_regexpr(...) and unit_conversion(...); scalar functions like ratio_of(...)
			if (token.textEquals(DDL.FUNCTION_PARAM_ASSIGNMENT_OP) && parentToken != null && 
					parentToken.textEquals(DDL.PARENS_OPEN_STRING) && !alignedParentTokens.contains(parentToken)) {
				changed |= moveOpeningParenthesis(parentToken);
				changed |= alignParameters(code, parentToken, DDL.FUNCTION_PARAM_ASSIGNMENT_OP, paramPos, alignAssignmentOps, alignActualParams);
				changed |= moveTokenAfterClosingParens(token, DdlNextAfterParensPos.KEEP_AS_IS);
				
				// ensure that this function is not processed again, but continue inside the parentheses to search for inner function calls
				alignedParentTokens.add(parentToken);
			}
			token = token.getNextCodeToken();
		}
		
		return changed;
	}
}
