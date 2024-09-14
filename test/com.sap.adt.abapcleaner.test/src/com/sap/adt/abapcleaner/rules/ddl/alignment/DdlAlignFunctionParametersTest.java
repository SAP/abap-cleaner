package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public class DdlAlignFunctionParametersTest extends RuleTestBase {
	private DdlAlignFunctionParametersRule rule;
	private DdlSpacesAroundSignsRule spacesAroundSignsRule;
	private DdlSpacesAroundBracketsRule spacesAroundBracketsRule;
	
	DdlAlignFunctionParametersTest() {
		super(RuleID.DDL_ALIGN_FUNCTION_PARAMETERS);

		rule = (DdlAlignFunctionParametersRule)getRule();
		spacesAroundSignsRule = (DdlSpacesAroundSignsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_SIGNS);
		spacesAroundBracketsRule = (DdlSpacesAroundBracketsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_BRACKETS); 
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configParameterPos.setEnumValue(DdlFunctionParamPos.CONTINUE);
		rule.configAlignAssignmentOps.setValue(true);
		rule.configAlignActualParams.setValue(true);

		// setup configuration of other rules that is reused by the rule under test
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.KEEP_AS_IS);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.ALWAYS);
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.NEVER);
	}

	@Test
	void testParamsContinueAfterOpeningParens() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("  association to I_ThirdSource  as _PreviousPeriod");
		buildSrc("    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(");
		buildSrc("                                                               fiscal_variant => $projection.FiscalYearVariant,");
		buildSrc("                                                               base => $projection.FiscalYearPeriod,");
		buildSrc("                                                               base_level => CALENDAR_FISCAL_LEVEL.#period, shift => abap.int2'-1'");
		buildSrc("                                                                )");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency,");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType,");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency,");
		buildSrc("");
		buildSrc("      unit_conversion");
		buildSrc("       (quantity => AnyAlias.Quantity, source_unit =>");
		buildSrc("        AnyAlias.QuantityUnit, target_unit=> $parameters.P_DisplayUnit)");
		buildSrc("");
		buildSrc("      power(base=> OtherAlias.AnyNumericValue, exponent => 2) as ValueSquare,");
		buildSrc("");
		buildSrc("      ratio_of( portion  =>");
		buildSrc("(  OtherAlias.TotalAmount");
		buildSrc(" - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,");
		buildSrc("total =>OtherAlias.TotalAmount) as AmountRatio");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("  association to I_ThirdSource  as _PreviousPeriod");
		buildExp("    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(fiscal_variant => $projection.FiscalYearVariant,");
		buildExp("                                                                base           => $projection.FiscalYearPeriod,");
		buildExp("                                                                base_level     => CALENDAR_FISCAL_LEVEL.#period,");
		buildExp("                                                                shift          => abap.int2'-1')");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(amount             => AnyAlias.Amount,");
		buildExp("                          source_currency    => AnyAlias.Currency,");
		buildExp("                          target_currency    => $parameters.P_DisplayCurrency,");
		buildExp("                          exchange_rate_type => $parameters.P_ExchangeRateType,");
		buildExp("                          exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("//                          round              => 'X',");
		buildExp("//                          error_handling     => 'SET_TO_NULL'");
		buildExp("                         ) as AmountInDisplayCurrency,");
		buildExp("");
		buildExp("      unit_conversion(quantity    => AnyAlias.Quantity,");
		buildExp("                      source_unit => AnyAlias.QuantityUnit,");
		buildExp("                      target_unit => $parameters.P_DisplayUnit)");
		buildExp("");
		buildExp("      power(base     => OtherAlias.AnyNumericValue,");
		buildExp("            exponent => 2) as ValueSquare,");
		buildExp("");
		buildExp("      ratio_of(portion => (  OtherAlias.TotalAmount");
		buildExp("                           - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,");
		buildExp("               total   => OtherAlias.TotalAmount) as AmountRatio");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowFunctionNamePlus2() {
		rule.configParameterPos.setEnumValue(DdlFunctionParamPos.FUNCTION_NAME_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("  association to I_ThirdSource  as _PreviousPeriod");
		buildSrc("    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(");
		buildSrc("                                                               fiscal_variant => $projection.FiscalYearVariant,");
		buildSrc("                                                               base => $projection.FiscalYearPeriod,");
		buildSrc("                                                               base_level => CALENDAR_FISCAL_LEVEL.#period, shift => abap.int2'-1'");
		buildSrc("                                                                )");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency,");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType,");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency,");
		buildSrc("");
		buildSrc("      unit_conversion");
		buildSrc("       (quantity => AnyAlias.Quantity, source_unit =>");
		buildSrc("        AnyAlias.QuantityUnit, target_unit=> $parameters.P_DisplayUnit)");
		buildSrc("");
		buildSrc("      power(base=> OtherAlias.AnyNumericValue, exponent => 2) as ValueSquare,");
		buildSrc("");
		buildSrc("      ratio_of( portion  =>");
		buildSrc("(  OtherAlias.TotalAmount");
		buildSrc(" - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,");
		buildSrc("total =>OtherAlias.TotalAmount) as AmountRatio");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("  association to I_ThirdSource  as _PreviousPeriod");
		buildExp("    on _PreviousPeriod.FiscalYearPeriod = fiscal_calendar_shift(");
		buildExp("                                            fiscal_variant => $projection.FiscalYearVariant,");
		buildExp("                                            base           => $projection.FiscalYearPeriod,");
		buildExp("                                            base_level     => CALENDAR_FISCAL_LEVEL.#period,");
		buildExp("                                            shift          => abap.int2'-1')");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(");
		buildExp("        amount             => AnyAlias.Amount,");
		buildExp("        source_currency    => AnyAlias.Currency,");
		buildExp("        target_currency    => $parameters.P_DisplayCurrency,");
		buildExp("        exchange_rate_type => $parameters.P_ExchangeRateType,");
		buildExp("        exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("//        round              => 'X',");
		buildExp("//        error_handling     => 'SET_TO_NULL'");
		buildExp("        ) as AmountInDisplayCurrency,");
		buildExp("");
		buildExp("      unit_conversion(");
		buildExp("        quantity    => AnyAlias.Quantity,");
		buildExp("        source_unit => AnyAlias.QuantityUnit,");
		buildExp("        target_unit => $parameters.P_DisplayUnit)");
		buildExp("");
		buildExp("      power(");
		buildExp("        base     => OtherAlias.AnyNumericValue,");
		buildExp("        exponent => 2) as ValueSquare,");
		buildExp("");
		buildExp("      ratio_of(");
		buildExp("        portion => (  OtherAlias.TotalAmount");
		buildExp("                    - OtherAlias.TotalAmount * OtherAlias.Discount) * OtherAlias.Tax,");
		buildExp("        total   => OtherAlias.TotalAmount) as AmountRatio");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowFunctionNamePlus4() {
		rule.configParameterPos.setEnumValue(DdlFunctionParamPos.FUNCTION_NAME_PLUS_4);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency,");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType,");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(");
		buildExp("          amount             => AnyAlias.Amount,");
		buildExp("          source_currency    => AnyAlias.Currency,");
		buildExp("          target_currency    => $parameters.P_DisplayCurrency,");
		buildExp("          exchange_rate_type => $parameters.P_ExchangeRateType,");
		buildExp("          exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("//          round              => 'X',");
		buildExp("//          error_handling     => 'SET_TO_NULL'");
		buildExp("          ) as AmountInDisplayCurrency");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowFirstParamAsIs() {
		rule.configParameterPos.setEnumValue(DdlFunctionParamPos.KEEP_AS_IS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency, // comment");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType, -- comment");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(");
		buildExp("       amount             => AnyAlias.Amount,");
		buildExp("       source_currency    => AnyAlias.Currency, // comment");
		buildExp("       target_currency    => $parameters.P_DisplayCurrency,");
		buildExp("       exchange_rate_type => $parameters.P_ExchangeRateType, -- comment");
		buildExp("       exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("//       round              => 'X',");
		buildExp("//       error_handling     => 'SET_TO_NULL'");
		buildExp("       ) as AmountInDisplayCurrency");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignAssignmentOps() {
		rule.configAlignAssignmentOps.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency,");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType,");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(amount =>             AnyAlias.Amount,");
		buildExp("                          source_currency =>    AnyAlias.Currency,");
		buildExp("                          target_currency =>    $parameters.P_DisplayCurrency,");
		buildExp("                          exchange_rate_type => $parameters.P_ExchangeRateType,");
		buildExp("                          exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("//                          round =>              'X',");
		buildExp("//                          error_handling =>     'SET_TO_NULL'");
		buildExp("                         ) as AmountInDisplayCurrency");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlign() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      currency_conversion(");
		buildSrc("       amount => AnyAlias.Amount,");
		buildSrc("        source_currency =>AnyAlias.Currency,");
		buildSrc("     target_currency =>$parameters.P_DisplayCurrency,");
		buildSrc("      exchange_rate_type=> $parameters.P_ExchangeRateType,");
		buildSrc("        exchange_rate_date=>");
		buildSrc("          $parameters.P_ExchangeRateDate");
		buildSrc("// other comment at line start");
		buildSrc("// other comment containing => but more than one word before it");
		buildSrc("// +not%allowed!as?param;name => pseudo_value");
		buildSrc("// incomplete =>");
		buildSrc("//    round   =>'X',");
		buildSrc("// error_handling => 'SET_TO_NULL'");
		buildSrc("    ) as AmountInDisplayCurrency");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      currency_conversion(amount => AnyAlias.Amount,");
		buildExp("                          source_currency => AnyAlias.Currency,");
		buildExp("                          target_currency => $parameters.P_DisplayCurrency,");
		buildExp("                          exchange_rate_type => $parameters.P_ExchangeRateType,");
		buildExp("                          exchange_rate_date => $parameters.P_ExchangeRateDate");
		buildExp("// other comment at line start");
		buildExp("// other comment containing => but more than one word before it");
		buildExp("// +not%allowed!as?param;name => pseudo_value");
		buildExp("//                          incomplete =>");
		buildExp("//                          round => 'X',");
		buildExp("//                          error_handling => 'SET_TO_NULL'");
		buildExp("                         ) as AmountInDisplayCurrency");
		buildExp("}");

		testRule();
	}

}
