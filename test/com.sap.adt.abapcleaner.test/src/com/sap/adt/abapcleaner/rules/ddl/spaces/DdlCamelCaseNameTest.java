package com.sap.adt.abapcleaner.rules.ddl.spaces;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlCamelCaseNameTest extends RuleTestBase {
	private DdlCamelCaseNameRule rule;
	
	DdlCamelCaseNameTest() {
		super(RuleID.DDL_CAMEL_CASE_NAME);
		rule = (DdlCamelCaseNameRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configFixEntityName.setValue(true);
		rule.configFixAliases.setValue(true);
		rule.configFixFieldNames.setValue(true);
		rule.configFixRefsInAnnotations.setValue(true);
		
		rule.configOnlyApprovedNames.setValue(true);
		rule.configRequireAllFieldsKnown.setValue(true);
	}

	@Test
	void testDependsOnExternalFiles() {
		assertTrue(rule.dependsOnExternalFiles());
	}

	@Test
	void testFixEntityName() {
		rule.configOnlyApprovedNames.setValue(false);

		buildSrc("// comment");
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("// comment");
		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'I_ControllingAreaStdVH',");
		buildExp("                                                    element: 'ControllingArea' }}]");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotFixEntityName() {
		rule.configFixEntityName.setValue(false);
		rule.configOnlyApprovedNames.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("define view entity i_Companycode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildExp("                                                    element: 'ControllingArea' }}]");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testFixAliases() {
		// expect _i_countryregion to be fixed as a known entity name, 
		// but _z_countryregion to be unchanged, because "Z" is not a known entity prefix
		
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("  association[0..1] to I_Country as _country");
		buildSrc("    on $projection.Country = _country.Country");
		buildSrc("");
		buildSrc("  association[0..1] to I_ChartOfAccounts as _i_chartofaccounts");
		buildSrc("    on $projection.ChartOfAccounts = _i_chartofaccounts.ChartOfAccounts");
		buildSrc("");
		buildSrc("  association[0..1] to I_CountryRegion as _i_countryregion");
		buildSrc("    on $projection.Country = _i_countryregion.Country");
		buildSrc("");
		buildSrc("  association[0..1] to I_CountryRegion as _z_countryregion");
		buildSrc("    on $projection.Country = _z_countryregion.Country");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_country'");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_i_CHARTOFACCOUNTS'");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("");
		buildSrc("      _country,");
		buildSrc("      _i_chartofaccounts");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("  association[0..1] to I_Country as _Country");
		buildExp("    on $projection.Country = _country.Country");
		buildExp("");
		buildExp("  association[0..1] to I_ChartOfAccounts as _I_ChartOfAccounts");
		buildExp("    on $projection.ChartOfAccounts = _i_chartofaccounts.ChartOfAccounts");
		buildExp("");
		buildExp("  association[0..1] to I_CountryRegion as _I_CountryRegion");
		buildExp("    on $projection.Country = _i_countryregion.Country");
		buildExp("");
		buildExp("  association[0..1] to I_CountryRegion as _z_countryregion");
		buildExp("    on $projection.Country = _z_countryregion.Country");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_Country'");
		buildExp("      d.land1    as Country,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_I_ChartOfAccounts'");
		buildExp("      d.ktopl    as ChartOfAccounts,");
		buildExp("");
		buildExp("      _country,");
		buildExp("      _i_chartofaccounts");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotFixAliases() {
		rule.configFixAliases.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("  association[0..1] to I_Country as _country");
		buildSrc("    on $projection.Country = _country.Country");
		buildSrc("");
		buildSrc("  association[0..1] to I_ChartOfAccounts as _chartofaccounts");
		buildSrc("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_country'");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("");
		buildSrc("      _country,");
		buildSrc("      _chartofaccounts");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("  association[0..1] to I_Country as _country");
		buildExp("    on $projection.Country = _country.Country");
		buildExp("");
		buildExp("  association[0..1] to I_ChartOfAccounts as _chartofaccounts");
		buildExp("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_country'");
		buildExp("      d.land1    as Country,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'");
		buildExp("      d.ktopl    as ChartOfAccounts,");
		buildExp("");
		buildExp("      _country,");
		buildExp("      _chartofaccounts");
		buildExp("}");

		testRule();
	}

	@Test
	void testFixFieldNames() {
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      // comment");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("      d.ort01    as CityName,");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("      d.periv    as FiscalyeaRVariant,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea,");
		buildSrc("");
		buildSrc("      d.mwska    as nontaxabletransactiontaxcode,");
		buildSrc("      d.xvatdate as TAXRPTGDATEISACTIVE,");
		buildSrc("      d.xskfn    as cashdiscountbaseamtisnetamt");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['CompanyCodeName']");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      // comment");
		buildExp("      d.butxt    as CompanyCodeName,");
		buildExp("      d.ort01    as CityName,");
		buildExp("      d.land1    as Country,");
		buildExp("      d.ktopl    as ChartOfAccounts,");
		buildExp("      d.periv    as FiscalYearVariant,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildExp("                                                    element: 'ControllingArea' }}]");
		buildExp("      d.kokrs    as ControllingArea,");
		buildExp("");
		buildExp("      d.mwska    as NonTaxableTransactionTaxCode,");
		buildExp("      d.xvatdate as TaxRptgDateIsActive,");
		buildExp("      d.xskfn    as CashDiscountBaseAmtIsNetAmt");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotFixFieldNames() {
		rule.configFixFieldNames.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("      d.ort01    as CityName,");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("      d.periv    as FiscalyeaRVariant,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea,");
		buildSrc("");
		buildSrc("      d.mwska    as nontaxabletransactiontaxcode,");
		buildSrc("      d.xvatdate as TAXRPTGDATEISACTIVE,");
		buildSrc("      d.xskfn    as cashdiscountbaseamtisnetamt");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['companycodename']");
		buildExp("  key d.bukrs    as companycode,");
		buildExp("");
		buildExp("      d.butxt    as companyCodename,");
		buildExp("      d.ort01    as CityName,");
		buildExp("      d.land1    as COUNTRY,");
		buildExp("      d.ktopl    as chArtOfAccOUnts,");
		buildExp("      d.periv    as FiscalyeaRVariant,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildExp("                                                    element: 'CONTROLLINGAREA' }}]");
		buildExp("      d.kokrs    as controllingarea,");
		buildExp("");
		buildExp("      d.mwska    as nontaxabletransactiontaxcode,");
		buildExp("      d.xvatdate as TAXRPTGDATEISACTIVE,");
		buildExp("      d.xskfn    as cashdiscountbaseamtisnetamt");
		buildExp("}");

		testRule();
	}

	@Test
	void testFixAnnotationRefs() {
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("  association[0..1] to I_Country as _country");
		buildSrc("    on $projection.Country = _country.Country");
		buildSrc("");
		buildSrc("  association[0..1] to I_ChartOfAccounts as _chartofaccounts");
		buildSrc("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_country'");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("");
		buildSrc("      @Consumption: { derivation.resultElement: 'nontaxabletransactiontaxcode',");
		buildSrc("                      valueHelpDefinition: [ { entity: { name: 'i_controllingareastdvh', element: 'CONTROLLINGAREA' } } ] }");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("  association[0..1] to I_Country as _Country");
		buildExp("    on $projection.Country = _country.Country");
		buildExp("");
		buildExp("  association[0..1] to I_ChartOfAccounts as _ChartOfAccounts");
		buildExp("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['CompanyCodeName']");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      d.butxt    as CompanyCodeName,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_Country'");
		buildExp("      d.land1    as Country,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_ChartOfAccounts'");
		buildExp("      d.ktopl    as ChartOfAccounts,");
		buildExp("");
		buildExp("      @Consumption: { derivation.resultElement: 'NonTaxableTransactionTaxCode',");
		buildExp("                      valueHelpDefinition: [ { entity: { name: 'i_controllingareastdvh', element: 'ControllingArea' } } ] }");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotFixAnnotationRefs() {
		rule.configFixRefsInAnnotations.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("  association[0..1] to I_Country as _country");
		buildSrc("    on $projection.Country = _country.Country");
		buildSrc("");
		buildSrc("  association[0..1] to I_ChartOfAccounts as _chartofaccounts");
		buildSrc("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_country'");
		buildSrc("      d.land1    as COUNTRY,");
		buildSrc("");
		buildSrc("      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'");
		buildSrc("      d.ktopl    as chArtOfAccOUnts,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("  association[0..1] to I_Country as _Country");
		buildExp("    on $projection.Country = _country.Country");
		buildExp("");
		buildExp("  association[0..1] to I_ChartOfAccounts as _ChartOfAccounts");
		buildExp("    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['companycodename']");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      d.butxt    as CompanyCodeName,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_country'");
		buildExp("      d.land1    as Country,");
		buildExp("");
		buildExp("      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'");
		buildExp("      d.ktopl    as ChartOfAccounts,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildExp("                                                    element: 'CONTROLLINGAREA' }}]");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testOnlyUseApprovedNames() {
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildExp("                                                    element: 'ControllingArea' }}]");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testFixNonApprovedNamesToo() {
		rule.configOnlyApprovedNames.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',");
		buildSrc("                                                    element: 'CONTROLLINGAREA' }}]");
		buildSrc("      d.kokrs    as controllingarea");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [{entity: { name:    'I_ControllingAreaStdVH',");
		buildExp("                                                    element: 'ControllingArea' }}]");
		buildExp("      d.kokrs    as ControllingArea");
		buildExp("}");

		testRule();
	}

	@Test
	void testRequiredAllFieldsKnown() {
		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      d.AnyUnknownFieldName,");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("      d.ort01    as CityName,");
		buildSrc("      1          as otherunknownfieldname");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['companycodename']");
		buildExp("  key d.bukrs    as companycode,");
		buildExp("");
		buildExp("      d.AnyUnknownFieldName,");
		buildExp("      d.butxt    as companyCodename,");
		buildExp("      d.ort01    as CityName,");
		buildExp("      1          as otherunknownfieldname");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotRequireAllFieldsKnown() {
		rule.configRequireAllFieldsKnown.setValue(false);

		buildSrc("define view entity i_Companycode");
		buildSrc("  as select from t001 as d");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @ObjectModel.text.element: ['companycodename']");
		buildSrc("  key d.bukrs    as companycode,");
		buildSrc("");
		buildSrc("      d.butxt    as companyCodename,");
		buildSrc("      d.ort01    as CityName,");
		buildSrc("");
		buildSrc("      1          as unknownfieldname");
		buildSrc("}");

		buildExp("define view entity I_CompanyCode");
		buildExp("  as select from t001 as d");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['CompanyCodeName']");
		buildExp("  key d.bukrs    as CompanyCode,");
		buildExp("");
		buildExp("      d.butxt    as CompanyCodeName,");
		buildExp("      d.ort01    as CityName,");
		buildExp("");
		buildExp("      1          as unknownfieldname");
		buildExp("}");

		testRule();
	}
}
