package com.sap.adt.abapcleaner.rules.prettyprinter;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class CamelCaseNameTest extends RuleTestBase {
	private CamelCaseNameRule rule;
	
	CamelCaseNameTest() {
		super(RuleID.CAMEL_CASE_NAME);
		rule = (CamelCaseNameRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configProcessViewNames.setValue(true);
		rule.configProcessFieldNames.setValue(true);
		rule.configProcessComments.setValue(true);
		rule.configMinLengthOfSureMatch.setValue(11);
		rule.configRequireApprovalForSureMatch.setValue(false);
		rule.configContextAllKnownAction.setEnumValue(CamelCaseContextAllKnownAction.CHANGE_ALL_KNOWN);
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_NONE);
		rule.configDeviationAction.setEnumValue(CamelCaseDeviationAction.CHANGE_IF_APPROVED);
		rule.configCustomViewNamesFile.setValue("CustomViewNames.txt");
		rule.configCustomFieldNamesFile.setValue("CustomFieldNames.txt");
	}

	@Test
	void testDependsOnExternalFiles() {
		assertTrue(rule.dependsOnExternalFiles());
	}
	
	@Test
	void testChangeViewAndFieldNames() {
		buildSrc("    DATA lt_company TYPE STANDARD TABLE OF i_companycode.");
		buildSrc("    DATA lv_fiscal_year_variant TYPE i_companycode-fiscalyearvariant.");
		buildSrc("");
		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        companycodename              = 'Company Name'");
		buildSrc("                                        cityname                     = 'Berlin'");
		buildSrc("                                        chartofaccounts              = 'ABCD'");
		buildSrc("                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        nontaxabletransactiontaxcode = 'AB'");
		buildSrc("                                        taxrptgdateisactive          = abap_true");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");

		buildExp("    DATA lt_company TYPE STANDARD TABLE OF I_CompanyCode.");
		buildExp("    DATA lv_fiscal_year_variant TYPE I_CompanyCode-FiscalYearVariant.");
		buildExp("");
		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        CompanyCodeName              = 'Company Name'");
		buildExp("                                        CityName                     = 'Berlin'");
		buildExp("                                        ChartOfAccounts              = 'ABCD'");
		buildExp("                                        FiscalYearVariant            = 'K4'");
		buildExp("                                        NonTaxableTransactionTaxCode = 'AB'");
		buildExp("                                        TaxRptgDateIsActive          = abap_true");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotChangeViewNames() {
		rule.configProcessViewNames.setValue(false);

		buildSrc("    DATA lt_company TYPE STANDARD TABLE OF i_companycode.");
		buildSrc("    DATA lv_fiscal_year_variant TYPE i_companycode-fiscalyearvariant.");
		buildSrc("");
		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");

		buildExp("    DATA lt_company TYPE STANDARD TABLE OF i_companycode.");
		buildExp("    DATA lv_fiscal_year_variant TYPE i_companycode-FiscalYearVariant.");
		buildExp("");
		buildExp("    lt_company = VALUE I_COMPANYCODE( ( CompanyCode                  = '1234'");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotChangeFieldNames() {
		rule.configProcessFieldNames.setValue(false);

		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        companycodename              = 'Company Name'");
		buildSrc("                                        cityname                     = 'Berlin'");
		buildSrc("                                        chartofaccounts              = 'ABCD'");
		buildSrc("                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        nontaxabletransactiontaxcode = 'AB'");
		buildSrc("                                        taxrptgdateisactive          = abap_true");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");
		buildSrc("");
		buildSrc("    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).");
		buildSrc("    LOOP AT lt_company ASSIGNING FIELD-SYMBOL(<ls_company>) WHERE cityname = 'Berlin'.");
		buildSrc("      any_method( iv_cash_discount_is_net = <ls_company>-cashdiscountbaseamtisnetamt");
		buildSrc("                  iv_fiscal_year_variant  = <ls_company>-fiscalyearvariant ).");
		buildSrc("    ENDLOOP.");

		buildExp("    lt_company = VALUE I_CompanyCode( ( companycode                  = '1234'");
		buildExp("                                        companycodename              = 'Company Name'");
		buildExp("                                        cityname                     = 'Berlin'");
		buildExp("                                        chartofaccounts              = 'ABCD'");
		buildExp("                                        FiscalyeaRVariant            = 'K4'");
		buildExp("                                        nontaxabletransactiontaxcode = 'AB'");
		buildExp("                                        taxrptgdateisactive          = abap_true");
		buildExp("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");
		buildExp("");
		buildExp("    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).");
		buildExp("    LOOP AT lt_company ASSIGNING FIELD-SYMBOL(<ls_company>) WHERE cityname = 'Berlin'.");
		buildExp("      any_method( iv_cash_discount_is_net = <ls_company>-cashdiscountbaseamtisnetamt");
		buildExp("                  iv_fiscal_year_variant  = <ls_company>-fiscalyearvariant ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSureMatchMinLength() {
		rule.configMinLengthOfSureMatch.setValue(16);

		// ensure that only the DATA declaration is changed, because the fields in the other commands are less than 16 chars long
		buildSrc("    DATA lv_fiscal_year_variant TYPE i_companycode-fiscalyearvariant.");
		buildSrc("");
		buildSrc("    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).");
		buildSrc("");
		buildSrc("    lt_company = VALUE #( ( companycode     = '1234'");
		buildSrc("                            currency        = 'EUR'");
		buildSrc("                            language        = 'E'");
		buildSrc("                            ChartofACcounts = 'ABCD' ) ).");

		buildExp("    DATA lv_fiscal_year_variant TYPE i_companycode-FiscalYearVariant.");
		buildExp("");
		buildExp("    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>).");
		buildExp("");
		buildExp("    lt_company = VALUE #( ( companycode     = '1234'");
		buildExp("                            currency        = 'EUR'");
		buildExp("                            language        = 'E'");
		buildExp("                            ChartofACcounts = 'ABCD' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRequireApprovalForSureMatch() {
		rule.configRequireApprovalForSureMatch.setValue(true);

		// ensure that only the first statement is changed, because the second one does not contain an approved field name
		buildSrc("    ls_vdm_style = VALUE #( refid        = '1'");
		buildSrc("                            owner        = 'anyone'");
		buildSrc("                            cityname     = 'Hamburg'");
		buildSrc("                            lognumber    = '12345'");
		buildSrc("                            dataprovider = 'XYZ' ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");

		buildExp("    ls_vdm_style = VALUE #( RefID        = '1'");
		buildExp("                            Owner        = 'anyone'");
		buildExp("                            CityName     = 'Hamburg'");
		buildExp("                            LogNumber    = '12345'");
		buildExp("                            DataProvider = 'XYZ' ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY website");
		buildExp("                        webaddress");
		buildExp("                        visitedPlacename.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllKnownChangeAll() {
		buildSrc("    ls_vdm_style = VALUE #( refid        = '1'");
		buildSrc("                            owner        = 'anyone'");
		buildSrc("                            cityname     = 'Hamburg'");
		buildSrc("                            lognumber    = '12345'");
		buildSrc("                            dataprovider = 'XYZ' ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");

		buildExp("    ls_vdm_style = VALUE #( RefID        = '1'");
		buildExp("                            Owner        = 'anyone'");
		buildExp("                            CityName     = 'Hamburg'");
		buildExp("                            LogNumber    = '12345'");
		buildExp("                            DataProvider = 'XYZ' ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY Website");
		buildExp("                        WebAddress");
		buildExp("                        visitedPlacename.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllKnownChangeApprovedOnly() {
		rule.configContextAllKnownAction.setEnumValue(CamelCaseContextAllKnownAction.CHANGE_ALL_APPROVED);

		buildSrc("    ls_vdm_style = VALUE #( refid        = '1'");
		buildSrc("                            owner        = 'anyone'");
		buildSrc("                            cityname     = 'Hamburg'");
		buildSrc("                            lognumber    = '12345'");
		buildSrc("                            dataprovider = 'XYZ' ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");

		buildExp("    ls_vdm_style = VALUE #( refid        = '1'");
		buildExp("                            owner        = 'anyone'");
		buildExp("                            CityName     = 'Hamburg'");
		buildExp("                            LogNumber    = '12345'");
		buildExp("                            DataProvider = 'XYZ' ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY website");
		buildExp("                        webaddress");
		buildExp("                        visitedPlacename.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllKnownChangeSureOnly() {
		rule.configContextAllKnownAction.setEnumValue(CamelCaseContextAllKnownAction.CHANGE_SURE_ONLY);

		buildSrc("    ls_vdm_style = VALUE #( refid                = '1'");
		buildSrc("                            cityname             = 'Hamburg'");
		buildSrc("                            lognumber            = '12345'");
		buildSrc("                            dataprovider         = 'XYZ'");
		buildSrc("                            yearmonthfourthmonth = '2024004' ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");

		buildExp("    ls_vdm_style = VALUE #( refid                = '1'");
		buildExp("                            cityname             = 'Hamburg'");
		buildExp("                            lognumber            = '12345'");
		buildExp("                            DataProvider         = 'XYZ'");
		buildExp("                            YearMonthFourthMonth = '2024004' ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY website");
		buildExp("                        webaddress");
		buildExp("                        visitedPlacename.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllKnownChangeApprovedSureOnly() {
		rule.configRequireApprovalForSureMatch.setValue(true);
		rule.configContextAllKnownAction.setEnumValue(CamelCaseContextAllKnownAction.CHANGE_SURE_ONLY);

		// expect 'yearmonthfourthmonth' to be unchanged, because it is not approved
		buildSrc("    ls_vdm_style = VALUE #( refid                = '1'");
		buildSrc("                            cityname             = 'Hamburg'");
		buildSrc("                            lognumber            = '12345'");
		buildSrc("                            dataprovider         = 'XYZ'");
		buildSrc("                            yearmonthfourthmonth = '2024004' ).");

		buildExp("    ls_vdm_style = VALUE #( refid                = '1'");
		buildExp("                            cityname             = 'Hamburg'");
		buildExp("                            lognumber            = '12345'");
		buildExp("                            DataProvider         = 'XYZ'");
		buildExp("                            yearmonthfourthmonth = '2024004' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownChangeNone() {
		buildSrc("    ls_unknown_struc = VALUE #( refid              = '1'");
		buildSrc("                                currency           = 'EUR'");
		buildSrc("                                unknown_field_name = 42");
		buildSrc("                                fiscalyearvariant  = 'K4' ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownChangeSureOnly() {
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_SURE_ONLY);

		buildSrc("    ls_unknown_struc = VALUE #( refid                = '1'");
		buildSrc("                                currency             = 'EUR'");
		buildSrc("                                unknown_field_name   = 42");
		buildSrc("                                fiscalyearvariant    = 'K4'");
		buildSrc("                                yearmonthfourthmonth = '2024004' ).");

		buildExp("    ls_unknown_struc = VALUE #( refid                = '1'");
		buildExp("                                currency             = 'EUR'");
		buildExp("                                unknown_field_name   = 42");
		buildExp("                                FiscalYearVariant    = 'K4'");
		buildExp("                                YearMonthFourthMonth = '2024004' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownChangeApprovedSureOnly() {
		rule.configRequireApprovalForSureMatch.setValue(true);
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_SURE_ONLY);

		// expect 'yearmonthfourthmonth' to be unchanged, because it is not approved
		buildSrc("    ls_unknown_struc = VALUE #( refid                = '1'");
		buildSrc("                                currency             = 'EUR'");
		buildSrc("                                unknown_field_name   = 42");
		buildSrc("                                fiscalyearvariant    = 'K4'");
		buildSrc("                                yearmonthfourthmonth = '2024004' ).");

		buildExp("    ls_unknown_struc = VALUE #( refid                = '1'");
		buildExp("                                currency             = 'EUR'");
		buildExp("                                unknown_field_name   = 42");
		buildExp("                                FiscalYearVariant    = 'K4'");
		buildExp("                                yearmonthfourthmonth = '2024004' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownChangeApproved() {
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_ALL_APPROVED);

		buildSrc("    ls_unknown_struc = VALUE #( refid              = '1'");
		buildSrc("                                currency           = 'EUR'");
		buildSrc("                                unknown_field_name = 42");
		buildSrc("                                fiscalyearvariant  = 'K4' ).");

		buildExp("    ls_unknown_struc = VALUE #( refid              = '1'");
		buildExp("                                Currency           = 'EUR'");
		buildExp("                                unknown_field_name = 42");
		buildExp("                                FiscalYearVariant  = 'K4' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownChangeAllKnown() {
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_ALL_KNOWN);

		buildSrc("    ls_unknown_struc = VALUE #( refid              = '1'");
		buildSrc("                                currency           = 'EUR'");
		buildSrc("                                unknown_field_name = 42");
		buildSrc("                                fiscalyearvariant  = 'K4' ).");

		buildExp("    ls_unknown_struc = VALUE #( RefID              = '1'");
		buildExp("                                Currency           = 'EUR'");
		buildExp("                                unknown_field_name = 42");
		buildExp("                                FiscalYearVariant  = 'K4' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCorrectIfKnown() {
		rule.configDeviationAction.setEnumValue(CamelCaseDeviationAction.CHANGE_IF_KNOWN);

		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");
		buildSrc("");
		buildSrc("    lt_company = VALUE #( ( companycode     = '1234'");
		buildSrc("                            currency        = 'EUR'");
		buildSrc("                            language        = 'E'");
		buildSrc("                            ChartofACcounts = 'ABCD' ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        FiscalYearVariant            = 'K4'");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY Website");
		buildExp("                        WebAddress");
		buildExp("                        VisitedPlaceName.");
		buildExp("");
		buildExp("    lt_company = VALUE #( ( CompanyCode     = '1234'");
		buildExp("                            Currency        = 'EUR'");
		buildExp("                            Language        = 'E'");
		buildExp("                            ChartOfAccounts = 'ABCD' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCorrectIfApproved() {
		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");
		buildSrc("");
		buildSrc("    lt_company = VALUE #( ( companycode     = '1234'");
		buildSrc("                            currency        = 'EUR'");
		buildSrc("                            language        = 'E'");
		buildSrc("                            ChartofACcounts = 'ABCD' ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        FiscalYearVariant            = 'K4'");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY Website");
		buildExp("                        WebAddress");
		buildExp("                        visitedPlacename.");
		buildExp("");
		buildExp("    lt_company = VALUE #( ( CompanyCode     = '1234'");
		buildExp("                            Currency        = 'EUR'");
		buildExp("                            Language        = 'E'");
		buildExp("                            ChartOfAccounts = 'ABCD' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCorrectNever() {
		rule.configDeviationAction.setEnumValue(CamelCaseDeviationAction.NONE);

		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");
		buildSrc("");
		buildSrc("    SORT lt_web_info BY website");
		buildSrc("                        webaddress");
		buildSrc("                        visitedPlacename.");
		buildSrc("");
		buildSrc("    lt_company = VALUE #( ( companycode     = '1234'");
		buildSrc("                            currency        = 'EUR'");
		buildSrc("                            language        = 'E'");
		buildSrc("                            ChartofACcounts = 'ABCD' ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        FiscalyeaRVariant            = 'K4'");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");
		buildExp("");
		buildExp("    SORT lt_web_info BY Website");
		buildExp("                        WebAddress");
		buildExp("                        visitedPlacename.");
		buildExp("");
		buildExp("    lt_company = VALUE #( ( CompanyCode     = '1234'");
		buildExp("                            Currency        = 'EUR'");
		buildExp("                            Language        = 'E'");
		buildExp("                            ChartofACcounts = 'ABCD' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFromWhereWithParam() {
		buildSrc("    SELECT applicationlog       AS log_number,");
		buildSrc("           applicationloghandle AS log_handle,");
		buildSrc("           companycode          AS bukrs");
		buildSrc("      FROM c_slsordrevnrecgnrealtmeiss( p_tofiscalyearperiod = @lv_fiscal_period )");
		buildSrc("      INTO TABLE @mt_worklist");
		buildSrc("      WHERE companycode           IN @lt_company_code");
		buildSrc("        AND referencedocument     IN @lt_ref_doc");
		buildSrc("        AND referencedocumenttype IN @lt_ref_doc_type");
		buildSrc("        AND salesdocument         IN @lt_sales_doc.");

		buildExp("    SELECT ApplicationLog       AS log_number,");
		buildExp("           ApplicationLogHandle AS log_handle,");
		buildExp("           CompanyCode          AS bukrs");
		buildExp("      FROM C_SlsOrdRevnRecgnRealTmeIss( P_ToFiscalYearPeriod = @lv_fiscal_period )");
		buildExp("      INTO TABLE @mt_worklist");
		buildExp("      WHERE CompanyCode           IN @lt_company_code");
		buildExp("        AND ReferenceDocument     IN @lt_ref_doc");
		buildExp("        AND ReferenceDocumentType IN @lt_ref_doc_type");
		buildExp("        AND SalesDocument         IN @lt_sales_doc.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFromWhereWithParamNoPrefix() {
		buildSrc("    SELECT applicationlog AS log_number,");
		buildSrc("           companycode    AS bukrs");
		buildSrc("      FROM c_slsordrevnrecgnrealtmeiss( tofiscalyearperiod = @lv_fiscal_period )");
		buildSrc("      INTO TABLE @mt_worklist");
		buildSrc("      WHERE companycode           IN @lt_company_code");
		buildSrc("        AND salesdocument         IN @lt_sales_doc.");

		buildExp("    SELECT ApplicationLog AS log_number,");
		buildExp("           CompanyCode    AS bukrs");
		buildExp("      FROM C_SlsOrdRevnRecgnRealTmeIss( ToFiscalYearPeriod = @lv_fiscal_period )");
		buildExp("      INTO TABLE @mt_worklist");
		buildExp("      WHERE CompanyCode           IN @lt_company_code");
		buildExp("        AND SalesDocument         IN @lt_sales_doc.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComponentsInActualParams() {
		buildSrc("    lo_instance=>any_method( iv_service_doc_id      = ls_sdoc-servicedocument");
		buildSrc("                             iv_service_doc_item_id = ls_sdoc-servicedocumentitem");
		buildSrc("                             iv_service_doc_type    = ls_sdoc-servicedocumenttype ).");

		buildExp("    lo_instance=>any_method( iv_service_doc_id      = ls_sdoc-ServiceDocument");
		buildExp("                             iv_service_doc_item_id = ls_sdoc-ServiceDocumentItem");
		buildExp("                             iv_service_doc_type    = ls_sdoc-ServiceDocumentType ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStrucsWithApprovedOrUnknownComps() {
		// expect that the (approved) fields of 'es_approved' are changed, while none of the fields of 'en_unknown'
		// is changed, because it contains the unknown component 'field_name'; this must be detected regardless of the 
		// casing of 'ES_unKNOWN'
		buildSrc("    es_approved-companycode     = '1234'.");
		buildSrc("    es_approved-cityname        = 'Berlin'.");
		buildSrc("    es_unknown-refid              = '1'.");
		buildSrc("    es_unknown-currency           = 'EUR'.");
		buildSrc("    es_unknown-fiscalyearvariant  = 'K4'.");
		buildSrc("");
		buildSrc("    IF a = 1.");
		buildSrc("      ES_unKNOWN-field_name = 42.");
		buildSrc("      es_approved-companycodename = 'Company Name'.");
		buildSrc("    ENDIF.");

		buildExp("    es_approved-CompanyCode     = '1234'.");
		buildExp("    es_approved-CityName        = 'Berlin'.");
		buildExp("    es_unknown-refid              = '1'.");
		buildExp("    es_unknown-currency           = 'EUR'.");
		buildExp("    es_unknown-fiscalyearvariant  = 'K4'.");
		buildExp("");
		buildExp("    IF a = 1.");
		buildExp("      ES_unKNOWN-field_name = 42.");
		buildExp("      es_approved-CompanyCodeName = 'Company Name'.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStrucsWithPathToApprovedOrUnknownComps() {
		rule.configContextWithUnknownAction.setEnumValue(CamelCaseContextWithUnknownAction.CHANGE_ALL_APPROVED);

		buildSrc("    es_approved-obj->tab[ 1 ]-companycode     = '1234'.");
		buildSrc("    es_approved-obj->tab[ 2 ]-cityname        = 'Berlin'.");
		buildSrc("    es_unknown-obj->tab[ 3 ]-refid              = '1'.");
		buildSrc("    es_unknown-obj->tab[ 4 ]-currency           = 'EUR'.");
		buildSrc("    es_unknown-obj->tab[ 5 ]-fiscalyearvariant  = 'K4'.");
		buildSrc("");
		buildSrc("    IF a = 1.");
		buildSrc("      es_unknown-obj->tab[ 6 ]-unknown_field_name = 42.");
		buildSrc("      es_approved-obj->tab[ 7 ]-companycodename = 'Company Name'.");
		buildSrc("    ENDIF.");

		buildExp("    es_approved-obj->tab[ 1 ]-CompanyCode     = '1234'.");
		buildExp("    es_approved-obj->tab[ 2 ]-CityName        = 'Berlin'.");
		buildExp("    es_unknown-obj->tab[ 3 ]-refid              = '1'.");
		buildExp("    es_unknown-obj->tab[ 4 ]-Currency           = 'EUR'.");
		buildExp("    es_unknown-obj->tab[ 5 ]-FiscalYearVariant  = 'K4'.");
		buildExp("");
		buildExp("    IF a = 1.");
		buildExp("      es_unknown-obj->tab[ 6 ]-unknown_field_name = 42.");
		buildExp("      es_approved-obj->tab[ 7 ]-CompanyCodeName = 'Company Name'.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testReadTableKey() {
		buildSrc("    READ TABLE it_any_table-inner_table INTO DATA(ls_any_struc)");
		buildSrc("         WITH KEY keystruc-ledger             = is_key-ledger");
		buildSrc("                  keystruc-companycode        = is_key-companycode");
		buildSrc("                  keystruc-fiscalyear         = is_key-fiscalyear");
		buildSrc("                  keystruc-accountingdocument = is_key-accountingdocument");
		buildSrc("                  keystruc-ledgergllineitem   = is_key-ledgergllineitem.");

		buildExp("    READ TABLE it_any_table-inner_table INTO DATA(ls_any_struc)");
		buildExp("         WITH KEY keystruc-Ledger             = is_key-Ledger");
		buildExp("                  keystruc-CompanyCode        = is_key-CompanyCode");
		buildExp("                  keystruc-FiscalYear         = is_key-FiscalYear");
		buildExp("                  keystruc-AccountingDocument = is_key-AccountingDocument");
		buildExp("                  keystruc-LedgerGLLineItem   = is_key-LedgerGLLineItem.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testReadTableKeyComponents() {
		buildSrc("    READ TABLE it_any_table-inner_table INTO DATA(ls_any_struc) WITH KEY ledger");
		buildSrc("         COMPONENTS keystruc-ledger             = is_key-ledger");
		buildSrc("                    keystruc-companycode        = is_key-companycode.");

		buildExp("    READ TABLE it_any_table-inner_table INTO DATA(ls_any_struc) WITH KEY ledger");
		buildExp("         COMPONENTS keystruc-Ledger             = is_key-Ledger");
		buildExp("                    keystruc-CompanyCode        = is_key-CompanyCode.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithDefaultComponents() {
		buildSrc("    lt_company = VALUE I_COMPANYCODE( companycode     = '1234'");
		buildSrc("                                      companycodename = 'Company Name'");
		buildSrc("                                      ( cityname = 'Berlin' )");
		buildSrc("                                      ( cityname = 'Hamburg' ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( CompanyCode     = '1234'");
		buildExp("                                      CompanyCodeName = 'Company Name'");
		buildExp("                                      ( CityName = 'Berlin' )");
		buildExp("                                      ( CityName = 'Hamburg' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithJoinAndAliases() {
		buildSrc("  SELECT");
		buildSrc("    FROM i_companycode AS cpny");
		buildSrc("           INNER JOIN");
		buildSrc("             I_Country AS ctry ON cpny~country = ctry~country");
		buildSrc("    FIELDS cpny~companycode,");
		buildSrc("           cpny~companycodename,");
		buildSrc("           ctry~countryisocode,");
		buildSrc("           ctry~countrycurrency");
		buildSrc("    WHERE cpny~currency              = 'EUR'");
		buildSrc("      AND ctry~countrycurrency       = cpny~currency");
		buildSrc("      AND ctry~iseuropeanunionmember = @abap_true");
		buildSrc("    INTO TABLE @DATA(lt_any).");

		buildExp("  SELECT");
		buildExp("    FROM I_CompanyCode AS cpny");
		buildExp("           INNER JOIN");
		buildExp("             I_Country AS ctry ON cpny~Country = ctry~Country");
		buildExp("    FIELDS cpny~CompanyCode,");
		buildExp("           cpny~CompanyCodeName,");
		buildExp("           ctry~CountryISOCode,");
		buildExp("           ctry~CountryCurrency");
		buildExp("    WHERE cpny~Currency              = 'EUR'");
		buildExp("      AND ctry~CountryCurrency       = cpny~Currency");
		buildExp("      AND ctry~IsEuropeanUnionMember = @abap_true");
		buildExp("    INTO TABLE @DATA(lt_any).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithJoinAndAliasesOnlyViewName() {
		rule.configProcessFieldNames.setValue(false);

		buildSrc("  SELECT");
		buildSrc("    FROM i_companycode AS cpny");
		buildSrc("           INNER JOIN");
		buildSrc("             I_Country AS ctry ON cpny~country = ctry~country");
		buildSrc("    FIELDS cpny~companycode,");
		buildSrc("           ctry~countryisocode");
		buildSrc("    WHERE cpny~currency = 'EUR'");
		buildSrc("    INTO TABLE @DATA(lt_any).");

		buildExp("  SELECT");
		buildExp("    FROM I_CompanyCode AS cpny");
		buildExp("           INNER JOIN");
		buildExp("             I_Country AS ctry ON cpny~country = ctry~country");
		buildExp("    FIELDS cpny~companycode,");
		buildExp("           ctry~countryisocode");
		buildExp("    WHERE cpny~currency = 'EUR'");
		buildExp("    INTO TABLE @DATA(lt_any).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithJoinAndAliasesOnlyFieldNames() {
		rule.configProcessViewNames.setValue(false);

		buildSrc("  SELECT");
		buildSrc("    FROM i_companycode AS cpny");
		buildSrc("           INNER JOIN");
		buildSrc("             I_Country AS ctry ON cpny~country = ctry~country");
		buildSrc("    FIELDS cpny~companycode,");
		buildSrc("           ctry~countryisocode");
		buildSrc("    WHERE cpny~currency = 'EUR'");
		buildSrc("    INTO TABLE @DATA(lt_any).");

		buildExp("  SELECT");
		buildExp("    FROM i_companycode AS cpny");
		buildExp("           INNER JOIN");
		buildExp("             I_Country AS ctry ON cpny~Country = ctry~Country");
		buildExp("    FIELDS cpny~CompanyCode,");
		buildExp("           ctry~CountryISOCode");
		buildExp("    WHERE cpny~Currency = 'EUR'");
		buildExp("    INTO TABLE @DATA(lt_any).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCorrespondingMappingExcept() {
		buildSrc("    ls_target_struc = CORRESPONDING #( is_source_struc");
		buildSrc("                                       MAPPING accountingprinciple = acct_principle DISCARDING DUPLICATES");
		buildSrc("                                               companycode         = company_code");
		buildSrc("                                       EXCEPT  companycodename");
		buildSrc("                                               cityname ).");

		buildExp("    ls_target_struc = CORRESPONDING #( is_source_struc");
		buildExp("                                       MAPPING AccountingPrinciple = acct_principle DISCARDING DUPLICATES");
		buildExp("                                               CompanyCode         = company_code");
		buildExp("                                       EXCEPT  CompanyCodeName");
		buildExp("                                               CityName ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFilterWhere() {
		buildSrc("    it_any_table = FILTER #( it_other_table USING KEY ledger");
		buildSrc("                             WHERE ledger              = '0L'");
		buildSrc("                               AND companycode         = '0123'");
		buildSrc("                               AND fiscalyear         >= '2023'");
		buildSrc("                               AND accountingdocument  = VALUE #( ) ).");

		buildExp("    it_any_table = FILTER #( it_other_table USING KEY ledger");
		buildExp("                             WHERE Ledger              = '0L'");
		buildExp("                               AND CompanyCode         = '0123'");
		buildExp("                               AND FiscalYear         >= '2023'");
		buildExp("                               AND AccountingDocument  = VALUE #( ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAppendToSortedBy() {
		buildSrc("    APPEND ls_struc TO lt_target SORTED BY companyCode.");

		buildExp("    APPEND ls_struc TO lt_target SORTED BY CompanyCode.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentedOutComponentsChanged() {
		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        companycodename              = 'Company Name'");
		buildSrc("                                        cityname                     = 'Berlin'");
		buildSrc("*                                        chartofaccounts              = 'ABCD'");
		buildSrc("*                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        nontaxabletransactiontaxcode = 'AB'");
		buildSrc("                                        taxrptgdateisactive          = abap_true");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        CompanyCodeName              = 'Company Name'");
		buildExp("                                        CityName                     = 'Berlin'");
		buildExp("*                                        ChartOfAccounts              = 'ABCD'");
		buildExp("*                                        FiscalYearVariant            = 'K4'");
		buildExp("                                        NonTaxableTransactionTaxCode = 'AB'");
		buildExp("                                        TaxRptgDateIsActive          = abap_true");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentedOutComponentsUnchanged() {
		rule.configProcessComments.setValue(false);

		buildSrc("    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'");
		buildSrc("                                        companycodename              = 'Company Name'");
		buildSrc("                                        cityname                     = 'Berlin'");
		buildSrc("*                                        chartofaccounts              = 'ABCD'");
		buildSrc("*                                        FiscalyeaRVariant            = 'K4'");
		buildSrc("                                        nontaxabletransactiontaxcode = 'AB'");
		buildSrc("                                        taxrptgdateisactive          = abap_true");
		buildSrc("                                        cashdiscountbaseamtisnetamt  = abap_false ) ).");

		buildExp("    lt_company = VALUE I_CompanyCode( ( CompanyCode                  = '1234'");
		buildExp("                                        CompanyCodeName              = 'Company Name'");
		buildExp("                                        CityName                     = 'Berlin'");
		buildExp("*                                        chartofaccounts              = 'ABCD'");
		buildExp("*                                        FiscalyeaRVariant            = 'K4'");
		buildExp("                                        NonTaxableTransactionTaxCode = 'AB'");
		buildExp("                                        TaxRptgDateIsActive          = abap_true");
		buildExp("                                        CashDiscountBaseAmtIsNetAmt  = abap_false ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
