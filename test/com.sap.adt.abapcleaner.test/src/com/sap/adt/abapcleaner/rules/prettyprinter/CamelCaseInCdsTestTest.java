package com.sap.adt.abapcleaner.rules.prettyprinter;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class CamelCaseInCdsTestTest extends RuleTestBase {
	private CamelCaseInCdsTestRule rule;
	
	CamelCaseInCdsTestTest() {
		super(RuleID.CAMEL_CASE_IN_CDS_TEST);
		rule = (CamelCaseInCdsTestRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configProcessClassName.setValue(true);
		rule.configProcessVariableNames.setValue(true);
		rule.configProcessLiterals.setValue(true);
		rule.configProcessComments.setValue(true);
		rule.configAddPseudoCommentNoWhere.setValue(true);
	}

	@Test
	void testDependsOnExternalFiles() {
		assertTrue(rule.dependsOnExternalFiles());
	}

	@Test
	void testProcessClassName() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotProcessClassName() {
		rule.configProcessClassName.setValue(false);

		buildSrc("\"!some other ABAP Doc");
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!some other ABAP Doc");
		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testNoTestingAnnotation() {
		// ensure that nothing is changed if the class is not properly annotated with @testing

		buildSrc("\"! testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();
		
		testRule();
	}

	@Test
	void testUnknownViewName() {
		buildSrc("\"!@testing C_UNKNOWNVIEWNAME");
		buildSrc("CLASS ltc_c_unknownviewname DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    DATA td_i_otherunknownviewname    TYPE STANDARD TABLE OF i_otherunknownviewname WITH EMPTY KEY.");
		buildSrc("    DATA longprefix_i_businesspartner TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA tdicustomer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                  TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_unknownviewname IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_UNKNOWNVIEWNAME");
		buildExp("CLASS ltc_c_unknownviewname DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildExp("");
		buildExp("    DATA td_i_otherunknownviewname    TYPE STANDARD TABLE OF i_otherunknownviewname WITH EMPTY KEY.");
		buildExp("    DATA longprefix_i_businesspartner TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA tdicustomer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                  TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_unknownviewname IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPerfOblgnWithToCurPerdAmt' ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testOtherClassImplementation() {
		// ensure that in the CLASS ... IMPLEMENTATION of a different class, nothing is changed
		
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperd TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_i_businesspartner          TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                 TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                   TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_other_class IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildExp("");
		buildExp("    DATA td_I_RAPerfOblgnWithToCurPerd TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_I_BusinessPartner          TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_I_Customer                 TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                   TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_other_class IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testProcessVariableNames() {
		buildSrc("\" any comment");
		buildSrc("\"! @ testing  C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_testdata.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for several entities");
		buildSrc("    \" Prepare test data for 'i_raperfoblgnwithtocurperdamt'");
		buildSrc("    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #( ( Customer = 'Any' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\" any comment");
		buildExp("\"! @ testing  C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA td_I_RAPerfOblgnWithToCurPerdA TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_I_Customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_testdata.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    \" Prepare test data for several entities");
		buildExp("    \" Prepare test data for 'I_RAPerfOblgnWithToCurPerdAmt'");
		buildExp("    td_I_RAPerfOblgnWithToCurPerdA = VALUE #( ( ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_RAPerfOblgnWithToCurPerdA ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'I_BusinessPartner'");
		buildExp("    td_I_BusinessPartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_BusinessPartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'I_Customer'");
		buildExp("    td_I_Customer = VALUE #( ( Customer = 'Any' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_Customer ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotProcessVariableNames() {
		rule.configProcessVariableNames.setValue(false);

		buildSrc("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_testdata.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_raperfoblgnwithtocurperdamt'");
		buildSrc("    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #( ( Customer = 'Any' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_i_businesspartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_i_customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_testdata.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    \" Prepare test data for 'I_RAPerfOblgnWithToCurPerdAmt'");
		buildExp("    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'I_BusinessPartner'");
		buildExp("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'I_Customer'");
		buildExp("    td_i_customer = VALUE #( ( Customer = 'Any' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_customer ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotProcessComments() {
		rule.configProcessComments.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_testdata.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    DATA lv_unrelated_variable TYPE i.");
		buildSrc("    \" Prepare test data for 'i_raperfoblgnwithtocurperdamt'");
		buildSrc("    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_I_BusinessPartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #( ( Customer = 'Any' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA td_I_RAPerfOblgnWithToCurPerdA TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_I_Customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_testdata.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    DATA lv_unrelated_variable TYPE i.");
		buildExp("    \" Prepare test data for 'i_raperfoblgnwithtocurperdamt'");
		buildExp("    td_I_RAPerfOblgnWithToCurPerdA = VALUE #( ( ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_RAPerfOblgnWithToCurPerdA ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'i_businesspartner'");
		buildExp("    td_I_BusinessPartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_BusinessPartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'i_customer'");
		buildExp("    td_I_Customer = VALUE #( ( Customer = 'Any' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_I_Customer ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testProcessLiterals() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( )."); 
		buildSrc("    environment = cl_cds_test_environment=>create( 'C_RAPERFOBLGNWITHTOCURPERDAMT' )."); 
		buildSrc("    environment = cl_cds_test_environment=>create(");
		buildSrc("                    i_for_entity      = 'C_RAPERFOBLGNWITHTOCURPERDAMT'");
		buildSrc("                    i_dependency_list = VALUE #( type ='CDS_VIEW'");
		buildSrc("                                                 ( name = 'I_RAPERFOBLGNWITHTOCURPERDAMT' )");
		buildSrc("                                                 ( name = 'I_BUSINESSPARTNER' )");
		buildSrc("                                                 ( name = 'I_CUSTOMER' ) ) ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA td_I_RAPerfOblgnWithToCurPerdA TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_I_Customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( )."); 
		buildExp("    environment = cl_cds_test_environment=>create( 'C_RAPerfOblgnWithToCurPerdAmt' )."); 
		buildExp("    environment = cl_cds_test_environment=>create(");
		buildExp("                    i_for_entity      = 'C_RAPerfOblgnWithToCurPerdAmt'");
		buildExp("                    i_dependency_list = VALUE #( type ='CDS_VIEW'");
		buildExp("                                                 ( name = 'I_RAPerfOblgnWithToCurPerdAmt' )");
		buildExp("                                                 ( name = 'I_BusinessPartner' )");
		buildExp("                                                 ( name = 'I_Customer' ) ) ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotProcessLiterals() {
		rule.configProcessLiterals.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY.");
		buildSrc("    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY.");
		buildSrc("    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create(");
		buildSrc("                    i_for_entity      = 'C_RAPERFOBLGNWITHTOCURPERDAMT'");
		buildSrc("                    i_dependency_list = VALUE #( type ='CDS_VIEW'");
		buildSrc("                                                 ( name = 'I_RAPERFOBLGNWITHTOCURPERDAMT' )");
		buildSrc("                                                 ( name = 'I_BUSINESSPARTNER' )");
		buildSrc("                                                 ( name = 'I_CUSTOMER' ) ) ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA td_I_RAPerfOblgnWithToCurPerdA TYPE STANDARD TABLE OF I_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("    DATA td_I_BusinessPartner           TYPE STANDARD TABLE OF I_BusinessPartner WITH EMPTY KEY.");
		buildExp("    DATA td_I_Customer                  TYPE STANDARD TABLE OF I_Customer WITH EMPTY KEY.");
		buildExp("    DATA act_results                    TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create(");
		buildExp("                    i_for_entity      = 'C_RAPERFOBLGNWITHTOCURPERDAMT'");
		buildExp("                    i_dependency_list = VALUE #( type ='CDS_VIEW'");
		buildExp("                                                 ( name = 'I_RAPERFOBLGNWITHTOCURPERDAMT' )");
		buildExp("                                                 ( name = 'I_BUSINESSPARTNER' )");
		buildExp("                                                 ( name = 'I_CUSTOMER' ) ) ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void test() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA act_results TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA act_results TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPerfOblgnWithToCurPerdAmt' ).");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD aunit_for_cds_method.");
		buildExp("    prepare_testdata( ).");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results. \"#EC CI_NOWHERE");
		buildExp("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testAddPseudoComment() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA act_results TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results.");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results2. \"#EC CI_NOWHERE");
		buildSrc("    SELECT * FROM C_OtherView INTO TABLE @DATA(other_results).");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt WHERE AnyField = 1 INTO TABLE @act_results.");
		buildSrc("    SELECT any_field AS a FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      UNION SELECT any_field AS a FROM I_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      WHERE any_field IN ( SELECT any_field FROM I_AnyView )");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA act_results TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPerfOblgnWithToCurPerdAmt' ).");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD aunit_for_cds_method.");
		buildExp("    prepare_testdata( ).");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results. \"#EC CI_NOWHERE");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results2. \"#EC CI_NOWHERE");
		buildExp("    SELECT * FROM C_OtherView INTO TABLE @DATA(other_results).");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt WHERE AnyField = 1 INTO TABLE @act_results.");
		buildExp("    SELECT any_field AS a FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("      UNION SELECT any_field AS a FROM I_RAPerfOblgnWithToCurPerdAmt");
		buildExp("      INTO TABLE @act_results.");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("      WHERE any_field IN ( SELECT any_field FROM I_AnyView )");
		buildExp("      INTO TABLE @act_results.");
		buildExp("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotAddPseudoComment() {
		rule.configAddPseudoCommentNoWhere.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA act_results TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA act_results TYPE STANDARD TABLE OF C_RAPerfOblgnWithToCurPerdAmt WITH EMPTY KEY.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_C_RAPerfOblgnWithToCurPerd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPerfOblgnWithToCurPerdAmt' ).");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD aunit_for_cds_method.");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}
}
