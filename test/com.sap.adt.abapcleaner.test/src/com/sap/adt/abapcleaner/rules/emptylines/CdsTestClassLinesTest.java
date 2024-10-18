package com.sap.adt.abapcleaner.rules.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rules.ddl.emptylines.RemoveCommentCondition;

public class CdsTestClassLinesTest extends RuleTestBase {
	private CdsTestClassLinesRule rule;
	
	CdsTestClassLinesTest() {
		super(RuleID.CDS_TEST_CLASS_LINES);
		rule = (CdsTestClassLinesRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRemoveAbapDoc.setValue(true);
		rule.configRemoveToDoComments.setEnumValue(RemoveCommentCondition.IF_VALUE_HAS_CONTENT);
		rule.configEmptyLineAboveInsertTestData.setValue(true);
		rule.configEmptyLineAboveSelect.setValue(true);
		rule.configEmptyLineBelowSelect.setValue(true);
		rule.configMovePrepareMethods.setValue(false);
	}

	@Test
	void testRemoveAbapDocFromCommands() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    \"! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    \"! SETUP method creates a common start state for each test method,");
		buildSrc("    \"! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.");
		buildSrc("    METHODS setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_testdata.");
		buildSrc("    \"! In this method test data is inserted into the generated double(s) and the test is executed and");
		buildSrc("    \"! the results should be asserted with the actuals.");
		buildSrc("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildExp("");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    CLASS-METHODS class_teardown.");
		buildExp("");
		buildExp("    METHODS setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_testdata.");
		buildExp("    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveAbapDocFromTokens() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    CLASS-METHODS:");
		buildSrc("      \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("      class_setup RAISING cx_static_check,");
		buildSrc("      \"! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.");
		buildSrc("      class_teardown.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! SETUP method creates a common start state for each test method,");
		buildSrc("      \"! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.");
		buildSrc("      setup RAISING cx_static_check,");
		buildSrc("      prepare_testdata,");
		buildSrc("      \"! In this method test data is inserted into the generated double(s) and the test is executed and");
		buildSrc("      \"! the results should be asserted with the actuals.");
		buildSrc("      aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildExp("");
		buildExp("    CLASS-METHODS:");
		buildExp("      class_setup RAISING cx_static_check,");
		buildExp("      class_teardown.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      setup RAISING cx_static_check,");
		buildExp("      prepare_testdata,");
		buildExp("      aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotRemoveAbapDoc() {
		rule.configRemoveAbapDoc.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    \"! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! SETUP method creates a common start state for each test method,");
		buildSrc("      \"! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.");
		buildSrc("      setup RAISING cx_static_check,");
		buildSrc("      prepare_testdata,");
		buildSrc("      \"! In this method test data is inserted into the generated double(s) and the test is executed and");
		buildSrc("      \"! the results should be asserted with the actuals.");
		buildSrc("      aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoAbapDocOrUnknownText() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-DATA environment TYPE REF TO if_cds_test_environment.");
		buildSrc("");
		buildSrc("    \" In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    \"! In CLASS_TEARDOWN GENERATED database entities (doubles & clones) should be deleted at the end of test class execution.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \" SETUP method creates a common start state for each test method,");
		buildSrc("      \"! clear_doubles clears the test data for all the doubles used in the test method before each test method execution");
		buildSrc("      setup RAISING cx_static_check,");
		buildSrc("      prepare_testdata,");
		buildSrc("      \"! In this method test data is inserted into the generated double and the test is executed and");
		buildSrc("      \" the results should be asserted with the actuals.");
		buildSrc("      aunit_for_cds_method FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoCommentBeforeClassDef() {
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoAbapDocOnClassDef() {
		// '!' missing
		buildSrc("\"@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoTestingAnnotation() {
		// '@' missing
		buildSrc("\"!testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysRemoveTodoComments() {
		rule.configRemoveToDoComments.setEnumValue(RemoveCommentCondition.ALWAYS);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_customer = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    \" Prepare test data for 'i_businesspartner'");
		buildExp("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("");
		buildExp("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'i_customer'");
		buildExp("    td_i_customer = VALUE #( ( ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_customer ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveTodoCommentsIfValueHasContent() {
		rule.configEmptyLineAboveInsertTestData.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_customer = VALUE #(");
		buildSrc("      (");
		buildSrc("      )");
		buildSrc("    ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    call_any_method( ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    \" Prepare test data for 'i_businesspartner'");
		buildExp("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'i_customer'");
		buildExp("    \" TODO: Provide the test data here");
		buildExp("    td_i_customer = VALUE #(");
		buildExp("      (");
		buildExp("      )");
		buildExp("    ).");
		buildExp("    environment->insert_test_data( i_data = td_i_customer ).");
		buildExp("");
		buildExp("    call_any_method( ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testNeverRemoveToDoComments() {
		rule.configRemoveToDoComments.setEnumValue(RemoveCommentCondition.NEVER);
		rule.configEmptyLineAboveInsertTestData.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    \" TODO: Provide the test data here");
		buildSrc("    td_i_customer = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testInsertEmptyLineAboveInsertTestData() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #( ( ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD prepare_testdata.");
		buildExp("    \" Prepare test data for 'i_businesspartner'");
		buildExp("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildExp("                                    ( BusinessPartner = 'other' ) ).");
		buildExp("");
		buildExp("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildExp("");
		buildExp("    \" Prepare test data for 'i_customer'");
		buildExp("    td_i_customer = VALUE #( ( ) ).");
		buildExp("    environment->insert_test_data( i_data = td_i_customer ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotInsertEmptyLineAboveInsertTestData() {
		rule.configEmptyLineAboveInsertTestData.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #(");
		buildSrc("      (");
		buildSrc("      )");
		buildSrc("    ).");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testEmptyLineOrCommentFoundAboveInsertTestData() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD prepare_testdata.");
		buildSrc("    \" Prepare test data for 'i_businesspartner'");
		buildSrc("    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )");
		buildSrc("                                    ( BusinessPartner = 'other' ) ).");
		buildSrc("");
		buildSrc("    environment->insert_test_data( i_data = td_i_businesspartner ).");
		buildSrc("");
		buildSrc("    \" Prepare test data for 'i_customer'");
		buildSrc("    td_i_customer = VALUE #( ( ) ).");
		buildSrc("    \" comment");
		buildSrc("    environment->insert_test_data( i_data = td_i_customer ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testInsertEmptyLineAroundSelect() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    \" query the view under test");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD aunit_for_cds_method.");
		buildExp("    prepare_testdata( ).");
		buildExp("");
		buildExp("    \" query the view under test");
		buildExp("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildExp("      INTO TABLE @act_results.");
		buildExp("");
		buildExp("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testNoEmptyLineAroundSelectWithoutSiblings() {
		// expect that empty lines are only added around SELECT if it has a sibling Command above / below
		
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    IF iv_any_condition = abap_true.");
		buildSrc("      \" query the view under test");
		buildSrc("      SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("        INTO TABLE @act_results.");
		buildSrc("    ENDIF.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testDoNotInsertEmptyLineAboveSelect() {
		rule.configEmptyLineAboveSelect.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    \" query the view under test");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testDoNotInsertEmptyLineBelowSelect() {
		rule.configEmptyLineBelowSelect.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("");
		buildSrc("    \" query the view under test");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testDoNotInsertEmptyLinAroundSelect() {
		rule.configEmptyLineAboveSelect.setValue(false);
		rule.configEmptyLineBelowSelect.setValue(false);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD aunit_for_cds_method.");
		buildSrc("    prepare_testdata( ).");
		buildSrc("    \" query the view under test");
		buildSrc("    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt");
		buildSrc("      INTO TABLE @act_results.");
		buildSrc("    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testMoveTwoPrepareMethods() {
		rule.configMovePrepareMethods.setValue(true);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_method_1.");
		buildSrc("    METHODS prepare_method_2.");
		buildSrc("    METHODS test_method_1 FOR TESTING.");
		buildSrc("    METHODS test_method_2 FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD class_teardown.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD test_method_1.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD test_method_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method_1.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    CLASS-METHODS class_teardown.");
		buildExp("");
		buildExp("    METHODS setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_method_1.");
		buildExp("    METHODS prepare_method_2.");
		buildExp("    METHODS test_method_1 FOR TESTING.");
		buildExp("    METHODS test_method_2 FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD class_teardown.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_method_1.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_method_2.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD test_method_1.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD test_method_2.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testMoveSecondPrepareMethod() {
		rule.configMovePrepareMethods.setValue(true);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_method_1.");
		buildSrc("    METHODS prepare_method_2.");
		buildSrc("    METHODS test_method_1 FOR TESTING.");
		buildSrc("    METHODS test_method_2 FOR TESTING RAISING cx_static_check.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD class_teardown.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method_1.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD test_method_1.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD test_method_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildExp("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildExp("    CLASS-METHODS class_teardown.");
		buildExp("");
		buildExp("    METHODS setup RAISING cx_static_check.");
		buildExp("    METHODS prepare_method_1.");
		buildExp("    METHODS prepare_method_2.");
		buildExp("    METHODS test_method_1 FOR TESTING.");
		buildExp("    METHODS test_method_2 FOR TESTING RAISING cx_static_check.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildExp("  METHOD class_setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD setup.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD class_teardown.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_method_1.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD prepare_method_2.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD test_method_1.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD test_method_2.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotMovePrepareMethodWithoutTestMethod() {
		rule.configMovePrepareMethods.setValue(true);

		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_method.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD class_teardown.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testDoNotMovePrepareMethod() {
		buildSrc("\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL");
		buildSrc("  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    CLASS-METHODS class_setup RAISING cx_static_check.");
		buildSrc("    CLASS-METHODS class_teardown.");
		buildSrc("");
		buildSrc("    METHODS setup RAISING cx_static_check.");
		buildSrc("    METHODS prepare_method.");
		buildSrc("    METHODS test_method FOR TESTING.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION.");
		buildSrc("  METHOD class_setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD setup.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD class_teardown.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD test_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD prepare_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}
}