package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EscapeCharForParametersTest extends RuleTestBase {
	private EscapeCharForParametersRule rule;
	
	EscapeCharForParametersTest() {
		super(RuleID.ESCAPE_CHAR_FOR_PARAMS);
		rule = (EscapeCharForParametersRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_FOR_ABAP_WORDS);
	}
	
	@Test
	void testAddEscapeChar() {
		// expect ! to be added everywhere except before VALUE(...)
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ALWAYS);

		buildSrc("    DATA mv_any TYPE i.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name          TYPE string");
		buildSrc("        EXPORTING");
		buildSrc("          !et_error_table   TYPE ty_tt_any");
		buildSrc("          !ev_value         TYPE i");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          iv_any_param     LIKE mv_any");
		buildSrc("          iv_other_param   TYPE string DEFAULT 'abc'");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE char10");
		buildSrc("        RAISING");
		buildSrc("          cx_any_exception,");
		buildSrc("");
		buildSrc("      third_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          min    TYPE i DEFAULT 0");
		buildSrc("          max    TYPE i DEFAULT 100");
		buildSrc("          sum    TYPE i");
		buildSrc("        EXPORTING");
		buildSrc("          output TYPE string.");

		buildExp("    DATA mv_any TYPE i.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("          !iv_name          TYPE string");
		buildExp("        EXPORTING");
		buildExp("          !et_error_table   TYPE ty_tt_any");
		buildExp("          !ev_value         TYPE i");
		buildExp("        CHANGING");
		buildExp("          !cts_result_table TYPE ty_ts_any,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !iv_any_param     LIKE mv_any");
		buildExp("          !iv_other_param   TYPE string DEFAULT 'abc'");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE char10");
		buildExp("        RAISING");
		buildExp("          cx_any_exception,");
		buildExp("");
		buildExp("      third_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !min    TYPE i DEFAULT 0");
		buildExp("          !max    TYPE i DEFAULT 100");
		buildExp("          !sum    TYPE i");
		buildExp("        EXPORTING");
		buildExp("          !output TYPE string.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAddEscapeCharToMixedCase() {
		// expect ! to be added everywhere, but to be removed before VALUE(...) and REFERENCE(...)
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ALWAYS);

		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !it_source_table TYPE i OPTIONAL");
		buildSrc("        iv_name TYPE string");
		buildSrc("        REFERENCE(iv_ref) TYPE char1");
		buildSrc("        !VALUE(iv_value) TYPE char1");
		buildSrc("        min TYPE i");
		buildSrc("      EXPORTING");
		buildSrc("        et_error_table TYPE i");
		buildSrc("        !ev_value TYPE i");
		buildSrc("        !VALUE(ev_other_value) TYPE char2");
		buildSrc("        REFERENCE(iv_other_ref) TYPE char1");
		buildSrc("        max TYPE i");
		buildSrc("      CHANGING");
		buildSrc("        !cts_result_table TYPE i.");

		buildExp("    METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !it_source_table TYPE i OPTIONAL");
		buildExp("        !iv_name TYPE string");
		buildExp("        REFERENCE(iv_ref) TYPE char1");
		buildExp("        VALUE(iv_value) TYPE char1");
		buildExp("        !min TYPE i");
		buildExp("      EXPORTING");
		buildExp("        !et_error_table TYPE i");
		buildExp("        !ev_value TYPE i");
		buildExp("        VALUE(ev_other_value) TYPE char2");
		buildExp("        REFERENCE(iv_other_ref) TYPE char1");
		buildExp("        !max TYPE i");
		buildExp("      CHANGING");
		buildExp("        !cts_result_table TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveEscapeChar() {
		// expect ! to be removed from all parameter names that are non-ABAP words, 
		// but to be added where a parameter name is an ABAP word

		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_FOR_ABAP_WORDS);

		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name          TYPE string");
		buildSrc("        EXPORTING");
		buildSrc("          !et_error_table   TYPE ty_tt_any");
		buildSrc("          !ev_value         TYPE i");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          iv_any_param     TYPE i      OPTIONAL");
		buildSrc("          iv_other_param   TYPE string DEFAULT 'abc'");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE char10");
		buildSrc("        RAISING");
		buildSrc("          cx_any_exception,");
		buildSrc("");
		buildSrc("      third_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          min    TYPE i DEFAULT 0");
		buildSrc("          max    TYPE i DEFAULT 100");
		buildSrc("          low    TYPE i");
		buildSrc("          high   TYPE i");
		buildSrc("          sum    TYPE i");
		buildSrc("        EXPORTING");
		buildSrc("          output TYPE string.");

		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("          iv_name          TYPE string");
		buildExp("        EXPORTING");
		buildExp("          et_error_table   TYPE ty_tt_any");
		buildExp("          ev_value         TYPE i");
		buildExp("        CHANGING");
		buildExp("          cts_result_table TYPE ty_ts_any,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING");
		buildExp("          iv_any_param     TYPE i      OPTIONAL");
		buildExp("          iv_other_param   TYPE string DEFAULT 'abc'");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE char10");
		buildExp("        RAISING");
		buildExp("          cx_any_exception,");
		buildExp("");
		buildExp("      third_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !min    TYPE i DEFAULT 0");
		buildExp("          !max    TYPE i DEFAULT 100");
		buildExp("          !low    TYPE i");
		buildExp("          !high   TYPE i");
		buildExp("          !sum    TYPE i");
		buildExp("        EXPORTING");
		buildExp("          !output TYPE string.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveAbapWordAddCritical() {
		// expect ! to be removed from all parameter names that do not create syntax errors
		// but to be added for the critical parameter name 'changing'

		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_AVOID_ERRORS);

		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !min     TYPE i DEFAULT 0");
		buildSrc("          !max     TYPE i DEFAULT 100");
		buildSrc("          !low     TYPE i");
		buildSrc("          !high    TYPE i");
		buildSrc("          !sum     TYPE i");
		buildSrc("        EXPORTING");
		buildSrc("          changing TYPE string.");

		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          min     TYPE i DEFAULT 0");
		buildExp("          max     TYPE i DEFAULT 100");
		buildExp("          low     TYPE i");
		buildExp("          high    TYPE i");
		buildExp("          sum     TYPE i");
		buildExp("        EXPORTING");
		buildExp("          !changing TYPE string.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveEscapeCharFromMixedCase() {
		// expect ! to be removed from all parameter names that are non-ABAP words, 
		// but to be added where a parameter name is an ABAP word
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_FOR_ABAP_WORDS);

		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !it_source_table TYPE i OPTIONAL");
		buildSrc("        iv_name TYPE string");
		buildSrc("        REFERENCE(iv_ref) TYPE char1");
		buildSrc("        !VALUE(iv_value) TYPE char1");
		buildSrc("        min TYPE i");
		buildSrc("      EXPORTING");
		buildSrc("        et_error_table TYPE i");
		buildSrc("        !ev_value TYPE i");
		buildSrc("        !VALUE(ev_other_value) TYPE char2");
		buildSrc("        REFERENCE(iv_other_ref) TYPE char1");
		buildSrc("        max TYPE i");
		buildSrc("      CHANGING");
		buildSrc("        !cts_result_table TYPE i.");

		buildExp("    METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        it_source_table TYPE i OPTIONAL");
		buildExp("        iv_name TYPE string");
		buildExp("        REFERENCE(iv_ref) TYPE char1");
		buildExp("        VALUE(iv_value) TYPE char1");
		buildExp("        !min TYPE i");
		buildExp("      EXPORTING");
		buildExp("        et_error_table TYPE i");
		buildExp("        ev_value TYPE i");
		buildExp("        VALUE(ev_other_value) TYPE char2");
		buildExp("        REFERENCE(iv_other_ref) TYPE char1");
		buildExp("        !max TYPE i");
		buildExp("      CHANGING");
		buildExp("        cts_result_table TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEscapeCharKeptInCriticalCase() {
		// expect ! to NOT be removed, as it is critical for syntactical correctness 

		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_AVOID_ERRORS);

		buildSrc("    METHODS fourth_method");
		buildSrc("      IMPORTING");
		buildSrc("        !exporting TYPE i OPTIONAL");
		buildSrc("      EXPORTING");
		buildSrc("        !changing  TYPE string");
		buildSrc("      CHANGING");
		buildSrc("        !raising   TYPE i.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepEscapeChar() {
		// expect nothing to be changed
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.KEEP_AS_IS);

		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name          TYPE string");
		buildSrc("        EXPORTING");
		buildSrc("          !et_error_table   TYPE ty_tt_any");
		buildSrc("          !ev_value         TYPE i");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          iv_any_param     TYPE i      OPTIONAL");
		buildSrc("          iv_other_param   TYPE string DEFAULT 'abc'");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE char10");
		buildSrc("        RAISING");
		buildSrc("          cx_any_exception,");
		buildSrc("");
		buildSrc("      third_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          min    TYPE i DEFAULT 0");
		buildSrc("          max    TYPE i DEFAULT 100");
		buildSrc("          sum    TYPE i");
		buildSrc("        EXPORTING");
		buildSrc("          output TYPE string.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepEscapeCharInMixedCase() {
		// expect nothing to be changed
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.KEEP_AS_IS);

		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !it_source_table TYPE i OPTIONAL");
		buildSrc("        iv_name TYPE string");
		buildSrc("        REFERENCE(iv_ref) TYPE char1");
		buildSrc("        !VALUE(iv_value) TYPE char1");
		buildSrc("        min TYPE i");
		buildSrc("      EXPORTING");
		buildSrc("        et_error_table TYPE i");
		buildSrc("        !ev_value TYPE i");
		buildSrc("        !VALUE(ev_other_value) TYPE char2");
		buildSrc("        REFERENCE(iv_other_ref) TYPE char1");
		buildSrc("        max TYPE i");
		buildSrc("      CHANGING");
		buildSrc("        !cts_result_table TYPE i.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testAddEscapeCharToInterfaceMixedCase() {
		// expect ! to be added everywhere, but to be removed before VALUE(...) and REFERENCE(...)
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ALWAYS);

		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !it_source_table TYPE i OPTIONAL");
		buildSrc("        iv_name TYPE string");
		buildSrc("        REFERENCE(iv_ref) TYPE char1");
		buildSrc("        !VALUE(iv_value) TYPE char1");
		buildSrc("        min TYPE i");
		buildSrc("      EXPORTING");
		buildSrc("        et_error_table TYPE i");
		buildSrc("        !ev_value TYPE i");
		buildSrc("        !VALUE(ev_other_value) TYPE char2");
		buildSrc("        REFERENCE(iv_other_ref) TYPE char1");
		buildSrc("        max TYPE i");
		buildSrc("      CHANGING");
		buildSrc("        !cts_result_table TYPE i.");

		buildExp("    METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !it_source_table TYPE i OPTIONAL");
		buildExp("        !iv_name TYPE string");
		buildExp("        REFERENCE(iv_ref) TYPE char1");
		buildExp("        VALUE(iv_value) TYPE char1");
		buildExp("        !min TYPE i");
		buildExp("      EXPORTING");
		buildExp("        !et_error_table TYPE i");
		buildExp("        !ev_value TYPE i");
		buildExp("        VALUE(ev_other_value) TYPE char2");
		buildExp("        REFERENCE(iv_other_ref) TYPE char1");
		buildExp("        !max TYPE i");
		buildExp("      CHANGING");
		buildExp("        !cts_result_table TYPE i.");

		putAnyInterfaceDefAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testOutsideClassOrInterfaceDef() {
		// expect nothing to happen
		
		rule.configUseEscapeCharForParams.setEnumValue(EscapeCharAction.ONLY_FOR_ABAP_WORDS);

		buildSrc("    DO 5 TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodsForEvent() {
		buildSrc("    METHODS handle_any_event");
		buildSrc("      FOR EVENT any_event OF cl_any_sender");
		buildSrc("      IMPORTING !any_param");
		buildSrc("                !other_param");
		buildSrc("                !sender.");
		buildSrc("");
		buildSrc("    METHODS handle_other_event");
		buildSrc("      FOR EVENT other_event OF cl_any_sender");
		buildSrc("      IMPORTING !any_param !other_param !sender.");

		buildExp("    METHODS handle_any_event");
		buildExp("      FOR EVENT any_event OF cl_any_sender");
		buildExp("      IMPORTING any_param");
		buildExp("                other_param");
		buildExp("                sender.");
		buildExp("");
		buildExp("    METHODS handle_other_event");
		buildExp("      FOR EVENT other_event OF cl_any_sender");
		buildExp("      IMPORTING any_param other_param sender.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}
