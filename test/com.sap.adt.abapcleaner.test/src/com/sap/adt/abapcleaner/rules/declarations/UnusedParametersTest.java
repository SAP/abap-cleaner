package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class UnusedParametersTest extends RuleTestBase {
	private UnusedParametersRule rule;
	
	UnusedParametersTest() {
		super(RuleID.UNUSED_PARAMETERS);
		rule = (UnusedParametersRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configImportingParamScope.setEnumValue(UnusedParameterScope.NON_INTERFACE_METHODS);
		rule.configExportingParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configChangingParamScope.setEnumValue(UnusedParameterScope.NON_INTERFACE_METHODS);
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.NEVER);
		rule.configIgnoreEmptyMethods.setValue(true);
		rule.configIgnoreExportingByValue.setValue(true);
	}
	
	@Test
	void testAllParametersUsedOrAssigned() {
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.PRIVATE_ONLY);
		rule.configIgnoreExportingByValue.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    CLEAR ev_count.");
		buildSrc("    rv_result = its_any_table[ 1 ]-any_value + cs_any_struc-any_comp.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAddTodoComments() {
		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    CLEAR ev_count.");
		buildSrc("*    other_method( its_any_table ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING ev_count         TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is only used in commented-out code (ABAP cleaner)");
		buildExp("    \" TODO: parameter CS_ANY_STRUC is never used or assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    CLEAR ev_count.");
		buildExp("*    other_method( its_any_table ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testInferfaceMethod() {
		rule.configImportingParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configChangingParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configIgnoreEmptyMethods.setValue(false);

		buildSrc("INTERFACE if_any_interface.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_any_interface.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildSrc("  METHOD if_any_interface~any_method.");
		buildSrc("*    CLEAR cs_any_struc.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE if_any_interface.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING ev_count         TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_detect_unused_parameters DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildExp("  METHOD if_any_interface~any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildExp("    \" TODO: parameter EV_COUNT is never cleared or assigned (ABAP cleaner)");
		buildExp("    \" TODO: parameter CS_ANY_STRUC is only used or assigned in commented-out code (ABAP cleaner)");
		buildExp("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildExp("");
		buildExp("*    CLEAR cs_any_struc.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testNonInterfaceMethodsOnly() {
		rule.configExportingParamScope.setEnumValue(UnusedParameterScope.NON_INTERFACE_METHODS);
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.NON_INTERFACE_METHODS);
		rule.configIgnoreEmptyMethods.setValue(false);

		buildSrc("INTERFACE if_any_interface.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDINTERFACE.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_any_interface.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildSrc("  METHOD if_any_interface~any_method.");
		buildSrc("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildSrc("");
		buildSrc("*    CLEAR cs_any_struc.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("INTERFACE if_any_interface.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING ev_count         TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_detect_unused_parameters DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildExp("  METHOD if_any_interface~any_method.");
		buildExp("*    CLEAR cs_any_struc.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testSuppressNeededReportReturningParam() {
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.NON_INTERFACE_METHODS);

		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table        TYPE ty_ts_table");
		buildSrc("                iv_unused_but_needed TYPE string ##NEEDED");
		buildSrc("      EXPORTING ev_count             TYPE i");
		buildSrc("      CHANGING  cs_any_struc         TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result)     TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    CLEAR ev_count.");
		buildSrc("*    other_method( its_any_table ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table        TYPE ty_ts_table");
		buildExp("                iv_unused_but_needed TYPE string ##NEEDED");
		buildExp("      EXPORTING ev_count             TYPE i");
		buildExp("      CHANGING  cs_any_struc         TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result)     TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is only used in commented-out code (ABAP cleaner)");
		buildExp("    \" TODO: parameter CS_ANY_STRUC is never used or assigned (ABAP cleaner)");
		buildExp("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    CLEAR ev_count.");
		buildExp("*    other_method( its_any_table ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testSuppressReportForPublicMethod() {
		rule.configImportingParamScope.setEnumValue(UnusedParameterScope.PROTECTED_OR_PRIVATE);
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.PRIVATE_ONLY);

		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    CLEAR cs_any_struc.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING ev_count         TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter EV_COUNT is never cleared or assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    DATA lv_any TYPE i.");
		buildExp("    CLEAR cs_any_struc.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveOldTodosIgnoreExportingByValue() {
		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING VALUE(ev_count)  TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildSrc("    \" TODO: parameter EV_OBSOLETE is never cleared or assigned (ABAP cleaner)");
		buildSrc("    \" TODO: parameter EV_COUNT is never cleared or assigned (ABAP cleaner)");
		buildSrc("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildSrc("");
		buildSrc("    \" TODO: parameter related comment, but not generated by ABAP cleaner!");
		buildSrc("    CLEAR cs_any_struc.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING VALUE(ev_count)  TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildExp("");
		buildExp("    \" TODO: parameter related comment, but not generated by ABAP cleaner!");
		buildExp("    CLEAR cs_any_struc.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testReportExportingByValue() {
		rule.configIgnoreExportingByValue.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      EXPORTING VALUE(ev_count)  TYPE i");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    other_method( ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      EXPORTING VALUE(ev_count)  TYPE i");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter EV_COUNT is never assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    other_method( ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveOldTodosOnly() {
		rule.configImportingParamScope.setEnumValue(UnusedParameterScope.NEVER);
		rule.configExportingParamScope.setEnumValue(UnusedParameterScope.NEVER);
		rule.configChangingParamScope.setEnumValue(UnusedParameterScope.NEVER);
		rule.configIgnoreExportingByValue.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    METHODS any_method.");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING iv_any TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildSrc("    \" TODO: parameter EV_OBSOLETE is never cleared or assigned (ABAP cleaner)");
		buildSrc("    \" TODO: parameter EV_COUNT is never cleared or assigned (ABAP cleaner)");
		buildSrc("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildSrc("");
		buildSrc("    \" comment");
		buildSrc("    CLEAR cs_any_struc.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD other_method.");
		buildSrc("    \" TODO: parameter ITS_OBSOLETE is never used (ABAP cleaner)");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    METHODS any_method.");
		buildExp("    METHODS other_method");
		buildExp("      IMPORTING iv_any TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" comment");
		buildExp("    CLEAR cs_any_struc.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD other_method.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testMethodWithNoExecutableCommand() {
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.PRIVATE_ONLY);

		buildSrc("CLASS cl_detect_unused_parameters DEFINITION.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("*     comment");
		buildSrc("    \" comment");
		buildSrc("    LOG-POINT ID group.");
		buildSrc("    BREAK-POINT.");
		buildSrc("    ASSERT 1 = 2.");
		buildSrc("    ##NO_TEXT");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testUnknownMethodSignatures() {
		rule.configImportingParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configChangingParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);

		buildSrc("CLASS cl_detect_unused_parameters DEFINITION.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildSrc("  METHOD other_method.");
		buildSrc("    CLEAR iv_any.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD if_any_interface~any_method.");
		buildSrc("    CLEAR iv_any.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testIncludeEmptyMethods() {
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.PRIVATE_ONLY);
		rule.configIgnoreEmptyMethods.setValue(false);

		buildSrc("CLASS cl_detect_unused_parameters DEFINITION.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildSrc("      EXPORTING ev_count         TYPE i");
		buildSrc("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_detect_unused_parameters DEFINITION.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table    TYPE ty_ts_table");
		buildExp("      EXPORTING ev_count         TYPE i");
		buildExp("      CHANGING  cs_any_struc     TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_detect_unused_parameters IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is never used (ABAP cleaner)");
		buildExp("    \" TODO: parameter EV_COUNT is never cleared or assigned (ABAP cleaner)");
		buildExp("    \" TODO: parameter CS_ANY_STRUC is never used or assigned (ABAP cleaner)");
		buildExp("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}
	
	@Test
	void testInheritedSignatures() {
		// ensure that method signatures are also found up the class hierarchy
		// also, ensure that 'RETURN 42.' is identified as an assignment to RV_RESULT in OTHER_METHOD
		
		rule.configReturningParamScope.setEnumValue(UnusedParameterScope.ALL_METHODS);
		
		buildSrc("CLASS cl_grandparent_class DEFINITION.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING iv_any_value     TYPE i");
		buildSrc("      EXPORTING et_error         TYPE ty_tt_error");
		buildSrc("      RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_parent_class DEFINITION INHERITING FROM cl_grandparent_class.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING its_any_table        TYPE ty_ts_table");
		buildSrc("                iv_unused_but_needed TYPE string ##NEEDED");
		buildSrc("      EXPORTING ev_count             TYPE i");
		buildSrc("      CHANGING  cs_any_struc         TYPE ty_s_struc");
		buildSrc("      RETURNING VALUE(rv_result)     TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_any_class DEFINITION INHERITING FROM cl_parent_class.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method REDEFINITION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    METHODS other_method REDEFINITION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    CLEAR ev_count.");
		buildSrc("");
		buildSrc("*    LOOP AT its_any_table ASSIGNING <ls_any_struc>.");
		buildSrc("*      lv_commented_out += 1.");
		buildSrc("*    ENDLOOP.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("  METHOD other_method.");
		buildSrc("    RETURN 42.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_grandparent_class DEFINITION.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    METHODS other_method");
		buildExp("      IMPORTING iv_any_value     TYPE i");
		buildExp("      EXPORTING et_error         TYPE ty_tt_error");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_parent_class DEFINITION INHERITING FROM cl_grandparent_class.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING its_any_table        TYPE ty_ts_table");
		buildExp("                iv_unused_but_needed TYPE string ##NEEDED");
		buildExp("      EXPORTING ev_count             TYPE i");
		buildExp("      CHANGING  cs_any_struc         TYPE ty_s_struc");
		buildExp("      RETURNING VALUE(rv_result)     TYPE i.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_any_class DEFINITION INHERITING FROM cl_parent_class.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method REDEFINITION.");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    METHODS other_method REDEFINITION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    \" TODO: parameter ITS_ANY_TABLE is only used in commented-out code (ABAP cleaner)");
		buildExp("    \" TODO: parameter CS_ANY_STRUC is never used or assigned (ABAP cleaner)");
		buildExp("    \" TODO: parameter RV_RESULT is never assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    CLEAR ev_count.");
		buildExp("");
		buildExp("*    LOOP AT its_any_table ASSIGNING <ls_any_struc>.");
		buildExp("*      lv_commented_out += 1.");
		buildExp("*    ENDLOOP.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD other_method.");
		buildExp("    \" TODO: parameter IV_ANY_VALUE is never used (ABAP cleaner)");
		buildExp("    \" TODO: parameter ET_ERROR is never cleared or assigned (ABAP cleaner)");
		buildExp("");
		buildExp("    RETURN 42.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testAMDPMethodSkipped() {
		buildSrc("CLASS cl_any DEFINITION.");
		buildSrc("  PUBLIC_SECTION.");
		buildSrc("     METHODS any_amdp");
		buildSrc("        IMPORTING VALUE(iv_param) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_any IMPLEMENTATION.");
		buildSrc("  METHOD any_amdp BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.");
		buildSrc("    DECLARE query STRING;");
		buildSrc("    query = 'text' || iv_param;");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testDynamicAssign() {
		// ensure that methods with dynamic assigns are completely skipped
		buildSrc("CLASS cl_any DEFINITION.");
		buildSrc("  PUBLIC_SECTION.");
		buildSrc("     METHODS any_method");
		buildSrc("        IMPORTING iv_any_param TYPE i");
		buildSrc("        RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_any IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    ASSIGN ('iv_any_param') TO FIELD-SYMBOL(<lv_any>).");
		buildSrc("    rv_result = <lv_any>.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCallTransformationResult() {
		buildSrc("CLASS any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS serialize RETURNING VALUE(result) TYPE any_type.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS any_class IMPLEMENTATION.");
		buildSrc("  METHOD serialize.");
		buildSrc("    \" TODO: parameter RESULT is never cleared or assigned (ABAP cleaner)");
		buildSrc("    CALL TRANSFORMATION id");
		buildSrc("         SOURCE values = object");
		buildSrc("         RESULT XML result.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS serialize RETURNING VALUE(result) TYPE any_type.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS any_class IMPLEMENTATION.");
		buildExp("  METHOD serialize.");
		buildExp("    CALL TRANSFORMATION id");
		buildExp("         SOURCE values = object");
		buildExp("         RESULT XML result.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}
}
