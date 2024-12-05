package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

class AlignMethodsDeclarationTest extends RuleTestBase {
	private AlignMethodsDeclarationRule rule;
	
	AlignMethodsDeclarationTest() {
		super(RuleID.ALIGN_METHODS_DECLARATION);
		rule = (AlignMethodsDeclarationRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configContinueAfterAccess.setEnumValue(ChangeType.ALWAYS);
		rule.configFillPercentageToJustifyOwnColumn.setValue(40);
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.KEEP_EXISTING);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ONE_LINERS);
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(false);
		rule.configSeparateWithEmptyLine.setValue(true);
	}
	
	@Test
	void testNonChains() {
		// expectation:
		// - TYPE and OPTIONAL/DEFAULT are aligned 
		// - line-breaks after method names are kept
		// - line-breaks after IMPORTING etc. are removed
		
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ONE_LINERS);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception");
		buildSrc("        !cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("      EXPORTING !ev_any_result   TYPE i");
		buildExp("                !ev_other_result TYPE string");
		buildExp("      RAISING   !cx_any_exception");
		buildExp("                !cx_other_exception.");
		buildExp("");
		buildExp("    METHODS other_method IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                                   !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                         EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChains() {
		// expectation:
		// - TYPE and OPTIONAL/DEFAULT are aligned
		// - line-breaks after method names are kept
		// - line-breaks after IMPORTING etc. are removed
		// - an empty line is added below the one-liners
		
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING !iv_any_param   TYPE i      OPTIONAL");
		buildExp("                  !iv_other_param TYPE string DEFAULT 'abc'");
		buildExp("        RAISING   !cx_any_exception,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("                  !iv_name          TYPE string");
		buildExp("        CHANGING  !cts_result_table TYPE ty_ts_any.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainMixWithAndWithoutParams() {
		// expectation: all three methods are put behind "METHODS:", but alignment of access keywords is  
		// NOT influenced by a (very long) method name that has NO parameters to it 
		buildSrc("    METHODS: short_name IMPORTING !iv_new_value TYPE i,");
		buildSrc("      very_long_method_name,");
		buildSrc("      short_name_2 RETURNING VALUE(rv_value) TYPE i.");

		buildExp("    METHODS: short_name   IMPORTING !iv_new_value   TYPE i,");
		buildExp("             very_long_method_name,");
		buildExp("             short_name_2 RETURNING VALUE(rv_value) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysContinueAfterKeyword() {
		// expectation: name of first method is put directly behind 'METHODS:'
		
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);

		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");

		buildExp("    METHODS: any_chained_method");
		buildExp("      IMPORTING !iv_any_param   TYPE i      OPTIONAL");
		buildExp("                !iv_other_param TYPE string DEFAULT 'abc'");
		buildExp("      RAISING   !cx_any_exception,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("                  !iv_name          TYPE string");
		buildExp("        CHANGING  !cts_result_table TYPE ty_ts_any.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNeverContinueAfterKeyword() {
		// expectation: line-breaks are introduced between METHODS and method name, except for one-liners 
		
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS");
		buildExp("      any_method");
		buildExp("        IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                  !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("        EXPORTING !ev_any_result   TYPE i");
		buildExp("                  !ev_other_result TYPE string");
		buildExp("        RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS");
		buildExp("      other_method IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                             !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                   EXPORTING !ev_any_result   TYPE i");
		buildExp("                             !ev_other_result TYPE string");
		buildExp("                   RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS");
		buildExp("      get_max_value");
		buildExp("        RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysContinueAfterMethodName() {
		// expectation: 
		// - IMPORTING etc. keywords are moved directly behind the method name 
		// - IMPORTING etc. keywords are aligned per method (and for one-liners, across several methods)
		
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS any_method IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                                       !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                             EXPORTING !ev_any_result   TYPE i");
		buildExp("                                       !ev_other_result TYPE string");
		buildExp("                             RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_max_value      RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNeverContinueAfterMethodName() {
		// expectation: IMPORTING etc. keywords are moved to the next line, below METHODS (NOT below the method name) 

		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);

		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");

		buildExp("    METHODS other_method");
		buildExp("      IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("      EXPORTING !ev_any_result   TYPE i");
		buildExp("                !ev_other_result TYPE string");
		buildExp("      RAISING   !cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepLineBreaksAfterAccessNonChains() {
		// expectation: 
		// - line breaks after IMPORTING etc. are kept as they are
		// - alignment is done per method (and for one-liners, across several methods)
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ONE_LINERS);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !iv_any_param    TYPE i      OPTIONAL");
		buildExp("        !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("      EXPORTING");
		buildExp("        !ev_any_result   TYPE i");
		buildExp("        !ev_other_result TYPE string");
		buildExp("      RAISING");
		buildExp("        !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS other_method IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                                   !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                         EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepLineBreaksAfterAccessChains() {
		// expectation: 
		// - line breaks after IMPORTING etc. are kept as they are
		// - alignment is done per method (and for one-liners, across several methods)
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configSeparateWithEmptyLine.setValue(false);
		
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !iv_any_param   TYPE i      OPTIONAL");
		buildExp("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildExp("        RAISING");
		buildExp("          !cx_any_exception,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("          !iv_name          TYPE string");
		buildExp("        CHANGING");
		buildExp("          !cts_result_table TYPE ty_ts_any.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNeverContinueAfterAccess() {
		// expectation: 
		// - line breaks are introduced after IMPORTING etc., except in one-liners
		// - alignment is done per method (and for one-liners, across several methods)
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.NEVER);

		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS other_method IMPORTING");
		buildExp("                           !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                           !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                         EXPORTING");
		buildExp("                           !ev_any_result   TYPE i");
		buildExp("                           !ev_other_result TYPE string");
		buildExp("                         RAISING");
		buildExp("                           !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testHighRequiredFillRatio() {
		// expectation:  
		// - alignment is done per method (and for one-liners, across several methods)
		// - OPTIONAL/DEFAULT are NOT aligned, because they do not meet the required fill ratio of 80%
		
		rule.configFillPercentageToJustifyOwnColumn.setValue(80);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception.");

		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING !iv_any_param    TYPE i OPTIONAL");
		buildExp("                !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("      EXPORTING !ev_any_result   TYPE i");
		buildExp("                !ev_other_result TYPE string");
		buildExp("      RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS other_method IMPORTING !iv_any_param    TYPE i OPTIONAL");
		buildExp("                                   !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                         EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING !iv_any_param   TYPE i      OPTIONAL");
		buildExp("                  !iv_other_param TYPE string DEFAULT 'abc'");
		buildExp("        RAISING   !cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateOneLinersNonChained() {
		// expectation:  
		// - the potential one-liner method declaration GET_MAX_VALUE is changed into a one-liner
		// - alignment is done across all one-liner methods
		
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_max_value      RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateOneLinersChained() {
		// expectation:  
		// - the potential one-liner method declaration GET_MAX_VALUE_CHAIN is changed into a one-liner
		// - alignment is done across all one-liner methods

		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);

		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDissolveOneLiners() {
		// expectation:  
		// - all one-liners are dissolved into multi-liners
		// - alignment is done per method (however in this case, there is nothing to align)
		
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configContinueAfterAccess.setEnumValue(ChangeType.NEVER);
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.SAME_AS_MULTI_LINERS);
		rule.configSeparateWithEmptyLine.setValue(false);
		
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value");
		buildExp("      IMPORTING");
		buildExp("        !iv_new_value TYPE i.");
		buildExp("    METHODS get_current_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained");
		buildExp("      IMPORTING");
		buildExp("        !iv_new_value TYPE i,");
		buildExp("      get_current_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i,");
		buildExp("      get_previous_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i,");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDissolveOneLinersEmptyLines() {
		// expectation:  
		// - all one-liners are dissolved into multi-liners
		// - alignment is done per method (however in this case, there is nothing to align)
		
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configContinueAfterAccess.setEnumValue(ChangeType.NEVER);
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.SAME_AS_MULTI_LINERS);
		rule.configSeparateWithEmptyLine.setValue(true);
		
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value");
		buildExp("      IMPORTING");
		buildExp("        !iv_new_value TYPE i.");
		buildExp("");
		buildExp("    METHODS get_current_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_previous_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained");
		buildExp("      IMPORTING");
		buildExp("        !iv_new_value TYPE i,");
		buildExp("");
		buildExp("      get_current_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i,");
		buildExp("");
		buildExp("      get_previous_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i,");
		buildExp("");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAlignConsecutiveOneLiners() {
		// expectation: alignment is only done per method, NOT across methods (so, in this case, there is nothing to align)
		
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.NEVER);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAcrossEmptyLinesAndComments() {
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);
		rule.configAlignAcrossCommentLines.setValue(true);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");
		buildSrc("*   comment");
		buildSrc("    METHODS: set_price IMPORTING !iv_price TYPE i,");
		buildSrc("      get_price RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS  set_value                  IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS  get_current_value          RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS  get_previous_value         RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    \" comment");
		buildExp("    METHODS  get_max_value              RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("*   comment");
		buildExp("    METHODS: set_price                  IMPORTING !iv_price        TYPE i,");
		buildExp("             get_price                  RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDoNotAlignAcrossComments() {
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);
		rule.configAlignAcrossCommentLines.setValue(false);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");
		buildSrc("*   comment");
		buildSrc("    METHODS: set_price IMPORTING !iv_price TYPE i,");
		buildSrc("      get_price RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    \" comment");
		buildExp("    METHODS  get_max_value              RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("*   comment");
		buildExp("    METHODS: set_price IMPORTING !iv_price        TYPE i,");
		buildExp("             get_price RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testDoNotAlignAcrossEmptyLines() {
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);
		rule.configAlignAcrossEmptyLines.setValue(false);
		rule.configAlignAcrossCommentLines.setValue(true);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");
		buildSrc("*   comment");
		buildSrc("    METHODS: set_price IMPORTING !iv_price TYPE i,");
		buildSrc("      get_price RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value         IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    \" comment");
		buildExp("    METHODS get_max_value      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained         IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("*   comment");
		buildExp("    METHODS: set_price IMPORTING !iv_price        TYPE i,");
		buildExp("             get_price RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}



	@Test
	void testDoNotAlignAcrossEmptyLinesOrComments() {
		rule.configHandleOneLiners.setEnumValue(MethodsOneLinerAction.CREATE);
		rule.configAlignAcrossEmptyLines.setValue(false);
		rule.configAlignAcrossCommentLines.setValue(false);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");
		buildSrc("*   comment");
		buildSrc("    METHODS: set_price IMPORTING !iv_price TYPE i,");
		buildSrc("      get_price RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value         IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    \" comment");
		buildExp("    METHODS get_max_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: set_value_chained         IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("*   comment");
		buildExp("    METHODS: set_price IMPORTING !iv_price        TYPE i,");
		buildExp("             get_price RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentAlignment() {
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("    METHODS");
		buildSrc("\" comment A");
		buildSrc("      get_max_value");
		buildSrc("\" comment B");
		buildSrc("        RETURNING");
		buildSrc("\" comment C");
		buildSrc("          VALUE(rv_result) TYPE i");
		buildSrc("\" comment D");
		buildSrc("        RAISING");
		buildSrc("\" comment E");
		buildSrc("          cx_any_exception.");
		buildSrc("");
		buildSrc("    CLASS-METHODS:");
		buildSrc("\" comment A");
		buildSrc("      get_min_value");
		buildSrc("\" comment B");
		buildSrc("        RETURNING");
		buildSrc("\" comment C");
		buildSrc("          VALUE(rv_result) TYPE i");
		buildSrc("\" comment D");
		buildSrc("        RAISING");
		buildSrc("\" comment E");
		buildSrc("          cx_any_exception.");

		buildExp("    METHODS");
		buildExp("      \" comment A");
		buildExp("      get_max_value");
		buildExp("        \" comment B");
		buildExp("        RETURNING");
		buildExp("          \" comment C");
		buildExp("          VALUE(rv_result) TYPE i");
		buildExp("        \" comment D");
		buildExp("        RAISING");
		buildExp("          \" comment E");
		buildExp("          cx_any_exception.");
		buildExp("");
		buildExp("    CLASS-METHODS:");
		buildExp("      \" comment A");
		buildExp("      get_min_value");
		buildExp("        \" comment B");
		buildExp("        RETURNING");
		buildExp("          \" comment C");
		buildExp("          VALUE(rv_result) TYPE i");
		buildExp("        \" comment D");
		buildExp("        RAISING");
		buildExp("          \" comment E");
		buildExp("          cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testContinueLineWithComments() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);

		buildSrc("    CLASS-METHODS any_method \" comment A");
		buildSrc("      IMPORTING \" comment B");
		buildSrc("        !iv_any_param TYPE i OPTIONAL \" comment C");
		buildSrc("        !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("  \" comment D");
		buildSrc("      EXPORTING");
		buildSrc("  \" comment E");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("        !ev_other_result TYPE string");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS \" comment A");
		buildSrc("      get_max_value \" comment B");
		buildSrc("        RETURNING \" comment C");
		buildSrc("          VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS: \" comment A");
		buildSrc("      any_chained_method \" comment B");
		buildSrc("        IMPORTING \" comment C");
		buildSrc("          !iv_any_param TYPE i OPTIONAL \" comment D");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("* comment E");
		buildSrc("        RAISING");
		buildSrc("* comment F");
		buildSrc("          !cx_any_exception.");

		buildExp("    CLASS-METHODS any_method \" comment A");
		buildExp("                             IMPORTING \" comment B");
		buildExp("                                       !iv_any_param    TYPE i      OPTIONAL \" comment C");
		buildExp("                                       !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                             \" comment D");
		buildExp("                             EXPORTING");
		buildExp("                                       \" comment E");
		buildExp("                                       !ev_any_result   TYPE i");
		buildExp("                                       !ev_other_result TYPE string");
		buildExp("                             RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS \" comment A");
		buildExp("            get_max_value \" comment B");
		buildExp("                          RETURNING \" comment C");
		buildExp("                                    VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS: \" comment A");
		buildExp("             any_chained_method \" comment B");
		buildExp("                                IMPORTING \" comment C");
		buildExp("                                          !iv_any_param   TYPE i      OPTIONAL \" comment D");
		buildExp("                                          !iv_other_param TYPE string DEFAULT 'abc'");
		buildExp("* comment E");
		buildExp("                                RAISING");
		buildExp("* comment F");
		buildExp("                                          !cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOneLinersAfterChainWithComment() {
		// ensure that chained one-liners are aligned even if METHODS: is followed by a line-end comment 
		// (i.e. if, strictly speaking, the first declaration is not a one-liner)

		buildSrc("    METHODS: \" comment");
		buildSrc("      any_method RETURNING VALUE(rv_any) TYPE abap_bool,");
		buildSrc("      other_method RETURNING VALUE(rv_other) TYPE i,");
		buildSrc("      method_with_long_name RETURNING VALUE(rv_third) TYPE string.");

		buildExp("    METHODS: \" comment");
		buildExp("      any_method            RETURNING VALUE(rv_any)   TYPE abap_bool,");
		buildExp("      other_method          RETURNING VALUE(rv_other) TYPE i,");
		buildExp("      method_with_long_name RETURNING VALUE(rv_third) TYPE string.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultipleExceptionsInSameLine() {
		// ensure that multiple exception (class) names in the same line are kept, 
		// but the first of each line is nevertheless aligned
		
		buildSrc("    CLASS-METHODS any_method");
		buildSrc("    IMPORTING");
		buildSrc("    !iv_any_param TYPE i");
		buildSrc("    RAISING");
		buildSrc("    cx_any_exception RESUMABLE(cx_other_exception) cx_third_exception.");
		buildSrc("");
		buildSrc("    CLASS-METHODS other_method");
		buildSrc("    IMPORTING");
		buildSrc("    !iv_any_param TYPE i");
		buildSrc("    RAISING");
		buildSrc("    RESUMABLE(cx_any_exception) cx_other_exception");
		buildSrc("    cx_third_exception.");

		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING !iv_any_param TYPE i");
		buildExp("      RAISING   cx_any_exception RESUMABLE(cx_other_exception) cx_third_exception.");
		buildExp("");
		buildExp("    CLASS-METHODS other_method");
		buildExp("      IMPORTING !iv_any_param TYPE i");
		buildExp("      RAISING   RESUMABLE(cx_any_exception) cx_other_exception");
		buildExp("                cx_third_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testMethodsForTesting() {
		// ensure that optional additions like "FOR TESTING" keep their position (either on a new line or behind the method name)
		
 		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);

		buildSrc("    METHODS any_method");
		buildSrc("      FOR TESTING");
		buildSrc("      RAISING");
		buildSrc("        cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS: other_method FOR TESTING");
		buildSrc("        RAISING cx_any_exception,");
		buildSrc("      third_method FOR TESTING");
		buildSrc("        RAISING cx_any_exception.");

		buildExp("    METHODS any_method");
		buildExp("      FOR TESTING RAISING cx_any_exception.");
		buildExp("");
		buildExp("    METHODS: other_method FOR TESTING RAISING cx_any_exception,");
		buildExp("             third_method FOR TESTING RAISING cx_any_exception.");

		testRule();
	}

	@Test
	void testDoNotAlignClassMethodsWithMethods() {
		// expectation: CLASS-METHODS one-liners are aligned independently from the METHODS one-liners
		
		buildSrc("    CLASS-METHODS set_value_static IMPORTING !iv_new_value TYPE i.");
		buildSrc("    CLASS-METHODS get_current_value_static RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    CLASS-METHODS get_previous_value_static RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS set_value_static          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    CLASS-METHODS get_current_value_static  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    CLASS-METHODS get_previous_value_static RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testAlignConsecutiveTabular() {
		// expectation: both multi-line declarations and one-liners are aligned 

		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);

		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         EXPORTING ev_any_result TYPE i");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");

		buildExp("    METHODS other_method                IMPORTING !iv_any_param                TYPE i OPTIONAL");
		buildExp("                                                  !iv_other_param              TYPE string DEFAULT 'abc'");
		buildExp("                                        EXPORTING !ev_any_result               TYPE i");
		buildExp("                                                  !ev_other_result             TYPE string");
		buildExp("                                        RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value                   IMPORTING !iv_new_value                TYPE i.");
		buildExp("    METHODS get_current_value           RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("    METHODS get_previous_value          RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        EXPORTING ev_any_result                TYPE i");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignConsecutiveTabularAcrossComment() {
		// expectation: both multi-line declarations and one-liners are aligned, even though there are comments between them

		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);
		rule.configAlignAcrossCommentLines.setValue(true);

		buildSrc("CLASS cl_any_class DEFINITION FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    \" comment on other method");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    \" comment on one-liners");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \"! comment on third method");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         EXPORTING ev_any_result TYPE i");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION FINAL.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    \" comment on other method");
		buildExp("    METHODS other_method                IMPORTING !iv_any_param                TYPE i OPTIONAL");
		buildExp("                                                  !iv_other_param              TYPE string DEFAULT 'abc'");
		buildExp("                                        EXPORTING !ev_any_result               TYPE i");
		buildExp("                                                  !ev_other_result             TYPE string");
		buildExp("                                        RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    \" comment on one-liners");
		buildExp("    METHODS set_value                   IMPORTING !iv_new_value                TYPE i.");
		buildExp("    METHODS get_current_value           RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("    METHODS get_previous_value          RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("");
		buildExp("    \"! comment on third method");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        EXPORTING ev_any_result                TYPE i");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testCreateTabularAndAlignAll() {
		// expectation: tabular format is introduced and all declarations - one-liners, multi-liners, chains - are aligned 

		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);

		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("          !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");
		buildSrc("");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    METHODS  get_max_value              RETURNING VALUE(rv_result)  TYPE i.");
		buildExp("");
		buildExp("    METHODS: any_chained_method         IMPORTING !iv_any_param     TYPE i OPTIONAL");
		buildExp("                                                  !iv_other_param   TYPE string DEFAULT 'abc'");
		buildExp("                                        RAISING   !cx_any_exception,");
		buildExp("");
		buildExp("             other_chained_method       IMPORTING !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("                                                  !iv_name          TYPE string");
		buildExp("                                        CHANGING  !cts_result_table TYPE ty_ts_any.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value     TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result)  TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result)  TYPE i,");
		buildExp("             get_max_value_chained      RETURNING VALUE(rv_result)  TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSeparateWithEmptyLine() {
		// expectation: 
		// - multi-line declarations get an empty line above and below,
		// - however, no empty line is added at the beginning (following PUBLIC SECTION) and none at the end (before ENDCLASS) 
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("    METHODS other_method EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS:");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !iv_any_param  TYPE i OPTIONAL");
		buildExp("      EXPORTING");
		buildExp("        !ev_any_result TYPE i");
		buildExp("      RAISING");
		buildExp("        !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS other_method EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS set_value         IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !iv_any_param TYPE i OPTIONAL");
		buildExp("        RAISING");
		buildExp("          !cx_any_exception,");
		buildExp("");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("          !iv_name          TYPE string");
		buildExp("        CHANGING");
		buildExp("          !cts_result_table TYPE ty_ts_any.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSeparateForTestingWithEmptyLine() {
		// expectation: 
		// - multi-line declarations get an empty line above and below,
		// - but FOR TESTING methods do NOT get an empty line, even if they are multi-line 
		//   (because the AlignMethodsForTestingRule may still align them into tabular layout)
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("    METHODS other_method EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("    METHODS any_test_method");
		buildSrc("      FOR TESTING.");
		buildSrc("    METHODS other_test_method");
		buildSrc("      FOR TESTING RAISING cx_any_exception.");
		buildSrc("    METHODS third_test_method");
		buildSrc("      FOR TESTING.");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    METHODS other_method EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS any_test_method");
		buildExp("      FOR TESTING.");
		buildExp("    METHODS other_test_method");
		buildExp("      FOR TESTING RAISING cx_any_exception.");
		buildExp("    METHODS third_test_method");
		buildExp("      FOR TESTING.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSeparateRedefinitionWithEmptyLine() {
		// expectation: 
		// - multi-line declarations get an empty line above and below,
		// - but [FINAL] REDEFINITION methods do NOT get an empty line, even if they are multi-line 
		//   (because the AlignMethodsRedefinitionRule may still align them into tabular layout)
		
		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("    METHODS other_method EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("    METHODS any_redefined_method");
		buildSrc("      REDEFINITION.");
		buildSrc("    METHODS other_redefined_method");
		buildSrc("      FINAL REDEFINITION.");
		buildSrc("    METHODS third_redefined_method");
		buildSrc("      REDEFINITION.");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");

		buildExp("    METHODS other_method EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS any_redefined_method");
		buildExp("      REDEFINITION.");
		buildExp("    METHODS other_redefined_method");
		buildExp("      FINAL REDEFINITION.");
		buildExp("    METHODS third_redefined_method");
		buildExp("      REDEFINITION.");
		buildExp("");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSeparateWithEmptyLineBeforeComment() {
		// expectation: 
		// - multi-line declarations get an empty line above and below the related comment line(s) that are attached to them
		//   (whether ABAP Doc, asterisk, or quotation mark)  

		rule.configContinueAfterAccess.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossCommentLines.setValue(true);

		buildSrc("    \"! ABAP Doc comment");
		buildSrc("    CLASS-METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_param TYPE i OPTIONAL");
		buildSrc("      EXPORTING");
		buildSrc("        !ev_any_result TYPE i");
		buildSrc("      RAISING");
		buildSrc("        !cx_any_exception.");
		buildSrc("*   asterisk comment");
		buildSrc("*   more asterisk comment");
		buildSrc("    METHODS other_method EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("    \"! ABAP Doc comment");
		buildSrc("    \"! more ABAP Doc comment");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    \"! ABAP Doc comment");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    \" normal comment which is NOT attached to the next method");
		buildSrc("");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         \" comment on parameter");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");
		buildSrc("    \" normal comment");
		buildSrc("    METHODS get_max_value");
		buildSrc("      RETURNING");
		buildSrc("        VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS:");
		buildSrc("      \" normal comment");
		buildSrc("      any_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !iv_any_param TYPE i OPTIONAL");
		buildSrc("        RAISING");
		buildSrc("          !cx_any_exception,");
		buildSrc("      \"! ABAP Doc comment");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING");
		buildSrc("          !it_source_table TYPE ty_tt_any OPTIONAL");
		buildSrc("          !iv_name TYPE string");
		buildSrc("        CHANGING");
		buildSrc("          !cts_result_table TYPE ty_ts_any.");
		buildSrc("    METHODS: set_value_chained IMPORTING !iv_new_value TYPE i,");
		buildSrc("      get_current_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("      get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildSrc("*     asterisk comment");
		buildSrc("      get_max_value_chained");
		buildSrc("        RETURNING");
		buildSrc("          VALUE(rv_result) TYPE i.");

		buildExp("    \"! ABAP Doc comment");
		buildExp("    CLASS-METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !iv_any_param  TYPE i OPTIONAL");
		buildExp("      EXPORTING");
		buildExp("        !ev_any_result TYPE i");
		buildExp("      RAISING");
		buildExp("        !cx_any_exception.");
		buildExp("");
		buildExp("*   asterisk comment");
		buildExp("*   more asterisk comment");
		buildExp("    METHODS other_method EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    \"! ABAP Doc comment");
		buildExp("    \"! more ABAP Doc comment");
		buildExp("    METHODS set_value         IMPORTING !iv_new_value    TYPE i.");
		buildExp("    \"! ABAP Doc comment");
		buildExp("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    \" normal comment which is NOT attached to the next method");
		buildExp("");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        \" comment on parameter");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");
		buildExp("");
		buildExp("    \" normal comment");
		buildExp("    METHODS get_max_value");
		buildExp("      RETURNING");
		buildExp("        VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \" normal comment");
		buildExp("      any_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !iv_any_param TYPE i OPTIONAL");
		buildExp("        RAISING");
		buildExp("          !cx_any_exception,");
		buildExp("");
		buildExp("      \"! ABAP Doc comment");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING");
		buildExp("          !it_source_table  TYPE ty_tt_any OPTIONAL");
		buildExp("          !iv_name          TYPE string");
		buildExp("        CHANGING");
		buildExp("          !cts_result_table TYPE ty_ts_any.");
		buildExp("");
		buildExp("    METHODS: set_value_chained          IMPORTING !iv_new_value    TYPE i,");
		buildExp("             get_current_value_chained  RETURNING VALUE(rv_result) TYPE i,");
		buildExp("             get_previous_value_chained RETURNING VALUE(rv_result) TYPE i,");
		buildExp("");
		buildExp("*     asterisk comment");
		buildExp("      get_max_value_chained");
		buildExp("        RETURNING");
		buildExp("          VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testForTestingAndRedefinitionAlignOneLiners() {
		// ensure that FOR TESTING and [FINAL] REDEFINITION is aligned

		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);

		buildSrc("    METHODS any_test_method");
		buildSrc("      FOR TESTING RAISING cx_any_exception.");
		buildSrc("    METHODS other_test_method");
		buildSrc("      FOR TESTING.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS third_test_method FOR TESTING.");
		buildSrc("    METHODS fourth_test_method");
		buildSrc("      FOR TESTING RAISING cx_other_exception.");
		buildSrc("    METHODS inherited_method REDEFINITION.");
		buildSrc("    METHODS other_inherited_method");
		buildSrc("      FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         EXPORTING ev_any_result TYPE i");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");

		buildExp("    METHODS any_test_method   FOR TESTING RAISING cx_any_exception.");
		buildExp("    METHODS other_test_method FOR TESTING.");
		buildExp("");
		buildExp("    METHODS other_method IMPORTING !iv_any_param    TYPE i      OPTIONAL");
		buildExp("                                   !iv_other_param  TYPE string DEFAULT 'abc'");
		buildExp("                         EXPORTING !ev_any_result   TYPE i");
		buildExp("                                   !ev_other_result TYPE string");
		buildExp("                         RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS third_test_method      FOR TESTING.");
		buildExp("    METHODS fourth_test_method     FOR TESTING RAISING cx_other_exception.");
		buildExp("    METHODS inherited_method       REDEFINITION.");
		buildExp("    METHODS other_inherited_method FINAL REDEFINITION.");
		buildExp("");
		buildExp("    METHODS set_value              IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value      RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value     RETURNING VALUE(rv_result) TYPE i.");
		buildExp("");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        EXPORTING ev_any_result                TYPE i");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testForTestingAndRedefinitionAlignTabular() {
		// ensure that FOR TESTING and [FINAL] REDEFINITION is aligned with IMPORTING etc. of other method definitions

		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);

		buildSrc("    METHODS any_test_method");
		buildSrc("      FOR TESTING RAISING cx_any_exception.");
		buildSrc("    METHODS other_test_method");
		buildSrc("      FOR TESTING.");
		buildSrc("");
		buildSrc("    METHODS other_method IMPORTING !iv_any_param TYPE i OPTIONAL");
		buildSrc("                                   !iv_other_param TYPE string DEFAULT 'abc'");
		buildSrc("                         EXPORTING !ev_any_result TYPE i");
		buildSrc("                                   !ev_other_result TYPE string");
		buildSrc("                         RAISING !cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS third_test_method FOR TESTING.");
		buildSrc("    METHODS fourth_test_method");
		buildSrc("      FOR TESTING RAISING cx_other_exception.");
		buildSrc("    METHODS inherited_method REDEFINITION.");
		buildSrc("    METHODS other_inherited_method");
		buildSrc("      FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name TYPE i");
		buildSrc("                         EXPORTING ev_any_result TYPE i");
		buildSrc("                         CHANGING ets_any_table_with_long_name TYPE ty_ts_table");
		buildSrc("                         RAISING cx_any_exception.");

		buildExp("    METHODS any_test_method             FOR TESTING RAISING cx_any_exception.");
		buildExp("    METHODS other_test_method           FOR TESTING.");
		buildExp("");
		buildExp("    METHODS other_method                IMPORTING !iv_any_param                TYPE i OPTIONAL");
		buildExp("                                                  !iv_other_param              TYPE string DEFAULT 'abc'");
		buildExp("                                        EXPORTING !ev_any_result               TYPE i");
		buildExp("                                                  !ev_other_result             TYPE string");
		buildExp("                                        RAISING   !cx_any_exception.");
		buildExp("");
		buildExp("    METHODS third_test_method           FOR TESTING.");
		buildExp("    METHODS fourth_test_method          FOR TESTING RAISING cx_other_exception.");
		buildExp("    METHODS inherited_method            REDEFINITION.");
		buildExp("    METHODS other_inherited_method      FINAL REDEFINITION.");
		buildExp("");
		buildExp("    METHODS set_value                   IMPORTING !iv_new_value                TYPE i.");
		buildExp("    METHODS get_current_value           RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("    METHODS get_previous_value          RETURNING VALUE(rv_result)             TYPE i.");
		buildExp("");
		buildExp("    METHODS third_method_with_long_name IMPORTING iv_any_param_with_long_name  TYPE i");
		buildExp("                                        EXPORTING ev_any_result                TYPE i");
		buildExp("                                        CHANGING  ets_any_table_with_long_name TYPE ty_ts_table");
		buildExp("                                        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testMethodsForEvent() {
		buildSrc("  METHODS handle_any_event");
		buildSrc("  FOR EVENT any_event OF cl_any_sender");
		buildSrc("  IMPORTING");
		buildSrc("  !any_param");
		buildSrc("    !other_param");
		buildSrc("        !sender.");
		buildSrc("");
		buildSrc("  METHODS handle_other_event");
		buildSrc("  FOR EVENT any_event OF cl_any_sender");
		buildSrc("  IMPORTING   !any_param !other_param !sender.");

		buildExp("    METHODS handle_any_event");
		buildExp("      FOR EVENT any_event OF cl_any_sender");
		buildExp("      IMPORTING !any_param");
		buildExp("                !other_param");
		buildExp("                !sender.");
		buildExp("");
		buildExp("    METHODS handle_other_event");
		buildExp("      FOR EVENT any_event OF cl_any_sender");
		buildExp("      IMPORTING !any_param !other_param !sender.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testMethodsForEventBreakAfterImporting() {
		rule.configContinueAfterAccess.setEnumValue(ChangeType.NEVER);

		buildSrc("  METHODS handle_any_event");
		buildSrc("  FOR EVENT any_event OF cl_any_sender");
		buildSrc("  IMPORTING !any_param");
		buildSrc("    !other_param");
		buildSrc("        !sender.");
		buildSrc("");
		buildSrc("  METHODS handle_other_event");
		buildSrc("  FOR EVENT other_event OF cl_any_sender");
		buildSrc("  IMPORTING !any_param !other_param !sender.");

		buildExp("    METHODS handle_any_event");
		buildExp("      FOR EVENT any_event OF cl_any_sender");
		buildExp("      IMPORTING");
		buildExp("        !any_param");
		buildExp("        !other_param");
		buildExp("        !sender.");
		buildExp("");
		buildExp("    METHODS handle_other_event");
		buildExp("      FOR EVENT other_event OF cl_any_sender");
		buildExp("      IMPORTING");
		buildExp("        !any_param !other_param !sender.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testMethodsAbstract() {
		buildSrc("  METHODS any_method");
		buildSrc("          ABSTRACT");
		buildSrc("    IMPORTING");
		buildSrc("      !iv_any TYPE i");
		buildSrc("    EXPORTING");
		buildSrc("      !ev_other TYPE i");
		buildSrc("    RAISING");
		buildSrc("      cx_any_exception.");

		buildExp("  METHODS any_method");
		buildExp("    ABSTRACT");
		buildExp("    IMPORTING !iv_any   TYPE i");
		buildExp("    EXPORTING !ev_other TYPE i");
		buildExp("    RAISING   cx_any_exception.");

		testRule();
	}

	@Test
	void testBreakAfterKeywordAlignOneLiners() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ONE_LINERS);

		buildSrc("  METHODS:");
		buildSrc("    any_method IMPORTING iv_any TYPE i,");
		buildSrc("    other_method IMPORTING iv_other TYPE i,");
		buildSrc("    third_method_with_long_name IMPORTING iv_third_param TYPE i.");
		buildSrc("");
		buildSrc("  DATA lv_any TYPE i.");

		buildExp("  METHODS:");
		buildExp("    any_method                  IMPORTING iv_any         TYPE i,");
		buildExp("    other_method                IMPORTING iv_other       TYPE i,");
		buildExp("    third_method_with_long_name IMPORTING iv_third_param TYPE i.");
		buildExp("");
		buildExp("  DATA lv_any TYPE i.");

		testRule();
	}

	@Test
	void testBreakAfterKeywordAlignAllTabular() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);
		rule.configAlignConsecutive.setEnumValue(MethodsSequenceAlignment.ALL_TABULAR);

		buildSrc("  METHODS:");
		buildSrc("    any_method IMPORTING iv_any TYPE i,");
		buildSrc("    other_method IMPORTING iv_other TYPE i,");
		buildSrc("    third_method_with_long_name IMPORTING iv_third_param TYPE i.");
		buildSrc("");
		buildSrc("  DATA lv_any TYPE i.");

		buildExp("  METHODS:");
		buildExp("    any_method                  IMPORTING iv_any         TYPE i,");
		buildExp("    other_method                IMPORTING iv_other       TYPE i,");
		buildExp("    third_method_with_long_name IMPORTING iv_third_param TYPE i.");
		buildExp("");
		buildExp("  DATA lv_any TYPE i.");

		testRule();
	}

	@Test
	void testBreakAfterKeywordButKeepOneLiners() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);

		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i.");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i.");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i.");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i.");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		testRule();
	}

	@Test
	void testFinalForEvent() {
		buildSrc("    METHODS any_method FINAL");
		buildSrc("        FOR EVENT any_event OF cl_any_class");
		buildSrc("       IMPORTING iv_any");
		buildSrc("               iv_other");
		buildSrc("      iv_third.");

		buildExp("    METHODS any_method FINAL");
		buildExp("      FOR EVENT any_event OF cl_any_class");
		buildExp("      IMPORTING iv_any");
		buildExp("                iv_other");
		buildExp("                iv_third.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFinalForEventInOwnLines() {
		buildSrc("    METHODS any_method");
		buildSrc("   FINAL");
		buildSrc("        FOR EVENT any_event OF cl_any_class");
		buildSrc("       IMPORTING iv_any");
		buildSrc("               iv_other");
		buildSrc("      iv_third.");

		buildExp("    METHODS any_method");
		buildExp("      FINAL");
		buildExp("      FOR EVENT any_event OF cl_any_class");
		buildExp("      IMPORTING iv_any");
		buildExp("                iv_other");
		buildExp("                iv_third.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultiplePeriods() {
		buildSrc("    METHODS set_value IMPORTING !iv_new_value TYPE i...");
		buildSrc("    METHODS get_current_value RETURNING VALUE(rv_result) TYPE i..");
		buildSrc("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		buildExp("    METHODS set_value          IMPORTING !iv_new_value    TYPE i...");
		buildExp("    METHODS get_current_value  RETURNING VALUE(rv_result) TYPE i..");
		buildExp("    METHODS get_previous_value RETURNING VALUE(rv_result) TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}