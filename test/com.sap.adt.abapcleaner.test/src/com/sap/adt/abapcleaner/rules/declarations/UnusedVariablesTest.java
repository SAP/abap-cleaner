package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class UnusedVariablesTest extends RuleTestBase {
	private UnusedVariablesRule rule;
	
	UnusedVariablesTest() {
		super(RuleID.UNUSED_VARIABLES);
		rule = (UnusedVariablesRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMeasureForVarsNeverUsed.setEnumValue(UnusedVariableMeasure.DELETE);
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		rule.configMeasureForAssignedVars.setEnumValue(UnusedVariableMeasureIfAssigned.ADD_TODO_COMMENT);
		rule.configMeasureForAssignedVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasureIfAssigned.ADD_TODO_COMMENT);
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
	}
	
	@Test
	void testDataDeclaration() {
		buildSrc("    DATA: lv_unused_b      TYPE string,");
		buildSrc("          lv_used_var      TYPE i,");
		buildSrc("          lv_commented_out LIKE lv_used_var.");
		buildSrc("");
		buildSrc("    CLEAR ev_count.");
		buildSrc("");
		buildSrc("    lv_used_var = 0.");
		buildSrc("    lv_used_var += 1.");
		buildSrc("    ev_count = lv_used_var.");
		buildSrc("");
		buildSrc("*    lv_commented_out = 0.");
		buildSrc("*    lv_commented_out += 1.");

		buildExp("    DATA: lv_used_var      TYPE i.");
		buildExp("*          lv_commented_out LIKE lv_used_var.");
		buildExp("");
		buildExp("    CLEAR ev_count.");
		buildExp("");
		buildExp("    lv_used_var = 0.");
		buildExp("    lv_used_var += 1.");
		buildExp("    ev_count = lv_used_var.");
		buildExp("");
		buildExp("*    lv_commented_out = 0.");
		buildExp("*    lv_commented_out += 1.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testFieldSymbolsDeclaration() {
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("      <ls_unused_a>      TYPE ty_s_any_struc,");
		buildSrc("      <ls_commented_out> LIKE LINE OF its_table,");
		buildSrc("      <ls_used>          TYPE tt_table.");
		buildSrc("");
		buildSrc("    LOOP AT its_table ASSIGNING <ls_used>.");
		buildSrc("      a += <ls_used>-component.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("*    LOOP AT its_table ASSIGNING <ls_commented_out>.");
		buildSrc("*    ENDLOOP.");

		buildExp("*    FIELD-SYMBOLS: <ls_commented_out> LIKE LINE OF its_table,");
		buildExp("    FIELD-SYMBOLS:");
		buildExp("      <ls_used>          TYPE tt_table.");
		buildExp("");
		buildExp("    LOOP AT its_table ASSIGNING <ls_used>.");
		buildExp("      a += <ls_used>-component.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("*    LOOP AT its_table ASSIGNING <ls_commented_out>.");
		buildExp("*    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMixedDeclarationChain() {
		buildSrc("    DATA: lv_unused_b      TYPE string,");
		buildSrc("          lv_used_var      TYPE i,");
		buildSrc("          lv_commented_out LIKE lv_used_var.");
		buildSrc("");
		buildSrc("    lv_used_var = 0.");
		buildSrc("    DO 5 TIMES.");
		buildSrc("      lv_used_var += 1.");
		buildSrc("    ENDDO.");
		buildSrc("    ev_count = lv_used_var.");
		buildSrc("");
		buildSrc("*    lv_commented_out = 0.");
		buildSrc("*    DO 5 TIMES.");
		buildSrc("*      lv_commented_out += 1.");
		buildSrc("*    ENDDO.");

		buildExp("    DATA: lv_used_var      TYPE i.");
		buildExp("*          lv_commented_out LIKE lv_used_var.");
		buildExp("");
		buildExp("    lv_used_var = 0.");
		buildExp("    DO 5 TIMES.");
		buildExp("      lv_used_var += 1.");
		buildExp("    ENDDO.");
		buildExp("    ev_count = lv_used_var.");
		buildExp("");
		buildExp("*    lv_commented_out = 0.");
		buildExp("*    DO 5 TIMES.");
		buildExp("*      lv_commented_out += 1.");
		buildExp("*    ENDDO.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMentioningInTextComment() {
		buildSrc("    DATA lv_unused_a TYPE i.");
		buildSrc("*    this comment mentions the variable lv_unused_a, but that does not count:");
		buildSrc("*    only variables in commented-out ABAP code count, not variables in text comments!");

		buildExp("*    this comment mentions the variable lv_unused_a, but that does not count:");
		buildExp("*    only variables in commented-out ABAP code count, not variables in text comments!");

		putAnyMethodAroundSrcAndExp();

		deactivateRuleUseCheck(); // TODO: since the first line is simply deleted, there is currently no way to log the rule use 
		testRule();
	}

	@Test
	void testUnusedWithNeededPragma() {
		// expect that ##NEEDED is correctly identified for all variables and nothing is changed
		buildSrc("    DATA lv_unused_but_needed TYPE string ##NEEDED.");
		buildSrc("    DATA lv_unused_but_needed_2 ##NEEDED TYPE string.");
		buildSrc("    DATA ##NEEDED:");
		buildSrc("      lv_unused_but_needed_3 TYPE string,");
		buildSrc("      lv_unused_but_needed_4 TYPE string.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUnusedWithNeededPseudoComment() {
		// expect that "#EC NEEDED is correctly identified for all variables and nothing is changed; 
		// "#EC NEEDED is valid for all elements of a chain if it is before, or in any of the comments following the chain colon
		buildSrc("    DATA lv_unused_but_needed TYPE string. \"#EC NEEDED");
		buildSrc("    DATA lv_unused_but_needed_2 \"#EC NEEDED");
		buildSrc("         TYPE string.");
		buildSrc("    DATA: \"#EC NEEDED");
		buildSrc("      lv_unused_but_needed_3 TYPE string,");
		buildSrc("      lv_unused_but_needed_4 TYPE string.");
		buildSrc("    DATA:");
		buildSrc("      \" other comment");
		buildSrc("      \"#EC NEEDED");
		buildSrc("      lv_unused_but_needed_5 TYPE string,");
		buildSrc("      lv_unused_but_needed_6 TYPE string.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedInDeclarationChain() {
		buildSrc("    DATA: lv_only_assigned TYPE i,");
		buildSrc("          lv_assigned_but_used_incomment TYPE i,");
		buildSrc("          lv_unused_but_needed TYPE string ##NEEDED.");
		buildSrc("");
		buildSrc("    CLEAR lv_only_assigned.");
		buildSrc("    lv_only_assigned = lv_only_assigned + 2.");
		buildSrc("    lv_assigned_but_used_incomment = 1.");
		buildSrc("");
		buildSrc("*    rv_result = lv_assigned_but_used_incomment.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA: lv_only_assigned TYPE i,");
		buildExp("          \" TODO: variable is assigned but only used in commented-out code (ABAP cleaner)");
		buildExp("          lv_assigned_but_used_incomment TYPE i,");
		buildExp("          lv_unused_but_needed TYPE string ##NEEDED.");
		buildExp("");
		buildExp("    CLEAR lv_only_assigned.");
		buildExp("    lv_only_assigned = lv_only_assigned + 2.");
		buildExp("    lv_assigned_but_used_incomment = 1.");
		buildExp("");
		buildExp("*    rv_result = lv_assigned_but_used_incomment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUnusedAndOnlyAssignedInChain() {
		buildSrc("    DATA: lv_unused TYPE i,");
		buildSrc("          lv_only_assigned TYPE i,");
		buildSrc("          lv_unused_but_needed TYPE string ##NEEDED,");
		buildSrc("          lv_assigned_but_used_incomment TYPE i.");
		buildSrc("");
		buildSrc("    CLEAR lv_only_assigned.");
		buildSrc("    lv_only_assigned = lv_only_assigned + 2.");
		buildSrc("    lv_assigned_but_used_incomment = 1.");
		buildSrc("");
		buildSrc("*    rv_result = lv_assigned_but_used_incomment.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA: lv_only_assigned TYPE i,");
		buildExp("          lv_unused_but_needed TYPE string ##NEEDED,");
		buildExp("          \" TODO: variable is assigned but only used in commented-out code (ABAP cleaner)");
		buildExp("          lv_assigned_but_used_incomment TYPE i.");
		buildExp("");
		buildExp("    CLEAR lv_only_assigned.");
		buildExp("    lv_only_assigned = lv_only_assigned + 2.");
		buildExp("    lv_assigned_but_used_incomment = 1.");
		buildExp("");
		buildExp("*    rv_result = lv_assigned_but_used_incomment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedInImporting() {
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           ev_result2 = DATA(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_result1.");

		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildExp("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("                           ev_result2 = DATA(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_result1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedFinalInImporting() {
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = FINAL(lv_result1)");
		buildSrc("                           ev_result2 = FINAL(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_result1.");

		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = FINAL(lv_result1)");
		buildExp("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("                           ev_result2 = FINAL(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_result1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedInCatch() {
		// for assignments in CATCH statements, the comment cannot be added as the 'previous sibling' (because that must be the 'TRY'), 
		// therefore expect it to be added at the end of the CATCH statement
		
		buildSrc("    TRY.");
		buildSrc("        \" do something");
		buildSrc("      CATCH cx_any_error INTO DATA(lo_exp).");
		buildSrc("        \" do nothing with the error object");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        \" do something");
		buildExp("      CATCH cx_any_error INTO DATA(lo_exp). \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("        \" do nothing with the error object");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedToDoAlreadyInserted() {
		// prepare a code in which the to-do message 'variable is assigned but never used' was already inserted by a 
		// previous cleanup run in two cases (but is missing in a third case)
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    DATA(lv_value) = 1.");
		buildSrc("    DATA(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("                           ev_result2 = DATA(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_result1.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA(lv_value) = 1.");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA(lv_other_value) = 2.");
		buildExp("");
		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildExp("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("                           ev_result2 = DATA(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_result1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnlyAssignedFinalToDoAlreadyInserted() {
		// prepare a code in which the to-do message 'variable is assigned but never used' was already inserted by a 
		// previous cleanup run in two cases (but is missing in a third case)
		
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    FINAL(lv_value) = 1.");
		buildSrc("    FINAL(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("                           ev_result2 = DATA(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_result1.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    FINAL(lv_value) = 1.");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    FINAL(lv_other_value) = 2.");
		buildExp("");
		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildExp("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("                           ev_result2 = DATA(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_result1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUnusedDeclChainWithAsteriskLines() {
		// test a chain that already contains commented-out lines, which requires special care
		
		buildSrc("    DATA: lo_any_instance TYPE REF TO if_any_interface,");
		buildSrc("*          lo_item_exp     TYPE REF TO cl_item,");
		buildSrc("*          lo_item_act     TYPE REF TO cl_item,");
		buildSrc("          lo_order_exp    TYPE REF TO cl_order,");
		buildSrc("          lo_order_act    TYPE REF TO cl_order.");

		buildExp("*          lo_item_exp     TYPE REF TO cl_item,");
		buildExp("*          lo_item_act     TYPE REF TO cl_item,");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAssignedButUsedInComment() {
		buildSrc("    DATA lv_assigned_but_used_incomment TYPE i.");
		buildSrc("");
		buildSrc("    lv_assigned_but_used_incomment = 1.");
		buildSrc("");
		buildSrc("*    DO 5 * lv_assigned_but_used_incomment TIMES.");
		buildSrc("*      \" comment");
		buildSrc("*    ENDDO.");

		buildExp("    \" TODO: variable is assigned but only used in commented-out code (ABAP cleaner)");
		buildExp("    DATA lv_assigned_but_used_incomment TYPE i.");
		buildExp("");
		buildExp("    lv_assigned_but_used_incomment = 1.");
		buildExp("");
		buildExp("*    DO 5 * lv_assigned_but_used_incomment TIMES.");
		buildExp("*      \" comment");
		buildExp("*    ENDDO.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsedOnlyInSelfAssignment() {
		buildSrc("    DATA lv_only_assigned TYPE i.");
		buildSrc("");
		buildSrc("    \" lv_only_assigned is only used to modifying its own value");
		buildSrc("    CLEAR lv_only_assigned.");
		buildSrc("    lv_only_assigned = lv_only_assigned + 2.");
		buildSrc("    MULTIPLY lv_only_assigned BY 2.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_only_assigned TYPE i.");
		buildExp("");
		buildExp("    \" lv_only_assigned is only used to modifying its own value");
		buildExp("    CLEAR lv_only_assigned.");
		buildExp("    lv_only_assigned = lv_only_assigned + 2.");
		buildExp("    MULTIPLY lv_only_assigned BY 2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsedOnlyInSelfAssignmentWithOffset() {
		buildSrc("    DATA lv_only_assigned TYPE i.");
		buildSrc("");
		buildSrc("    \" lv_only_assigned is only used to modify its own value");
		buildSrc("    lv_only_assigned+4 = '10'.");
		buildSrc("    lv_only_assigned+2(2) = '10'.");
		buildSrc("    lv_only_assigned+4(2) = lv_only_assigned+2(2).");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_only_assigned TYPE i.");
		buildExp("");
		buildExp("    \" lv_only_assigned is only used to modify its own value");
		buildExp("    lv_only_assigned+4 = '10'.");
		buildExp("    lv_only_assigned+2(2) = '10'.");
		buildExp("    lv_only_assigned+4(2) = lv_only_assigned+2(2).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsedOnlyInSelfAssignmentWithComponent() {
		buildSrc("    DATA ls_only_assigned TYPE ty_ts_struc.");
		buildSrc("");
		buildSrc("    \" ls_only_assigned is only used to modifying its own components");
		buildSrc("    ls_only_assigned-component_a = 5.");
		buildSrc("    ls_only_assigned-component_b = ls_only_assigned-component_a.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA ls_only_assigned TYPE ty_ts_struc.");
		buildExp("");
		buildExp("    \" ls_only_assigned is only used to modifying its own components");
		buildExp("    ls_only_assigned-component_a = 5.");
		buildExp("    ls_only_assigned-component_b = ls_only_assigned-component_a.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAssignedWithSideEffects() {
		// test a variable that is assigned but unused, where, however, the call may have side effects

		buildSrc("    DATA(lo_unused_util) = get_utility( ).");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA(lo_unused_util) = get_utility( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAssignedFinalWithSideEffects() {
		// test a variable that is assigned but unused, where, however, the call may have side effects
		buildSrc("    FINAL(lo_unused_util) = get_utility( ).");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    FINAL(lo_unused_util) = get_utility( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUnusedDeclChainWithComment() {
		buildSrc("    \" comments between chain colon : and identifier:");
		buildSrc("    FIELD-SYMBOLS: \" comment");
		buildSrc("      <ls_item> LIKE LINE OF its_item.");

		buildExp("    \" comments between chain colon : and identifier:");

		putAnyMethodAroundSrcAndExp();
		
		deactivateRuleUseCheck(); // TODO: since the first line is simply deleted, there is currently no way to log the rule use
		testRule();
	}

	@Test
	void testUnusedDeclWithFullLineComment() {
		buildSrc("    \" only the full-line comment shall remain here:");
		buildSrc("    FIELD-SYMBOLS: \" comment above the first identifier");
		buildSrc("      <ls_item>  LIKE LINE OF its_item, \" comment");
		buildSrc("      \" full-line comment within the chain");
		buildSrc("      <ls_order> LIKE LINE OF mt_order.");

		buildExp("    \" only the full-line comment shall remain here:");
		buildExp("    \" full-line comment within the chain");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDeclarationWithLengthInfo() {
		// test identifiers with length information in parentheses
		
		buildSrc("    DATA lv_textdat1(20) TYPE c.");
		buildSrc("    DATA lv_textdat2(20) TYPE c.");
		buildSrc("    WRITE: iv_date1 TO lv_textdat1,");
		buildSrc("           iv_date2 TO lv_textdat2.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_textdat1(20) TYPE c.");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_textdat2(20) TYPE c.");
		buildExp("    WRITE: iv_date1 TO lv_textdat1,");
		buildExp("           iv_date2 TO lv_textdat2.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsageWithOffsetAndLength() {
		// test identifiers that are used with 'identifier+offset(length)'

		buildSrc("    DATA lv_date TYPE ty_date.");
		buildSrc("*    ev_month = lv_date+4(2).");

		buildExp("*    DATA lv_date TYPE ty_date.");
		buildExp("*    ev_month = lv_date+4(2).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsageAsOffsetOrLength() {
		buildSrc("    DATA lv_offset_a TYPE i.");
		buildSrc("    DATA lv_offset_b TYPE i.");
		buildSrc("    DATA lv_length_a TYPE i.");
		buildSrc("    DATA lv_length_b TYPE i.");
		buildSrc("");
		buildSrc("    lv_offset_a = 1.");
		buildSrc("    lv_offset_b = 2.");
		buildSrc("    lv_length_a = 3.");
		buildSrc("    lv_length_b = 4.");
		buildSrc("");
		buildSrc("    ev_result+lv_offset_a(lv_length_a) = iv_value+lv_offset_b(lv_length_b).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsageInSqlStatement() {
		// ensure that @identifiers in SQL statements are correctly identified
		buildSrc("    DATA lv_value TYPE dtab-any_field.");
		buildSrc("    DATA ls_item  TYPE ty_s_item.");
		buildSrc("");
		buildSrc("    SELECT SINGLE any_field");
		buildSrc("      FROM dtab");
		buildSrc("      WHERE other_field = @ls_item-other_field");
		buildSrc("      INTO @lv_value.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_value TYPE dtab-any_field.");
		buildExp("    DATA ls_item  TYPE ty_s_item.");
		buildExp("");
		buildExp("    SELECT SINGLE any_field");
		buildExp("      FROM dtab");
		buildExp("      WHERE other_field = @ls_item-other_field");
		buildExp("      INTO @lv_value.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFieldAccessOnObject() {
		buildSrc("    \" lo_instance must NOT be marked as 'only assigned to', because the expression ...->mth_item is using it:");
		buildSrc("    DATA lo_instance TYPE REF TO cl_any_class.");
		buildSrc("    lo_instance ?= mo_instance.");
		buildSrc("    lo_instance->mth_item = VALUE #( ( item_id = ls_struc-item_id");
		buildSrc("                                       item    = lo_item ) ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testConstants() {
		buildSrc("    CONSTANTS lc_unused        TYPE i VALUE 1200.");
		buildSrc("    CONSTANTS lc_commented_out TYPE i VALUE 2.");
		buildSrc("    CONSTANTS lc_used_constant TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    DO lc_used_constant TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("*    DO lc_commented_out * 5 TIMES.");
		buildSrc("*      \" do something");
		buildSrc("*    ENDDO.");

		buildExp("*    CONSTANTS lc_unused        TYPE i VALUE 1200.");
		buildExp("*    CONSTANTS lc_commented_out TYPE i VALUE 2.");
		buildExp("    CONSTANTS lc_used_constant TYPE i VALUE 3.");
		buildExp("");
		buildExp("    DO lc_used_constant TIMES.");
		buildExp("      \" do something");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("*    DO lc_commented_out * 5 TIMES.");
		buildExp("*      \" do something");
		buildExp("*    ENDDO.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// configuration settings for unused variables 

	@Test
	void testCommentOutUnusedVarWithAsterisk() {
		rule.configMeasureForVarsNeverUsed.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		
		buildSrc("    DATA lv_unused TYPE string.");

		buildExp("*    DATA lv_unused TYPE string.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentOutUnusedVarWithQuotMark() {
		rule.configMeasureForVarsNeverUsed.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_QUOT);
		
		buildSrc("    DATA lv_unused TYPE string.");

		buildExp("    \" DATA lv_unused TYPE string.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAddTodoCommentForUnusedVar() {
		rule.configMeasureForVarsNeverUsed.setEnumValue(UnusedVariableMeasure.ADD_TODO_COMMENT);

		buildSrc("    DATA lv_unused TYPE string.");

		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    DATA lv_unused TYPE string.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIgnoreUnusedVar() {
		rule.configMeasureForVarsNeverUsed.setEnumValue(UnusedVariableMeasure.IGNORE);

		buildSrc("    DATA lv_unused TYPE string.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// configuration settings for variables only used in comments

	@Test
	void testDeleteVarOnlyUsedInComment() {
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.DELETE);
		
		buildSrc("    DATA lv_only_used_in_comment TYPE string.");
		buildSrc("*    rv_result = lv_only_used_in_comment.");

		buildExp("*    rv_result = lv_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();

		deactivateRuleUseCheck(); // TODO: the rule use is stored on the deleted Command and therefore not detected
		
		testRule();
	}

	@Test
	void testCommentOutVarOnlyUsedInCommentWithQuot() {
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_QUOT);
		
		buildSrc("    DATA lv_only_used_in_comment TYPE string.");
		buildSrc("*    lv_only_used_in_comment = 10.");
		buildSrc("*    lv_only_used_in_comment += 1.");
		buildSrc("*    rv_result = lv_only_used_in_comment.");

		buildExp("    \" DATA lv_only_used_in_comment TYPE string.");
		buildExp("*    lv_only_used_in_comment = 10.");
		buildExp("*    lv_only_used_in_comment += 1.");
		buildExp("*    rv_result = lv_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentOutVarOnlyUsedInCommentWithAsterisk() {
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		
		buildSrc("    DATA lv_only_used_in_comment TYPE string.");
		buildSrc("*    rv_result = lv_only_used_in_comment.");

		buildExp("*    DATA lv_only_used_in_comment TYPE string.");
		buildExp("*    rv_result = lv_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAddTodoCommentForVarOnlyUsedInComment() {
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.ADD_TODO_COMMENT);

		buildSrc("    DATA lv_only_used_in_comment TYPE i.");
		buildSrc("*    rv_result = lv_only_used_in_comment.");

		buildExp("    \" TODO: variable is only used in commented-out code (ABAP cleaner)");
		buildExp("    DATA lv_only_used_in_comment TYPE i.");
		buildExp("*    rv_result = lv_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIgnoreVarOnlyUsedInComment() {
		rule.configMeasureForVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.IGNORE);

		buildSrc("    DATA lv_only_used_in_comment TYPE i.");
		buildSrc("*    rv_result = lv_only_used_in_comment.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// configuration settings for unused constants

	@Test
	void testDeleteUnusedConstant() {
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.DELETE);
		
		buildSrc("    CONSTANTS lc_unused TYPE i VALUE 42.");
		buildSrc("    \" do something");

		buildExp("    \" do something");

		putAnyMethodAroundSrcAndExp();
		
		deactivateRuleUseCheck(); // TODO: the rule use is stored on the deleted Command and therefore not detected

		testRule();
	}

	@Test
	void testCommentOutUnusedConstantWithAsterisk() {
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		
		buildSrc("    CONSTANTS lc_unused TYPE i VALUE 42.");

		buildExp("*    CONSTANTS lc_unused TYPE i VALUE 42.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentOutUnusedConstantWithQuotMark() {
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_QUOT);
		
		buildSrc("    CONSTANTS lc_unused TYPE i VALUE 42.");

		buildExp("    \" CONSTANTS lc_unused TYPE i VALUE 42.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAddTodoCommentForUnusedConstant() {
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.ADD_TODO_COMMENT);

		buildSrc("    CONSTANTS lc_unused TYPE i VALUE 42.");

		buildExp("    \" TODO: constant is never used (ABAP cleaner)");
		buildExp("    CONSTANTS lc_unused TYPE i VALUE 42.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIgnoreUnusedConstant() {
		rule.configMeasureForConstantsNeverUsed.setEnumValue(UnusedVariableMeasure.IGNORE);

		buildSrc("    CONSTANTS lc_unused TYPE i VALUE 42.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// configuration settings for constants only used in comments

	@Test
	void testDeleteConstantOnlyUsedInComment() {
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.DELETE);
		
		buildSrc("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildSrc("*    rv_result = lc_only_used_in_comment.");

		buildExp("*    rv_result = lc_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();

		deactivateRuleUseCheck(); // TODO: the rule use is stored on the deleted Command and therefore not detected
		
		testRule();
	}

	@Test
	void testCommentOutConstantOnlyUsedInCommentWithQuot() {
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_QUOT);
		
		buildSrc("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildSrc("*    rv_result = lc_only_used_in_comment.");

		buildExp("    \" CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildExp("*    rv_result = lc_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentOutConstantOnlyUsedInCommentWithAsterisk() {
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
		
		buildSrc("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildSrc("*    rv_result = lc_only_used_in_comment.");

		buildExp("*    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildExp("*    rv_result = lc_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAddTodoCommentForConstantOnlyUsedInComment() {
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.ADD_TODO_COMMENT);

		buildSrc("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildSrc("*    rv_result = lc_only_used_in_comment.");

		buildExp("    \" TODO: constant is only used in commented-out code (ABAP cleaner)");
		buildExp("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildExp("*    rv_result = lc_only_used_in_comment.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIgnoreConstantOnlyUsedInComment() {
		rule.configMeasureForConstantsOnlyUsedInComment.setEnumValue(UnusedVariableMeasure.IGNORE);

		buildSrc("    CONSTANTS lc_only_used_in_comment TYPE string VALUE 'abc'.");
		buildSrc("*    rv_result = lc_only_used_in_comment.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// other configuration settings 

	@Test
	void testIgnoreAssigned() {
		rule.configMeasureForAssignedVars.setEnumValue(UnusedVariableMeasureIfAssigned.IGNORE);

		buildSrc("    DATA: lv_only_assigned TYPE i,");
		buildSrc("          lv_unused_but_needed TYPE string ##NEEDED.");
		buildSrc("");
		buildSrc("    lv_only_assigned = 1.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIgnoreAssignedOnlyUsedInComment() {
		rule.configMeasureForAssignedVarsOnlyUsedInComment.setEnumValue(UnusedVariableMeasureIfAssigned.IGNORE);

		buildSrc("    DATA: lv_assigned_but_used_incomment TYPE i,");
		buildSrc("          lv_unused_but_needed TYPE string ##NEEDED.");
		buildSrc("");
		buildSrc("    lv_assigned_but_used_incomment = 1.");
		buildSrc("");
		buildSrc("*    rv_result = lv_assigned_but_used_incomment.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMessageIntoUnusedData() {
		buildSrc("    MESSAGE e111(any_msg_class) WITH iv_item_id INTO DATA(lv_msg).");
		buildSrc("    mo_message_handler->add_symessage( iv_ctx_type  = if_any_interface=>co_context_item_id");
		buildSrc("                                       iv_ctx_value = is_item-item_id ).");

		buildExp("    \" TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)");
		buildExp("    MESSAGE e111(any_msg_class) WITH iv_item_id INTO DATA(lv_msg).");
		buildExp("    mo_message_handler->add_symessage( iv_ctx_type  = if_any_interface=>co_context_item_id");
		buildExp("                                       iv_ctx_value = is_item-item_id ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMessageIntoUnusedFinal() {
		buildSrc("    MESSAGE e111(any_msg_class) WITH iv_item_id INTO FINAL(lv_msg).");
		buildSrc("    mo_message_handler->add_symessage( iv_ctx_type  = if_any_interface=>co_context_item_id");
		buildSrc("                                       iv_ctx_value = is_item-item_id ).");

		buildExp("    \" TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)");
		buildExp("    MESSAGE e111(any_msg_class) WITH iv_item_id INTO FINAL(lv_msg).");
		buildExp("    mo_message_handler->add_symessage( iv_ctx_type  = if_any_interface=>co_context_item_id");
		buildExp("                                       iv_ctx_value = is_item-item_id ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testConstantUsedAsLength() {
		buildSrc("    CONSTANTS lc_length TYPE i VALUE 2.");
		buildSrc("    lv_result = iv_text+4(lc_length).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testVarUsedInSelect() {
		buildSrc("    DATA lv_company_code TYPE bukrs.");
		buildSrc("    lv_company_code = 'XYZ'.");
		buildSrc("    SELECT DISTINCT item_id");
		buildSrc("           INTO TABLE @ets_item_id");
		buildSrc("           FROM dtab");
		buildSrc("           WHERE company_code = @lv_company_code.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testVarUsedInExecSql() {
		// Expect the code to remain unchanged, specifically: 
		// - expect the first variable to NOT be reported as 'assigned but never used' and 
		// - expect the second variable to NOT be removed as 'unused'.
		// This requires the EXEC SQL section (which accesses these variables as host variables) to be considered. 

		buildSrc("    DATA lv_company_code TYPE bukrs.");
		buildSrc("    DATA lv_from_year    TYPE gjahr.");
		buildSrc("    lv_company_code = 'XYZ'.");
		buildSrc("    EXEC SQL.");
		buildSrc("      SELECT from_year");
		buildSrc("        FROM dtab");
		buildSrc("        INTO :lv_from_year");
		buildSrc("        WHERE bukrs = :lv_company_code");
		buildSrc("    ENDEXEC.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testClassInReportBeforeStartOfSelection() {
		buildSrc("REPORT ztest.");
		buildSrc("");
		buildSrc("CLASS lcl_main DEFINITION FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS constructor.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_test TYPE abap_bool.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_main IMPLEMENTATION.");
		buildSrc("  METHOD constructor.");
		buildSrc("    mv_test = abap_true.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  PERFORM main.");
		buildSrc("");
		buildSrc("FORM main.");
		buildSrc("  NEW lcl_main( ).");
		buildSrc("ENDFORM.");
		
		copyExpFromSrc();

		testRule();
	}

	@Test
	void testClassInReportAfterStartOfSelection() {
		buildSrc("REPORT ztest.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  PERFORM main.");
		buildSrc("");
		buildSrc("CLASS lcl_main DEFINITION FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS constructor.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_test TYPE abap_bool.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_main IMPLEMENTATION.");
		buildSrc("  METHOD constructor.");
		buildSrc("    mv_test = abap_true.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("FORM main.");
		buildSrc("  NEW lcl_main( ).");
		buildSrc("ENDFORM.");
		
		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAtSelectionScreen() {
		// Ensure that the first "AT SELECTION-SCREEN" is correctly identified as a levelOpener, despite the comments between
		// the keywords (and despite the fact that "AT" is also a levelOpener, which would require an "ENDAT"!)
		// Only then the first lv_var will be correctly identified as "assigned but never used".
		
		buildSrc("REPORT ztest.");
		buildSrc("");
		buildSrc("AT \" comment");
		buildSrc("* comment");
		buildSrc("SELECTION-SCREEN.");
		buildSrc("  DATA lv_var TYPE i.");
		buildSrc("  lv_var = 1.");
		buildSrc("");
		buildSrc("AT SELECTION-SCREEN.");
		buildSrc("  DATA lv_var TYPE i.");
		buildSrc("  gv_var = lv_var.");
		
		buildExp("REPORT ztest.");
		buildExp("");
		buildExp("AT \" comment");
		buildExp("* comment");
		buildExp("SELECTION-SCREEN.");
		buildExp("  \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("  DATA lv_var TYPE i.");
		buildExp("  lv_var = 1.");
		buildExp("");
		buildExp("AT SELECTION-SCREEN.");
		buildExp("  DATA lv_var TYPE i.");
		buildExp("  gv_var = lv_var.");

		testRule();
	}

	@Test
	void testFieldSymbolAssignedButUnused() {
		buildSrc("    FIELD-SYMBOLS <ls_any> TYPE ty_s_any.");
		buildSrc("");
		buildSrc("    ASSIGN its_table[ a = 1 ] TO <ls_any>.");
		buildSrc("    INSERT VALUE #( a = 2 ) INTO TABLE its_table ASSIGNING <ls_any>.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    FIELD-SYMBOLS <ls_any> TYPE ty_s_any.");
		buildExp("");
		buildExp("    ASSIGN its_table[ a = 1 ] TO <ls_any>.");
		buildExp("    INSERT VALUE #( a = 2 ) INTO TABLE its_table ASSIGNING <ls_any>.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testFieldSymbolAssignedAtDeclarationButUnused() {
		buildSrc("    ASSIGN its_table[ a = 2 ] TO FIELD-SYMBOL(<ls_any>).");
		buildSrc("");
		buildSrc("    LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_other>).");
		buildSrc("      \" do nothing with <ls_other>");
		buildSrc("    ENDLOOP.");

		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    ASSIGN its_table[ a = 2 ] TO FIELD-SYMBOL(<ls_any>).");
		buildExp("");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_other>).");
		buildExp("      \" do nothing with <ls_other>");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveToDosInChainWhenUsedAgain() {
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    DATA: lv_used_again TYPE i,");
		buildSrc("          lv_unused_but_needed TYPE string ##NEEDED,");
		buildSrc("          \" TODO: variable is assigned but only used in commented-out code (ABAP cleaner)");
		buildSrc("          lv_assigned_and_used_again TYPE i.");
		buildSrc("");
		buildSrc("    CLEAR lv_used_again.");
		buildSrc("    ev_result = lv_used_again + 2.");
		buildSrc("    lv_assigned_and_used_again = 1.");
		buildSrc("");
		buildSrc("    rv_result = lv_assigned_and_used_again.");

		buildExp("    DATA: lv_used_again TYPE i,");
		buildExp("          lv_unused_but_needed TYPE string ##NEEDED,");
		buildExp("          lv_assigned_and_used_again TYPE i.");
		buildExp("");
		buildExp("    CLEAR lv_used_again.");
		buildExp("    ev_result = lv_used_again + 2.");
		buildExp("    lv_assigned_and_used_again = 1.");
		buildExp("");
		buildExp("    rv_result = lv_assigned_and_used_again.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testRemoveToDosForFieldSymbols() {
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    ASSIGN its_table[ a = 2 ] TO FIELD-SYMBOL(<ls_any>).");
		buildSrc("    rv_result = <ls_any>-component.");
		buildSrc("");
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_other>).");
		buildSrc("      other_method( <ls_other> ).");
		buildSrc("    ENDLOOP.");

		buildExp("    ASSIGN its_table[ a = 2 ] TO FIELD-SYMBOL(<ls_any>).");
		buildExp("    rv_result = <ls_any>-component.");
		buildExp("");
		buildExp("    LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_other>).");
		buildExp("      other_method( <ls_other> ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testRemoveToDosForData() {
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    DATA(lv_value) = 1.");
		buildSrc("    DATA(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("                           ev_result2 = DATA(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_value + lv_result1 * lv_result2.");

		buildExp("    DATA(lv_value) = 1.");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA(lv_other_value) = 2.");
		buildExp("");
		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildExp("                           ev_result2 = DATA(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_value + lv_result1 * lv_result2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testRemoveToDosForFinal() {
		buildSrc("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("    FINAL(lv_value) = 1.");
		buildSrc("    FINAL(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = FINAL(lv_result1)");
		buildSrc("                           \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("                           ev_result2 = FINAL(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_value + lv_result1 * lv_result2.");

		buildExp("    FINAL(lv_value) = 1.");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    FINAL(lv_other_value) = 2.");
		buildExp("");
		buildExp("    method_call( EXPORTING iv_data    = 5");
		buildExp("                 IMPORTING ev_result1 = FINAL(lv_result1)");
		buildExp("                           ev_result2 = FINAL(lv_result2) ).");
		buildExp("");
		buildExp("    rv_result = lv_value + lv_result1 * lv_result2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testRemoveToDosFromConstants() {
		buildSrc("    \" TODO: constant is never used (ABAP cleaner)");
		buildSrc("    CONSTANTS lc_used_again TYPE i VALUE 42.");
		buildSrc("    \" TODO: constant is only used in commented-out code (ABAP cleaner)");
		buildSrc("    CONSTANTS lc_also_used_again TYPE i VALUE 45.");
		buildSrc("");
		buildSrc("    rv_result = lc_used_again + lc_also_used_again.");

		buildExp("    CONSTANTS lc_used_again TYPE i VALUE 42.");
		buildExp("    CONSTANTS lc_also_used_again TYPE i VALUE 45.");
		buildExp("");
		buildExp("    rv_result = lc_used_again + lc_also_used_again.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepUnrelatedToDos() {
		buildSrc("    \" comment on a constant");
		buildSrc("    CONSTANTS lc_used_again TYPE i VALUE 42.");
		buildSrc("");
		buildSrc("    \" TODO: some other TODO comment");
		buildSrc("    DATA(lv_value) = 1.");
		buildSrc("    DATA(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           \" TODO: yet another TODO comment");
		buildSrc("                           ev_result2 = DATA(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_value * lv_other_value - lv_result1 * lv_result2 + lc_used_again.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepUnrelatedToDosWithFinal() {
		buildSrc("    \" comment on a constant");
		buildSrc("    CONSTANTS lc_used_again TYPE i VALUE 42.");
		buildSrc("");
		buildSrc("    \" TODO: some other TODO comment");
		buildSrc("    FINAL(lv_value) = 1.");
		buildSrc("    DATA(lv_other_value) = 2.");
		buildSrc("");
		buildSrc("    method_call( EXPORTING iv_data    = 5");
		buildSrc("                 IMPORTING ev_result1 = DATA(lv_result1)");
		buildSrc("                           \" TODO: yet another TODO comment");
		buildSrc("                           ev_result2 = FINAL(lv_result2) ).");
		buildSrc("");
		buildSrc("    rv_result = lv_value * lv_other_value - lv_result1 * lv_result2 + lc_used_again.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUsedFieldSymbols() {
		// ensure that 'usages' of field-symbols are correctly detected 

		buildSrc("    FIELD-SYMBOLS <ls_any> TYPE ty_s_any.");
		buildSrc("");
		buildSrc("    READ TABLE mt_table ASSIGNING <ls_any> WITH KEY id = 1.");
		buildSrc("    <ls_any> = ls_buffer.");
		buildSrc("");
		buildSrc("    ASSIGN mt_table[ id = 1 ] TO FIELD-SYMBOL(<ls_other>).");
		buildSrc("    ls_buffer = <ls_other>.");
		buildSrc("");
		buildSrc("    LOOP AT mt_table ASSIGNING FIELD-SYMBOL(<ls_third>).");
		buildSrc("      <ls_third>-component = 1.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("    LOOP AT mt_table ASSIGNING FIELD-SYMBOL(<ls_fourth>).");
		buildSrc("      CLEAR <ls_fourth>-component.");
		buildSrc("    ENDLOOP.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testBoundStructuresUnused() {
		buildSrc("    TYPES: BEGIN OF ty_s_struct,");
		buildSrc("             component TYPE i,");
		buildSrc("           END OF ty_s_struct.");
		buildSrc("");
		buildSrc("    CONSTANTS: BEGIN OF lc_struct,");
		buildSrc("                 component TYPE i VALUE 1,");
		buildSrc("               END OF lc_struct.");
		buildSrc("");
		buildSrc("    STATICS: BEGIN OF st_struct,");
		buildSrc("               component TYPE i,");
		buildSrc("             END OF st_struct.");
		buildSrc("");
		buildSrc("    DATA: BEGIN OF ld_struct,");
		buildSrc("            component TYPE i,");
		buildSrc("          END OF ld_struct.");

		buildExp("    TYPES: BEGIN OF ty_s_struct,");
		buildExp("             component TYPE i,");
		buildExp("           END OF ty_s_struct.");
		buildExp("");
		buildExp("    \" TODO: constant is never used (ABAP cleaner)");
		buildExp("    CONSTANTS: BEGIN OF lc_struct,");
		buildExp("                 component TYPE i VALUE 1,");
		buildExp("               END OF lc_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    STATICS: BEGIN OF st_struct,");
		buildExp("               component TYPE i,");
		buildExp("             END OF st_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    DATA: BEGIN OF ld_struct,");
		buildExp("            component TYPE i,");
		buildExp("          END OF ld_struct.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBoundStructuresWith2KeywordsUnused() {
		buildSrc("    TYPES: BEGIN OF ty_s_struct,");
		buildSrc("             component TYPE i.");
		buildSrc("    TYPES: END OF ty_s_struct.");
		buildSrc("");
		buildSrc("    CONSTANTS: BEGIN OF lc_struct,");
		buildSrc("                 component TYPE i VALUE 1.");
		buildSrc("    CONSTANTS: END OF lc_struct.");
		buildSrc("");
		buildSrc("    STATICS: BEGIN OF st_struct,");
		buildSrc("               component TYPE i.");
		buildSrc("    STATICS: END OF st_struct.");
		buildSrc("");
		buildSrc("    DATA: BEGIN OF ld_struct,");
		buildSrc("            component TYPE i.");
		buildSrc("    DATA: END OF ld_struct.");

		buildExp("    TYPES: BEGIN OF ty_s_struct,");
		buildExp("             component TYPE i.");
		buildExp("    TYPES: END OF ty_s_struct.");
		buildExp("");
		buildExp("    \" TODO: constant is never used (ABAP cleaner)");
		buildExp("    CONSTANTS: BEGIN OF lc_struct,");
		buildExp("                 component TYPE i VALUE 1.");
		buildExp("    CONSTANTS: END OF lc_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    STATICS: BEGIN OF st_struct,");
		buildExp("               component TYPE i.");
		buildExp("    STATICS: END OF st_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    DATA: BEGIN OF ld_struct,");
		buildExp("            component TYPE i.");
		buildExp("    DATA: END OF ld_struct.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBoundStructuresUsed() {
		buildSrc("    TYPES: BEGIN OF ty_s_struct,");
		buildSrc("             component TYPE i.");
		buildSrc("    TYPES: END OF ty_s_struct.");
		buildSrc("");
		buildSrc("    CONSTANTS: BEGIN OF lc_struct,");
		buildSrc("                 component TYPE i VALUE 1.");
		buildSrc("    CONSTANTS: END OF lc_struct.");
		buildSrc("");
		buildSrc("    STATICS: BEGIN OF st_struct,");
		buildSrc("               component TYPE i.");
		buildSrc("    STATICS: END OF st_struct.");
		buildSrc("");
		buildSrc("    DATA: BEGIN OF ld_struct,");
		buildSrc("            component TYPE i.");
		buildSrc("    DATA: END OF ld_struct.");
		buildSrc("");
		buildSrc("    rv_result = lc_struct-component + st_struct-component + ld_struct-component.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBoundStructuresUnused() {
		buildSrc("    TYPES: BEGIN OF ty_s_struct,");
		buildSrc("             component TYPE i,");
		buildSrc("             BEGIN OF ty_s_inner_struct,");
		buildSrc("               name TYPE string,");
		buildSrc("             END OF ty_s_inner_struct,");
		buildSrc("           END OF ty_s_struct.");
		buildSrc("");
		buildSrc("    CONSTANTS: BEGIN OF lc_struct,");
		buildSrc("                 component TYPE i VALUE 1,");
		buildSrc("                 BEGIN OF inner,");
		buildSrc("                   name TYPE string VALUE `abc`,");
		buildSrc("                 END OF inner,");
		buildSrc("               END OF lc_struct.");
		buildSrc("");
		buildSrc("    STATICS: BEGIN OF st_struct,");
		buildSrc("               component TYPE i,");
		buildSrc("               BEGIN OF inner,");
		buildSrc("                 name TYPE string,");
		buildSrc("               END OF inner,");
		buildSrc("             END OF st_struct.");
		buildSrc("");
		buildSrc("    DATA: BEGIN OF ld_struct,");
		buildSrc("            component TYPE i,");
		buildSrc("            BEGIN OF inner,");
		buildSrc("              name TYPE string,");
		buildSrc("            END OF inner,");
		buildSrc("          END OF ld_struct.");

		buildExp("    TYPES: BEGIN OF ty_s_struct,");
		buildExp("             component TYPE i,");
		buildExp("             BEGIN OF ty_s_inner_struct,");
		buildExp("               name TYPE string,");
		buildExp("             END OF ty_s_inner_struct,");
		buildExp("           END OF ty_s_struct.");
		buildExp("");
		buildExp("    \" TODO: constant is never used (ABAP cleaner)");
		buildExp("    CONSTANTS: BEGIN OF lc_struct,");
		buildExp("                 component TYPE i VALUE 1,");
		buildExp("                 BEGIN OF inner,");
		buildExp("                   name TYPE string VALUE `abc`,");
		buildExp("                 END OF inner,");
		buildExp("               END OF lc_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    STATICS: BEGIN OF st_struct,");
		buildExp("               component TYPE i,");
		buildExp("               BEGIN OF inner,");
		buildExp("                 name TYPE string,");
		buildExp("               END OF inner,");
		buildExp("             END OF st_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is never used (ABAP cleaner)");
		buildExp("    DATA: BEGIN OF ld_struct,");
		buildExp("            component TYPE i,");
		buildExp("            BEGIN OF inner,");
		buildExp("              name TYPE string,");
		buildExp("            END OF inner,");
		buildExp("          END OF ld_struct.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBoundStructuresUsedInComment() {
		buildSrc("    TYPES: BEGIN OF ty_s_struct,");
		buildSrc("             component TYPE i,");
		buildSrc("             BEGIN OF ty_s_inner_struct,");
		buildSrc("               name TYPE string,");
		buildSrc("             END OF ty_s_inner_struct,");
		buildSrc("           END OF ty_s_struct.");
		buildSrc("");
		buildSrc("    CONSTANTS: BEGIN OF lc_struct,");
		buildSrc("                 component TYPE i VALUE 1,");
		buildSrc("                 BEGIN OF inner,");
		buildSrc("                   name TYPE string VALUE `abc`,");
		buildSrc("                 END OF inner,");
		buildSrc("               END OF lc_struct.");
		buildSrc("");
		buildSrc("    STATICS: BEGIN OF st_struct,");
		buildSrc("               component TYPE i,");
		buildSrc("               BEGIN OF inner,");
		buildSrc("                 name TYPE string,");
		buildSrc("               END OF inner,");
		buildSrc("             END OF st_struct.");
		buildSrc("");
		buildSrc("    DATA: BEGIN OF ld_struct,");
		buildSrc("            component TYPE i,");
		buildSrc("            BEGIN OF inner,");
		buildSrc("              name TYPE string,");
		buildSrc("            END OF inner,");
		buildSrc("          END OF ld_struct.");
		buildSrc("");
		buildSrc("*   rv_result = lc_struct-inner-name && st_struct-inner-name && ld_struct-inner-name. ");

		buildExp("    TYPES: BEGIN OF ty_s_struct,");
		buildExp("             component TYPE i,");
		buildExp("             BEGIN OF ty_s_inner_struct,");
		buildExp("               name TYPE string,");
		buildExp("             END OF ty_s_inner_struct,");
		buildExp("           END OF ty_s_struct.");
		buildExp("");
		buildExp("    \" TODO: constant is only used in commented-out code (ABAP cleaner)");
		buildExp("    CONSTANTS: BEGIN OF lc_struct,");
		buildExp("                 component TYPE i VALUE 1,");
		buildExp("                 BEGIN OF inner,");
		buildExp("                   name TYPE string VALUE `abc`,");
		buildExp("                 END OF inner,");
		buildExp("               END OF lc_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is only used in commented-out code (ABAP cleaner)");
		buildExp("    STATICS: BEGIN OF st_struct,");
		buildExp("               component TYPE i,");
		buildExp("               BEGIN OF inner,");
		buildExp("                 name TYPE string,");
		buildExp("               END OF inner,");
		buildExp("             END OF st_struct.");
		buildExp("");
		buildExp("    \" TODO: variable is only used in commented-out code (ABAP cleaner)");
		buildExp("    DATA: BEGIN OF ld_struct,");
		buildExp("            component TYPE i,");
		buildExp("            BEGIN OF inner,");
		buildExp("              name TYPE string,");
		buildExp("            END OF inner,");
		buildExp("          END OF ld_struct.");
		buildExp("");
		buildExp("*   rv_result = lc_struct-inner-name && st_struct-inner-name && ld_struct-inner-name. ");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testMultipleDeclarationsInLine() {
		// ensure that variable "c" is not moved behind the line-end comment when variable "b" is deleted  
		
		buildSrc("  DATA: a TYPE i, \" comment");
		buildSrc("        b TYPE i, c TYPE i.");
		buildSrc("");
		buildSrc("    a = 1.");
		buildSrc("    c = 1.");

		buildExp("  \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("  DATA: a TYPE i, \" comment");
		buildExp("        \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("        c TYPE i.");
		buildExp("");
		buildExp("    a = 1.");
		buildExp("    c = 1.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testLikeTableOfLineOfRefToUnchanged() {
		// ensure that variables are not removed if they are used as models for LIKE TABLE OF, LIKE LINE OF, LIKE REF TO etc.   
		
		buildSrc("  DATA lv_x100        TYPE x LENGTH 100.");
		buildSrc("  DATA lt_x100        LIKE TABLE OF lv_x100.");
		buildSrc("");
		buildSrc("  DATA lv_c100        TYPE c LENGTH 100.");
		buildSrc("  DATA lt_c100        LIKE SORTED TABLE OF lv_c100 WITH UNIQUE KEY table_line.");
		buildSrc("");
		buildSrc("  DATA lt_any_table   TYPE ty_tt_table.");
		buildSrc("  DATA ls_any_struc   LIKE LINE OF lt_any_table.");
		buildSrc("");
		buildSrc("  DATA lt_other_table TYPE ty_tt_table.");
		buildSrc("  DATA lr_other_table LIKE REF TO lt_other_table.");
		buildSrc("");
		buildSrc("  et_x100  = lt_x100.");
		buildSrc("  et_c100  = lt_c100.");
		buildSrc("  es_struc = ls_any_struc.");
		buildSrc("  er_table = lr_other_table.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDefineSectionSkipped() {
		// expect macro definitions (i.e. DEFINE ... END-OF-DEFINITION sections) to be skipped when building the list of 
		// local variables - otherwise, 'DATA: lt_&1 ...' would be regarded as the declaration of an unused variable 
		buildSrc("    METHOD any_method.");
		buildSrc("      DEFINE def_table_and_structure.");
		buildSrc("        DATA: lt_&1 TYPE STANDARD TABLE OF ts_&1 WITH DEFAULT KEY.");
		buildSrc("      END-OF-DEFINITION.");
		buildSrc("");
		buildSrc("      def_table_and_structure any.");
		buildSrc("      def_table_and_structure other.");
		buildSrc("");
		buildSrc("      rv_result = lines( lt_any ) + lines( lt_other ).");
		buildSrc("    ENDMETHOD.");

		copyExpFromSrc();
		
		testRule();
	}
}
