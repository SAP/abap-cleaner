package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class LogicalOperatorPositionTest extends RuleTestBase {
	private LogicalOperatorPositionRule rule;
	
	LogicalOperatorPositionTest() {
		super(RuleID.LOGICAL_OPERATOR_POSITION);
		rule = (LogicalOperatorPositionRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMoveKeyword.setValue(true);
		rule.configMoveBooleanOperators.setValue(true);
	}
	
	@Test
	void testIfWithMultipleOr() {
		buildSrc("    IF ls_data-item_type = if_any_interface=>co_any_item_type OR");
		buildSrc("       ls_data-start_date IS NOT INITIAL OR");
		buildSrc("       ls_data-end_date IS INITIAL OR");
		buildSrc("       ls_data-processing_method = if_any_interface=>co_any_method.");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF    ls_data-item_type          = if_any_interface=>co_any_item_type");
		buildExp("       OR ls_data-start_date        IS NOT INITIAL");
		buildExp("       OR ls_data-end_date          IS INITIAL");
		buildExp("       OR ls_data-processing_method  = if_any_interface=>co_any_method.");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIfWithOrInParentheses() {
		buildSrc("    IF  ( c IS NOT SUPPLIED OR");
		buildSrc("          b IS INITIAL ) AND");
		buildSrc("        ( d IS SUPPLIED OR");
		buildSrc("          b IS NOT INITIAL ).");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF     (    c IS NOT SUPPLIED");
		buildExp("             OR b IS INITIAL )");
		buildExp("       AND (    d IS SUPPLIED");
		buildExp("             OR b IS NOT INITIAL ).");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIfAndElseIfUnchanged() {
		rule.configMoveBooleanOperators.setValue(false);

		buildSrc("    IF ls_item_data-category = if_any_interface=>co_any_category OR");
		buildSrc("       ls_item_data-start_date IS NOT INITIAL OR");
		buildSrc("       ls_item_data-end_date IS INITIAL OR");
		buildSrc("       ls_item_data-processing_method = if_other_interface=>co_any_processing_method.");
		buildSrc("");
		buildSrc("    ELSEIF  ( c IS NOT SUPPLIED OR");
		buildSrc("              b IS INITIAL ) AND");
		buildSrc("            ( d IS SUPPLIED OR");
		buildSrc("              b IS NOT INITIAL ).");
		buildSrc("");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testXsdBoolWithMultipleOr() {
		buildSrc("    rv_result = xsdbool( lv_flag_a = abap_true OR");
		buildSrc("                         lv_flag_b = abap_true OR");
		buildSrc("                         lv_flag_c = abap_true ).");

		buildExp("    rv_result = xsdbool(    lv_flag_a = abap_true");
		buildExp("                         OR lv_flag_b = abap_true");
		buildExp("                         OR lv_flag_c = abap_true ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testXsdBoolInAssignment() {
		buildSrc("    DATA(lv_result) = xsdbool( lv_value_a > 0 AND");
		buildSrc("                               ( lv_quantity + lv_quantity_delta ) <= 0 AND");
		buildSrc("                               ( lv_quantity <> 0 OR lv_quantity_delta <> 0 ) ).");

		buildExp("    DATA(lv_result) = xsdbool(     lv_value_a > 0");
		buildExp("                               AND ( lv_quantity + lv_quantity_delta ) <= 0");
		buildExp("                               AND ( lv_quantity <> 0 OR lv_quantity_delta <> 0 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testChainUnchanged() {
		// expect commands that contain a chain colon to be skipped
		
		buildSrc("    CHECK iv_param > 0 AND");
		buildSrc("          iv_param < : iv_limit_a, iv_limit_b.");
		buildSrc("    CHECK: iv_param > 0 AND");
		buildSrc("           iv_param < iv_limit_a,");
		buildSrc("           iv_param > 0 OR");
		buildSrc("           iv_param < iv_limit_b.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMoveWhereInLoop() {
		buildSrc("    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_any>) WHERE");
		buildSrc("      component = 1.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_any>)");
		buildExp("      WHERE component = 1.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWhereInDelete() {
		buildSrc("    DELETE lts_other_table WHERE");
		buildSrc("           status = 'a' OR");
		buildSrc("           status = 'b' AND");
		buildSrc("           obsolete = abap_true.");

		buildExp("    DELETE lts_other_table");
		buildExp("           WHERE    status = 'a'");
		buildExp("                 OR     status   = 'b'");
		buildExp("                    AND obsolete = abap_true.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWhereInDeleteWithCommentsAndEmptyLine() {
		buildSrc("    DELETE lts_other_table WHERE \" comment");
		buildSrc("*          comment line");
		buildSrc("");
		buildSrc("           status = 'a' OR");
		buildSrc("           status = 'b' AND");
		buildSrc("           obsolete = abap_true.");

		buildExp("    DELETE lts_other_table \" comment");
		buildExp("*          comment line");
		buildExp("");
		buildExp("           WHERE    status = 'a'");
		buildExp("                 OR     status   = 'b'");
		buildExp("                    AND obsolete = abap_true.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveWhereInDeleteKeepingBoolOps() {
		rule.configMoveBooleanOperators.setValue(false);

		buildSrc("    DELETE lts_other_table WHERE");
		buildSrc("           status = 'a' OR");
		buildSrc("           status = 'b' AND");
		buildSrc("           obsolete = abap_true.");

		buildExp("    DELETE lts_other_table");
		buildExp("           WHERE status = 'a' OR");
		buildExp("                 status = 'b' AND");
		buildExp("                 obsolete = abap_true.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWhereInDeleteNoAction() {
		rule.configMoveKeyword.setValue(false);
		rule.configMoveBooleanOperators.setValue(false);

		buildSrc("    DELETE lts_other_table WHERE");
		buildSrc("           status = 'a' OR");
		buildSrc("           status = 'b' AND");
		buildSrc("           obsolete = abap_true.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveWhereInConstructorExpressions() {
		buildSrc("    rt_result = VALUE #( FOR ls_struc IN lt_table WHERE");
		buildSrc("                         ( status = 'a' )");
		buildSrc("                         ( ls_struc ) ).");
		buildSrc("");
		buildSrc("    lt_other_result = FILTER #( lt_any_table USING KEY any_key WHERE");
		buildSrc("                                active = abap_true AND used = abap_true ).");

		buildExp("    rt_result = VALUE #( FOR ls_struc IN lt_table");
		buildExp("                         WHERE ( status = 'a' )");
		buildExp("                         ( ls_struc ) ).");
		buildExp("");
		buildExp("    lt_other_result = FILTER #( lt_any_table USING KEY any_key");
		buildExp("                                WHERE active = abap_true AND used = abap_true ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveUntilInReduceConstructor() {
		buildSrc("    lv_sum = REDUCE i( INIT s = 0");
		buildSrc("                       FOR i = 1 UNTIL");
		buildSrc("                       i = 10 OR");
		buildSrc("                       i >= iv_max");
		buildSrc("                       NEXT s += i ).");

		buildExp("    lv_sum = REDUCE i( INIT s = 0");
		buildExp("                       FOR i = 1");
		buildExp("                       UNTIL    i  = 10");
		buildExp("                             OR i >= iv_max");
		buildExp("                       NEXT s += i ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testKeepXsdBoolWithComment() {
		// ensure that unlike "WHERE" or "UNTIL", the rule does NOT try to move "xsdbool(" to the next line
		// because it opens a level and therefore can't be moved easily
		buildSrc("    lv_any = xsdbool( \" comment");
		buildSrc("               lv_any < lv_other ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveBoolOpsKeepingKeyword() {
		rule.configMoveKeyword.setValue(false);

		buildSrc("    DELETE lts_other_table WHERE");
		buildSrc("           status = 'a' OR");
		buildSrc("           status = 'b' AND");
		buildSrc("           obsolete = abap_true.");
		buildSrc("");
		buildSrc("    lv_sum = REDUCE i( INIT s = 0");
		buildSrc("                       FOR i = 1 UNTIL");
		buildSrc("                       i = 10 OR");
		buildSrc("                       i >= iv_max");
		buildSrc("                       NEXT s += i ).");

		buildExp("    DELETE lts_other_table WHERE");
		buildExp("              status = 'a'");
		buildExp("           OR     status   = 'b'");
		buildExp("              AND obsolete = abap_true.");
		buildExp("");
		buildExp("    lv_sum = REDUCE i( INIT s = 0");
		buildExp("                       FOR i = 1 UNTIL");
		buildExp("                          i  = 10");
		buildExp("                       OR i >= iv_max");
		buildExp("                       NEXT s += i ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWaitUntil() {
		buildSrc("    WAIT FOR ASYNCHRONOUS TASKS UNTIL");
		buildSrc("         lo_instance=>is_task_done( lv_task_id ) = abap_true.");
		buildSrc("");
		buildSrc("    WAIT FOR ASYNCHRONOUS TASKS MESSAGING CHANNELS UNTIL");
		buildSrc("         lo_instance=>is_task_done( lv_task_id ) = abap_true");
		buildSrc("         UP TO 10 SECONDS.");

		buildExp("    WAIT FOR ASYNCHRONOUS TASKS");
		buildExp("         UNTIL lo_instance=>is_task_done( lv_task_id ) = abap_true.");
		buildExp("");
		buildExp("    WAIT FOR ASYNCHRONOUS TASKS MESSAGING CHANNELS");
		buildExp("         UNTIL lo_instance=>is_task_done( lv_task_id ) = abap_true");
		buildExp("         UP TO 10 SECONDS.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWhereInLoop() {
		buildSrc("    LOOP AT lts_any_table WHERE");
		buildSrc("         comp = 1 OR");
		buildSrc("         comp = 2 TRANSPORTING NO FIELDS.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT lts_any_table");
		buildExp("         WHERE    comp = 1");
		buildExp("               OR comp = 2 TRANSPORTING NO FIELDS.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCheckWithComments() {
		buildSrc("    CHECK a IS INITIAL AND");
		buildSrc("          \" comment 1");
		buildSrc("      abs( lv_value ) > abs( lv_other_value ) AND \" comment 2");
		buildSrc("          \" comment 3");
		buildSrc("*         comment 4");
		buildSrc("      b <> c.");
		buildSrc("");
		buildSrc("    CHECK sy-subrc = 0 AND");
		buildSrc("         ( lv_any = 1 OR \" comment");
		buildSrc("            lv_other = 2 ). \" comment");
		buildSrc("");
		buildSrc("    CHECK ( <ls_any>-comp = abap_true AND");
		buildSrc("           <ls_any>-other_comp = abap_true ) OR");
		buildSrc("          \" comment");
		buildSrc("          ( <ls_any>-comp = abap_false AND");
		buildSrc("                <ls_any>-other_comp = abap_false AND \" comment");
		buildSrc("                <ls_any>-third_comp = 1");
		buildSrc("          \" another comment");
		buildSrc("              ).");
		buildSrc("");
		buildSrc("    CHECK \" comment");
		buildSrc("         ( <ls_any>-comp = abap_false AND");
		buildSrc("         <ls_any>-other_comp = abap_true ) OR");
		buildSrc("          \" other comment");
		buildSrc("*           third comment");
		buildSrc("         ( <ls_any>-comp = abap_true AND");
		buildSrc("         <ls_any>-other_comp = abap_false ).");

		buildExp("    CHECK     a               IS INITIAL");
		buildExp("          \" comment 1");
		buildExp("          AND abs( lv_value )  > abs( lv_other_value ) \" comment 2");
		buildExp("          \" comment 3");
		buildExp("*         comment 4");
		buildExp("          AND b               <> c.");
		buildExp("");
		buildExp("    CHECK     sy-subrc = 0");
		buildExp("          AND (    lv_any   = 1 \" comment");
		buildExp("                OR lv_other = 2 ). \" comment");
		buildExp("");
		buildExp("    CHECK    (     <ls_any>-comp       = abap_true");
		buildExp("               AND <ls_any>-other_comp = abap_true )");
		buildExp("          \" comment");
		buildExp("          OR (     <ls_any>-comp       = abap_false");
		buildExp("               AND <ls_any>-other_comp = abap_false \" comment");
		buildExp("               AND <ls_any>-third_comp = 1");
		buildExp("          \" another comment");
		buildExp("             ).");
		buildExp("");
		buildExp("    CHECK \" comment");
		buildExp("             (     <ls_any>-comp       = abap_false");
		buildExp("               AND <ls_any>-other_comp = abap_true )");
		buildExp("          \" other comment");
		buildExp("*           third comment");
		buildExp("          OR (     <ls_any>-comp       = abap_true");
		buildExp("               AND <ls_any>-other_comp = abap_false ).");

		testRule();
	}

	@Test
	void testLoopWithComments() {
		buildSrc("    LOOP AT mt_any ASSIGNING <ls_any>");
		buildSrc("         WHERE comp1 = 1 AND");
		buildSrc("           component2 = 2 AND");
		buildSrc("*          comp3 = 3 AND");
		buildSrc("           comp4 = 4.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_any ASSIGNING <ls_any>");
		buildExp("         WHERE     comp1      = 1");
		buildExp("               AND component2 = 2");
		buildExp("*          comp3 = 3 AND");
		buildExp("               AND comp4      = 4.");
		buildExp("    ENDLOOP.");

		testRule();
	}
}
