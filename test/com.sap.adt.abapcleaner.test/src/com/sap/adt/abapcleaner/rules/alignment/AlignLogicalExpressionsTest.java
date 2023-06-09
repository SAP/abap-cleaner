package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.AlignStyle;

class AlignLogicalExpressionsTest extends RuleTestBase {
	private AlignLogicalExpressionsRule rule;
	
	AlignLogicalExpressionsTest() {
		super(RuleID.ALIGN_LOGICAL_EXPRESSIONS);
		rule = (AlignLogicalExpressionsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configAlignElseIfWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configAlignWhileWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configRightAlignComparisonOps.setValue(true);
		rule.configOnlyAlignSameObjects.setValue(false);
		rule.configMaxInnerSpaces.setValue(20);
	}
	
	@Test
	void testCheckWithIsAndEquals() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		
		buildSrc("    CHECK is_item_buffer-contract_id IS NOT INITIAL");
		buildSrc("       AND is_item_buffer-any_flag = abap_false");
		buildSrc("           AND is_item_buffer-other_flag = abap_false");
		buildSrc("                 AND is_item_buffer-third_flag = abap_true.");

		buildExp("    CHECK     is_item_buffer-contract_id IS NOT INITIAL");
		buildExp("          AND is_item_buffer-any_flag     = abap_false");
		buildExp("          AND is_item_buffer-other_flag   = abap_false");
		buildExp("          AND is_item_buffer-third_flag   = abap_true.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLeftAlignCheckWithOperators() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		
		buildSrc("    CHECK is_item_buffer-contract_id IS NOT INITIAL");
		buildSrc("       AND is_item_buffer-any_flag = abap_false");
		buildSrc("           AND is_item_buffer-other_flag = abap_false");
		buildSrc("                 AND is_item_buffer-third_flag = abap_true.");

		buildExp("    CHECK is_item_buffer-contract_id IS NOT INITIAL");
		buildExp("    AND   is_item_buffer-any_flag     = abap_false");
		buildExp("    AND   is_item_buffer-other_flag   = abap_false");
		buildExp("    AND   is_item_buffer-third_flag   = abap_true.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRightAlignCheckWithOperators() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		
		buildSrc("    CHECK is_item_buffer-contract_id IS NOT INITIAL");
		buildSrc("       OR is_item_buffer-any_flag = abap_false");
		buildSrc("           OR is_item_buffer-other_flag = abap_false");
		buildSrc("                 OR is_item_buffer-third_flag = abap_true.");

		buildExp("    CHECK is_item_buffer-contract_id IS NOT INITIAL");
		buildExp("       OR is_item_buffer-any_flag     = abap_false");
		buildExp("       OR is_item_buffer-other_flag   = abap_false");
		buildExp("       OR is_item_buffer-third_flag   = abap_true.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCheckWithInlineLogicalExprs() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		
		buildSrc("    CHECK line_exists( its_table[ 0 ] )    AND  its_table[ 0 ]-processed  =     abap_true");
		buildSrc("                OR line_exists( its_other_table[ 1 ] ) AND lines( its_other_table )   >     2.");

		buildExp("    CHECK    line_exists( its_table[ 0 ] )       AND its_table[ 0 ]-processed = abap_true");
		buildExp("          OR line_exists( its_other_table[ 1 ] ) AND lines( its_other_table ) > 2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithInlineLogicalExprs() {
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		
		buildSrc("    IF a = abap_false AND b > 3");
		buildSrc("         OR a = abap_true AND b <= 10.");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF    a = abap_false AND b  > 3");
		buildExp("       OR a = abap_true  AND b <= 10.");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAlignIfAndOperators() {
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		
		buildSrc("    IF a = abap_false AND b > 3");
		buildSrc("         OR a = abap_true AND b <= 10.");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF a = abap_false AND b  > 3");
		buildExp("    OR a = abap_true  AND b <= 10.");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRightAlignElseIfAndOperators() {
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configAlignElseIfWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		
		buildSrc("    IF a = abap_false AND b > 3");
		buildSrc("         OR a = abap_true AND b <= 10.");
		buildSrc("      \" do something");
		buildSrc("    ELSEIF a = abap_true AND b > 3");
		buildSrc("         AND a = abap_false AND b <= 10.");
		buildSrc("      \" do something else");
		buildSrc("    ENDIF.");

		buildExp("    IF    a = abap_false AND b  > 3");
		buildExp("       OR a = abap_true  AND b <= 10.");
		buildExp("      \" do something");
		buildExp("    ELSEIF a = abap_true  AND b  > 3");
		buildExp("       AND a = abap_false AND b <= 10.");
		buildExp("      \" do something else");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLeftAlignElseIfAndOperators() {
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		rule.configAlignElseIfWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		
		buildSrc("    IF a = abap_false AND b > 3");
		buildSrc("         OR a = abap_true AND b <= 10.");
		buildSrc("      \" do something");
		buildSrc("    ELSEIF a = abap_true AND b > 3");
		buildSrc("         AND a = abap_false AND b <= 10.");
		buildSrc("      \" do something else");
		buildSrc("    ENDIF.");

		buildExp("    IF a = abap_false AND b  > 3");
		buildExp("    OR a = abap_true  AND b <= 10.");
		buildExp("      \" do something");
		buildExp("    ELSEIF a = abap_true  AND b  > 3");
		buildExp("    AND    a = abap_false AND b <= 10.");
		buildExp("      \" do something else");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testXsdBoolWithBetweenAnd() {
		buildSrc("    DATA(is_work_day) = xsdbool( sy-fdayw BETWEEN 1 AND 5 ).");

		buildExp("    DATA(is_work_day) = xsdbool( sy-fdayw BETWEEN 1 AND 5 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testWhileComplexWithInlineLogicalExprs() {
		rule.configAlignWhileWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		
		buildSrc("    WHILE ( a = abap_true      OR b > 3 AND ( c IS BOUND OR d IS INSTANCE OF cl_x ) )");
		buildSrc("       AND ( a = abap_false OR b <= 10 AND ( c_alt IS NOT BOUND OR e_alt IS INSTANCE OF cl_xyz ) )");
		buildSrc("          OR ( c IS NOT SUPPLIED AND b IS INITIAL AND e BETWEEN f AND g )");
		buildSrc("       AND ( d IS SUPPLIED OR b IS NOT INITIAL )");
		buildSrc("       AND line_exists( its_table[ 0 ] )");
		buildSrc("            EQUIV     lines( its_table ) > 2");
		buildSrc("       AND line_exists( its_table[ 1 ] ).");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE               ( a  = abap_true    OR  b  > 3  AND ( c     IS BOUND     OR d     IS INSTANCE OF cl_x ) )");
		buildExp("                    AND ( a  = abap_false   OR  b <= 10 AND ( c_alt IS NOT BOUND OR e_alt IS INSTANCE OF cl_xyz ) )");
		buildExp("                OR      ( c IS NOT SUPPLIED AND b IS INITIAL AND e BETWEEN f AND g )");
		buildExp("                    AND ( d IS SUPPLIED     OR  b IS NOT INITIAL )");
		buildExp("                    AND line_exists( its_table[ 0 ] )");
		buildExp("          EQUIV     lines( its_table ) > 2");
		buildExp("                AND line_exists( its_table[ 1 ] ).");
		buildExp("      \" do something");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRightAlignWhileAndOperators() {
		rule.configAlignWhileWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		
		buildSrc("    WHILE a = abap_true");
		buildSrc("           AND b = abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE a = abap_true");
		buildExp("      AND b = abap_false.");
		buildExp("      \" do something");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLeftAlignWhileAndOperators() {
		rule.configAlignWhileWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		
		buildSrc("    WHILE a = abap_true");
		buildSrc("           OR b = abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");

		buildExp("    WHILE a = abap_true");
		buildExp("    OR    b = abap_false.");
		buildExp("      \" do something");
		buildExp("    ENDWHILE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfComplexWithParens() {
		buildSrc("    IF mo_abc->ms_data-component = if_any_interfc_name=>co_any_constant");
		buildSrc("    AND mo_abc->ms_data-comp2 = if_any_interfc_name=>co_other_constant");
		buildSrc("    AND ( ( lv_quantity >= 0 AND iv_total_quantity_xy >= lv_quantity )");
		buildSrc("    OR ( lv_quantity < 0 AND iv_total_quantity_xy <= lv_quantity ) ).");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF     mo_abc->ms_data-component = if_any_interfc_name=>co_any_constant");
		buildExp("       AND mo_abc->ms_data-comp2     = if_any_interfc_name=>co_other_constant");
		buildExp("       AND (    ( lv_quantity >= 0 AND iv_total_quantity_xy >= lv_quantity )");
		buildExp("             OR ( lv_quantity  < 0 AND iv_total_quantity_xy <= lv_quantity ) ).");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithLogicalOpsAtLineEnd() {
		buildSrc("    IF ls_other_comp-gjahr = ls_any_comp-gjahr AND");
		buildSrc("       ls_other_comp-poper >  ls_any_comp-poper.");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF lv_type_old <> <ls_data>-typ OR");
		buildSrc("       lv_category_old  <> <ls_data>-category.");
		buildSrc("    ENDIF.");

		buildExp("    IF ls_other_comp-gjahr = ls_any_comp-gjahr AND");
		buildExp("       ls_other_comp-poper > ls_any_comp-poper.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF lv_type_old     <> <ls_data>-typ OR");
		buildExp("       lv_category_old <> <ls_data>-category.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	

	@Test
	void testIfWithLogicalOpsAtLineEndAndParens() {
		// test examples with (some of the) Boolean operators at the end of the line
		
		buildSrc("    IF <ls_struc>-comp        >= ms_struc-component AND");
		buildSrc("      ( <ls_struc>-long_component = abap_true OR");
		buildSrc("        <ls_struc>-component  <> if_any_interface=>co_any_constant ).");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF mo_item->ms_data-any_component = if_any_interface=>co_any_constant");
		buildSrc("      AND mo_item->ms_data-other_component = if_any_interface=>co_other_constant");
		buildSrc("      AND ( ( lv_quantity >= 0 AND lv_other_quantity >= lv_quantity ) OR");
		buildSrc("            ( lv_quantity < 0  AND lv_other_quantity <= lv_quantity ) ). \" comment");
		buildSrc("    ENDIF.");

		buildExp("    IF <ls_struc>-comp >= ms_struc-component AND");
		buildExp("       ( <ls_struc>-long_component  = abap_true OR");
		buildExp("         <ls_struc>-component      <> if_any_interface=>co_any_constant ).");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF     mo_item->ms_data-any_component   = if_any_interface=>co_any_constant");
		buildExp("       AND mo_item->ms_data-other_component = if_any_interface=>co_other_constant");
		buildExp("       AND ( ( lv_quantity >= 0 AND lv_other_quantity >= lv_quantity ) OR");
		buildExp("             ( lv_quantity  < 0 AND lv_other_quantity <= lv_quantity ) ). \" comment");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithLogicalOpsAtLineMidAndEnd() {
		buildSrc("    IF <ls_any> IS ASSIGNED AND <ls_other> IS ASSIGNED");
		buildSrc("      AND <ls_any>-statistic <> <ls_other>-statistic.");
		buildSrc("      CONTINUE.");
		buildSrc("    ENDIF.");

		buildExp("    IF     <ls_any>           IS ASSIGNED AND <ls_other> IS ASSIGNED");
		buildExp("       AND <ls_any>-statistic <> <ls_other>-statistic.");
		buildExp("      CONTINUE.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithLogicalOpsAndCommentsAtLineEnd() {
		buildSrc("    IF sy-subrc <> 0                           OR \" comment");
		buildSrc("       <ls_item>-comp1 <> is_item_key-comp1 OR \" more comment");
		buildSrc("       <ls_item>-component2 <> is_item_key-component2. \" another comment");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF <lo_item_ref>->ms_data-any_flag = abap_true  OR");
		buildSrc("      ( <lo_item_ref>->ms_data-other_flag = abap_true AND \" comment");
		buildSrc("        <lo_item_ref>->ms_data-yet_another_flag = abap_false ).");
		buildSrc("      lv_any_flag = abap_true.");
		buildSrc("    ENDIF.");

		buildExp("    IF sy-subrc             <> 0 OR \" comment");
		buildExp("       <ls_item>-comp1      <> is_item_key-comp1 OR \" more comment");
		buildExp("       <ls_item>-component2 <> is_item_key-component2. \" another comment");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF <lo_item_ref>->ms_data-any_flag = abap_true OR");
		buildExp("       ( <lo_item_ref>->ms_data-other_flag       = abap_true AND \" comment");
		buildExp("         <lo_item_ref>->ms_data-yet_another_flag = abap_false ).");
		buildExp("      lv_any_flag = abap_true.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfElseIfComplex() {
		buildSrc("    IF  ( c IS NOT SUPPLIED");
		buildSrc("       OR b IS INITIAL )");
		buildSrc("       AND e BETWEEN f AND g");
		buildSrc("       AND ( d IS SUPPLIED");
		buildSrc("       OR b IS NOT INITIAL ).");
		buildSrc("      \" do something");
		buildSrc("");
		buildSrc("    ELSEIF line_exists( its_table[ 0 ] )");
		buildSrc("       OR ( lines( its_table ) > 2");
		buildSrc("    AND line_exists( its_table[ 1 ] ) ).");
		buildSrc("      \" do something");
		buildSrc("    ENDIF.");

		buildExp("    IF     (    c IS NOT SUPPLIED");
		buildExp("             OR b IS INITIAL )");
		buildExp("       AND e BETWEEN f AND g");
		buildExp("       AND (    d IS SUPPLIED");
		buildExp("             OR b IS NOT INITIAL ).");
		buildExp("      \" do something");
		buildExp("");
		buildExp("    ELSEIF    line_exists( its_table[ 0 ] )");
		buildExp("           OR (     lines( its_table ) > 2");
		buildExp("                AND line_exists( its_table[ 1 ] ) ).");
		buildExp("      \" do something");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithComparisonOpsInAndNotIn() {
		// test logical expression with comparison operators IN and NOT IN
		
		buildSrc("    IF attr_a = iv_param_a");
		buildSrc("       AND attr_b = iv_param_b");
		buildSrc("       AND attribute_c IN lrt_table_c");
		buildSrc("       AND doc_date NOT IN lrt_doc_date.");
		buildSrc("    ENDIF.");

		buildExp("    IF     attr_a        = iv_param_a");
		buildExp("       AND attr_b        = iv_param_b");
		buildExp("       AND attribute_c  IN lrt_table_c");
		buildExp("       AND doc_date NOT IN lrt_doc_date.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIfWithMixOfSameAndDifferentObjects() {
		// test example with a mix of same and different objects
		
		buildSrc("    CHECK  is_item_buffer-item_id IS NOT INITIAL");
		buildSrc("       AND is_item_buffer-any_flag = abap_false");
		buildSrc("       AND is_other_object-other_flag = abap_false");
		buildSrc("       AND is_other_object-other_flag = abap_false");
		buildSrc("       AND is_yet_another_object-only_flag = abap_false");
		buildSrc("       AND is_item_buffer-third_flag = abap_true");
		buildSrc("       AND is_item_buffer-has_no_change = abap_false.");

		buildExp("    CHECK     is_item_buffer-item_id          IS NOT INITIAL");
		buildExp("          AND is_item_buffer-any_flag          = abap_false");
		buildExp("          AND is_other_object-other_flag       = abap_false");
		buildExp("          AND is_other_object-other_flag       = abap_false");
		buildExp("          AND is_yet_another_object-only_flag  = abap_false");
		buildExp("          AND is_item_buffer-third_flag        = abap_true");
		buildExp("          AND is_item_buffer-has_no_change     = abap_false.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLoopAtWhereWithEqualsAndInOps() {
		buildSrc("    LOOP AT mts_table ASSIGNING FIELD-SYMBOL(<ls_entry>)");
		buildSrc("         WHERE field_a = iv_field_a");
		buildSrc("           AND long_field_b = iv_long_field_b");
		buildSrc("           AND amount IN lrt_amount");
		buildSrc("           AND start_date IN lrt_date.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mts_table ASSIGNING FIELD-SYMBOL(<ls_entry>)");
		buildExp("         WHERE     field_a       = iv_field_a");
		buildExp("               AND long_field_b  = iv_long_field_b");
		buildExp("               AND amount       IN lrt_amount");
		buildExp("               AND start_date   IN lrt_date.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLoopAtWhereWithMixedComparisonOps() {
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE item_id =  <ls_item>-item_id");
		buildSrc("         AND entry_type <> if_any_interface=>co_any_entry_type");
		buildSrc("         AND role <> if_any_interface=>co_any_role.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("    LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>) USING KEY any_key");
		buildSrc("         WHERE is_valid = abap_true");
		buildSrc("             AND category = if_any_interface=>co_any_category");
		buildSrc("              AND indicator <> if_any_interface=>co_any_indicator");
		buildSrc("               AND amount = if_any_interface=>co_any_amount");
		buildSrc("                AND statistic = abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE     item_id     = <ls_item>-item_id");
		buildExp("               AND entry_type <> if_any_interface=>co_any_entry_type");
		buildExp("               AND role       <> if_any_interface=>co_any_role.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("    LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>) USING KEY any_key");
		buildExp("         WHERE     is_valid   = abap_true");
		buildExp("               AND category   = if_any_interface=>co_any_category");
		buildExp("               AND indicator <> if_any_interface=>co_any_indicator");
		buildExp("               AND amount     = if_any_interface=>co_any_amount");
		buildExp("               AND statistic  = abap_false.");
		buildExp("      \" do something");
		buildExp("    ENDLOOP.");


		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLoopAtWhereWithParentheses() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE is_valid = abap_true");
		buildSrc("         AND any_flag = abap_true");
		buildSrc("         AND ( any_other_flag = abap_true");
		buildSrc("         OR change_flag = abap_true ).");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE     is_valid = abap_true");
		buildExp("               AND any_flag = abap_true");
		buildExp("               AND (    any_other_flag = abap_true");
		buildExp("                     OR change_flag    = abap_true ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testRightAlignWhereWithOperators() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE is_valid = abap_true");
		buildSrc("         AND any_flag = abap_true");
		buildSrc("         AND ( other_flag = abap_true");
		buildSrc("         OR change_flag = abap_true ).");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE is_valid = abap_true");
		buildExp("           AND any_flag = abap_true");
		buildExp("           AND (    other_flag  = abap_true");
		buildExp("                 OR change_flag = abap_true ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void tesLeftAlignWhereWithOperators() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE is_valid = abap_true");
		buildSrc("         AND any_flag = abap_true");
		buildSrc("         AND ( other_flag = abap_true");
		buildSrc("         OR change_flag = abap_true ).");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE is_valid = abap_true");
		buildExp("         AND   any_flag = abap_true");
		buildExp("         AND   (    other_flag  = abap_true");
		buildExp("                 OR change_flag = abap_true ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testLoopAtWhereComplex() {
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("      WHERE item_id = <lv_item_id>");
		buildSrc("        AND timestamp+0(7) >= lv_last_period");
		buildSrc("        AND (  category IS INITIAL");
		buildSrc("            OR category = if_any_interface=>co_any_category");
		buildSrc("            OR category = if_any_interface=>co_other_category )");
		buildSrc("        AND status <> if_any_interface=>co_any_status");
		buildSrc("        AND event_type = if_any_interface=>co_any_event_type.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("      WHERE     item_id         = <lv_item_id>");
		buildExp("            AND timestamp+0(7) >= lv_last_period");
		buildExp("            AND (    category IS INITIAL");
		buildExp("                  OR category  = if_any_interface=>co_any_category");
		buildExp("                  OR category  = if_any_interface=>co_other_category )");
		buildExp("            AND status     <> if_any_interface=>co_any_status");
		buildExp("            AND event_type  = if_any_interface=>co_any_event_type.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testXsdBool() {
		buildSrc("    DATA(lv_result) = xsdbool( ( ( mv_flag = abap_true");
		buildSrc("    AND mv_other_flag = abap_true");
		buildSrc("    AND ( ir_item->condition_type = ms_struc-any_cond_type");
		buildSrc("    OR ir_item->category = if_any_interface=>co_any_category ) )");
		buildSrc("    OR ( mv_flag = abap_true");
		buildSrc("    AND mv_other_flag = abap_false ) )");
		buildSrc("    AND ( lv_third_flag = abap_true");
		buildSrc("    OR lv_fourth_flag = abap_true");
		buildSrc("    OR lv_fifth_flag_with_long_name = abap_true ) ).");

		buildExp("    DATA(lv_result) = xsdbool(     (    (     mv_flag       = abap_true");
		buildExp("                                          AND mv_other_flag = abap_true");
		buildExp("                                          AND (    ir_item->condition_type = ms_struc-any_cond_type");
		buildExp("                                                OR ir_item->category       = if_any_interface=>co_any_category ) )");
		buildExp("                                     OR (     mv_flag       = abap_true");
		buildExp("                                          AND mv_other_flag = abap_false ) )");
		buildExp("                               AND (    lv_third_flag                = abap_true");
		buildExp("                                     OR lv_fourth_flag               = abap_true");
		buildExp("                                     OR lv_fifth_flag_with_long_name = abap_true ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testDeleteWhere() {
		buildSrc("    DELETE lt_table");
		buildSrc("     WHERE field IS NOT INITIAL");
		buildSrc("       AND field <> if_any_interface=>co_any_constant.");

		buildExp("    DELETE lt_table");
		buildExp("     WHERE     field IS NOT INITIAL");
		buildExp("           AND field <> if_any_interface=>co_any_constant.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCondConstructor() {
		buildSrc("    cv_value = COND #( WHEN is_struc-comp   = if_any_interface=>co_any_constant");
		buildSrc("                            AND mv_attribute = abap_true");
		buildSrc("                            AND ( ir_item->item_type = ms_struc-item_type");
		buildSrc("                             OR ir_item->category = if_any_interface=>co_any_category )");
		buildSrc("                       THEN ls_structure-component_a");
		buildSrc("                       ELSE ls_structure-component_b ).");

		buildExp("    cv_value = COND #( WHEN     is_struc-comp = if_any_interface=>co_any_constant");
		buildExp("                            AND mv_attribute  = abap_true");
		buildExp("                            AND (    ir_item->item_type = ms_struc-item_type");
		buildExp("                                  OR ir_item->category  = if_any_interface=>co_any_category )");
		buildExp("                       THEN ls_structure-component_a");
		buildExp("                       ELSE ls_structure-component_b ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCondConstructorInParamAssignment() {
		buildSrc("    lv_amount = calc_amount( iv_first_param  = <ls_item>-amount_a");
		buildSrc("                             iv_second_param = <ls_item>-amount_total + <ls_item>-amount_delta + lv_add");
		buildSrc("                             iv_third_param  = <ls_item>-any_flag");
		buildSrc("                             iv_fourth_param = <ls_ref_item>-amount");
		buildSrc("                                             + <ls_ref_item>-amount_delta");
		buildSrc("                                             + COND ty_amount( WHEN <ls_ref_item>-any_flag = abap_false");
		buildSrc("                                                               AND <ls_item>-other_flag = abap_true");
		buildSrc("                                                               THEN is_intermediate_item-amount_sum + is_intermediate_item-amount_delta )");
		buildSrc("                             iv_fifth_param  = <ls_item>-amount + <ls_item>-amount_delta ).");

		buildExp("    lv_amount = calc_amount( iv_first_param  = <ls_item>-amount_a");
		buildExp("                             iv_second_param = <ls_item>-amount_total + <ls_item>-amount_delta + lv_add");
		buildExp("                             iv_third_param  = <ls_item>-any_flag");
		buildExp("                             iv_fourth_param = <ls_ref_item>-amount");
		buildExp("                                             + <ls_ref_item>-amount_delta");
		buildExp("                                             + COND ty_amount( WHEN     <ls_ref_item>-any_flag = abap_false");
		buildExp("                                                                    AND <ls_item>-other_flag   = abap_true");
		buildExp("                                                               THEN is_intermediate_item-amount_sum + is_intermediate_item-amount_delta )");
		buildExp("                             iv_fifth_param  = <ls_item>-amount + <ls_item>-amount_delta ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAvoidLargeGaps() {
		// test two examples that could get huge inner gaps, depending on configuration
		
		buildSrc("    DATA(lv_value) = xsdbool( is_valid( ) = abap_false");
		buildSrc("                              AND mo_struc->if_any_interface~get_method( )->ms_data-any_component <> if_any_interface=>co_any_constant ).");
		buildSrc("");
		buildSrc("    DATA(lv_value_2) = xsdbool( ms_parameters-any_type = if_any_interface=>co_any_type");
		buildSrc("    AND ( <ls_ref_item>-comp IS INITIAL");
		buildSrc("    OR ( <ls_ref_item>-component2 IS NOT INITIAL");
		buildSrc("    AND is_some_condition_fulfilled( iv_param_a = <ls_update_item>-item_key");
		buildSrc("                                     iv_param_b = <ls_ref_item>-item_key ) = abap_true ) )");
		buildSrc("    AND is_another_condition_fulfilled( iv_param_a = ms_parameters-any_component_with_a_long_name");
		buildSrc("                                        iv_param_b = <ls_item>-item_key ) = abap_false");
		buildSrc("    AND mo_abc->if_any_interface~get_item( )->ms_data-component <> if_any_interface=>co_any_constant ).");

		buildExp("    DATA(lv_value) = xsdbool(     is_valid( ) = abap_false");
		buildExp("                              AND mo_struc->if_any_interface~get_method( )->ms_data-any_component <> if_any_interface=>co_any_constant ).");
		buildExp("");
		buildExp("    DATA(lv_value_2) = xsdbool(     ms_parameters-any_type = if_any_interface=>co_any_type");
		buildExp("                                AND (    <ls_ref_item>-comp IS INITIAL");
		buildExp("                                      OR (     <ls_ref_item>-component2 IS NOT INITIAL");
		buildExp("                                           AND is_some_condition_fulfilled( iv_param_a = <ls_update_item>-item_key");
		buildExp("                                                                            iv_param_b = <ls_ref_item>-item_key )   = abap_true ) )");
		buildExp("                                AND is_another_condition_fulfilled( iv_param_a = ms_parameters-any_component_with_a_long_name");
		buildExp("                                                                    iv_param_b = <ls_item>-item_key ) = abap_false");
		buildExp("                                AND mo_abc->if_any_interface~get_item( )->ms_data-component <> if_any_interface=>co_any_constant ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLeftAlignComparisonOps() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		rule.configRightAlignComparisonOps.setValue(false);
		
		buildSrc("    CHECK is_buffer-main_item_id IS NOT INITIAL");
		buildSrc("       OR is_buffer-any_flag = abap_false");
		buildSrc("           OR is_buffer-count <= 10");
		buildSrc("                 OR is_buffer-third_flag = abap_true.");

		buildExp("    CHECK is_buffer-main_item_id IS NOT INITIAL");
		buildExp("       OR is_buffer-any_flag     =  abap_false");
		buildExp("       OR is_buffer-count        <= 10");
		buildExp("       OR is_buffer-third_flag   =  abap_true.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAlignDifferentObjects() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configOnlyAlignSameObjects.setValue(false);
		
		buildSrc("    CHECK is_item_buffer-main_item_id IS NOT INITIAL");
		buildSrc("           AND is_other_buffer-other_flag = abap_false");
		buildSrc("                 AND is_yet_other_buffer-num_entries <= 10.");

		buildExp("    CHECK     is_item_buffer-main_item_id     IS NOT INITIAL");
		buildExp("          AND is_other_buffer-other_flag       = abap_false");
		buildExp("          AND is_yet_other_buffer-num_entries <= 10.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testOnlyAlignSameObjects() {
		rule.configAlignCheckWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);
		rule.configOnlyAlignSameObjects.setValue(true);
		
		buildSrc("    CHECK is_item_buffer-main_item_id IS NOT INITIAL");
		buildSrc("           AND is_other_buffer-other_flag = abap_false");
		buildSrc("                 AND is_yet_other_buffer-num_entries <= 10.");

		buildExp("    CHECK     is_item_buffer-main_item_id IS NOT INITIAL");
		buildExp("          AND is_other_buffer-other_flag = abap_false");
		buildExp("          AND is_yet_other_buffer-num_entries <= 10.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMaxInnerSpacesSufficient() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		rule.configMaxInnerSpaces.setValue(25);
		
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE very_very_long_variable_name = abap_true");
		buildSrc("         AND short_var = abap_true.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE very_very_long_variable_name = abap_true");
		buildExp("           AND short_var                    = abap_true.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMaxInnerSpacesExceeded() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		rule.configMaxInnerSpaces.setValue(15);
		
		buildSrc("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildSrc("         WHERE very_very_long_variable_name = abap_true");
		buildSrc("         AND short_var = abap_true.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mt_buffer ASSIGNING <ls_buffer>");
		buildExp("         WHERE very_very_long_variable_name = abap_true");
		buildExp("           AND short_var = abap_true.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testLeftAlignIfWithParentheses() {
		// regression test - ensure that the indent of 'IF' is not changed, even if the whole logical expression is in parentheses

		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);

		buildSrc("    IF ( a = 1 OR a = 2 ).");
		buildSrc("    ENDIF.");
		buildSrc("    IF ( a = 1");
		buildSrc("    OR a = 2 ).");
		buildSrc("    ENDIF.");

		buildExp("    IF ( a = 1 OR a = 2 ).");
		buildExp("    ENDIF.");
		buildExp("    IF (    a = 1");
		buildExp("         OR a = 2 ).");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOrInOwnLine() {
		buildSrc("    IF a = 1");
		buildSrc("    AND b = 2");
		buildSrc("    OR");
		buildSrc("    a = 2");
		buildSrc("    AND b = 1.");
		buildSrc("    ENDIF.");

		buildExp("    IF        a = 1");
		buildExp("          AND b = 2");
		buildExp("       OR");
		buildExp("              a = 2");
		buildExp("          AND b = 1.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOrInOwnLineWithComment() {
		buildSrc("  IF \" comment 1");
		buildSrc("  a = 1");
		buildSrc("  AND b = 2");
		buildSrc("  OR");
		buildSrc("  a = 2");
		buildSrc("  AND b = 1.");
		buildSrc("  ENDIF.");

		buildExp("  IF \" comment 1");
		buildExp("            a = 1");
		buildExp("        AND b = 2");
		buildExp("     OR");
		buildExp("            a = 2");
		buildExp("        AND b = 1.");
		buildExp("  ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAndInOwnLineWithComment() {
		buildSrc("  IF \" comment 1");
		buildSrc("  a = 1");
		buildSrc("  AND");
		buildSrc("  b = 2.");
		buildSrc("  ENDIF.");

		buildExp("  IF \" comment 1");
		buildExp("         a = 1");
		buildExp("     AND");
		buildExp("         b = 2.");
		buildExp("  ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOrInOwnLineWithCommentsInOwnLine() {
		buildSrc("  IF");
		buildSrc("  \" comment 1");
		buildSrc("  a = 1");
		buildSrc("  AND b = 2");
		buildSrc("  OR");
		buildSrc("  \" comment 2");
		buildSrc("  a = 2");
		buildSrc("  AND b = 1.");
		buildSrc("  ENDIF.");

		buildExp("  IF");
		buildExp("  \" comment 1");
		buildExp("            a = 1");
		buildExp("        AND b = 2");
		buildExp("     OR");
		buildExp("  \" comment 2");
		buildExp("            a = 2");
		buildExp("        AND b = 1.");
		buildExp("  ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTestAndOrOpeningBracketInOwnLine() {
		rule.configAlignIfWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);

		buildSrc("    IF");
		buildSrc("    a = 1");
		buildSrc("    AND");
		buildSrc("    (");
		buildSrc("    b = 2");
		buildSrc("    OR");
		buildSrc("    b = 3");
		buildSrc("    )");
		buildSrc("    OR");
		buildSrc("    a = 2");
		buildSrc("    AND");
		buildSrc("    b = 1");
		buildSrc("    AND");
		buildSrc("    (");
		buildSrc("    c = 1");
		buildSrc("    OR");
		buildSrc("    c = 2");
		buildSrc("    ).");
		buildSrc("    ENDIF.");

		buildExp("    IF");
		buildExp("           a = 1");
		buildExp("       AND");
		buildExp("           (");
		buildExp("                b = 2");
		buildExp("             OR");
		buildExp("                b = 3");
		buildExp("           )");
		buildExp("    OR");
		buildExp("           a = 2");
		buildExp("       AND");
		buildExp("           b = 1");
		buildExp("       AND");
		buildExp("           (");
		buildExp("                c = 1");
		buildExp("             OR");
		buildExp("                c = 2");
		buildExp("           ).");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignWhereInConstructorExpressions() {
		buildSrc("    rt_result = VALUE #( FOR ls_struc IN lt_table");
		buildSrc("                         WHERE ( status = 'a'");
		buildSrc("                         OR status = 'b' )");
		buildSrc("                         ( ls_struc ) ).");
		buildSrc("");
		buildSrc("    lt_other_result = FILTER #( lt_any_table USING KEY any_key");
		buildSrc("                                WHERE active = abap_true   AND   used = abap_true ).");

		buildExp("    rt_result = VALUE #( FOR ls_struc IN lt_table");
		buildExp("                         WHERE (    status = 'a'");
		buildExp("                                 OR status = 'b' )");
		buildExp("                         ( ls_struc ) ).");
		buildExp("");
		buildExp("    lt_other_result = FILTER #( lt_any_table USING KEY any_key");
		buildExp("                                WHERE active = abap_true AND used = abap_true ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUntilAndWhileInReduceConstructor() {
		buildSrc("    lv_sum = REDUCE i( INIT s = 0");
		buildSrc("                       FOR i = 1");
		buildSrc("                       UNTIL i = 10");
		buildSrc("                       OR i >= iv_max");
		buildSrc("                       NEXT s += i ).");
		buildSrc("");
		buildSrc("    lv_sum = REDUCE i( INIT s = 0");
		buildSrc("                       FOR i = 1");
		buildSrc("                       WHILE i < 10");
		buildSrc("                       AND i < iv_max");
		buildSrc("                       NEXT s += i ).");

		buildExp("    lv_sum = REDUCE i( INIT s = 0");
		buildExp("                       FOR i = 1");
		buildExp("                       UNTIL    i  = 10");
		buildExp("                             OR i >= iv_max");
		buildExp("                       NEXT s += i ).");
		buildExp("");
		buildExp("    lv_sum = REDUCE i( INIT s = 0");
		buildExp("                       FOR i = 1");
		buildExp("                       WHILE     i < 10");
		buildExp("                             AND i < iv_max");
		buildExp("                       NEXT s += i ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWaitUntil() {
		buildSrc("    WAIT FOR ASYNCHRONOUS TASKS MESSAGING CHANNELS");
		buildSrc("         UNTIL lo_instance=>is_task_done( lv_task_id ) = abap_true");
		buildSrc("         OR lo_instance=>is_task_cancelled( lv_task_id ) = abap_true");
		buildSrc("         UP TO 10 SECONDS.");

		buildExp("    WAIT FOR ASYNCHRONOUS TASKS MESSAGING CHANNELS");
		buildExp("         UNTIL    lo_instance=>is_task_done( lv_task_id )      = abap_true");
		buildExp("               OR lo_instance=>is_task_cancelled( lv_task_id ) = abap_true");
		buildExp("         UP TO 10 SECONDS.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWhereParenthesisInNextLine() {
		// expect the WHERE condition to be moved below the FOR
		buildSrc("    lt_result = VALUE #( FOR ls_struc IN lt_table WHERE");
		buildSrc("                         ( comp = '1'");
		buildSrc("AND comp2 = 'A' )");
		buildSrc("                         ( ls_struc ) ).");

		buildExp("    lt_result = VALUE #( FOR ls_struc IN lt_table WHERE");
		buildExp("                         (     comp  = '1'");
		buildExp("                           AND comp2 = 'A' )");
		buildExp("                         ( ls_struc ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWhereParenthesisInNextLineLeftOfEquals() {
		// expect the WHERE condition to be moved below the FOR, which is at its leftmost position here
		buildSrc("    lt_result = VALUE #(");
		buildSrc("        FOR ls_struc IN lt_table WHERE");
		buildSrc("    ( comp = '1'");
		buildSrc("    AND comp2 = 'A' )");
		buildSrc("        ( ls_struc ) ).");

		buildExp("    lt_result = VALUE #(");
		buildExp("        FOR ls_struc IN lt_table WHERE");
		buildExp("        (     comp  = '1'");
		buildExp("          AND comp2 = 'A' )");
		buildExp("        ( ls_struc ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
