package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignSelectListsTest extends RuleTestBase {
	private AlignSelectListsRule rule;
	
	AlignSelectListsTest() {
		super(RuleID.ALIGN_SELECT_LISTS);
		rule = (AlignSelectListsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configComplexSelectListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configComplexOrderByListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configSimpleOrderByListLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configSelectIntoLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configConsiderTildeAsComplex.setValue(true);
		rule.configAlignAsInSelectList.setValue(true);
		rule.configAlignAdditionsInOrderByList.setValue(false);
	}

	@Test
	void testComplexSelectToMultiLineComplexGroupDerive() {
		buildSrc("    SELECT t1~any_col AS any_alias, t1~other_col AS other_alias,");
		buildSrc("       t2~third_col,");
		buildSrc("       SUM( fourth_col ) AS fourth_col, SUM( fifth_col ) AS fifth_col");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields");
		buildSrc("      WHERE t1~any_col    = @ms_any_struc-any_comp");
		buildSrc("        AND t2~third_col IN @lt_other_table");
		buildSrc("      GROUP BY t1~any_col,");
		buildSrc("      t1~other_col,");
		buildSrc("      t2~third_col,");
		buildSrc("      fourth_col, fifth_col.");

		buildExp("    SELECT t1~any_col        AS any_alias,");
		buildExp("           t1~other_col      AS other_alias,");
		buildExp("           t2~third_col,");
		buildExp("           SUM( fourth_col ) AS fourth_col,");
		buildExp("           SUM( fifth_col )  AS fifth_col");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields");
		buildExp("      WHERE t1~any_col    = @ms_any_struc-any_comp");
		buildExp("        AND t2~third_col IN @lt_other_table");
		buildExp("      GROUP BY t1~any_col,");
		buildExp("               t1~other_col,");
		buildExp("               t2~third_col,");
		buildExp("               fourth_col,");
		buildExp("               fifth_col.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexSelectAndComplexGroupToSingleLine() {
		rule.configComplexSelectListLayout.setEnumValue(SelectListLayout.ONE_LINE);
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.ONE_LINE);
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.MULTI_LINE);

		buildSrc("    SELECT t1~any_col AS any_alias, t1~other_col AS other_alias,");
		buildSrc("       t2~third_col,");
		buildSrc("       SUM( fourth_col ) AS fourth_col, SUM( fifth_col ) AS fifth_col");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields");
		buildSrc("      WHERE t1~any_col    = @ms_any_struc-any_comp");
		buildSrc("        AND t2~third_col IN @lt_other_table");
		buildSrc("      GROUP BY t1~any_col,");
		buildSrc("      t1~other_col,");
		buildSrc("      t2~third_col,");
		buildSrc("      fourth_col, fifth_col.");

		buildExp("    SELECT t1~any_col AS any_alias, t1~other_col AS other_alias, t2~third_col, SUM( fourth_col ) AS fourth_col, SUM(");
		buildExp("           fifth_col ) AS fifth_col");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields");
		buildExp("      WHERE t1~any_col    = @ms_any_struc-any_comp");
		buildExp("        AND t2~third_col IN @lt_other_table");
		buildExp("      GROUP BY t1~any_col, t1~other_col, t2~third_col, fourth_col, fifth_col.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleSelectMultiLineComplexOrderSingleLine110() {
		rule.configMaxLineLength.setValue(110);
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configComplexOrderByListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("    SELECT first_column,");
		buildSrc("      second_column,");
		buildSrc("      third_column, fourth_column,");
		buildSrc("      fifth_column, sixth_column,");
		buildSrc("*  seventh_column");
		buildSrc("      eigth_column");
		buildSrc("      FROM any_dtab");
		buildSrc("      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		buildExp("    SELECT first_column,");
		buildExp("           second_column,");
		buildExp("           third_column,");
		buildExp("           fourth_column,");
		buildExp("           fifth_column,");
		buildExp("           sixth_column,");
		buildExp("*           seventh_column");
		buildExp("           eigth_column");
		buildExp("      FROM any_dtab");
		buildExp("      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column");
		buildExp("               DESCENDING");
		buildExp("      INTO TABLE @DATA(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleSelectSingleLineIntoMultiLineTildeSimple() {
		rule.configSelectIntoLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configConsiderTildeAsComplex.setValue(false);

		buildSrc("    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col");
		buildSrc("      FROM any_dtab");
		buildSrc("           INNER JOIN fifth_col ON t2~col_6 = t1~col_6");
		buildSrc("      WHERE col_7 = @is_any_struc-any_comp");
		buildSrc("        AND col_8 = @is_any_struc-other_comp");
		buildSrc("      INTO ( @lv_any_value, @lv_other_value,");
		buildSrc("      @DATA(lv_third_value),");
		buildSrc("     @FINAL(lv_fourth_value) ).");

		buildExp("    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col");
		buildExp("      FROM any_dtab");
		buildExp("           INNER JOIN fifth_col ON t2~col_6 = t1~col_6");
		buildExp("      WHERE col_7 = @is_any_struc-any_comp");
		buildExp("        AND col_8 = @is_any_struc-other_comp");
		buildExp("      INTO ( @lv_any_value,");
		buildExp("             @lv_other_value,");
		buildExp("             @DATA(lv_third_value),");
		buildExp("             @FINAL(lv_fourth_value) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexSelectDeriveIntoOneLineTildeComplex() {
		rule.configComplexSelectListLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configSelectIntoLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col");
		buildSrc("      FROM any_dtab");
		buildSrc("           INNER JOIN fifth_col ON t2~col_6 = t1~col_6");
		buildSrc("      WHERE col_7 = @is_any_struc-any_comp");
		buildSrc("        AND col_8 = @is_any_struc-other_comp");
		buildSrc("      INTO ( @lv_any_value, @lv_other_value,");
		buildSrc("      @DATA(lv_third_value),");
		buildSrc("     @FINAL(lv_fourth_value) ).");

		buildExp("    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col");
		buildExp("      FROM any_dtab");
		buildExp("           INNER JOIN fifth_col ON t2~col_6 = t1~col_6");
		buildExp("      WHERE col_7 = @is_any_struc-any_comp");
		buildExp("        AND col_8 = @is_any_struc-other_comp");
		buildExp("      INTO ( @lv_any_value, @lv_other_value, @DATA(lv_third_value), @FINAL(lv_fourth_value) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleSelectSingleLineAscendingAligned() {
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.ONE_LINE);
		rule.configAlignAdditionsInOrderByList.setValue(true);

		buildSrc("    SELECT first_column,");
		buildSrc("      second_column,");
		buildSrc("      third_column, fourth_column,");
		buildSrc("      fifth_column, sixth_column,");
		buildSrc("*  seventh_column");
		buildSrc("      eigth_column");
		buildSrc("      FROM any_dtab");
		buildSrc("      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		buildExp("    SELECT first_column, second_column, third_column, fourth_column, fifth_column, sixth_column,");
		buildExp("*           seventh_column");
		buildExp("           eigth_column");
		buildExp("      FROM any_dtab");
		buildExp("      ORDER BY seventh_column ASCENDING NULLS FIRST,");
		buildExp("               first_column   DESCENDING NULLS LAST,");
		buildExp("               second_column  DESCENDING");
		buildExp("      INTO TABLE @DATA(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOneLinersSkipped() {
		buildSrc("    SELECT SINGLE any_col other_col INTO ls_any_struc FROM any_dtab WHERE other_col = gc_any_constant.");
		buildSrc("    SELECT any_col other_col third_col FROM any_dtab INTO TABLE lt_any_table.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testMaxLineLength80KeepOrderBy() {
		rule.configMaxLineLength.setValue(80);
		rule.configComplexOrderByListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);
		rule.configSimpleOrderByListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);

		buildSrc("    SELECT any_col, other_col, third_col, fourth_col,");
		buildSrc("      fifth_col, sixth_column_with_long_name, seventh_column_with_long_name");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE any_col   IN @ir_any_ref");
		buildSrc("        AND other_col IN @ir_other_ref");
		buildSrc("      ORDER BY fifth_col,");
		buildSrc("             any_col,");
		buildSrc("              other_col");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		buildExp("    SELECT any_col, other_col, third_col, fourth_col, fifth_col,");
		buildExp("           sixth_column_with_long_name, seventh_column_with_long_name");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE any_col   IN @ir_any_ref");
		buildExp("        AND other_col IN @ir_other_ref");
		buildExp("      ORDER BY fifth_col,");
		buildExp("             any_col,");
		buildExp("              other_col");
		buildExp("      INTO TABLE @DATA(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFieldsTildeSimple() {
		rule.configConsiderTildeAsComplex.setValue(false);

		buildSrc("    SELECT FROM first_dtab AS t1");
		buildSrc("             CROSS JOIN second_dtab AS t2");
		buildSrc("           FIELDS t1~a AS a1, t1~b AS b1, t2~c AS c2, t2~d AS d2");
		buildSrc("           WHERE t2~d = t1~d");
		buildSrc("           ORDER BY t1~d t1~c");
		buildSrc("           INTO CORRESPONDING FIELDS OF TABLE @lt_table.");

		buildExp("    SELECT FROM first_dtab AS t1");
		buildExp("             CROSS JOIN second_dtab AS t2");
		buildExp("           FIELDS t1~a AS a1,");
		buildExp("                  t1~b AS b1,");
		buildExp("                  t2~c AS c2,");
		buildExp("                  t2~d AS d2");
		buildExp("           WHERE t2~d = t1~d");
		buildExp("           ORDER BY t1~d t1~c");
		buildExp("           INTO CORRESPONDING FIELDS OF TABLE @lt_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectAsteriskDynamicLists() {
		buildSrc("    SELECT *");
		buildSrc("      FROM any_dtab");
		buildSrc("      GROUP BY (lv_group)");
		buildSrc("      ORDER BY (lv_order)");
		buildSrc("      INTO TABLE @lt_any.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSimpleSelectKeepComplexOrderKeep() {
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);
		rule.configComplexOrderByListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);

		buildSrc("    SELECT first_column,");
		buildSrc("      second_column,");
		buildSrc("      third_column, fourth_column,");
		buildSrc("      fifth_column, sixth_column,");
		buildSrc("*  seventh_column");
		buildSrc("      eigth_column");
		buildSrc("      FROM any_dtab");
		buildSrc("      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIndentComment() {
		buildSrc("    SELECT first_column,");
		buildSrc("*           second_column,");
		buildSrc("      third_column, fourth_column,");
		buildSrc("*fifth_column");
		buildSrc("**");
		buildSrc("*  seventh_column");
		buildSrc("      eigth_column");
		buildSrc("      FROM any_dtab");
		buildSrc("      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		buildExp("    SELECT first_column,");
		buildExp("*           second_column,");
		buildExp("           third_column,");
		buildExp("           fourth_column,");
		buildExp("*           fifth_column");
		buildExp("**");
		buildExp("*           seventh_column");
		buildExp("           eigth_column");
		buildExp("      FROM any_dtab");
		buildExp("      ORDER BY seventh_column ASCENDING NULLS FIRST,");
		buildExp("               first_column DESCENDING NULLS LAST,");
		buildExp("               second_column DESCENDING");
		buildExp("      INTO TABLE @DATA(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexFieldsMultiLineGroupingSetsSkipped() {
		buildSrc("    SELECT FROM any_dtab");
		buildSrc("      FIELDS any_col, other_col, third_col, SUM( fourth_col ) AS fifth_col,");
		buildSrc("             grouping( any_col ) AS col_6, grouping( other_col ) AS col_7,");
		buildSrc("             grouping( third_col ) AS col_8");
		buildSrc("      WHERE any_col = 'T1'");
		buildSrc("      GROUP BY GROUPING SETS ( ( any_col, other_col, third_col ),");
		buildSrc("                               ( any_col, other_col ),");
		buildSrc("                               ( other_col, third_col ),");
		buildSrc("                               ( any_col ),");
		buildSrc("                               ( ) )");
		buildSrc("      INTO TABLE @FINAL(lv_any_value).");

		buildExp("    SELECT FROM any_dtab");
		buildExp("      FIELDS any_col,");
		buildExp("             other_col,");
		buildExp("             third_col,");
		buildExp("             SUM( fourth_col )     AS fifth_col,");
		buildExp("             grouping( any_col )   AS col_6,");
		buildExp("             grouping( other_col ) AS col_7,");
		buildExp("             grouping( third_col ) AS col_8");
		buildExp("      WHERE any_col = 'T1'");
		buildExp("      GROUP BY GROUPING SETS ( ( any_col, other_col, third_col ),");
		buildExp("                               ( any_col, other_col ),");
		buildExp("                               ( other_col, third_col ),");
		buildExp("                               ( any_col ),");
		buildExp("                               ( ) )");
		buildExp("      INTO TABLE @FINAL(lv_any_value).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNonStrictSyntaxWithAdditions() {
		buildSrc("          SELECT any_col AS any_col");
		buildSrc("               other_col AS third_col");
		buildSrc("           INTO CORRESPONDING FIELDS OF TABLE lt_any_table");
		buildSrc("           FROM any_dtab");
		buildSrc("           WHERE fourth_col = 'T1'");
		buildSrc("           ORDER BY any_col ASCENDING other_col DESCENDING.");

		buildExp("          SELECT any_col   AS any_col");
		buildExp("                 other_col AS third_col");
		buildExp("           INTO CORRESPONDING FIELDS OF TABLE lt_any_table");
		buildExp("           FROM any_dtab");
		buildExp("           WHERE fourth_col = 'T1'");
		buildExp("           ORDER BY any_col ASCENDING");
		buildExp("                    other_col DESCENDING.");

		testRule();
	}

	@Test
	void testInnerQueries() {
		buildSrc("    SELECT any_col, other_col, third_col FROM any_dtab APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table");
		buildSrc("           WHERE other_col IN ( SELECT FROM other_dtab FIELDS other_col )");
		buildSrc("             AND third_col IN ( SELECT FROM third_dtab FIELDS third_col ). \"#EC CI_BUFFSUBQ.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexSelectDeriveOrderByDerive() {
		rule.configComplexSelectListLayout.setEnumValue(SelectListLayout.DERIVE);

		buildSrc("    SELECT any_col, CAST( ' ' AS CHAR( 2 ) ) AS other_col,");
		buildSrc("          third_col, char1 && '_' && char2 AS fourth_col, fifth_col,");
		buildSrc("        MIN( num1 ) * MIN( num2 ) AS fifth_col");
		buildSrc("      FROM any_dtab");
		buildSrc("      ORDER BY any_col,");
		buildSrc("       third_col, fourth_col,");
		buildSrc("           fifth_col");
		buildSrc("      INTO @DATA(lv_any_value).");
		buildSrc("    ENDSELECT.");

		buildExp("    SELECT any_col, CAST( ' ' AS CHAR( 2 ) ) AS other_col, third_col, char1 && '_' && char2 AS fourth_col, fifth_col,");
		buildExp("           MIN( num1 ) * MIN( num2 ) AS fifth_col");
		buildExp("      FROM any_dtab");
		buildExp("      ORDER BY any_col,");
		buildExp("               third_col,");
		buildExp("               fourth_col,");
		buildExp("               fifth_col");
		buildExp("      INTO @DATA(lv_any_value).");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWithCommonTableExpressions() {
		buildSrc("    WITH");
		buildSrc("      +any_cte AS (");
		buildSrc("        SELECT any_dtab~any_id, any_name, other_id, value_from, value_to");
		buildSrc("          FROM any_dtab");
		buildSrc("           INNER JOIN other_dtab ON other_dtab~any_id = any_dtab~any_id");
		buildSrc("          WHERE any_dtab~any_id BETWEEN @from_id AND @to_id ),");
		buildSrc("");
		buildSrc("      +other_cte AS (");
		buildSrc("        SELECT any_id, other_id, SUM( any_value ) AS sum_value");
		buildSrc("          FROM third_dtab");
		buildSrc("          WHERE any_id BETWEEN @from_id AND @to_id");
		buildSrc("          GROUP BY any_id, other_id ),");
		buildSrc("");
		buildSrc("      +result_cte( name, name2, name3, name4, name5 ) AS (");
		buildSrc("        SELECT any_name, c~other_id, value_from, value_to, sum_value");
		buildSrc("          FROM +any_cte AS c");
		buildSrc("            INNER JOIN +other_cte AS s ON  c~any_id = s~any_id");
		buildSrc("                                       AND c~other_id = s~other_id )");
		buildSrc("");
		buildSrc("      SELECT * FROM +result_cte");
		buildSrc("        ORDER BY name, name2");
		buildSrc("        INTO TABLE @FINAL(result1).");

		buildExp("    WITH");
		buildExp("      +any_cte AS (");
		buildExp("        SELECT any_dtab~any_id,");
		buildExp("               any_name,");
		buildExp("               other_id,");
		buildExp("               value_from,");
		buildExp("               value_to");
		buildExp("          FROM any_dtab");
		buildExp("           INNER JOIN other_dtab ON other_dtab~any_id = any_dtab~any_id");
		buildExp("          WHERE any_dtab~any_id BETWEEN @from_id AND @to_id ),");
		buildExp("");
		buildExp("      +other_cte AS (");
		buildExp("        SELECT any_id,");
		buildExp("               other_id,");
		buildExp("               SUM( any_value ) AS sum_value");
		buildExp("          FROM third_dtab");
		buildExp("          WHERE any_id BETWEEN @from_id AND @to_id");
		buildExp("          GROUP BY any_id, other_id ),");
		buildExp("");
		buildExp("      +result_cte( name, name2, name3, name4, name5 ) AS (");
		buildExp("        SELECT any_name,");
		buildExp("               c~other_id,");
		buildExp("               value_from,");
		buildExp("               value_to,");
		buildExp("               sum_value");
		buildExp("          FROM +any_cte AS c");
		buildExp("            INNER JOIN +other_cte AS s ON  c~any_id = s~any_id");
		buildExp("                                       AND c~other_id = s~other_id )");
		buildExp("");
		buildExp("      SELECT * FROM +result_cte");
		buildExp("        ORDER BY name, name2");
		buildExp("        INTO TABLE @FINAL(result1).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentAfterIntoParenthesis() {
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configSelectIntoLayout.setEnumValue(SelectListLayout.MULTI_LINE);

		buildSrc("    SELECT any_col other_col");
		buildSrc("      UP TO 10 ROWS");
		buildSrc("      FROM any_dtab");
		buildSrc("      INTO ( \" comment");
		buildSrc("             ev_any_param, ev_other_param )");
		buildSrc("      WHERE     third_col = lv_any_value");
		buildSrc("            AND fourth_col = iv_any_param.");
		buildSrc("    ENDSELECT.");

		buildExp("    SELECT any_col");
		buildExp("           other_col");
		buildExp("      UP TO 10 ROWS");
		buildExp("      FROM any_dtab");
		buildExp("      INTO ( \" comment");
		buildExp("             ev_any_param,");
		buildExp("             ev_other_param )");
		buildExp("      WHERE     third_col = lv_any_value");
		buildExp("            AND fourth_col = iv_any_param.");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpacesInList() {
		// the SELECT list contains superfluous spaces and a missing space after the comma
		buildSrc("    SELECT  t1~any_col AS other_col, t1~third_col AS fourth_col,t1~fifth_col AS col_6");
		buildSrc("    FROM @it_any_table AS t1 ##ITAB_KEY_IN_SELECT");
		buildSrc("    WHERE t1~third_col IN @gt_any_table");
		buildSrc("    INTO CORRESPONDING FIELDS OF TABLE @mt_any_table.");

		buildExp("    SELECT t1~any_col   AS other_col,");
		buildExp("           t1~third_col AS fourth_col,");
		buildExp("           t1~fifth_col AS col_6");
		buildExp("    FROM @it_any_table AS t1 ##ITAB_KEY_IN_SELECT");
		buildExp("    WHERE t1~third_col IN @gt_any_table");
		buildExp("    INTO CORRESPONDING FIELDS OF TABLE @mt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentAtListStart() {
		buildSrc("    SELECT DISTINCT");
		buildSrc("         \" comment1");
		buildSrc("*       comment2");
		buildSrc("            any_col AS c1,");
		buildSrc("             other_col AS c2,");
		buildSrc("             \" comment3");
		buildSrc("              third_col AS c3");
		buildSrc("      FROM any_cds( iv_any_param = @lv_any_value )");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @mv_any_attribute");
		buildSrc("      WHERE c1 IN @lt_any_table");
		buildSrc("        AND c3 IN @lt_other_table.");

		buildExp("    SELECT DISTINCT \" comment1");
		buildExp("*                    comment2");
		buildExp("                    any_col   AS c1,");
		buildExp("                    other_col AS c2,");
		buildExp("                    \" comment3");
		buildExp("                    third_col AS c3");
		buildExp("      FROM any_cds( iv_any_param = @lv_any_value )");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @mv_any_attribute");
		buildExp("      WHERE c1 IN @lt_any_table");
		buildExp("        AND c3 IN @lt_other_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitSelectListAndMoveIntoOrFromToOwnLine() {
		// ensure that if the SELECT list is split into multiple lines, a FROM or INTO clause that follows
		// on the same line is moved to the next line
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.MULTI_LINE);

		buildSrc("    SELECT any_col, other_col, third_col INTO TABLE lt_any_table");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE any_col < 10.");
		buildSrc("");
		buildSrc("    SELECT any_col, other_col, third_col FROM any_dtab");
		buildSrc("      INTO TABLE lt_any_table");
		buildSrc("      WHERE any_col < 10.");

		buildExp("    SELECT any_col,");
		buildExp("           other_col,");
		buildExp("           third_col");
		buildExp("      INTO TABLE lt_any_table");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE any_col < 10.");
		buildExp("");
		buildExp("    SELECT any_col,");
		buildExp("           other_col,");
		buildExp("           third_col");
		buildExp("      FROM any_dtab");
		buildExp("      INTO TABLE lt_any_table");
		buildExp("      WHERE any_col < 10.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitSelectListAndMoveComplexFromToOwnLine() {
		// ensure that even a multi-line FROM clause is correctly moved to the next line 
		// if it continued after a SELECT list that is split into multiple lines
		rule.configSimpleSelectListLayout.setEnumValue(SelectListLayout.MULTI_LINE);

		buildSrc("    SELECT any_col, other_col, third_col FROM any_dtab AS a");
		buildSrc("                                                INNER JOIN");
		buildSrc("                                                  other_dtab AS b ON a~any = b~any");
		buildSrc("      INTO TABLE lt_any_table");
		buildSrc("      WHERE any_col < 10.");

		buildExp("    SELECT any_col,");
		buildExp("           other_col,");
		buildExp("           third_col");
		buildExp("      FROM any_dtab AS a");
		buildExp("             INNER JOIN");
		buildExp("               other_dtab AS b ON a~any = b~any");
		buildExp("      INTO TABLE lt_any_table");
		buildExp("      WHERE any_col < 10.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveSpacesBeforeComma() {
		buildSrc("    SELECT any_col,");
		buildSrc("           MAX( other_col ) AS c2   ,");
		buildSrc("           MIN( third_col ) AS c3     ,");
		buildSrc("           SUM( fourth_col ) AS c4 \" comment");
		buildSrc("           , fifth_col AS c4");
		buildSrc("      FROM any_dtab");
		buildSrc("      GROUP BY any_col");
		buildSrc("      %_HINTS HDB 'RESULT_CACHE' \"#EC CI_HINTS");
		buildSrc("      INTO TABLE @FINAL(lv_any_value)");
		buildSrc("           EXTENDED RESULT @lo_extended_result.");

		buildExp("    SELECT any_col,");
		buildExp("           MAX( other_col )  AS c2,");
		buildExp("           MIN( third_col )  AS c3,");
		buildExp("           SUM( fourth_col ) AS c4 \" comment");
		buildExp("           ,");
		buildExp("           fifth_col         AS c4");
		buildExp("      FROM any_dtab");
		buildExp("      GROUP BY any_col");
		buildExp("      %_HINTS HDB 'RESULT_CACHE' \"#EC CI_HINTS");
		buildExp("      INTO TABLE @FINAL(lv_any_value)");
		buildExp("           EXTENDED RESULT @lo_extended_result.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAddSpaceInParenthesesOfInto() {
		// ensure that a space is added at the beginning and end of an INTO ( ... ) list with one element

		buildSrc("    SELECT SINGLE any_col FROM any_dtab");
		buildSrc("      WHERE any_col = @lv_any_value");
		buildSrc("      INTO (@DATA(dobj1)).");
		buildSrc("");
		buildSrc("    SELECT SINGLE any_col FROM any_dtab");
		buildSrc("      WHERE any_col = @lv_any_value");
		buildSrc("      INTO (@FINAL(dobj2)).");
		buildSrc("");
		buildSrc("    SELECT SINGLE any_col FROM any_dtab");
		buildSrc("      WHERE any_col = @lv_any_value");
		buildSrc("      INTO (NEW @DATA(dref1)).");
		buildSrc("");
		buildSrc("    SELECT SINGLE any_col FROM any_dtab");
		buildSrc("      WHERE any_col = @lv_any_value");
		buildSrc("      INTO (NEW @FINAL(dref2)).");

		buildExp("    SELECT SINGLE any_col FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("      INTO ( @DATA(dobj1) ).");
		buildExp("");
		buildExp("    SELECT SINGLE any_col FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("      INTO ( @FINAL(dobj2) ).");
		buildExp("");
		buildExp("    SELECT SINGLE any_col FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("      INTO ( NEW @DATA(dref1) ).");
		buildExp("");
		buildExp("    SELECT SINGLE any_col FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("      INTO ( NEW @FINAL(dref2) ).");

		putAnyMethodAroundSrcAndExp();

		// RND Parser categorizes INTO (NEW @DATA(...)) as erroneous, although ABAP syntax allows this
		deactivateSyntaxCheckAfterParse();
		
		testRule();
	}

	@Test
	void testSelectListWithHostVariableAndLiteral() {
		buildSrc("    SELECT any_col, @lr_any_ref->mv_any_attribute AS other_col, third_col , '2' AS fourth_col");
		buildSrc("      FROM any_dtab");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");

		buildExp("    SELECT any_col,");
		buildExp("           @lr_any_ref->mv_any_attribute AS other_col,");
		buildExp("           third_col,");
		buildExp("           '2'                           AS fourth_col");
		buildExp("      FROM any_dtab");
		buildExp("      INTO TABLE @DATA(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFieldsWithCaseWhen() {
		buildSrc("    SELECT SINGLE");
		buildSrc("      FROM @data_tab AS data_tab");
		buildSrc("      FIELDS CASE WHEN 1 = 0 THEN 0 END AS col1, col2  ,");
		buildSrc("         CASE WHEN 1 = 0 THEN 0 END AS col3,   CASE WHEN 1 = 0 THEN 42 END AS col4, col5");
		buildSrc("      INTO CORRESPONDING FIELDS OF @wa_ind2");
		buildSrc("           INDICATORS NULL BITFIELD null_ind.");

		buildExp("    SELECT SINGLE");
		buildExp("      FROM @data_tab AS data_tab");
		buildExp("      FIELDS CASE WHEN 1 = 0 THEN 0 END  AS col1,");
		buildExp("             col2,");
		buildExp("             CASE WHEN 1 = 0 THEN 0 END  AS col3,");
		buildExp("             CASE WHEN 1 = 0 THEN 42 END AS col4,");
		buildExp("             col5");
		buildExp("      INTO CORRESPONDING FIELDS OF @wa_ind2");
		buildExp("           INDICATORS NULL BITFIELD null_ind.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectListWithFirstValueOver() {
		buildSrc("    SELECT");
		buildSrc("          id,");
		buildSrc("          col1,");
		buildSrc("          col2,");
		buildSrc("          col3,");
		buildSrc("          FIRST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 )");
		buildSrc("                      AS first_value,");
		buildSrc("          LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 )");
		buildSrc("                      AS last_value,");
		buildSrc("          LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3");
		buildSrc("                      ROWS BETWEEN UNBOUNDED PRECEDING");
		buildSrc("                      AND UNBOUNDED FOLLOWING )");
		buildSrc("                      AS last_value_correct");
		buildSrc("          FROM demo_update");
		buildSrc("          INTO TABLE @DATA(result).");

		buildExp("    SELECT id,");
		buildExp("           col1,");
		buildExp("           col2,");
		buildExp("           col3,");
		buildExp("           FIRST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 ) AS first_value,");
		buildExp("           LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 )  AS last_value,");
		buildExp("           LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3");
		buildExp("                       ROWS BETWEEN UNBOUNDED PRECEDING");
		buildExp("                       AND UNBOUNDED FOLLOWING )                       AS last_value_correct");
		buildExp("          FROM demo_update");
		buildExp("          INTO TABLE @DATA(result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
