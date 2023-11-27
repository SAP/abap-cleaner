package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignSelectClausesTest extends RuleTestBase {
	private AlignSelectClausesRule rule;
	
	AlignSelectClausesTest() {
		super(RuleID.ALIGN_SELECT_CLAUSES);
		rule = (AlignSelectClausesRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configMainQueryOneLinerAction.setEnumValue(SelectOneLinerAction.KEEP_EXISTING);
		rule.configSubQueryOneLinerAction.setEnumValue(SelectOneLinerAction.KEEP_EXISTING);
		rule.configMaxSelectListLengthBeforeFrom.setValue(30);
		rule.configNewLineForFromWithJoins.setValue(true);
		rule.configSelectClauseIndent.setEnumValue(SelectClauseIndent.PLUS_2);
		rule.configSelectUnionIndent.setEnumValue(SelectUnionIndent.PLUS_0);
		rule.configNextSelectPos.setEnumValue(SelectUnionNextSelectPos.BELOW_PLUS_0);
		rule.configSelectUnionIntoIndent.setEnumValue(SelectUnionIntoIndent.PLUS_0);
	}

	@Test
	void testKeepOneLiners() {
		buildSrc("    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.");
		buildSrc("");
		buildSrc("    SELECT any_col");
		buildSrc("    FROM any_dtab");
		buildSrc("    INTO TABLE lt_any_table.");

		buildExp("    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.");
		buildExp("");
		buildExp("    SELECT any_col FROM any_dtab");
		buildExp("      INTO TABLE lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateOneLinerButSplitOverlength() {
		rule.configMaxLineLength.setValue(90);
		rule.configMainQueryOneLinerAction.setEnumValue(SelectOneLinerAction.CREATE);

		buildSrc("    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.");
		buildSrc("");
		buildSrc("    SELECT any_col");
		buildSrc("    FROM any_dtab");
		buildSrc("    INTO TABLE lt_any_table.");

		buildExp("    SELECT SINGLE any_col INTO lv_any_value");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE other_col = gc_any_constant.");
		buildExp("");
		buildExp("    SELECT any_col FROM any_dtab INTO TABLE lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysSplitOneLiner() {
		rule.configMainQueryOneLinerAction.setEnumValue(SelectOneLinerAction.ALWAYS_SPLIT);

		buildSrc("    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant.");

		buildExp("    SELECT SINGLE any_col INTO lv_any_value");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE other_col = gc_any_constant.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitSubQueryOneLiner() {
		rule.configSubQueryOneLinerAction.setEnumValue(SelectOneLinerAction.ALWAYS_SPLIT);

		buildSrc("    SELECT any_col, other_col, third_col FROM any_dtab APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table");
		buildSrc("           WHERE other_col IN ( SELECT other_col FROM other_dtab WHERE fourth_col = @lv_any_value )");
		buildSrc("             AND third_col IN ( SELECT third_col FROM third_dtab WHERE fifth_col > @lv_other_value ). \"#EC CI_BUFFSUBQ.");

		buildExp("    SELECT any_col, other_col, third_col");
		buildExp("      FROM any_dtab");
		buildExp("      APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table");
		buildExp("      WHERE other_col IN ( SELECT other_col FROM other_dtab");
		buildExp("                             WHERE fourth_col = @lv_any_value )");
		buildExp("        AND third_col IN ( SELECT third_col FROM third_dtab");
		buildExp("                             WHERE fifth_col > @lv_other_value ). \"#EC CI_BUFFSUBQ.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveFromToNextLine() {
		rule.configMaxSelectListLengthBeforeFrom.setValue(10);

		buildSrc("    SELECT any_col FROM any_dtab");
		buildSrc("        WHERE any_col IN ( SELECT any_col FROM other_dtab");
		buildSrc("         WHERE other_col = 'X' )");
		buildSrc("          INTO CORRESPONDING FIELDS OF @ls_any_struc. \"#EC CI_BUFFSUBQ");
		buildSrc("    ENDSELECT.");

		buildExp("    SELECT any_col");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE any_col IN ( SELECT any_col");
		buildExp("                           FROM other_dtab");
		buildExp("                           WHERE other_col = 'X' )");
		buildExp("      INTO CORRESPONDING FIELDS OF @ls_any_struc. \"#EC CI_BUFFSUBQ");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepFromWithJoinsBehindSelect() {
		rule.configNewLineForFromWithJoins.setValue(false);

		buildSrc("    SELECT FROM any_dtab AS t1");
		buildSrc("                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildSrc("                                            AND t1~other_col = t2~other_col");
		buildSrc("                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildSrc("      FIELDS (lt_fields)");
		buildSrc("       GROUP BY (lt_group)");
		buildSrc("             INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		buildExp("    SELECT FROM any_dtab AS t1");
		buildExp("                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildExp("                                            AND t1~other_col = t2~other_col");
		buildExp("                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildExp("      FIELDS (lt_fields)");
		buildExp("      GROUP BY (lt_group)");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveFromWithJoinsToNextLine() {
		buildSrc("    SELECT FROM any_dtab AS t1");
		buildSrc("                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildSrc("                                            AND t1~other_col = t2~other_col");
		buildSrc("                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildSrc("      FIELDS (lt_fields)");
		buildSrc("       GROUP BY (lt_group)");
		buildSrc("             INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildExp("                                       AND t1~other_col = t2~other_col");
		buildExp("           LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildExp("      FIELDS (lt_fields)");
		buildExp("      GROUP BY (lt_group)");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepFromWithJoinsBehindSelectForPlus7() {
		// in the special case 'FROM' being directly after 'SELECT', and indent +7 for the clauses, 
		// there is no point in moving 'FROM' down by one line
		rule.configSelectClauseIndent.setEnumValue(SelectClauseIndent.PLUS_7);

		buildSrc("    SELECT FROM any_dtab AS t1");
		buildSrc("                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildSrc("                                            AND t1~other_col = t2~other_col");
		buildSrc("                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildSrc("      FIELDS (lt_fields)");
		buildSrc("       GROUP BY (lt_group)");
		buildSrc("             INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		buildExp("    SELECT FROM any_dtab AS t1");
		buildExp("                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col");
		buildExp("                                            AND t1~other_col = t2~other_col");
		buildExp("                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col");
		buildExp("           FIELDS (lt_fields)");
		buildExp("           GROUP BY (lt_group)");
		buildExp("           INTO CORRESPONDING FIELDS OF TABLE @lts_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIndentClausesBy7() {
		rule.configSubQueryOneLinerAction.setEnumValue(SelectOneLinerAction.ALWAYS_SPLIT);
		rule.configSelectClauseIndent.setEnumValue(SelectClauseIndent.PLUS_7);

		buildSrc("    SELECT any_col, other_col, third_col FROM any_dtab APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table");
		buildSrc("           WHERE other_col IN ( SELECT other_col FROM other_dtab WHERE fourth_col = @lv_any_value )");
		buildSrc("             AND third_col IN ( SELECT third_col FROM third_dtab WHERE fifth_col > @lv_other_value ). \"#EC CI_BUFFSUBQ.");

		buildExp("    SELECT any_col, other_col, third_col");
		buildExp("           FROM any_dtab");
		buildExp("           APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table");
		buildExp("           WHERE other_col IN ( SELECT other_col FROM other_dtab");
		buildExp("                                       WHERE fourth_col = @lv_any_value )");
		buildExp("             AND third_col IN ( SELECT third_col FROM third_dtab");
		buildExp("                                       WHERE fifth_col > @lv_other_value ). \"#EC CI_BUFFSUBQ.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnionAndSelectBelow() {
		buildSrc("    SELECT a AS c1, b AS c2, c AS c3");
		buildSrc("           FROM any_dtab");
		buildSrc("           UNION DISTINCT");
		buildSrc("    SELECT d AS c1, e AS c2, f AS c3");
		buildSrc("          FROM other_dtab");
		buildSrc("      UNION DISTINCT");
		buildSrc("        SELECT g AS c1, h AS c2, i AS c3");
		buildSrc("          FROM third_dtab");
		buildSrc("            INTO TABLE @FINAL(lt_distinct_result).");

		buildExp("    SELECT a AS c1, b AS c2, c AS c3");
		buildExp("      FROM any_dtab");
		buildExp("    UNION DISTINCT");
		buildExp("    SELECT d AS c1, e AS c2, f AS c3");
		buildExp("      FROM other_dtab");
		buildExp("    UNION DISTINCT");
		buildExp("    SELECT g AS c1, h AS c2, i AS c3");
		buildExp("      FROM third_dtab");
		buildExp("    INTO TABLE @FINAL(lt_distinct_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnionAdd2SelectAdd2() {
		rule.configSelectUnionIndent.setEnumValue(SelectUnionIndent.PLUS_2);
		rule.configNextSelectPos.setEnumValue(SelectUnionNextSelectPos.BELOW_PLUS_2);

		buildSrc("    SELECT a AS c1, b AS c2, c AS c3");
		buildSrc("           FROM any_dtab");
		buildSrc("           UNION DISTINCT");
		buildSrc("    SELECT d AS c1, e AS c2, f AS c3");
		buildSrc("          FROM other_dtab");
		buildSrc("      UNION DISTINCT");
		buildSrc("        SELECT g AS c1, h AS c2, i AS c3");
		buildSrc("          FROM third_dtab");
		buildSrc("            INTO TABLE @FINAL(lt_distinct_result).");
		buildSrc("");
		buildSrc("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildSrc("            FROM any_dtab");
		buildSrc("    INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildSrc("        FROM other_dtab");
		buildSrc("    UNION");
		buildSrc("    SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildSrc("               FROM third_dtab )");
		buildSrc("               INTO TABLE @FINAL(lt_result).");

		buildExp("    SELECT a AS c1, b AS c2, c AS c3");
		buildExp("      FROM any_dtab");
		buildExp("      UNION DISTINCT");
		buildExp("        SELECT d AS c1, e AS c2, f AS c3");
		buildExp("          FROM other_dtab");
		buildExp("          UNION DISTINCT");
		buildExp("            SELECT g AS c1, h AS c2, i AS c3");
		buildExp("              FROM third_dtab");
		buildExp("    INTO TABLE @FINAL(lt_distinct_result).");
		buildExp("");
		buildExp("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildExp("      FROM any_dtab");
		buildExp("      INTERSECT");
		buildExp("        ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildExp("            FROM other_dtab");
		buildExp("            UNION");
		buildExp("              SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildExp("                FROM third_dtab )");
		buildExp("    INTO TABLE @FINAL(lt_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnionAdd2SelectContinueOrBelowSecond() {
		rule.configSelectClauseIndent.setEnumValue(SelectClauseIndent.PLUS_7);
		rule.configSelectUnionIndent.setEnumValue(SelectUnionIndent.PLUS_7);
		rule.configNextSelectPos.setEnumValue(SelectUnionNextSelectPos.CONTINUE_OR_BELOW_SECOND);

		buildSrc("    SELECT a AS c1, b AS c2, c AS c3");
		buildSrc("           FROM any_dtab");
		buildSrc("           UNION DISTINCT");
		buildSrc("    SELECT d AS c1, e AS c2, f AS c3");
		buildSrc("          FROM other_dtab");
		buildSrc("      UNION DISTINCT");
		buildSrc("        SELECT g AS c1, h AS c2, i AS c3");
		buildSrc("          FROM third_dtab");
		buildSrc("            INTO TABLE @FINAL(lt_distinct_result).");
		buildSrc("");
		buildSrc("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildSrc("            FROM any_dtab");
		buildSrc("    INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildSrc("        FROM other_dtab");
		buildSrc("    UNION");
		buildSrc("    SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildSrc("               FROM third_dtab )");
		buildSrc("               INTO TABLE @FINAL(lt_result).");

		buildExp("    SELECT a AS c1, b AS c2, c AS c3");
		buildExp("           FROM any_dtab");
		buildExp("           UNION DISTINCT");
		buildExp("                 SELECT d AS c1, e AS c2, f AS c3");
		buildExp("                        FROM other_dtab");
		buildExp("                        UNION DISTINCT");
		buildExp("                              SELECT g AS c1, h AS c2, i AS c3");
		buildExp("                                     FROM third_dtab");
		buildExp("    INTO TABLE @FINAL(lt_distinct_result).");
		buildExp("");
		buildExp("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildExp("           FROM any_dtab");
		buildExp("           INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildExp("                              FROM other_dtab");
		buildExp("                              UNION SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildExp("                                           FROM third_dtab )");
		buildExp("    INTO TABLE @FINAL(lt_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnionAdd4SelectContinueIntoAdd2() {
		rule.configSelectUnionIndent.setEnumValue(SelectUnionIndent.PLUS_4);
		rule.configNextSelectPos.setEnumValue(SelectUnionNextSelectPos.CONTINUE);
		rule.configSelectUnionIntoIndent.setEnumValue(SelectUnionIntoIndent.PLUS_2);

		buildSrc("    SELECT a AS c1, b AS c2, c AS c3");
		buildSrc("           FROM any_dtab");
		buildSrc("           UNION DISTINCT");
		buildSrc("    SELECT d AS c1, e AS c2, f AS c3");
		buildSrc("          FROM other_dtab");
		buildSrc("      UNION DISTINCT");
		buildSrc("        SELECT g AS c1, h AS c2, i AS c3");
		buildSrc("          FROM third_dtab");
		buildSrc("            INTO TABLE @FINAL(lt_distinct_result).");
		buildSrc("");
		buildSrc("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildSrc("            FROM any_dtab");
		buildSrc("    INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildSrc("        FROM other_dtab");
		buildSrc("    UNION");
		buildSrc("    SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildSrc("               FROM third_dtab )");
		buildSrc("               INTO TABLE @FINAL(lt_result).");

		buildExp("    SELECT a AS c1, b AS c2, c AS c3");
		buildExp("      FROM any_dtab");
		buildExp("        UNION DISTINCT SELECT d AS c1, e AS c2, f AS c3");
		buildExp("                         FROM other_dtab");
		buildExp("                           UNION DISTINCT SELECT g AS c1, h AS c2, i AS c3");
		buildExp("                                            FROM third_dtab");
		buildExp("      INTO TABLE @FINAL(lt_distinct_result).");
		buildExp("");
		buildExp("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildExp("      FROM any_dtab");
		buildExp("        INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildExp("                      FROM other_dtab");
		buildExp("                        UNION SELECT i AS c1, j AS c2, k AS c3, l AS c4");
		buildExp("                                FROM third_dtab )");
		buildExp("      INTO TABLE @FINAL(lt_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnionWithMultipleParentheses() {
		rule.configSelectUnionIndent.setEnumValue(SelectUnionIndent.PLUS_2);
		rule.configNextSelectPos.setEnumValue(SelectUnionNextSelectPos.CONTINUE);

		buildSrc("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildSrc("            FROM any_dtab");
		buildSrc("    INTERSECT ( ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildSrc("        FROM other_dtab");
		buildSrc("    EXCEPT");
		buildSrc("    SELECT h AS c1, i AS c2, j AS c3, k AS c4");
		buildSrc("        FROM third_dtab )");
		buildSrc("    UNION");
		buildSrc("    SELECT l AS c1, m AS c2, n AS c3, o AS c4");
		buildSrc("               FROM fourth_dtab )");
		buildSrc("               INTO TABLE @FINAL(lt_result).");

		buildExp("    SELECT a AS c1, b AS c2, c AS c3, d AS c4");
		buildExp("      FROM any_dtab");
		buildExp("      INTERSECT ( ( SELECT d AS c1, e AS c2, f AS c3, g AS c4");
		buildExp("                      FROM other_dtab");
		buildExp("                      EXCEPT SELECT h AS c1, i AS c2, j AS c3, k AS c4");
		buildExp("                               FROM third_dtab )");
		buildExp("                    UNION SELECT l AS c1, m AS c2, n AS c3, o AS c4");
		buildExp("                            FROM fourth_dtab )");
		buildExp("    INTO TABLE @FINAL(lt_result).");

		testRule();
	}

	@Test
	void testWithCommonTableExpressions() {
		buildSrc("    WITH");
		buildSrc("      +any_cte AS (");
		buildSrc("        SELECT any_dtab~any_id, any_name, other_id, value_from, value_to FROM any_dtab");
		buildSrc("          INNER JOIN other_dtab ON other_dtab~any_id = any_dtab~any_id");
		buildSrc("             WHERE any_dtab~any_id BETWEEN @from_id AND @to_id ),");
		buildSrc("");
		buildSrc("      +other_cte AS (");
		buildSrc("        SELECT any_id, other_id, SUM( any_value ) AS sum_value");
		buildSrc("        FROM third_dtab WHERE any_id BETWEEN @from_id AND @to_id");
		buildSrc("       GROUP BY any_id, other_id ),");
		buildSrc("");
		buildSrc("      +result_cte( name, name2, name3, name4, name5 ) AS (");
		buildSrc("        SELECT any_name, c~other_id, value_from, value_to, sum_value");
		buildSrc("                 FROM +any_cte AS c");
		buildSrc("                   INNER JOIN +other_cte AS s ON  c~any_id = s~any_id");
		buildSrc("                                              AND c~other_id = s~other_id )");
		buildSrc("");
		buildSrc("      SELECT *");
		buildSrc("         FROM +result_cte");
		buildSrc("              ORDER BY name, name2 INTO TABLE @FINAL(result1).");

		buildExp("    WITH");
		buildExp("      +any_cte AS (");
		buildExp("        SELECT any_dtab~any_id, any_name, other_id, value_from, value_to");
		buildExp("          FROM any_dtab");
		buildExp("          INNER JOIN other_dtab ON other_dtab~any_id = any_dtab~any_id");
		buildExp("          WHERE any_dtab~any_id BETWEEN @from_id AND @to_id ),");
		buildExp("");
		buildExp("      +other_cte AS (");
		buildExp("        SELECT any_id, other_id, SUM( any_value ) AS sum_value");
		buildExp("          FROM third_dtab");
		buildExp("          WHERE any_id BETWEEN @from_id AND @to_id");
		buildExp("          GROUP BY any_id, other_id ),");
		buildExp("");
		buildExp("      +result_cte( name, name2, name3, name4, name5 ) AS (");
		buildExp("        SELECT any_name, c~other_id, value_from, value_to, sum_value");
		buildExp("          FROM +any_cte AS c");
		buildExp("            INNER JOIN +other_cte AS s ON  c~any_id = s~any_id");
		buildExp("                                       AND c~other_id = s~other_id )");
		buildExp("");
		buildExp("      SELECT * FROM +result_cte");
		buildExp("        ORDER BY name, name2");
		buildExp("        INTO TABLE @FINAL(result1).");

		testRule();
	}

	@Test
	void testIntoCorrespondingBehindSelectListWithTilde() {
		rule.configMaxSelectListLengthBeforeInto.setValue(80);

		buildSrc("    SELECT t1~any_col t1~other_col");
		buildSrc("      INTO    CORRESPONDING FIELDS OF TABLE lt_any_table");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("      INNER JOIN other_dtab AS t2 ON t2~fourth_col = t1~fourth_col");
		buildSrc("      FOR ALL ENTRIES IN it_any_table");
		buildSrc("      WHERE t1~any_col = it_any_table-any_comp");
		buildSrc("        AND t1~other_col = it_any_table-other_comp. \"#EC CI_BUFFJOIN");

		buildExp("    SELECT t1~any_col t1~other_col INTO CORRESPONDING FIELDS OF TABLE lt_any_table");
		buildExp("      FROM any_dtab AS t1");
		buildExp("      INNER JOIN other_dtab AS t2 ON t2~fourth_col = t1~fourth_col");
		buildExp("      FOR ALL ENTRIES IN it_any_table");
		buildExp("      WHERE t1~any_col = it_any_table-any_comp");
		buildExp("        AND t1~other_col = it_any_table-other_comp. \"#EC CI_BUFFJOIN");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWhitespaceAfterIntoLineBreakAfterWhere() {
		buildSrc("    SELECT SINGLE any_col");
		buildSrc("      INTO   lv_any_value");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE");
		buildSrc("        other_col = gc_any_constant.");

		buildExp("    SELECT SINGLE any_col INTO lv_any_value");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE other_col = gc_any_constant.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentAfterIntoAndWhereKeywords() {
		buildSrc("    SELECT SINGLE any_col");
		buildSrc("      INTO \" comment1");
		buildSrc("        lv_any_value");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE \" comment2");
		buildSrc("        other_col = gc_any_constant.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentAfterSelect() {
		buildSrc("   SELECT *");
		buildSrc("   FROM any_table");
		buildSrc("   INTO TABLE @lt_any.");
		buildSrc("");
		buildSrc("   SELECT * \" comment");
		buildSrc("   FROM any_table");
		buildSrc("   INTO TABLE @lt_any.");

		buildExp("   SELECT * FROM any_table");
		buildExp("     INTO TABLE @lt_any.");
		buildExp("");
		buildExp("   SELECT * \" comment");
		buildExp("     FROM any_table");
		buildExp("     INTO TABLE @lt_any.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFromCdsWithParameters() {
		// ensure that FROM with parameters is NOT moved behind SELECT * 
		buildSrc("    SELECT *");
		buildSrc("   FROM any_cds( iv_any_param   = @lv_any_value,");
		buildSrc("                 iv_other_param = @lv_other_value,");
		buildSrc("                 iv_third_param = @lv_third_value )");
		buildSrc("            ORDER BY any_col, other_col");
		buildSrc("     INTO TABLE @DATA(lv_fourth_value).");

		buildExp("    SELECT *");
		buildExp("      FROM any_cds( iv_any_param   = @lv_any_value,");
		buildExp("                    iv_other_param = @lv_other_value,");
		buildExp("                    iv_third_param = @lv_third_value )");
		buildExp("      ORDER BY any_col, other_col");
		buildExp("      INTO TABLE @DATA(lv_fourth_value).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectAggregate() {
		// ensure that a complex aggregate in the SELECT list can be parsed
		buildSrc("    SELECT SUM( CAST( dec2 AS DEC( 10,2 ) ) ) AS sum1");
		buildSrc("    FROM any_dtab INTO @lv_result.");

		buildExp("    SELECT SUM( CAST( dec2 AS DEC( 10,2 ) ) ) AS sum1");
		buildExp("      FROM any_dtab");
		buildExp("      INTO @lv_result.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDBHintsAndExtendedResult() {
		buildSrc("    FINAL(lo_extended_result) = NEW cl_osql_extended_result( iv_cached_view = abap_true ).");
		buildSrc("");
		buildSrc("    SELECT any_col, other_col");
		buildSrc("           FROM any_dtab");
		buildSrc("            GROUP BY any_col");
		buildSrc("         %_HINTS HDB 'RESULT_CACHE' \"#EC CI_HINTS");
		buildSrc("             INTO TABLE @FINAL(lt_any_table)");
		buildSrc("                  EXTENDED RESULT @lo_extended_result.");

		buildExp("    FINAL(lo_extended_result) = NEW cl_osql_extended_result( iv_cached_view = abap_true ).");
		buildExp("");
		buildExp("    SELECT any_col, other_col FROM any_dtab");
		buildExp("      GROUP BY any_col");
		buildExp("      %_HINTS HDB 'RESULT_CACHE' \"#EC CI_HINTS");
		buildExp("      INTO TABLE @FINAL(lt_any_table)");
		buildExp("           EXTENDED RESULT @lo_extended_result.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIntoIndicatorsStructure() {
		buildSrc("    SELECT SINGLE *");
		buildSrc("           FROM any_dtab");
		buildSrc("           WHERE any_col = @lv_any_value");
		buildSrc("           INTO @lv_other_value INDICATORS NULL STRUCTURE null_ind.");

		buildExp("    SELECT SINGLE * FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("      INTO @lv_other_value INDICATORS NULL STRUCTURE null_ind.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDBFeatureModePragma() {
		buildSrc("    SELECT any_col, COUNT(*) AS other_col");
		buildSrc("      FROM any_dtab ##DB_FEATURE_MODE[TABLE_LEN_MAX1]");
		buildSrc("     WHERE any_col IN @lt_any_table");
		buildSrc("      GROUP BY any_col");
		buildSrc("      INTO TABLE @lt_other_table.");

		buildExp("    SELECT any_col, COUNT(*) AS other_col");
		buildExp("      FROM any_dtab ##DB_FEATURE_MODE[TABLE_LEN_MAX1]");
		buildExp("      WHERE any_col IN @lt_any_table");
		buildExp("      GROUP BY any_col");
		buildExp("      INTO TABLE @lt_other_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
