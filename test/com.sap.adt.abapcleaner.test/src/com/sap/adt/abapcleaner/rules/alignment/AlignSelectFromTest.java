package com.sap.adt.abapcleaner.rules.alignment;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignSelectFromTest extends RuleTestBase {
	private AlignSelectFromRule rule;
	
	AlignSelectFromTest() {
		super(RuleID.ALIGN_SELECT_FROM);
		rule = (AlignSelectFromRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configFirstTableNamePosition.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_2);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.BELOW_PLUS_2);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.CONTINUE);
		rule.configAlignAsAcrossJoins.setValue(false);
		rule.configAlignOnAcrossJoins.setValue(false);
		rule.configSelectClientPosition.setEnumValue(SelectFromClientPosition.OWN_LINE_AFTER_JOINS);
	}

	@Test
	void testIsConfigValueEnabled() {
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.CONTINUE);
		assertTrue(rule.isConfigValueEnabled(rule.configAlignOnAcrossJoins));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAsAcrossJoins)); // pars pro toto

		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN);
		assertFalse(rule.isConfigValueEnabled(rule.configAlignOnAcrossJoins));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAsAcrossJoins)); 

		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN_WORD_2);
		assertFalse(rule.isConfigValueEnabled(rule.configAlignOnAcrossJoins));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAsAcrossJoins)); 
	}
	
	@Test
	void testJoinBelowTableAfterJoin() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildSrc("          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                              AND t4~d = t3~d");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t1~b AS b1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~d AS d3,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildExp("           LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildExp("           INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildExp("                                             AND t4~d = t3~d");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t1~b AS b1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~d AS d3,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinBelowTableAfterJoinNested() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

		buildSrc("    SELECT");
		buildSrc("      FROM ( any_dtab AS t1");
		buildSrc("           INNER JOIN");
		buildSrc("             other_dtab AS t2 ON t2~any_col = t1~any_col )");
		buildSrc("           LEFT OUTER JOIN");
		buildSrc("           third_dtab AS t3 ON t3~other_col = t2~any_col");
		buildSrc("             INNER JOIN");
		buildSrc("             fourth_dtab AS t4 ON t4~other_col = t3~other_col");
		buildSrc("      FIELDS t1~third_col AS any_alias,");
		buildSrc("             t4~fifth_col AS other_alias");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM ( any_dtab AS t1");
		buildExp("             INNER JOIN other_dtab AS t2 ON t2~any_col = t1~any_col )");
		buildExp("           LEFT OUTER JOIN third_dtab AS t3 ON t3~other_col = t2~any_col");
		buildExp("           INNER JOIN fourth_dtab AS t4 ON t4~other_col = t3~other_col");
		buildExp("      FIELDS t1~third_col AS any_alias,");
		buildExp("             t4~fifth_col AS other_alias");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinBelowPlus2TableBelowPlus2AsAligned() {
		// rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_2);
		// rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.BELOW_PLUS_2);
		rule.configAlignAsAcrossJoins.setValue(true);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildSrc("          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                              AND t4~d = t3~d");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t1~b AS b1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~d AS d3,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab                AS t1");
		buildExp("             INNER JOIN");
		buildExp("               other_dtab          AS t2 ON t2~a = t1~a");
		buildExp("                 LEFT OUTER JOIN");
		buildExp("                   third_dtab      AS t3 ON t3~b = t2~a");
		buildExp("                     INNER JOIN");
		buildExp("                       fourth_dtab AS t4 ON  t4~c = t3~b");
		buildExp("                                     AND t4~d = t3~d");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t1~b AS b1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~d AS d3,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinBelowPlus2TableBelowPlus2AsAlignedNested() {
		// rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_2);
		// rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.BELOW_PLUS_2);
		rule.configAlignAsAcrossJoins.setValue(true);

		buildSrc("    SELECT");
		buildSrc("      FROM ( any_dtab AS t1");
		buildSrc("           INNER JOIN");
		buildSrc("             other_dtab AS t2 ON t2~any_col = t1~any_col )");
		buildSrc("           LEFT OUTER JOIN");
		buildSrc("           third_dtab AS t3 ON t3~other_col = t2~any_col");
		buildSrc("             INNER JOIN");
		buildSrc("             fourth_dtab AS t4 ON t4~other_col = t3~other_col");
		buildSrc("      FIELDS t1~third_col AS any_alias,");
		buildSrc("             t4~fifth_col AS other_alias");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM ( any_dtab       AS t1");
		buildExp("               INNER JOIN");
		buildExp("                 other_dtab AS t2 ON t2~any_col = t1~any_col )");
		buildExp("             LEFT OUTER JOIN");
		buildExp("               third_dtab      AS t3 ON t3~other_col = t2~any_col");
		buildExp("                 INNER JOIN");
		buildExp("                   fourth_dtab AS t4 ON t4~other_col = t3~other_col");
		buildExp("      FIELDS t1~third_col AS any_alias,");
		buildExp("             t4~fifth_col AS other_alias");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinBelowPlus4TableAfterJoinOnBelowPlus2() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_4);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN_WORD_2);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildSrc("          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                              AND t4~d = t3~d");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t1~b AS b1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~d AS d3,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("               INNER JOIN other_dtab AS t2");
		buildExp("                     ON t2~a = t1~a");
		buildExp("                   LEFT OUTER JOIN third_dtab AS t3");
		buildExp("                        ON t3~b = t2~a");
		buildExp("                       INNER JOIN fourth_dtab AS t4");
		buildExp("                             ON  t4~c = t3~b");
		buildExp("                                  AND t4~d = t3~d");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t1~b AS b1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~d AS d3,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinBelowPlus4TableAfterJoinOnBelowPlus2Nested() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_4);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN_WORD_2);

		buildSrc("    SELECT");
		buildSrc("      FROM ( any_dtab AS t1");
		buildSrc("           INNER JOIN");
		buildSrc("             other_dtab AS t2 ON t2~any_col = t1~any_col )");
		buildSrc("           LEFT OUTER JOIN");
		buildSrc("           third_dtab AS t3 ON t3~other_col = t2~any_col");
		buildSrc("             INNER JOIN");
		buildSrc("             fourth_dtab AS t4 ON t4~other_col = t3~other_col");
		buildSrc("      FIELDS t1~third_col AS any_alias,");
		buildSrc("             t4~fifth_col AS other_alias");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM ( any_dtab AS t1");
		buildExp("                 INNER JOIN other_dtab AS t2");
		buildExp("                       ON t2~any_col = t1~any_col )");
		buildExp("               LEFT OUTER JOIN third_dtab AS t3");
		buildExp("                    ON t3~other_col = t2~any_col");
		buildExp("                   INNER JOIN fourth_dtab AS t4");
		buildExp("                         ON t4~other_col = t3~other_col");
		buildExp("      FIELDS t1~third_col AS any_alias,");
		buildExp("             t4~fifth_col AS other_alias");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllBelowClientBelow() {
		rule.configFirstTableNamePosition.setEnumValue(SelectFromTablePosition.BELOW_PLUS_0);
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.BELOW_PLUS_0);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN);
		rule.configSelectClientPosition.setEnumValue(SelectFromClientPosition.OWN_LINE);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENT '123'");
		buildSrc("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildSrc("      INTO TABLE @FINAL(lt_any_table).");

		buildExp("    SELECT");
		buildExp("      FROM");
		buildExp("      any_dtab AS t1");
		buildExp("      LEFT OUTER JOIN");
		buildExp("      other_dtab AS t2");
		buildExp("      ON t1~any_col = t2~any_col");
		buildExp("      USING CLIENT '123'");
		buildExp("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildExp("      INTO TABLE @FINAL(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepClientHandling() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configSelectClientPosition.setEnumValue(SelectFromClientPosition.KEEP_AS_IS);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING ALL CLIENTS");
		buildSrc("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildSrc("      INTO TABLE @FINAL(lt_any_table).");
		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENTS IN @lt_client");
		buildSrc("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildSrc("      INTO TABLE @FINAL(lt_any_table).");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING ALL CLIENTS");
		buildExp("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildExp("      INTO TABLE @FINAL(lt_any_table).");
		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENTS IN @lt_client");
		buildExp("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildExp("      INTO TABLE @FINAL(lt_any_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClientToOwnLineOnlyAfterJoins() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

		buildSrc("    SELECT *");
		buildSrc("      FROM any_dtab CLIENT SPECIFIED");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");
		buildSrc("");
		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab");
		buildSrc("      USING CLIENT '123'");
		buildSrc("      FIELDS any_col, other_col");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");
		buildSrc("");
		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab \" comment");
		buildSrc("      USING CLIENT '123'");
		buildSrc("      FIELDS any_col, other_col");
		buildSrc("      INTO TABLE @DATA(lt_any_table).");
		buildSrc("");
		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENT '123'");
		buildSrc("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildSrc("      INTO TABLE @DATA(lt_other_table).");

		buildExp("    SELECT *");
		buildExp("      FROM any_dtab CLIENT SPECIFIED");
		buildExp("      INTO TABLE @DATA(lt_any_table).");
		buildExp("");
		buildExp("    SELECT");
		buildExp("      FROM any_dtab USING CLIENT '123'");
		buildExp("      FIELDS any_col, other_col");
		buildExp("      INTO TABLE @DATA(lt_any_table).");
		buildExp("");
		buildExp("    SELECT");
		buildExp("      FROM any_dtab \" comment");
		buildExp("      USING CLIENT '123'");
		buildExp("      FIELDS any_col, other_col");
		buildExp("      INTO TABLE @DATA(lt_any_table).");
		buildExp("");
		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col");
		buildExp("           USING CLIENT '123'");
		buildExp("      FIELDS t2~other_col, t1~any_col, t2~third_col");
		buildExp("      INTO TABLE @DATA(lt_other_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAsAndOn() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configAlignAsAcrossJoins.setValue(true);
		rule.configAlignOnAcrossJoins.setValue(true);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildSrc("          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                              AND t4~d = t3~d");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t1~b AS b1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~d AS d3,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab                   AS t1");
		buildExp("           INNER JOIN other_dtab      AS t2 ON t2~a = t1~a");
		buildExp("           LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildExp("           INNER JOIN fourth_dtab     AS t4 ON  t4~c = t3~b");
		buildExp("                                                 AND t4~d = t3~d");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t1~b AS b1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~d AS d3,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontAlignAsButAlignOn() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configAlignOnAcrossJoins.setValue(true);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a");
		buildSrc("          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                              AND t4~d = t3~d");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t1~b AS b1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~d AS d3,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2      ON t2~a = t1~a");
		buildExp("           LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a");
		buildExp("           INNER JOIN fourth_dtab AS t4     ON  t4~c = t3~b");
		buildExp("                                                 AND t4~d = t3~d");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t1~b AS b1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~d AS d3,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testHierarchyUnchanged() {
		buildSrc("    SELECT");
		buildSrc("      FROM HIERARCHY(");
		buildSrc("             SOURCE any_cds");
		buildSrc("             CHILD TO PARENT ASSOCIATION _relat");
		buildSrc("             START WHERE id = 'A'");
		buildSrc("             SIBLINGS ORDER BY id )");
		buildSrc("      FIELDS any_col,");
		buildSrc("             other_col,");
		buildSrc("             hierarchy_level");
		buildSrc("      INTO TABLE @DATA(hierarchy).");
		buildSrc("");
		buildSrc("    SELECT agg~*");
		buildSrc("           FROM HIERARCHY_DESCENDANTS_AGGREGATE(");
		buildSrc("                  SOURCE HIERARCHY(");
		buildSrc("                           SOURCE any_cds");
		buildSrc("                           CHILD TO PARENT ASSOCIATION _relat");
		buildSrc("                           START WHERE id = 'A'");
		buildSrc("                           SIBLINGS ORDER BY id )");
		buildSrc("                  MEASURES MIN( num ) AS min,");
		buildSrc("                           MAX( num ) AS max,");
		buildSrc("                           SUM( num ) AS sum,");
		buildSrc("                           COUNT( * ) AS cnt");
		buildSrc("                  WHERE hierarchy_level <= @level ) AS agg");
		buildSrc("           INTO TABLE @DATA(lt_any_table).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCdsViewWithParameter() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

		buildSrc("    SELECT * FROM any_cds( p_any = @cs_any_struc-any_comp ) AS t1");
		buildSrc("      INNER JOIN @lt_any_table AS t2 ON t2~any_col = t1~other_col AND t2~third_col = t1~fourth_col");
		buildSrc("      WHERE t1~fifth_col = @ls_any_struc-lo_any_instance->mv_any_attribute");
		buildSrc("        AND ( t1~col_6 EQ 'T1' OR t1~col_7 EQ 'T2' )");
		buildSrc("      APPENDING CORRESPONDING FIELDS OF TABLE @cs_any_struc-table ##DB_FEATURE_MODE[TABLE_LEN_MAX1].");

		buildExp("    SELECT * FROM any_cds( p_any = @cs_any_struc-any_comp ) AS t1");
		buildExp("                  INNER JOIN @lt_any_table AS t2 ON t2~any_col = t1~other_col AND t2~third_col = t1~fourth_col");
		buildExp("      WHERE t1~fifth_col = @ls_any_struc-lo_any_instance->mv_any_attribute");
		buildExp("        AND ( t1~col_6 EQ 'T1' OR t1~col_7 EQ 'T2' )");
		buildExp("      APPENDING CORRESPONDING FIELDS OF TABLE @cs_any_struc-table ##DB_FEATURE_MODE[TABLE_LEN_MAX1].");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWithCommonTableExpressions() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

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
		buildExp("        SELECT any_dtab~any_id, any_name, other_id, value_from, value_to");
		buildExp("          FROM any_dtab");
		buildExp("               INNER JOIN other_dtab ON other_dtab~any_id = any_dtab~any_id");
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
		buildExp("               INNER JOIN +other_cte AS s ON  c~any_id = s~any_id");
		buildExp("                                          AND c~other_id = s~other_id )");
		buildExp("");
		buildExp("      SELECT * FROM +result_cte");
		buildExp("        ORDER BY name, name2");
		buildExp("        INTO TABLE @FINAL(result1).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnAfterOn() {
		// rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_2);
		// rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.BELOW_PLUS_2);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN_PLUS_2);

		buildSrc("  SELECT");
		buildSrc("    FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~d = t1~d");
		buildSrc("        LEFT OUTER JOIN third_dtab AS t3");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON t4~l = t3~l ON t3~l = t2~d");
		buildSrc("    FIELDS t1~a AS a1,");
		buildSrc("           t2~d AS d2,");
		buildSrc("           t3~i AS i3,");
		buildSrc("           t4~l AS l4");
		buildSrc("    INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("  SELECT");
		buildExp("    FROM any_dtab AS t1");
		buildExp("           INNER JOIN");
		buildExp("             other_dtab AS t2");
		buildExp("               ON t2~d = t1~d");
		buildExp("               LEFT OUTER JOIN");
		buildExp("                 third_dtab AS t3");
		buildExp("                   INNER JOIN");
		buildExp("                     fourth_dtab AS t4");
		buildExp("                       ON t4~l = t3~l");
		buildExp("                   ON t3~l = t2~d");
		buildExp("    FIELDS t1~a AS a1,");
		buildExp("           t2~d AS d2,");
		buildExp("           t3~i AS i3,");
		buildExp("           t4~l AS l4");
		buildExp("    INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnAfterOnBelowJoin() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_2);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);
		rule.configSelectOnPosition.setEnumValue(SelectFromOnPosition.BELOW_JOIN);

		buildSrc("  SELECT");
		buildSrc("    FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~d = t1~d");
		buildSrc("        LEFT OUTER JOIN third_dtab AS t3");
		buildSrc("            INNER JOIN fourth_dtab AS t4 ON t4~l = t3~l ON t3~l = t2~d");
		buildSrc("    FIELDS t1~a AS a1,");
		buildSrc("           t2~d AS d2,");
		buildSrc("           t3~i AS i3,");
		buildSrc("           t4~l AS l4");
		buildSrc("    INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("  SELECT");
		buildExp("    FROM any_dtab AS t1");
		buildExp("           INNER JOIN other_dtab AS t2");
		buildExp("           ON t2~d = t1~d");
		buildExp("             LEFT OUTER JOIN third_dtab AS t3");
		buildExp("               INNER JOIN fourth_dtab AS t4");
		buildExp("               ON t4~l = t3~l");
		buildExp("             ON t3~l = t2~d");
		buildExp("    FIELDS t1~a AS a1,");
		buildExp("           t2~d AS d2,");
		buildExp("           t3~i AS i3,");
		buildExp("           t4~l AS l4");
		buildExp("    INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDynamicFrom() {
		buildSrc("    SELECT SINGLE ('any_col')");
		buildSrc("           FROM (from)");
		buildSrc("           WHERE (where)");
		buildSrc("           INTO ( NEW @lr_any ).");

		putAnyMethodAroundSrcAndExp();

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testParenthesesAtEndOfFrom() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);
		rule.configFurtherTableNamePositions.setEnumValue(SelectFromTablePosition.CONTINUE);

		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1 CROSS JOIN ( other_dtab AS t2 CROSS JOIN third_dtab AS t3 )");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t2~b AS b2,");
		buildSrc("             t3~c AS c3");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("           CROSS JOIN ( other_dtab AS t2");
		buildExp("                        CROSS JOIN third_dtab AS t3 )");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t2~b AS b2,");
		buildExp("             t3~c AS c3");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOnWithParenthesisAfterTableName() {
		buildSrc("      SELECT any_col");
		buildSrc("        APPENDING CORRESPONDING FIELDS OF TABLE ct_any_table");
		buildSrc("        FROM any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2");
		buildSrc("          ON (     t2~other_col = t1~other_col");
		buildSrc("               AND t2~third_col = t1~third_col )");
		buildSrc("        WHERE t2~other_col = mv_any_value.");

		buildExp("      SELECT any_col");
		buildExp("        APPENDING CORRESPONDING FIELDS OF TABLE ct_any_table");
		buildExp("        FROM any_dtab AS t1");
		buildExp("               INNER JOIN");
		buildExp("                 other_dtab AS t2 ON (     t2~other_col = t1~other_col");
		buildExp("                                       AND t2~third_col = t1~third_col )");
		buildExp("        WHERE t2~other_col = mv_any_value.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentBetweenTableNameAndOn() {
		buildSrc("    SELECT t1~any_col INTO TABLE lv_any_value");
		buildSrc("           FROM any_dtab AS t2 LEFT OUTER JOIN other_dtab AS t1 \" comment");
		buildSrc("           ON t2~any_col = t1~other_col AND");
		buildSrc("              t1~any_col = sy-langu");
		buildSrc("           ORDER BY t1~any_col t1~other_col.");

		buildExp("    SELECT t1~any_col INTO TABLE lv_any_value");
		buildExp("           FROM any_dtab AS t2");
		buildExp("                  LEFT OUTER JOIN");
		buildExp("                    other_dtab AS t1 \" comment");
		buildExp("                                     ON t2~any_col = t1~other_col AND");
		buildExp("                                        t1~any_col = sy-langu");
		buildExp("           ORDER BY t1~any_col t1~other_col.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultipleParentheses() {
		rule.configSelectJoinIndent.setEnumValue(SelectFromJoinIndent.PLUS_0);

		buildSrc("    SELECT");
		buildSrc("      FROM ( ( ( any_dtab AS t1");
		buildSrc("        INNER JOIN other_dtab AS t2 ON t2~a = t1~a )");
		buildSrc("        LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a )");
		buildSrc("          INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b");
		buildSrc("                                       AND t4~d = t3~d )");
		buildSrc("      FIELDS t1~a AS a1,");
		buildSrc("             t2~c AS c2,");
		buildSrc("             t3~e AS e3,");
		buildSrc("             t4~f AS f4");
		buildSrc("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM ( ( ( any_dtab AS t1");
		buildExp("                 INNER JOIN");
		buildExp("                   other_dtab AS t2 ON t2~a = t1~a )");
		buildExp("               LEFT OUTER JOIN");
		buildExp("                 third_dtab AS t3 ON t3~b = t2~a )");
		buildExp("             INNER JOIN");
		buildExp("               fourth_dtab AS t4 ON  t4~c = t3~b");
		buildExp("                              AND t4~d = t3~d )");
		buildExp("      FIELDS t1~a AS a1,");
		buildExp("             t2~c AS c2,");
		buildExp("             t3~e AS e3,");
		buildExp("             t4~f AS f4");
		buildExp("      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDBFeatureModePragma() {
		buildSrc("    SELECT any_col, COUNT(*) AS other_col");
		buildSrc("      FROM    any_dtab ##DB_FEATURE_MODE[TABLE_LEN_MAX1]");
		buildSrc("      WHERE any_col IN @lt_any_table");
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

	@Test
	void testJoinWithoutOptionalInner() {
		// test a case that only has 'JOIN' without the optional '[INNER [cardinality]]' 
		
		buildSrc("    SELECT");
		buildSrc("      FROM any_dtab AS t1");
		buildSrc("      JOIN other_dtab AS t2 ON t2~any_col = t1~any_col");
		buildSrc("      FIELDS DISTINCT t2~other_col");
		buildSrc("      WHERE t1~third_col = @lv_any_value");
		buildSrc("      INTO TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM any_dtab AS t1");
		buildExp("             JOIN");
		buildExp("               other_dtab AS t2 ON t2~any_col = t1~any_col");
		buildExp("      FIELDS DISTINCT t2~other_col");
		buildExp("      WHERE t1~third_col = @lv_any_value");
		buildExp("      INTO TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinWithoutInnerLeftOrRight() {
		buildSrc("    SELECT *");
		buildSrc("      FROM dtab1");
		buildSrc("      JOIN");
		buildSrc("      dtab2 ON dtab1~any_id = dtab2~any_id");
		buildSrc("      JOIN");
		buildSrc("      dtab3 ON dtab3~other_id = dtab2~other_id");
		buildSrc("      INTO TABLE @DATA(lt_table).");

		buildExp("    SELECT *");
		buildExp("      FROM dtab1");
		buildExp("             JOIN");
		buildExp("               dtab2 ON dtab1~any_id = dtab2~any_id");
		buildExp("                 JOIN");
		buildExp("                   dtab3 ON dtab3~other_id = dtab2~other_id");
		buildExp("      INTO TABLE @DATA(lt_table).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
