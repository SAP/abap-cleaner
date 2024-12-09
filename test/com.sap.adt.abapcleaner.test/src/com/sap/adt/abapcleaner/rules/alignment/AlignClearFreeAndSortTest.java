package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignClearFreeAndSortTest extends RuleTestBase {
	private AlignClearFreeAndSortRule rule;
	
	AlignClearFreeAndSortTest() {
		super(RuleID.ALIGN_CLEAR_FREE_AND_SORT);
		rule = (AlignClearFreeAndSortRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configDistinctLineClear.setEnumValue(DistinctLineClear.ALWAYS);
		rule.configDistinctLineFree.setEnumValue(DistinctLineFree.ALWAYS);
		rule.configDistinctLineSort.setEnumValue(DistinctLineSort.ALWAYS);
		rule.configDistinctLineCatch.setEnumValue(DistinctLineCatch.ALWAYS);
	}

	@Test
	void testClearAlwaysToDistinctLine() {
		buildSrc("    CLEAR: mv_any_value,");
		buildSrc("      mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("        ls_any_structure-other_component,");
		buildSrc("        mt_any_table, mt_other_table, mt_third_table.");
		buildSrc("");
		buildSrc("    CLEAR:   mv_any_value,");
		buildSrc("      mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildSrc("      mv_third_value WITH NULL,");
		buildSrc("         mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		buildExp("    CLEAR: mv_any_value,");
		buildExp("           mv_other_value,");
		buildExp("           ls_any_structure-any_component,");
		buildExp("           ls_any_structure-other_component,");
		buildExp("           mt_any_table,");
		buildExp("           mt_other_table,");
		buildExp("           mt_third_table.");
		buildExp("");
		buildExp("    CLEAR: mv_any_value,");
		buildExp("           mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildExp("           mv_third_value WITH NULL,");
		buildExp("           mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearOnlyAdditionsToDistinctLine() {
		rule.configDistinctLineClear.setEnumValue(DistinctLineClear.ONLY_WITH_ADDITIONS);

		buildSrc("    CLEAR: mv_any_value,");
		buildSrc("      mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("        ls_any_structure-other_component,");
		buildSrc("        mt_any_table, mt_other_table, mt_third_table.");
		buildSrc("");
		buildSrc("    CLEAR:   mv_any_value,");
		buildSrc("      mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildSrc("      mv_third_value WITH NULL,");
		buildSrc("         mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		buildExp("    CLEAR: mv_any_value, mv_other_value, ls_any_structure-any_component, ls_any_structure-other_component, mt_any_table,");
		buildExp("           mt_other_table, mt_third_table.");
		buildExp("");
		buildExp("    CLEAR: mv_any_value,");
		buildExp("           mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildExp("           mv_third_value WITH NULL,");
		buildExp("           mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearNeverToDistinctLine() {
		rule.configDistinctLineClear.setEnumValue(DistinctLineClear.NEVER);

		buildSrc("    CLEAR: mv_any_value,");
		buildSrc("      mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("        ls_any_structure-other_component,");
		buildSrc("        mt_any_table, mt_other_table, mt_third_table.");
		buildSrc("");
		buildSrc("    CLEAR:   mv_any_value,");
		buildSrc("      mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildSrc("      mv_third_value WITH NULL,");
		buildSrc("         mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		buildExp("    CLEAR: mv_any_value, mv_other_value, ls_any_structure-any_component, ls_any_structure-other_component, mt_any_table,");
		buildExp("           mt_other_table, mt_third_table.");
		buildExp("");
		buildExp("    CLEAR: mv_any_value, mv_other_value WITH lv_initial_value IN CHARACTER MODE, mv_third_value WITH NULL,");
		buildExp("           mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearWithShortLines() {
		rule.configMaxLineLength.setValue(80);
		rule.configDistinctLineClear.setEnumValue(DistinctLineClear.NEVER);

		buildSrc("    CLEAR: mv_any_value,");
		buildSrc("      mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("        ls_any_structure-other_component,");
		buildSrc("        mt_any_table, mt_other_table, mt_third_table.");
		buildSrc("");
		buildSrc("    CLEAR:   mv_any_value,");
		buildSrc("      mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildSrc("      mv_third_value WITH NULL,");
		buildSrc("         mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		buildExp("    CLEAR: mv_any_value, mv_other_value, ls_any_structure-any_component,");
		buildExp("           ls_any_structure-other_component, mt_any_table, mt_other_table,");
		buildExp("           mt_third_table.");
		buildExp("");
		buildExp("    CLEAR: mv_any_value, mv_other_value WITH lv_initial_value IN CHARACTER MODE,");
		buildExp("           mv_third_value WITH NULL,");
		buildExp("           mv_fourth_value WITH lv_initial_value IN BYTE MODE.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearWithLineBreakAfterKeyword() {
		// ensure that with a line break after the CLEAR: keyword, all variables align with the given position 
		// of the first variable 
		buildSrc("    CLEAR:");
		buildSrc("      mv_any_value,");
		buildSrc("       mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("ls_any_structure-other_component,");
		buildSrc("      mt_any_table, mt_other_table, mt_third_table.");

		buildExp("    CLEAR:");
		buildExp("      mv_any_value,");
		buildExp("      mv_other_value,");
		buildExp("      ls_any_structure-any_component,");
		buildExp("      ls_any_structure-other_component,");
		buildExp("      mt_any_table,");
		buildExp("      mt_other_table,");
		buildExp("      mt_third_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearWithFunction() {
		// ensure that the functional operands are moved along with the variables 
		buildSrc("    CLEAR:   lv_any_value WITH get_initial_value( iv_any_param   = 1");
		buildSrc("                                                  iv_other_param = 2 ) IN CHARACTER MODE,");
		buildSrc("   lv_other_value,");
		buildSrc("lv_third_value WITH get_initial_value(");
		buildSrc("                        iv_any_param   = 3");
		buildSrc("                        \" comment");
		buildSrc("                        iv_other_param = 4 ) IN BYTE MODE.");

		buildExp("    CLEAR: lv_any_value WITH get_initial_value( iv_any_param   = 1");
		buildExp("                                                iv_other_param = 2 ) IN CHARACTER MODE,");
		buildExp("           lv_other_value,");
		buildExp("           lv_third_value WITH get_initial_value(");
		buildExp("                                   iv_any_param   = 3");
		buildExp("                                   \" comment");
		buildExp("                                   iv_other_param = 4 ) IN BYTE MODE.");

		testRule();
	}

	@Test
	void testClearUnchanged() {
		rule.configDistinctLineClear.setEnumValue(DistinctLineClear.KEEP_AS_IS);

		buildSrc("    CLEAR: mv_any_value,");
		buildSrc("      mv_other_value,");
		buildSrc("         ls_any_structure-any_component,");
		buildSrc("        ls_any_structure-other_component,");
		buildSrc("        mt_any_table, mt_other_table, mt_third_table.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFreeAlwaysToDistinctLine() {
		buildSrc("    FREE: mt_any_table, mts_other_table,");
		buildSrc("         mth_third_table.");

		buildExp("    FREE: mt_any_table,");
		buildExp("          mts_other_table,");
		buildExp("          mth_third_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFreeNeverToDistinctLine() {
		rule.configDistinctLineFree.setEnumValue(DistinctLineFree.NEVER);

		buildSrc("    FREE: mt_any_table, mts_other_table,");
		buildSrc("         mth_third_table.");

		buildExp("    FREE: mt_any_table, mts_other_table, mth_third_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFreeWithComments() {
		rule.configDistinctLineFree.setEnumValue(DistinctLineFree.NEVER);

		buildSrc("    FREE:  mt_any_table, mts_other_table, \" comment");
		buildSrc("         mth_third_table,");
		buildSrc("* comment");
		buildSrc("       mth_fourth_table,");
		buildSrc("      mts_fifth_table.");

		buildExp("    FREE: mt_any_table, mts_other_table, \" comment");
		buildExp("          mth_third_table,");
		buildExp("* comment");
		buildExp("          mth_fourth_table, mts_fifth_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFreeUnchanged() {
		rule.configDistinctLineFree.setEnumValue(DistinctLineFree.KEEP_AS_IS);

		buildSrc("    FREE: mt_any_table, mts_other_table,");
		buildSrc("         mth_third_table.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortAlwaysToDistinctLine() {
		buildSrc("    SORT mt_any_table STABLE BY comp1 comp2");
		buildSrc("     comp3");
		buildSrc("     comp4.");
		buildSrc("");
		buildSrc("    SORT mt_other_table BY   comp1 comp2 DESCENDING");
		buildSrc("     comp3 comp4 AS TEXT.");

		buildExp("    SORT mt_any_table STABLE BY comp1");
		buildExp("                                comp2");
		buildExp("                                comp3");
		buildExp("                                comp4.");
		buildExp("");
		buildExp("    SORT mt_other_table BY comp1");
		buildExp("                           comp2 DESCENDING");
		buildExp("                           comp3");
		buildExp("                           comp4 AS TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortOnlyAdditionsToDistinctLine() {
		// ensure that additions ASCENDING, DESCENDING, AS TEXT after components cause distinct lines, 
		// but NOT if these additions only appear after the table name, as in the first example
		rule.configDistinctLineSort.setEnumValue(DistinctLineSort.ONLY_WITH_ADDITIONS);

		buildSrc("    SORT mt_any_table STABLE DESCENDING BY comp1 comp2");
		buildSrc("     comp3");
		buildSrc("     comp4.");
		buildSrc("");
		buildSrc("    SORT mt_other_table BY   comp1 comp2 DESCENDING");
		buildSrc("     comp3 comp4 AS TEXT.");

		buildExp("    SORT mt_any_table STABLE DESCENDING BY comp1 comp2 comp3 comp4.");
		buildExp("");
		buildExp("    SORT mt_other_table BY comp1");
		buildExp("                           comp2 DESCENDING");
		buildExp("                           comp3");
		buildExp("                           comp4 AS TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortNeverToDistinctLine() {
		rule.configDistinctLineSort.setEnumValue(DistinctLineSort.NEVER);

		buildSrc("    SORT mt_any_table STABLE BY comp1 comp2");
		buildSrc("     comp3");
		buildSrc("     comp4.");
		buildSrc("");
		buildSrc("    SORT mt_other_table BY   comp1 comp2 DESCENDING");
		buildSrc("     comp3 comp4 AS TEXT.");

		buildExp("    SORT mt_any_table STABLE BY comp1 comp2 comp3 comp4.");
		buildExp("");
		buildExp("    SORT mt_other_table BY comp1 comp2 DESCENDING comp3 comp4 AS TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortWithDistinctLineSections() {
		// ensure that components that span multiple lines (such as comp2 ... AS TEXT) are moved as a whole
		buildSrc("    SORT mt_other_table");
		buildSrc("      BY comp1");
		buildSrc("      comp2");
		buildSrc("        \" comment explaining DESCENDING");
		buildSrc("        DESCENDING");
		buildSrc("        \" comment explaining AS TEXT");
		buildSrc("        AS TEXT");
		buildSrc("      comp3");
		buildSrc("        \" comment between components");
		buildSrc("     comp4 AS TEXT.");

		buildExp("    SORT mt_other_table");
		buildExp("      BY comp1");
		buildExp("         comp2");
		buildExp("           \" comment explaining DESCENDING");
		buildExp("           DESCENDING");
		buildExp("           \" comment explaining AS TEXT");
		buildExp("           AS TEXT");
		buildExp("         comp3");
		buildExp("           \" comment between components");
		buildExp("         comp4 AS TEXT.");

		testRule();
	}

	@Test
	void testNothingToAlign() {
		buildSrc("    \" comment"); 
		buildSrc("    CLEAR: mv_any_value.");
		buildSrc("    FREE: mt_any_table.");
		buildSrc("    SORT mt_any_table BY comp1.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortWithoutByUnchanged() {
		// ensure that cases without BY are skipped 
		
		buildSrc("    SORT mt_any_table.");
		buildSrc("    SORT mt_any_table DESCENDING.");
		buildSrc("    SORT mt_other_table STABLE AS TEXT.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortBySortOrderTabUnchanged() {
		// ensure that cases in which an ABAP_SORTORDER_TAB is provided are skipped 
		// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapsort_itab.htm#!ABAP_ADDITION_5@5@
		// and https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapsort_itab.htm#!ABAP_ADDITION_6@6@
		
		buildSrc("    SORT mt_any_table STABLE BY (lt_sortorder_tab).");
		buildSrc("");
		buildSrc("    SORT mt_any_table BY VALUE #( ( name = 'comp1' )");
		buildSrc("                                  ( name = 'comp2' ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortChain() {
		buildSrc("    SORT: lt_any    BY comp1 comp2 comp3,");
		buildSrc("          lt_other  BY comp1 component2,");
		buildSrc("          lt_third  BY (otab),");
		buildSrc("          lt_fourth STABLE AS TEXT");
		buildSrc("                    BY component1 ASCENDING component2 DESCENDING,");
		buildSrc("          lt_fifth  DESCENDING");
		buildSrc("                    BY component1 AS TEXT component2.");

		buildExp("    SORT: lt_any    BY comp1");
		buildExp("                       comp2");
		buildExp("                       comp3,");
		buildExp("          lt_other  BY comp1");
		buildExp("                       component2,");
		buildExp("          lt_third  BY (otab),");
		buildExp("          lt_fourth STABLE AS TEXT");
		buildExp("                    BY component1 ASCENDING");
		buildExp("                       component2 DESCENDING,");
		buildExp("          lt_fifth  DESCENDING");
		buildExp("                    BY component1 AS TEXT");
		buildExp("                       component2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortChainWithByInDifferentPlaces() {
		buildSrc("    SORT: lt_any BY comp1 comp2 comp3,");
		buildSrc("          lt_other BY comp1 component2,");
		buildSrc("          lt_third BY (otab),");
		buildSrc("          lt_fourth STABLE AS TEXT BY component1 ASCENDING component2 DESCENDING,");
		buildSrc("          lt_fifth DESCENDING BY component1 AS TEXT component2.");

		buildExp("    SORT: lt_any BY comp1");
		buildExp("                    comp2");
		buildExp("                    comp3,");
		buildExp("          lt_other BY comp1");
		buildExp("                      component2,");
		buildExp("          lt_third BY (otab),");
		buildExp("          lt_fourth STABLE AS TEXT BY component1 ASCENDING");
		buildExp("                                      component2 DESCENDING,");
		buildExp("          lt_fifth DESCENDING BY component1 AS TEXT");
		buildExp("                                 component2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSortUnchanged() {
		rule.configDistinctLineSort.setEnumValue(DistinctLineSort.KEEP_AS_IS);

		buildSrc("    SORT mt_any_table STABLE BY comp1 comp2");
		buildSrc("     comp3");
		buildSrc("     comp4.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchAlwaysToDistinctLine() {
		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("");
		buildSrc("      CATCH   cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception.");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception cx_seventh_exception cx_eighth_exception  cx_ninth_exception INTO lx_exception.");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("");
		buildExp("      CATCH cx_any_exception");
		buildExp("            cx_other_exception");
		buildExp("            cx_third_exception.");
		buildExp("");
		buildExp("      CATCH cx_fourth_exception");
		buildExp("            cx_fifth_exception");
		buildExp("            cx_sixth_exception");
		buildExp("            cx_seventh_exception");
		buildExp("            cx_eighth_exception");
		buildExp("            cx_ninth_exception INTO lx_exception.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchAlwaysToDistinctLineIntoMovedUp() {
		// ensure that with exception classes on distinct lines, "INTO oref" is moved behind the last exception class
		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("      CATCH cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception");
		buildSrc("        INTO DATA(lx_exception) ##NO_HANDLER.");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception");
		buildSrc("      INTO lx_exception.");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("      CATCH cx_any_exception");
		buildExp("            cx_other_exception");
		buildExp("            cx_third_exception INTO DATA(lx_exception) ##NO_HANDLER.");
		buildExp("");
		buildExp("      CATCH cx_fourth_exception");
		buildExp("            cx_fifth_exception");
		buildExp("            cx_sixth_exception INTO lx_exception.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchNeverToDistinctLine() {
		rule.configDistinctLineCatch.setEnumValue(DistinctLineCatch.NEVER);

		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("");
		buildSrc("      CATCH BEFORE UNWIND cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception.");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception cx_seventh_exception cx_eighth_exception  cx_ninth_exception INTO DATA(lx_exception).");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("");
		buildExp("      CATCH BEFORE UNWIND cx_any_exception cx_other_exception cx_third_exception.");
		buildExp("");
		buildExp("      CATCH cx_fourth_exception cx_fifth_exception cx_sixth_exception cx_seventh_exception cx_eighth_exception");
		buildExp("            cx_ninth_exception INTO DATA(lx_exception).");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchNeverToDistinctLineIntoOnOwnLine() {
		// ensure that "INTO oref" can be kept on its own line or, depending on maximum line length, moved to its own line

		rule.configMaxLineLength.setValue(80);
		rule.configDistinctLineCatch.setEnumValue(DistinctLineCatch.NEVER);

		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("      CATCH cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception INTO DATA(lx_exception) ##NO_HANDLER.");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception");
		buildSrc("      INTO lx_exception.");
		buildSrc("        \" handle exceptions");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("      CATCH cx_any_exception cx_other_exception cx_third_exception");
		buildExp("            INTO DATA(lx_exception) ##NO_HANDLER.");
		buildExp("");
		buildExp("      CATCH cx_fourth_exception cx_fifth_exception cx_sixth_exception");
		buildExp("            INTO lx_exception.");
		buildExp("        \" handle exceptions");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchWithShortLines() {
		rule.configMaxLineLength.setValue(80);
		rule.configDistinctLineCatch.setEnumValue(DistinctLineCatch.NEVER);

		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("");
		buildSrc("      CATCH cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception INTO lx_exception.");
		buildSrc("        \" handle exceptions");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception   cx_sixth_exception cx_seventh_exception cx_eighth_exception  cx_ninth_exception INTO lx_exception.");
		buildSrc("        \" handle other exceptions");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("");
		buildExp("      CATCH cx_any_exception cx_other_exception cx_third_exception");
		buildExp("            INTO lx_exception.");
		buildExp("        \" handle exceptions");
		buildExp("");
		buildExp("      CATCH cx_fourth_exception cx_fifth_exception cx_sixth_exception");
		buildExp("            cx_seventh_exception cx_eighth_exception cx_ninth_exception");
		buildExp("            INTO lx_exception.");
		buildExp("        \" handle other exceptions");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCatchUnchanged() {
		rule.configMaxLineLength.setValue(80);
		rule.configDistinctLineCatch.setEnumValue(DistinctLineCatch.KEEP_AS_IS);

		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("");
		buildSrc("      CATCH cx_any_exception  cx_other_exception");
		buildSrc("        cx_third_exception INTO lx_exception.");
		buildSrc("        \" handle exceptions");
		buildSrc("");
		buildSrc("      CATCH cx_fourth_exception cx_fifth_exception cx_sixth_exception cx_seventh_exception INTO lx_exception.");
		buildSrc("        \" handle other exceptions");
		buildSrc("    ENDTRY.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
