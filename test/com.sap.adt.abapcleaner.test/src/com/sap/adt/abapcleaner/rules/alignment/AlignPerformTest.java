package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class AlignPerformTest extends RuleTestBase {
	private AlignPerformRule rule;
	
	AlignPerformTest() {
		super(RuleID.ALIGN_PERFORM);
		rule = (AlignPerformRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configParamCountAfterPerform.setValue(3);
		rule.configBreakAfterAdditions.setValue(false);
		rule.configAlignWithFormName.setValue(false);
		rule.configContinueAfterParamGroupKeyword.setValue(true);
		rule.configParamCountToCondense.setValue(16);
	}

	@Test
	void testContinueAfterPerform() {
		buildSrc("  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table");
		buildSrc("          USING lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM");
		buildSrc("    subr_name(prog_name)");
		buildSrc("    IF FOUND");
		buildSrc("    TABLES lt_any USING");
		buildSrc("    lv_any_value CHANGING lv_other_value.");

		buildExp("  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table");
		buildExp("                                                   lt_other_table");
		buildExp("                                            USING  lv_other_value.");
		buildExp("");
		buildExp("  PERFORM subr_name(prog_name) IF FOUND TABLES   lt_any");
		buildExp("                                        USING    lv_any_value");
		buildExp("                                        CHANGING lv_other_value.");

		testRule();
	}

	@Test
	void testBreakAfterPerform() {
		rule.configParamCountAfterPerform.setValue(2);

		buildSrc("  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table");
		buildSrc("          USING lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM");
		buildSrc("    subr_name(prog_name)");
		buildSrc("    IF FOUND");
		buildSrc("    TABLES lt_any USING");
		buildSrc("    lv_any_value CHANGING lv_other_value.");

		buildExp("  PERFORM third_subr IN PROGRAM any_program");
		buildExp("    TABLES lt_any_table");
		buildExp("           lt_other_table");
		buildExp("    USING  lv_other_value.");
		buildExp("");
		buildExp("  PERFORM subr_name(prog_name) IF FOUND");
		buildExp("    TABLES   lt_any");
		buildExp("    USING    lv_any_value");
		buildExp("    CHANGING lv_other_value.");

		testRule();
	}

	@Test
	void testBreafAfterAdditions() {
		rule.configBreakAfterAdditions.setValue(true);

		buildSrc("  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table");
		buildSrc("          USING lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM third_subr TABLES lt_any_table lt_other_table");
		buildSrc("          USING lv_other_value.");

		buildExp("  PERFORM third_subr IN PROGRAM any_program");
		buildExp("    TABLES lt_any_table");
		buildExp("           lt_other_table");
		buildExp("    USING  lv_other_value.");
		buildExp("");
		buildExp("  PERFORM third_subr TABLES lt_any_table");
		buildExp("                            lt_other_table");
		buildExp("                     USING  lv_other_value.");

		testRule();
	}

	@Test
	void testMoveIfFoundUp() {
		// ensure that 'IF FOUND' is moved up from the end of the statement (where ABAP syntax accepts it) in front of 
		// the first TABLES / USING / CHANGING keyword ('IF FOUND' refers to the subroutine, not the parameters!)
		
		buildSrc("  PERFORM (lv_subr_name) IN PROGRAM other_program");
		buildSrc("  TABLES lr_table_ref->*");
		buildSrc("    <lt_other_table>");
		buildSrc("    USING if_any_interface=>co_any_constant");
		buildSrc("      lo_any_instance->mv_any_attribute");
		buildSrc("      CHANGING <ls_any_structure>-any_component IF FOUND.");

		buildExp("  PERFORM (lv_subr_name) IN PROGRAM other_program IF FOUND");
		buildExp("    TABLES   lr_table_ref->*");
		buildExp("             <lt_other_table>");
		buildExp("    USING    if_any_interface=>co_any_constant");
		buildExp("             lo_any_instance->mv_any_attribute");
		buildExp("    CHANGING <ls_any_structure>-any_component.");

		testRule();
	}

	@Test
	void testAlignWithFormName() {
		rule.configAlignWithFormName.setValue(true);

		buildSrc("  PERFORM any_subroutine_with_a_long_name USING lv_any_value 42 'ABCDEFGHIJKLMN' gc_any_constant '3.14159265'.");
		buildSrc("");
		buildSrc("  PERFORM (lv_subr_name) IN PROGRAM other_program");
		buildSrc("  TABLES lr_table_ref->*");
		buildSrc("    <lt_other_table>");
		buildSrc("    USING if_any_interface=>co_any_constant");
		buildSrc("      lo_any_instance->mv_any_attribute");
		buildSrc("      CHANGING <ls_any_structure>-any_component IF FOUND.");

		buildExp("  PERFORM any_subroutine_with_a_long_name");
		buildExp("          USING lv_any_value");
		buildExp("                42");
		buildExp("                'ABCDEFGHIJKLMN'");
		buildExp("                gc_any_constant");
		buildExp("                '3.14159265'.");
		buildExp("");
		buildExp("  PERFORM (lv_subr_name) IN PROGRAM other_program IF FOUND");
		buildExp("          TABLES   lr_table_ref->*");
		buildExp("                   <lt_other_table>");
		buildExp("          USING    if_any_interface=>co_any_constant");
		buildExp("                   lo_any_instance->mv_any_attribute");
		buildExp("          CHANGING <ls_any_structure>-any_component.");

		testRule();
	}

	@Test
	void testBreakAfterTablesUsingAndChanging() {
		rule.configContinueAfterParamGroupKeyword.setValue(false);

		// test both a case where the parameters continue behind PERFORM, and where they are moved below PERFORM 
		buildSrc("  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table");
		buildSrc("          USING lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM (lv_subr_name) IN PROGRAM other_program");
		buildSrc("  TABLES lr_table_ref->*");
		buildSrc("    <lt_other_table>");
		buildSrc("    USING if_any_interface=>co_any_constant");
		buildSrc("      lo_any_instance->mv_any_attribute");
		buildSrc("      CHANGING <ls_any_structure>-any_component IF FOUND.");

		buildExp("  PERFORM third_subr IN PROGRAM any_program TABLES");
		buildExp("                                              lt_any_table");
		buildExp("                                              lt_other_table");
		buildExp("                                            USING");
		buildExp("                                              lv_other_value.");
		buildExp("");
		buildExp("  PERFORM (lv_subr_name) IN PROGRAM other_program IF FOUND");
		buildExp("    TABLES");
		buildExp("      lr_table_ref->*");
		buildExp("      <lt_other_table>");
		buildExp("    USING");
		buildExp("      if_any_interface=>co_any_constant");
		buildExp("      lo_any_instance->mv_any_attribute");
		buildExp("    CHANGING");
		buildExp("      <ls_any_structure>-any_component.");

		testRule();
	}

	@Test
	void testTableWithBrackets() {
		// since 'lt_any_table[]' is one single Token, expect parameter count to be 3 
		rule.configParamCountAfterPerform.setValue(3);
		rule.configParamCountToCondense.setValue(3);

		buildSrc("  PERFORM other_subr");
		buildSrc("    TABLES lt_any");
		buildSrc("    USING lt_any_table[] lv_other_table[].");

		buildExp("  PERFORM other_subr TABLES lt_any");
		buildExp("                     USING  lt_any_table[] lv_other_table[].");

		testRule();
	}

	@Test
	void testPerformOf() {
		buildSrc("  PERFORM lv_subr_index OF form_one form_two form_three form_four");
		buildSrc("   form_five form_six form_seven.");

		buildExp("  PERFORM lv_subr_index OF form_one");
		buildExp("                           form_two");
		buildExp("                           form_three");
		buildExp("                           form_four");
		buildExp("                           form_five");
		buildExp("                           form_six");
		buildExp("                           form_seven.");

		testRule();
	}

	@Test
	void testCondenseParameterLists() {
		rule.configParamCountToCondense.setValue(7);

		buildSrc("  PERFORM (lv_subr_name)  IN PROGRAM (lv_program_name) IF  FOUND");
		buildSrc("    USING 100  200   300 400   500 600 700 800 \" comment");
		buildSrc("       'abcde' 'fghij' 'klmno' 'pqrst'  'uwvxy'");
		buildSrc("      gc_any_constant gc_other_constant");
		buildSrc("   gc_third_constant gc_fourth_constant");
		buildSrc("    CHANGING lv_any_value lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM lv_subr_index OF form_one form_two form_three form_four");
		buildSrc("   form_five form_six form_seven.");

		buildExp("  PERFORM (lv_subr_name) IN PROGRAM (lv_program_name) IF FOUND");
		buildExp("    USING    100 200 300 400 500 600 700 800 \" comment");
		buildExp("             'abcde' 'fghij' 'klmno' 'pqrst' 'uwvxy' gc_any_constant gc_other_constant gc_third_constant");
		buildExp("             gc_fourth_constant");
		buildExp("    CHANGING lv_any_value lv_other_value.");
		buildExp("");
		buildExp("  PERFORM lv_subr_index OF form_one form_two form_three form_four form_five form_six form_seven.");

		testRule();
	}

	@Test
	void testCondenseWithLowMaximumLineLength() {
		rule.configMaxLineLength.setValue(80);
		rule.configParamCountToCondense.setValue(7);

		buildSrc("  PERFORM (lv_subr_name)  IN PROGRAM (lv_program_name) IF  FOUND");
		buildSrc("    USING 100  200   300 400   500 600 700 800 \" comment");
		buildSrc("       'abcde' 'fghij' 'klmno' 'pqrst'  'uwvxy'");
		buildSrc("      gc_any_constant gc_other_constant");
		buildSrc("   gc_third_constant gc_fourth_constant");
		buildSrc("    CHANGING lv_any_value lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM lv_subr_index OF form_one form_two form_three form_four");
		buildSrc("   form_five form_six form_seven.");

		buildExp("  PERFORM (lv_subr_name) IN PROGRAM (lv_program_name) IF FOUND");
		buildExp("    USING    100 200 300 400 500 600 700 800 \" comment");
		buildExp("             'abcde' 'fghij' 'klmno' 'pqrst' 'uwvxy' gc_any_constant");
		buildExp("             gc_other_constant gc_third_constant gc_fourth_constant");
		buildExp("    CHANGING lv_any_value lv_other_value.");
		buildExp("");
		buildExp("  PERFORM lv_subr_index OF form_one form_two form_three form_four form_five");
		buildExp("                           form_six form_seven.");

		testRule();
	}

	@Test
	void testCondenseAlreadyDone() {
		rule.configMaxLineLength.setValue(80);
		rule.configParamCountToCondense.setValue(7);

		// prepare condensed cases which are already formatted according to settings
		buildSrc("  PERFORM (lv_subr_name) IN PROGRAM (lv_program_name) IF FOUND");
		buildSrc("    USING    100 200 300 400 500 600 700 800 \" comment");
		buildSrc("             'abcde' 'fghij' 'klmno' 'pqrst' 'uwvxy' gc_any_constant");
		buildSrc("             gc_other_constant gc_third_constant gc_fourth_constant");
		buildSrc("    CHANGING lv_any_value lv_other_value.");
		buildSrc("");
		buildSrc("  PERFORM lv_subr_index OF form_one form_two form_three form_four form_five");
		buildSrc("                           form_six form_seven.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testPerformInSameProgram() {
		// the program name can be omitted if neither 'IF FOUND' nor any parameter follows; 
		// nevertheless, expect spaces to be condensed 
		buildSrc("  PERFORM   (lv_subr_name)  IN  PROGRAM.");

		buildExp("  PERFORM (lv_subr_name) IN PROGRAM.");

		putAnyFormAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPerformOnSkipped() {
		buildSrc("  PERFORM lv_subr_name ON COMMIT.");
		buildSrc("  PERFORM lv_subr_name ON COMMIT LEVEL idx.");
		buildSrc("  PERFORM lv_subr_name ON ROLLBACK.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testChainSkipped() {
		buildSrc("  PERFORM: lv_subr_name USING lv_any_value,");
		buildSrc("           lv_other_subr_nume IN PROGRAM any_program");
		buildSrc("    USING 1 '3.1415' lv_other_value.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testComments() {
		// ensure that comments in weird places won't break anything

		buildSrc("  PERFORM \" comment1");
		buildSrc("  third_subr IN \" comment2");
		buildSrc("  PROGRAM any_program \" comment3");
		buildSrc("    TABLES \" comment4");
		buildSrc("    lt_any_table \" comment5");
		buildSrc("    lt_other_table \" comment 6");
		buildSrc("    USING lv_other_value \" comment7");
		buildSrc("    .");

		buildExp("  PERFORM \" comment1");
		buildExp("  third_subr IN \" comment2");
		buildExp("  PROGRAM any_program \" comment3");
		buildExp("                      TABLES \" comment4");
		buildExp("                             lt_any_table \" comment5");
		buildExp("                             lt_other_table \" comment 6");
		buildExp("                      USING  lv_other_value \" comment7");
		buildExp("    .");

		putAnyFormAroundSrcAndExp();
		
		testRule();
	}
}
