package com.sap.adt.abapcleaner.rules.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EmptyLinesWithinMethodsTest extends RuleTestBase {
	private EmptyLinesWithinMethodsRule rule;
	
	EmptyLinesWithinMethodsTest() {
		super(RuleID.EMPTY_LINES_WITHIN_METHODS);
		rule = (EmptyLinesWithinMethodsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxEmptyLinesAtMethodStart.setValue(0);
		rule.configMaxEmptyLinesWithinMethods.setValue(1);
		rule.configMaxEmptyLinesAtMethodEnd.setValue(0);
		rule.configEmptyLineAboveFirstExecutable.setValue(true);
	}
	
	@Test
	void testRemoveEmptyLinesAtMethodStart() {
		buildSrc("  METHOD empty_lines.");
		buildSrc("");
		buildSrc("    \" nothing happens here");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines.");
		buildExp("    \" nothing happens here");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testKeepEmptyLinesAtMethodStart() {
		rule.configMaxEmptyLinesAtMethodStart.setValue(2);

		buildSrc("  METHOD empty_lines.");
		buildSrc("");
		buildSrc("");
		buildSrc("    \" nothing happens here");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testRemoveEmptyLinesAtMethodEnd() {
		buildSrc("  METHOD empty_lines.");
		buildSrc("    \" nothing happens here");
		buildSrc("");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines.");
		buildExp("    \" nothing happens here");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testKeepEmptyLinesAtMethodEnd() {
		rule.configMaxEmptyLinesAtMethodEnd.setValue(2);

		buildSrc("  METHOD empty_lines.");
		buildSrc("    \" nothing happens here");
		buildSrc("");
		buildSrc("");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testEmptyLinesBetweenCommands() {
		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    do_nothing( ).");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    hesitate( ).");
		buildSrc("");
		buildSrc("");
		buildSrc("    procrastinate( ).");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    do_nothing( ).");
		buildExp("");
		buildExp("    hesitate( ).");
		buildExp("");
		buildExp("    procrastinate( ).");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testMax2EmptyLinesBetweenCommands() {
		rule.configMaxEmptyLinesWithinMethods.setValue(2);

		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    do_nothing( ).");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    hesitate( ).");
		buildSrc("");
		buildSrc("");
		buildSrc("    procrastinate( ).");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    do_nothing( ).");
		buildExp("");
		buildExp("");
		buildExp("    hesitate( ).");
		buildExp("");
		buildExp("");
		buildExp("    procrastinate( ).");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testEmptyLineAboveFirstExecutable() {
		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE i.");
		buildSrc("    do_something( ).");
		buildSrc("    do_something_more( ).");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE i.");
		buildExp("");
		buildExp("    do_something( ).");
		buildExp("    do_something_more( ).");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testEmptyLineAboveFirstExecutableOnly() {
		// ensure the empty line is only inserted above the first(!) executable statement, 
		// even if more declarations and further executable statements follow
		
		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    do_something( ).");
		buildSrc("    DATA lv_other TYPE i.");
		buildSrc("    do_something_more( ).");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    DATA lv_any   TYPE i.");
		buildExp("");
		buildExp("    do_something( ).");
		buildExp("    DATA lv_other TYPE i.");
		buildExp("    do_something_more( ).");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testEmptyLineAboveInlineDeclaration() {
		// ensure that inline declarations are correctly considered as executable statements

		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA(lo_util) = cl_factory=>get( )->get_utility( ).");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    DATA lv_any   TYPE i.");
		buildExp("");
		buildExp("    DATA(lo_util) = cl_factory=>get( )->get_utility( ).");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testEmptyLineAboveFirstExecutableWithComments() {
		// ensure that comments are kept with the first executable statement

		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    TYPES ty_any TYPE int8.");
		buildSrc("    \" empty line should be inserted above the comment lines");
		buildSrc("    \" that belong to the first executable statement");
		buildSrc("    rv_result = 1.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD empty_lines_within_methods.");
		buildExp("    TYPES ty_any TYPE int8.");
		buildExp("");
		buildExp("    \" empty line should be inserted above the comment lines");
		buildExp("    \" that belong to the first executable statement");
		buildExp("    rv_result = 1.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testNoEmptyLineAboveEndMethod() {
		// ensure that no empty line is inserted above ENDMETHOD, even if there is no executable statement in the method
		
		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    FIELD-SYMBOLS <ls_any> TYPE any_type.");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testNoLineAboveFirstExecutable() {
		rule.configEmptyLineAboveFirstExecutable.setValue(false);

		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    do_nothing( ).");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testNoLineAboveSecondExecutable() {
		buildSrc("  METHOD empty_lines_within_methods.");
		buildSrc("    first_executable( ).");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    second_executable( ).");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testEmptyLinesInEventBlocks() {
		// expect empty lines 
		// - at the beginning of event blocks to be removed
		// - within event blocks to be condensed to maximum 1 empty line
		// - at the end of event blocks (i.e. directly above a new event block) to be unchanged 
		//   (because this would rather be a case for the EmptyLinesOutsideMethodsRule)
		
	   buildSrc("REPORT zreport.");
		buildSrc("INITIALIZATION.");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("LOAD-OF-PROGRAM.");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("AT SELECTION-SCREEN.");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("FORM test_form.");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" code");
		buildSrc("");
		buildSrc("ENDFORM.");

	   buildExp("REPORT zreport.");
		buildExp("INITIALIZATION.");
		buildExp("  \" code");
		buildExp("");
		buildExp("  \" code");
		buildExp("");
		buildExp("");
		buildExp("LOAD-OF-PROGRAM.");
		buildExp("  \" code");
		buildExp("");
		buildExp("  \" code");
		buildExp("");
		buildExp("AT SELECTION-SCREEN.");
		buildExp("  \" code");
		buildExp("");
		buildExp("  \" code");
		buildExp("START-OF-SELECTION.");
		buildExp("  \" code");
		buildExp("");
		buildExp("  \" code");
		buildExp("FORM test_form.");
		buildExp("  \" code");
		buildExp("");
		buildExp("  \" code");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testIncludeType() {
		// ensure that the 'INCLUDE TYPE ...' Command is not mistaken as an executable command
		
		buildSrc("    TYPES: BEGIN OF ty_s_struc,");
		buildSrc("             comp TYPE i.");
		buildSrc("             INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES  END OF ty_s_struc.");
		buildSrc("    rv_result = 1.");

		buildExp("    TYPES: BEGIN OF ty_s_struc,");
		buildExp("             comp TYPE i.");
		buildExp("             INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES  END OF ty_s_struc.");
		buildExp("");
		buildExp("    rv_result = 1.");

		testRule();
	}
}
