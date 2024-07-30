package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignAbapDocTest extends RuleTestBase {
	private AlignAbapDocRule rule;
	
	AlignAbapDocTest() {
		super(RuleID.ALIGN_ABAP_DOC);
		rule = (AlignAbapDocRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossNonEmptyLines.setValue(true);
	}

	@Test
	void testAlignParametersAndRaising() {
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Any method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! @parameter iv_other_param | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"! @parameter ev_param_with_long_name | <p class=\"shorttext synchronized\" lang=\"en\">parameter with a long name</p>");
		buildSrc("    \"! @parameter et_any_table | <p class=\"shorttext synchronized\" lang=\"en\">any result table</p>");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING !iv_any_param            TYPE i");
		buildSrc("                !iv_other_param          TYPE string");
		buildSrc("      EXPORTING !ev_param_with_long_name TYPE i");
		buildSrc("                !et_any_table            TYPE ty_tt_table");
		buildSrc("      RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Any method description</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_param            | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildExp("    \"! @parameter iv_other_param          | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildExp("    \"! @parameter ev_param_with_long_name | <p class=\"shorttext synchronized\" lang=\"en\">parameter with a long name</p>");
		buildExp("    \"! @parameter et_any_table            | <p class=\"shorttext synchronized\" lang=\"en\">any result table</p>");
		buildExp("    \"! @raising   cx_any_exception        | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING !iv_any_param            TYPE i");
		buildExp("                !iv_other_param          TYPE string");
		buildExp("      EXPORTING !ev_param_with_long_name TYPE i");
		buildExp("                !et_any_table            TYPE ty_tt_table");
		buildExp("      RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testAlignAcrossEmptyLines() {
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_param       | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildExp("    \"!");
		buildExp("    \"! @raising   cx_any_exception   | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildExp("    METHODS other_method");
		buildExp("      IMPORTING !iv_any_param   TYPE i");
		buildExp("                !iv_other_param TYPE string");
		buildExp("      RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAcrossNonEmptyLines() {
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Third method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! Further description of any parameter. This text is not synchronized, but it is displayed in ADT.");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"! Further description of other parameter.");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    \"! Detailed description of the exception.");
		buildSrc("    METHODS third_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Third method description</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_param       | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildExp("    \"! Further description of any parameter. This text is not synchronized, but it is displayed in ADT.");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildExp("    \"! Further description of other parameter.");
		buildExp("    \"! @raising   cx_any_exception   | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildExp("    \"! Detailed description of the exception.");
		buildExp("    METHODS third_method");
		buildExp("      IMPORTING !iv_any_param   TYPE i");
		buildExp("                !iv_other_param TYPE string");
		buildExp("      RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAlignAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDonNotAlignAcrossNonEmptyLines() {
		rule.configAlignAcrossNonEmptyLines.setValue(false);
		
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Third method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! Further description of any parameter. This text is not synchronized, but it is displayed in ADT.");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"! Further description of other parameter.");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    \"! Detailed description of the exception.");
		buildSrc("    METHODS third_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAlignIfDocuInNextLine() {
		// even with alignment across empty and non-empty lines, expect the following ABAP Doc to NOT be aligned, 
		// because the documentation is (partly) in the next line:
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param |");
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! @parameter iv_other_parameter |<p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"! @raising cx_any_exception |");
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAlignDetachedAbapDoc() {
		// expect the following ABAP Doc to NOT be aligned, because there is a non-ABAP-Doc comment line following it:
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"en\">Other method description</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_param | <p class=\"shorttext synchronized\" lang=\"en\">any parameter</p>");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter</p>");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\" lang=\"en\">any exception</p>");
		buildSrc("    \" another comment that is NOT ABAP Doc");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOtherAbapDocWithPipe() {
		// expect the following to be not changed
		buildSrc("    \" |");
		buildSrc("    \" any | text");
		buildSrc("    \" any text with | pipe character inside");
		buildSrc("    \" any text ending with pipe character |");
		buildSrc("    \" | any text starting with pipe character");
		buildSrc("    \"! |");
		buildSrc("    \"! any | text");
		buildSrc("    \"! any text with | pipe character inside");
		buildSrc("    \"! any text ending with pipe character |");
		buildSrc("    \"! | any text starting with pipe character");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING !iv_any_param   TYPE i");
		buildSrc("                !iv_other_param TYPE string");
		buildSrc("      RAISING   cx_any_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}
