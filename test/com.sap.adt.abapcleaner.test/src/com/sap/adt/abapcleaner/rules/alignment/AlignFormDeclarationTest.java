package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class AlignFormDeclarationTest extends RuleTestBase {
	private AlignFormDeclarationRule rule;
	
	AlignFormDeclarationTest() {
		super(RuleID.ALIGN_FORM_DECLARATION);
		rule = (AlignFormDeclarationRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configParamCountBehindFormName.setValue(3);
		rule.configContinueAfterParamGroupKeyword.setValue(true);
		rule.configAlignTypes.setValue(true);
		rule.configAddEmptyLine.setValue(true);
		rule.configRemoveEmptyLine.setValue(true);
	}

	@Test
	void testContinueAfterFormNameAddEmptyLine() {
		buildSrc("FORM other_form USING iv_any_value TYPE i iv_other_value TYPE string CHANGING cv_third_value TYPE i.");
		buildSrc("  \" other FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM other_form USING    iv_any_value   TYPE i");
		buildExp("                         iv_other_value TYPE string");
		buildExp("                CHANGING cv_third_value TYPE i.");
		buildExp("");
		buildExp("  \" other FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testBreakAfterFormName() {
		rule.configParamCountBehindFormName.setValue(1);

		buildSrc("FORM other_form USING iv_any_value TYPE i CHANGING cv_third_value TYPE i.");
		buildSrc("  \" other FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM other_form");
		buildExp("  USING    iv_any_value   TYPE i");
		buildExp("  CHANGING cv_third_value TYPE i.");
		buildExp("");
		buildExp("  \" other FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testContinueAfterFormNameBreakAfterKeywords() {
		rule.configContinueAfterParamGroupKeyword.setValue(false);

		buildSrc("FORM other_form USING iv_any_value TYPE i iv_other_value TYPE string CHANGING cv_third_value TYPE i.");
		buildSrc("ENDFORM.");

		buildExp("FORM other_form USING");
		buildExp("                  iv_any_value   TYPE i");
		buildExp("                  iv_other_value TYPE string");
		buildExp("                CHANGING");
		buildExp("                  cv_third_value TYPE i.");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testBreakAfterFormNameAddEmptyLine() {
		buildSrc("FORM third_form_with_a_long_name TABLES it_any_table STRUCTURE ty_s_any_struc");
		buildSrc("  it_other_table TYPE STANDARD TABLE it_third_table it_fourth_table TYPE ty_tt_any");
		buildSrc("  CHANGING ct_table TYPE ty_tt_table cs_struc TYPE LINE OF ty_tt_any cs_other_struc LIKE cs_any");
		buildSrc("    cs_third_struc LIKE LINE OF ct_table.");
		buildSrc("  \" third FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM third_form_with_a_long_name");
		buildExp("  TABLES   it_any_table    STRUCTURE ty_s_any_struc");
		buildExp("           it_other_table  TYPE STANDARD TABLE");
		buildExp("           it_third_table");
		buildExp("           it_fourth_table TYPE ty_tt_any");
		buildExp("  CHANGING ct_table        TYPE ty_tt_table");
		buildExp("           cs_struc        TYPE LINE OF ty_tt_any");
		buildExp("           cs_other_struc  LIKE cs_any");
		buildExp("           cs_third_struc  LIKE LINE OF ct_table.");
		buildExp("");
		buildExp("  \" third FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testBreakAfterFormNameBreakAfterKeywords() {
		// also ensure that RAISING class names do NOT affect parameter name column width
		rule.configContinueAfterParamGroupKeyword.setValue(false);

		buildSrc("FORM fourth_form");
		buildSrc("  USING");
		buildSrc("    VALUE(iv_any) TYPE string");
		buildSrc("    iv_other TYPE REF TO object");
		buildSrc("  RAISING");
		buildSrc("    cx_any_exception RESUMABLE(cx_other_exception) cx_third_exception.");
		buildSrc("  \" fourth FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM fourth_form");
		buildExp("  USING");
		buildExp("    VALUE(iv_any) TYPE string");
		buildExp("    iv_other      TYPE REF TO object");
		buildExp("  RAISING");
		buildExp("    cx_any_exception");
		buildExp("    RESUMABLE(cx_other_exception)");
		buildExp("    cx_third_exception.");
		buildExp("");
		buildExp("  \" fourth FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testDoNotAlignTypes() {
		rule.configAlignTypes.setValue(false);

		buildSrc("FORM third_form_with_a_long_name TABLES it_any_table STRUCTURE ty_s_any_struc");
		buildSrc("  it_other_table TYPE STANDARD TABLE it_third_table it_fourth_table TYPE ty_tt_any");
		buildSrc("  CHANGING ct_table TYPE ty_tt_table cs_struc TYPE LINE OF ty_tt_any cs_other_struc LIKE cs_any");
		buildSrc("    cs_third_struc LIKE LINE OF ct_table.");
		buildSrc("  \" third FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM third_form_with_a_long_name");
		buildExp("  TABLES   it_any_table STRUCTURE ty_s_any_struc");
		buildExp("           it_other_table TYPE STANDARD TABLE");
		buildExp("           it_third_table");
		buildExp("           it_fourth_table TYPE ty_tt_any");
		buildExp("  CHANGING ct_table TYPE ty_tt_table");
		buildExp("           cs_struc TYPE LINE OF ty_tt_any");
		buildExp("           cs_other_struc LIKE cs_any");
		buildExp("           cs_third_struc LIKE LINE OF ct_table.");
		buildExp("");
		buildExp("  \" third FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testDoNotAddEmptyLineAfterMultiLiner() {
		rule.configAddEmptyLine.setValue(false);

		buildSrc("FORM fourth_form");
		buildSrc("  USING");
		buildSrc("    VALUE(iv_any) TYPE string");
		buildSrc("    iv_other TYPE REF TO object.");
		buildSrc("  \" fourth FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM fourth_form USING VALUE(iv_any) TYPE string");
		buildExp("                       iv_other      TYPE REF TO object.");
		buildExp("  \" fourth FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testRemoveEmptyLineAfterOneLiner() {
		buildSrc("FORM any_form USING iv_any_value TYPE string.");
		buildSrc("");
		buildSrc("  \" any FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM any_form USING iv_any_value TYPE string.");
		buildExp("  \" any FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testAddEmptyLineBlocked() {
		// ensure that although the FORM declaration is processed, no empty line is added, because the rule is blocked
		// on the comment Command
		buildSrc("FORM fourth_form");
		buildSrc("  USING");
		buildSrc("    VALUE(iv_any) TYPE string");
		buildSrc("    iv_other TYPE REF TO object.");
		buildSrc("  \" fourth FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM fourth_form USING VALUE(iv_any) TYPE string");
		buildExp("                       iv_other      TYPE REF TO object.");
		buildExp("  \" fourth FORM implementation");
		buildExp("ENDFORM.");

		blockCommand(1);

		testRule();
	}

	@Test
	void testKeepEmptyLineAfterOneLiner() {
		rule.configRemoveEmptyLine.setValue(false);

		buildSrc("FORM any_form USING iv_any_value TYPE string.");
		buildSrc("");
		buildSrc("  \" any FORM implementation");
		buildSrc("ENDFORM.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testFormWithoutParameters() {
		buildSrc("FORM any_form.");
		buildSrc("  \" any FORM implementation");
		buildSrc("ENDFORM.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testEmptyFormWithoutParameters() {
		buildSrc("FORM any_form.");
		buildSrc("ENDFORM.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testComments() {
		// ensure that comments in weird places won't break anything

		buildSrc("FORM \" comment1");
		buildSrc("  other_form \" comment2");
		buildSrc("  USING \" commment3");
		buildSrc("  iv_any_value TYPE \" comment4");
		buildSrc("  i iv_other_value \" comment5");
		buildSrc("  TYPE string CHANGING cv_third_value \" comment6");
		buildSrc("  TYPE i \" comment7");
		buildSrc("  .");
		buildSrc("  \" other FORM implementation");
		buildSrc("ENDFORM.");

		buildExp("FORM \" comment1");
		buildExp("     other_form \" comment2");
		buildExp("                USING \" commment3");
		buildExp("                         iv_any_value   TYPE \" comment4");
		buildExp("                           i");
		buildExp("                         iv_other_value \" comment5");
		buildExp("                                        TYPE string");
		buildExp("                CHANGING cv_third_value \" comment6");
		buildExp("                                        TYPE i \" comment7");
		buildExp("  .");
		buildExp("");
		buildExp("  \" other FORM implementation");
		buildExp("ENDFORM.");

		testRule();
	}
}
