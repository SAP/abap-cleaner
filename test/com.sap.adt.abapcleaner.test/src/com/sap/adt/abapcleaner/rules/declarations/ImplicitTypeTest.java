package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class ImplicitTypeTest extends RuleTestBase {
	private ImplicitTypeRule rule;
	
	ImplicitTypeTest() {
		super(RuleID.IMPLICIT_TYPE);
		rule = (ImplicitTypeRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnTypes.setValue(true);
		rule.configExecuteOnConstantsAndStatics.setValue(true);
		rule.configExecuteOnDataAndClassData.setValue(true);
		rule.configReplaceParenthesisWithLength.setValue(true);
	}
	

	@Test
	void testTypes() {
		buildSrc("    TYPES: ty_b(5),");
		buildSrc("           ty_c LENGTH 5.");
		buildSrc("");
		buildSrc("    TYPES: BEGIN OF ty_s_implicit,");
		buildSrc("             b(5),");
		buildSrc("             c LENGTH 5,");
		buildSrc("           END OF ty_s_implicit.");

		buildExp("    TYPES: ty_b TYPE c LENGTH 5,");
		buildExp("           ty_c TYPE c LENGTH 5.");
		buildExp("");
		buildExp("    TYPES: BEGIN OF ty_s_implicit,");
		buildExp("             b TYPE c LENGTH 5,");
		buildExp("             c TYPE c LENGTH 5,");
		buildExp("           END OF ty_s_implicit.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	

	@Test
	void testTypesUnchanged() {
		rule.configExecuteOnTypes.setValue(false);

		buildSrc("    TYPES: ty_b(5),");
		buildSrc("           ty_c LENGTH 5.");
		buildSrc("");
		buildSrc("    TYPES: BEGIN OF ty_s_implicit,");
		buildSrc("             b(5),");
		buildSrc("             c LENGTH 5,");
		buildSrc("           END OF ty_s_implicit.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testConstants() {
		buildSrc("    CONSTANTS: gc_a VALUE 'a',");
		buildSrc("               gc_b(5) VALUE 'abcde',");
		buildSrc("               gc_d TYPE c VALUE 'a',");
		buildSrc("               gc_e TYPE n VALUE '2',");
		buildSrc("               gc_f TYPE x VALUE ''.");

		buildExp("    CONSTANTS: gc_a TYPE c LENGTH 1 VALUE 'a',");
		buildExp("               gc_b TYPE c LENGTH 5 VALUE 'abcde',");
		buildExp("               gc_d TYPE c LENGTH 1 VALUE 'a',");
		buildExp("               gc_e TYPE n LENGTH 1 VALUE '2',");
		buildExp("               gc_f TYPE x LENGTH 1 VALUE ''.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testConstantsUnchanged() {
		rule.configExecuteOnConstantsAndStatics.setValue(false);

		buildSrc("    CONSTANTS: gc_a VALUE 'a',");
		buildSrc("               gc_b(5) VALUE 'abcde',");
		buildSrc("               gc_d TYPE c VALUE 'a',");
		buildSrc("               gc_e TYPE n VALUE '2',");
		buildSrc("               gc_f TYPE x VALUE ''.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testStatics() {
		buildSrc("    STATICS:");
		buildSrc("      st_g TYPE p VALUE '123456789012345-',");
		buildSrc("      st_h(5) TYPE p VALUE '123456789-',");
		buildSrc("      st_i TYPE p LENGTH 3 VALUE '12345-',");
		buildSrc("      st_j TYPE p DECIMALS 4 VALUE '12345678901.2345-'.");

		buildExp("    STATICS:");
		buildExp("      st_g TYPE p LENGTH 8 DECIMALS 0 VALUE '123456789012345-',");
		buildExp("      st_h TYPE p LENGTH 5 DECIMALS 0 VALUE '123456789-',");
		buildExp("      st_i TYPE p LENGTH 3 DECIMALS 0 VALUE '12345-',");
		buildExp("      st_j TYPE p LENGTH 8 DECIMALS 4 VALUE '12345678901.2345-'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStaticsUnchanged() {
		rule.configExecuteOnConstantsAndStatics.setValue(false);

		buildSrc("    STATICS:");
		buildSrc("      st_g TYPE p VALUE '123456789012345-',");
		buildSrc("      st_h(5) TYPE p VALUE '123456789-',");
		buildSrc("      st_i TYPE p LENGTH 3 VALUE '12345-',");
		buildSrc("      st_j TYPE p DECIMALS 4 VALUE '12345678901.2345-'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testClassData() {
		buildSrc("    CLASS-DATA:");
		buildSrc("      gv_a, gv_b(5),");
		buildSrc("      gv_d TYPE c, gv_e TYPE n, gv_f TYPE x,");
		buildSrc("      \" comment");
		buildSrc("      gv_g TYPE p, gv_h(5) TYPE p, gv_i TYPE p LENGTH 3, gv_j TYPE p DECIMALS 4.");

		buildExp("    CLASS-DATA:");
		buildExp("      gv_a TYPE c LENGTH 1,");
		buildExp("      gv_b TYPE c LENGTH 5,");
		buildExp("      gv_d TYPE c LENGTH 1,");
		buildExp("      gv_e TYPE n LENGTH 1,");
		buildExp("      gv_f TYPE x LENGTH 1,");
		buildExp("      \" comment");
		buildExp("      gv_g TYPE p LENGTH 8 DECIMALS 0,");
		buildExp("      gv_h TYPE p LENGTH 5 DECIMALS 0,");
		buildExp("      gv_i TYPE p LENGTH 3 DECIMALS 0,");
		buildExp("      gv_j TYPE p LENGTH 8 DECIMALS 4.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClassDataUnchanged() {
		rule.configExecuteOnDataAndClassData.setValue(false);

		buildSrc("    CLASS-DATA:");
		buildSrc("      gv_a, gv_b(5),");
		buildSrc("      gv_d TYPE c, gv_e TYPE n, gv_f TYPE x,");
		buildSrc("      \" comment");
		buildSrc("      gv_g TYPE p, gv_h(5) TYPE p, gv_i TYPE p LENGTH 3, gv_j TYPE p DECIMALS 4.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testData() {
		buildSrc("    DATA lv_a.");
		buildSrc("    DATA lv_b(5).");
		buildSrc("    DATA lv_d TYPE c.");
		buildSrc("    DATA lv_e TYPE n.");
		buildSrc("    DATA lv_f TYPE x.");
		buildSrc("");
		buildSrc("    DATA lv_g TYPE p.");
		buildSrc("    DATA lv_h(5) TYPE p.");
		buildSrc("    DATA lv_i TYPE p LENGTH 3.");
		buildSrc("    DATA lv_j TYPE p DECIMALS 4.");

		buildExp("    DATA lv_a TYPE c LENGTH 1.");
		buildExp("    DATA lv_b TYPE c LENGTH 5.");
		buildExp("    DATA lv_d TYPE c LENGTH 1.");
		buildExp("    DATA lv_e TYPE n LENGTH 1.");
		buildExp("    DATA lv_f TYPE x LENGTH 1.");
		buildExp("");
		buildExp("    DATA lv_g TYPE p LENGTH 8 DECIMALS 0.");
		buildExp("    DATA lv_h TYPE p LENGTH 5 DECIMALS 0.");
		buildExp("    DATA lv_i TYPE p LENGTH 3 DECIMALS 0.");
		buildExp("    DATA lv_j TYPE p LENGTH 8 DECIMALS 4.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testDataUnchanged() {
		rule.configExecuteOnDataAndClassData.setValue(false);

		buildSrc("    DATA lv_a.");
		buildSrc("    DATA lv_b(5).");
		buildSrc("    DATA lv_d TYPE c.");
		buildSrc("    DATA lv_e TYPE n.");
		buildSrc("    DATA lv_f TYPE x.");
		buildSrc("");
		buildSrc("    DATA lv_g TYPE p.");
		buildSrc("    DATA lv_h(5) TYPE p.");
		buildSrc("    DATA lv_i TYPE p LENGTH 3.");
		buildSrc("    DATA lv_j TYPE p DECIMALS 4.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLengthInParensUnchanged() {
		rule.configReplaceParenthesisWithLength.setValue(false);

		buildSrc("CLASS cl_implicit_type DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    TYPES: ty_b(5),");
		buildSrc("           ty_c LENGTH 5.");
		buildSrc("");
		buildSrc("    TYPES: BEGIN OF ty_s_implicit,");
		buildSrc("             b(5),");
		buildSrc("             c LENGTH 5,");
		buildSrc("           END OF ty_s_implicit.");
		buildSrc("");
		buildSrc("    CONSTANTS: gc_a VALUE 'a',");
		buildSrc("               gc_b(5) VALUE 'abcde'.");
		buildSrc("");
		buildSrc("    CLASS-METHODS make_implicit_type_explicit.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_implicit_type IMPLEMENTATION.");
		buildSrc("  METHOD make_implicit_type_explicit.");
		buildSrc("    STATICS:");
		buildSrc("      st_g TYPE p VALUE '123456789012345-',");
		buildSrc("      st_h(5) TYPE p VALUE '123456789-'.");
		buildSrc("");
		buildSrc("    DATA lv_b(5).");
		buildSrc("    DATA lv_h(5) TYPE p.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_implicit_type DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    TYPES: ty_b(5) TYPE c,");
		buildExp("           ty_c TYPE c LENGTH 5.");
		buildExp("");
		buildExp("    TYPES: BEGIN OF ty_s_implicit,");
		buildExp("             b(5) TYPE c,");
		buildExp("             c TYPE c LENGTH 5,");
		buildExp("           END OF ty_s_implicit.");
		buildExp("");
		buildExp("    CONSTANTS: gc_a TYPE c LENGTH 1 VALUE 'a',");
		buildExp("               gc_b(5) TYPE c VALUE 'abcde'.");
		buildExp("");
		buildExp("    CLASS-METHODS make_implicit_type_explicit.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_implicit_type IMPLEMENTATION.");
		buildExp("  METHOD make_implicit_type_explicit.");
		buildExp("    STATICS:");
		buildExp("      st_g TYPE p LENGTH 8 DECIMALS 0 VALUE '123456789012345-',");
		buildExp("      st_h(5) TYPE p DECIMALS 0 VALUE '123456789-'.");
		buildExp("");
		buildExp("    DATA lv_b(5) TYPE c.");
		buildExp("    DATA lv_h(5) TYPE p DECIMALS 0.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}
	
	@Test
	void testEnumUnchanged() {
		rule.configExecuteOnDataAndClassData.setValue(false);

		buildSrc("    TYPES: BEGIN OF ENUM any_enum,");
		buildSrc("             any_value,");
		buildSrc("             other_value,");
		buildSrc("           END OF ENUM any_enum.");
		buildSrc("");
		buildSrc("    TYPES BEGIN OF ENUM other_enum.");
		buildSrc("    TYPES   any_value.");
		buildSrc("    TYPES   other_value.");
		buildSrc("    TYPES END OF ENUM other_enum.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainMixingImplicitAndExplicit() {
		// insert full explicit declarations in the middle / beginning / end of some lines; also, test a declaration with 
		// "LIKE" and a pragma in a wrong position (this would create a compiler warning, but it happens)
		buildSrc("    DATA:");
		buildSrc("      lv_a, lv_any TYPE any_type, lv_b(5),");
		buildSrc("      lv_other TYPE c LENGTH 2, lv_d TYPE c, ##NEEDED");
		buildSrc("      lv_e TYPE n, lv_f TYPE x, lv_third TYPE p LENGTH 5 DECIMALS 3,");
		buildSrc("      \" comment");
		buildSrc("      lv_g TYPE p, lv_h(5) TYPE p, lv_fourth LIKE lv_any, lv_i TYPE p LENGTH 3, lv_j TYPE p DECIMALS 4.");

		buildExp("    DATA:");
		buildExp("      lv_a TYPE c LENGTH 1,");
		buildExp("      lv_any TYPE any_type,");
		buildExp("      lv_b TYPE c LENGTH 5,");
		buildExp("      lv_other TYPE c LENGTH 2,");
		buildExp("      lv_d TYPE c LENGTH 1, ##NEEDED");
		buildExp("      lv_e TYPE n LENGTH 1,");
		buildExp("      lv_f TYPE x LENGTH 1,");
		buildExp("      lv_third TYPE p LENGTH 5 DECIMALS 3,");
		buildExp("      \" comment");
		buildExp("      lv_g TYPE p LENGTH 8 DECIMALS 0,");
		buildExp("      lv_h TYPE p LENGTH 5 DECIMALS 0,");
		buildExp("      lv_fourth LIKE lv_any,");
		buildExp("      lv_i TYPE p LENGTH 3 DECIMALS 0,");
		buildExp("      lv_j TYPE p LENGTH 8 DECIMALS 4.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataOccursUnchanged() {
		// ensure that obsolete TYPES/DATA ... OCCURS definitions are unchanged, because changing  
		// 'DATA lt_val(20) OCCURS 0 WITH HEADER LINE.' into
		// 'DATA lt_any TYPE c LENGTH 20 OCCURS 0 WITH HEADER LINE.' would be a syntax error
		
		buildSrc("  TYPES ty_tt(20) OCCURS 0.");
		buildSrc("  DATA lt_any(20) OCCURS 0 WITH HEADER LINE.");
		buildSrc("  STATICS  gt_any(20) OCCURS 0 WITH HEADER LINE.");
		buildSrc("  CONSTANTS ct_any(20) OCCURS 0 VALUE IS INITIAL."); // not a syntax error, although ADT highlights OCCURS in red

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		deactivateSyntaxCheckAfterParse();
		testRule();
	}

	@Test
	void testConstantLengthInParenthesis() {
		buildSrc("  CONSTANTS lc_length TYPE i VALUE 10.");
		buildSrc("");
		buildSrc("  CONSTANTS lc_other(lc_length) VALUE 'abcde'.");
		buildSrc("");
		buildSrc("  DATA lv_text(lc_length).");
		buildSrc("  DATA lv_text2(lc_length) TYPE c.");

		buildExp("  CONSTANTS lc_length TYPE i VALUE 10.");
		buildExp("");
		buildExp("  CONSTANTS lc_other TYPE c LENGTH lc_length VALUE 'abcde'.");
		buildExp("");
		buildExp("  DATA lv_text TYPE c LENGTH lc_length.");
		buildExp("  DATA lv_text2 TYPE c LENGTH lc_length.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testConstantLengthInParenthesisOfStructure() {
		buildSrc("  CONSTANTS lc_length TYPE i VALUE 10.");
		buildSrc("  TYPES: BEGIN OF ty_s_implicit,");
		buildSrc("           b(lc_length),");
		buildSrc("         END OF ty_s_implicit.");

		buildExp("  CONSTANTS lc_length TYPE i VALUE 10.");
		buildExp("  TYPES: BEGIN OF ty_s_implicit,");
		buildExp("           b TYPE c LENGTH lc_length,");
		buildExp("         END OF ty_s_implicit.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testReplaceTypeButNotLength() {
		rule.configReplaceParenthesisWithLength.setValue(false);

		buildSrc("    CONSTANTS lc_text_length TYPE i VALUE 10.");
		buildSrc("    DATA lv_k(lc_text_length).");

		buildExp("    CONSTANTS lc_text_length TYPE i VALUE 10.");
		buildExp("    DATA lv_k(lc_text_length) TYPE c.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
