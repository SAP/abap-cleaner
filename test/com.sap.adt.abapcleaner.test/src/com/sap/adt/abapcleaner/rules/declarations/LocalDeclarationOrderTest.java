package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class LocalDeclarationOrderTest extends RuleTestBase {
	private LocalDeclarationOrderRule rule;
	
	LocalDeclarationOrderTest() {
		super(RuleID.LOCAL_DECLARATION_ORDER);
		rule = (LocalDeclarationOrderRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configTypesOrder.setEnumValue(LocalTypesDeclarationOrder.METHOD_START_KEEP_ORDER);
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_KEEP_ORDER);
		rule.configStaticsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);

		rule.configDistinctBlocks.setValue(true);
		rule.configEmptyLine.setValue(true);
		rule.configMoveComments.setValue(true);
		rule.configRearrangeChains.setValue(true);
		rule.configConsiderComments.setValue(false);
	}

	@Test
	void testDistinctBlocksEmptyLineChangeOrder() {
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configStaticsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);

		buildSrc("    DATA lv_a8 TYPE i.");
		buildSrc("    STATICS sv_b2 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildSrc("    DATA: lv_d4 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildSrc("    STATICS sv_f5 TYPE i.");
		buildSrc("    CONSTANTS: lc_g6 TYPE i VALUE 1.");
		buildSrc("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildSrc("");
		buildSrc("    UNASSIGN <lv_e1>.");
		buildSrc("    CLEAR sv_b2.");
		buildSrc("    IF iv_case = lc_h3.");
		buildSrc("      CLEAR lv_d4.");
		buildSrc("      CLEAR sv_f5.");
		buildSrc("    ELSEIF iv_case = lc_g6.");
		buildSrc("      UNASSIGN <lv_c7>.");
		buildSrc("      CLEAR lv_a8.");
		buildSrc("    ENDIF.");

		buildExp("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildExp("    CONSTANTS: lc_g6 TYPE i VALUE 1.");
		buildExp("");
		buildExp("    STATICS sv_b2 TYPE i.");
		buildExp("    STATICS sv_f5 TYPE i.");
		buildExp("");
		buildExp("    DATA: lv_d4 TYPE i.");
		buildExp("    DATA lv_a8 TYPE i.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildExp("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildExp("");
		buildExp("    UNASSIGN <lv_e1>.");
		buildExp("    CLEAR sv_b2.");
		buildExp("    IF iv_case = lc_h3.");
		buildExp("      CLEAR lv_d4.");
		buildExp("      CLEAR sv_f5.");
		buildExp("    ELSEIF iv_case = lc_g6.");
		buildExp("      UNASSIGN <lv_c7>.");
		buildExp("      CLEAR lv_a8.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testDistinctBlocksEmptyLineKeepOrder() {
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_KEEP_ORDER);
		rule.configStaticsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_KEEP_ORDER);
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_KEEP_ORDER);
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_KEEP_ORDER);

		buildSrc("    DATA lv_a8 TYPE i.");
		buildSrc("    STATICS sv_b2 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildSrc("    DATA lv_d4 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildSrc("    STATICS sv_f5 TYPE i.");
		buildSrc("    CONSTANTS lc_g6 TYPE i VALUE 1.");
		buildSrc("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildSrc("");
		buildSrc("    UNASSIGN <lv_e1>.");
		buildSrc("    CLEAR sv_b2.");
		buildSrc("    IF iv_case = lc_h3.");
		buildSrc("      CLEAR lv_d4.");
		buildSrc("      CLEAR sv_f5.");
		buildSrc("    ELSEIF iv_case = lc_g6.");
		buildSrc("      UNASSIGN <lv_c7>.");
		buildSrc("      CLEAR lv_a8.");
		buildSrc("    ENDIF.");

		buildExp("    CONSTANTS lc_g6 TYPE i VALUE 1.");
		buildExp("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildExp("");
		buildExp("    STATICS sv_b2 TYPE i.");
		buildExp("    STATICS sv_f5 TYPE i.");
		buildExp("");
		buildExp("    DATA lv_a8 TYPE i.");
		buildExp("    DATA lv_d4 TYPE i.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildExp("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildExp("");
		buildExp("    UNASSIGN <lv_e1>.");
		buildExp("    CLEAR sv_b2.");
		buildExp("    IF iv_case = lc_h3.");
		buildExp("      CLEAR lv_d4.");
		buildExp("      CLEAR sv_f5.");
		buildExp("    ELSEIF iv_case = lc_g6.");
		buildExp("      UNASSIGN <lv_c7>.");
		buildExp("      CLEAR lv_a8.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJointBlocksNoEmptyLineChangeOrder() {
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configStaticsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);

		rule.configDistinctBlocks.setValue(false);
		rule.configEmptyLine.setValue(false);

		buildSrc("    DATA lv_a8 TYPE i.");
		buildSrc("    STATICS sv_b2 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildSrc("    DATA lv_d4 TYPE i.");
		buildSrc("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildSrc("    STATICS sv_f5 TYPE i.");
		buildSrc("    CONSTANTS lc_g6 TYPE i VALUE 1.");
		buildSrc("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildSrc("");
		buildSrc("    UNASSIGN <lv_e1>.");
		buildSrc("    CLEAR sv_b2.");
		buildSrc("    IF iv_case = lc_h3.");
		buildSrc("      CLEAR lv_d4.");
		buildSrc("      CLEAR sv_f5.");
		buildSrc("    ELSEIF iv_case = lc_g6.");
		buildSrc("      UNASSIGN <lv_c7>.");
		buildSrc("      CLEAR lv_a8.");
		buildSrc("    ENDIF.");

		buildExp("    CONSTANTS lc_h3 TYPE i VALUE 1.");
		buildExp("    CONSTANTS lc_g6 TYPE i VALUE 1.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <lv_e1> TYPE ty_s_data.");
		buildExp("    STATICS sv_b2 TYPE i.");
		buildExp("    DATA lv_d4 TYPE i.");
		buildExp("    STATICS sv_f5 TYPE i.");
		buildExp("    FIELD-SYMBOLS <lv_c7> TYPE ty_s_data.");
		buildExp("    DATA lv_a8 TYPE i.");
		buildExp("");
		buildExp("    UNASSIGN <lv_e1>.");
		buildExp("    CLEAR sv_b2.");
		buildExp("    IF iv_case = lc_h3.");
		buildExp("      CLEAR lv_d4.");
		buildExp("      CLEAR sv_f5.");
		buildExp("    ELSEIF iv_case = lc_g6.");
		buildExp("      UNASSIGN <lv_c7>.");
		buildExp("      CLEAR lv_a8.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveTypesToMethodStart() {
		// ensure that all TYPES declarations are placed at method start (but after the initial comment),
		// and comments between multiple TYPES are kept

		buildSrc("    \" initial comment on this method");
		buildSrc("    FIELD-SYMBOLS <ls_any> TYPE ty_s_data.");
		buildSrc("    TYPES: BEGIN OF ty_s_data,");
		buildSrc("             comp TYPE i.");
		buildSrc("             INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES  END OF ty_s_data.");
		buildSrc("    STATICS sv_any TYPE i.");
		buildSrc("    IF iv_case = 1.");
		buildSrc("      TYPES ty_tt_data TYPE STANDARD TABLE OF ty_s_data WITH EMPTY KEY.");
		buildSrc("      \" comment on ty_ts_data");
		buildSrc("      TYPES ty_ts_data TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY comp.");
		buildSrc("      \" comment on lv_any");
		buildSrc("      DATA lv_any TYPE i VALUE 1.");
		buildSrc("");
		buildSrc("      sv_any += lv_any.");
		buildSrc("    ENDIF.");
		buildSrc("    rv_result = sv_any + <ls_any>-value.");

		buildExp("    \" initial comment on this method");
		buildExp("");
		buildExp("    TYPES: BEGIN OF ty_s_data,");
		buildExp("             comp TYPE i.");
		buildExp("             INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES  END OF ty_s_data.");
		buildExp("    TYPES ty_tt_data TYPE STANDARD TABLE OF ty_s_data WITH EMPTY KEY.");
		buildExp("    \" comment on ty_ts_data");
		buildExp("    TYPES ty_ts_data TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY comp.");
		buildExp("");
		buildExp("    STATICS sv_any TYPE i.");
		buildExp("");
		buildExp("    \" comment on lv_any");
		buildExp("    DATA lv_any TYPE i VALUE 1.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <ls_any> TYPE ty_s_data.");
		buildExp("");
		buildExp("    IF iv_case = 1.");
		buildExp("      sv_any += lv_any.");
		buildExp("    ENDIF.");
		buildExp("    rv_result = sv_any + <ls_any>-value.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFieldSymbolKeptAtMethodStart() {
		// ensure that when ls_struc is moved the write position under the method start (which is then pointing to 
		// 'DATA ls_struc...') is correctly updated; otherwise, the FIELD-SYMBOL would erroneously be moved into the IF block 
		
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA lts_data TYPE ty_tt_data.");
		buildSrc("    DATA ls_struc TYPE ty_s_any.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS <ls_data> LIKE LINE OF lts_data.");
		buildSrc("");
		buildSrc("    LOOP AT lts_data ASSIGNING <ls_data>.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("    IF iv_case = 1.");
		buildSrc("      CLEAR ls_struc.");
		buildSrc("    ENDIF.");

		buildExp("    DATA lts_data TYPE ty_tt_data.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <ls_data> LIKE LINE OF lts_data.");
		buildExp("");
		buildExp("    LOOP AT lts_data ASSIGNING <ls_data>.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("    IF iv_case = 1.");
		buildExp("      DATA ls_struc TYPE ty_s_any.");
		buildExp("");
		buildExp("      CLEAR ls_struc.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineBreaksAfterParentKept() {
		// ensure that existing line breaks at the beginning of an enclosing block are kept 
		
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("  METHOD any_method.");
		buildSrc("");
		buildSrc("");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    DATA lv_other TYPE i.");
		buildSrc("");
		buildSrc("    CLEAR lv_other.");
		buildSrc("");
		buildSrc("    IF iv_case = 1");
		buildSrc("    OR iv_case = 2.");
		buildSrc("");
		buildSrc("");
		buildSrc("      CLEAR lv_any.");
		buildSrc("    ENDIF.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("");
		buildExp("");
		buildExp("    DATA lv_other TYPE i.");
		buildExp("");
		buildExp("    CLEAR lv_other.");
		buildExp("");
		buildExp("    IF iv_case = 1");
		buildExp("    OR iv_case = 2.");
		buildExp("");
		buildExp("");
		buildExp("      DATA lv_any TYPE i.");
		buildExp("");
		buildExp("      CLEAR lv_any.");
		buildExp("    ENDIF.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testVariableUsedWithLikeIsKeptAtStart() {
		// although it appears as if 'DATA lts_data' could be moved into the IF block, ensure that it is always kept it 
		// method start, because it is used in a 'LIKE' clause (which could be moved around, too) 
		
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA lts_data TYPE ty_tt_data.");
		buildSrc("");
		buildSrc("    IF iv_case = 1.");
		buildSrc("      FIELD-SYMBOLS <ls_data> LIKE LINE OF lts_data.");
		buildSrc("      LOOP AT lts_data ASSIGNING <ls_data>.");
		buildSrc("      ENDLOOP.");
		buildSrc("    ENDIF.");

		buildExp("    DATA lts_data TYPE ty_tt_data.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <ls_data> LIKE LINE OF lts_data.");
		buildExp("");
		buildExp("    IF iv_case = 1.");
		buildExp("      LOOP AT lts_data ASSIGNING <ls_data>.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testChainKeptAtMethodStart() {
		// ensure that a declaration chain is kept at method start, even if its first variable (or even all its variables)
		// could move inside a block
		
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA: a TYPE i, \" comment");
		buildSrc("          b TYPE i.");
		buildSrc("");
		buildSrc("    IF lv_case = 1.");
		buildSrc("      a = 1.");
		buildSrc("      b = a.");
		buildSrc("      rv_result = b.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWritePosAfterInitialMethodComment() {
		// expect the initial comment to be considered as a comment for the whole method, meaning that declarations
		// should be moved below it

		buildSrc("    \" comment on whole method");
		buildSrc("    CLEAR ev_any.");
		buildSrc("");
		buildSrc("    DATA b TYPE string VALUE `abc`.");
		buildSrc("    DATA a TYPE string.");
		buildSrc("");
		buildSrc("    rv_result = a + b.");

		buildExp("    \" comment on whole method");
		buildExp("");
		buildExp("    DATA a TYPE string.");
		buildExp("    DATA b TYPE string VALUE `abc`.");
		buildExp("");
		buildExp("    CLEAR ev_any.");
		buildExp("");
		buildExp("    rv_result = a + b.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWritePosAfterInitialMethodComments() {
		// expect the initial comment to be considered as a comment for the whole method, but the comment just before 
		// the CLEAR command to be considered as a comment that belongs to the executable code; therefore, expect 
		// that declarations are inserted between those two types of comments

		buildSrc("    \" comment on whole method");
		buildSrc("");
		buildSrc("    \" more comment on whole method");
		buildSrc("");
		buildSrc("    \" comment on CLEAR");
		buildSrc("    CLEAR ev_any.");
		buildSrc("");
		buildSrc("    DATA b TYPE string VALUE `abc`.");
		buildSrc("    DATA a TYPE string.");
		buildSrc("");
		buildSrc("    rv_result = a + b.");

		buildExp("    \" comment on whole method");
		buildExp("");
		buildExp("    \" more comment on whole method");
		buildExp("");
		buildExp("    DATA a TYPE string.");
		buildExp("    DATA b TYPE string VALUE `abc`.");
		buildExp("");
		buildExp("    \" comment on CLEAR");
		buildExp("    CLEAR ev_any.");
		buildExp("");
		buildExp("    rv_result = a + b.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWritePosRightAfterEnclosingCommand() {
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA b TYPE string VALUE `abc`.");
		buildSrc("    DATA a TYPE string.");
		buildSrc("");
		buildSrc("    IF lv_calculate_result = abap_true.");
		buildSrc("      \" explanation of IF block");
		buildSrc("      a = 1.");
		buildSrc("      b = a.");
		buildSrc("      rv_result = b.");
		buildSrc("    ENDIF.");

		buildExp("    IF lv_calculate_result = abap_true.");
		buildExp("      DATA a TYPE string.");
		buildExp("      DATA b TYPE string VALUE `abc`.");
		buildExp("");
		buildExp("      \" explanation of IF block");
		buildExp("      a = 1.");
		buildExp("      b = a.");
		buildExp("      rv_result = b.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testExtendSectionWithPragmaAfterPeriod() {
		// ensure that pragmas are moved along with the declaration, even if they are in an incorrect position, 
		// after the period in the same line
		
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA b TYPE string VALUE `abc`. ##NO_TEXT");
		buildSrc("    DATA a TYPE string. ##NEEDED");
		buildSrc("");
		buildSrc("    IF lv_calculate_result = abap_true.");
		buildSrc("      \" explanation");
		buildSrc("      a = 1.");
		buildSrc("      b = a.");
		buildSrc("      rv_result = b.");
		buildSrc("    ENDIF.");

		buildExp("    IF lv_calculate_result = abap_true.");
		buildExp("      DATA a TYPE string. ##NEEDED");
		buildExp("      DATA b TYPE string VALUE `abc`. ##NO_TEXT");
		buildExp("");
		buildExp("      \" explanation");
		buildExp("      a = 1.");
		buildExp("      b = a.");
		buildExp("      rv_result = b.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentsOnExecutableCommandsNotMoved() {
		// ensure that comments that look like they describe following executable code are not moved, 
		// while comments that seem to describe the declarations are moved
		// (for this, the rule analyzes whether there are empty lines or other comments before the next executable command)
		
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("");
		buildSrc("    \" comment on first executable section");
		buildSrc("    TYPES ty_any TYPE i.");
		buildSrc("    DATA lv_data TYPE ty_any.");
		buildSrc("    CLEAR lv_data.");
		buildSrc("    do_something( lv_data ).");
		buildSrc("");
		buildSrc("    \" comment on other type");
		buildSrc("    TYPES ty_other TYPE i.");
		buildSrc("    \" comment on other data");
		buildSrc("    DATA lv_other TYPE ty_other.");
		buildSrc("    \" comment on second executable section");
		buildSrc("    rv_result = lv_data + lv_value + lv_other.");

		buildExp("    TYPES ty_any TYPE i.");
		buildExp("    \" comment on other type");
		buildExp("    TYPES ty_other TYPE i.");
		buildExp("");
		buildExp("    DATA lv_data TYPE ty_any.");
		buildExp("    DATA lv_value TYPE i.");
		buildExp("    \" comment on other data");
		buildExp("    DATA lv_other TYPE ty_other.");
		buildExp("");
		buildExp("    \" comment on first executable section");
		buildExp("    CLEAR lv_data.");
		buildExp("    do_something( lv_data ).");
		buildExp("");
		buildExp("    \" comment on second executable section");
		buildExp("    rv_result = lv_data + lv_value + lv_other.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	@Test
	void testMovePragmaBeforeExistingDeclaration() {
		rule.configDataOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);

		buildSrc("    DATA a TYPE string. ##NEEDED");
		buildSrc("");
		buildSrc("    IF lv_calculate_result = abap_true.");
		buildSrc("      DATA b TYPE string VALUE `abc`. ##NO_TEXT");
		buildSrc("      a = 1.");
		buildSrc("      b = a.");
		buildSrc("      rv_result = b.");
		buildSrc("    ENDIF.");

		buildExp("    IF lv_calculate_result = abap_true.");
		buildExp("      DATA a TYPE string. ##NEEDED");
		buildExp("      DATA b TYPE string VALUE `abc`. ##NO_TEXT");
		buildExp("");
		buildExp("      a = 1.");
		buildExp("      b = a.");
		buildExp("      rv_result = b.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainMovedForFirstVariable() {
		// ensure that a declaration chain is moved depending on the usage position of its first variable ('b' in this case)
		
		buildSrc("    DATA: b TYPE i,");
		buildSrc("          d TYPE i.");
		buildSrc("    DATA a TYPE i.");
		buildSrc("    DATA c TYPE i.");
		buildSrc("");
		buildSrc("    a = 1.");
		buildSrc("    b = 2.");
		buildSrc("    c = 3.");
		buildSrc("    d = 4.");
		buildSrc("    rv_result = a + b + c + d.");

		buildExp("    DATA a TYPE i.");
		buildExp("    DATA: b TYPE i,");
		buildExp("          d TYPE i.");
		buildExp("    DATA c TYPE i.");
		buildExp("");
		buildExp("    a = 1.");
		buildExp("    b = 2.");
		buildExp("    c = 3.");
		buildExp("    d = 4.");
		buildExp("    rv_result = a + b + c + d.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testToDoCommentAttachedBeforeAndAfter() {
		// ensure that a to-do comments that were generated by ABAP cleaner for a declaration 
		// is moved with the declaration, even if it is attached both to the declaration and the preceding parent Command 
		
		buildSrc("    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildSrc("      \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildSrc("      DATA lv_value TYPE i.");
		buildSrc("      \" TODO: constant is never used (ABAP cleaner)");
		buildSrc("      CONSTANTS lc_count TYPE i VALUE 1.");
		buildSrc("      CLEAR lv_value.");
		buildSrc("    ENDLOOP.");

		buildExp("    \" TODO: constant is never used (ABAP cleaner)");
		buildExp("    CONSTANTS lc_count TYPE i VALUE 1.");
		buildExp("");
		buildExp("    \" TODO: variable is assigned but never used (ABAP cleaner)");
		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildExp("      CLEAR lv_value.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIgnoreUsageInComment() {
		// ensure that (as configured here), the usage of the field-symbol in the commented is NOT considered as its  
		// first usage, so the field-symbol can be moved inside the IF block
		
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);
		rule.configConsiderComments.setValue(false);

		buildSrc("    FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data.");
		buildSrc("*    UNASSIGN <ls_data_avg>.");
		buildSrc("");
		buildSrc("    IF iv_get_average = abap_true.");
		buildSrc("      ls_data_avg-name = lc_name_average.");
		buildSrc("      LOOP AT lts_data ASSIGNING <ls_data_avg>.");
		buildSrc("        ls_data_avg-count += <ls_data_avg>-count.");
		buildSrc("      ENDLOOP.");
		buildSrc("    ENDIF.");

		buildExp("*    UNASSIGN <ls_data_avg>.");
		buildExp("");
		buildExp("    IF iv_get_average = abap_true.");
		buildExp("      FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data.");
		buildExp("");
		buildExp("      ls_data_avg-name = lc_name_average.");
		buildExp("      LOOP AT lts_data ASSIGNING <ls_data_avg>.");
		buildExp("        ls_data_avg-count += <ls_data_avg>-count.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testConsiderUsageInComment() {
		// ensure that (as configured here), the usage of the field-symbol in the commented IS considered as its  
		// first usage, so the field-symbol can NOT be moved inside the IF block
		
		rule.configFieldSymbolsOrder.setEnumValue(LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER);
		rule.configConsiderComments.setValue(true);

		buildSrc("    FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data.");
		buildSrc("");
		buildSrc("*    UNASSIGN <ls_data_avg>.");
		buildSrc("");
		buildSrc("    IF iv_get_average = abap_true.");
		buildSrc("      ls_data_avg-name = lc_name_average.");
		buildSrc("      LOOP AT lts_data ASSIGNING <ls_data_avg>.");
		buildSrc("        ls_data_avg-count += <ls_data_avg>-count.");
		buildSrc("      ENDLOOP.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveAttachedCommentsWithDeclarations() {
		// ensure that the attached comments are moved along with the declarations

		rule.configMoveComments.setValue(true);

		buildSrc("    TYPES ty_s_data TYPE i.");
		buildSrc("    \" structure for calculating the average count");
		buildSrc("    DATA ls_data_avg TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    \" structure for calculating the maximum count");
		buildSrc("    DATA ls_data_max TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    CLEAR ls_data_max.");
		buildSrc("    CLEAR ls_data_avg.");

		buildExp("    TYPES ty_s_data TYPE i.");
		buildExp("");
		buildExp("    \" structure for calculating the maximum count");
		buildExp("    DATA ls_data_max TYPE ty_s_data.");
		buildExp("    \" structure for calculating the average count");
		buildExp("    DATA ls_data_avg TYPE ty_s_data.");
		buildExp("");
		buildExp("    CLEAR ls_data_max.");
		buildExp("    CLEAR ls_data_avg.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testMoveCommentAttachedBeforeAndAfter() {
		// ensure that the attached comments are moved, even though they are also attached above 

		rule.configMoveComments.setValue(true);

		buildSrc("    TYPES ty TYPE i.");
		buildSrc("    \" structure for calculating the average count");
		buildSrc("    DATA ls_data_avg TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    CLEAR lts_result.");
		buildSrc("    \" structure for calculating the maximum count");
		buildSrc("    DATA ls_data_max TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    CLEAR ls_data_max.");
		buildSrc("    CLEAR ls_data_avg.");

		buildExp("    TYPES ty TYPE i.");
		buildExp("");
		buildExp("    \" structure for calculating the maximum count");
		buildExp("    DATA ls_data_max TYPE ty_s_data.");
		buildExp("    \" structure for calculating the average count");
		buildExp("    DATA ls_data_avg TYPE ty_s_data.");
		buildExp("");
		buildExp("    CLEAR lts_result.");
		buildExp("");
		buildExp("    CLEAR ls_data_max.");
		buildExp("    CLEAR ls_data_avg.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	@Test
	void testKeepCommentsInPlace() {
		// ensure that (as configured here), attached comments are NOT moved with the declarations

		rule.configMoveComments.setValue(false);

		buildSrc("    TYPES ty_s_data TYPE i.");
		buildSrc("    \" structure for calculating the average count");
		buildSrc("    DATA ls_data_avg TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    \" structure for calculating the maximum count");
		buildSrc("    DATA ls_data_max TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    CLEAR ls_data_max.");
		buildSrc("    CLEAR ls_data_avg.");

		buildExp("    TYPES ty_s_data TYPE i.");
		buildExp("");
		buildExp("    DATA ls_data_max TYPE ty_s_data.");
		buildExp("    DATA ls_data_avg TYPE ty_s_data.");
		buildExp("");
		buildExp("    \" structure for calculating the average count");
		buildExp("");
		buildExp("    \" structure for calculating the maximum count");
		buildExp("");
		buildExp("    CLEAR ls_data_max.");
		buildExp("    CLEAR ls_data_avg.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAbapDocCommentsMovedWithDeclarations() {
		// ensure that despite configuration, ABAP Doc comments are ALWAYS moved with the declarations

		rule.configMoveComments.setValue(false);

		buildSrc("    TYPES ty_s_data TYPE i.");
		buildSrc("    \"! structure for calculating the average count");
		buildSrc("    DATA ls_data_avg TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    \"! structure for calculating the maximum count");
		buildSrc("    DATA ls_data_max TYPE ty_s_data.");
		buildSrc("");
		buildSrc("    CLEAR ls_data_max.");
		buildSrc("    CLEAR ls_data_avg.");

		buildExp("    TYPES ty_s_data TYPE i.");
		buildExp("");
		buildExp("    \"! structure for calculating the maximum count");
		buildExp("    DATA ls_data_max TYPE ty_s_data.");
		buildExp("    \"! structure for calculating the average count");
		buildExp("    DATA ls_data_avg TYPE ty_s_data.");
		buildExp("");
		buildExp("    CLEAR ls_data_max.");
		buildExp("    CLEAR ls_data_avg.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleChain() {
		buildSrc("    DATA:");
		buildSrc("      a2 TYPE string,");
		buildSrc("      b3 TYPE i,");
		buildSrc("      c1 TYPE ty_s_struc.");
		buildSrc("");
		buildSrc("    CLEAR: c1, a2, b3.");

		buildExp("    DATA:");
		buildExp("      c1 TYPE ty_s_struc,");
		buildExp("      a2 TYPE string,");
		buildExp("      b3 TYPE i.");
		buildExp("");
		buildExp("    CLEAR: c1, a2, b3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleChainUnchanged() {
		rule.configRearrangeChains.setValue(false);

		buildSrc("    DATA:");
		buildSrc("      a2 TYPE string,");
		buildSrc("      b3 TYPE i,");
		buildSrc("      c1 TYPE ty_s_struc.");
		buildSrc("");
		buildSrc("    CLEAR: c1, a2, b3.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleChainOtherOrder() {
		buildSrc("    DATA:");
		buildSrc("      a3 TYPE string,");
		buildSrc("      b1 TYPE i,");
		buildSrc("      c2 TYPE ty_s_struc.");
		buildSrc("");
		buildSrc("    CLEAR: b1, c2, a3.");

		buildExp("    DATA:");
		buildExp("      b1 TYPE i,");
		buildExp("      c2 TYPE ty_s_struc,");
		buildExp("      a3 TYPE string.");
		buildExp("");
		buildExp("    CLEAR: b1, c2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithCommentsOnDeclarations() {
		buildSrc("    DATA: a2 TYPE string, \" comment on a2");
		buildSrc("          b3 TYPE i, \" comment on b3");
		buildSrc("* two lines of comments");
		buildSrc("* on c1");
		buildSrc("          c1 TYPE ty_s_struc. \" another comment on c1");
		buildSrc("");
		buildSrc("    CLEAR: c1, a2, b3.");

		buildExp("    DATA:");
		buildExp("* two lines of comments");
		buildExp("* on c1");
		buildExp("          c1 TYPE ty_s_struc, \" another comment on c1");
		buildExp("          a2 TYPE string, \" comment on a2");
		buildExp("          b3 TYPE i. \" comment on b3");
		buildExp("");
		buildExp("    CLEAR: c1, a2, b3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithCommentsOtherOrder() {
		buildSrc("    DATA: a3 TYPE string, \" comment on a2");
		buildSrc("          b1 TYPE i, \" comment on b3");
		buildSrc("* two lines of comments");
		buildSrc("* on c1");
		buildSrc("          c2 TYPE ty_s_struc. \" another comment on c1");
		buildSrc("");
		buildSrc("    CLEAR: b1, c2, a3.");

		buildExp("    DATA: b1 TYPE i, \" comment on b3");
		buildExp("* two lines of comments");
		buildExp("* on c1");
		buildExp("          c2 TYPE ty_s_struc, \" another comment on c1");
		buildExp("          a3 TYPE string. \" comment on a2");
		buildExp("");
		buildExp("    CLEAR: b1, c2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testChainWithCommentsThirdOrder() {
		buildSrc("    DATA:");
		buildSrc("* two lines of comments");
		buildSrc("* on a3");
		buildSrc("          a3 TYPE ty_s_struc, \" another comment on a3");
		buildSrc("          b2 TYPE string, \" comment on b2");
		buildSrc("          c1 TYPE i. \" comment on c1");
		buildSrc("");
		buildSrc("    CLEAR: c1, b2, a3.");

		buildExp("    DATA:");
		buildExp("          c1 TYPE i, \" comment on c1");
		buildExp("          b2 TYPE string, \" comment on b2");
		buildExp("* two lines of comments");
		buildExp("* on a3");
		buildExp("          a3 TYPE ty_s_struc. \" another comment on a3");
		buildExp("");
		buildExp("    CLEAR: c1, b2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithCommentOnWholeChain() {
		buildSrc("    DATA: \" comment on the whole chain");
		buildSrc("      \" comment on a2");
		buildSrc("      a2 TYPE string, \" another comment on a2");
		buildSrc("      b3 TYPE i, \" comment on b3");
		buildSrc("* two lines of comments");
		buildSrc("* on c1");
		buildSrc("      c1 TYPE ty_s_struc. \" another comment on c1");
		buildSrc("");
		buildSrc("    CLEAR: c1, a2, b3.");

		buildExp("    DATA: \" comment on the whole chain");
		buildExp("* two lines of comments");
		buildExp("* on c1");
		buildExp("      c1 TYPE ty_s_struc, \" another comment on c1");
		buildExp("      \" comment on a2");
		buildExp("      a2 TYPE string, \" another comment on a2");
		buildExp("      b3 TYPE i. \" comment on b3");
		buildExp("");
		buildExp("    CLEAR: c1, a2, b3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithPragmasAndComments() {
		// ensure that the pragmas stay with the declarations which they belong to, 
		// even if some of them are in an incorrect position

		buildSrc("    DATA ##NEEDED: \" this pragma belongs to the whole chain!");
		buildSrc("      a2 TYPE string VALUE `abc`, ##NO_TEXT \" wrong pragma position is tolerated");
		buildSrc("      ##NEEDED b3 TYPE i, \" correct pragma position");
		buildSrc("      c1 TYPE string VALUE `def`.");
		buildSrc("");
		buildSrc("    CLEAR: c1, a2, b3.");

		buildExp("    DATA ##NEEDED: \" this pragma belongs to the whole chain!");
		buildExp("      c1 TYPE string VALUE `def`,");
		buildExp("      a2 TYPE string VALUE `abc`, ##NO_TEXT \" wrong pragma position is tolerated");
		buildExp("      ##NEEDED b3 TYPE i. \" correct pragma position");
		buildExp("");
		buildExp("    CLEAR: c1, a2, b3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testChainWithMultiLineElements() {
		buildSrc("    DATA: a3 TYPE SORTED TABLE OF ty_s_any_struc");
		buildSrc("                  WITH UNIQUE KEY any_component");
		buildSrc("                                  other_component,");
		buildSrc("          b2 \" comment in an odd place");
		buildSrc("             TYPE i,");
		buildSrc("          c1 TYPE p");
		buildSrc("             LENGTH 5");
		buildSrc("             DECIMALS 2");
		buildSrc("             VALUE '3.14'.");
		buildSrc("");
		buildSrc("    CLEAR: c1, b2, a3.");

		buildExp("    DATA: c1 TYPE p");
		buildExp("             LENGTH 5");
		buildExp("             DECIMALS 2");
		buildExp("             VALUE '3.14',");
		buildExp("          b2 \" comment in an odd place");
		buildExp("             TYPE i,");
		buildExp("          a3 TYPE SORTED TABLE OF ty_s_any_struc");
		buildExp("                  WITH UNIQUE KEY any_component");
		buildExp("                                  other_component.");
		buildExp("");
		buildExp("    CLEAR: c1, b2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainInOneLine() {
		buildSrc("    DATA: a3 TYPE string, b1 TYPE i, c2 TYPE ty_s_struc.");
		buildSrc("    CLEAR: b1, c2, a3.");

		buildExp("    DATA: b1 TYPE i,");
		buildExp("          c2 TYPE ty_s_struc,");
		buildExp("          a3 TYPE string.");
		buildExp("");
		buildExp("    CLEAR: b1, c2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainInOneLineWithPragmaAndComment() {
		buildSrc("    DATA: a3 TYPE string, b1 TYPE i, ##NEEDED c2 TYPE ty_s_struc. \" comment");
		buildSrc("    CLEAR: b1, c2, a3.");

		buildExp("    DATA: b1 TYPE i,");
		buildExp("          ##NEEDED c2 TYPE ty_s_struc, \" comment");
		buildExp("          a3 TYPE string.");
		buildExp("");
		buildExp("    CLEAR: b1, c2, a3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainOfStructuredConstantsSkipped() {
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);

		buildSrc("    CONSTANTS:");
		buildSrc("      BEGIN OF lc_struc_a,");
		buildSrc("        comp1 TYPE i VALUE 1,");
		buildSrc("        comp2 TYPE i VALUE 2,");
		buildSrc("      END OF lc_struc_a,");
		buildSrc("      BEGIN OF lc_struc_b,");
		buildSrc("        comp1 TYPE i VALUE 1,");
		buildSrc("        comp2 TYPE i VALUE 2,");
		buildSrc("      END OF lc_struc_b,");
		buildSrc("      BEGIN OF lc_struc_c,");
		buildSrc("        comp1 TYPE i VALUE 1,");
		buildSrc("        comp2 TYPE i VALUE 2,");
		buildSrc("      END OF lc_struc_c.");
		buildSrc("");
		buildSrc("    rv_result = lc_struc_b-comp1 + lc_struc_c-comp2 + lc_struc_a-comp1.");

		copyExpFromSrc();
		
	   // (a less restrictive condition in LocalDeclarationOrderRule.rearrange() could allow processing of declarations 
		// in which all BEGIN OFs are closed with corresponding(!) END OFs)
		/*
		buildExp("    CONSTANTS:");
		buildExp("      BEGIN OF lc_struc_b,");
		buildExp("        comp1 TYPE i VALUE 1,");
		buildExp("        comp2 TYPE i VALUE 2,");
		buildExp("      END OF lc_struc_b,");
		buildExp("      BEGIN OF lc_struc_c,");
		buildExp("        comp1 TYPE i VALUE 1,");
		buildExp("        comp2 TYPE i VALUE 2,");
		buildExp("      END OF lc_struc_c,");
		buildExp("      BEGIN OF lc_struc_a,");
		buildExp("        comp1 TYPE i VALUE 1,");
		buildExp("        comp2 TYPE i VALUE 2,");
		buildExp("      END OF lc_struc_a.");
		buildExp("");
		buildExp("    rv_result = lc_struc_b-comp1 + lc_struc_c-comp2 + lc_struc_a-comp1.");
		*/
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainsOfStructuredConstantsSkipped() {
		// ensure that nothing happens if END OF is in a different chain
		
		rule.configConstantsOrder.setEnumValue(LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);

		buildSrc("    CONSTANTS:");
		buildSrc("      BEGIN OF lc_struc_a,");
		buildSrc("        comp1 TYPE i VALUE 1,");
		buildSrc("        comp2 TYPE i VALUE 2,");
		buildSrc("    CONSTANTS:");
		buildSrc("      END OF lc_struc_a,");
		buildSrc("      BEGIN OF lc_struc_b,");
		buildSrc("        comp1 TYPE i VALUE 1,");
		buildSrc("        comp2 TYPE i VALUE 2,");
		buildSrc("    CONSTANTS:");
		buildSrc("      END OF lc_struc_b.");
		buildSrc("");
		buildSrc("    rv_result = lc_struc_b-comp1 + lc_struc_a-comp1.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainsOfStructuredDataSkipped() {
		// ensure that nothing happens here, especially that lv_any does NOT switch positions with 'END OF ls_struc'
		
		buildSrc("    DATA: BEGIN OF ls_struc.");
		buildSrc("            INCLUDE STRUCTURE any_struc.");
		buildSrc("    DATA: END OF ls_struc,");
		buildSrc("          lv_any TYPE i.");
		buildSrc("");
		buildSrc("    rv_result = lv_any + ls_struc-comp.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testEmptyLinesKeptIfOrderKept() {
		// ensure that empty lines are kept if the order is 'correct' to allow for purposeful introduction of empty lines
		// after the first rearrangement
		
		buildSrc("    DATA a TYPE i.");
		buildSrc("    DATA b TYPE i.");
		buildSrc("");
		buildSrc("    DATA c TYPE i.");
		buildSrc("    DATA d TYPE i.");
		buildSrc("");
		buildSrc("    \" comment");
		buildSrc("    DATA e TYPE i.");
		buildSrc("    DATA f TYPE i.");
		buildSrc("");
		buildSrc("    rv_result = a + b + c + d + e + f.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLikeLineOfRefAttribute() {
		// except 'LIKE LINE OF lr_ref->any_table' to be correctly identified as a usage of lr_ref, and thus expect that
		// 'DATA ls_struc' is NOT moved up to the method start

		buildSrc("    io_util->any_method( IMPORTING er_ref = DATA(lr_ref) ).");
		buildSrc("    DATA ls_struc LIKE LINE OF lr_ref->any_table.");
		buildSrc("    rs_result = ls_struc.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLikeTableAfterStructure() {
		// ensure that the FIELD-SYMBOLS declaration is NOT moved in front of DATA, because it refers to LT_TABLE
		buildSrc("    DATA: BEGIN OF ls_struc,");
		buildSrc("            comp TYPE i,");
		buildSrc("          END OF ls_struc,");
		buildSrc("          lt_table TYPE STANDARD TABLE OF ty_s_any WITH DEFAULT KEY.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS <ls_any> LIKE LINE OF lt_table.");
		buildSrc("");
		buildSrc("    rv_result = ls_struc-comp + lines( lt_table ) + <ls_any>-comp.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTypesLikeDataEtc() {
		// ensure that none of the declarations are moved (including the TYPES declaration!), 
		// because they would be moved in front of a declaration they refer to with LIKE 

		buildSrc("    DATA lt_any TYPE farr_ts_contract_id.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS <ls_any>   LIKE LINE OF lt_any.");
		buildSrc("    FIELD-SYMBOLS <ls_other> LIKE lt_any.");
		buildSrc("");
		buildSrc("    CONSTANTS ls_any   LIKE LINE OF lt_any VALUE IS INITIAL.");
		buildSrc("    CONSTANTS ls_other LIKE <ls_any> VALUE IS INITIAL.");
		buildSrc("");
		buildSrc("    TYPES ty_tt_any LIKE lt_any.");
		buildSrc("    TYPES ty_s_any  LIKE ls_other.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTestSeamSkipped() {
		// ensure that nothing happens if a (product code) method contains a TEST-SEAM, 
		// because we are not sure what the test injection does with the declarations and usages
		
		buildSrc("    TEST-SEAM declaration.");
		buildSrc("      DATA lv_any TYPE i.");
		buildSrc("    END-TEST-SEAM.");
		buildSrc("");
		buildSrc("    TEST-SEAM usage.");
		buildSrc("      \" a usage of lv_any may be injected here");
		buildSrc("    END-TEST-SEAM.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTestInjectionSkipped() {
		// ensure that nothing happens if a (test) method contains a TEST-INJECTION,   
		// because variables declared inside an injection must NOT be moved away

		buildSrc("    TEST-INJECTION usage.");
		buildSrc("      rv_result = lv_any + lv_other.");
		buildSrc("    END-TEST-INJECTION.");
		buildSrc("");
		buildSrc("    TEST-INJECTION declaration.");
		buildSrc("      DATA lv_other TYPE i.");
		buildSrc("    END-TEST-INJECTION.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAbapDocCommentKeptWithVariable() {
		// ensure that no line break is introduced between the ABAP Doc "! comment and the variable, 
		// although a comment directly after the METHOD command may look like an overall comment  
		buildSrc("    \"! comment on variable");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("");
		buildSrc("    rv_result = lv_any.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTypesAndDataObjectsWithSameNames() {
		// ensure that the types 't1', 't2' and 't3', which appear in 'descending order' t3, t2, t1, are correctly distinguished   
		// from the data objects 't1', 't2' and 't3', which are used in 'ascending order' t1, t2, t3, so DATA t1 must be moved up 
		
		buildSrc("  METHOD any_method.");
		buildSrc("    TYPES t3 TYPE SORTED   TABLE OF ty_s3 WITH KEY a b c.");
		buildSrc("    TYPES t2 TYPE HASHED   TABLE OF ty_s2 WITH KEY a b.");
		buildSrc("    TYPES t1 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    DATA t2 TYPE i.");
		buildSrc("    DATA t1 TYPE i.");
		buildSrc("    DATA t3 TYPE i.");
		buildSrc("");
		buildSrc("    any_method( VALUE t3( ) ).");
		buildSrc("    any_method( VALUE t2( ) ).");
		buildSrc("    any_method( VALUE t1( ) ).");
		buildSrc("");
		buildSrc("    t1 = 1.");
		buildSrc("    t2 = 2.");
		buildSrc("    t3 = 3.");
		buildSrc("    rv_result = t1 + t2 + t3.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("    TYPES t3 TYPE SORTED   TABLE OF ty_s3 WITH KEY a b c.");
		buildExp("    TYPES t2 TYPE HASHED   TABLE OF ty_s2 WITH KEY a b.");
		buildExp("    TYPES t1 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildExp("");
		buildExp("    DATA t1 TYPE i.");
		buildExp("    DATA t2 TYPE i.");
		buildExp("    DATA t3 TYPE i.");
		buildExp("");
		buildExp("    any_method( VALUE t3( ) ).");
		buildExp("    any_method( VALUE t2( ) ).");
		buildExp("    any_method( VALUE t1( ) ).");
		buildExp("");
		buildExp("    t1 = 1.");
		buildExp("    t2 = 2.");
		buildExp("    t3 = 3.");
		buildExp("    rv_result = t1 + t2 + t3.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testMethodWithConstantsOnly() {
		// ensure that a method that consists of CONSTANTS declarations only does not raise an exception
		buildSrc("    CONSTANTS a TYPE i VALUE 1.");
		buildSrc("    CONSTANTS b TYPE i VALUE 2.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIdentifiersWithEscapeChar() {
		// ensure that identifiers with the ! escape char are correctly processed 
		
		buildSrc("    DATA !c TYPE char10.");
		buildSrc("    DATA !b TYPE char5.");
		buildSrc("    DATA !a TYPE ty_s_struc.");
		buildSrc("    !a-component = !b+2(2) + !c.");

		buildExp("    DATA !a TYPE ty_s_struc.");
		buildExp("    DATA !b TYPE char5.");
		buildExp("    DATA !c TYPE char10.");
		buildExp("");
		buildExp("    !a-component = !b+2(2) + !c.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
