package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class NeedlessClearTest extends RuleTestBase {
	private NeedlessClearRule rule;
	
	NeedlessClearTest() {
		super(RuleID.NEEDLESS_CLEAR);
		rule = (NeedlessClearRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configActionAtStart.setEnumValue(NeedlessClearAction.DELETE);
		rule.configActionAtEnd.setEnumValue(NeedlessClearAction.ADD_TODO_COMMENT);
		rule.configKeepStrucBeforeAssign.setValue(true);
	}
	
	@Test
	void testWithAndWithoutChain() {
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    DATA lv_third TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    CLEAR: ev_result,");
		buildSrc("           lv_other,");
		buildSrc("           et_table,");
		buildSrc("           lv_third.");
		buildSrc("");
		buildSrc("    any_method( ).");
		buildSrc("");
		buildSrc("    CLEAR: lv_other, lv_any.");
		buildSrc("    CLEAR lv_third.");
		buildSrc("    CLEAR ev_table.");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    DATA lv_third TYPE i VALUE 3.");
		buildExp("");
		buildExp("    CLEAR: ev_result,");
		buildExp("           et_table,");
		buildExp("           lv_third.");
		buildExp("");
		buildExp("    any_method( ).");
		buildExp("");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR: lv_other, lv_any.");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lv_third.");
		buildExp("    CLEAR ev_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testNoExecutableCommandExceptClear() {
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    DATA lv_third TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    CLEAR: ev_result,");
		buildSrc("           lv_other,");
		buildSrc("           et_table,");
		buildSrc("           lv_third.");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    DATA lv_third TYPE i VALUE 3.");
		buildExp("");
		buildExp("    CLEAR: ev_result,");
		buildExp("           et_table,");
		buildExp("           lv_third.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearOfStaticsKept() {
		buildSrc("    STATICS lv_any TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    DATA lv_third TYPE i.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    CLEAR: ev_result, lv_other, et_table, lv_third.");

		buildExp("    STATICS lv_any TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    DATA lv_third TYPE i.");
		buildExp("");
		buildExp("    CLEAR lv_any.");
		buildExp("    CLEAR: ev_result, et_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearOfTableExprEtcKept() {
		// ensure that with any attribute access, table expression, dereferencing etc., CLEAR is kept;
		// however "CLEAR !lv_any" is removed, as well as "CLEAR lv_other", which has "VALUE IS INITIAL"
		buildSrc("    DATA lo_instance TYPE REF TO cl_any.");
		buildSrc("    DATA lt_table TYPE ty_tt_table.");
		buildSrc("    DATA lr_any TYPE REF TO data.");
		buildSrc("    DATA lv_any TYPE i.");
		buildSrc("    DATA lv_other TYPE string VALUE IS INITIAL.");
		buildSrc("");
		buildSrc("    CLEAR lo_instance->attribute.");
		buildSrc("    CLEAR !lt_table[ 1 ]-component.");
		buildSrc("    CLEAR lr_any->*.");
		buildSrc("    CLEAR: !lv_any, !lv_other.");

		buildExp("    DATA lo_instance TYPE REF TO cl_any.");
		buildExp("    DATA lt_table TYPE ty_tt_table.");
		buildExp("    DATA lr_any TYPE REF TO data.");
		buildExp("    DATA lv_any TYPE i.");
		buildExp("    DATA lv_other TYPE string VALUE IS INITIAL.");
		buildExp("");
		buildExp("    CLEAR lo_instance->attribute.");
		buildExp("    CLEAR !lt_table[ 1 ]-component.");
		buildExp("    CLEAR lr_any->*.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentsPragmaLinesAndAsserts() {
		// ensure that CLEAR is processed across comments, pragma lines, and (at method start) 
		// ASSERTs, BREAK-POINTs and LOG-POINTs; however, at method end, CLEAR before ASSERT, 
		// BREAK-POINT or LOG-POINT must NOT be processed
		
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    \" comment");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    LOG-POINT ID any_id.");
		buildSrc("    BREAK-POINT.");
		buildSrc("* comment");
		buildSrc("    ##ANY_PRAGMA");
		buildSrc("    ASSERT it_table IS NOT INITIAL.");
		buildSrc("    DATA lv_third TYPE i.");
		buildSrc("");
		buildSrc("    CLEAR: lv_other,");
		buildSrc("          et_table.");
		buildSrc("");
		buildSrc("    lv_any = 1.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    ASSERT lv_any IS INITIAL.");
		buildSrc("    CLEAR lv_other.");
		buildSrc("    DATA lv_fourth TYPE i.");
		buildSrc("    \" comment");
		buildSrc("* comment");
		buildSrc("    ##ANY_PRAGMA");
		buildSrc("    CLEAR lv_third.");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    \" comment");
		buildExp("");
		buildExp("    LOG-POINT ID any_id.");
		buildExp("    BREAK-POINT.");
		buildExp("* comment");
		buildExp("    ##ANY_PRAGMA");
		buildExp("    ASSERT it_table IS NOT INITIAL.");
		buildExp("    DATA lv_third TYPE i.");
		buildExp("");
		buildExp("    CLEAR: et_table.");
		buildExp("");
		buildExp("    lv_any = 1.");
		buildExp("");
		buildExp("    CLEAR lv_any.");
		buildExp("    ASSERT lv_any IS INITIAL.");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lv_other.");
		buildExp("    DATA lv_fourth TYPE i.");
		buildExp("    \" comment");
		buildExp("* comment");
		buildExp("    ##ANY_PRAGMA");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lv_third.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAtSelectionScreen() {
		// ensure that AT SELECTION-SCREEN without 'method end' works 
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("AT SELECTION-SCREEN.");
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("");
		buildSrc("    lv_any = 1.");
		buildSrc("");
		buildSrc("    CLEAR lv_other.");

		buildExp("REPORT any_report.");
		buildExp("");
		buildExp("AT SELECTION-SCREEN.");
		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("");
		buildExp("    lv_any = 1.");
		buildExp("");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lv_other.");

		testRule();
	}
	
	@Test
	void testTodoCommentsRemoved() {
		// ensure that to-do comments are removed if they are no longer valid (typically because the line 'other_method( )' 
		// was added at the end since the cleanup was last exeucted)
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    DATA lv_third TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    CLEAR: lv_other, \" comment1");
		buildSrc("           \" comment2");
		buildSrc("           et_table.");
		buildSrc("");
		buildSrc("    any_method( ).");
		buildSrc("");
		buildSrc("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildSrc("    CLEAR: lv_other, lv_any.");
		buildSrc("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildSrc("    CLEAR lv_third.");
		buildSrc("    CLEAR ev_table.");
		buildSrc("");
		buildSrc("    other_method( ).");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    DATA lv_third TYPE i VALUE 3.");
		buildExp("");
		buildExp("    \" comment2");
		buildExp("    CLEAR: et_table.");
		buildExp("");
		buildExp("    any_method( ).");
		buildExp("");
		buildExp("    CLEAR: lv_other, lv_any.");
		buildExp("    CLEAR lv_third.");
		buildExp("    CLEAR ev_table.");
		buildExp("");
		buildExp("    other_method( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearStructureBeforeAssignKept() {
		// ensure that 'CLEAR ls_struc' is kept, because in the next Command (after the comments), a component of that
		// structure is assigned
		
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA ls_struc TYPE ty_s_struc.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("");
		buildSrc("    CLEAR ls_struc.");
		buildSrc("    \" comment");
		buildSrc("*    comment");
		buildSrc("    ls_struc-id   = 1.");
		buildSrc("    APPEND ls_struc TO et_table.");
		buildSrc("");
		buildSrc("    any_method( ).");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA ls_struc TYPE ty_s_struc.");
		buildExp("");
		buildExp("    CLEAR ls_struc.");
		buildExp("    \" comment");
		buildExp("*    comment");
		buildExp("    ls_struc-id   = 1.");
		buildExp("    APPEND ls_struc TO et_table.");
		buildExp("");
		buildExp("    any_method( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearStructureBeforeAssignRemoved() {
		rule.configKeepStrucBeforeAssign.setValue(false);
		
		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA ls_struc TYPE ty_s_struc.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("");
		buildSrc("    CLEAR ls_struc.");
		buildSrc("    ls_struc-id   = 1.");
		buildSrc("    APPEND ls_struc TO et_table.");
		buildSrc("");
		buildSrc("    any_method( ).");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA ls_struc TYPE ty_s_struc.");
		buildExp("");
		buildExp("    ls_struc-id   = 1.");
		buildExp("    APPEND ls_struc TO et_table.");
		buildExp("");
		buildExp("    any_method( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testToDoAtStartDeleteAtEnd() {
		// ensure that "CLEAR lv_third" is removed from the end, because its "VALUE 3" does not matter there
		
		rule.configActionAtStart.setEnumValue(NeedlessClearAction.ADD_TODO_COMMENT);
		rule.configActionAtEnd.setEnumValue(NeedlessClearAction.DELETE);

		buildSrc("    DATA lv_any   TYPE i.");
		buildSrc("    DATA lv_other TYPE string.");
		buildSrc("    DATA lv_third TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    CLEAR lv_any.");
		buildSrc("    CLEAR: ev_result,");
		buildSrc("           lv_other,");
		buildSrc("           et_table,");
		buildSrc("           lv_third.");
		buildSrc("");
		buildSrc("    any_method( ).");
		buildSrc("");
		buildSrc("    CLEAR: lv_other, lv_any.");
		buildSrc("    CLEAR lv_third.");
		buildSrc("    CLEAR ev_table.");

		buildExp("    DATA lv_any   TYPE i.");
		buildExp("    DATA lv_other TYPE string.");
		buildExp("    DATA lv_third TYPE i VALUE 3.");
		buildExp("");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lv_any.");
		buildExp("    CLEAR: ev_result,");
		buildExp("           \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("           lv_other,");
		buildExp("           et_table,");
		buildExp("           lv_third.");
		buildExp("");
		buildExp("    any_method( ).");
		buildExp("");
		buildExp("    CLEAR ev_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testEmptyMethod() {
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClearExportingParameterKept() {
		// ensure that 'CLEAR ev_value' is kept even with the method signature being visible, 
		// i.e. when EV_VALUE is part of to the LocalVariables of ANY_METHOD

		buildSrc("CLASS any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      EXPORTING ev_value TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    CLEAR ev_value.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testClearOnSameLineAsPrevCommand() {
		buildSrc("    DATA lo_any TYPE REF TO lcl_any.");
		buildSrc("    FREE lo_any. CLEAR lo_any.");

		buildExp("    DATA lo_any TYPE REF TO lcl_any.");
		buildExp("    FREE lo_any.");
		buildExp("    \" TODO: remove needless CLEAR (ABAP cleaner)");
		buildExp("    CLEAR lo_any.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}