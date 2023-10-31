package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class NeedlessSpacesTest extends RuleTestBase {
	private NeedlessSpacesRule rule;
	
	NeedlessSpacesTest() {
		super(RuleID.NEEDLESS_SPACES);
		rule = (NeedlessSpacesRule)getRule();
		
		rule.configSearchAcrossEmptyLines.setValue(true);
		rule.configSearchAcrossCommentLines.setValue(true);
		rule.configProcessLineEndComments.setValue(false);
		rule.configProcessEmptyBrackets.setValue(true);
	}

	@Test
	void testSimpleCases() {
		buildSrc("    CLEAR:    ev_value,");
		buildSrc("              ev_other_value.");
		buildSrc("    CLEAR     ev_third_value.");
		buildSrc("");
		buildSrc("    SORT lt_table   BY  first_comp    ASCENDING");
		buildSrc("                        second_comp   DESCENDING");
		buildSrc("                        third_comp.");

		buildExp("    CLEAR: ev_value,");
		buildExp("           ev_other_value.");
		buildExp("    CLEAR  ev_third_value.");
		buildExp("");
		buildExp("    SORT lt_table BY first_comp  ASCENDING");
		buildExp("                     second_comp DESCENDING");
		buildExp("                     third_comp.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCondensedSimpleCasesUnchanged() {
		buildSrc("    CLEAR: ev_value,");
		buildSrc("           ev_other_value.");
		buildSrc("    CLEAR  ev_third_value.");
		buildSrc("");
		buildSrc("    SORT lt_table BY first_comp  ASCENDING");
		buildSrc("                     second_comp DESCENDING");
		buildSrc("                     third_comp.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSearchAcrossEmptyLinesAndComments() {
		buildSrc("    a       =  1.");
		buildSrc("    bb      =  2.");
		buildSrc("");
		buildSrc("    ccc     =  3.");
		buildSrc("    dddd    =  4.");
		buildSrc("    \" comment");
		buildSrc("    eeeee   =  5.");
		buildSrc("    ffffff  =  6.");

		buildExp("    a      = 1.");
		buildExp("    bb     = 2.");
		buildExp("");
		buildExp("    ccc    = 3.");
		buildExp("    dddd   = 4.");
		buildExp("    \" comment");
		buildExp("    eeeee  = 5.");
		buildExp("    ffffff = 6.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testSearchAcrossEmptyLinesOnly() {
		rule.configSearchAcrossCommentLines.setValue(false);

		buildSrc("    a       =  1.");
		buildSrc("    bb      =  2.");
		buildSrc("");
		buildSrc("    ccc     =  3.");
		buildSrc("    dddd    =  4.");
		buildSrc("    \" comment");
		buildSrc("    eeeee   =  5.");
		buildSrc("    ffffff  =  6.");

		buildExp("    a    = 1.");
		buildExp("    bb   = 2.");
		buildExp("");
		buildExp("    ccc  = 3.");
		buildExp("    dddd = 4.");
		buildExp("    \" comment");
		buildExp("    eeeee  = 5.");
		buildExp("    ffffff = 6.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSearchAcrossCommentsOnly() {
		rule.configSearchAcrossEmptyLines.setValue(false);

		buildSrc("    a       =  1.");
		buildSrc("    bb      =  2.");
		buildSrc("");
		buildSrc("    ccc     =  3.");
		buildSrc("    dddd    =  4.");
		buildSrc("    \" comment");
		buildSrc("    eeeee   =  5.");
		buildSrc("    ffffff  =  6.");

		buildExp("    a  = 1.");
		buildExp("    bb = 2.");
		buildExp("");
		buildExp("    ccc    = 3.");
		buildExp("    dddd   = 4.");
		buildExp("    \" comment");
		buildExp("    eeeee  = 5.");
		buildExp("    ffffff = 6.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSearchAcrossEmptyLinesOrComments() {
		rule.configSearchAcrossEmptyLines.setValue(false);
		rule.configSearchAcrossCommentLines.setValue(false);

		buildSrc("    a       =  1.");
		buildSrc("    bb      =  2.");
		buildSrc("");
		buildSrc("    ccc     =  3.");
		buildSrc("    dddd    =  4.");
		buildSrc("    \" comment");
		buildSrc("    eeeee   =  5.");
		buildSrc("    ffffff  =  6.");

		buildExp("    a  = 1.");
		buildExp("    bb = 2.");
		buildExp("");
		buildExp("    ccc  = 3.");
		buildExp("    dddd = 4.");
		buildExp("    \" comment");
		buildExp("    eeeee  = 5.");
		buildExp("    ffffff = 6.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBreakSectionsWhenIndentChanges() {
		// expect processing sections to break whenever indent changes, i.e. expect DO sections to be aligned independently 

		buildSrc("    DO 5 TIMES.");
		buildSrc("      a           =   1.");
		buildSrc("      bbb         =   3.");
		buildSrc("    ENDDO.");
		buildSrc("    DO 10 TIMES.");
		buildSrc("      ccccc       =   5.");
		buildSrc("      ddddddd     =   7.");
		buildSrc("    ENDDO.");
		buildSrc("    eeeeeeeee     =   9.");

		buildExp("    DO 5 TIMES.");
		buildExp("      a   = 1.");
		buildExp("      bbb = 3.");
		buildExp("    ENDDO.");
		buildExp("    DO 10 TIMES.");
		buildExp("      ccccc   = 5.");
		buildExp("      ddddddd = 7.");
		buildExp("    ENDDO.");
		buildExp("    eeeeeeeee = 9.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepLineEndComments() {
		buildSrc("    lv_some_value     =   3.      \" comment");
		buildSrc("    lv_any_value      =   44.     \" second comment, aligned with first one");
		buildSrc("    lv_other_value    =   'abc'.    \" third comment");
		buildSrc("    lv_fourth_value   =   'a'.      \" fourth comment, aligned with third one");

		buildExp("    lv_some_value   = 3.      \" comment");
		buildExp("    lv_any_value    = 44.     \" second comment, aligned with first one");
		buildExp("    lv_other_value  = 'abc'.    \" third comment");
		buildExp("    lv_fourth_value = 'a'.      \" fourth comment, aligned with third one");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveLineEndComments() {
		rule.configProcessLineEndComments.setValue(true);

		buildSrc("    lv_some_value     =   3.    \" comment");
		buildSrc("    lv_any_value      =   44.   \" second comment, aligned with first one");
		buildSrc("    lv_other_value    =   'abc'.  \" third comment");
		buildSrc("    lv_fourth_value   =   'a'.    \" fourth comment, aligned with third one");
 
		buildExp("    lv_some_value   = 3.  \" comment");
		buildExp("    lv_any_value    = 44. \" second comment, aligned with first one");
		buildExp("    lv_other_value  = 'abc'. \" third comment");
		buildExp("    lv_fourth_value = 'a'.   \" fourth comment, aligned with third one");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPreserveRightAlignedAssignmentOps() {
		buildSrc("    lo_any_instance     =   get_instance( ).");
		buildSrc("    lo_other_instance  ?=   get_instance( ).");

		buildExp("    lo_any_instance    = get_instance( ).");
		buildExp("    lo_other_instance ?= get_instance( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPreserveCalculationAssignmentOpsAlignment() {
		buildSrc("    lv_value   +=   1.");
		buildSrc("    lv_value   *=   2.");
		buildSrc("    lv_value    =   3.");
		buildSrc("    lv_value   -=   4.");
		buildSrc("    lv_value   /=   5.");

		buildExp("    lv_value += 1.");
		buildExp("    lv_value *= 2.");
		buildExp("    lv_value  = 3.");
		buildExp("    lv_value -= 4.");
		buildExp("    lv_value /= 5.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPreserveNumericAlignment() {
		buildSrc("    lts_table[ 1 ]-value   =      1.");
		buildSrc("    lts_table[ 2 ]-value   =     42.");
		buildSrc("    lts_table[ 3 ]-value   =   -100.");
		buildSrc("    lts_table[ 4 ]-value   =     '3.14'.");
		buildSrc("    lts_table[ 5 ]-value   =    '43.21-'.");
		buildSrc("    lts_table[ 6 ]-value   =   '-12.34'.");

		buildExp("    lts_table[ 1 ]-value =    1.");
		buildExp("    lts_table[ 2 ]-value =   42.");
		buildExp("    lts_table[ 3 ]-value = -100.");
		buildExp("    lts_table[ 4 ]-value =   '3.14'.");
		buildExp("    lts_table[ 5 ]-value =  '43.21-'.");
		buildExp("    lts_table[ 6 ]-value = '-12.34'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCondensedAssignmentOpsUnchanged() {
		buildSrc("    lo_any_instance    = get_instance( ).");
		buildSrc("    lo_other_instance ?= get_instance( ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCondensedCalculationAssignmentOpsUnchanged() {
		buildSrc("    lv_value += 1.");
		buildSrc("    lv_value *= 2.");
		buildSrc("    lv_value  = 3.");
		buildSrc("    lv_value -= 4.");
		buildSrc("    lv_value /= 5.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCondensedNumericValuesUnchanged() {
		buildSrc("    lts_table[ 1 ]-value =    1.");
		buildSrc("    lts_table[ 2 ]-value =   42.");
		buildSrc("    lts_table[ 3 ]-value = -100.");
		buildSrc("    lts_table[ 4 ]-value =   '3.14'.");
		buildSrc("    lts_table[ 5 ]-value =  '43.21-'.");
		buildSrc("    lts_table[ 6 ]-value = '-12.34'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testParametersInCallChainUnchanged() {
		// expect spaces to be removed only OUTSIDE the method call parameter lists
		
		buildSrc("    lv_result    =   any_method(  a   =  1");
		buildSrc("                                  b   =  2  )   +    other_method(   c  =   3");
		buildSrc("                                                                     d  =   4 ).");
		buildSrc("");
		buildSrc("    lv_result   =  cl_singleton=>get( )->any_method(");
		buildSrc("        iv_param    =    '9' ).");

		buildExp("    lv_result = any_method(  a   =  1");
		buildExp("                             b   =  2  ) + other_method(   c  =   3");
		buildExp("                                                           d  =   4 ).");
		buildExp("");
		buildExp("    lv_result = cl_singleton=>get( )->any_method(");
		buildExp("        iv_param    =    '9' ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSelectUnchanged() {
		buildSrc("    SELECT * FROM any_table");
		buildSrc("      WHERE   a   =  1");
		buildSrc("        AND   b   =  2");
		buildSrc("      INTO TABLE @DATA(lt_result).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDeclarationsUnchanged() {
		buildSrc("    DATA   lv_any   TYPE    i.");
		buildSrc("    DATA   lv_other TYPE    string.");
		buildSrc("");
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("             component_a    TYPE   i,");
		buildSrc("             comp_b         TYPE   string.");
		buildSrc("             INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES: END OF ty_s_any_struc.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodsDeclarationsUnchanged() {
		buildSrc("    METHODS  any_method.");
		buildSrc("    METHODS  other_method.");
		buildSrc("");
		buildSrc("    CLASS-METHODS  any_class_method.");
		buildSrc("    CLASS-METHODS  other_class_method.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testParameterListsUnchanged() {
		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("        iv_any    =    lv_any");
		buildSrc("        iv_other  =    lv_other.");
		buildSrc("");
		buildSrc("    RAISE EXCEPTION   TYPE   cx_any EXPORTING iv_msgid    =   '123'");
		buildSrc("                                              iv_param2   =   2.");
		buildSrc("");
		buildSrc("    RAISE SHORTDUMP TYPE cx_any_type");
		buildSrc("      EXPORTING");
		buildSrc("        any_param     =   1");
		buildSrc("        other_param   =   'text'.");
		buildSrc("");
		buildSrc("    CREATE OBJECT lo_any_object");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = iv_any_value");
		buildSrc("        iv_second_param = lv_other_value.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testComponentListsUnchanged() {
		buildSrc("    READ TABLE ith_table ASSIGNING <ls_entry>");
		buildSrc("         WITH KEY   main_item_id     =    <lo_item>->ms_data-item_id");
		buildSrc("                    name             =    <lo_item>->ms_data-name.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNonAbapUnchanged() {
		buildSrc("    EXEC SQL.");
		buildSrc("      DISCONNECT    :lv_connection_name");
		buildSrc("      DISCONNECT    :lv_other_connection_name");
		buildSrc("    ENDEXEC.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLogicalExpressionUnchanged() {
		buildSrc("    IF   a   =   abap_false  AND  b   >    3");
		buildSrc("    OR   a   =   abap_true   AND  b  <=   10.");
		buildSrc("      \" do something");
		buildSrc("    ELSEIF  a  =  abap_true    AND  b   >   3");
		buildSrc("       AND  a  =  abap_false   AND  b  <=  10.");
		buildSrc("      \" do something else");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    WHILE   a  =  abap_true");
		buildSrc("       OR   b  =  abap_false.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCondExpressionUnchanged() {
		// expect spaces to be removed only outside the COND and SWITCH expressions
		buildSrc("    ev_value   =   COND #( WHEN   iv_value IS SUPPLIED");
		buildSrc("                           THEN   iv_value");
		buildSrc("                           ELSE   gc_default_value ).");
		buildSrc("");
		buildSrc("    lv_text    =   SWITCH string( sy-index");
		buildSrc("                                  WHEN   1   THEN   'one'");
		buildSrc("                                  WHEN   2   THEN   'two'");
		buildSrc("                                  WHEN   3   THEN   'three'");
		buildSrc("                                  ELSE THROW cx_overflow( ) ).");
	
		buildExp("    ev_value = COND #( WHEN   iv_value IS SUPPLIED");
		buildExp("                       THEN   iv_value");
		buildExp("                       ELSE   gc_default_value ).");
		buildExp("");
		buildExp("    lv_text  = SWITCH string( sy-index");
		buildExp("                              WHEN   1   THEN   'one'");
		buildExp("                              WHEN   2   THEN   'two'");
		buildExp("                              WHEN   3   THEN   'three'");
		buildExp("                              ELSE THROW cx_overflow( ) ).");
	
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testProcessAcrossWhenSections() {
		// ensure that the existing alignment across several WHEN sections is kept (but only within a CASE ... ENDCASE)

		rule.configProcessLineEndComments.setValue(true);

		buildSrc("    CASE lv_value.");
		buildSrc("      WHEN 1.         \" case 1");
		buildSrc("        ls_struc-comp1            =   lv_any_value.");
		buildSrc("      WHEN 2.         \" case 2");
		buildSrc("        ls_struc-component2       =   lv_other_value.");
		buildSrc("      WHEN 3.         \" case 3");
		buildSrc("        ls_struc-long_component   =   lv_third_value.");
		buildSrc("      WHEN OTHERS.    \" default ");
		buildSrc("        ls_struc-comp4            =   lv_default_value.");
		buildSrc("    ENDCASE.");
		buildSrc("");
		buildSrc("    CASE lv_second_value.");
		buildSrc("      WHEN 1.         \" case 1");
		buildSrc("        ls_struc-comp1            =   lv_any_value.");
		buildSrc("      WHEN OTHERS.    \" default ");
		buildSrc("        ls_struc-component2       =   lv_default_value.");
		buildSrc("    ENDCASE.");

		buildExp("    CASE lv_value.");
		buildExp("      WHEN 1.      \" case 1");
		buildExp("        ls_struc-comp1          = lv_any_value.");
		buildExp("      WHEN 2.      \" case 2");
		buildExp("        ls_struc-component2     = lv_other_value.");
		buildExp("      WHEN 3.      \" case 3");
		buildExp("        ls_struc-long_component = lv_third_value.");
		buildExp("      WHEN OTHERS. \" default ");
		buildExp("        ls_struc-comp4          = lv_default_value.");
		buildExp("    ENDCASE.");
		buildExp("");
		buildExp("    CASE lv_second_value.");
		buildExp("      WHEN 1.      \" case 1");
		buildExp("        ls_struc-comp1      = lv_any_value.");
		buildExp("      WHEN OTHERS. \" default ");
		buildExp("        ls_struc-component2 = lv_default_value.");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testProcessAcrossElseIfAndElse() {
		// ensure that the existing alignment across ELSEIF and ELSE is kept (but only within an IF ... ENDIF)

		rule.configProcessLineEndComments.setValue(true);

		buildSrc("    IF a = 1.");
		buildSrc("      ls_struc-comp1            =   lv_any_value.");
		buildSrc("    ELSEIF a = 2.");
		buildSrc("      ls_struc-component2       =   lv_other_value.");
		buildSrc("    ELSE.");
		buildSrc("      ls_struc-long_component   =   lv_third_value.");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF b = 1.");
		buildSrc("      ls_struc-comp1            =   lv_any_value.");
		buildSrc("    ELSEIF b = 2.");
		buildSrc("      ls_struc-component2       =   lv_default_value.");
		buildSrc("    ENDIF.");

		buildExp("    IF a = 1.");
		buildExp("      ls_struc-comp1          = lv_any_value.");
		buildExp("    ELSEIF a = 2.");
		buildExp("      ls_struc-component2     = lv_other_value.");
		buildExp("    ELSE.");
		buildExp("      ls_struc-long_component = lv_third_value.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF b = 1.");
		buildExp("      ls_struc-comp1      = lv_any_value.");
		buildExp("    ELSEIF b = 2.");
		buildExp("      ls_struc-component2 = lv_default_value.");
		buildExp("    ENDIF.");

		testRule();
	}

	@Test
	void testAvoidOverlappingOfLeftAndRightAlign() {
		// ensure that overlapping of left-aligned and right-aligned Tokens is avoided, i.e. we do NOT get result like:
		//   lv_long_var_name = 'abc'.
		//   lv_short_name  &&= 'def'.
		// This could happen if NeedlessSpacesRule.processSequence only calculated spacesToSpace from the respective   
		// Token.spacesLeft, instead of using the maxPrevEndIndex. 

		buildSrc("    lv_long_var_name    =   'abc'.");
		buildSrc("    lv_short_name     &&=   'def'.");
		buildSrc("    lv_long_var_name    =   'abc'.");
		buildSrc("    lv_short_name     &&=   'def'.");

		buildExp("    lv_long_var_name   = 'abc'.");
		buildExp("    lv_short_name    &&= 'def'.");
		buildExp("    lv_long_var_name   = 'abc'.");
		buildExp("    lv_short_name    &&= 'def'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAvoidOverlappingWithNumbers() {
		// ensure that overlapping of left-aligned and right-aligned Tokens is avoided, i.e. we do NOT get result like:
		// lv_long_var_name = 12.");
		// lv_short_name =  1234.");
		// lv_value =     123456.");

		buildSrc("    lv_long_var_name =       12.");
		buildSrc("    lv_short_name =        1234.");
		buildSrc("    lv_value =           123456.");

		buildExp("    lv_long_var_name =     12.");
		buildExp("    lv_short_name =      1234.");
		buildExp("    lv_value =         123456.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAvoidOverlappingUnchanged() {
		// ensure that the following is not condensed any further:
		
		buildSrc("    lv_long_var_name =     12.");
		buildSrc("    lv_short_name =      1234.");
		buildSrc("    lv_value =         123456.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodChain() {
		buildSrc("    ev_result = class_name=>get_tool(  )->get_value(    ).");

		buildExp("    ev_result = class_name=>get_tool( )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodChainUnchanged() {
		rule.configProcessEmptyBrackets.setValue(false);

		buildSrc("    ev_result = class_name=>get_tool(  )->get_value(    ).");

		copyExpFromSrc(); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentLineInsideCommand() {
		buildSrc("    class_name=>get_tool(");
		buildSrc("        \" comment");
		buildSrc("        )->get_value(    ).");

		buildExp("    class_name=>get_tool(");
		buildExp("        \" comment");
		buildExp("        )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLineBreaksNotRemoved() {
		// expect no change in case of line breaks within the brackets
		
		buildSrc("    ev_result = get_value(");
		buildSrc("                ).");

		copyExpFromSrc(); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIndentAdjustedForEmptyParens() {
		// expect the indent of the second and third line to be adjusted, but not the indent of the last three lines
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method( iv_any_param    = 1");
		buildSrc("                                                                iv_other_param  = 2");
		buildSrc("                                                                iv_third_param  = 3 ).");

		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method( iv_any_param    = 1");
		buildExp("                                                        iv_other_param  = 2");
		buildExp("                                                        iv_third_param  = 3 ).");

		testRule();
	}

	@Test
	void testIndentAdjustedForAttachedLiterals() {
		// expect the indent of the second and third line to be adjusted, but not the indent of the last three lines
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method( iv_any_param    = 1");
		buildSrc("                                                                iv_other_param  = 2");
		buildSrc("                                                                iv_third_param  = 3 ).");
		buildSrc("");
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method(");
		buildSrc("        iv_any_param    = 1");
		buildSrc("        iv_other_param  = 2");
		buildSrc("        iv_third_param  = 3 ).");

		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method( iv_any_param    = 1");
		buildExp("                                                        iv_other_param  = 2");
		buildExp("                                                        iv_third_param  = 3 ).");
		buildExp("");
		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method(");
		buildExp("        iv_any_param    = 1");
		buildExp("        iv_other_param  = 2");
		buildExp("        iv_third_param  = 3 ).");

		testRule();
	}

}
