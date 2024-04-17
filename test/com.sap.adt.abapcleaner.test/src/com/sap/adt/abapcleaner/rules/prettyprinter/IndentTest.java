package com.sap.adt.abapcleaner.rules.prettyprinter;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class IndentTest extends RuleTestBase {
	private IndentRule rule;
	
	IndentTest() {
		super(RuleID.INSET);
		rule = (IndentRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnClassDefinitionSections.setValue(true);
		
		rule.configAlignWithFollowingElse.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
		rule.configAlignWithFollowingWhen.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
		rule.configAlignWithFollowingCatch.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
	}
	
	@Test
	void testClassMethodDoLoopIf() {
		buildSrc("CLASS tests IMPLEMENTATION.");
		buildSrc("METHOD pretty_print_indent.");
		buildSrc("DO 4 TIMES.");
		buildSrc("LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_row>).");
		buildSrc("IF <ls_row>-ignore = abap_true.");
		buildSrc("\" comment");
		buildSrc("CONTINUE.");
		buildSrc("ENDIF.");
		buildSrc("ENDLOOP.");
		buildSrc("ENDDO.");
		buildSrc("ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS tests IMPLEMENTATION.");
		buildExp("  METHOD pretty_print_indent.");
		buildExp("    DO 4 TIMES.");
		buildExp("      LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_row>).");
		buildExp("        IF <ls_row>-ignore = abap_true.");
		buildExp("          \" comment");
		buildExp("          CONTINUE.");
		buildExp("        ENDIF.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDDO.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testSelectIntoTable() {
		// expect the indent to NOT change, as no ENDSELECT is required here

		buildSrc("    SELECT * FROM dtab INTO TABLE @DATA(lt_item).");
		buildSrc("");
		buildSrc("    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_item");
		buildSrc("      FROM dtab WHERE item_id = 'ABC'.");
		buildSrc("");
		buildSrc("    SELECT FROM dtab FIELDS field_a, field_b INTO TABLE @mt_table.");
	    
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSelectAppendingTable() {
		// expect the indent to NOT change, as no ENDSELECT is required here

		buildSrc("    SELECT * FROM dtab APPENDING TABLE gt_table");
		buildSrc("      FOR ALL ENTRIES IN lt_item");
		buildSrc("      WHERE field = lt_item-component.");
		buildSrc("");
		buildSrc("    SELECT FROM dtab");
		buildSrc("      FIELDS field_a, field_b");
		buildSrc("      APPENDING CORRESPONDING FIELDS OF TABLE @mts_buffer.");
	    
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSelectSingle() {
		// expect the indent to NOT change, as no ENDSELECT is required here

		buildSrc("    SELECT SINGLE field_name INTO lv_value");
		buildSrc("      FROM dtab");
		buildSrc("      WHERE other_field = gc_any_value.");
		buildSrc("");
	    
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSelectAggregateFunctionsOnly() {
		// expect the indent to NOT change, as no ENDSELECT is required here

		buildSrc("    SELECT MAX( field_a ) MAX( field_b )");
		buildSrc("      INTO ( gv_result_a, lv_result_b )");
		buildSrc("      FROM dtab");
		buildSrc("      WHERE any_field NE space.");
		buildSrc("");
		buildSrc("    SELECT MIN( aa ) AS a, MAX( bb ) AS b, SUM( cc ) AS c, \" comment");
		buildSrc("           STDDEV( ee ) AS e, CORR_SPEARMAN( ff, gg ) AS f");
		buildSrc("      FROM any_table");
		buildSrc("      INTO CORRESPONDING FIELDS OF wa_test");
		buildSrc("      WHERE kdauf NE space.");
		buildSrc("");
		buildSrc("    SELECT COUNT(*) INTO rv_value");
		buildSrc("      FROM dtab2");
		buildSrc("      WHERE bukrs EQ mo_instance->mv_bukrs.");
		buildSrc("");
		buildSrc("    SELECT COUNT( * )");
		buildSrc("      FROM dtab");
		buildSrc("      INTO rv_result");
		buildSrc("      WHERE bukrs EQ mo_instance->mv_bukrs.");
		buildSrc("");
	    
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSelectWithEndselect() {
		// expect indent to be added, because all SELECT statements need ENDSELECT

		buildSrc("    SELECT DISTINCT a b c FROM ztest_table INTO CORRESPONDING FIELDS OF wa_test.");
		buildSrc("    SELECT * INTO wa_test FROM dtab WHERE item_id = 'ITEM_1'.");
		buildSrc("");
		buildSrc("    SELECT FROM dtab");
		buildSrc("      FIELDS field_a, field_b");
		buildSrc("      INTO @wa_test3.");
		buildSrc("");
		buildSrc("    SELECT MIN( aa ), MAX( b ) AS bb, SUM( cc ), \" comment");
		buildSrc("           STDDEV( ee ), ff");
		buildSrc("      FROM my_table");
		buildSrc("      INTO CORRESPONDING FIELDS OF wa_test");
		buildSrc("      WHERE kdauf NE space.");
		buildSrc("");
		buildSrc("    \" inner statement.");
		buildSrc("    ENDSELECT.");
		buildSrc("    ENDSELECT.");
		buildSrc("    ENDSELECT.");
		buildSrc("    ENDSELECT.");

		buildExp("    SELECT DISTINCT a b c FROM ztest_table INTO CORRESPONDING FIELDS OF wa_test.");
		buildExp("      SELECT * INTO wa_test FROM dtab WHERE item_id = 'ITEM_1'.");
		buildExp("");
		buildExp("        SELECT FROM dtab");
		buildExp("          FIELDS field_a, field_b");
		buildExp("          INTO @wa_test3.");
		buildExp("");
		buildExp("          SELECT MIN( aa ), MAX( b ) AS bb, SUM( cc ), \" comment");
		buildExp("                 STDDEV( ee ), ff");
		buildExp("            FROM my_table");
		buildExp("            INTO CORRESPONDING FIELDS OF wa_test");
		buildExp("            WHERE kdauf NE space.");
		buildExp("");
		buildExp("            \" inner statement.");
		buildExp("          ENDSELECT.");
		buildExp("        ENDSELECT.");
		buildExp("      ENDSELECT.");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOnEndOfTask() {
		// make sure that "ON END OF TASK" is not mixed up with a BEGIN OF ... END OF section in declaration statements, 
		// i.e. that it does not reduce the indent by changing the 'block level' in Command.addNext
		
		buildSrc("    DO.");
		buildSrc("    CALL FUNCTION 'FUNCTION_NAME'");
		buildSrc("         STARTING NEW TASK name");
		buildSrc("         DESTINATION mc_rfc_destination");
		buildSrc("         CALLING on_end_of_rfc ON END OF TASK.");
		buildSrc("    ENDDO.");
	    
		buildExp("    DO.");
		buildExp("      CALL FUNCTION 'FUNCTION_NAME'");
		buildExp("           STARTING NEW TASK name");
		buildExp("           DESTINATION mc_rfc_destination");
		buildExp("           CALLING on_end_of_rfc ON END OF TASK.");
		buildExp("    ENDDO.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// test configuration settings for class definition 
	
	@Test
	void testClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(true);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("PUBLIC SECTION.");
		buildSrc("INTERFACES if_any_interface.");
		buildSrc("PROTECTED SECTION.");
		buildSrc("DATA mo_item TYPE REF TO if_item.");
		buildSrc("PRIVATE SECTION.");
		buildSrc("DATA mo_other_item TYPE REF TO if_other_interface.");
		buildSrc("CLASS-METHODS create");
		buildSrc("  RETURNING");
		buildSrc("    VALUE(ro_instance) TYPE REF TO if_third_interface.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_item TYPE REF TO if_item.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA mo_other_item TYPE REF TO if_other_interface.");
		buildExp("    CLASS-METHODS create");
		buildExp("      RETURNING");
		buildExp("        VALUE(ro_instance) TYPE REF TO if_third_interface.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testSkipClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("PUBLIC SECTION.");
		buildSrc("INTERFACES if_any_interface.");
		buildSrc("PROTECTED SECTION.");
		buildSrc("DATA mo_item TYPE REF TO if_item.");
		buildSrc("PRIVATE SECTION.");
		buildSrc("DATA mo_other_item TYPE REF TO if_other_interface.");
		buildSrc("CLASS-METHODS create");
		buildSrc("  RETURNING");
		buildSrc("    VALUE(ro_instance) TYPE REF TO if_third_interface.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	// -------------------------------------------------------------------------
	// test comment alignment configuration settings for ELSE / ELSEIF
	
	@Test
	void testAlwaysAlignCommentWithElse() {
		rule.configAlignWithFollowingElse.setEnumValue(AlignWithNextCommandMode.ALWAYS);

		buildSrc("IF iv_value = 1.");
		buildSrc("iv_value += 1.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("ELSEIF iv_value = 2.");
		buildSrc("iv_value += 2.");
		buildSrc("");
		buildSrc("\" two comment lines on the ELSE branch");
		buildSrc("\" with an empty line above them");
		buildSrc("ELSE.");
		buildSrc("iv_value += 1.");
		buildSrc("ENDIF.");

		buildExp("    IF iv_value = 1.");
		buildExp("      iv_value += 1.");
		buildExp("    \" comment with no empty line above it");
		buildExp("    ELSEIF iv_value = 2.");
		buildExp("      iv_value += 2.");
		buildExp("");
		buildExp("    \" two comment lines on the ELSE branch");
		buildExp("    \" with an empty line above them");
		buildExp("    ELSE.");
		buildExp("      iv_value += 1.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignCommentWithElseIfElse() {
		rule.configAlignWithFollowingElse.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);

		buildSrc("IF iv_value = 1.");
		buildSrc("\" comment on next line");
		buildSrc("iv_value += 3.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("ELSEIF iv_value = 2.");
		buildSrc("iv_value += 2.");
		buildSrc("");
		buildSrc("\" two comment lines on the ELSE branch");
		buildSrc("\" with an empty line above them");
		buildSrc("ELSE.");
		buildSrc("iv_value += 1.");
		buildSrc("ENDIF.");

		buildExp("    IF iv_value = 1.");
		buildExp("      \" comment on next line");
		buildExp("      iv_value += 3.");
		buildExp("      \" comment with no empty line above it");
		buildExp("    ELSEIF iv_value = 2.");
		buildExp("      iv_value += 2.");
		buildExp("");
		buildExp("    \" two comment lines on the ELSE branch");
		buildExp("    \" with an empty line above them");
		buildExp("    ELSE.");
		buildExp("      iv_value += 1.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNeverAlignCommentWithElse() {
		rule.configAlignWithFollowingElse.setEnumValue(AlignWithNextCommandMode.NEVER);

		buildSrc("IF iv_value = 1.");
		buildSrc("iv_value += 1.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("ELSEIF iv_value = 2.");
		buildSrc("iv_value += 2.");
		buildSrc("");
		buildSrc("\" two comment lines on the ELSE branch");
		buildSrc("\" with an empty line above them");
		buildSrc("ELSE.");
		buildSrc("iv_value += 1.");
		buildSrc("ENDIF.");

		buildExp("    IF iv_value = 1.");
		buildExp("      iv_value += 1.");
		buildExp("      \" comment with no empty line above it");
		buildExp("    ELSEIF iv_value = 2.");
		buildExp("      iv_value += 2.");
		buildExp("");
		buildExp("      \" two comment lines on the ELSE branch");
		buildExp("      \" with an empty line above them");
		buildExp("    ELSE.");
		buildExp("      iv_value += 1.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// test comment alignment configuration settings for WHEN
	
	@Test
	void testAlwaysAlignCommentWithWhen() {
		rule.configAlignWithFollowingWhen.setEnumValue(AlignWithNextCommandMode.ALWAYS);

		buildSrc("CASE iv_value.");
		buildSrc("\" comment on WHEN");
		buildSrc("WHEN 1.");
		buildSrc("\" comment on next line");
		buildSrc("iv_value += 1.");
		buildSrc("");
		buildSrc("\" two comment lines on WHEN");
		buildSrc("\" with an empty line above them");
		buildSrc("WHEN 2.");
		buildSrc("iv_value += 2.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("WHEN 3.");
		buildSrc("iv_value += 3.");
		buildSrc("ENDCASE.");

		buildExp("    CASE iv_value.");
		buildExp("      \" comment on WHEN");
		buildExp("      WHEN 1.");
		buildExp("        \" comment on next line");
		buildExp("        iv_value += 1.");
		buildExp("");
		buildExp("      \" two comment lines on WHEN");
		buildExp("      \" with an empty line above them");
		buildExp("      WHEN 2.");
		buildExp("        iv_value += 2.");
		buildExp("      \" comment with no empty line above it");
		buildExp("      WHEN 3.");
		buildExp("        iv_value += 3.");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignCommentWithWhen() {
		rule.configAlignWithFollowingWhen.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);

		buildSrc("CASE iv_value.");
		buildSrc("\" comment on WHEN");
		buildSrc("WHEN 1.");
		buildSrc("\" comment on next line");
		buildSrc("iv_value += 1.");
		buildSrc("");
		buildSrc("\" two comment lines on WHEN");
		buildSrc("\" with an empty line above them");
		buildSrc("WHEN 2.");
		buildSrc("iv_value += 2.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("WHEN 3.");
		buildSrc("iv_value += 3.");
		buildSrc("ENDCASE.");

		buildExp("    CASE iv_value.");
		buildExp("      \" comment on WHEN");
		buildExp("      WHEN 1.");
		buildExp("        \" comment on next line");
		buildExp("        iv_value += 1.");
		buildExp("");
		buildExp("      \" two comment lines on WHEN");
		buildExp("      \" with an empty line above them");
		buildExp("      WHEN 2.");
		buildExp("        iv_value += 2.");
		buildExp("        \" comment with no empty line above it");
		buildExp("      WHEN 3.");
		buildExp("        iv_value += 3.");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNeverAlignCommentWithWhen() {
		rule.configAlignWithFollowingWhen.setEnumValue(AlignWithNextCommandMode.NEVER);

		buildSrc("CASE iv_value.");
		buildSrc("\" comment on WHEN");
		buildSrc("WHEN 1.");
		buildSrc("\" comment on next line");
		buildSrc("iv_value += 1.");
		buildSrc("");
		buildSrc("\" two comment lines on WHEN");
		buildSrc("\" with an empty line above them");
		buildSrc("WHEN 2.");
		buildSrc("iv_value += 2.");
		buildSrc("\" comment with no empty line above it");
		buildSrc("WHEN 3.");
		buildSrc("iv_value += 3.");
		buildSrc("ENDCASE.");

		buildExp("    CASE iv_value.");
		buildExp("      \" comment on WHEN");
		buildExp("      WHEN 1.");
		buildExp("        \" comment on next line");
		buildExp("        iv_value += 1.");
		buildExp("");
		buildExp("        \" two comment lines on WHEN");
		buildExp("        \" with an empty line above them");
		buildExp("      WHEN 2.");
		buildExp("        iv_value += 2.");
		buildExp("        \" comment with no empty line above it");
		buildExp("      WHEN 3.");
		buildExp("        iv_value += 3.");
		buildExp("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// test comment alignment configuration settings for CATCH
	
	@Test
	void testAlwaysAlignCommentWithCatch() {
		rule.configAlignWithFollowingCatch.setEnumValue(AlignWithNextCommandMode.ALWAYS);

		buildSrc("TRY.");
		buildSrc("\" comment");
		buildSrc("any_method_call( ).");
		buildSrc("\" two comment lines on CATCH");
		buildSrc("\" with no empty line above them");
		buildSrc("CATCH cx_message.");
		buildSrc("\" no handler");
		buildSrc("");
		buildSrc("\" comment on CATCH with an empty line above it");
		buildSrc("CATCH cx_fatal_message.");
		buildSrc("\" no handler");
		buildSrc("ENDTRY.");

		buildExp("    TRY.");
		buildExp("        \" comment");
		buildExp("        any_method_call( ).");
		buildExp("      \" two comment lines on CATCH");
		buildExp("      \" with no empty line above them");
		buildExp("      CATCH cx_message.");
		buildExp("        \" no handler");
		buildExp("");
		buildExp("      \" comment on CATCH with an empty line above it");
		buildExp("      CATCH cx_fatal_message.");
		buildExp("        \" no handler");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAlignCommentWithCatch() {
		rule.configAlignWithFollowingCatch.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);

		buildSrc("TRY.");
		buildSrc("\" comment");
		buildSrc("any_method_call( ).");
		buildSrc("\" two comment lines on CATCH");
		buildSrc("\" with no empty line above them");
		buildSrc("CATCH cx_message.");
		buildSrc("\" no handler");
		buildSrc("");
		buildSrc("\" comment on CATCH with an empty line above it");
		buildSrc("CATCH cx_fatal_message.");
		buildSrc("\" no handler");
		buildSrc("ENDTRY.");

		buildExp("    TRY.");
		buildExp("        \" comment");
		buildExp("        any_method_call( ).");
		buildExp("        \" two comment lines on CATCH");
		buildExp("        \" with no empty line above them");
		buildExp("      CATCH cx_message.");
		buildExp("        \" no handler");
		buildExp("");
		buildExp("      \" comment on CATCH with an empty line above it");
		buildExp("      CATCH cx_fatal_message.");
		buildExp("        \" no handler");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	
	@Test
	void testNeverAlignCommentWithCatch() {
		rule.configAlignWithFollowingCatch.setEnumValue(AlignWithNextCommandMode.NEVER);

		buildSrc("TRY.");
		buildSrc("\" comment");
		buildSrc("any_method_call( ).");
		buildSrc("\" two comment lines on CATCH");
		buildSrc("\" with no empty line above them");
		buildSrc("CATCH cx_message.");
		buildSrc("\" no handler");
		buildSrc("");
		buildSrc("\" comment on CATCH with an empty line above it");
		buildSrc("CATCH cx_fatal_message.");
		buildSrc("\" no handler");
		buildSrc("ENDTRY.");

		buildExp("    TRY.");
		buildExp("        \" comment");
		buildExp("        any_method_call( ).");
		buildExp("        \" two comment lines on CATCH");
		buildExp("        \" with no empty line above them");
		buildExp("      CATCH cx_message.");
		buildExp("        \" no handler");
		buildExp("");
		buildExp("        \" comment on CATCH with an empty line above it");
		buildExp("      CATCH cx_fatal_message.");
		buildExp("        \" no handler");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------

	@Test
	void testTestSeam() {
		buildSrc("TEST-SEAM selection.");
		buildSrc("SELECT SINGLE * FROM sflight INTO wa");
		buildSrc("       WHERE carrid = carrid");
		buildSrc("         AND connid = connid");
		buildSrc("         AND fldate = fldate.");
		buildSrc("END-TEST-SEAM.");
		buildSrc("");
		buildSrc("TEST-SEAM modification.");
		buildSrc("MODIFY sflight FROM wa.");
		buildSrc("END-TEST-SEAM.");

		buildExp("    TEST-SEAM selection.");
		buildExp("      SELECT SINGLE * FROM sflight INTO wa");
		buildExp("             WHERE carrid = carrid");
		buildExp("               AND connid = connid");
		buildExp("               AND fldate = fldate.");
		buildExp("    END-TEST-SEAM.");
		buildExp("");
		buildExp("    TEST-SEAM modification.");
		buildExp("      MODIFY sflight FROM wa.");
		buildExp("    END-TEST-SEAM.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTestInjection() {
		buildSrc("TEST-INJECTION selection.");
		buildSrc("wa-price = 100.");
		buildSrc("END-TEST-INJECTION.");
		buildSrc("");
		buildSrc("invoke_and_assert( 100 ).");
		buildSrc("");
		buildSrc("TEST-INJECTION modification.");
		buildSrc("END-TEST-INJECTION.");
		buildSrc("");
		buildSrc("invoke_and_assert( 90 ).");
		buildSrc("");
		buildSrc("TEST-INJECTION modification.");
		buildSrc("sy-subrc = 4.");
		buildSrc("END-TEST-INJECTION.");
		buildSrc("");
		buildSrc("invoke_and_assert( -2 ).");

		buildExp("    TEST-INJECTION selection.");
		buildExp("      wa-price = 100.");
		buildExp("    END-TEST-INJECTION.");
		buildExp("");
		buildExp("    invoke_and_assert( 100 ).");
		buildExp("");
		buildExp("    TEST-INJECTION modification.");
		buildExp("    END-TEST-INJECTION.");
		buildExp("");
		buildExp("    invoke_and_assert( 90 ).");
		buildExp("");
		buildExp("    TEST-INJECTION modification.");
		buildExp("      sy-subrc = 4.");
		buildExp("    END-TEST-INJECTION.");
		buildExp("");
		buildExp("    invoke_and_assert( -2 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testReportWithNestedSelectionBlock() {
		// expect NO indent after REPORT, but indent for nested SELECTION-SCREEN BEGIN OF BLOCK
		
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("SELECTION-SCREEN BEGIN OF BLOCK any_block.");
		buildSrc("PARAMETERS p_any TYPE ty_any.");
		buildSrc("PARAMETERS p_other TYPE ty_other.");
		buildSrc("SELECTION-SCREEN BEGIN OF BLOCK inner_block.");
		buildSrc("PARAMETERS p_abc TYPE ty_any.");
		buildSrc("PARAMETERS p_defg TYPE ty_other.");
		buildSrc("SELECTION-SCREEN END OF BLOCK inner_block.");
		buildSrc("SELECTION-SCREEN END OF BLOCK any_block.");

		buildExp("REPORT any_report.");
		buildExp("");
		buildExp("SELECTION-SCREEN BEGIN OF BLOCK any_block.");
		buildExp("  PARAMETERS p_any TYPE ty_any.");
		buildExp("  PARAMETERS p_other TYPE ty_other.");
		buildExp("  SELECTION-SCREEN BEGIN OF BLOCK inner_block.");
		buildExp("    PARAMETERS p_abc TYPE ty_any.");
		buildExp("    PARAMETERS p_defg TYPE ty_other.");
		buildExp("  SELECTION-SCREEN END OF BLOCK inner_block.");
		buildExp("SELECTION-SCREEN END OF BLOCK any_block.");

		testRule();
	}

	@Test
	void testAuthorityCheckDisable() {
		buildSrc("    AUTHORITY-CHECK DISABLE BEGIN CONTEXT root~name.");
		buildSrc("    AUTHORITY-CHECK OBJECT 'ANYOBJ' ID 'ANYID' DUMMY.");
		buildSrc("    AUTHORITY-CHECK DISABLE BEGIN CONTEXT inner~name.");
		buildSrc("    AUTHORITY-CHECK OBJECT 'OTHEROBJ' ID 'OTHERID' DUMMY.");
		buildSrc("    AUTHORITY-CHECK DISABLE END.");
		buildSrc("    AUTHORITY-CHECK DISABLE END.");

		buildExp("    AUTHORITY-CHECK DISABLE BEGIN CONTEXT root~name.");
		buildExp("      AUTHORITY-CHECK OBJECT 'ANYOBJ' ID 'ANYID' DUMMY.");
		buildExp("      AUTHORITY-CHECK DISABLE BEGIN CONTEXT inner~name.");
		buildExp("        AUTHORITY-CHECK OBJECT 'OTHEROBJ' ID 'OTHERID' DUMMY.");
		buildExp("      AUTHORITY-CHECK DISABLE END.");
		buildExp("    AUTHORITY-CHECK DISABLE END.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTryCleanupNested() {
		buildSrc("    TRY.");
		buildSrc("    TRY.");
		buildSrc("    RAISE EXCEPTION exc.");
		buildSrc("    CLEANUP.");
		buildSrc("    cl_demo_output=>write( 'Cleanup' ).");
		buildSrc("    ENDTRY.");
		buildSrc("    CATCH cx_demo1.");
		buildSrc("    cl_demo_output=>write( 'Catching cx_demo1' ).");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        TRY.");
		buildExp("            RAISE EXCEPTION exc.");
		buildExp("          CLEANUP.");
		buildExp("            cl_demo_output=>write( 'Cleanup' ).");
		buildExp("        ENDTRY.");
		buildExp("      CATCH cx_demo1.");
		buildExp("        cl_demo_output=>write( 'Catching cx_demo1' ).");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTryCatchCleanupNested() {
		buildSrc("    TRY.");
		buildSrc("    TRY.");
		buildSrc("    RAISE EXCEPTION exc.");
		buildSrc("    CATCH cx_demo0.");
		buildSrc("    cl_demo_output=>write( 'Catching cx_demo0' ).");
		buildSrc("    CLEANUP.");
		buildSrc("    cl_demo_output=>write( 'Cleanup' ).");
		buildSrc("    ENDTRY.");
		buildSrc("    CATCH cx_demo1.");
		buildSrc("    cl_demo_output=>write( 'Catching cx_demo1' ).");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        TRY.");
		buildExp("            RAISE EXCEPTION exc.");
		buildExp("          CATCH cx_demo0.");
		buildExp("            cl_demo_output=>write( 'Catching cx_demo0' ).");
		buildExp("          CLEANUP.");
		buildExp("            cl_demo_output=>write( 'Cleanup' ).");
		buildExp("        ENDTRY.");
		buildExp("      CATCH cx_demo1.");
		buildExp("        cl_demo_output=>write( 'Catching cx_demo1' ).");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	
	@Test
	void testAlignCommentWithCleanup() {
		rule.configAlignWithFollowingCatch.setEnumValue(AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);

		buildSrc("TRY.");
		buildSrc("\" comment");
		buildSrc("any_method_call( ).");
		buildSrc("\" two comment lines on CLEANUP");
		buildSrc("\" with no empty line above them");
		buildSrc("CLEANUP.");
		buildSrc("\" no handler");
		buildSrc("ENDTRY.");
		buildSrc("");
		buildSrc("TRY.");
		buildSrc("\" comment");
		buildSrc("any_method_call( ).");
		buildSrc("");
		buildSrc("\" comment on CLEANUP with an empty line above it");
		buildSrc("CLEANUP.");
		buildSrc("\" no handler");
		buildSrc("ENDTRY.");

		buildExp("    TRY.");
		buildExp("        \" comment");
		buildExp("        any_method_call( ).");
		buildExp("        \" two comment lines on CLEANUP");
		buildExp("        \" with no empty line above them");
		buildExp("      CLEANUP.");
		buildExp("        \" no handler");
		buildExp("    ENDTRY.");
		buildExp("");
		buildExp("    TRY.");
		buildExp("        \" comment");
		buildExp("        any_method_call( ).");
		buildExp("");
		buildExp("      \" comment on CLEANUP with an empty line above it");
		buildExp("      CLEANUP.");
		buildExp("        \" no handler");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWithNotStartingSelectLoop() {
		// expect the any_method call to be moved to the same indentation as WITH, because no SELECT loop is started 

		buildSrc("    WITH");
		buildSrc("      +cte1 AS ( SELECT fld1, fld2 FROM dtab1");
		buildSrc("                   JOIN dtab2 ON dtab1~id = dtab2~id");
		buildSrc("                   WHERE dtab1~id = @id ),");
		buildSrc("      +cte2 AS ( SELECT COUNT(*) AS cnt FROM +cte1 )");
		buildSrc("      SELECT * FROM +cte2");
		buildSrc("        CROSS JOIN +cte1");
		buildSrc("        ORDER BY fld1, fld2");
		buildSrc("        INTO CORRESPONDING FIELDS OF TABLE @itab.");
		buildSrc("");
		buildSrc("      any_method( itab ).");

		buildExp("    WITH");
		buildExp("      +cte1 AS ( SELECT fld1, fld2 FROM dtab1");
		buildExp("                   JOIN dtab2 ON dtab1~id = dtab2~id");
		buildExp("                   WHERE dtab1~id = @id ),");
		buildExp("      +cte2 AS ( SELECT COUNT(*) AS cnt FROM +cte1 )");
		buildExp("      SELECT * FROM +cte2");
		buildExp("        CROSS JOIN +cte1");
		buildExp("        ORDER BY fld1, fld2");
		buildExp("        INTO CORRESPONDING FIELDS OF TABLE @itab.");
		buildExp("");
		buildExp("    any_method( itab ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWithStartingSelectLoop() {
		// expect the any_method call to get additional indentation, because WITH starts a SELECT loop 

		buildSrc("    WITH");
		buildSrc("      +cte AS ( SELECT FROM dtab1 FIELDS f1, f2 )");
		buildSrc("      SELECT FROM dtab2 AS b");
		buildSrc("        INNER JOIN +cte AS a ON b~g1 = a~f1");
		buildSrc("        FIELDS a~f2, s~g2");
		buildSrc("        WHERE s~g1 = 'NN'");
		buildSrc("        INTO @FINAL(wa)");
		buildSrc("        UP TO 10 ROWS.");
		buildSrc("");
		buildSrc("    any_method( wa ).");
		buildSrc("    ENDWITH.");

		buildExp("    WITH");
		buildExp("      +cte AS ( SELECT FROM dtab1 FIELDS f1, f2 )");
		buildExp("      SELECT FROM dtab2 AS b");
		buildExp("        INNER JOIN +cte AS a ON b~g1 = a~f1");
		buildExp("        FIELDS a~f2, s~g2");
		buildExp("        WHERE s~g1 = 'NN'");
		buildExp("        INTO @FINAL(wa)");
		buildExp("        UP TO 10 ROWS.");
		buildExp("");
		buildExp("      any_method( wa ).");
		buildExp("    ENDWITH.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithLiteralsHostVarsEndSelect() {
		// ENDSELECT is required because only (untyped and typed) literals and host variables are used, 
		// but no aggregate function, since COUNT inside the last arithmetic expression is commented out 
		buildSrc("      SELECT +1 AS untyped1,");
		buildSrc("             -1 AS untyped2,");
		buildSrc("             'any' AS untyped3,");
		buildSrc("             `any` AS untyped4,");
		buildSrc("             1 + 2 * ( 3 + @lc_any ) AS untyped5,");
		buildSrc("");
		buildSrc("             int1`1` AS typed1,");
		buildSrc("             d16n`1.1` AS typed2,");
		buildSrc("             dats`20230419` AS typed3,");
		buildSrc("             cuky`EUR` AS typed4,");
		buildSrc("             concat( char`abc`, @lc_text ) AS typed5");
		buildSrc("");
		buildSrc("             \" ( 1 + @lc_any * COUNT( * ) ) AS aggr_func_used");
		buildSrc("        FROM dtab INTO @DATA(ls_and).");
		buildSrc("      ENDSELECT.");

		buildExp("    SELECT +1 AS untyped1,");
		buildExp("           -1 AS untyped2,");
		buildExp("           'any' AS untyped3,");
		buildExp("           `any` AS untyped4,");
		buildExp("           1 + 2 * ( 3 + @lc_any ) AS untyped5,");
		buildExp("");
		buildExp("           int1`1` AS typed1,");
		buildExp("           d16n`1.1` AS typed2,");
		buildExp("           dats`20230419` AS typed3,");
		buildExp("           cuky`EUR` AS typed4,");
		buildExp("           concat( char`abc`, @lc_text ) AS typed5");
		buildExp("");
		buildExp("           \" ( 1 + @lc_any * COUNT( * ) ) AS aggr_func_used");
		buildExp("      FROM dtab INTO @DATA(ls_and).");
		buildExp("    ENDSELECT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithLiteralsHostVarsAndAggrFunc() {
		// ENDSELECT is NOT required because of the aggregate function COUNT inside the last arithmetic expression 
		buildSrc("      SELECT +1 AS untyped1,");
		buildSrc("             -1 AS untyped2,");
		buildSrc("             'any' AS untyped3,");
		buildSrc("             `any` AS untyped4,");
		buildSrc("             1 + 2 * ( 3 + @lc_any ) AS untyped5,");
		buildSrc("");
		buildSrc("             int1`1` AS typed1,");
		buildSrc("             d16n`1.1` AS typed2,");
		buildSrc("             dats`20230419` AS typed3,");
		buildSrc("             cuky`EUR` AS typed4,");
		buildSrc("             concat( char`abc`, @lc_text ) AS typed5,");
		buildSrc("");
		buildSrc("             ( 1 + @lc_any * COUNT( * ) ) AS aggr_func_used");
		buildSrc("        FROM dtab INTO @DATA(ls_and).");

		buildExp("    SELECT +1 AS untyped1,");
		buildExp("           -1 AS untyped2,");
		buildExp("           'any' AS untyped3,");
		buildExp("           `any` AS untyped4,");
		buildExp("           1 + 2 * ( 3 + @lc_any ) AS untyped5,");
		buildExp("");
		buildExp("           int1`1` AS typed1,");
		buildExp("           d16n`1.1` AS typed2,");
		buildExp("           dats`20230419` AS typed3,");
		buildExp("           cuky`EUR` AS typed4,");
		buildExp("           concat( char`abc`, @lc_text ) AS typed5,");
		buildExp("");
		buildExp("           ( 1 + @lc_any * COUNT( * ) ) AS aggr_func_used");
		buildExp("      FROM dtab INTO @DATA(ls_and).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testModuleAfterEventBlock() {
		// ensure that MODULE is NOT being indented

		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("INITIALIZATION.");
		buildSrc("");
		buildSrc("   MODULE any_module INPUT.");
		buildSrc("     DATA lv_any TYPE i.");
		buildSrc("     lv_any = 1.");
		buildSrc("   ENDMODULE.");

		buildExp("REPORT any_report.");
		buildExp("");
		buildExp("INITIALIZATION.");
		buildExp("");
		buildExp("MODULE any_module INPUT.");
		buildExp("  DATA lv_any TYPE i.");
		buildExp("  lv_any = 1.");
		buildExp("ENDMODULE.");

		testRule();
	}

	@Test
	void testTwoCommandsOnSameLine() {
		// ensure that the assignment (and with it, then WHEN ... THEN ... ELSE ... lines) are NOT moved, 
		// because they follow on the same line as the WHEN command
		
		buildSrc("    CASE sy-tabix.");
		buildSrc("      WHEN 1. lv_flag = COND #(");
		buildSrc("                            WHEN lv_cond = 1");
		buildSrc("                            THEN abap_true");
		buildSrc("                            ELSE abap_false ).");
		buildSrc("    ENDCASE.");

		putAnyMethodAroundSrcAndExp();

		copyExpFromSrc();
		
		testRule();
	}

	@Test
	void testClassDefinitionDeferredChainInEvent() {
		// ensure that the 'CLASS: ... DEFINITION DEFERRED' chain is accepted by the parser, 
		// and that it does NOT close the START-OF-SELECTION block: the 'WRITE 2' Command is executed as part of this block
		
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("WRITE 1.");
		buildSrc("CLASS: lcl_any DEFINITION DEFERRED,");
		buildSrc("       lcl_other DEFINITION DEFERRED.");
		buildSrc("WRITE 2.");
		buildSrc("");
		buildSrc("CLASS lcl_any DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_any IMPLEMENTATION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_other DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS lcl_other IMPLEMENTATION.");
		buildSrc("ENDCLASS.");

		buildExp("REPORT any_report.");
		buildExp("");
		buildExp("START-OF-SELECTION.");
		buildExp("  WRITE 1.");
		buildExp("  CLASS: lcl_any DEFINITION DEFERRED,");
		buildExp("         lcl_other DEFINITION DEFERRED.");
		buildExp("  WRITE 2.");
		buildExp("");
		buildExp("CLASS lcl_any DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS lcl_any IMPLEMENTATION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS lcl_other DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS lcl_other IMPLEMENTATION.");
		buildExp("ENDCLASS.");

		testRule();
	}
}
