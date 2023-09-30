package com.sap.adt.abapcleaner.parser;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.TaskType;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;

public class CodeTest extends CodeTestBase {
	private static final String SEP = ABAP.LINE_SEPARATOR;

	@Test
	void testExecSqlWithDotAsFieldSelector() {
		// If parsed as ABAP code, the following code section would trigger a parse error,
		// because the field selector dot (".") in "any_dtab.any_field" would be
		// mistaken as the end of an ABAP Command. Consequently, a new Command would be started
		// after the dot ("."), containing the text "any_field * ( ... ) ), 2 ) ...", 
		// in which more parentheses are closed than were opened. This would cause an  
		// UnexpectedSyntaxException in Token.addNext when adding the second ")".
		// However, since the code is inside EXEC ... ENDEXEC, this parse error must NOT happen.

		buildSrc("    EXEC SQL.");
		buildSrc("      UPDATE any_dtab");
		buildSrc("        SET any_dtab.any_field");
		buildSrc("            = to_decimal( ");
		buildSrc("               ( any_dtab.any_field *");
		buildSrc("                 ( SELECT quantity");
		buildSrc("                   FROM other_dtab");
		buildSrc("                   WHERE client = :sy-mandt");
		buildSrc("                 )");
		buildSrc("               ),");
		buildSrc("               2 ");
		buildSrc("             )");
		buildSrc("        WHERE client = :sy-mandt");
		buildSrc("          AND field_name = 'ABC'");
		buildSrc("    ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();
		
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testExecSqlInOneLine() {
		buildSrc("    EXEC SQL. CONNECT TO :lv_connection_name ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();

		// check that ENDEXEC is a separate Command 
		assertTrue(code.lastCommand.getPrev().firstToken.isKeyword("ENDEXEC"));
		
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testExecCommentSql() {
		// check that EXEC SQL ... ENDEXEC is correctly identified even if there are comments between "EXEC" and "SQL"

		buildSrc("    EXEC \" comment");
		buildSrc("*      comment line");
		buildSrc("      SQL. ");
		buildSrc("      DISCONNECT :lv_connection_name");
		buildSrc("    ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testExecSqlWithEndExecInComment() {
		// check that EXEC SQL ... ENDEXEC can contain * and " comments 
		// and that ENDEXEC inside a comment is NOT mistaken as ending the section
		
		buildSrc("    EXEC SQL. ");
		buildSrc("********* initial comment line containing the keyword ENDEXEC.");
      buildSrc("      ALTER SYSTEM ALTER CONFIGURATION ('global.ini', 'system') \" line-end comment");
		buildSrc("      \" another comment containing the keyword ENDEXEC.");
      buildSrc("      SET ('expensive_statement', 'trace_parameter_values') = 'true' WITH RECONFIGURE;");
		buildSrc("********* final comment line containing the keyword ENDEXEC.");
		buildSrc("    ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testExecSqlWithEndExecInLiteral() {
		// check that EXEC SQL ... ENDEXEC can contain '...' literals 
		// and that "ENDEXEC" inside a literal is NOT mistaken as ending the section
		
		buildSrc("    EXEC SQL. ");
      buildSrc("      ALTER SYSTEM ALTER CONFIGURATION ('global.ini', 'system')");
      buildSrc("      SET ('ENDEXEC.', 'trace_parameter_values') = 'true' WITH RECONFIGURE;");
		buildSrc("    ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testEmptyExecSql() {
		buildSrc("    EXEC SQL.");
		buildSrc("    ENDEXEC.");
		
		putAnyMethodAroundSrc();
		
		Code code = testParseCode();

		// check that ENDEXEC is identified as ABAP code 
		assertEquals(Language.ABAP, code.lastCommand.getPrev().getLanguage());
		
		assertExecSqlSectionFound(code.firstCommand.getNext(), code.lastCommand.getPrev());
	}

	@Test
	void testMethodByDatabase() {
		buildSrc("  METHOD get_wbs BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING prps tkkaa PRHI.");
      buildSrc("    declare lv_table_lines int;");
      buildSrc("");
      buildSrc("    lt_result = select a.mandt, a.abgsl");
      buildSrc("                from dtab as a");
      buildSrc("                where a.rflg1 = 'X';");
		buildSrc("  ENDMETHOD.");
		
		Code code = testParseCode();
		assertMethodByDatabaseSectionFound(code.firstCommand, code.lastCommand);
	}

	@Test
	void testClassDefinitionLoad() {
		// check that the (obsolete) CLASS ... DEFINITION LOAD statement (which IS NOT followed by ENDCLASS) is handled correctly 
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapclass_interface_load.htm
		
		buildSrc("  CLASS cl_nn DEFINITION LOAD.");
		
		testParseCode();
	}

	@Test
	void testInterfaceLoad() {
		// check that the (obsolete) INTERFACE ... LOAD statement (which does NOT require an ENDINTERFACE statement) is handled correctly
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapclass_interface_load.htm

		buildSrc("  PUBLIC SECTION.");
      buildSrc("    TYPE-POOLS abap .");
      buildSrc("    INTERFACE /abc/if_any_interface LOAD .");
      buildSrc("    INTERFACES /abc/if_other_interface .");
		
		putAnyClassDefAroundSrc();
		
		testParseCode();
	}

	@Test
	void testInterfaceDeferred() {
		// check that the INTERFACE ... DEFERRED statement (which does NOT require an ENDINTERFACE statement) is handled correctly
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapinterface_deferred.htm
		
      buildSrc(" INTERFACE intf2 DEFERRED.");
      buildSrc("");
      buildSrc(" INTERFACE intf1.");
      buildSrc("   DATA iref TYPE REF TO intf2.");
      buildSrc(" ENDINTERFACE.");
      buildSrc("");
      buildSrc(" INTERFACE intf2.");
      buildSrc("   \" ...");
      buildSrc(" ENDINTERFACE.");

      testParseCode();
	}

	@Test
	void testCatchSystemExceptions() {
		// check that the (obsolete) CATCH SYSTEM-EXCEPTIONS statement (which requires an ENDCATCH statement) is handled correctly
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=ABAPCATCH_SYS.htm

		buildSrc("    CATCH SYSTEM-EXCEPTIONS arithmetic_errors = 3.");
      buildSrc("      output = input.");
      buildSrc("    ENDCATCH.");
		
		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testCleanupRangeOnInterfaceDefinition() {
		// ensure that the cleanup range is correctly expanded to the whole interface definition
		
		buildSrc("INTERFACE if_test");
		buildSrc("  PUBLIC.");
		buildSrc("");
		buildSrc("  METHODS any_method.");
		buildSrc("  METHODS other_method.");
		buildSrc("");
		buildSrc("  METHODS third_method.");
		buildSrc("ENDINTERFACE.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
	}

	@Test
	void testCleanupRangeOnClassDefinition() {
		// ensure that the cleanup range is correctly expanded to the sections of a class definition as defined here

		buildSrc("CLASS lcl_test DEFINITION FINAL CREATE PRIVATE FOR TESTING.");
		endOfExpandSection();
		
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_test_class.");
		buildSrc("");
		buildSrc("    CLASS-METHODS create RETURNING VALUE(io_any_ref) TYPE REF TO lcl_test.");
		buildSrc("    METHODS any_public_method.");
		endOfExpandSection();
	
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    DATA mv_protected TYPE abap_bool.");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("    METHODS other_protected_method.");
		endOfExpandSection();

		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_private TYPE abap_bool.");
		buildSrc("    DATA mv_next_num TYPE i.");
		endOfExpandSection();
		
		buildSrc("ENDCLASS.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
	}

	@Test
	void testCleanupRangeOnClassImplementation() {
		// ensure that the cleanup range is correctly expanded to the sections of a class implementation as defined here
		
		buildSrc("CLASS lcl_test IMPLEMENTATION.");
		endOfExpandSection();

		buildSrc("  METHOD alpha.");
		buildSrc("    \" do something");
		buildSrc("  ENDMETHOD.");
		endOfExpandSection();
		
		buildSrc("  METHOD beta.");
		buildSrc("    CONSTANTS lc_count TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    DO lc_count TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");
		buildSrc("  ENDMETHOD.");
		endOfExpandSection();
		
		buildSrc("ENDCLASS.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
	}
	
	@Test
	void testCleanupRangeOnClassDefinition2() {
		// ensure that the cleanup range is correctly expanded to the whole class definition
		
		buildSrc("CLASS lcl_test DEFINITION FINAL CREATE PRIVATE FOR TESTING.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_test_class.");
		buildSrc("");
		buildSrc("    CLASS-METHODS create RETURNING VALUE(io_any_ref) TYPE REF TO lcl_test.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    DATA mv_protected TYPE abap_bool.");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_private TYPE abap_bool.");
		buildSrc("    DATA mv_next_num TYPE i.");
		buildSrc("ENDCLASS.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_CLASS);
	}

	@Test
	void testCleanupRangeOnClassImplementation2() {
		// ensure that the cleanup range is correctly expanded to the whole class implementation
		
		buildSrc("CLASS lcl_test IMPLEMENTATION.");
		buildSrc("  METHOD alpha.");
		buildSrc("    \" do something");
		buildSrc("  ENDMETHOD.");
		buildSrc("  METHOD beta.");
		buildSrc("    CONSTANTS lc_count TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    DO lc_count TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_CLASS);
	}
	
	@Test
	void testCleanupRangeOnMethodsOnly() {
		// ensure that the cleanup range is correctly expanded to method level even if the CLASS definition is missing
		
		buildSrc("  METHOD alpha.");
		buildSrc("    \" do something");
		buildSrc("  ENDMETHOD.");
		endOfExpandSection();
		
		buildSrc("  METHOD beta.");
		buildSrc("    CONSTANTS lc_count TYPE i VALUE 3.");
		buildSrc("");
		buildSrc("    DO lc_count TIMES.");
		buildSrc("      \" do something");
		buildSrc("    ENDDO.");
		buildSrc("  ENDMETHOD.");
		endOfExpandSection();
		
		buildSrc("  METHOD gamma.");
		buildSrc("    \" do something");
		buildSrc("  ENDMETHOD.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
	}

	@Test
	void testCleanupRangeOnReport() {
		// ensure that the cleanup range is correctly expanded to the sections of a report as defined here
		
		buildSrc("REPORT my_report.");
		buildSrc("DATA:");
		buildSrc("  gv_var_1 TYPE i,");
		buildSrc("  gv_var_2 TYPE string.");
		buildSrc("");
		buildSrc("SELECTION-SCREEN BEGIN OF BLOCK programs WITH FRAME TITLE lbl_a.");
		buildSrc("  PARAMETERS:");
		buildSrc("    p_any TYPE boolean AS CHECKBOX DEFAULT abap_true.");
		buildSrc("");
		buildSrc("  SELECTION-SCREEN COMMENT /1(60) comm_gen.");
		buildSrc("SELECTION-SCREEN END OF BLOCK programs.");
		buildSrc("");
		buildSrc("SELECTION-SCREEN BEGIN OF BLOCK delay WITH FRAME TITLE lbl_b.");
		buildSrc("  PARAMETERS:");
		buildSrc("    p_abc(2) TYPE n DEFAULT gc_abc.");
		buildSrc("SELECTION-SCREEN END OF BLOCK delay.");
		endOfExpandSection();
		
		buildSrc("INITIALIZATION.");
		buildSrc("  lbl_a = 'Some text' ##NO_TEXT.");
		buildSrc("  lbl_b = 'More text' ##NO_TEXT.");
		endOfExpandSection();
		
		buildSrc("INITIALIZATION.");
		buildSrc("  txt_any = 'text' ##NO_TEXT.");
		endOfExpandSection();
		
		buildSrc("AT SELECTION-SCREEN OUTPUT.");
		buildSrc("  p_any = cl_class=>method( iv_param = p_any ).");
		endOfExpandSection();
		
		buildSrc("AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xyz.");
		buildSrc("  \" comment");
		buildSrc("  p_xyz = cl_class=>method( p_any ).");
		endOfExpandSection();
		
		buildSrc("START-OF-SELECTION.");
		buildSrc("  gt_id = go_run->get_ids( so_any[] ).");
		buildSrc("  WRITE /.");
		endOfExpandSection();
		
		testParseWithCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
	}

	@Test
	void testPragmaAndCommentLine() {
		buildSrc("    ##pragma \" comment");

		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testMultiPragmaAndCommentLine() {
		buildSrc("    ##pragma ##pragma ##pragma \" comment");

		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testPragmaPeriodComment() {
		buildSrc("  METHOD main ##pragma. \" comment");
		buildSrc("    ##pragma. \" comment");
		buildSrc("    method_call( ) ##pragma. \" comment");
		buildSrc("    ##pragma. \" comment");
		buildSrc("  ENDMETHOD ##pragma. \" comment");
		
		testParseCode();
	}

	@Test
	void testPeriodPragmaComment() {
		// a pragma should be placed BEFORE the period '.', but as ABAP does not consider this a syntax error, 
		// it must be ensured that ABAP cleaner accepts this input; the parser will actually move each ##pragma to the next line
		
		buildSrc("  METHOD main. ##pragma \" comment");
		buildSrc("    . ##pragma \" comment");
		buildSrc("    method_call( ). ##pragma \" comment");
		buildSrc("    . ##pragma \" comment");
		buildSrc("  ENDMETHOD. ##pragma \" comment");
		
		testParseCode();
	}

	@Test
	void testSelectWithDataSourceTilde() {
		// ensure that in SELECT statements, the tilde ~ after the data source is parsed without error 

		buildSrc("    SELECT FROM I_View AS c");
		buildSrc("           FIELDS c~carrid,");
		buildSrc("    	             c~connid,"); 
		buildSrc("           WHERE c~currcode = 'USD'");
		buildSrc("           INTO TABLE @gt_data.");
		
		testParseCode();
	}

	@Test
	void testSelectWithAssociationBackslash() {
		// ensure that in SELECT statements, the prefix \ for associations and the tilde ~ after the data source
		// are parsed without error
		
		buildSrc("    SELECT FROM I_View AS c");
		buildSrc("           FIELDS carrid,");
		buildSrc("    	             connid,"); 
		buildSrc("                  \\_Carrier-carrname,");
		buildSrc("                  c~\\_Carrier-currcode,");
		buildSrc("           WHERE c~\\_Carrier-currcode = 'USD'");
		buildSrc("           INTO TABLE @gt_data.");
		
		testParseCode();
	}

	@Test
	void testAtSelectionScreenWithComment() {
		// ensure that "AT SELECTION-SCREEN" is correctly identified as a levelOpener, despite the comments between
		// the keywords (and despite the fact that "AT" is also a levelOpener, which would require an "ENDAT"!)
		// if this fails, a parse error 'expected ENDAT, but found AT SELECTION-SCREEN.' would be raised
		
		buildSrc("REPORT ztest.");
		buildSrc("");
		buildSrc("AT \" comment");
		buildSrc("* comment");
		buildSrc("SELECTION-SCREEN.");
		buildSrc("  \" do something");
		buildSrc("AT SELECTION-SCREEN.");
		buildSrc("  \" do something else");
		
		testParseCode();
	}

	@Test
	void testSelectWithComparisonOps() {
		buildSrc("    SELECT a~field");
		buildSrc("      FROM @it_table AS a");
		buildSrc("      INNER JOIN dtab AS b");
		buildSrc("        ON a~field = b~field");
		buildSrc("      WHERE a~date >= b~date");
		buildSrc("      INTO TABLE @rt_result.");
		
		testParseCode();
	}

	@Test
	void testChainColonInsideParenthesesExc() {
		// this code is syntactically correct, but ABAP cleaner cannot process it correctly, because "(" is opened once, 
		// but closed with ")" three times; we therefore expect ABAP cleaner to raise a ParseException (from Token.addNext)
		// that asks the user to refactor the chain colon first
		
		buildSrc("    process_task( iv_task = : lv_task_1 ),");
		buildSrc("                              lv_task_2 ),");
		buildSrc("                              lv_task_3 ).");
		
		putAnyMethodAroundSrc();
		
		testParseCodeExpectingException("chain");
	}

	@Test
	void testChainColonInsideBracketsExc() {
		// this code is syntactically correct, but ABAP cleaner cannot process it correctly, because "[" is opened once, 
		// but closed with "]" three times; we therefore expect ABAP cleaner to raise a ParseException (from Token.addNext)
		// that asks the user to refactor the chain colon first

		buildSrc("    ev_value += lt_value[ country = 'DE' : city = 'Walldorf' ],");
		buildSrc("                                           city = 'Berlin' ],");
		buildSrc("                                           city = 'Munich' ].");
		
		putAnyMethodAroundSrc();
		
		testParseCodeExpectingException("chain");
	}

	@Test
	void testChainOfIfExc() {
		// this code is syntactically correct, because the compiler will create two nested IF commands; however, ABAP cleaner 
		// cannot process this correctly; we therefore expect ABAP cleaner to raise a ParseException (from Command.finishBuild)
		// that asks the user to refactor the chain first
		
		buildSrc("    IF a > : 1, 2.");
		buildSrc("        \" do something");
		buildSrc("      ENDIF.");
		buildSrc("    ENDIF.");
		
		putAnyMethodAroundSrc();
		
		testParseCodeExpectingException("chain");
	}

	@Test
	void testChainOfLoopsExc() {
		// this code is syntactically correct, because the compiler will create two nested LOOP AT commands and two nested
		// ENDLOOP commands; however, ABAP cleaner cannot process this correctly; we therefore expect ABAP cleaner to raise a
		// ParseException (from Command.finishBuild) that asks the user to refactor the chain first
		
		buildSrc("    LOOP AT it_tab ASSIGNING : FIELD-SYMBOL(<ls_outer>), FIELD-SYMBOL(<ls_inner>).");
		buildSrc("    ENDLOOP:,.");
		
		putAnyMethodAroundSrc();
		
		testParseCodeExpectingException("chain");
	}

	@Test
	void testChainOfEndIfExc() {
		// this code is syntactically correct, because the compiler will create two nested ENDIF commands; however, ABAP cleaner 
		// cannot process this correctly; we therefore expect ABAP cleaner to raise a ParseException (from Command.finishBuild)
		// that asks the user to refactor the chain first
		
		buildSrc("    IF a = 1.");
		buildSrc("      IF b = 2.");
		buildSrc("    ENDIF:,.");
		
		putAnyMethodAroundSrc();
		
		testParseCodeExpectingException("chain");
	}

	@Test
	void testStringTemplateWithDoubleEscape() {
		// ensure that a string template which contains two consecutive || is parsed correctly
		
		buildSrc("    DATA(lv_string_template)     =  |a\\|\\|b|.");
		buildSrc("    DATA(lv_text_string_literal) =  `a````b`.");
		buildSrc("    DATA(lv_text_field_literal)  =  'a''''b'.");
		
		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testEscapeCharWithValueOrReference() {
		// ensure that matching between RND Parser tokens and ABAP cleaner tokens works without error for 
		// !VALUE(...) and !REFERENCE(...) - which is syntactically correct
		
		buildSrc("METHODS any_method");
		buildSrc("      IMPORTING !VALUE(ev_any_value)   TYPE char1");
		buildSrc("                !REFERENCE(ev_any_ref) TYPE char1.");
		
		putAnyClassDefAroundSrc();
		
		testParseCode();
	}

	@Test
	void testEscapeCharWithVariousIdentifiers() {
		// ensure that matching between RND Parser tokens and ABAP cleaner tokens works without error  
		// if the "!" escape char is used for structures, components, parameters, and method names 
		
		buildSrc("  !lv_value = !ls_struc-comp + !lt_table[ !comp = !lv_value ]-num + !get_value( !iv_param = !lv_any ).");
		
		putAnyClassDefAroundSrc();
		
		testParseCode();
	}

	@Test
	void testIntLiteralWithPlus() {
		buildSrc("    ev_value = +1.");
		
		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testToCleanupResult() {
		buildSrc("METHOD any_method.");
		buildSrc("  IF a = 1.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDMETHOD.");
		
		String codeText = sourceCodeBuilder.toString();

		try {
			Code code = Code.parse(null, ParseParams.createForWholeCode("test", codeText, ABAP.NEWEST_RELEASE));
			CleanupResult cleanupResult = code.toCleanupResult();
			assertTrue(cleanupResult.hasCleanedCode());
			assertEquals(codeText, cleanupResult.getCleanedCode());
			assertFalse(cleanupResult.hasLineSelection());
			assertFalse(cleanupResult.hasErrorMessage());
			assertEquals("", cleanupResult.getSelectedText());
			
			code = Code.parse(null, ParseParams.createForCleanupRange("test", codeText, ABAP.NEWEST_RELEASE, CleanupRange.create(1, 4, false), CleanupRangeExpandMode.FULL_METHOD));
			cleanupResult = code.toCleanupResult();
			assertTrue(cleanupResult.hasCleanedCode());
			assertEquals(codeText, cleanupResult.getCleanedCode());
			assertTrue(cleanupResult.hasLineSelection());
			assertFalse(cleanupResult.hasErrorMessage());
			assertEquals("  IF a = 1." + LINE_SEP + "    RETURN." + LINE_SEP + "  ENDIF.", cleanupResult.getSelectedText());
			
		} catch (ParseException e) {
			fail();
		}
	}
	
	private Code createCode(String codeText) {
		try {
			return Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail();
			return null;
		}
	}

	@Test
	void testReferentialIntegrityEmpty() {
		try {
			createCode("").testReferentialIntegrity(true);
		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	void expReferentialIntegrityBroken(Code code) {
		try {
			code.testReferentialIntegrity(true);
			fail();
		} catch (IntegrityBrokenException e) {
			// expected case
		}
	}

	@Test
	void testReferentialIntegrityErr() {
		String codeText = "DO 5 TIMES. a += 1. ENDDO.";

		Code code = createCode("");
		Code otherCode = createCode(codeText);
		code.firstCommand = otherCode.firstCommand;
		expReferentialIntegrityBroken(code);

		code = createCode("");
		otherCode = createCode(codeText);
		code.lastCommand = otherCode.firstCommand;
		expReferentialIntegrityBroken(code);

		code = createCode("");
		code.commandCount = 1;
		expReferentialIntegrityBroken(code);

		code = createCode(codeText);
		code.firstCommand.setPrev(code.lastCommand);
		expReferentialIntegrityBroken(code);
		
		code = createCode(codeText);
		code.firstCommand.setPrevSibling(code.lastCommand);
		expReferentialIntegrityBroken(code);
		
		code = createCode(codeText);
		code.lastCommand.setNext(code.firstCommand);
		expReferentialIntegrityBroken(code);

		code = createCode(codeText);
		code.lastCommand.setNextSibling(code.firstCommand);
		expReferentialIntegrityBroken(code);

		code = createCode(codeText);
		otherCode = createCode(codeText);
		code.firstCommand = otherCode.firstCommand;
		code.lastCommand = otherCode.lastCommand;
		expReferentialIntegrityBroken(code);
	}
	
	@Test
	void testReferentialIntegrityEmptyProgress() {
		String codeText = "DO 5 TIMES. a += 1. ENDDO.";
		
		try {
			ProgressDouble progressDouble = new ProgressDouble(1, TaskType.INTEGRITY_TEST);
			createCode(codeText).testReferentialIntegrity(true, progressDouble);
			assertEquals(1, progressDouble.getCallCountIsCancellationPending());
			assertTrue(progressDouble.wasReportCalled());

			progressDouble = new ProgressDouble(2, TaskType.INTEGRITY_TEST);
			createCode(codeText).testReferentialIntegrity(true, progressDouble);
			assertEquals(2, progressDouble.getCallCountIsCancellationPending());
			assertTrue(progressDouble.wasReportCalled());

			progressDouble = new ProgressDouble(-1, TaskType.INTEGRITY_TEST);
			createCode(codeText).testReferentialIntegrity(true, progressDouble);
			assertEquals(3, progressDouble.getCallCountIsCancellationPending());
			assertTrue(progressDouble.wasReportCalled());

		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	@Test
	void testFindFirstCommandInCleanupRangeEmpty() {
		assertEquals(null, createCode("").findFirstCommandInCleanupRange());
		assertEquals(null, createCode("").findLastCommandInCleanupRange());
	}
	
	@Test
	void testAddRuleUseNull() {
		Code code = createCode("DO 5 TIMES. a += 1. ENDDO.");
		try {
			code.addRuleUse(null, code.firstCommand);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		Profile profile = Profile.createDefault();
		try {
			code.addRuleUse(profile.getRule(RuleID.CALL_METHOD), null);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		// expect addRuleUses to work without NullPointerException (although rule == null) if the 
		// Command array is empty
		try {
			Command[] commands = null;
			code.addRuleUses(null, commands);

			commands = new Command[0];
			code.addRuleUses(null, commands);

			ArrayList<Command> commandList = null;
			code.addRuleUses(null, commandList);
		} catch (NullPointerException ex) {
			fail();
		}
	}
	
	@Test
	void testAddRuleUsesNull() {
		Code code = createCode("DO 5 TIMES. a += 1. ENDDO.");

		Profile profile = Profile.createDefault();
		try {
			code.addRuleUse(profile.getRule(RuleID.CALL_METHOD), null);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}
	}
	
	@Test
	void testCompareWithSource() {
		String code1 = "DO 5 TIMES." + SEP + "  a += 1." + SEP + "  b += 1." + SEP + "ENDDO." + SEP + "";
		String code2 = "DO 5 TIMES." + SEP + "  a -= 1." + SEP + "  c += 1." + SEP + "ENDDO.";
		String code3 = "DO 6 TIMES." + SEP + "  a += 1." + SEP + "  d -= 1." + SEP + "ENDDO." + SEP + "b = 2." + SEP + "";
		String code4 = "DO 5 TIMES." + SEP + "  a += 1." + SEP + "  b += 1." + SEP + "ENDDO." + SEP + "b = 2.";
		String code5 = "" + SEP + "";
		String code1WithTabs = "DO\t5 TIMES." + SEP + "  a +=\t1." + SEP + "\t\tb += 1." + SEP + "ENDDO.";
		
		Code code = createCode(code1);

		try {
			code.compareWithSource(null, 10);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}
		
		// expect the resultMessage to be null, as no difference should be found
		String resultMessage = code.compareWithSource(code1, 10);
		assertEquals(null, resultMessage);
		
		// expect the resultMessage to be null, as the tabs should NOT count as differences 
		resultMessage = code.compareWithSource(code1WithTabs, 10);
		assertEquals(null, resultMessage);
		
		// expect lines 2, 3 to be reported as different
		resultMessage = code.compareWithSource(code2, 10);
		assertTrue(resultMessage.indexOf("line 2, 3") >= 0);
		assertFalse(resultMessage.indexOf("line count mismatch") >= 0);
		
		// again (with maxReportLineCount < 0), expect lines 2, 3 to be reported as different
		resultMessage = code.compareWithSource(code2, -1);
		assertTrue(resultMessage.indexOf("line 2, 3") >= 0);
		assertFalse(resultMessage.indexOf("line count mismatch") >= 0);
		
		// with maxReportLineCount = 1, expect only line 2 to be reported as different, not "2, 3"
		resultMessage = code.compareWithSource(code2, 1);
		assertTrue(resultMessage.indexOf("line 2") >= 0);
		assertFalse(resultMessage.indexOf("line 2,") >= 0);
		assertFalse(resultMessage.indexOf("line count mismatch") >= 0);

		// expect lines 1, 3 and the line count to be reported as different
		resultMessage = code.compareWithSource(code3, 10);
		assertTrue(resultMessage.indexOf("line 1, 3") >= 0);
		assertTrue(resultMessage.indexOf("line count mismatch") >= 0);

		// expect only the line count to be reported as different
		resultMessage = code.compareWithSource(code4, 10);
		assertTrue(resultMessage.indexOf("line count mismatch") >= 0);

		// expect only the line count to be reported as different
		resultMessage = code.compareWithSource(code5, -1);
		assertTrue(resultMessage.indexOf("line count mismatch") >= 0);
	}
	
	@Test
	void testGetTotalTokenCount() {
		Code code = createCode("");
		assertEquals(0, code.getTotalTokenCount());
		
		code = createCode("DO 5 TIMES. a += 1. ENDDO.");
		assertEquals(10, code.getTotalTokenCount());
	}
	
	@Test
	void testExpendCleanupRangeEmpty() {
		buildSrc("METHOD any_method.");
		buildSrc("  IF a = 1.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDMETHOD.");
		
		String codeText = sourceCodeBuilder.toString();

		try {
			// expand cleanup range to full document; expect .cleanupRange to be null
			Code code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			code.cleanupRange = CleanupRange.create(1, 3, true);
			code.expandCleanupRange(CleanupRangeExpandMode.FULL_DOCUMENT);
			assertEquals(null, code.cleanupRange);

			// provide cleanup range with a start command that is out of range
			code.cleanupRange = CleanupRange.create(10, 20, true);
			code.expandCleanupRange(CleanupRangeExpandMode.FULL_METHOD);
			assertFalse(code.cleanupRange.expandRange);

		} catch (ParseException e) {
			fail();
		}
	}
	
	@Test
	void testHasClassStartWithClass() {
		buildSrc("METHOD any_method.");
		buildSrc("  IF a = 1.");
		buildSrc("    RETURN.");
		buildSrc("  ENDIF.");
		buildSrc("ENDMETHOD.");
		
		putAnyClassDefAroundSrc();
		
		Code code = createCode(sourceCodeBuilder.toString());
		assertTrue(code.hasClassStart());
		assertTrue(code.hasMethodFunctionOrFormStart());
		assertTrue(code.hasMethodFunctionFormOrEventBlockStart());
	}
	
	@Test
	void testHasClassStartWithReport() {
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("INITIALIZATION.");
		buildSrc("  \" comment");
		
		Code code = createCode(sourceCodeBuilder.toString());
		assertFalse(code.hasClassStart());
		assertFalse(code.hasMethodFunctionOrFormStart());
		assertTrue(code.hasMethodFunctionFormOrEventBlockStart());
	}
	
	@Test
	void testHasClassStartWithReportAndForm() {
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("FORM any_form.");
		buildSrc("  \" comment");
		buildSrc("ENDFORM.");
		
		Code code = createCode(sourceCodeBuilder.toString());
		assertFalse(code.hasClassStart());
		assertTrue(code.hasMethodFunctionOrFormStart());
		assertTrue(code.hasMethodFunctionFormOrEventBlockStart());
	}

	@Test
	void testPositionAndLengthWithLeadingZeros() {
		buildSrc("    WRITE AT 1(5) 'abc'.");
		buildSrc("    WRITE AT 01(5) 'abc'.");
		buildSrc("    WRITE AT 1(05) 'abc'.");
		buildSrc("    WRITE AT 001(005) 'abc'.");
		
		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testUlineAt() {
		// ensure that the parser accepts all the following ways of using ULINE [AT] (as does ABAP syntax) 
		
		buildSrc("  DATA pos TYPE i.");
		buildSrc("  DATA len TYPE i.");
		buildSrc("");
		buildSrc("  ULINE.");
		buildSrc("");
		buildSrc("  ULINE AT /.");
		buildSrc("  ULINE AT /(20).");
		buildSrc("  ULINE AT /(len).");
		buildSrc("  ULINE AT /(*).");
		buildSrc("  ULINE AT /(**).");
		buildSrc("");
		buildSrc("  ULINE AT /10.");
		buildSrc("  ULINE AT /10(20).");
		buildSrc("  ULINE AT /10(len).");
		buildSrc("  ULINE AT /10(*).");
		buildSrc("  ULINE AT /10(**).");
		buildSrc("");
		buildSrc("  ULINE AT /pos.");
		buildSrc("  ULINE AT /pos(20).");
		buildSrc("  ULINE AT /pos(len).");
		buildSrc("  ULINE AT /pos(*).");
		buildSrc("  ULINE AT /pos(**).");
		buildSrc("");
		buildSrc("  ULINE AT (20).");
		buildSrc("  ULINE AT (len).");
		buildSrc("  ULINE AT (*).");
		buildSrc("  ULINE AT (**).");
		buildSrc("");
		buildSrc("  ULINE AT 10.");
		buildSrc("  ULINE AT 10(20).");
		buildSrc("  ULINE AT 10(len).");
		buildSrc("  ULINE AT 10(*).");
		buildSrc("  ULINE AT 10(**).");
		buildSrc("");
		buildSrc("  ULINE AT pos.");
		buildSrc("  ULINE AT pos(20).");
		buildSrc("  ULINE AT pos(len).");
		buildSrc("  ULINE AT pos(*).");
		buildSrc("  ULINE AT pos(**).");
		buildSrc("");
		buildSrc("  ULINE /.");
		buildSrc("  ULINE /(20).");
		buildSrc("  ULINE /(len)."); // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /(*).");
		buildSrc("  ULINE /(**).");
		buildSrc("");
		buildSrc("  ULINE /10.");
		buildSrc("  ULINE /10(20).");
		buildSrc("  ULINE /10(len)."); // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /10(*).");
		buildSrc("  ULINE /10(**).");
		buildSrc("");
		buildSrc("  ULINE /pos."); 	  // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /pos(20).");  // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /pos(len)."); // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /pos(*).");   // warning about missing 'AT', but no syntax error
		buildSrc("  ULINE /pos(**).");  // warning about missing 'AT', but no syntax error
		buildSrc("");
		buildSrc("  ULINE (20).");
		buildSrc("  ULINE (*).");
		buildSrc("  ULINE (**).");
		buildSrc("");
		buildSrc("  ULINE 10.");
		buildSrc("  ULINE 10(20).");
		buildSrc("  ULINE 10(*).");
		buildSrc("  ULINE 10(**).");

		putAnyMethodAroundSrc();
		
		testParseCode();
	}
	
	@Test
	void testWriteAt() {
		// ensure that the parser accepts all the following ways of using WRITE [AT] (as does ABAP syntax) 
		
		buildSrc("  DATA pos TYPE i.");
		buildSrc("  DATA len TYPE i.");
		buildSrc("");
		buildSrc("  WRITE AT / 'a'.");
		buildSrc("  WRITE AT /(20) 'a'.");
		buildSrc("  WRITE AT /(len) 'a'.");
		buildSrc("  WRITE AT /(*) 'a'.");
		buildSrc("  WRITE AT /(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE AT /10 'a'.");
		buildSrc("  WRITE AT /10(20) 'a'.");
		buildSrc("  WRITE AT /10(len) 'a'.");
		buildSrc("  WRITE AT /10(*) 'a'.");
		buildSrc("  WRITE AT /10(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE AT /pos 'a'.");
		buildSrc("  WRITE AT /pos(20) 'a'.");
		buildSrc("  WRITE AT /pos(len) 'a'.");
		buildSrc("  WRITE AT /pos(*) 'a'.");
		buildSrc("  WRITE AT /pos(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE AT (20) 'a'.");
		buildSrc("  WRITE AT (len) 'a'.");
		buildSrc("  WRITE AT (*) 'a'.");
		buildSrc("  WRITE AT (**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE AT 10 'a'.");
		buildSrc("  WRITE AT 10(20) 'a'.");
		buildSrc("  WRITE AT 10(len) 'a'.");
		buildSrc("  WRITE AT 10(*) 'a'.");
		buildSrc("  WRITE AT 10(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE AT pos 'a'.");
		buildSrc("  WRITE AT pos(20) 'a'.");
		buildSrc("  WRITE AT pos(len) 'a'.");
		buildSrc("  WRITE AT pos(*) 'a'.");
		buildSrc("  WRITE AT pos(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE / 'a'.");
		buildSrc("  WRITE /(20) 'a'.");
		buildSrc("  WRITE /(len) 'a'."); // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /(*) 'a'.");
		buildSrc("  WRITE /(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE /10 'a'.");
		buildSrc("  WRITE /10(20) 'a'.");
		buildSrc("  WRITE /10(len) 'a'."); // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /10(*) 'a'.");
		buildSrc("  WRITE /10(**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE /pos 'a'.");      // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /pos(20) 'a'.");  // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /pos(len) 'a'."); // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /pos(*) 'a'.");   // warning about missing 'AT', but no syntax error
		buildSrc("  WRITE /pos(**) 'a'.");  // warning about missing 'AT', but no syntax error
		buildSrc("");
		buildSrc("  WRITE (20) 'a'.");
		buildSrc("  WRITE (*) 'a'.");
		buildSrc("  WRITE (**) 'a'.");
		buildSrc("");
		buildSrc("  WRITE 10 'a'.");
		buildSrc("  WRITE 10(20) 'a'.");
		buildSrc("  WRITE 10(*) 'a'.");
		buildSrc("  WRITE 10(**) 'a'.");

		putAnyMethodAroundSrc();
		
		testParseCode();
	}

	@Test
	void testDefineWithClassDefinition() {
		// ensure that CLASS can be nested in DEFINE: CLASS is an optional level closer in the context of a REPORT, 
		// but does NOT close a level in this case
		
		buildSrc("DEFINE any_macro.");
		buildSrc("  \" comment");
		buildSrc("  CLASS &1 DEFINITION FOR TESTING INHERITING FROM cx_static_check.");
		buildSrc("  ENDCLASS.");
		buildSrc("END-OF-DEFINITION.");
		buildSrc("");
		buildSrc("DEFINE other_macro.");
		buildSrc("  CLASS &1 DEFINITION FOR TESTING INHERITING FROM cx_static_check.");
		buildSrc("  ENDCLASS.");
		buildSrc("");
		buildSrc("  CLASS &1 IMPLEMENTATION.");
		buildSrc("  ENDCLASS.");
		buildSrc("END-OF-DEFINITION.");
		
		testParseCode();
	}
}
