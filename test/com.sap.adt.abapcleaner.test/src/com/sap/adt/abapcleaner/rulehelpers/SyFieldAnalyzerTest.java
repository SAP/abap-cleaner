package com.sap.adt.abapcleaner.rulehelpers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.programbase.ParseException;

public class SyFieldAnalyzerTest {
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;
	private StringBuilder sb = new StringBuilder();
	
	void buildSrc(String line) {
		buildCode(line);
	}

	private void buildCode(String line) {
		if (sb.length() > 0)
			sb.append(LINE_SEP);
		sb.append(StringUtil.trimEnd(line));
	}
	
	private void putAnyMethodAroundSrc() {
		sb.insert(0, "  METHOD any_test_method." + LINE_SEP);
		sb.append(LINE_SEP + "  ENDMETHOD.");
	}
	
	private Command findCommand(Code code, String textPart) {
		if (textPart == null)
			return null;
		Command command = code.firstCommand;
		while (command != null) {
			if (commandMatches(command, textPart))
				return command;
			command = command.getNext();
		}
		fail("Command '" + textPart + "' not found!");
		return null;
	}
	
	private boolean commandMatches(Command command, String text) {
		return (command.toString().indexOf(text) >= 0);
	}
	
	private void testSubrc(String startCommandText, String... matchingCommandTexts) {
		String sourceCode = sb.toString();

		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}
		
		Command startCommand = findCommand(code, startCommandText);
		ArrayList<Command> matchingCommands = SyFieldAnalyzer.getSyFieldReadersFor(ABAP.SyField.SUBRC, startCommand);

		assertEquals(matchingCommandTexts.length, matchingCommands.size());
		int matchIndex = 0;
		for (String firstTokenOfMatch : matchingCommandTexts) {
			assertTrue(commandMatches(matchingCommands.get(matchIndex), firstTokenOfMatch));
			++matchIndex;
		}
	}
	
	// =========================================================================
	// tests for SY-SUBRC
	
	@Test
	void testDoWithoutContinueOrExit() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - 'b = sy-subrc.' is found, because it can be reached 
		// - 'a = sy-subrc.' and 'c = sy-subrc' are NOT found, because 'CALL METHOD other_method' overwrites sy-subrc  
		buildSrc("    DO lv_count TIMES.");
		buildSrc("      a = sy-subrc.");
		buildSrc("      CALL METHOD any_method.");
		buildSrc("      IF iv_param < 5.");
		buildSrc("        RETURN.");
		buildSrc("      ENDIF.");
		buildSrc("      b = sy-subrc.");
		buildSrc("      CALL METHOD other_method.");
		buildSrc("    ENDDO.");
		buildSrc("    c = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "b = sy-subrc.");
	}
	
	@Test
	void testDoWithContinue() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// both 'lv_any = sy-subrc.' AND 'lv_other = sy-subrc.' are found  
		buildSrc("    DO lv_count TIMES.");
		buildSrc("      lv_any = sy-subrc.");
		buildSrc("      CALL METHOD any_method.");
		buildSrc("      IF iv_param < 5.");
		buildSrc("        CONTINUE.");
		buildSrc("      ENDIF.");
		buildSrc("      CALL METHOD other_method.");
		buildSrc("    ENDDO.");
		buildSrc("    lv_other = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "lv_any = sy-subrc.", "lv_other = sy-subrc.");
	}
	
	@Test
	void testDoWithExit() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// only 'lv_other = sy-subrc.' is found
		buildSrc("    DO lv_count TIMES.");
		buildSrc("      lv_any = sy-subrc.");
		buildSrc("      CALL METHOD any_method.");
		buildSrc("      IF iv_param < 5.");
		buildSrc("        \" do something");
		buildSrc("      ELSE.");
		buildSrc("        EXIT.");
		buildSrc("      ENDIF.");
		buildSrc("      CALL METHOD other_method.");
		buildSrc("    ENDDO.");
		buildSrc("    lv_other = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "lv_other = sy-subrc.");
	}

	@Test
	void testUnreachableCodeAfterIfNotConsidered() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// 'a = sy-subrc.' is found, because 'CLEAR sy-subrc.' is unreachable
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    DO.");
		buildSrc("      IF sy-index = 1.");
		buildSrc("        CONTINUE.");
		buildSrc("      ELSE.");
		buildSrc("        EXIT.");
		buildSrc("      ENDIF.");
		buildSrc("      CLEAR sy-subrc.");
		buildSrc("    ENDDO.");
		buildSrc("    a = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "a = sy-subrc.");
	}

	@Test
	void testCodeAfterCheckInLoopConsidered() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// both 'a = sy-subrc.' and 'b = sy-subrc' are found, because 'CHECK' may be passed
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    DO.");
		buildSrc("      CHECK c = d.");
		buildSrc("      a = sy-subrc.");
		buildSrc("      EXIT.");
		buildSrc("    ENDDO.");
		buildSrc("    b = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "a = sy-subrc.", "b = sy-subrc.");
	}

	@Test
	void testUnreachableCodeAfterDoNotConsidered() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// neither 'a = sy-subrc.' nor 'b = sy-subrc' are found, because they are never reached
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    DO.");
		buildSrc("      IF sy-index = 1.");
		buildSrc("        RETURN.");
		buildSrc("      ELSE.");
		buildSrc("        RAISE NEW cx_any_exception( ).");
		buildSrc("      ENDIF.");
		buildSrc("      a = sy-subrc.");
		buildSrc("    ENDDO.");
		buildSrc("    b = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.");
	}

	@Test
	void testDoSkipConsidered() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// 'a = sy-subrc.' is found, because the DO could be skipped if iv_count = 0
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    DO iv_count TIMES.");
		buildSrc("      RETURN.");
		buildSrc("    ENDDO.");
		buildSrc("    a = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "a = sy-subrc.");
	}

	@Test
	void testUnreachableCodeAfterExit() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// 'a = sy-subrc.' is NOT found, because it is unreachable 
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    EXIT.");
		buildSrc("    a = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.");
	}

	@Test
	void testIfWithFunctionalCall() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - the first three IF / ELSEIF are found, because they evaluate sy-subrc
		// - the lines 'a = sy-subrc.', 'b = sy-subrc.' and 'c = sy-subrc' are NOT found, because get_value( ) overwrites sy-subrc
		// - 'd = sy-subrc.' is found, because it can be reached via the empty branch
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    IF sy-subrc = 0.");
		buildSrc("      do_something( ).");
		buildSrc("    ELSEIF sy-subrc = 4.");
		buildSrc("    ELSEIF get_value( sy-subrc ) = abap_true.");
		buildSrc("      a = sy-subrc.");
		buildSrc("    ELSEIF iv_value = abap_bool.");
		buildSrc("      b = sy-subrc.");
		buildSrc("    ELSE.");
		buildSrc("      c = sy-subrc.");
		buildSrc("    ENDIF.");
		buildSrc("    d = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "IF sy-subrc = 0.", "ELSEIF sy-subrc = 4.", "ELSEIF get_value(", "d = sy-subrc.");
	}

	@Test
	void testAllIfOverwritingSySubrc() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - the first two IF / ELSEIF are found, because they evaluate sy-subrc
		// - the lines 'a = sy-subrc.' and 'b = sy-subrc' are NOT found, because get_value( ) overwrites sy-subrc
		// - 'c = sy-subrc.' is NOT found, because sy-subrc is overwritten in each branch
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    IF sy-subrc = 0.");
		buildSrc("      do_something( ).");
		buildSrc("    ELSEIF sy-subrc = 4.");
		buildSrc("      CLEAR sy-subrc.");
		buildSrc("    ELSEIF get_value( ) = abap_true.");
		buildSrc("      a = sy-subrc.");
		buildSrc("    ELSE.");
		buildSrc("      b = sy-subrc.");
		buildSrc("    ENDIF.");
		buildSrc("    c = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "IF sy-subrc = 0.", "ELSEIF sy-subrc = 4.");
	}

	@Test
	void testFunctionalCallInIfClause() {
		// analyzing sy-subrc reads after 'get_value( )', ensure that all sy-subrc reads are found 
		buildSrc("    IF get_value( ) = abap_true.");
		buildSrc("      a = sy-subrc.");
		buildSrc("    ELSEIF sy-subrc = 0.");
		buildSrc("      b = sy-subrc.");
		buildSrc("    ELSE.");
		buildSrc("      c = sy-subrc.");
		buildSrc("    ENDIF.");

		putAnyMethodAroundSrc();
	
		testSubrc("IF get_value( ) = abap_true.", "a = sy-subrc.", "ELSEIF sy-subrc = 0.", "b = sy-subrc.", "c = sy-subrc.");
	}

	@Test
	void testCaseWhenWithoutWhenOthers() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - both 'CASE sy-subrc' and 'a = sy-subrc' are found, because they evaluate sy-subrc
		// - 'b = sy-subrc.' is NOT found, because CLEAR sy-subrc overwrites sy-subrc
		// - 'c = sy-subrc.' is NOT found, because get_value( ) overwrites sy-subrc
		// - 'd = sy-subrc.' is found, because WHEN OTHERS is missing
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    CASE sy-subrc.");
		buildSrc("      WHEN 1.");
		buildSrc("        a = sy-subrc.");
		buildSrc("        CLEAR sy-subrc.");
		buildSrc("        b = sy-subrc.");
		buildSrc("      WHEN 2.");
		buildSrc("        get_value( ).");
		buildSrc("        c = sy-subrc.");
		buildSrc("    ENDCASE.");
		buildSrc("    d = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "CASE sy-subrc.", "a = sy-subrc.", "d = sy-subrc.");
	}

	@Test
	void testCaseWhenWithWhenOthers() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - 'CASE sy-subrc' is found
		// - 'a = sy-subrc.' is NOT found, because all WHEN branches overwrite sy-subrc or terminate the method
		buildSrc("    CALL METHOD any_method.");
		buildSrc("    CASE sy-subrc.");
		buildSrc("      WHEN 1.");
		buildSrc("        CLEAR sy-subrc.");
		buildSrc("      WHEN 2.");
		buildSrc("        get_value( ).");
		buildSrc("      WHEN OTHERS.");
		buildSrc("        RAISE any_classic_exception.");
		buildSrc("    ENDCASE.");
		buildSrc("    a = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "CASE sy-subrc.");
	}

	@Test
	void testWhileWithFunctionalCall() {
		// analyzing sy-subrc reads after 'CALL METHOD any_method', ensure that  
		// - 'a = sy-subrc' is NOT found, because get_next_value( ) changes sy-subrc
		// - 'b = sy-subrc.' and c = sy-subrc are found
		buildSrc("    WHILE get_next_value( ) < 5.");
		buildSrc("      a = sy-subrc.");
		buildSrc("      CALL METHOD any_method.");
		buildSrc("      b = sy-subrc.");
		buildSrc("    ENDWHILE.");
		buildSrc("    c = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("CALL METHOD any_method.", "b = sy-subrc.", "c = sy-subrc.");
	}

	@Test
	void testNonExecutable() {
		// ensure that calling the ProgramFlowAnalyzer for non-executable commands does not dump
		
		buildSrc("INTERFACE any_interface PUBLIC.");
		buildSrc("ENDINTERFACE.");
		buildSrc("CLASS any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    DATA mv_any TYPE i.");
		buildSrc("ENDCLASS.");
		buildSrc("CLASS any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		testSubrc(null);
		testSubrc("INTERFACE any_interface PUBLIC.");
		testSubrc("ENDINTERFACE.");
		testSubrc("CLASS any_class DEFINITION.");
		testSubrc("  PUBLIC SECTION.");
		testSubrc("    DATA mv_any TYPE i.");
		testSubrc("ENDCLASS.");
		testSubrc("CLASS any_class IMPLEMENTATION.");
		testSubrc("  METHOD any_method.");
		testSubrc("  ENDMETHOD.");
		testSubrc("ENDCLASS.");
	}
	
	@Test
	void testCallInIfInLoop() {
		// analyzing sy-subrc reads after 'any_method( ls_line ).', ensure that  
		// - 'a = sy-subrc.', 'b = sy-subrc' and 'c = sy-subrc' are found, because they can be reached in the next LOOP cycle
		// - 'd = sy-subrc.' is NOT found, because ENDLOOP sets sy-subrc (including when the LOOP is skipped!)
		buildSrc("    LOOP AT its_table INTO DATA(ls_line).");
		buildSrc("      CASE sy-tabix.");
		buildSrc("        WHEN 1 OR 2.");
		buildSrc("          IF ls_line-comp > 1.");
		buildSrc("            any_method( ls_line ).");
		buildSrc("          ELSEIF ls_line-comp < 10.");
		buildSrc("            a = sy-subrc.");
		buildSrc("          ELSE.");
		buildSrc("            b = sy-subrc.");
		buildSrc("          ENDIF.");
		buildSrc("        WHEN OTHERS.");
		buildSrc("          c = sy-subrc.");
		buildSrc("      ENDCASE.");
		buildSrc("    ENDLOOP.");
		buildSrc("    d = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("any_method( ls_line ).", "a = sy-subrc", "b = sy-subrc.", "c = sy-subrc.");
	}
	
	@Test
	void testCallInInnermostLoop() {
		// analyzing sy-subrc reads after 'any_method( ).', ensure that  
		// - 'a..f = sy-subrc.' are found, because they can be reached after the functional call
		// - 'g = sy-subrc.' is NOT found, ENDLOOP changes sy-subrc
		buildSrc("    LOOP AT its_table INTO DATA(ls_line).");
		buildSrc("      a = sy-subrc.");
		buildSrc("      DO 5 TIMES.");
		buildSrc("        b = sy-subrc.");
		buildSrc("        WHILE b <> c.");
		buildSrc("          c = sy-subrc.");
		buildSrc("          any_method( ).");
		buildSrc("          d = sy-subrc.");
		buildSrc("        ENDWHILE.");
		buildSrc("        e = sy-subrc.");
		buildSrc("      ENDDO.");
		buildSrc("      f = sy-subrc.");
		buildSrc("    ENDLOOP.");
		buildSrc("    g = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("any_method( ).", "d = sy-subrc.", "c = sy-subrc.", "e = sy-subrc.", "b = sy-subrc.", "f = sy-subrc.", "a = sy-subrc.");
	}

	@Test
	void testReportWithEventBlocks() {
		// ensure that event block ends are properly determined
		buildSrc("REPORT any_report");
		buildSrc("");
		buildSrc("INITIALIZATION.");
		buildSrc("  CALL METHOD any_method.");
		buildSrc("  a = sy-subrc.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  CALL METHOD other_method.");
		buildSrc("  b = sy-subrc.");

		testSubrc("CALL METHOD any_method.", "a = sy-subrc");
		testSubrc("CALL METHOD other_method.", "b = sy-subrc");
	}

	@Test
	void testArithmeticFunctionsNotChangingSySubrc() {
		// ensure that 'g = sy-subrc.' is found, because arithmetic function do NOT set sy-subrc
		buildSrc("    any_method( ).");
		buildSrc("    a = sign( b ) * cos( c ) * nmin( val1 = d  val2 = e ) * log( exp( sqrt( round( f ) ) ) ).");
		buildSrc("    g = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("any_method( ).", "g = sy-subrc.");
	}

	@Test
	void testSelfEvaluationInDo() {
		// analyzing sy-subrc reads after 'any_method( sy-subrc ).', ensure that  
		// 'any_method( sy-subrc ).' itself is found, because it evaluates its own result from the previous loop cycle
		buildSrc("    DO 5 TIMES.");
		buildSrc("      any_method( sy-subrc ).");
		buildSrc("    ENDDO.");

		putAnyMethodAroundSrc();
	
		testSubrc("any_method( sy-subrc ).", "any_method( sy-subrc ).");
	}

	@Test
	void testReturnValueConsidered() {
		// ensure that 'RETURN sy-subrc.' is found
		buildSrc("    any_method( ).");
		buildSrc("    IF sy-subrc <> 0.");
		buildSrc("      RETURN sy-subrc.");
		buildSrc("    ENDIF.");
		buildSrc("    a = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("any_method( ).", "IF sy-subrc <> 0.", "RETURN sy-subrc.", "a = sy-subrc.");
	}

	@Test
	void testSySubrcReadAfterPerform() {
		// ensure that it is assumed that PERFORM changes SY-SUBRC 
		// if(!) PERFORM is directly followed by a command that evaluates SY-SUBRC  
		buildSrc("    lo_instance->any_method( ).");
		buildSrc("    a = sy-subrc.");
		buildSrc("    PERFORM any_form.");
		buildSrc("    b = sy-subrc.");

		putAnyMethodAroundSrc();
	
		testSubrc("lo_instance->any_method( ).", "a = sy-subrc.");
	}

}
