package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.base.ABAP.SyField;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class CommandTest {
	private static final String SEP = ABAP.LINE_SEPARATOR;
	private Command[] commands;

	private void assertStringArrayEquals(String[] exp, String[] act) {
		assertEquals(exp.length, act.length);
		for (int i = 0; i < exp.length; ++i)
			assertEquals(exp[i], act[i]);
	}

	private Command buildCommand(String codeText) {
		try {
			Code code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			
			commands = new Command[code.commandCount];
			commands[0] = code.firstCommand;
			for (int i = 1; i < code.commandCount; ++i)
				commands[i] = commands[i - 1].getNext();
			
			return code.firstCommand;
		} catch (ParseException e) {
			fail();
			return null;
		}
	}

	private void buildCommandExpectingParseExc(String codeText) {
		try {
			Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			fail();
		} catch (ParseException e) {
		}
	}

	private Command buildSqlScriptCommand(String sqlScriptCode) {
		String codeText = "METHOD any_method BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT." 
								+ SEP + sqlScriptCode + SEP + "ENDMETHOD.";
		try {
			Code code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			return code.firstCommand.getNext();
		} catch (ParseException e) {
			fail();
			return null;
		}
	}

	private Section buildSection(String codeText) {
		try {
			Code code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			return Section.create(code.firstCommand, code.lastCommand);
		} catch (ParseException | UnexpectedSyntaxException e) {
			fail();
			return null;
		}
	}

	@Test
	void testIsAssignment() {
		assertFalse(buildCommand("CLEAR ev_param.").isAssignment(false, false));
		assertFalse(buildCommand(".").isAssignment(false, false));
		assertFalse(buildCommand("any_method( iv_param = 1 ).").isAssignment(false, false));
		assertFalse(buildCommand("DATA(a) = 1.").isAssignment(false, false));
		assertFalse(buildCommand("DATA(a) = get_value( ).").isAssignment(false, false));
	
		assertTrue(buildCommand("a = 1.").isAssignment(false, false));
		assertTrue(buildCommand("a += 1.").isAssignment(false, false));
		assertTrue(buildCommand("ls_struc-comp = 1.").isAssignment(false, false));
		assertTrue(buildCommand("ls_struc-comp *= 2.").isAssignment(false, false));
		assertTrue(buildCommand("<ls_any>-comp = 1.").isAssignment(false, false));
		assertTrue(buildCommand("<ls_any>-comp -= 1.").isAssignment(false, false));
		assertTrue(buildCommand("DATA(a) = 1.").isAssignment(true, false));
		assertTrue(buildCommand("DATA(a) = get_value( ).").isAssignment(true, false));
	}

	@Test
	void testIsAssignmentToTableExpr() {
		assertFalse(buildCommand("CLEAR ev_param.").isAssignment(false, true));
		assertFalse(buildCommand(".").isAssignment(false, true));
		assertFalse(buildCommand("any_method( iv_param = 1 ).").isAssignment(false, true));
		assertFalse(buildCommand("DATA(a) = 1.").isAssignment(false, true));
		assertFalse(buildCommand("DATA(a) = get_value( ).").isAssignment(false, true));
	
		assertTrue(buildCommand("lt_any[ 1 ] = ls_any.").isAssignment(false, true));
		assertTrue(buildCommand("lt_any[ comp = 1 ]-name = 'a'.").isAssignment(false, true));
		assertTrue(buildCommand("lt_any[ 1 ]-inner[ 2 ]-inner_inner[ 3 ]-comp = 1.").isAssignment(false, true));
	}

	@Test
	void testIsEmpty() {
		Command emptyCommand = buildCommand("  \" comment");
		emptyCommand.firstToken.text = "";
		assertTrue(emptyCommand.isEmpty());
	}

	@Test
	void testIsMethodFunctionOrFormStart() {
		assertTrue(buildCommand("METHOD any. ENDMETHOD.").isMethodFunctionOrFormStart());
		assertTrue(buildCommand("FUNCTION any. ENDFUNCTION.").isMethodFunctionOrFormStart());
		assertTrue(buildCommand("FORM any. ENDFORM.").isMethodFunctionOrFormStart());
	}

	@Test
	void testIsLateChain() {
		assertFalse(buildCommand("a += 1.").isLateChain());
		assertFalse(buildCommand("lv_text = `:`.").isLateChain());
		assertFalse(buildCommand("  \" comment with : colon in it").isLateChain());
		assertFalse(buildCommand("DATA: lv_any TYPE i.").isLateChain());
		assertFalse(buildCommand("WRITE: / a, b.").isLateChain());
		assertFalse(buildCommand("DATA: \" comment\r\n  lv_any TYPE i.").isLateChain());
		
		assertTrue(buildCommand("a += : 1, 2, 3.").isLateChain());
		assertTrue(buildCommand("a += \" comment\r\n  : 1, 2, 3.").isLateChain());
		assertTrue(buildCommand("CALL METHOD : any_method, other_method.").isLateChain());
	}
	
	@Test
	void testIsSqlScript() {
		assertFalse(buildCommand("* comment").isSqlScript());
		assertFalse(buildCommand("DATA lv_value TYPE i.").isSqlScript());

		assertTrue(buildSqlScriptCommand("* comment").isSqlScript());
		assertTrue(buildSqlScriptCommand("INSERT INTO dtab (field1, field2) VALUES ('value1', 'value2');").isSqlScript());
	}
	
	@Test
	void testAddNextWithWrongCloserForMethod() {
		Command command = buildCommand("METHOD any.");
		try {
			command.addNext(buildCommand("ENDFUNCTION."));
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expect an error message that mentions ENDMETHOD as the expected level closer and ENDFUNCTION as the actual level closer
			assertTrue(ex.getMessage().indexOf("ENDMETHOD") >= 0);
			assertTrue(ex.getMessage().indexOf("ENDFUNCTION") >= 0);
		}
	}
	
	@Test
	void testAddNextWithWrongCloserForIf() {
		Command command = buildCommand("IF a = 1.");
		try {
			command.addNext(buildCommand("ENDLOOP."));
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expect an error message that mentions ELSEIF, ELSE, and ENDIF as the expected level closer and ENDLOOP as the actual level closer
			assertTrue(ex.getMessage().indexOf("ELSEIF") >= 0);
			assertTrue(ex.getMessage().indexOf("ELSE") >= 0);
			assertTrue(ex.getMessage().indexOf("ENDIF") >= 0);
			assertTrue(ex.getMessage().indexOf("ENDLOOP") >= 0);
		}
	}
	
	@Test
	void testAddNextWithMissingOpener() {
		Command command = buildCommand("DATA a TYPE i.");
		try {
			command.addNext(buildCommand("ENDLOOP."));
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expect an error message that mentions LOOP as the expected level opener for ENDLOOP 
			assertTrue(ex.getMessage().indexOf("LOOP") >= 0);
			assertTrue(ex.getMessage().indexOf("ENDLOOP") >= 0);
		}
	}
	
	@Test
	void testAddNextWithMissingOpeners() {
		Command command = buildCommand("CLASS any_class DEFINITION DEFERRED.");
		try {
			command.addNext(buildCommand("PRIVATE SECTION."));
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expect an error message that mentions CLASS/PUBLIC/PROTECTED as the expected level opener for PRIVATE [SECTION]
			assertTrue(ex.getMessage().indexOf("CLASS") >= 0);
			assertTrue(ex.getMessage().indexOf("PUBLIC") >= 0);
			assertTrue(ex.getMessage().indexOf("PROTECTED") >= 0);
			assertTrue(ex.getMessage().indexOf("PRIVATE") >= 0);
		}
	}
	
	@Test
	void testAddNextNull() {
		Command command = buildCommand("a = 1.");
		try {
			command.addNext(null);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testAddNextWhenNextExists() {
		Command command = buildCommand("a = 1. b = 2.");
		Command newCommand = buildCommand("c = 3.");
		try {
			command.addNext(newCommand);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testAddNextWhenNewCommandHasPrev() {
		Command command = buildCommand("c = 3.");
		Command newCommand = buildCommand("a = 1. b = 2.").getNext();
		
		try {
			command.addNext(newCommand);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testInsertFirstChildNull() {
		Command command = buildCommand("IF a = 1.");
		
		try {
			command.insertFirstChild(null);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testInsertFirstChildWhenChildExists() {
		Command command = buildCommand("IF a = 1. RETURN. ENDIF.");
		Command newCommand = buildCommand("a = 2.");

		try {
			command.insertFirstChild(newCommand);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
		}
	}
	
	@Test
	void testInsertFirstChildThatHasPrevCommand() {
		Command command = buildCommand("IF a = 1. RETURN. ENDIF.");
		Command newCommand = buildCommand("a = 2. a = 3.").getNext();

		try {
			command.insertFirstChild(newCommand);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
		}
	}
	
	@Test
	void testInsertFirstChildThatClosesLevel() {
		Command command = buildCommand("IF a = 1. RETURN. ENDIF.");
		Command newCommand = buildCommand("ENDIF.");

		try {
			command.insertFirstChild(newCommand);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
		}
	}
	
	@Test
	void testInsertFirstChildWhenParentDoesNotOpenLevel() {
		Command command = buildCommand("a = 1.");
		Command newCommand = buildCommand("a = 2.");

		try {
			command.insertFirstChild(newCommand);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
		}
	}
	
	@Test
	void testInsertFirstChildThatHasChildren() {
		Command command = buildCommand("IF a = 1.");
		Command newCommand = buildCommand("IF a = 2. RETURN. ENDIF.");

		try {
			command.insertFirstChild(newCommand);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
		}
	}

	@Test
	void testInsertRightSiblingNull() {
		Command command = buildCommand("DATA a TYPE i.");
		Command newCommand = null;
		
		try {
			command.insertRightSibling(newCommand, false);
			fail();
		} catch (IntegrityBrokenException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testInsertRightSiblingToLastCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i.");
		Command newCommand = buildCommand("DATA b TYPE string."); 
		command.insertRightSibling(newCommand, false);
	}
	
	@Test
	void testInsertRightSiblingToInnerCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i. DATA c TYPE i.");
		Command newCommand = buildCommand("DATA b TYPE string.");
		command.insertRightSibling(newCommand, false);
	}
	
	@Test
	void testInsertRightSiblingToLastChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. a += 1. ENDIF.").getNext();
		Command newCommand = buildCommand("RETURN.");
		command.insertRightSibling(newCommand, false);
	}
	
	@Test
	void testInsertRightSiblingToInnerChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. a += 1. RETURN. ENDIF.").getNext();
		Command newCommand = buildCommand("b += 1.");
		command.insertRightSibling(newCommand, false);
	}
	
	@Test
	void testInsertRightSiblingThatHasChildren() {
		Command command = buildCommand("DATA a TYPE i.");
		Command newCommand = buildCommand("IF a = 2. RETURN. ENDIF.");

		try {
			command.insertRightSibling(newCommand, false);
			fail();
		} catch (IntegrityBrokenException ex) {
		}
	}
	
	@Test
	void testInsertRightSiblingToCommandWithChildren() {
		Command command = buildCommand("IF a = 2. RETURN. ENDIF.");
		Command newCommand = buildCommand("a = 1.");

		try {
			command.insertRightSibling(newCommand, false);
			fail();
		} catch (IntegrityBrokenException ex) {
		}
	}

	@Test
	void testInsertRightSiblingSectionNull() {
		Command command = buildCommand("DATA a TYPE i.");
		Section newSection = null;
		
		try {
			command.insertRightSibling(newSection, false);
			fail();
		} catch (IntegrityBrokenException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testInsertRightSiblingSectionOfOneCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i.");
		Section newSection = buildSection("a = 1.");
		command.insertRightSibling(newSection, false);
	}
	
	@Test
	void testInsertRightSiblingSectionToLastCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i.");
		Section newSection = buildSection("IF a = 1. RETURN. ENDIF.");
		command.insertRightSibling(newSection, false);
	}
	
	@Test
	void testInsertRightSiblingSectionToLastChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. a += 1. ENDIF.").getNext();
		Section newSection = buildSection("IF b = 1. RETURN. ENDIF.");
		command.insertRightSibling(newSection, false);
	}
	
	@Test
	void testInsertRightSiblingSectionToInnerChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. a += 1. a += 1. ENDIF.").getNext();
		Section newSection = buildSection("IF b = 1. RETURN. ENDIF.");
		command.insertRightSibling(newSection, false);
	}
	
	@Test
	void testInsertRightSiblingSectionToInnerCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i. a = 2.");
		Section newSection = buildSection("IF a = 1. RETURN. ENDIF.");
		command.insertRightSibling(newSection, false);
	}
	
	@Test
	void testInsertRightSiblingSectionToCommandWithChildren() {
		Command command = buildCommand("IF a = 2. RETURN. ENDIF.");
		Section newSection = buildSection("IF a = 1. RETURN. ENDIF.");

		try {
			command.insertRightSibling(newSection, false);
			fail();
		} catch (IntegrityBrokenException ex) {
		}
	}
	
	@Test
	void testAddTokenNull() {
		Command command = buildCommand("DATA a TYPE i.");
		try {
			command.addToken(null);
			fail();
		} catch(NullPointerException ex) {
		}
	}
	
	@Test
	void testCanAddNull() {
		Command command = buildCommand("DATA a TYPE i.");
		try {
			command.canAdd(null);
			fail();
		} catch(NullPointerException ex) {
		}
	}
	
	@Test
	void testCanAddToPragma() {
		Token tokenInNewLine = Token.createForAbap(1, 4, "\" comment", TokenType.COMMENT, 1);
		
		assertFalse(buildCommand("##PRAGMA").canAdd(tokenInNewLine));
		assertFalse(buildCommand("##PRAGMA ##ANOTHER").canAdd(tokenInNewLine));
		assertFalse(buildCommand("##PRAGMA ##ANOTHER \" comment").canAdd(tokenInNewLine));
	}
	
	@Test 
	void testFinishBuildDefLocalFriends() {
		assertFalse(buildCommand("CLASS cl_any DEFINITION DEFERRED.").getOpensLevel());
		assertFalse(buildCommand("CLASS cl_any DEFINITION LOCAL FRIENDS cl_other.").getOpensLevel());
	}
	
	@Test 
	void testFinishBuildChainOfLevelOpener() {
		// if a Command opens a level, only a chain of one is accepted:
		buildCommand("IF : a = 1.");
		buildCommand("DO : 5 TIMES.");

		// by contrast, a chain of multiple elements must throw a ParseException
		buildCommandExpectingParseExc("IF : a = 1, b = 2.");
		buildCommandExpectingParseExc("DO : 5 TIMES, 10 TIMES.");
	}

	@Test
	void testInsertLeftSiblingNull() {
		Command command = buildCommand("DATA a TYPE i.");
		Command newCommand = null;
		
		try {
			command.insertLeftSibling(newCommand);
			fail();
		} catch (IntegrityBrokenException ex) {
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test
	void testInsertLeftSiblingToFirstCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA b TYPE string.");
		Command newCommand = buildCommand("DATA a TYPE i.");
		command.insertLeftSibling(newCommand);
	}
	
	@Test
	void testInsertLeftSiblingToInnerCommand() throws IntegrityBrokenException {
		Command command = buildCommand("DATA a TYPE i. DATA c TYPE i. DATA d TYPE i.").getNext();
		Command newCommand = buildCommand("DATA b TYPE string.");
		command.insertLeftSibling(newCommand);
	}
	
	@Test
	void testInsertLeftSiblingToFirstChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. RETURN. ENDIF.").getNext();
		Command newCommand = buildCommand("a += 1.");
		command.insertLeftSibling(newCommand);
	}
	
	@Test
	void testInsertLeftSiblingToInnerChild() throws IntegrityBrokenException {
		Command command = buildCommand("IF a = 1. a += 1. c += 1. d += 1. ENDIF.").getNext().getNext();
		Command newCommand = buildCommand("b += 1.");
		command.insertLeftSibling(newCommand);
	}
	
	@Test
	void testInsertLeftSiblingThatHasChildren() {
		Command command = buildCommand("b = 1.");
		Command newCommand = buildCommand("IF a = 2. RETURN. ENDIF.");

		try {
			command.insertLeftSibling(newCommand);
			fail();
		} catch (IntegrityBrokenException ex) {
		}
	}

	void assertIntegrityBroken(Command command) {
		try {
			command.testReferentialIntegrity(true);
			fail();
		} catch(IntegrityBrokenException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	void assertIntegrityBroken(Command command1, Command command2) {
		assertIntegrityBroken(command1);
		assertIntegrityBroken(command2);
	}
	
	@Test
	void testReferentialIntegrity() {
		final String code =  "IF a = 2. RETURN. ENDIF.";
		
		buildCommand(code);
		commands[0].setNext(null);
		assertIntegrityBroken(commands[0], commands[1]);
		
		buildCommand(code);
		commands[1].setPrev(null);
		assertIntegrityBroken(commands[0]);
		
		buildCommand(code);
		commands[0].setNextSibling(null);
		assertIntegrityBroken(commands[0], commands[2]);
		
		buildCommand(code);
		commands[0].setNextSibling(commands[1]);
		assertIntegrityBroken(commands[0]);
		
		buildCommand(code);
		commands[2].setPrevSibling(null);
		assertIntegrityBroken(commands[0], commands[2]);
		
		buildCommand(code);
		commands[2].setPrevSibling(commands[1]);
		assertIntegrityBroken(commands[0]);
		
		buildCommand(code);
		commands[0].setFirstChild(null);
		assertIntegrityBroken(commands[0], commands[1]);
		
		buildCommand(code);
		commands[0].setFirstChild(commands[2]);
		assertIntegrityBroken(commands[0]);
		
		buildCommand(code);
		commands[0].setLastChild(null);
		assertIntegrityBroken(commands[0]);
		
		buildCommand(code);
		commands[0].setLastChild(commands[2]);
		assertIntegrityBroken(commands[0]);
	}

	@Test
	void testReferentialIntegrityCodeIncomplete() {
		final String code =  "IF a = 2. RETURN.";
		
		buildCommand(code);
		commands[0].setNext(null);
		assertIntegrityBroken(commands[0], commands[1]);
	}

	@Test
	void testReferentialIntegrityOfTokens() {
		final String code =  "IF a = 2. RETURN. ENDIF.";
		Token newToken = Token.createForAbap(1, 1, "\" comment", TokenType.COMMENT, 1);
		
		buildCommand(code);
		commands[0].getFirstToken().setPrev(newToken);
		assertIntegrityBroken(commands[0]);

		buildCommand(code);
		commands[0].getLastToken().setNext(newToken);
		assertIntegrityBroken(commands[0]);

		buildCommand("\" comment");
		try {
			commands[0].getFirstToken().addNext(newToken);
		} catch (UnexpectedSyntaxException e) {
		}
		assertIntegrityBroken(commands[0]);

		buildCommand(code);
		commands[0].getLastToken().setParentCommand(null);
		assertIntegrityBroken(commands[0]);

		buildCommand(code);
		try {
			commands[0].getLastToken().addNext(Token.createForAbap(0, 1, ABAP.COMMA_SIGN_STRING, TokenType.COMMA, 1));
		} catch (UnexpectedSyntaxException e) {
		};
		assertIntegrityBroken(commands[0]);
	}
	
	@Test
	void testConstructorTokenNull() {
		Code code = Code.createEmptyForTests();
		try {
			Command.create(code,  null, Language.ABAP);
			fail();
		} catch (NullPointerException ex) {
		}
	}
	
	@Test 
	void testSectionToString() {
		Command command = buildCommand("\r\nDATA a\r\nTYPE i.");
		assertEquals("\r\nDATA a\r\nTYPE i.", Command.sectionToString(command.firstToken, command.lastToken, false));
		assertEquals("DATA a TYPE i.", Command.sectionToString(command.firstToken, command.lastToken, true));
	}

	@Test
	void testRemoveFromCodeWithChildren() {
		Command command = buildCommand("IF a = 2. RETURN. ENDIF.");
		try {
			command.removeFromCode();
			fail();
		} catch(UnexpectedSyntaxException ex) {
		} catch(IntegrityBrokenException ex) {
			fail();
		}
	}

	@Test
	void testRemoveFromCodeFirstAndLast() throws UnexpectedSyntaxException, IntegrityBrokenException {
		buildCommand("a = 1. b = 2. c = 3.");
		commands[0].removeFromCode();
		assertEquals(commands[1].getParentCode().firstCommand, commands[1]);

		buildCommand("a = 1. b = 2. c = 3.");
		commands[2].removeFromCode();
		assertEquals(commands[1].getParentCode().lastCommand, commands[1]);
	}
	
	@Test
	void testPutCommentAboveLineOfErr() {
		Command command = buildCommand("DATA a TYPE i.");
		Token newToken = Token.createForAbap(1, 1, "SKIP", TokenType.KEYWORD, 1);
		
		try {
			command.putCommentAboveLineOf(command.firstToken, null);
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException  e) {
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		try {
			command.putCommentAboveLineOf(null, "comment");
			fail();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException  e) {
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		try {
			command.putCommentAboveLineOf(newToken, "comment");
			fail();
		} catch (UnexpectedSyntaxException  e) {
			// expected case
		} catch (IntegrityBrokenException ex) {
			fail();
		}
	}
	
	@Test
	void testPutCommentAboveLineOf() {
		try {
			buildCommand("DATA a TYPE i.");
			commands[0].putCommentAboveLineOf(commands[0].firstToken.getNext(), "comment");
			assertTrue(commands[0].getPrev() != null);
			
			buildCommand("DATA n TYPE string." + SEP + "DATA a TYPE i.").getNext();
			commands[1].putCommentAboveLineOf(commands[1].firstToken.getNext(), "comment");
			assertTrue(commands[1].getPrev() != commands[0]);

			buildCommand("\" other comment" + SEP + "DATA a TYPE i.").getNext();
			commands[1].putCommentAboveLineOf(commands[1].firstToken.getNext(), "comment");
			assertTrue(commands[1].getPrev() != commands[0]);

			buildCommand("\" comment" + SEP + "DATA a TYPE i.").getNext();
			commands[1].putCommentAboveLineOf(commands[1].firstToken.getNext(), "comment");
			assertTrue(commands[1].getPrev() == commands[0]);

		} catch (IntegrityBrokenException | UnexpectedSyntaxException  e) {
			fail();
		}
	}
	
	@Test
	void testRemoveMatchingCommentAboveLineOfErr() {
		String code = "DATA a TYPE i.";
		
		// call with token == null
		try {
			Command command = buildCommand(code);
			command.removeMatchingCommentAboveLineOf(null, "comment A");
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException | UnexpectedSyntaxAfterChanges ex) {
			fail();
		}

		// call with a token that belongs to another Command
		try {
			Token tokenInOtherCommand = Token.createForAbap(1, 1, "SKIP", TokenType.KEYWORD, 1);
			Command command = buildCommand(code);
			command.removeMatchingCommentAboveLineOf(tokenInOtherCommand, "comment A");
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		} catch (UnexpectedSyntaxAfterChanges e) {
			fail();
		}
	}
	
	@Test
	void testRemoveMatchingCommentAboveLineOf() {
		String code = "  DATA n TYPE string." + SEP + SEP + "  \" comment C" + SEP + "  DATA a TYPE i.";
		try {
			// call without any commentTextsToMatch parameters
			buildCommand(code);
			assertFalse(commands[2].removeMatchingCommentAboveLineOf(commands[2].firstToken.getNext()));

			// call with no previous line
			buildCommand("DATA a TYPE i.");
			assertFalse(commands[0].removeMatchingCommentAboveLineOf(commands[0].firstToken.getNext(), "\" comment A"));

			// call with the previous line NOT being a comment line
			buildCommand("DATA a TYPE i." + SEP + "DATA b TYPE i.");
			assertFalse(commands[1].removeMatchingCommentAboveLineOf(commands[1].firstToken.getNext(), "\" comment A"));
			
			// call with no comment matching the comment in the previous line 
			buildCommand(code);
			assertFalse(commands[2].removeMatchingCommentAboveLineOf(commands[2].firstToken.getNext(), "\" comment A", "\" comment B"));
			
			// call with the second comment matching the comment in the previous line 
			buildCommand(code);
			assertTrue(commands[2].removeMatchingCommentAboveLineOf(commands[2].firstToken.getNext(), "\" comment A", "\" comment C"));
			// expect the empty lines to be transferred from the deleted comment to the declaration of variable "a"
			assertEquals(2, commands[2].firstToken.lineBreaks);

		} catch (UnexpectedSyntaxAfterChanges | UnexpectedSyntaxException e) {
			fail();
		}
	}

	@Test
	void testAppendCommentToLineOfErr() {
		String code = "DATA: a TYPE i," + SEP + "  b TYPE i.";
		String comment = "\" comment";
		
		// call with token == null
		try {
			Command command = buildCommand(code);
			command.appendCommentToLineOf(null, comment);
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
			fail();
		}
		
		// call with commentText == null
		try {
			Command command = buildCommand(code);
			command.appendCommentToLineOf(command.getFirstToken(), null);
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
			fail();
		}

		// call with a token that belongs to another Command
		try {
			Token tokenInOtherCommand = Token.createForAbap(1, 1, "SKIP", TokenType.KEYWORD, 1);
			Command command = buildCommand(code);
			command.appendCommentToLineOf(tokenInOtherCommand, comment);
			fail();
		} catch (NullPointerException | IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			// expected case
		}
	}

	@Test
	void testAppendCommentToLineOf() throws NullPointerException, IntegrityBrokenException, UnexpectedSyntaxException {
		String code = "DATA: a TYPE i, \" old comment" + SEP + "  b TYPE i.";
		String comment = "new comment";
		
		// add a comment at the end of the Command
		Command command = buildCommand("DATA a TYPE i.");
		Token commentToken = command.appendCommentToLineOf(command.firstToken, comment);
		assertTrue(commentToken != null);
		assertTrue(commentToken.getNext() == null);

		// add a comment at the end of the line
		command = buildCommand("DATA: a TYPE i," + SEP + "  b TYPE i.");
		commentToken = command.appendCommentToLineOf(command.firstToken, comment);
		assertTrue(commentToken != null);
		assertEquals("b", commentToken.getNext().text);

		// add a comment to an existing comment
		command = buildCommand(code);
		commentToken = command.appendCommentToLineOf(command.firstToken, comment);
		assertTrue(commentToken != null);
		assertEquals("\" old comment \" new comment", commentToken.text);

		// keep an existing comment if it already contains the comment to be added
		command = buildCommand(code);
		commentToken = command.appendCommentToLineOf(command.firstToken, "old comment");
		assertTrue(commentToken != null);
		assertEquals("\" old comment", commentToken.text);
	}
	
	@Test
	void testRemoveMatchingCommentFromLineOf() {
		String code = "DATA a TYPE i.";
		
		// call with token == null
		try {
			Command command = buildCommand(code);
			command.removeMatchingCommentFromLineOf(null, "comment A");
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException | UnexpectedSyntaxAfterChanges ex) {
			fail();
		}

		// call with a token that belongs to another Command
		try {
			Token tokenInOtherCommand = Token.createForAbap(1, 1, "SKIP", TokenType.KEYWORD, 1);
			Command command = buildCommand(code);
			command.removeMatchingCommentFromLineOf(tokenInOtherCommand, "comment A");
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		} catch (UnexpectedSyntaxAfterChanges e) {
			fail();
		}
	}
	
	@Test
	void testGetNextNonCommentCommand() {
		buildCommand("* comment" + SEP + "  \" comment" + SEP + "  DATA a TYPE i.");

		assertEquals(commands[2], commands[0].getNextNonCommentCommand());
		assertEquals(commands[2], commands[1].getNextNonCommentCommand());
		assertEquals(null, commands[2].getNextNonCommentCommand());
	}
	
	@Test
	void testGetPrevNonCommentCommand() {
		buildCommand("  DATA a TYPE i." + SEP + "* comment" + SEP + "  \" comment");

		assertEquals(null, commands[0].getPrevNonCommentCommand());
		assertEquals(commands[0], commands[1].getPrevNonCommentCommand());
		assertEquals(commands[0], commands[2].getPrevNonCommentCommand());
	}

	@Test
	void testGetNextNonCommentSibling() {
		String code =      "* comment" 
				+ SEP + "IF a = 1." 
				+ SEP + "  \" comment" 
				+ SEP + "  a += 1." 
				+ SEP + "ENDIF." 
				+ SEP + "\" comment";

		buildCommand(code);

		assertEquals(commands[1], commands[0].getNextNonCommentSibling());
		assertEquals(commands[4], commands[1].getNextNonCommentSibling());
		assertEquals(commands[3], commands[2].getNextNonCommentSibling());
		assertEquals(null, commands[3].getNextNonCommentSibling());
		assertEquals(null, commands[4].getNextNonCommentSibling());
		assertEquals(null, commands[5].getNextNonCommentSibling());
	}
	
	@Test
	void testGetPrevNonCommentSibling() {
		String code =      "* comment" 
				+ SEP + "IF a = 1." 
				+ SEP + "  a += 1." 
				+ SEP + "  \" comment" 
				+ SEP + "ENDIF." 
				+ SEP + "\" comment";

		buildCommand(code);

		assertEquals(null, commands[0].getPrevNonCommentSibling());
		assertEquals(null, commands[1].getPrevNonCommentSibling());
		assertEquals(null, commands[2].getPrevNonCommentSibling());
		assertEquals(commands[2], commands[3].getPrevNonCommentSibling());
		assertEquals(commands[1], commands[4].getPrevNonCommentSibling());
		assertEquals(commands[4], commands[5].getPrevNonCommentSibling());
	}
	
	@Test
	void testContainsLineBreaksBetween() {
		Command command = buildCommand("DATA: a TYPE i, \" comment" + SEP + "  b TYPE string.");
		
		try {
			command.containsLineBreaksBetween(null, command.lastToken, false);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		assertTrue(command.containsLineBreaksBetween(command.firstToken, null, false));
		assertFalse(command.containsLineBreaksBetween(command.firstToken, null, true));
	}
	
	@Test 
	void testIsAbapSqlOperation() {
		assertTrue(buildCommand("SELECT * FROM dtab INTO TABLE result.").isAbapSqlOperation());
		assertTrue(buildCommand("OPEN CURSOR WITH HOLD @DATA(dbcur) FOR SELECT * FROM dtab.").isAbapSqlOperation());
		assertTrue(buildCommand("FETCH NEXT CURSOR @dbcur INTO @wa.").isAbapSqlOperation());
		assertTrue(buildCommand("CLOSE CURSOR @dbcur.").isAbapSqlOperation());

		assertTrue(buildCommand("INSERT INTO dtab VALUES wa.").isAbapSqlOperation());
		assertTrue(buildCommand("INSERT dtab FROM @( VALUE #( id = 'X' num1 = 1 ) ).").isAbapSqlOperation());
		assertTrue(buildCommand("INSERT dtab FROM TABLE itab. ").isAbapSqlOperation());
		assertTrue(buildCommand("INSERT dtab FROM ( SELECT * FROM dtab2 ). ").isAbapSqlOperation());
		assertTrue(buildCommand("INSERT dtab FROM @wa MAPPING FROM ENTITY.").isAbapSqlOperation());

		assertFalse(buildCommand("INSERT dref INTO TABLE ref_tab.").isAbapSqlOperation());
		assertFalse(buildCommand("INSERT sy-index INTO int_tab INDEX 1 REFERENCE INTO DATA(dref).").isAbapSqlOperation());
		
		assertTrue(buildCommand("DELETE FROM demo_update WHERE id = 'X'.").isAbapSqlOperation());
		// TODO: not yet identified:
		// assertTrue(buildCommand("DELETE dtab FROM @( VALUE #( id = 'X' ) )").isAbapSqlOperation());
		// assertTrue(buildCommand("DELETE dtab FROM TABLE @del_tab MAPPING FROM ENTITY.").isAbapSqlOperation());
		
		assertFalse(buildCommand("DELETE FROM SHARED BUFFER demo_indx_blob(XY) ID id.").isAbapSqlOperation());
		assertFalse(buildCommand("DELETE FROM MEMORY ID id.").isAbapSqlOperation());
		assertFalse(buildCommand("DELETE TABLE mesh-node1\\_node2[ mesh-node1[ 1 ] ].").isAbapSqlOperation());
		assertFalse(buildCommand("DELETE TABLE mesh-node1\\_node2[ mesh-node1[ 3 ] ] FROM VALUE line2( col3 = 33  ) USING KEY mkey. ").isAbapSqlOperation());
		assertFalse(buildCommand("DELETE itab WHERE table_line IS INITIAL.").isAbapSqlOperation());
		assertFalse(buildCommand("DELETE itab USING KEY skey FROM 4.").isAbapSqlOperation());

		assertFalse(buildCommand("MODIFY TABLE itab FROM VALUE #( BASE wa carrname = 'abc' ).").isAbapSqlOperation());
		assertFalse(buildCommand("MODIFY itab FROM VALUE line( col2 = 0 ) TRANSPORTING col2 WHERE col2 < 0.").isAbapSqlOperation());
		assertFalse(buildCommand("MODIFY itab FROM VALUE line( col1 = '_' ) USING KEY mkey TRANSPORTING col1 WHERE col2 = 0.").isAbapSqlOperation());
		assertFalse(buildCommand("MODIFY itab INDEX 1 USING KEY skey FROM VALUE #( col1 = 'X' ) TRANSPORTING col1. ").isAbapSqlOperation());
	}
	
	@Test 
	void testIsInClassImplementation() {
		buildCommand("CLASS any_class IMPLEMENTATION. METHOD any_method. DATA a TYPE i. ENDMETHOD. ENDCLASS.");

		assertTrue(commands[0].isInClassImplementation());
		assertTrue(commands[1].isInClassImplementation());
		assertTrue(commands[2].isInClassImplementation());
		assertTrue(commands[3].isInClassImplementation());
		assertTrue(commands[4].isInClassImplementation());

		buildCommand("CLASS any_class DEFINITION FINAL. PUBLIC SECTION. DATA a TYPE i. ENDCLASS.");

		assertFalse(commands[0].isInClassImplementation());
		assertFalse(commands[1].isInClassImplementation());
		assertFalse(commands[2].isInClassImplementation());
		assertFalse(commands[3].isInClassImplementation());
	}	
	
	@Test
	void testGetAllKeywordsWithCollocations() {
		Command command = buildCommand("LOOP AT itab FROM idx1 TRANSPORTING NO FIELDS WHERE table_line > 60. ENDLOOP.");
		assertStringArrayEquals(new String[] { "LOOP AT", "FROM", "TRANSPORTING NO FIELDS", "WHERE" }, command.getAllKeywordsWithCollocations());
	}
	
	@Test
	void testIsFunctionalCallOrCallChain() {
		assertTrue(buildCommand("any_method( iv_param = 1" + SEP + "  iv_name = `abc` ).").isFunctionalCallOrCallChain());
		assertTrue(buildCommand("any_method( ) \" comment" + SEP + "        .").isFunctionalCallOrCallChain());
		assertTrue(buildCommand("any_factory=>get( )->get_utility( )->any_method( iv_param = 1 ).").isFunctionalCallOrCallChain());
		
		assertFalse(buildCommand("a = get_value( ).").isFunctionalCallOrCallChain());
		assertFalse(buildCommand("DATA(a) = get_value( ).").isFunctionalCallOrCallChain());
		assertFalse(buildCommand("CALL METHOD any_method EXPORTING a = 1.").isFunctionalCallOrCallChain());
	}
	
	@Test
	void testGetDefinedName() {
		assertEquals("any_class", buildCommand("CLASS any_class DEFINITION FINAL.").getDefinedName());
		assertEquals("any_interface", buildCommand("INTERFACE any_interface PUBLIC.").getDefinedName());
		assertEquals("any_method", buildCommand("METHODS any_method FOR TESTING.").getDefinedName());
		assertEquals("any_method", buildCommand("METHODS: any_method FOR TESTING.").getDefinedName());
		assertEquals("any_method", buildCommand("METHODS: \" comment" + SEP + "    any_method FOR TESTING.").getDefinedName());

		assertEquals(null, buildCommand(".").getDefinedName());
		assertEquals(null, buildCommand("SKIP.").getDefinedName());
		assertEquals(null, buildCommand("\" comment").getDefinedName());
	}
	
	@Test
	void testRemoveAllFurtherChainColons() throws UnexpectedSyntaxAfterChanges {
		// nothing to remove
		Command command = buildCommand("DATA a TYPE i.");
		command.removeAllFurtherChainColons();
		assertEquals("DATA a TYPE i.", command.toString());

		// multiple chain colons
		command = buildCommand("DATA a : : TYPE :: i.");
		command.removeAllFurtherChainColons();
		assertEquals("DATA a TYPE i.", command.toString());

		// except whitespace before last chain colon to be transferred to the period .
		command = buildCommand("DATA a : TYPE : i \" comment" + SEP + "  : .");
		command.removeAllFurtherChainColons();
		assertEquals("DATA a TYPE i \" comment" + SEP + "  .", command.toString());

		// except whitespace before period . to be kept
		command = buildCommand("DATA a : TYPE : i \" comment" + SEP + "  :" + SEP + ".");
		command.removeAllFurtherChainColons();
		assertEquals("DATA a TYPE i \" comment" + SEP + ".", command.toString());
	}
	
	@Test
	void testCopyTokenRangeToNewCommand() {
		Command command = buildCommand("CALL METHOD any_method EXPORTING iv_param = : 1.");
		command.originalCommand = command;
		Token endToken = command.lastToken.getPrev().getPrev();
		
		try {
			Command newCommand = command.copyTokenRangeToNewCommand(command.firstToken, endToken, 1, 2);
			assertEquals("\r\n  CALL METHOD any_method EXPORTING iv_param =", newCommand.toString());

			newCommand = command.copyTokenRangeToNewCommand(command.firstToken.getNext().getNext(), endToken, 0, 4);
			assertEquals("    any_method EXPORTING iv_param =", newCommand.toString());

			newCommand = command.copyTokenRangeToNewCommand(null, endToken, 1, 2);
			assertEquals(null, newCommand);

		} catch (UnexpectedSyntaxException e) {
			fail();
		}
	}
	
	@Test
	void testGetLanguageOfNextCommand() {
		assertEquals(Language.SQL, buildCommand("EXEC SQL.").getLanguageOfNextCommand());

		assertEquals(Language.SQLSCRIPT, buildCommand("METHOD get_any_data BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING dtab1 dtab2.").getLanguageOfNextCommand());
		assertEquals(Language.SQL, buildCommand("METHOD get_any_data BY DATABASE FUNCTION FOR HDB LANGUAGE SQL.").getLanguageOfNextCommand());
		assertEquals(Language.GRAPH, buildCommand("METHOD get_shortest_path BY DATABASE PROCEDURE FOR HDB LANGUAGE GRAPH OPTIONS READ-ONLY USING cl_any=>any_workspace.").getLanguageOfNextCommand());
		assertEquals(Language.LLANG, buildCommand("METHOD any_method BY DATABASE PROCEDURE FOR HDB LANGUAGE LLANG OPTIONS READ-ONLY.").getLanguageOfNextCommand());
		assertEquals(Language.SQL, buildCommand("METHOD graph_workspace BY DATABASE GRAPH WORKSPACE FOR HDB LANGUAGE SQL USING any_graph").getLanguageOfNextCommand());
		
		// expect OTHER for an unknown language (e.g. a future extension) or if LANGUAGE is missing (which would be a syntax error)
		assertEquals(Language.OTHER, buildCommand("METHOD any_method BY DATABASE PROCEDURE FOR HDB LANGUAGE FUTURE_EXTENSION OPTIONS READ-ONLY.").getLanguageOfNextCommand());
		assertEquals(Language.OTHER, buildCommand("METHOD any_method BY DATABASE PROCEDURE FOR HDB OPTIONS READ-ONLY.").getLanguageOfNextCommand());

		// inside an EXEC SQL block, expect the language of the command following the comment to be SQL 
		assertEquals(Language.SQL, buildCommand("EXEC SQL." + SEP + "* comment").getNext().getLanguageOfNextCommand());
	}
	
	@Test
	void testRequiresEndSelect() {
		// since .requiresEndSelect() is private, we test .getOpensLevel() instead (see Command.finishBuild())
		assertTrue(buildCommand("SELECT DISTINCT (fieldlist) FROM dtab GROUP BY (scol) ORDER BY (scol) INTO (@dref->*, @count).").getOpensLevel());
		assertTrue(buildCommand("SELECT DISTINCT (fieldlist) FROM dtab WHERE fld1 = 'ABC' INTO @dref->*.").getOpensLevel());
		
		assertFalse(buildCommand("SELECT FROM dtab FIELDS fld1, MIN( price ) AS min_price, MAX( price ) AS max_price GROUP BY fld1 INTO TABLE @DATA(result).").getOpensLevel());
		assertFalse(buildCommand("SELECT FROM dtab FIELDS COUNT( CASE WHEN num1 < 4 THEN 'X' WHEN num1 BETWEEN 4 AND 7 THEN 'Y' END ) AS cnt, COUNT(*) AS cntstar INTO TABLE @DATA(result).").getOpensLevel());
		assertFalse(buildCommand("SELECT FROM dtab FIELDS COUNT(*) INTO (@DATA(avg)).").getOpensLevel());
		assertFalse(buildCommand("SELECT ( SUM( amt1 ) + SUM( amt2 ) ) AS amt_sum FROM any_table INTO @DATA(lt_amount).").getOpensLevel());
	}

	@Test
	void testInsertLeftSiblingSectionError() throws IntegrityBrokenException {
		boolean throwsException = false;
		Section nullSection = null; 
		try {
			buildCommand("CLEAR lv_any.").insertLeftSibling(nullSection);
		} catch (NullPointerException e) {
			throwsException = true;
		}
		assertTrue(throwsException);
	}

	@Test
	void testInsertLeftSiblingSectionToLevelCloserError() throws UnexpectedSyntaxException {
		boolean throwsException = false;
		Command newCommand = buildCommand("b = 2.");
		Section newSection = Section.create(newCommand, newCommand); 
		
		try {
			buildCommand("IF a = 1. c = 3. ENDIF.");
			commands[2].insertLeftSibling(newSection);
		} catch (IntegrityBrokenException e) {
			throwsException = true;
			assertTrue(StringUtil.contains(e.getMessage(), "closes a block"));
		}
		assertTrue(throwsException);
	}

	@Test
	void testInsertLeftSiblingSectionToFirstCommand() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Command command = buildCommand("DATA b TYPE string.");
		Command newCommand = buildCommand("DATA a TYPE i.");
		Section newSection = Section.create(newCommand, newCommand);
		command.insertLeftSibling(newSection);
	}
	
	@Test
	void testIsPublicClassDefinitionStart() {
		assertFalse(buildCommand("CLASS any_class DEFINITION CREATE PUBLIC.").isPublicClassDefinitionStart());
		assertFalse(buildCommand("CLASS any_class DEFINITION FINAL CREATE PRIVATE.").isPublicClassDefinitionStart());
		assertFalse(buildCommand("INTERFACE any_interface PUBLIC.").isPublicClassDefinitionStart());
		assertTrue(buildCommand("CLASS any_class DEFINITION ##PRAGMA PUBLIC.").isPublicClassDefinitionStart());
		assertTrue(buildCommand("CLASS any_class DEFINITION \"comment" + SEP + "PUBLIC \"comment" + SEP + ".").isPublicClassDefinitionStart());
	}
	
	private void assertChangesSyField(ABAP.SyField syField, boolean expChanges, String code) {
		assertEquals(expChanges, buildCommand(code).changesSyField(syField));
	}
	
	private void assertChangesSyField(ABAP.SyField syField, boolean expChanges, Command command) {
		assertEquals(expChanges, command.changesSyField(syField));
	}
	
	private void assertChangesSubrc(boolean expChanges, String code) {
		assertChangesSyField(SyField.SUBRC, expChanges, code);
	}
	
	private void assertChangesSubrc(boolean expChanges, Command command) {
		assertChangesSyField(SyField.SUBRC, expChanges, command);
	}
	
	@Test
	void testChangesSySubrc() {
		// expect changes on SY-SUBRC for: 
		assertChangesSubrc(true, "sy-subrc = lv_value.");
		assertChangesSubrc(true, "MOVE 1 TO sy-subrc.");

		// Object Creation
		assertChangesSubrc(true, "CREATE OBJECT lo_any EXPORTING param = lv_value.");
		assertChangesSubrc(true, "lo_instance = NEW cl_any_class( iv_param = lv_value ).");
		
		// Calling and Exiting Program Units
		assertChangesSubrc(true, "SUBMIT any_program VIA JOB lv_job_name NUMBER lv_jobcount AND RETURN.");
		assertChangesSubrc(true, "CALL FUNCTION 'ANY_FUNCTION' EXCEPTIONS any_exc = 1 OTHERS = 2.");
		assertChangesSubrc(true, "CALL METHOD me->(lc_method_name).");
		assertChangesSubrc(true, "SET HANDLER on_any_event FOR io_instance->get_event( ).");
		assertChangesSubrc(true, "a = b + c * cos( get_value( param = d ) ).");
		
		// Program Flow Logic
		assertChangesSubrc(true, "WAIT UP TO 5 SECONDS.");
		assertChangesSubrc(true, "RAISE any_classic_exception.");
		assertChangesSubrc(true, "RAISE RESUMABLE EXCEPTION NEW cx_any_exception( ).");
		
		// Assignments
		assertChangesSubrc(true, "ASSIGN itab[ 1 ] TO FIELD-SYMBOL(<ls_any>).");
		assertChangesSubrc(true, "ASSIGN lo_instance->mv_value TO <ls_any>.");

		// Processing Internal Data
		assertChangesSubrc(true, "CONCATENATE lv_year lv_month '01' INTO lv_date.");
		assertChangesSubrc(true, "FIND ls_data IN TABLE mt_data.");
		assertChangesSubrc(true, "OVERLAY text1 WITH text2 ONLY mask.");
		assertChangesSubrc(true, "REPLACE ALL OCCURRENCES OF lv_any IN <lv_other> WITH lv_other.");
		assertChangesSubrc(true, "SHIFT text UP TO 'abc'.");
		assertChangesSubrc(true, "SPLIT lv_value AT space INTO TABLE lt_table.");
		assertChangesSubrc(true, "GET BIT lv_bitpos OF lv_string INTO lv_value.");
		assertChangesSubrc(true, "SET BIT lv_bitpos OF lv_string TO lv_value.");
		assertChangesSubrc(true, "WRITE lv_timestamp TIME ZONE sy-zonlo TO lv_timestamp_str.");
		
		assertChangesSubrc(true, "CONVERT DATE lv_date TIME lv_time INTO TIME STAMP DATA(lv_timestamp) TIME ZONE lv_timezone.");
		assertChangesSubrc(true, "CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo INTO DATE lv_date.");
		
		assertChangesSubrc(true, "DELETE lt_table from 10.");
		assertChangesSubrc(true, "INSERT VALUE #( comp = 1 ) INTO TABLE lt_table.");
		assertChangesSubrc(true, buildCommand("LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.").getNext());
		assertChangesSubrc(true, "LOOP AT GROUP <fs> INTO member. ENDLOOP.");
		assertChangesSubrc(true, "READ TABLE its_table WITH KEY comp = 1 TRANSPORTING NO FIELDS.");
		
		// Processing External Data
		assertChangesSubrc(true, "DELETE FROM demo_update WHERE id = 'X'.");
		assertChangesSubrc(true, "FETCH NEXT CURSOR @dbcur INTO @wa.");
		assertChangesSubrc(true, "INSERT INTO dtab VALUES wa.");
		assertChangesSubrc(true, "INSERT dtab FROM ( SELECT * FROM dtab2 ). ");
		assertChangesSubrc(true, "SELECT * FROM dtab INTO TABLE result.");
		assertChangesSubrc(true, "OPEN CURSOR WITH HOLD @DATA(dbcur) FOR SELECT * FROM dtab.");
		
		assertChangesSubrc(true, buildCommand("EXEC SQL." + System.lineSeparator() + "ENDEXEC.").getNext());
		assertChangesSubrc(true, buildCommand("EXEC SQL." + System.lineSeparator() + "SQL." + System.lineSeparator() + "ENDEXEC.").getNext().getNext());
		assertChangesSubrc(true, "IMPORT any_tab = other_tab FROM DATABASE any_db(ab) ID any_id TO wa.");
		assertChangesSubrc(true, "IMPORT DIRECTORY INTO itab FROM DATABASE any_db(ab) ID 'AB'.");

		assertChangesSubrc(true, "CLOSE DATASET dset.");
		assertChangesSubrc(true, "DELETE DATASET dset.");
		assertChangesSubrc(true, "GET DATASET file POSITION FINAL(pos).");
		assertChangesSubrc(true, "OPEN DATASET dset FOR OUTPUT IN BINARY MODE.");
		assertChangesSubrc(true, "READ DATASET dset INTO xstr ACTUAL LENGTH FINAL(bytes).");
		assertChangesSubrc(true, "SET DATASET file POSITION END OF FILE.");
		assertChangesSubrc(true, "TRUNCATE DATASET dset AT CURRENT POSITION.");
		assertChangesSubrc(true, "TRANSFER text TO dset.");

		assertChangesSubrc(true, "AUTHORITY-CHECK OBJECT 'ANY_OBJ' ID 'ANYID' FIELD iv_value.");
		assertChangesSubrc(true, "COMMIT WORK.");
		assertChangesSubrc(true, "ROLLBACK WORK.");
		assertChangesSubrc(true, "SET UPDATE TASK LOCAL.");
		
		// ABAP for RAP Business Objects
		assertChangesSubrc(true, "COMMIT ENTITIES RESPONSES FAILED _failed_resp REPORTED _reported_resp.");
		
		// Program Parameters
		assertChangesSubrc(true, "GET PARAMETER ID 'PARAM_ID' FIELD DATA(lv_value).");
		assertChangesSubrc(true, "SET COUNTRY cntry.");
		assertChangesSubrc(true, "SET LANGUAGE lang.");
		
		// Program Editing
		assertChangesSubrc(true, "SET RUN TIME ANALYZER ON.");
		assertChangesSubrc(true, "GENERATE SUBROUTINE POOL itab NAME prog.");
		assertChangesSubrc(true, "INSERT REPORT prog FROM itab.");
		assertChangesSubrc(true, "INSERT TEXTPOOL prog FROM itab LANGUAGE lang.");
		assertChangesSubrc(true, "READ REPORT prog INTO itab MAXIMUM WIDTH INTO wid.");
		assertChangesSubrc(true, "READ TEXTPOOL prog INTO itab LANGUAGE lang.");
		assertChangesSubrc(true, "SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd.");
		
		// ABAP Data and Communication Interfaces
		assertChangesSubrc(true, "RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION' IMPORTING et_table = lt_table.");
		assertChangesSubrc(true, "WAIT FOR ASYNCHRONOUS TASKS UNTIL a > 10 UP TO 10 SECONDS.");
		assertChangesSubrc(true, "WAIT FOR MESSAGING CHANNELS UNTIL a > 10.");
		assertChangesSubrc(true, "WAIT FOR PUSH CHANNELS UNTIL a > 10.");
		
		assertChangesSubrc(true, "FREE OBJECT ole NO FLUSH.");
		assertChangesSubrc(true, "GET PROPERTY OF ole prop = dobj.");
		assertChangesSubrc(true, "SET PROPERTY OF ole prop = dobj.");
		
		// User Dialogs
		assertChangesSubrc(true, "GET CURSOR.");
		assertChangesSubrc(true, "SET TITLEBAR title OF PROGRAM prog.");
		assertChangesSubrc(true, "CALL SELECTION-SCREEN '0500' STARTING AT 10 10.");
		
		assertChangesSubrc(true, "DESCRIBE LIST NUMBER OF PAGES last_page.");
		assertChangesSubrc(true, "MODIFY CURRENT LINE LINE FORMAT COLOR 5.");
		assertChangesSubrc(true, "GET CURSOR LINE line.");
		assertChangesSubrc(true, "READ LINE lv_line FIELD VALUE flag date INTO wa.");
		assertChangesSubrc(true, "SCROLL LIST TO PAGE lv_page LINE lv_line.");
		
		// Enhancements
		assertChangesSubrc(true, "CALL BADI lo_badi->any_method CHANGING cts_table = lts_table.");
		
		// Statements for Experts
		assertChangesSubrc(true, "PROVIDE FIELDS col3 FROM itab1 INTO wa1 VALID flag1 BOUNDS col1 AND col2 FIELDS col3 FROM itab2 INTO wa2 VALID flag2 BOUNDS col1 AND col2 BETWEEN 2 AND 14."); 
		assertChangesSubrc(true, buildCommand("PROVIDE FIELDS col3 FROM itab1 INTO wa1 VALID flag1 BOUNDS col1 AND col2 FIELDS col3 FROM itab2 INTO wa2 VALID flag2 BOUNDS col1 AND col2 BETWEEN 2 AND 14. ENDPROVIDE.").getNext());
		
		// Obsolete Statements
		assertChangesSubrc(true, "CALL CUSTOMER-FUNCTION 'ABC'.");
		assertChangesSubrc(true, "CALL DIALOG dialog IMPORTING p1 TO a1.");
		assertChangesSubrc(true, "CATCH SYSTEM-EXCEPTIONS. ENDCATCH.");
		assertChangesSubrc(true, "SEARCH text FOR 'abc' ABBREVIATED.");
		assertChangesSubrc(true, "REFRESH itab FROM TABLE dbtab.");
		assertChangesSubrc(true, "DEMAND val1 = f1 val2 = f2 FROM CONTEXT any_context.");
		assertChangesSubrc(true, "EDITOR-CALL FOR lt_text BACKUP INTO lt_backup.");
		assertChangesSubrc(true, "COMMUNICATION ACCEPT ID id.");
		
		// Internal Statements
		assertChangesSubrc(true, "DELETE DYNPRO f.");
		assertChangesSubrc(true, "DELETE REPORT prog.");
		assertChangesSubrc(true, "GENERATE REPORT prog.");
		assertChangesSubrc(true, "DELETE TEXTPOOL prog LANGUAGE lg STATE state.");
		assertChangesSubrc(true, "LOAD REPORT prog PART 'BASE' INTO itab.");
		assertChangesSubrc(true, "IMPORT DYNPRO h f e m ID id.");
		assertChangesSubrc(true, "SCAN AND CHECK ABAP-SOURCE itab1 RESULT INTO itab2.");
		assertChangesSubrc(true, "SYNTAX-CHECK FOR DYNPRO h f e m MESSAGE f1 LINE f2 WORD f3.");
		assertChangesSubrc(true, "CALL 'ANY_FUNCTION' ID 'NAME' FIELD 'ANY_NAME' ID 'VALUE' FIELD any_value.");
		
		// ----------------------------------------------------------------------
		// expect NO changes on SY-SUBRC for: 
		
		assertChangesSubrc(false, "\" comment.");
		assertChangesSubrc(false, "a = xsdbool( a OR d).");
		assertChangesSubrc(false, "IF line_exists( lts_table[ comp = 1 ] ). ENDIF.");
		assertChangesSubrc(false, "a = sqrt( log( abs( ceil( floor( sign( 5 - 7 ) ) ) ) ) ) * sin( cos( lv_value ) ).");
		assertChangesSubrc(false, "a = nmax( val1 = b val2 = nmin( val1 = c val2 = d ) ).");
		assertChangesSubrc(false, "AT NEW comp. ENDAT."); // as opposed to 'a = NEW cl_any_class( ).'
		assertChangesSubrc(false, "SHIFT text BY off PLACES.");
		assertChangesSubrc(false, "SHIFT txt RIGHT DELETING TRAILING ` `.");
	}
	
	private void assertChangesTabix(boolean expChanges, String code) {
		assertChangesSyField(SyField.TABIX, expChanges, code);
	}
	
	private void assertChangesTabix(boolean expChanges, Command command) {
		assertChangesSyField(SyField.TABIX, expChanges, command);
	}
	
	@Test
	void testChangesSyTabix() {
		// expect changes on SY-TABIX for: 
		assertChangesTabix(true, "sy-tabix = lv_value.");
		assertChangesTabix(true, "MOVE 1 TO sy-tabix.");
		
		assertChangesTabix(true, "ASSIGN itab[ 1 ] TO FIELD-SYMBOL(<ls_any>).");
		assertChangesTabix(true, "ASSIGN its_table[ table_line = 1 ] TO <ls_other>.");
		
		assertChangesTabix(true, "LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.");
		assertChangesTabix(true, buildCommand("LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.").getNext());
		
		assertChangesTabix(true, "READ TABLE its_table WITH KEY comp = 1 TRANSPORTING NO FIELDS.");
		assertChangesTabix(true, "READ TABLE its_table INDEX 1 USING KEY sort_key ASSIGNING FIELD-SYMBOL(<ls_any>).");
		
		assertChangesTabix(true, "COLLECT ls_line INTO lts_table.");
		assertChangesTabix(true, "COLLECT <ls_line> INTO lts_table ASSIGNING FIELD-SYMBOL(<ls_any>).");
		
		assertChangesTabix(true, "APPEND VALUE #( comp = 1 ) TO lt_table.");
		assertChangesTabix(true, "APPEND ls_line TO lt_table ASSIGNING <ls_any>.");

		assertChangesTabix(true, "PROVIDE FIELDS col3 FROM itab1 INTO wa1 VALID flag1 BOUNDS col1 AND col2 FIELDS col3 FROM itab2 INTO wa2 VALID flag2 BOUNDS col1 AND col2 BETWEEN 2 AND 14."); 
		assertChangesTabix(true, buildCommand("PROVIDE FIELDS col3 FROM itab1 INTO wa1 VALID flag1 BOUNDS col1 AND col2 FIELDS col3 FROM itab2 INTO wa2 VALID flag2 BOUNDS col1 AND col2 BETWEEN 2 AND 14. ENDPROVIDE.").getNext()); 

		// expect NO changes on SY-TABIX for: 
		assertChangesTabix(false, "\" comment.");
		assertChangesTabix(false, "ASSIGN lo_instance->mv_value TO <ls_any>.");
		assertChangesTabix(false, "INSERT VALUE #( comp = 1 ) INTO TABLE lt_table.");
		assertChangesTabix(false, "DELETE lt_table from 10.");
		assertChangesTabix(false, "ls_line = lt_table[ 1 ].");
		assertChangesTabix(false, "ls_line = lt_table[ id = 1 ].");
	}
	
	private void assertChangesIndex(boolean expChanges, String code) {
		assertChangesSyField(SyField.INDEX, expChanges, code);
	}
	
	private void assertChangesIndex(boolean expChanges, Command command) {
		assertChangesSyField(SyField.INDEX, expChanges, command);
	}
	
	@Test
	void testChangesSyIndex() {
		// expect changes on SY-INDEX for: 
		assertChangesIndex(true, "sy-index = lv_value.");
		assertChangesIndex(true, "MOVE 1 TO sy-index.");
		
		assertChangesIndex(true, "DO 5 TIMES. ENDDO.");
		assertChangesIndex(true, buildCommand("DO 5 TIMES. ENDDO.").getNext());
		
		assertChangesIndex(true, "WHILE a < 5. ENDWHILE.");
		assertChangesIndex(true, buildCommand("WHILE a < 5. ENDWHILE.").getNext());
		
		// expect NO changes on SY-INDEX for: 
		assertChangesIndex(false, "\" comment.");
		assertChangesIndex(false, "LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.");
		assertChangesIndex(false, "DELETE lt_table from 10.");
		assertChangesIndex(false, "READ TABLE its_table WITH KEY comp = 1 TRANSPORTING NO FIELDS.");
		assertChangesIndex(false, "ls_line = lt_table[ id = 1 ].");
	}

	private void assertChangesTFillOrTLeng(boolean expChanges, String code) {
		assertChangesSyField(SyField.TFILL, expChanges, code);
		assertChangesSyField(SyField.TLENG, expChanges, code);
	}
	
	private void assertChangesTFillOrTLeng(boolean expChanges, Command command) {
		assertChangesSyField(SyField.TFILL, expChanges, command);
		assertChangesSyField(SyField.TLENG, expChanges, command);
	}
	
	@Test
	void testChangesSyTFillOrTLeng() {
		// expect changes on SY-TFILL or SY-TLENG for: 
		assertChangesTFillOrTLeng(true, "sy-tfill = lv_value.");
		assertChangesTFillOrTLeng(true, "MOVE 1 TO sy-tfill.");
		assertChangesTFillOrTLeng(true, "sy-tleng = lv_value.");
		assertChangesTFillOrTLeng(true, "MOVE 1 TO sy-tleng.");
		
		assertChangesTFillOrTLeng(true, "LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.");
		
		assertChangesTFillOrTLeng(true, "READ TABLE its_table WITH KEY comp = 1 TRANSPORTING NO FIELDS.");
		assertChangesTFillOrTLeng(true, "READ TABLE its_table INDEX 1 USING KEY sort_key ASSIGNING FIELD-SYMBOL(<ls_any>).");
		
		assertChangesTFillOrTLeng(true, "DESCRIBE TABLE lt_table.");
		assertChangesTFillOrTLeng(true, "DESCRIBE TABLE lt_table LINES lv_any.");
		assertChangesTFillOrTLeng(true, "DESCRIBE TABLE lt_table LINES lv_any OCCURS n.");

		// expect NO changes on SY-TFILL or SY-TLENG for: 
		assertChangesTFillOrTLeng(false, "\" comment.");
		assertChangesTFillOrTLeng(false, buildCommand("LOOP AT lt_any INTO DATA(ls_any). ENDLOOP.").getNext());
		assertChangesTFillOrTLeng(false, "LOOP AT GROUP <fs> INTO member. ENDLOOP.");
		assertChangesTFillOrTLeng(false, "INSERT VALUE #( comp = 1 ) INTO TABLE lt_table.");
		assertChangesTFillOrTLeng(false, "DELETE lt_table from 10.");
		assertChangesTFillOrTLeng(false, "ls_line = lt_table[ 1 ].");
	}
	
}
