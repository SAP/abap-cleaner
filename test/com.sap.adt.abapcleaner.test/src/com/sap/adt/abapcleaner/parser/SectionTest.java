package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class SectionTest {
	private static final String SEP = ABAP.LINE_SEPARATOR;

	private Code parseCode(String codeText) {
		try {
			return Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail();
			return null;
		}
	}

	private Section buildSection(String codeText) {
		try {
			Code code = parseCode(codeText);
			return Section.create(code.firstCommand, code.lastCommand);
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
		return null;
	}

	private Section getSection(Code code, int firstCommandIndex, int lastCommandIndex) {
		try {
			Command firstCommand = null;
			Command lastCommand = null;
			Command command = code.firstCommand;
			int index = 0;
			while (command != null) {
				if (index == firstCommandIndex)
					firstCommand = command;
				if (index == lastCommandIndex)
					lastCommand = command;
				command = command.getNext();
				++index;
			}
			return Section.create(firstCommand, lastCommand);
			
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
		return null;
	}

	@Test
	void testConstructorErr() {
		Code code = parseCode("a = 10. DO 5 TIMES. a += 1. ENDDO.");

		// call with null as the first parameter and expect an exception
		try {
			Section.create(null, code.firstCommand);
			fail();
		} catch (UnexpectedSyntaxException e) {
			fail();
		} catch(NullPointerException ex) {
			// expected case
		}

		// call with null as the second parameter and expect an exception
		try {
			Section.create(code.firstCommand, null);
			fail();
		} catch (UnexpectedSyntaxException e) {
			fail();
		} catch(NullPointerException ex) {
			// expected case
		}

		// call with a lastCommand that has children and expect an exception
		try {
			Section.create(code.firstCommand, code.firstCommand.getNext());
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expect the keyword of the lastCommand to be mentioned in the error message
			assertTrue(ex.getMessage().indexOf(code.firstCommand.firstToken.text) >= 0);
		} catch(NullPointerException ex) {
			fail();
		}

		// call with a firstCommand and a lastCommand that are NOT siblings
		try {
			Section.create(code.firstCommand, code.firstCommand.getNext().getNext());
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		} catch(NullPointerException ex) {
			fail();
		}
	}

	@Test
	void testIsSingleCommand() {
		assertTrue(buildSection("a = 10.").isSingleCommand());
		assertFalse(buildSection("DO 5 TIMES. a += 1. ENDDO.").isSingleCommand());
	}

	@Test
	void testGetSiblingCount() {
		assertEquals(1, buildSection("a = 10.").getSiblingCount());
		assertEquals(2, buildSection("DO 5 TIMES. a += 1. ENDDO.").getSiblingCount());
	}

	@Test
	void testGetCommandCountWithChildren() {
		assertEquals(1, buildSection("a = 10.").getCommandCountWithChildren());
		assertEquals(3, buildSection("DO 5 TIMES. a += 1. ENDDO.").getCommandCountWithChildren());
	}

	@Test
	void testToString() {
		assertEquals("a = 10.", buildSection("a = 10.").toString());
		assertEquals("DO 5 TIMES. a += 1. ENDDO.", buildSection("DO 5 TIMES. a += 1. ENDDO.").toString());
	}

	@Test
	void testAddIndent() {
		String codeText = 
				  SEP + "DO 5 TIMES." 
				+ SEP + "  a += 1." 
				+ SEP + "ENDDO.";

		Section section = buildSection(codeText);
		assertTrue(section.addIndent(-4));
		assertEquals(SEP + "DO 5 TIMES." + SEP + "a += 1." + SEP + "ENDDO.", section.toString());

		section = buildSection(codeText);
		assertTrue(section.addIndent(-2));
		assertEquals(SEP + "DO 5 TIMES." + SEP + "a += 1." + SEP + "ENDDO.", section.toString());

		section = buildSection(codeText);
		assertFalse(section.addIndent(0));
		assertEquals(codeText, section.toString());
		
		section = buildSection(codeText);
		assertTrue(section.addIndent(2));
		assertEquals(SEP + "  DO 5 TIMES." + SEP + "    a += 1." + SEP + "  ENDDO.", section.toString());

		section = buildSection(codeText);
		assertTrue(section.addIndent(4));
		assertEquals(SEP + "    DO 5 TIMES." + SEP + "      a += 1." + SEP + "    ENDDO.", section.toString());
	}

	@Test
	void testRemoveFromCode() {
		String codeText = "a = 1." 
				+ SEP + "b = 2." 
				+ SEP + "DO 5 TIMES." 
				+ SEP + "  a += 1." 
				+ SEP + "  \" comment" 
				+ SEP + "  b *= 2." 
				+ SEP + "ENDDO.";

		Code code = parseCode(codeText);
		getSection(code, 0, 1).removeFromCode();
		assertEquals(SEP + "DO 5 TIMES." + SEP + "  a += 1." + SEP + "  \" comment" + SEP + "  b *= 2." + SEP + "ENDDO.", code.toString());

		code = parseCode(codeText);
		getSection(code, 2, 6).removeFromCode();
		assertEquals("a = 1." + SEP + "b = 2.", code.toString());

		code = parseCode(codeText);
		getSection(code, 3, 4).removeFromCode();
		assertEquals("a = 1." + SEP + "b = 2." + SEP + "DO 5 TIMES." + SEP + "  b *= 2." + SEP + "ENDDO.", code.toString());

		code = parseCode(codeText);
		getSection(code, 4, 5).removeFromCode();
		assertEquals("a = 1." + SEP + "b = 2." + SEP + "DO 5 TIMES." + SEP + "  a += 1." + SEP + "ENDDO.", code.toString());

		code = parseCode(codeText);
		getSection(code, 3, 5).removeFromCode();
		assertEquals("a = 1." + SEP + "b = 2." + SEP + "DO 5 TIMES." + SEP + "ENDDO.", code.toString());

		code = parseCode(codeText);
		getSection(code, 4, 4).removeFromCode();
		assertEquals("a = 1." + SEP + "b = 2." + SEP + "DO 5 TIMES." + SEP + "  a += 1." + SEP + "  b *= 2." + SEP + "ENDDO.", code.toString());
	}

	@Test
	void testSetPrev() {
		String codeText = "a = 1." 
				+ SEP + "b = 2." 
				+ SEP + "c = 3." 
				+ SEP + "d = 4.";

		Code code = parseCode(codeText);
		Section section = getSection(code, 2, 3);
		assertEquals("b", section.getPrev().firstToken.text);
		assertEquals("b", section.firstCommand.getPrev().firstToken.text);

		section.setPrev(code.firstCommand);
		assertEquals("a", section.getPrev().firstToken.text);
		assertEquals("a", section.firstCommand.getPrev().firstToken.text);
	}

	@Test
	void testSetNext() {
		String codeText = "a = 1." 
				+ SEP + "b = 2." 
				+ SEP + "c = 3." 
				+ SEP + "d = 4.";

		Code code = parseCode(codeText);
		Section section = getSection(code, 0, 1);
		assertEquals("c", section.getNext().firstToken.text);
		assertEquals("c", section.lastCommand.getNext().firstToken.text);

		section.setNext(code.lastCommand);
		assertEquals("d", section.getNext().firstToken.text);
		assertEquals("d", section.lastCommand.getNext().firstToken.text);
	}
}
