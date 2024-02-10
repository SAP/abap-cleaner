package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;

public abstract class CodeTestBase {
	protected static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	protected StringBuilder sourceCodeBuilder = new StringBuilder();
	private int lineCount = 0;
	/** contains the 1-based last line (inclusive) of each section */
	private ArrayList<Integer> expandSectionLastLines = new ArrayList<Integer>();
	
	protected void buildSrc(String line) {
		buildCode(sourceCodeBuilder, line);
	}

	private void buildCode(StringBuilder sb, String line) {
		if (sb.length() > 0) 
			sb.append(LINE_SEP);
		sb.append(StringUtil.trimEnd(line));
		++lineCount;
	}

	protected void putAnyMethodAroundSrc() {
		putAroundSrc("  METHOD any_method." + LINE_SEP, LINE_SEP + "  ENDMETHOD.");
	}
	
	protected void putAnyClassDefAroundSrc() {
		putAroundSrc("CLASS any_class DEFINITION." + LINE_SEP, LINE_SEP + "ENDCLASS.");
	}

	protected void endOfExpandSection() {
		expandSectionLastLines.add(lineCount);
	}
	
	private void putAroundSrc(String start, String end) {
		sourceCodeBuilder.insert(0, start);
		sourceCodeBuilder.append(end);
	}

	protected void testParseWithCleanupRange(CleanupRangeExpandMode expandMode) {
		// parse the code and perform the related tests 
		Code code = testParseCode();
		
		// test the cleanup range resulting from various simulated cursor positions 
		// (all line numbers are 1-based and inclusive)
		int sectionStartLine = 1; 
		for (int sectionLastLine : expandSectionLastLines) { 

			for (int line = sectionStartLine; line <= sectionLastLine; ++line) {
				// pretend that cleanup is called from the add-on with the cursor somewhere on the 1-based 'line' number
				// and no code selected (so the line shall be expanded to the surrounding method or declaration section)
				CleanupRange cleanupRange = CleanupRange.create(line, line, true);
				
				// parse the code each time with a different cleanup range (but don't repeat all the tests in parseCode()
				code.cleanupRange = cleanupRange;
				code.expandCleanupRange(expandMode); 
				
				// check the cleanup range to which 'line' was expanded
				cleanupRange = code.cleanupRange;
				assertEquals(sectionStartLine, cleanupRange.startLine);
				assertEquals(sectionLastLine, cleanupRange.lastLine);
				assertFalse(cleanupRange.expandRange); 
			}

			sectionStartLine = sectionLastLine + 1;
		}
	}
	
	protected Code testParseCode() {
		// parse the source code
		String sourceCode = sourceCodeBuilder.toString();
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail(e.getMessage());
			return null;
		}

		// test the referential integrity of the parse result
		try {
			code.testReferentialIntegrity(true);
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code:" + e1.getMessage());
			return null;
		} 

		// assert that rebuilding the code from the objects (Code, Command, Token etc.) results in the same source text
		String parseCheckErrors = code.compareWithSource(sourceCode, 10); // null if rebuilt code matches source text
		if (parseCheckErrors != null) { 
			fail(parseCheckErrors);
			return null;
		}
		
		return code;
	}
	
	protected void testParseCodeExpectingException(String expMessageSubstring) {
		String sourceCode = sourceCodeBuilder.toString();
		try {
			Code.parse(null, ParseParams.createForTest(sourceCode, ABAP.NEWEST_RELEASE));
			fail("expected a ParseException!");
		} catch (ParseException e) {
			// ParseException was expected
			if (!StringUtil.isNullOrEmpty(expMessageSubstring)) {
				String message = e.getMessage();
				if (message.toUpperCase().indexOf(expMessageSubstring.toUpperCase()) < 0)
					fail("expected the text '" + expMessageSubstring + "' in the message, but got: '" + message);
			}
			return;
		}
	}
	
	protected void assertExecSqlSectionFound(Command execCommand, Command endExecCommand) {
		assertNonAbapSectionFound(execCommand, "EXEC", endExecCommand, "ENDEXEC");
	}

	
	protected void assertMethodByDatabaseSectionFound(Command methodCommand, Command endMethodCommand) {
		assertNonAbapSectionFound(methodCommand, "METHOD", endMethodCommand, "ENDMETHOD");
	}

	private void assertNonAbapSectionFound(Command startCommand, String startCommandText, Command endCommand, String endCommandText) {
		assertTrue(startCommand.firstToken.isKeyword(startCommandText));
		assertTrue(endCommand.firstToken.isKeyword(endCommandText));

		assertTrue(startCommand.isAbap());
		assertTrue(endCommand.isAbap());

		// check the Commands inside the non-ABAP section 
		Command command = startCommand.getNext();
		while (command != endCommand) {
			// the Commands must be identified as being inside the non-ABAP section  
			assertFalse(command.isAbap());
			assertTrue(command.getLanguage() != Language.ABAP);
			
			// Commands must NOT have child Commands (i.e. identifying Command blocks must be turned off inside non-ABAP sections)
			assertFalse(command.hasChildren());
			
			Token token = command.firstToken;
			while (token != null) {
				// Tokens may be comments or type NON_ABAP, but nothing else
				if (token.type != TokenType.COMMENT && token.type != TokenType.NON_ABAP)
					assertTrue(token.type == TokenType.COMMENT || token.type == TokenType.NON_ABAP);
				// Tokens must NOT have child Tokens (i.e. identifying levels of parentheses must be turned off)
				assertFalse(token.hasChildren());

				token = token.getNext();
			}
			
			command = command.getNext();
		}
	}

	
}
