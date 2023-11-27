package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import java.security.InvalidParameterException;
import java.util.ArrayList;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class TokenTest {
	private static final String SEP = ABAP.LINE_SEPARATOR;
	private Token[] tokens;
	private Code code;
	
	private Command buildCommand(String codeText) {
		try {
			code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));

			Command command = code.firstCommand;
			tokens = new Token[command.tokenCount];
			tokens[0] = command.firstToken;
			for (int i = 1; i < command.tokenCount; ++i)
				tokens[i] = tokens[i - 1].getNext();

			return command;
		} catch (ParseException e) {
			fail();
			return null;
		}
	}

	private Token buildCommand(String codeText, int tokenIndex) {
		Command command = buildCommand(codeText);
		Token token = command.firstToken;
		int index = 0;
		while (index < tokenIndex) {
			token = token.getNext();
			++index;
		}
		return token;
	}

	private Token buildCommand(String codeText, String tokenText) {
		Command command = buildCommand(codeText);
		return command.firstToken.getLastTokenDeep(false, TokenSearch.ASTERISK, tokenText);
	}

	@Test
	void testIsCommentAfterCode() {
		assertFalse(buildCommand("a = 1.", 0).isCommentAfterCode());
		assertFalse(buildCommand("  \" comment", 0).isCommentAfterCode());
		assertFalse(buildCommand("  any_method(" + SEP + "  \" comment" + SEP + "  iv_param = 1 ).", 1).isCommentAfterCode());
	
		assertTrue(buildCommand("  a = 1. \" comment", 4).isCommentAfterCode());
		assertTrue(buildCommand("  any_method( \" comment" + SEP + "  iv_param = 1 ).", 1).isCommentAfterCode());
	}
	
	@Test
	void testIsClosingParenthesisOrBracket() {
		assertFalse(buildCommand("a = 1.", 0).isClosingParenthesisOrBracket());
		assertFalse(buildCommand("a = 1 + 2.", 3).isClosingParenthesisOrBracket());
		assertTrue(buildCommand("any_method( ).", 1).isClosingParenthesisOrBracket());
		assertTrue(buildCommand("a = lts_table[ 1 ].", 4).isClosingParenthesisOrBracket());
	}
	
	@Test
	void testIsLastTokenInCommand() {
		assertFalse(buildCommand("a = 1.", 0).isLastTokenInCommand());
		assertTrue(buildCommand("a = 1.", 3).isLastTokenInCommand());
	}
	
	@Test
	void testIsLastTokenInLineExceptComment() {
		assertFalse(buildCommand("a = 1.", 0).isLastTokenInLineExceptComment());
		assertTrue(buildCommand("a = 1.", 3).isLastTokenInLineExceptComment());
		assertTrue(buildCommand("a = 1. \" comment", 3).isLastTokenInLineExceptComment());
		assertTrue(buildCommand("any_method(" + SEP + "  param = 1 ).", 0).isLastTokenInLineExceptComment());
	}
	
	@Test
	void testIsOnlyTokenInLine() {
		assertFalse(buildCommand("a = 1.", 0).isOnlyTokenInLine());
		assertFalse(buildCommand("a = 1.", 3).isOnlyTokenInLine());
		assertTrue(buildCommand("any_method(" + SEP + "  param = 1 ).", 0).isOnlyTokenInLine());
		assertTrue(buildCommand(".", 0).isOnlyTokenInLine());
	}
	
	@Test 
	void testOpensInlineDeclaration() {
		assertTrue(buildCommand("DATA(lv_value) TYPE i.", 0).opensInlineDeclaration());
		assertTrue(buildCommand("FINAL(lv_value) = 1.", 0).opensInlineDeclaration());
		assertTrue(buildCommand("FIELD-SYMBOL(<ls_struc>) TYPE ty_s_struc.", 0).opensInlineDeclaration());
		assertFalse(buildCommand("any_method( REF data( lv_value ) ).", 2).opensInlineDeclaration());
	}
	
	@Test 
	void testIsAttached() {
		assertFalse(buildCommand(" a = 1.", 0).isAttached());
		assertTrue(buildCommand(" a = 1.", 3).isAttached());
		assertTrue(buildCommand("DATA: a TYPE i, b TYPE i.", 1).isAttached());
		assertFalse(buildCommand("any_method(" + SEP + "param = 1 ).", 1).isAttached());
	}
	
	@Test
	void testIsFloatLiteral() {
		assertTrue(buildCommand(" a = '1'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '3.14'.", 2).isFloatLiteral());

		assertFalse(buildCommand(" a = 'a'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '+a'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = 'a-'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '3,14'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '3.14.15'.", 2).isFloatLiteral());

		// ensure that + and - are possible at the start or end of the number (but not both)
		assertTrue(buildCommand(" a = '-1'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '+1'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '1-'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '1+'.", 2).isFloatLiteral());

		assertTrue(buildCommand(" a = '-3.14'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '+3.14'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '3.14-'.", 2).isFloatLiteral());
		assertTrue(buildCommand(" a = '3.14+'.", 2).isFloatLiteral());

		assertFalse(buildCommand(" a = '-1-'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '+1+'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '-1+'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '+1-'.", 2).isFloatLiteral());

		assertFalse(buildCommand(" a = '-3.14-'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '+3.14+'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '-3.14+'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '+3.14-'.", 2).isFloatLiteral());

		// ensure that only one + or - is accepted 
		assertFalse(buildCommand(" a = '--1'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '++1'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '1--'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '1++'.", 2).isFloatLiteral());

		assertFalse(buildCommand(" a = '--3.14'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '++3.14'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '3.14--'.", 2).isFloatLiteral());
		assertFalse(buildCommand(" a = '3.14++'.", 2).isFloatLiteral());
	}
	
	@Test 
	void testIsPragma() {
		// ensure that ##SHADOW[INSERT] is parsed as one single pragma, including its parameter [INSERT]
		assertTrue(buildCommand("  METHODS insert ##SHADOW[INSERT].", 2).isPragma());
		assertTrue(buildCommand("  METHODS insert ##SHADOW[INSERT].", 2).textEquals("##SHADOW[INSERT]"));
		assertTrue(buildCommand("  METHODS insert ##SHADOW[INSERT].", 3).isPeriod());

		// do the same for a pragma with multiple parameters, including an empty parameter []
		assertTrue(buildCommand("  METHODS insert ##ANY_PRAGMA[PARAM1][][PARAM3].", 2).isPragma());
		assertTrue(buildCommand("  METHODS insert ##ANY_PRAGMA[PARAM1][][PARAM3].", 2).textEquals("##ANY_PRAGMA[PARAM1][][PARAM3]"));
		assertTrue(buildCommand("  METHODS insert ##ANY_PRAGMA[PARAM1][][PARAM3].", 3).isPeriod());

		// ensure that multiple pragmas are parsed into multiple Tokens
		assertTrue(buildCommand("  METHODS insert ##PRAGMA1 ##PRAGMA2[PARAM] ##PRAGMA3.", 2).isPragma());
		assertTrue(buildCommand("  METHODS insert ##PRAGMA1 ##PRAGMA2[PARAM] ##PRAGMA3.", 3).isPragma());
		assertTrue(buildCommand("  METHODS insert ##PRAGMA1 ##PRAGMA2[PARAM] ##PRAGMA3.", 4).isPragma());
		assertTrue(buildCommand("  METHODS insert ##PRAGMA1 ##PRAGMA2[PARAM] ##PRAGMA3.", 5).isPeriod());
	}
	
	@Test
	void testCreateErr() {
		try { 
			Token.createForAbap(0, 1, null, 1);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}

		try { 
			Token.create(0, 1, null, 1, Language.SQLSCRIPT);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}
	}
	
	@Test
	void testInferTypeFromAbapToken() {
		// e.g. "SELECTION-SCREEN PUSHBUTTON 2(10) but1 USER-COMMAND cli1."
		Token token = Token.createForAbap(0, 1, "1(75)", 1);
		assertEquals(TokenType.LITERAL, token.type);
	}
	
	@Test
	void testTokenTypeIdentifierWithExclamationMark() {
		// e.g. "DATA: !min TYPE i, !max TYPE i. !min = !max."
		assertEquals(TokenType.IDENTIFIER, Token.createForAbap(0, 1, "!min", 1).type);
		assertEquals(TokenType.IDENTIFIER, Token.createForAbap(0, 1, "!ls_any-comp", 1).type);
		assertEquals(TokenType.IDENTIFIER, Token.createForAbap(0, 1, "!any_method(", 1).type);
	}
	
	@Test
	void testAddNextErr() {
		Command command = buildCommand("a = 1.");
		Token newToken = Token.createForAbap(0, 1, "\" comment", TokenType.COMMENT, 1);

		// add null; expect NullPointerException
		try {
			command.lastToken.addNext(null);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		// add to a Token that has no parentCommand; expect NullPointerException
		Token tokenWithoutParentCommand = Token.createForAbap(0, 1, "a", TokenType.IDENTIFIER, 1);
		try {
			tokenWithoutParentCommand.addNext(newToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		// add to a Token that already has a next Token; expect NullPointerException
		try {
			command.firstToken.addNext(newToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		// add a Token that already has a previous Token; expect NullPointerException
		try {
			command.lastToken.addNext(command.lastToken.getPrev());
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		// add a ")" with no opening "("; expect UnexpectedSyntaxException
		try {
			command.lastToken.addNext(Token.createForAbap(0, 1, ")", TokenType.OTHER_OP, 1));
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		}
	}
	
	@Test
	void testColonInsideParens() {
		String codeText = "any_method( : 1 ), 2 ).";
		try {
			Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			fail();
		} catch(ParseException ex) {
			// expected case
			assertTrue(ex.getMessage().indexOf("Chain colons inside parentheses") >= 0);
			assertTrue(ex.getMessage().indexOf("not supported") >= 0);
			assertTrue(ex.getMessage().indexOf("Please rewrite") >= 0);
		}
	}
	
	@Test
	void testColonInsideParensWithoutComma() {
		// expect ( ... : ... ) to be accepted if there is no comma (i.e. a chain of one)
		boolean parseExceptionRaised = false;
		String codeText = "any_method( : 1 ).";
		try {
			Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
		} catch(ParseException ex) {
			parseExceptionRaised = true;
		}
		assertFalse(parseExceptionRaised);
	}
	
	@Test
	void testColonInsideBrackets() {
		String codeText = "SELECT \\_spfli[ (1) INNER WHERE :connid = @lc_id1 ]-connid AS connid FROM demo_cds_assoc_scarr AS scarr INTO @DATA(result1), connid = @lc_id2 ]-connid AS connid FROM demo_cds_assoc_scarr AS scarr INTO @DATA(result2).";
		try {
			Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			fail();
		} catch(ParseException ex) {
			// expected case
			assertTrue(ex.getMessage().indexOf("Chain colons inside parentheses") >= 0);
			assertTrue(ex.getMessage().indexOf("not supported") >= 0);
			assertTrue(ex.getMessage().indexOf("Please rewrite") >= 0);
		}
	}
	
	@Test
	void testColonInsideBracketsWithoutComma() {
		// expect [ ... : ... ] to be accepted if there is no comma (i.e. a chain of one)
		boolean parseExceptionRaised = false;
		String codeText = "SELECT \\_spfli[ (1) INNER WHERE :connid = @lc_id1 ]-connid AS connid FROM demo_cds_assoc_scarr AS scarr INTO @DATA(result1).";
		try {
			Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
		} catch(ParseException ex) {
			parseExceptionRaised = true;
		}
		assertFalse(parseExceptionRaised);
	}
	
	@Test
	void testGetLastTokenOfSequence() {
		Command command = buildCommand("a = 1 + 2.");
		
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, TokenSearch.MAX_ONE_NON_MATCHING_TOKEN, "1"));
		assertNotNull(command.firstToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "1"));
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, TokenSearch.MAX_ONE_NON_MATCHING_TOKEN));

		// search for literals
		assertNotNull(command.firstToken.getLastTokenOnSiblings(true, "a", "=", TokenSearch.ANY_LITERAL));
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, "a", TokenSearch.ANY_LITERAL));

		// search for identifiers or literals
		assertNotNull(command.firstToken.getLastTokenOnSiblings(true, TokenSearch.ANY_IDENTIFIER_OR_LITERAL, "=", TokenSearch.ANY_IDENTIFIER_OR_LITERAL));
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, "a", TokenSearch.ANY_IDENTIFIER_OR_LITERAL));
		
		// search for comparison operators
		command = buildCommand("a ?= 1 + 2.");
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, "a", TokenSearch.ANY_COMPARISON_OPERATOR));
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, "a", "?=", TokenSearch.ANY_COMPARISON_OPERATOR));
		command = buildCommand("IF a > 1.");
		assertNotNull(command.firstToken.getLastTokenOnSiblings(true, "IF", "a", TokenSearch.ANY_COMPARISON_OPERATOR));
		
		// search for comments
		command = buildCommand("a = 1 \" comment" + SEP + "* comment 2" + SEP + "  \" comment 3" + SEP + "  + 2.");
		assertNotNull(command.firstToken.getLastTokenOnSiblings(false, "a", "=", "1", TokenSearch.ANY_NUMBER_OF_COMMENTS, "+"));
		assertNotNull(command.firstToken.getLastTokenOnSiblings(false, "a", TokenSearch.ANY_NON_COMMENT, TokenSearch.ANY_NON_COMMENT, TokenSearch.ANY_NUMBER_OF_COMMENTS, "+"));
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(false, "a", "=", "1", TokenSearch.ANY_NON_COMMENT));

		// cause Token.getLastTokenOfPlainSequence to match beyond the end of the Command
		command = buildCommand("a = 1 .");
		assertEquals(null, command.firstToken.getLastTokenOnSiblings(true, "a", "=", "1 . 1|1 . 2"));
	}
	
	@Test
	void testRemoveFromCommandErr() {
		// try to remove "lts_table["; expect an error
		try {
			buildCommand("a = lts_table[ 1 ]", 2).removeFromCommand(false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}
		
		// try to remove the last Token; expect an error
		try {
			buildCommand(".", 0).removeFromCommand(false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}
		try {
			buildCommand("* comment", 0).removeFromCommand(false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}
	}
	
	@Test
	void testRemoveFromCommand() {
		try {
			buildCommand("  a = 1. \" comment").lastToken.removeFromCommand(true);
		} catch (UnexpectedSyntaxAfterChanges e) {
			fail();
		}
	}
	
	@Test
	void testInsertParenthesesUpToErr() {
		Token token3 = buildCommand("a = 2 * 3 + 5.", 4);
		Token tokenAfterParentheses = token3.getParentCommand().lastToken;
		
		try {
			token3.insertParenthesesUpTo(null);
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}

		try {
			token3.insertParenthesesUpTo(null, "(", ")");
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}

		try {
			token3.insertParenthesesUpTo(tokenAfterParentheses, null, ")");
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}

		try {
			token3.insertParenthesesUpTo(tokenAfterParentheses, "(", null);
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	@Test
	void testInsertParenthesesUpTo() {
		Token token3 = buildCommand("a = 2 * 3 + 5.", 4);
		Token tokenAfterParentheses = token3.getParentCommand().lastToken;
		
		try {
			token3.insertParenthesesUpTo(tokenAfterParentheses, "(", ")");
			assertEquals("a = 2 * ( 3 + 5 ).", token3.getParentCommand().toString());
		} catch (IntegrityBrokenException e) {
			fail();
		}
		
		// assume parentheses for new content shall be inserted
		token3 = buildCommand("a = 2 * 3 + 5.", 4);
		tokenAfterParentheses = token3.getParentCommand().lastToken;
		try {
			token3.insertParenthesesUpTo(token3, "(", ")");
			assertEquals("a = 2 * ( ) 3 + 5.", token3.getParentCommand().toString());
		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	@Test
	void testRemoveParenthesesErr() {
		// test removing of a non-parentheses; expect an exception
		try {
			buildCommand("a = 1.", 0).removeParentheses();
			fail();
		} catch (IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			// expected case
		}

		// test removing of empty parentheses, i.e. the inner "( )"; expect an exception
		try {
			buildCommand("a = VALUE #( ( ) ).", 4).removeParentheses();
			fail();
		} catch (IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			// expected case
		}
	}
	
	@Test
	void testRemoveParentheses() {
		try {
			Token openingParenthesis = buildCommand("a = 2 * ( 3 + 4 ).", 4);
			Command command = openingParenthesis.getParentCommand();
			openingParenthesis.removeParentheses();
			assertEquals("a = 2 * 3 + 4.", command.toString());
		} catch (IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
		
		try {
			Token openingParenthesis = buildCommand("a = 2 * ( 3 + 4 ) * 5.", 4);
			Command command = openingParenthesis.getParentCommand();
			openingParenthesis.removeParentheses();
			assertEquals("a = 2 * 3 + 4 * 5.", command.toString());
		} catch (IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
		
		// ensure that the period after the parenthesis is NOT attached to the comment
		try {
			Token openingParenthesis = buildCommand("a = 2 * ( 3 + 4 \" comment" + SEP + "  ).", 4);
			Command command = openingParenthesis.getParentCommand();
			openingParenthesis.removeParentheses();
			assertEquals("a = 2 * 3 + 4 \" comment" + SEP + "  .", command.toString());
		} catch (IntegrityBrokenException e) {
			fail();
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
	}
	
	@Test
	void testAppendParenthesesUpToErr() {
		Token methodNameToken = buildCommand("CALL METHOD any_method EXPORTING a = 1.", 2);
		
		try {
			methodNameToken.appendParenthesesUpTo(null, false);
			fail();
		} catch (NullPointerException e) {
			// expected case
		} catch (UnexpectedSyntaxAfterChanges e) {
			fail();
		}

		// expect an exception if the Token already has children
		try {
			Token token = buildCommand("any_method( a = 1 ).", 0); 
			token.appendParenthesesUpTo(token.getNextSibling(), false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}

		// expect an exception if the Token already opens a level
		try {
			Token token = buildCommand("a = lts_table[ 1 ].", 2); 
			token.appendParenthesesUpTo(token.getNextSibling(), false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}

		// expect an exception if the Token is the last in the command
		try {
			Token token = buildCommand("a = 1.", 3); 
			token.appendParenthesesUpTo(token.getPrev(), false);
			fail();
		} catch (UnexpectedSyntaxAfterChanges e) {
			// expected case
		}
	}
	
	void assertIntegrityBroken(Token token) {
		try {
			token.testReferentialIntegrity(true);
			fail();
		} catch(IntegrityBrokenException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	void assertIntegrityBroken(Token token1, Token token2) {
		assertIntegrityBroken(token1);
		assertIntegrityBroken(token2);
	}
	
	@Test
	void testReferentialIntegrity() {
		final String code = "any_method( param = b ).";
		
		buildCommand(code);
		tokens[0].setNext(null);
		assertIntegrityBroken(tokens[0], tokens[1]);
		
		buildCommand(code);
		tokens[1].setPrev(null);
		assertIntegrityBroken(tokens[0]);
		
		buildCommand(code);
		tokens[0].setNextSibling(null);
		assertIntegrityBroken(tokens[0], tokens[4]);
		
		buildCommand(code);
		tokens[0].setNextSibling(tokens[1]);
		assertIntegrityBroken(tokens[0]);
		
		buildCommand(code);
		tokens[4].setPrevSibling(null);
		assertIntegrityBroken(tokens[0], tokens[4]);
		
		buildCommand(code);
		tokens[4].setPrevSibling(tokens[1]);
		tokens[1].setNextSibling(tokens[4]);
		assertIntegrityBroken(tokens[4]);
		
		buildCommand(code);
		tokens[0].setFirstChild(null);
		assertIntegrityBroken(tokens[0]);
		
		buildCommand(code);
		tokens[1].setFirstChild(tokens[4]);
		assertIntegrityBroken(tokens[1]);
		
		buildCommand(code);
		tokens[0].setLastChild(null);
		assertIntegrityBroken(tokens[0]);
		
		buildCommand(code);
		tokens[0].setLastChild(tokens[4]);
		assertIntegrityBroken(tokens[0]);
		
		buildCommand("  CLEAR a \" comment" + SEP + "  .");
		tokens[3].lineBreaks = 0;
		assertIntegrityBroken(tokens[3]);

		Token tokenWithoutCommand = Token.createForAbap(0, 1, "\" comment", TokenType.COMMENT, 1);
		assertIntegrityBroken(tokenWithoutCommand);
	}

	@Test
	void testGetStructureVariable() {
		buildCommand("struct-comp = b.");
		
		assertEquals("struct", tokens[0].getStructureVariable());
		assertEquals(null, tokens[1].getStructureVariable());
		assertEquals(null, tokens[2].getStructureVariable());
	}
	
	@Test
	void testSetTextNull() {
		Token token = buildCommand("a = 1.", 0);
		
		try {
			token.setText(null, false);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}
	}
	
	@Test
	void testSetText() {
		Command command = buildCommand("a = 1.");
		
		command.lastToken.setText(",", true);
		assertEquals("a = 1,", command.toString());
	}
	
	@Test
	void testGetStartOfAttachedComments() {
		Command command = buildCommand("any_method(" + SEP + "* comment 1" + SEP + SEP + "* comment 2" + SEP + "  \" comment 3" + SEP + "* comment 4" + SEP + "  ).");
		Token firstAttachedComment = command.lastToken.getPrev().getStartOfAttachedComments();
		assertNotNull(firstAttachedComment);
		assertEquals("* comment 2", firstAttachedComment.text);

		command = buildCommand("any_method(" + SEP + "* comment 1" + SEP + "* comment 2" + SEP + "  \" comment 3" + SEP + "* comment 4" + SEP + "  ).");
		firstAttachedComment = command.lastToken.getPrev().getStartOfAttachedComments();
		assertNotNull(firstAttachedComment);
		assertEquals("* comment 1", firstAttachedComment.text);

		command = buildCommand(SEP + "any_method( ).");
		firstAttachedComment = command.firstToken.getStartOfAttachedComments();
		assertNotNull(firstAttachedComment);
		assertEquals("any_method(", firstAttachedComment.text);
	}
	
	@Test
	void testCopyWhitespaceFromErr() {
		Token token = buildCommand("a = 1.", 0);
		try {
			token.copyWhitespaceFrom(null);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		}
	}
	
	@Test
	void testInsertRightSiblingNull() {
		Token token = buildCommand("a = 1.", 0);
		Token nullToken = null;
		try {
			token.insertRightSibling(nullToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}

		token = buildCommand("a = 1.", 0);
		Term nullTerm = null;
		try {
			token.insertRightSibling(nullTerm);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	@Test
	void testInsertLeftSiblingNull() {
		Token token = buildCommand("a = 1.", 0);
		Token nullToken = null;
		try {
			token.insertLeftSibling(nullToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}

		token = buildCommand("a = 1.", 0);
		Term nullTerm = null;
		try {
			token.insertLeftSibling(nullTerm);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (IntegrityBrokenException e) {
			fail();
		}
	}
	
	@Test
	void testEnsureWhitespace() {
		Token token = buildCommand("a ='3.14'.", 2);
		token.ensureWhitespace();
		assertEquals(1, token.spacesLeft);
		assertEquals(0, token.lineBreaks);

		token = buildCommand("a =  1.", 2);
		token.ensureWhitespace();
		assertEquals(2, token.spacesLeft);
		assertEquals(0, token.lineBreaks);

		token = buildCommand("a =" + SEP + "1.", 2);
		token.ensureWhitespace();
		assertEquals(0, token.spacesLeft);
		assertEquals(1, token.lineBreaks);
	}
	
	@Test
	void testFindEndOfLogExprNull() {
		Token nullToken = null;
		try {
			Token.findEndOfLogicalExpression(nullToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
	}

	@Test
	void testFindEndOfRelExprNull() {
		Token nullToken = null;
		try {
			Token.findEndOfRelationalExpression(nullToken);
			fail();
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
	}

	void testFindEndOfLogExpr(String commandText) {
		Token token = buildCommand(commandText, 1);
		try {
			Token endToken = Token.findEndOfLogicalExpression(token);
			assertTrue(endToken.isPeriod());
		} catch (UnexpectedSyntaxException e) {
			fail();
		}
	}
	
	@Test
	void testFindEndOfLogExpr() {
		testFindEndOfLogExpr("IF a < b.");
		testFindEndOfLogExpr("IF a IS INITIAL AND NOT b IS INITIAL.");
		testFindEndOfLogExpr("IF <a> IS ASSIGNED OR lo_any IS NOT BOUND OR iv_any IS SUPPLIED.");
		testFindEndOfLogExpr("IF lo_obj IS INSTANCE OF cl_any AND 1 + 2 * ( 3 + 4 ) = 5.");
		testFindEndOfLogExpr("WHILE a > b AND a LE c.");
		testFindEndOfLogExpr("WHILE a > b AND ( b = 1 OR b = 2 ).");
		testFindEndOfLogExpr("WHILE a BETWEEN b AND c.");
		testFindEndOfLogExpr("WHILE a + 1 < b * ( 3 + 4 ) OR iv_any IS SUPPLIED AND c BETWEEN 3 AND 5.");
		testFindEndOfLogExpr("WHILE line_exists( lts_table[ 1 ] ).");
		testFindEndOfLogExpr("ASSERT matches( val = 'abcde' pcre = '[[:alpha:]]*' )."); 
		testFindEndOfLogExpr("ASSERT a in tab1 AND b in tab2."); 
	}

	void testFindEndOfLogExprError(String commandText, String expectedMessage) {
		Token token = buildCommand(commandText, 1);
		try {
			Token.findEndOfLogicalExpression(token);
			fail();
		} catch (UnexpectedSyntaxException e) {
			assertTrue(e.getMessage().indexOf(expectedMessage) >= 0);
		}
	}

	@Test
	void testFindEndOfLogExprError() {
		testFindEndOfLogExprError("IF a IS INITIAL AND NOT b IS 5.", "predicate expression");
		testFindEndOfLogExprError("WHILE a BETWEEN b OR c.", "AND");
	}
	
	@Test
	void testKeywordWithHyphenIsKeyword() {
		// ensure that ABAP keywords with hyphens such as BREAK-POINT, FIELD-SYMBOLS, MOVE-CORRESPONDING, FIELD-SYMBOL(,  
		// CLASS-DATA, and READ-ONLY are correctly classified as keywords (RND Parser classifies 'FIELD' in 'FIELD-SYMBOL('  
		// as being an identifier and 'suspicious', which should be corrected in TokenTypeRefinerRnd.refine())
		assertTrue(buildCommand("BREAK-POINT.", 0).isKeyword());
		assertTrue(buildCommand("FIELD-SYMBOLS <a> TYPE any.", 0).isKeyword());
		assertTrue(buildCommand("MOVE-CORRESPONDING ls_source TO ls_dest.", 0).isKeyword());
		assertTrue(buildCommand("ASSIGN lt_any[ 1 ] TO FIELD-SYMBOL(<ls_any>).", 5).isKeyword());
		assertTrue(buildCommand("CLASS-DATA mo_any TYPE REF TO cl_any READ-ONLY.", 0).isKeyword());
		assertTrue(buildCommand("CLASS-DATA mo_any TYPE REF TO cl_any READ-ONLY.", 6).isKeyword());
	}
	
	private void assertMemoryAccessType(MemoryAccessType expAccessType, Command command, ArrayList<String> tokenTexts) {
		for (String tokenText : tokenTexts) {
			Token token = command.getFirstToken().getLastTokenOfSequence(false, false, null, TokenSearch.ASTERISK, tokenText);
			assertNotNull(token);
			assertFalse(token.isKeyword());
			if (token != null) {
				MemoryAccessType actAccessType = token.getMemoryAccessType();
				if (expAccessType == MemoryAccessType.READ) {
					// allow for both READ and READ_OR_NONE
					assertTrue(actAccessType == MemoryAccessType.READ || actAccessType == MemoryAccessType.READ_OR_NONE, "unexpected MemoryAccessType for " + tokenText);
				} else if (expAccessType == MemoryAccessType.READ_WRITE) {
					// allow for both READ_WRITE and READ_WRITE_POSSIBLE
					assertTrue(actAccessType == MemoryAccessType.READ_WRITE || actAccessType == MemoryAccessType.READ_WRITE_POSSIBLE, "unexpected MemoryAccessType for " + tokenText);
				} else {

					assertEquals(expAccessType, actAccessType, "unexpected MemoryAccessType for " + tokenText);
				}
			}
		}
	}

	/**
	 * Checks the memory access type that is determined by {@link Token#getMemoryAccessType()} for the tokens that are marked with ? ! # the supplied command text.
	 * 
	 * @param commandTextWithMarkers <p>The ABAP code of a single Command with the following markers:</p>
	 * <ul><li>? preceding a variable name in a read position</li>
	 * <li>! preceding a variable name in a write position</li>
	 * <li># preceding a variable name in a read-write position</li></ul>
	 * <li>% preceding a field-symbol in an assignment position</li></ul>
	 * <p>For this test, variable names must not match keywords within the same command (non-case-sensitive comparison).</p>
	 */
	private void assertAccessType(String commandTextWithMarkers) {
		final char READ_MARKER = '?';
		final char WRITE_MARKER = '!';
		final char READ_WRITE_MARKER = '#';
		final char ASSIGN_TO_FIELD_SYMBOL_MARKER = '%';
		
		char[] markers = new char[] { READ_MARKER, WRITE_MARKER, READ_WRITE_MARKER, ASSIGN_TO_FIELD_SYMBOL_MARKER };
		
		ArrayList<String> expRead = new ArrayList<String>(); 
		ArrayList<String> expWrite = new ArrayList<String>(); 
		ArrayList<String> expReadWrite = new ArrayList<String>();
		ArrayList<String> expAssign = new ArrayList<String>();
		int markerPos = 0;
		int writePos = 0;
		StringBuilder commandText = new StringBuilder();
		while (writePos < commandTextWithMarkers.length()) {
			markerPos = StringUtil.indexOfAny(commandTextWithMarkers, markers, writePos);
			if (markerPos < 0) {
				commandText.append(commandTextWithMarkers.substring(writePos));
				break;
			}
			commandText.append(commandTextWithMarkers.substring(writePos, markerPos));
			writePos = markerPos + 1;
			String varName = ABAP.readTillEndOfVariableName(commandTextWithMarkers, writePos, true, true);
			if (StringUtil.isNullOrEmpty(varName)) {
				// the marker is not meant as a marker, e.g. in 'VALUE #( ... )'
				commandText.append(commandTextWithMarkers.charAt(markerPos));
				continue;
			}
			char nextChar = commandTextWithMarkers.charAt(writePos + varName.length());
			if (nextChar == '(' || nextChar == '[') {
				varName += nextChar;
			}
			switch(commandTextWithMarkers.charAt(markerPos)) {
				case READ_MARKER:
					expRead.add(varName);
					break;
				case WRITE_MARKER:
					expWrite.add(varName);
					break;
				case READ_WRITE_MARKER:
					expReadWrite.add(varName);
					break;
				case ASSIGN_TO_FIELD_SYMBOL_MARKER:
					expAssign.add(varName);
					break;
			}
		}
		Command command = buildCommand(commandText.toString());

		assertTrue(expRead.size() + expWrite.size() + expReadWrite.size() + expAssign.size() > 0);
		assertMemoryAccessType(MemoryAccessType.READ, command, expRead);
		assertMemoryAccessType(MemoryAccessType.WRITE, command, expWrite);
		assertMemoryAccessType(MemoryAccessType.READ_WRITE, command, expReadWrite);
		assertMemoryAccessType(MemoryAccessType.ASSIGN_TO_FS_OR_DREF, command, expAssign);
	}
	
	@Test
	void testAccessTypeInlineDeclaration() {
		assertAccessType("DATA(!a) = 1.");
		assertAccessType("FINAL(!a) = get_value( ).");
		assertAccessType("DATA(%dref) = REF any_type( dobj ).");
		assertAccessType("FINAL(%dref) = REF any_type( itab[ 1 ]-comp ).");
		assertAccessType("LOOP AT lt_data INTO DATA(!ls_data).");
		assertAccessType("LOOP AT lt_data REFERENCE INTO DATA(%lr_data).");
	}
	
	@Test
	void testAccessTypeDeclaration() {
		assertAccessType("DATA: !a TYPE i, !b TYPE string.");
		assertAccessType("CONSTANTS: !a TYPE i VALUE 1, !b TYPE char3 VALUE 'abc'.");
		assertAccessType("CLASS-DATA !a TYPE i.");
	}
	
	@Test
	void testAccessTypeAssignmentToFieldSymbol() {
		assertAccessType("ASSIGN lt_data[ 1 ] TO FIELD-SYMBOL(%<ls_data>).");
		assertAccessType("ASSIGN lt_data[ 1 ] TO %<ls_data>.");
		assertAccessType("UNASSIGN %<ls_data>.");
		assertAccessType("UNASSIGN: %<ls_any>, %<ls_other>.");
	}
	
	@Test
	void testAccessTypeGetReferenceInto() {
		assertAccessType("GET REFERENCE OF lt_data INTO %dref.");
		assertAccessType("GET REFERENCE OF lt_data INTO DATA(%dref).");
	}
	
	@Test
	void testAccessTypeAssignmentToVariable() {
		assertAccessType("!a = 1.");
		assertAccessType("!a = !b = !c = ?d.");
		assertAccessType("!ls_struc = get_value( param = ?lv_value ).");
	}
	
	@Test
	void testAccessTypeAssignmentToTableExpr() {
		assertAccessType("!lt_any[ 1 ] = ?ls_any.");
		assertAccessType("!lt_any[ 1 ]-comp = ?ls_other[ 1 ]-comp.");
		assertAccessType("!lt_any[ 1 ]-inner[ 2 ]-comp = 1.");
		assertAccessType("!lt_any[ 1 ]-inner[ 2 ] = ?lt_other[ 1 ]-inner[ 3 ].");
	}
	
	@Test
	void testAccessTypeAssignmentToDataRef() {
		assertAccessType("%dref = REF any_type( dobj ).");
		assertAccessType("%dref = REF any_type( itab[ 1 ]-comp ).");
	}
	
	@Test
	void testAccessTypeObjectCreation() {
		assertAccessType("CREATE DATA !dref.");
		assertAccessType("CREATE OBJECT !oref.");
		assertAccessType("CREATE OBJECT !ole 'class' NO FLUSH QUEUE-ONLY.");
	}
	
	@Test
	void testAccessTypeExceptionHandling() {
		assertAccessType("CATCH BEFORE UNWIND cx_class1 cx_class2 INTO !oref.");
		assertAccessType("CLEANUP INTO !oref.");
	}
	
	@Test
	void testAccessTypeAssignments() {
		assertAccessType("MOVE-CORRESPONDING EXACT ?struc1 TO !struc2 EXPANDING NESTED TABLES KEEPING TARGET LINES.");
		assertAccessType("MOVE-CORRESPONDING ?itab1 TO !itab2 KEEPING TARGET LINES.");
		assertAccessType("UNPACK ?source TO !destination.");
		assertAccessType("GET REFERENCE OF ?dobj INTO %dref.");
		assertAccessType("CLEAR !dobj WITH ?val IN CHARACTER MODE.");
		assertAccessType("CLEAR: !dobj1, !dobj2.");
		assertAccessType("FREE !dobj.");
		assertAccessType("FREE: !dobj1, !dobj2.");
	}

	@Test
	void testAccessTypeStringProcessing() {
		assertAccessType("CONCATENATE ?dobj1 ?dobj2 INTO !result IN CHARACTER MODE SEPARATED BY ?sep RESPECTING BLANKS.");
		assertAccessType("CONDENSE !text NO-GAPS.");
		assertAccessType("CONVERT TEXT ?txt INTO SORTABLE CODE !hex.");
		assertAccessType("FIND FIRST OCCURRENCE OF ?substring IN SECTION OFFSET ?off LENGTH ?len OF ?dobj MATCH COUNT !mcnt, MATCH OFFSET !moff, MATCH LENGTH !mlen, RESULTS !result_wa SUBMATCHES !s1 FINAL(s2) !s3.");
		assertAccessType("GET BIT ?bitpos OF ?byte_string INTO !val.");
		assertAccessType("OVERLAY !text1 WITH ?text2 ONLY mask.");
		assertAccessType("REPLACE ALL OCCURRENCES OF ?pattern IN SECTION OFFSET ?off LENGTH ?len OF !dobj WITH ?new IN BYTE MODE REPLACEMENT COUNT !rcnt, REPLACEMENT OFFSET !roff, REPLACEMENT LENGTH !rlen RESULTS !result_tab.");
		assertAccessType("REPLACE ALL OCCURRENCES OF ?pattern IN !dobj WITH ?new IN BYTE MODE REPLACEMENT COUNT !rcnt, REPLACEMENT OFFSET !roff, REPLACEMENT LENGTH !rlen RESULTS !result_tab.");
		assertAccessType("SET BIT ?bitpos OF !byte_string TO ?val.");
		assertAccessType("SHIFT !dobj BY ?num PLACES LEFT DELETING LEADING ?mask IN CHARACTER MODE.");
		assertAccessType("SPLIT ?dobj AT ?sep INTO TABLE !result_tab.");
		assertAccessType("SPLIT ?dobj AT ?sep INTO !result1 FINAL(result2) !result3 IN BYTE MODE.");
		assertAccessType("TRANSLATE !text TO UPPER CASE.");
		assertAccessType("TRANSLATE !text USING ?mask.");
		assertAccessType("WRITE ?source TO !destination TIME ZONE ?tz DECIMALS ?dec.");
	}

	@Test
	void testAccessTypeDateAndTimeProcessing() {
		assertAccessType("CONVERT DATE ?dat TIME ?tim FRACTIONAL SECONDS ?fs DAYLIGHT SAVING TIME ?dst TIME ZONE ?tz INTO UTCLONG !time_stamp.");
		assertAccessType("CONVERT DATE ?dat TIME ?tim DAYLIGHT SAVING TIME ?dst INTO TIME STAMP !time_stamp TIME ZONE !tz.");
		assertAccessType("CONVERT UTCLONG ?time_stamp INTO DATE !dat TIME !tim FRACTIONAL SECONDS !fs DAYLIGHT SAVING TIME !dst TIME ZONE !tz.");
		assertAccessType("CONVERT TIME STAMP ?time_stamp TIME ZONE ?tz INTO DATE !dat TIME !tim DAYLIGHT SAVING TIME !dst.");
		assertAccessType("GET TIME FIELD !tim.");
		assertAccessType("GET TIME STAMP FIELD !time_stamp.");
	}

	@Test
	void testAccessTypeInternalTables() {
		assertAccessType("APPEND LINES OF ?jtab FROM ?idx1 TO ?idx2 STEP ?n TO !itab SORTED BY ?comp REFERENCE INTO %dref.");
		assertAccessType("APPEND wa TO !itab SORTED BY ?comp REFERENCE INTO %dref.");
		assertAccessType("APPEND wa TO !itab SORTED BY ?comp REFERENCE INTO DATA(%dref).");
		assertAccessType("APPEND wa TO !itab SORTED BY ?comp REFERENCE INTO FINAL(%dref).");
		assertAccessType("COLLECT ?wa INTO !itab ASSIGNING %<fs> CASTING.");
		assertAccessType("COLLECT ?wa INTO !itab REFERENCE INTO %dref.");
		assertAccessType("COLLECT ?wa INTO !itab REFERENCE INTO DATA(%dref).");
		assertAccessType("COLLECT ?wa INTO !itab REFERENCE INTO FINAL(%dref).");
		assertAccessType("DELETE TABLE !itab FROM ?wa.");
		assertAccessType("DELETE !itab USING KEY ?keyname FROM ?idx1 TO ?idx2 STEP ?n.");
		assertAccessType("DELETE ADJACENT DUPLICATES FROM !itab COMPARING (?name1) (?name2).");
		assertAccessType("DELETE dtab FROM TABLE ?itab.");
		assertAccessType("INSERT LINES OF ?jtab FROM ?idx1 TO ?idx2 STEP ?n INTO TABLE !itab.");
		assertAccessType("INSERT LINES OF ?jtab FROM ?idx1 TO ?idx2 STEP ?n INTO !itab INDEX ?idx.");
		assertAccessType("INSERT INITIAL LINE INTO TABLE !itab REFERENCE INTO %dref.");
		assertAccessType("INSERT ?wa INTO !itab INDEX ?idx ASSIGNING %<fs> CASTING ELSE UNASSIGN.");
		assertAccessType("INSERT dtab FROM TABLE ?itab.");
		assertAccessType("LOOP AT ?itab INTO !wa.");
		assertAccessType("LOOP AT ?itab ASSIGNING %<fs> CASTING.");
		assertAccessType("LOOP AT ?itab REFERENCE INTO %dref.");
		assertAccessType("MODIFY TABLE !itab FROM ?wa REFERENCE INTO %dref.");
		assertAccessType("MODIFY !itab FROM ?wa TRANSPORTING ?comp1 ?comp2 WHERE fld IS NOT INITIAL.");
		assertAccessType("MODIFY dtab FROM TABLE ?itab.");
		assertAccessType("READ TABLE ?itab INTO !wa.");
		assertAccessType("READ TABLE ?itab ASSIGNING %<fs>.");
		assertAccessType("READ TABLE ?itab ASSIGNING FIELD-SYMBOL(%<fs>).");
		assertAccessType("READ TABLE ?itab REFERENCE INTO %dref.");
		assertAccessType("READ TABLE ?itab REFERENCE INTO FINAL(%dref).");
		assertAccessType("REPLACE FIRST OCCURRENCE OF ?pattern IN TABLE !itab.");
		assertAccessType("SORT !itab BY comp1 ASCENDING AS TEXT comp2 DESCENDING.");
		assertAccessType("DESCRIBE FIELD ?dobj TYPE !typ COMPONENTS !com LENGTH !ilen IN CHARACTER MODE DECIMALS !dec OUTPUT-LENGTH !olen HELP-ID !hlp EDIT MASK !msk.");
		assertAccessType("DESCRIBE TABLE ?itab KIND !knd LINES !lin OCCURS !n.");
		assertAccessType("DESCRIBE DISTANCE BETWEEN ?dobj1 AND ?dobj2 INTO !dst IN BYTE MODE.");
		assertAccessType("DESCRIBE FIELD ?dobj INTO !td.");
	}

	@Test
	void testAccessTypeAbapSql() {
		assertAccessType("CLOSE CURSOR !dbcur.");
		assertAccessType("FETCH NEXT CURSOR ?dbcur INTO CORRESPONDING FIELDS OF !wa.");
		assertAccessType("FETCH NEXT CURSOR ?dbcur APPENDING CORRESPONDING FIELDS OF TABLE !itab PACKAGE SIZE ?n EXTENDED RESULT !oref.");
		assertAccessType("OPEN CURSOR WITH HOLD !dbcur FOR SELECT * FROM dtab WHERE fld = ?value.");
		assertAccessType("SELECT * INTO CORRESPONDING FIELDS OF !wa.");
		assertAccessType("SELECT * INTO CORRESPONDING FIELDS OF TABLE !itab PACKAGE SIZE ?n EXTENDED RESULT !oref.");

		assertAccessType("SELECT * INTO CORRESPONDING FIELDS OF TABLE NEW !dref.");
		assertAccessType("SELECT * APPENDING TABLE NEW !dref.");
		assertAccessType("SELECT * INTO NEW !dref.");
		assertAccessType("SELECT * INTO ( !elem1, NEW !elem2 ).");
	}

	@Test
	void testAccessTypeDataClusters() {
		assertAccessType("IMPORT p1 = !dobj1 p2 = !dobj2 FROM medium.");
		assertAccessType("IMPORT p1 TO !dobj1 p2 TO !dobj2 FROM medium.");
		assertAccessType("IMPORT DIRECTORY INTO !itab FROM DATABASE dbtab(ar) TO !wa CLIENT ?cl ID ?id1.");
	}

	@Test
	void testAccessTypeFileInterface() {
		assertAccessType("GET DATASET ?dset POSITION !pos ATTRIBUTES !attr.");
		assertAccessType("READ DATASET ?dset INTO !dobj MAXIMUM LENGTH !mlen ACTUAL LENGTH !alen.");
	}

	@Test
	void testAccessTypeRapBusinessObjects() {
		assertAccessType("COMMIT ENTITIES RESPONSES FAILED !failed_resp REPORTED !reported_resp.");
		assertAccessType("GET PERMISSIONS ENTITY bdef FROM keys REQUEST request RESULT !result_tab FAILED !failed_resp REPORTED !reported_resp.");
		assertAccessType("MODIFY ENTITY bdef RESULT !result_tab FAILED !failed_resp MAPPED !mapped_resp REPORTED !reported_resp.");
		assertAccessType("READ ENTITY bdef RESULT !result_tab FAILED !failed_resp REPORTED !reported_resp.");
		assertAccessType("SET LOCKS ENTITY bdef FROM inst FAILED !failed_resp REPORTED !reported_resp.");
	}

	@Test
	void testAccessTypeProgramParameters() {
		assertAccessType("GET PARAMETER ID ?pid FIELD !dobj.");
		assertAccessType("GET LOCALE LANGUAGE !lang COUNTRY !cntry MODIFIER !mod.");
	}

	@Test
	void testAccessTypeProgramEditing() {
		assertAccessType("GET RUN TIME FIELD !rtime.");
		assertAccessType("READ REPORT ?prog INTO !itab MAXIMUM WIDTH INTO !wid.");
		assertAccessType("READ TEXTPOOL ?prog INTO !itab LANGUAGE ?lang.");
		assertAccessType("SYNTAX-CHECK FOR ?itab MESSAGE !mess LINE !lin WORD !wrd PROGRAM ?prog DIRECTORY ENTRY ?dir WITH CURRENT SWITCHSTATES INCLUDE !incl OFFSET !off MESSAGE-ID !mid.");
	}

	@Test
	void testAccessTypeOleInterface() {
		assertAccessType("CALL METHOD OF ?ole 'meth' = !rc EXPORTING p1 = ?f1 p2 = FINAL(f2) p3 = ?f3 NO FLUSH QUEUE-ONLY.");
		assertAccessType("GET PROPERTY OF ?ole 'prop' = !dobj NO FLUSH QUEUE-ONLY EXPORTING p1 = ?f1 p2 = FINAL(f2) p3 = ?f3.");
	}

	@Test
	void testAccessTypeDynpros() {
		assertAccessType("GET CURSOR FIELD !fld VALUE !val LENGTH !len OFFSET !off LINE !lin AREA !ar.");
		assertAccessType("GET PF-STATUS !status PROGRAM !prog EXCLUDING !fcode.");
		assertAccessType("LOOP AT SCREEN INTO !wa.");
		assertAccessType("REFRESH CONTROL !contrl FROM SCREEN ?dynnr.");
	}

	@Test
	void testAccessTypeUserDialogLists() {
		assertAccessType("DESCRIBE LIST NUMBER OF LINES !n.");
		assertAccessType("DESCRIBE LIST NUMBER OF PAGES !n.");
		assertAccessType("DESCRIBE LIST LINE !linno PAGE !pg.");
		assertAccessType("DESCRIBE LIST PAGE !pagno LINE-SIZE !width LINE-COUNT !page_lines LINES !lines1 FIRST-LINE !first_line TOP-LINES !top_lines TITLE-LINES !title_lines HEAD-LINES !header_lines END-LINES !footer_lines INDEX !idx.");
		assertAccessType("READ LINE ?lin OF PAGE ?pg INDEX ?idx LINE VALUE INTO !wa FIELD VALUE ?dobj1 INTO !wa1 ?dobj2 INTO FINAL(wa2) ?dobj3 INTO !wa3.");
	}

	@Test
	void testAccessTypeUserDialogMessages() {
		assertAccessType("MESSAGE ?txt TYPE ?mtype INTO !text.");
	}

	@Test
	void testAccessTypeEnhancements() {
		assertAccessType("GET BADI !badi_ FILTERS f1 = ?x1 f2 = ?x2.");
	}

	@Test
	void testAccessTypeObsoleteAssignments() {
		assertAccessType("MOVE EXACT ?source TO !destination.");
		assertAccessType("MOVE ?source TO: !destination1, !destination2, !destination3.");
		assertAccessType("PACK ?source TO !destination.");
	}

	@Test
	void testAccessTypeObsoleteCalculationStatements() {
		assertAccessType("COMPUTE EXACT !lhs = ?rhs.");
		assertAccessType("ADD ?dobj1 TO !dobj2.");
		assertAccessType("ADD-CORRESPONDING ?struc1 TO !struc2.");
		assertAccessType("SUBTRACT ?dobj1 FROM !dobj2.");
		assertAccessType("SUBTRACT-CORRESPONDING ?struc1 FROM !struc2.");
		assertAccessType("MULTIPLY !dobj2 BY ?dobj1.");
		assertAccessType("DIVIDE !dobj2 BY ?dobj1.");
		assertAccessType("MULTIPLY-CORRESPONDING !struc1 BY ?struc2.");
		assertAccessType("DIVIDE-CORRESPONDING !struc1 BY ?struc2.");
	}

	@Test
	void testAccessTypeCallMethodFunctionOrDbProc() {
		assertAccessType("CALL METHOD any_method( EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = !pi2 CHANGING c1 = #pc1 c2 = #pc2 RECEIVING r1 = !pr1 EXCEPTIONS exc1 = ?px1 exc2 = ?px2 ).");
		assertAccessType("CALL METHOD oref->(methodname) IMPORTING i1 = !pi1 i2 = !pi2 CHANGING c1 = #pc1 c2 = #pc2.");
		assertAccessType("CALL METHOD (methodname) EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = DATA(pi2) i3 = !pi3 CHANGING c1 = #pc1 c2 = #pc2 RECEIVING r1 = !pr1.");

		assertAccessType("CALL FUNCTION func EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = !pi2 CHANGING c1 = #pc1 c2 = #pc2 EXCEPTIONS exc1 = ?px1 exc2 = ?px2.");
		assertAccessType("CALL FUNCTION func IMPORTING i1 = !pi1 i2 = !pi2 TABLES t1 = #pt1 CHANGING c1 = #pc1 c2 = #pc2.");
		assertAccessType("CALL FUNCTION func EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = DATA(pi2) i3 = !pi3 CHANGING c1 = #pc1 c2 = #pc2.");

		assertAccessType("CALL DATABASE PROCEDURE proxy EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = !pi2.");
		assertAccessType("CALL DATABASE PROCEDURE proxy EXPORTING e1 = ?pe1 IMPORTING i1 = !pi1.");
	}	

	@Test
	void testAccessTypeReceiveAndPerform() {
		assertAccessType("RECEIVE RESULTS FROM FUNCTION func KEEPING TASK IMPORTING i1 = !pi1 i2 = !pi2 TABLES t1 = #itab1 t2 = #itab2 CHANGING c1 = #pc1 c2 = #pc2 EXCEPTIONS exc1 = ?px1 exc2 = ?px2.");
		
		// ensure that all actual parameters are treated as READ_WRITE or READ_WRITE_POSSIBLE, because the syntax check does not prevent changing a USING parameter 
		assertAccessType("PERFORM subr IN PROGRAM prog IF FOUND TABLES #itab1 #itab2 USING #u1 #u2 CHANGING #c1 #c2.");
	}

	@Test
	void testAccessTypeFunctionalCall() {
		assertAccessType("any_method( EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = !pi2 CHANGING c1 = #pc1 c2 = #pc2 RECEIVING r1 = !pr1 ).");
		assertAccessType("!pr1 = any_method( EXPORTING e1 = ?pe1 e2 = ?pe2 IMPORTING i1 = !pi1 i2 = !pi2 CHANGING c1 = #pc1 c2 = #pc2 ).");
		assertAccessType("!pr1 = any_method( e1 = ?pe1 e2 = ?pe2 ).");
		assertAccessType("any_method( e1 = any_inner_method( IMPORTING i1 = !pi1 i2 = !pi2 ) e1 = other_inner_method( e1 = ?pe1 e2 = ?pe2 ) e3 = third_inner_method( CHANGING c1 = #pc1 c2 = #pc2 ) ).");
		assertAccessType("!lv_result = xsdbool( ?lv_any = ?lv_other ).");
	}

	@Test
	void testAccessTypeTableExpression() {
		assertAccessType("!lt_table[ ?lv_index ]-comp = ?lv_value.");
		assertAccessType("!lt_table[ id = ?lv_id name = ?lv_name ]-comp = ?lv_value.");
		assertAccessType("CLEAR !lt_table[ ?lv_index ].");
		assertAccessType("CLEAR !lt_table[ ?lv_index ]-comp.");
		assertAccessType("CLEAR !lt_table[ id = ?lv_id name = ?lv_name ].");
		assertAccessType("CLEAR !lt_table[ id = ?lv_id name = ?lv_name ]-comp.");
		assertAccessType("CLEAR: !lt_any_table[ ?lv_index ], !lt_other_table[ id = ?lv_id name = ?lv_name ]-comp, lt_third_table[ get_index( ?iv_any ) ].");
	}

	@Test
	void testAccessTypeFuncInsideTableExpr() {
		assertAccessType("CLEAR !lt_table[ get_index( ?lv_any ) ].");
		assertAccessType("CLEAR !lt_table[ get_index( id = ?lv_id name = ?lv_name ) ].");
		assertAccessType("CLEAR !lt_table[ get_index( EXPORTING iv_any = ?lv_any IMPORTING ev_other = !lv_other ) ].");
	}
	
	@Test
	void testAccessTypeValueConstructor() {
		assertAccessType("!ls_result = VALUE #( comp = ?lv_any  comp2 = ?lv_other + ?lv_third ).");
		assertAccessType("!lt_result = VALUE #( comp = ?lv_any ( comp2 = ?lv_other ) ( comp2 = ?lv_third ) ).");
	}

	@Test
	void testAccessTypeNoneAfterColonInParens() {
		// ensure that prevToken == null does not trigger an exception when requesting the access type for "comp1" after "(:" 
		Command command = buildCommand("ls_struc = VALUE ty_s_struc(: comp1 = lv_value ).");
		Token token = command.getFirstToken().getLastTokenDeep(true, TokenSearch.ASTERISK, "comp1");
		assertEquals(token.getMemoryAccessType(), MemoryAccessType.NONE);
	}
	
	private void assertEndOfLogicalExpression(String commandText, String startTokenText, String expEndTokenText) {
		Token startToken = buildCommand(commandText, startTokenText);
		Token lastInLogExpr = startToken.getLastTokenOfLogicalExpression();
		String actEndTokenText = (lastInLogExpr == null) ? null : lastInLogExpr.getNextCodeToken().getText();
		assertEquals(expEndTokenText, actEndTokenText);
	}
	
	@Test
	void testGetEndOfLogicalExpression() {
		assertEndOfLogicalExpression("IF a < ( b + 10 ). ##PRAGMA \" comment", "IF", ".");
		assertEndOfLogicalExpression("ELSEIF a IS INITIAL AND NOT ( b IS INITIAL OR c IS INITIAL ) ##PRAGMA.", "ELSEIF", ".");
		assertEndOfLogicalExpression("CHECK its_data IS NOT INITIAL.", "CHECK", ".");

		assertEndOfLogicalExpression("LOOP AT lts_data ASSIGNING FIELD-SYMBOL(<ls_data>) WHERE a = 1.", "WHERE", ".");
		assertEndOfLogicalExpression("LOOP AT lts_data ASSIGNING <ls_data> WHERE a = 1 OR ( a = 2 AND b <> 3 ) GROUP BY <ls_data>-comp.", "WHERE", "GROUP");

		assertEndOfLogicalExpression("lv_result = xsdbool( a < 5 ).", "xsdbool(", ")");
	}

	@Test
	void testGetEndOfLogicalExpressionAbapSql() {
		assertEndOfLogicalExpression("SELECT * FROM dtab INTO TABLE lt_any WHERE col1 IS NOT NULL.", "WHERE", ".");
		assertEndOfLogicalExpression("SELECT * FROM dtab INTO TABLE lt_any WHERE col1 < ANY ( SELECT col2 FROM other_dtab ) OR col1 > SOME ( SELECT col3 FROM third_dtab ).", "WHERE", ".");
		assertEndOfLogicalExpression("SELECT FROM any_dtab FIELDS col1, CASE WHEN col2 BETWEEN 100 AND 300 OR col3 < 5 THEN 'a' ELSE 'b' END AS col4 INTO TABLE @FINAL(result).", "WHEN", "THEN");
		assertEndOfLogicalExpression("SELECT * FROM dtab INTO TABLE lt_any WHERE CASE WHEN col2 BETWEEN 100 AND 300 OR col3 < 5 THEN 'a' ELSE 'b' END = 'b'.", "WHERE", ".");
		assertEndOfLogicalExpression("SELECT * FROM dtab INTO TABLE lt_any WHERE col1 < ANY ( SELECT col2 FROM other_dtab ) OR col1 > SOME ( SELECT col3 FROM third_dtab ).", "WHERE", ".");
	}

	private void assertTextEquals(String expResultTokenText, Token resultToken) {
		String actResultTokenText = (resultToken == null) ? null : resultToken.getText();
		assertEquals(expResultTokenText, actResultTokenText);
	}

	private void assertNextTokenOfType(String commandText, String startTokenText, TokenType tokenType, String expResultTokenText) {
		Token startToken = buildCommand(commandText, startTokenText);
		assertTextEquals(expResultTokenText, startToken.getNextTokenOfType(tokenType));
	}

	@Test
	void testGetNextTokenOfType() {
		assertNextTokenOfType("IF a = 1 AND ( b IS INITIAL OR b = 3 ).", "=", TokenType.COMPARISON_OP, "=");
		assertNextTokenOfType("a = get_value( b = 1  c = 'abc' ) ##NO_TEXT.", "a", TokenType.PRAGMA, "##NO_TEXT");
		assertNextTokenOfType("CLEAR a.", ".", TokenType.KEYWORD, null);
	}
	
	private void assertNextTokenOfTypes(String commandText, String startTokenText, String expResultTokenText, TokenType... tokenTypes) {
		Token startToken = buildCommand(commandText, startTokenText);
		assertTextEquals(expResultTokenText, startToken.getNextTokenOfTypes(tokenTypes));
	}

	@Test
	void testGetNextTokenOfTypes() {
		assertNextTokenOfTypes("IF a = 1 AND ( b IS INITIAL OR b = 3 ).", "=", "(", TokenType.OTHER_OP, TokenType.COMMENT);
		assertNextTokenOfTypes("a = get_value( b = 'abc'  c = 1 ) ##NO_TEXT.", "a", "'abc'", TokenType.PRAGMA, TokenType.LITERAL);
		assertNextTokenOfTypes("CLEAR a.", ".", null, TokenType.KEYWORD, TokenType.IDENTIFIER, TokenType.OTHER_OP);
	}

	private void assertNextSiblingOfType(String commandText, String startTokenText, TokenType tokenType, String expResultTokenText) {
		Token startToken = buildCommand(commandText, startTokenText);
		assertTextEquals(expResultTokenText, startToken.getNextSiblingOfType(tokenType));
	}
	
	@Test
	void testGetNextSiblingOfType() {
		assertNextSiblingOfType("IF a = 1 AND ( b IS INITIAL OR b = 3 ) AND c < 4.", "=", TokenType.COMPARISON_OP, "<");
		assertNextSiblingOfType("CLEAR lv_any.", ".", TokenType.KEYWORD, null);

		assertNextSiblingOfType("any_method( a = 2 * ( 3 + 5 ) )->other_method( ).", "any_method(", TokenType.IDENTIFIER, ")->other_method(");
	}

	private void assertPrevSiblingOfType(String commandText, String startTokenText, TokenType tokenType, String expResultTokenText) {
		Token startToken = buildCommand(commandText, startTokenText);
		assertTextEquals(expResultTokenText, startToken.getPrevSiblingOfType(tokenType));
	}
	
	@Test
	void testGetPrevSiblingOfType() {
		assertPrevSiblingOfType("IF a = 1 AND ( b IS INITIAL OR b <> 3 ) AND c < 4.", "<", TokenType.COMPARISON_OP, "=");
		assertPrevSiblingOfType("CLEAR lv_any.", "CLEAR", TokenType.KEYWORD, null);
		assertPrevSiblingOfType("any_method( a = 2 ).", ")", TokenType.IDENTIFIER, "any_method(");
	}

	private void assertNextSiblingOfTypeAndText(String commandText, String startTokenText, TokenType tokenType, String expResultTokenText, String... texts) {
		Token startToken = buildCommand(commandText, startTokenText);
		assertTextEquals(expResultTokenText, startToken.getNextSiblingOfTypeAndText(tokenType, texts));
	}
	
	@Test
	void testGetNextSiblingOfTypeAndText() {
		assertNextSiblingOfTypeAndText("CALL METHOD any_method EXPORTING iv_any = 1 IMPORTING ev_any = lv_any CHANGING cv_any = lv_other.", "CALL", TokenType.KEYWORD, "IMPORTING", "IMPORTING", "CHANGING");
		assertNextSiblingOfTypeAndText("GET RUNTIME FIELD DATA(a).", "GET", TokenType.KEYWORD, "FIELD", "FIELD", "FIELDS");
		assertNextSiblingOfTypeAndText("RETURN.", ".", TokenType.KEYWORD, null, "RETURN", "CLEAR");
	}

	@Test
	void testNextOrPrevNull() {
		// test cases in which the prev/next Token is null directly
		assertNull(buildCommand("CLEAR lv_any.", ".").getNext());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextSibling());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextNonCommentToken());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextNonCommentSibling());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextCodeToken());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextCodeSibling());
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextTokenOfType(TokenType.KEYWORD));
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextTokenOfTypes(TokenType.KEYWORD));
		assertNull(buildCommand("CLEAR lv_any.", ".").getNextSiblingOfType(TokenType.KEYWORD));

		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrev());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevSibling());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevNonCommentToken());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevNonCommentSibling());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevCodeToken());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevCodeSibling());
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevTokenOfType(TokenType.KEYWORD));
		assertNull(buildCommand("CLEAR lv_any.", "CLEAR").getPrevSiblingOfType(TokenType.KEYWORD));
	}

	private void assertCreateForAbapIllegalArgument(String text, TokenType type) {
		boolean exceptionThrown = false;
		try {
			Token.createForAbap(0, 1, text, type, 1);
		} catch(IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testCreateForAbapError() {
		// except an IllegalArgumentException to be thrown, because the supplied Token text can never be of the supplied TokenType
		assertCreateForAbapIllegalArgument("LOOP", TokenType.COMPARISON_OP);
		assertCreateForAbapIllegalArgument("lv_text", TokenType.COLON);
		assertCreateForAbapIllegalArgument("=", TokenType.KEYWORD);
		assertCreateForAbapIllegalArgument(">=", TokenType.COMMENT);
		assertCreateForAbapIllegalArgument("\" commment", TokenType.PRAGMA);
		assertCreateForAbapIllegalArgument(",", TokenType.PERIOD);
	}
	
	@Test
	void testCreateForAbap() {
		assertEquals(TokenType.KEYWORD, Token.createForAbap(0, 1, "LOOP", TokenType.KEYWORD, 1).type);
		assertEquals(TokenType.IDENTIFIER, Token.createForAbap(0, 1, "LOOP", TokenType.IDENTIFIER, 1).type);

		assertEquals(TokenType.ASSIGNMENT_OP, Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, 1).type);
		assertEquals(TokenType.COMPARISON_OP, Token.createForAbap(0, 1, "=", TokenType.COMPARISON_OP, 1).type);
	}
	
	private Token getEmptyToken(String text) {
		Token token = Token.createForAbap(0, 1, text, 1);
		token.text = "";
		return token;
	}

	@Test
	void assertTokenMethodsTestEmptyCase() {
		// ensure that Token methods are safeguarded against Token.text being set to ""
		assertFalse(getEmptyToken("'3.14'").isFloatLiteral());
		assertFalse(getEmptyToken(")").isClosingParenthesisOrBracket());
		assertFalse(getEmptyToken("EQ").isTextualComparisonOp());
		assertFalse(getEmptyToken("`abc`").isStringLiteral());
		assertFalse(getEmptyToken("|abc{").startsStringTemplate());
		assertFalse(getEmptyToken("}abc|").endsStringTemplate());
		assertFalse(getEmptyToken("Abc").startsWithLetter());
		assertFalse(getEmptyToken("|abc{").startsEmbeddedExpression());
		assertFalse(getEmptyToken("}abc|").endsEmbeddedExpression());
		assertFalse(getEmptyToken(">=").isAnyComparisonOperator(">", "<", ">="));
	}	

	private Token getStandAloneToken(String text) {
		return Token.createForAbap(0, 1, text, 1);
	}

	@Test
	void assertTokenMethodsTestStandAloneCase() {
		assertFalse(getStandAloneToken("any").isFirstTokenInCommand());
		assertFalse(getStandAloneToken("any").isLastTokenInCommand());
		assertFalse(getStandAloneToken("any").isOnlyTokenInCommand());
		assertFalse(getStandAloneToken("DATA(").opensInlineDeclaration());
		assertFalse(getStandAloneToken("FIELD-SYMBOL(").opensInlineDeclarationForFieldSymbol());
	}
	
	@Test
	void testGetTypeAndTextForErrorMessage() {
		assertEquals("PRAGMA '##NEEDED'", getStandAloneToken("##NEEDED").getTypeAndTextForErrorMessage());
		assertEquals("KEYWORD 'CLEAR'", getStandAloneToken("CLEAR").getTypeAndTextForErrorMessage());
		assertEquals("IDENTIFIER 'lv_any'", getStandAloneToken("lv_any").getTypeAndTextForErrorMessage());
		assertEquals("LITERAL '42'", getStandAloneToken("42").getTypeAndTextForErrorMessage());
	}
	
	@Test
	void testInsertLeftSiblingError() {
		boolean throwsException = false;
		try {
			buildCommand("any_method( a = 1 ).", ")").insertLeftSibling(Token.createForAbap(0, 1, " \" comment", 1));
		} catch(IntegrityBrokenException e) {
			throwsException = true;
		}
		assertTrue(throwsException);
	}
	
	@Test
	void testInsertRightSiblingTerm() throws UnexpectedSyntaxException {
		// given: remove 'b = 3' from the original Command
		Token oneToken = buildCommand("result = get_value( a = 1 ) + get_value( b = 2  c = 3 ).", "1");
		Command command = oneToken.getParentCommand();
		Token paramB = oneToken.getLastTokenDeep(false, TokenSearch.ASTERISK, "c");
		Term paramBAssignment = Term.createForTokenRange(paramB, paramB.getNext().getNext());

		// when: insert 'c = 3' after 'a = 1', expecting integrity to be kept
		try {
			paramBAssignment.removeFromCommand(false);
			oneToken.insertRightSibling(paramBAssignment);
		} catch (IntegrityBrokenException e) {
			fail();
		}
		
		assertEquals("result = get_value( a = 1  c = 3 ) + get_value( b = 2 ).", command.toString());
	}
	
	@Test
	void testIsTypeIdentifier() {
		// type definition
		assertTrue(buildCommand("TYPES dtype TYPE c LENGTH 3.", "dtype").isTypeIdentifier());
		assertTrue(buildCommand("TYPES: dtype TYPE c LENGTH 3.", "dtype").isTypeIdentifier());
		assertTrue(buildCommand("TYPES: ##PRAGMA dtype TYPE i.", "dtype").isTypeIdentifier());
		assertTrue(buildCommand("TYPES: dtype TYPE i, dtype2 TYPE c LENGTH 3.", "dtype2").isTypeIdentifier());
		assertTrue(buildCommand("TYPES dtype TYPE LINE OF ty_tt_any.", "dtype").isTypeIdentifier());
		assertTrue(buildCommand("TYPES ref_type TYPE REF TO type.", "ref_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES BEGIN OF struct_type, comp TYPE i, END OF struct_type.", "struct_type").isTypeIdentifier());
		assertTrue(buildCommand("INCLUDE TYPE struct_type.", "struct_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES BEGIN OF ENUM enum_type, val1 VALUE IS INITIAL, val2, END OF ENUM enum_type.", "enum_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES BEGIN OF MESH mesh_type, node TYPE REF TO table_type, END OF MESH mesh_type.", "mesh_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES table_type TYPE STANDARD TABLE OF ty_s_struc WITH EMPTY KEY.", "table_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES table_type TYPE SORTED TABLE OF ty_s_struc WITH KEY a.", "table_type").isTypeIdentifier());
		assertTrue(buildCommand("TYPES dtype TYPE RANGE OF type.", "dtype").isTypeIdentifier());
		assertTrue(buildCommand("TYPES dtype TYPE struct WITH INDICATORS ind.", "dtype").isTypeIdentifier());

		// usage in constructor expression
		// "NEW", "VALUE", "CONV", "CORRESPONDING", "CAST", "REF", "EXACT", "REDUCE", "FILTER", "COND", "SWITCH"};
		assertTrue(buildCommand("a = VALUE ty_s_any( a = 1 ).", "ty_s_any(").isTypeIdentifier());
		assertTrue(buildCommand("any_method( a = VALUE ty_tt_any( ( a = 1 ) ) ).", "ty_tt_any(").isTypeIdentifier());
		assertTrue(buildCommand("a = REF ty( dobj ).", "ty(").isTypeIdentifier());
		assertTrue(buildCommand("any_method( NEW ty( ) ).", "ty(").isTypeIdentifier());
		assertTrue(buildCommand("a = CORRESPONDING ty( b ).", "ty(").isTypeIdentifier());
		assertTrue(buildCommand("a = COND ty( WHEN lv_condition = abap_true THEN b ELSE c ).", "ty(").isTypeIdentifier());
		
		// usage in a declaration
		assertTrue(buildCommand("DATA dobj TYPE REF TO ty.", "ty").isTypeIdentifier());
		assertTrue(buildCommand("DATA: a TYPE i, dobj TYPE STANDARD TABLE OF REF TO ty.", "ty").isTypeIdentifier());
		assertTrue(buildCommand("CONSTANTS: lc_any TYPE ty.", "ty").isTypeIdentifier());
		assertTrue(buildCommand("FIELD-SYMBOLS <ls_any> TYPE LINE OF ty_tt_any.", "ty_tt_any").isTypeIdentifier());
		assertTrue(buildCommand("TYPES dtype TYPE LINE OF ty_tt_any.", "ty_tt_any").isTypeIdentifier());
		
		// negative cases
		assertFalse(buildCommand("TYPES dtype TYPE LINE OF ty_tt_any.", "TYPES").isTypeIdentifier());
		assertFalse(buildCommand("a = any_method( b = 1 ).", "any_method(").isTypeIdentifier());
		assertFalse(buildCommand("a = any_method( b = 1 ).", "b").isTypeIdentifier());
		assertFalse(buildCommand("* comment", 0).isTypeIdentifier());
		assertFalse(buildCommand("DATA dobj LIKE dobj2.", "dobj2").isTypeIdentifier());
		assertFalse(buildCommand("TYPES dtyp LIKE LINE OF dobj.", "dobj").isTypeIdentifier());
		assertFalse(buildCommand("FIELD-SYMBOLS <ls_any> LIKE LINE OF lt_table.", "lt_table").isTypeIdentifier());
	}

	@Test
	void testStartOfTableExpression() {
		// if 'this' Token is not part of a table expression, 'this' should be returned 
		assertTrue(buildCommand("* comment", 0).getStartOfTableExpression().textEquals("* comment"));
		assertTrue(buildCommand("ASSERT a = 1.", 0).getStartOfTableExpression().textEquals("ASSERT"));
		assertTrue(buildCommand("lv_any = 1.", 0).getStartOfTableExpression().textEquals("lv_any"));
		
		assertTrue(buildCommand("lt_any[ 1 ]-name = 'a'.", "]-name").getStartOfTableExpression().textEquals("lt_any["));
		assertTrue(buildCommand("lt_any[ num = 1 ]-name = 'a'.", "]-name").getStartOfTableExpression().textEquals("lt_any["));
		assertTrue(buildCommand("lt_any[ num = 1 ]-ref->mv_msgty = 'a'.", "]-ref->mv_msgty").getStartOfTableExpression().textEquals("lt_any["));
		assertTrue(buildCommand("lt_any[ num = 1 ]-inner[ 2 ]-name", "]-name").getStartOfTableExpression().textEquals("lt_any["));
	}

	@Test
	void testEndOfTableExpression() {
		// if 'this' Token is not part of a table expression, 'this' should be returned 
		assertTrue(buildCommand("* comment", 0).getEndOfTableExpression().textEquals("* comment"));
		assertTrue(buildCommand("ASSERT a = 1.", 0).getEndOfTableExpression().textEquals("ASSERT"));
		assertTrue(buildCommand("lv_any = 1.", 0).getEndOfTableExpression().textEquals("lv_any"));

		assertTrue(buildCommand("lt_any[ 1 ]-name = 'a'.", 0).getEndOfTableExpression().textEquals("]-name"));
		assertTrue(buildCommand("lt_any[ num = 1 ]-name = 'a'.", 0).getEndOfTableExpression().textEquals("]-name"));
		assertTrue(buildCommand("lt_any[ num = 1 ]-ref->mv_msgty = 'a'.", 0).getEndOfTableExpression().textEquals("]-ref->mv_msgty"));
		assertTrue(buildCommand("lt_any[ num = 1 ]-inner[ 2 ]-name", 0).getEndOfTableExpression().textEquals("]-name"));
	}
	
	@Test
	void testCanInsertStressToken() {
		// test cases in which the next Token is attached
		assertFalse(buildCommand("DATA(lv_any) = 1.", 0).canInsertStressTestTokenAfter(StressTestType.COLON));
		assertTrue(buildCommand("DATA lv_any TYPE i.", 3).canInsertStressTestTokenAfter(StressTestType.LINE_END_COMMENT));
		assertTrue(buildCommand("DATA lv_any TYPE i, lv_other TYPE i.", 3).canInsertStressTestTokenAfter(StressTestType.LINE_END_COMMENT));
		assertTrue(buildCommand("DATA: lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.LINE_END_COMMENT));

		// test cases in which insertion is possible
		assertTrue(buildCommand("DATA lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.LINE_END_COMMENT));
		assertTrue(buildCommand("DATA lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.COMMENT_LINE));
		assertTrue(buildCommand("DATA lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.PRAGMA));
		assertTrue(buildCommand("DATA lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.COLON));

		// test cases in which a pragma can NOT be inserted
		assertFalse(buildCommand("* comment", 0).canInsertStressTestTokenAfter(StressTestType.PRAGMA));
		assertFalse(buildCommand(" any_method( ).", 2).canInsertStressTestTokenAfter(StressTestType.PRAGMA));
		assertFalse(buildCommand(" any_method( ). \" comment", 2).canInsertStressTestTokenAfter(StressTestType.PRAGMA));
		assertFalse(buildCommand(" any_method( ). \" comment", 3).canInsertStressTestTokenAfter(StressTestType.PRAGMA));

		// test cases in which a colon can NOT be inserted
		assertFalse(buildCommand("* comment", 0).canInsertStressTestTokenAfter(StressTestType.COLON));
		assertFalse(buildCommand(" any_method( ).", 2).canInsertStressTestTokenAfter(StressTestType.COLON));
		assertFalse(buildCommand(" any_method( ). \" comment", 2).canInsertStressTestTokenAfter(StressTestType.COLON));
		assertFalse(buildCommand(" any_method( ). \" comment", 3).canInsertStressTestTokenAfter(StressTestType.COLON));
		assertFalse(buildCommand(" any_method( ) \" comment" + SEP + ".", 2).canInsertStressTestTokenAfter(StressTestType.COLON));
		
		// test unsupported StressTestType
		try {
			buildCommand("DATA lv_any TYPE i.", 0).canInsertStressTestTokenAfter(StressTestType.NONE);
			fail();
		} catch (InvalidParameterException ex) {
			// expected case
		}
	}
	
	@Test
	void testInsertStressToken() throws IntegrityBrokenException {
		// most branches are tested by CommandTest.testInsertStressTestToken()
		
		assertFalse(buildCommand("DATA(lv_any) = 1", 0).insertStressTestTokenAfter(StressTestType.COMMENT_LINE));
	}
	
	@Test
	void testAbapSqlLiteralType() {
		// ensure that with ABAP SQL Typed Literals, the literal type is correctly identified 
		assertTrue(buildCommand("SELECT int1`1` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT decfloat16`3.14` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT d16n`3.14` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT char`abc` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT string`abc` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT numc`012` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT cuky`EUR` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());
		assertTrue(buildCommand("SELECT dats`20230419` AS lit1 FROM any_table INTO @DATA(ls_any).", 1).isSqlLiteralType());

		assertFalse(buildCommand("SELECT int1 FROM any_table INTO @DATA(ls_any). ENDSELECT.", 1).isSqlLiteralType());
	}
	
	@Test
	void testOffsetWithAsterisk() {
		// ensure that the type of the '*' token is corrected from COMMENT to OTHER_OP (see Token.addNext)
		assertTrue(buildCommand("lv_any+4(*) = lv_other.", 1).isOtherOp());
	}
	
	@Test
	void testSetLineBreaks() {
		assertFalse(buildCommand("a = 1.", 1).setLineBreaks(0));
		assertTrue(buildCommand("a = 1.", 1).setLineBreaks(1));
		assertTrue(buildCommand("a = 1.", 1).setLineBreaks(2));

		assertTrue(buildCommand("a" + SEP + " = 1.", 1).setLineBreaks(0));
		assertFalse(buildCommand("a" + SEP + " = 1.", 1).setLineBreaks(1));
		assertTrue(buildCommand("a" + SEP + " = 1.", 1).setLineBreaks(2));
	}
	
	@Test
	void testInsertNextCommentAsFirstChild() throws UnexpectedSyntaxException, IntegrityBrokenException {
		Command firstCommand = buildCommand("any_method( ).");
		Token comment = Token.createForAbap(0, 1, "\" comment", 0);
		firstCommand.firstToken.insertNext(comment);
		assertEquals("any_method( \" comment" + SEP + "            ).", code.toString());
	}
	
	@Test
	void testInsertNextTermAsFirstChild() throws UnexpectedSyntaxException, IntegrityBrokenException {
		Command firstCommand = buildCommand("any_method( ). other_method( a = 1 ).");
		Command secondCommand = firstCommand.getNext();
		Term assignment = Term.createForTokenRange(secondCommand.firstToken.getNext(), secondCommand.lastToken.getPrev().getPrev());
		assignment.removeFromCommand(false);
		firstCommand.firstToken.insertNext(assignment);
		assertEquals("any_method( a = 1 ). other_method( ).", code.toString());
	}
	
	@Test
	void testInsertNextCommentTermAsFirstChild() throws UnexpectedSyntaxException, IntegrityBrokenException {
		Command firstCommand = buildCommand("any_method( ).");
		Token comment = Token.createForAbap(0, 1, "\" comment", 0);
		Term term = Term.createForTokenRange(comment, comment);
		firstCommand.firstToken.insertNext(term);
		assertEquals("any_method( \" comment" + SEP + "            ).", code.toString());
	}
	
	@Test
	void testInsertNextTermBeforeExistingChild() throws UnexpectedSyntaxException, IntegrityBrokenException {
		Command firstCommand = buildCommand("any_method( b = 2 ). other_method( a = 1 ).");
		Command secondCommand = firstCommand.getNext();
		Term assignment = Term.createForTokenRange(secondCommand.firstToken.getNext(), secondCommand.lastToken.getPrev().getPrev());
		assignment.removeFromCommand(false);
		firstCommand.firstToken.insertNext(assignment);
		assertEquals("any_method( a = 1 b = 2 ). other_method( ).", code.toString());
	}
	
	@Test
	void testInsertNextTermAsSibling() throws UnexpectedSyntaxException, IntegrityBrokenException {
		Command firstCommand = buildCommand("  ##NO_TEXT. other_method( a = 1 ).");
		Command secondCommand = firstCommand.getNext();
		Term assignment = Term.createForTokenRange(secondCommand.firstToken.getNext(), secondCommand.lastToken.getPrev().getPrev());
		assignment.removeFromCommand(false);
		firstCommand.firstToken.insertNext(assignment);
		assertEquals("  ##NO_TEXT a = 1. other_method( ).", code.toString());
	}
	
	@Test
	void testInsertRightSiblingAfterLevelOpenerErr() {
		boolean raisedException = false;
		try {
			buildCommand("any_method( ).", 0).insertRightSibling(Token.createForAbap(0, 1, "a", 0), false);
		} catch (IntegrityBrokenException e) {
			// expected case
			raisedException = true;
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testInsertRightSiblingAfterFinalCommentErr() {
		boolean raisedException = false;
		try {
			buildCommand("a = 1. \" comment", 4).insertRightSibling(Token.createForAbap(0, 1, ".", 0), false);
		} catch (IntegrityBrokenException e) {
			// expected case
			raisedException = true;
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testInsertRightSiblingWithLineBreaksToComment() throws IntegrityBrokenException {
		Token comment = buildCommand("  a = \" comment" + SEP + "  1.", 2);
		comment.insertRightSibling(Token.createForAbap(2, 2, "-", 0), false);
		assertEquals("  a = \" comment" + SEP + SEP + "  -" + SEP + "  1.", comment.getParentCommand().toString());
	}
	
	@Test
	void testInsertRightSiblingToComment() throws IntegrityBrokenException {
		Token comment = buildCommand("  a = \" comment" + SEP + "  1.", 2);
		comment.insertRightSibling(Token.createForAbap(0, 1, "-", 0), false);
		assertEquals("  a = \" comment" + SEP + "  - 1.", comment.getParentCommand().toString());
	}
	
	@Test
	void testInsertRightSiblingCommentToComment() throws IntegrityBrokenException {
		Token comment = buildCommand("  a = \" comment" + SEP + SEP + "  1.", 2);
		comment.insertRightSibling(Token.createForAbap(0, 1, "\" new comment", 0), false);
		assertEquals("  a = \" comment" + SEP + SEP + "  \" new comment" + SEP + "  1.", comment.getParentCommand().toString());
	}
	
	@Test
	void testInsertRightSiblingComment() throws IntegrityBrokenException {
		Token equalsSign = buildCommand("  a = 1.", 1);
		equalsSign.insertRightSibling(Token.createForAbap(0, 1, "\" comment", 0), false);
		assertEquals("  a = \" comment" + SEP + "      1.", equalsSign.getParentCommand().toString());
	}


	@Test
	void testInsertRightSiblingTermAfterLevelOpenerErr() throws UnexpectedSyntaxException {
		boolean raisedException = false;
		Token bToken = Token.createForAbap(0, 1, "b", 0);
		Term term = Term.createForTokenRange(bToken, bToken);
		try {
			buildCommand("any_method( ).", 0).insertRightSibling(term, false);
		} catch (IntegrityBrokenException e) {
			// expected case
			raisedException = true;
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testInsertRightSiblingTermAfterFinalCommentErr() throws UnexpectedSyntaxException {
		boolean raisedException = false;
		Token bToken = Token.createForAbap(0, 1, "b", 0);
		Term term = Term.createForTokenRange(bToken, bToken);
		try {
			buildCommand("a = 1. \" comment", 4).insertRightSibling(term, false);
		} catch (IntegrityBrokenException e) {
			// expected case
			raisedException = true;
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testInsertRightSiblingTermWithLineBreaksToComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Command firstCommand = buildCommand("  a = \" comment" + SEP + "  1. c = " + SEP + SEP + "  b + 1.");
		Command secondCommand = firstCommand.getNext();

		// remove Term "b +" from the second Command  
		Token bToken = secondCommand.firstToken.getNext().getNext();
		Term term = Term.createForTokenRange(bToken, bToken.getNext());
		term.removeFromCommand(true);

		// insert "b +" after the comment
		Token comment = firstCommand.getFirstToken().getNext().getNext();
		comment.insertRightSibling(term, false);
		assertEquals("  a = \" comment" + SEP + SEP + "  b +" + SEP + "  1.", firstCommand.toString());
	}
	
	@Test
	void testInsertRightSiblingTermToComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Command firstCommand = buildCommand("  a = \" comment" + SEP + "  1. c = b + 1.");
		Command secondCommand = firstCommand.getNext();

		// remove Term "b +" from the second Command  
		Token bToken = secondCommand.firstToken.getNext().getNext();
		Term term = Term.createForTokenRange(bToken, bToken.getNext());
		term.removeFromCommand(true);

		// insert "b +" after the comment
		Token comment = firstCommand.getFirstToken().getNext().getNext();
		comment.insertRightSibling(term, false);
		assertEquals("  a = \" comment" + SEP + "  b + 1.", firstCommand.toString());
	}
	
	@Test
	void testInsertRightSiblingTermAddIndent() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Command firstCommand = buildCommand("  a = \" comment" + SEP + "      1. c = b + 1.");
		Command secondCommand = firstCommand.getNext();

		// remove Term "b +" from the second Command  
		Token bToken = secondCommand.firstToken.getNext().getNext();
		Term term = Term.createForTokenRange(bToken, bToken.getNext());
		term.removeFromCommand(true);

		// insert "b +" after the comment
		Token equalsSign = firstCommand.getFirstToken().getNext();
		equalsSign.insertRightSibling(term, true);
		assertEquals("  a = b + \" comment" + SEP + "          1.", firstCommand.toString());
	}
	
	@Test
	void testInsertRightSiblingTermCommentToComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Token newComment = Token.createForAbap(0, 1, "\" new comment", 0);
		Term term = Term.createForTokenRange(newComment, newComment);
		Token comment = buildCommand("  a = \" comment" + SEP + SEP + "  1.", 2);
		comment.insertRightSibling(term, false);
		assertEquals("  a = \" comment" + SEP + SEP + "  \" new comment" + SEP + "  1.", comment.getParentCommand().toString());
	}
	
	@Test
	void testInsertRightSiblingTermComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Token newComment = Token.createForAbap(0, 1, "\" comment", 0);
		Term term = Term.createForTokenRange(newComment, newComment);
		Token equalsSign = buildCommand("  a = 1.", 1);
		equalsSign.insertRightSibling(term, false);
		assertEquals("  a = \" comment" + SEP + "      1.", equalsSign.getParentCommand().toString());
	}
	
	@Test
	void testInsertLeftSiblingTermBeforeLevelCloserErr() throws UnexpectedSyntaxException {
		boolean raisedException = false;
		Token bToken = Token.createForAbap(0, 1, "b", 0);
		Term term = Term.createForTokenRange(bToken, bToken);
		try {
			buildCommand("any_method( ).", 1).insertLeftSibling(term);
		} catch (IntegrityBrokenException e) {
			// expected case
			raisedException = true;
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testInsertLeftSiblingTermAfterComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Command firstCommand = buildCommand("  a = \" comment" + SEP + "      1. c = b + 1.");
		Command secondCommand = firstCommand.getNext();

		// remove Term "b +" from the second Command  
		Token bToken = secondCommand.firstToken.getNext().getNext();
		Term term = Term.createForTokenRange(bToken, bToken.getNext());
		term.removeFromCommand(true);

		// insert "b +" after the comment
		Token oneToken = firstCommand.getLastToken().getPrev();
		oneToken.insertLeftSibling(term);
		assertEquals("  a = \" comment" + SEP + "      b + 1.", firstCommand.toString());
	}
	
	@Test
	void testInsertLeftSiblingCommentTermAfterComment() throws IntegrityBrokenException, UnexpectedSyntaxException {
		Token comment = Token.createForAbap(0, 1, "\" new comment", 0);
		Term term = Term.createForTokenRange(comment, comment);
		Command command = buildCommand("  a = \" comment" + SEP + "      1.");
		command.getLastToken().getPrev().insertLeftSibling(term);
		assertEquals("  a = \" comment" + SEP + "      \" new comment" + SEP + "      1.", command.toString());
	}
	
	@Test
	void testGetLineInCommand() {
		Command command = buildCommand("  a" + SEP + "  =" + SEP + SEP + "  1.");
		Token firstToken = command.getFirstToken();
		assertEquals(0, firstToken.getLineInCommand());
		assertEquals(1, firstToken.getNext().getLineInCommand());
		assertEquals(3, firstToken.getNext().getNext().getLineInCommand());

		command = buildCommand("  RETURN.");
		firstToken = command.getFirstToken();
		assertEquals(0, firstToken .getLineInCommand());
		assertEquals(0, firstToken .getNext().getLineInCommand());
	}
	
	@Test
	void testIsDeclarationKeyword() {
		assertTrue(buildCommand("  DATA(a) TYPE i.", 0).isDeclarationKeyword());
		assertTrue(buildCommand("  any_method( IMPORTING iv_any = DATA(lv_any) ).", 4).isDeclarationKeyword());
		assertTrue(buildCommand("  FIELD-SYMBOLS <ls_any> TYPE ty_s_any.", 0).isDeclarationKeyword());
		assertTrue(buildCommand("  METHODS any_method.", 0).isDeclarationKeyword());
		assertTrue(buildCommand("  CLASS-DATA mv_any TYPE i.", 0).isDeclarationKeyword());

		assertFalse(buildCommand("  ASSERT a = 1.", 0).isDeclarationKeyword());
		assertFalse(buildCommand("  DATA(a) TYPE i.", 1).isDeclarationKeyword());
		assertFalse(buildCommand("  FIELD-SYMBOLS <ls_any> TYPE ty_s_any.", 1).isDeclarationKeyword());
		assertFalse(buildCommand("  CALL METHOD any_method.", 1).isDeclarationKeyword());
	}
	
	@Test
	void testGetPrevWhileComment() {
		Command command = buildCommand("  a \" comment1" + SEP + "* comment2" + SEP + "  = 1.");
		Token firstToken = command.getFirstToken();
		assertEquals(firstToken, firstToken.getPrevWhileComment());
		assertEquals(firstToken, firstToken.getNext().getPrevWhileComment());
		assertEquals(firstToken, firstToken.getNext().getNext().getPrevWhileComment());
		assertNotEquals(firstToken, firstToken.getNext().getNext().getNext().getPrevWhileComment());
	}
	
	@Test
	void testIsSqlTypeInCast() {
		assertFalse(buildCommand("DATA a TYPE i.", 1).isSqlTypeInCast());
		assertFalse(buildCommand("DATA int1 TYPE i.", 1).isSqlTypeInCast());
		assertFalse(buildCommand("any_method( int1 = 1 ).", 1).isSqlTypeInCast());
		assertFalse(buildCommand("a = CAST ty_any( int1 ).", 4).isSqlTypeInCast());
	}
}
