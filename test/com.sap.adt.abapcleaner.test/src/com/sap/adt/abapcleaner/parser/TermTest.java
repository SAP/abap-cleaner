package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class TermTest {
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	private Command buildCommand(String codeText) {
		try {
			Code code = Code.parse(null, ParseParams.createForTest(codeText, ABAP.NEWEST_RELEASE));
			return code.firstCommand;
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

	@Test
	void testCreateNull() {
		Token anyToken = Token.createForAbap(0, 0, "\" comment", TokenType.COMMENT, 0);
		
		try {
			Term.createSimple(null);
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		try {
			Term.createArithmetic(null);
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		try {
			Term.createForTokenRange(null, anyToken);
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}

		try {
			Term.createForTokenRange(anyToken, null);
		} catch (NullPointerException ex) {
			// expected case
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testCreateSimpleErr() {
		Command command = buildCommand("CALL METHOD any_method.");
		try {
			Term.createSimple(command.firstToken);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		}
	}

	@Test
	void testCreateSimpleInlineDeclaration() {
		Command command = buildCommand("any_method( IMPORTING ev_result = DATA(lv_result) ).");
		Token firstToken = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "DATA(");
		try {
			Term term = Term.createSimple(firstToken);
			assertEquals("DATA(lv_result)", term.toString());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}

	@Test
	void testCreateSimpleFinalInlineDeclaration() {
		Command command = buildCommand("any_method( IMPORTING ev_result = FINAL(lv_result) ).");
		Token firstToken = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "FINAL(");
		try {
			Term term = Term.createSimple(firstToken);
			assertEquals("FINAL(lv_result)", term.toString());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}

	@Test
	void testCreateSimpleTextElement() {
		try {
			Term term = Term.createSimple(buildCommand("a = TEXT-001.", 2));
			assertEquals("TEXT-001", term.toString());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}

	@Test
	void testCreateArithmeticErr() {
		Token firstToken = buildCommand("lv_text = |a = { a }, b = { b }| && lv_sep && |c = { c }|.", 2);
		try {
			Term term = Term.createArithmetic(firstToken);
			assertEquals("|a = { a }, b = { b }| && lv_sep && |c = { c }|", term.toString());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testCreateForTokenRangeErr() {
		// expect creation of a Term from "5 * (" to throw an exception, because "(" has child Tokens
		Command command = buildCommand("a = 5 * ( 1 + 2 ).");
		Token firstToken = command.firstToken.getNext().getNext(); // "5"
		Token lastToken = firstToken.getNext().getNext(); // "("
		try {
			Term.createForTokenRange(firstToken, lastToken);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		}

		// expect creation of a Term from "5 * ( 1 + 2" to throw an exception, because "5" and "2" are no siblings
		lastToken = command.lastToken.getPrev().getPrev(); // "2"
		try {
			Term.createForTokenRange(firstToken, lastToken);
			fail();
		} catch (UnexpectedSyntaxException ex) {
			// expected case
		}
	}
	
	@Test
	void testIsFirstTokenAllowed() {
		// test Tokens of IF command
		Token token = buildCommand("IF a < 5 OR b = TEXT-002.", 0); // IF
		assertFalse(Term.isFirstTokenAllowed(token));
		
		token = token.getNext(); // a
		assertTrue(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // <
		assertFalse(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // 5
		assertTrue(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // OR
		assertFalse(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // b
		assertTrue(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // = (comparison operator)
		assertFalse(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // TEXT-002
		assertTrue(Term.isFirstTokenAllowed(token));
		
		// test Tokens of an assignment command
		token = buildCommand("a = VALUE #( ). \" comment", 0); // a
		assertTrue(Term.isFirstTokenAllowed(token));	

		token = token.getNext(); // = (assignment operator)
		assertFalse(Term.isFirstTokenAllowed(token));

		token = token.getNext(); // VALUE
		assertTrue(Term.isFirstTokenAllowed(token));

		token = token.getParentCommand().lastToken; // " comment
		assertFalse(Term.isFirstTokenAllowed(token));

		// test Tokens of a method call
		Command command = buildCommand("any_method( IMPORTING ev_result = DATA(lv_result) ).");
		token = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "DATA(");
		assertTrue(Term.isFirstTokenAllowed(token));

		// test Tokens of a method call
		command = buildCommand("any_method( IMPORTING ev_result = FINAL(lv_result) ).");
		token = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "FINAL(");
		assertTrue(Term.isFirstTokenAllowed(token));
}
	
	@Test
	void testIsSingleLiteral() {
		try {
			Term term = Term.createSimple(buildCommand("a = 5.", 2));
			assertTrue(term.isSingleToken());
			assertTrue(term.isSingleLiteral());
			assertTrue(term.isSingleIntegerLiteral());

			term = Term.createSimple(buildCommand("a = b.", 2));
			assertFalse(term.isSingleLiteral());
			assertFalse(term.isSingleIntegerLiteral());
			assertTrue(term.isSingleIdentifier());

			term = Term.createArithmetic(buildCommand("a = 3 * 4.", 2));
			assertFalse(term.isSingleToken());
			assertFalse(term.isSingleLiteral());

		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testGetSiblingCount() {
		try {
			Term term = Term.createSimple(buildCommand("a = 5.", 2));
			assertEquals(1, term.getTokenCountWithChildren());
			assertEquals(1, term.getSiblingCount());

			term = Term.createArithmetic(buildCommand("a = 3 * ( 4 + 5 ).", 2));
			assertEquals(7, term.getTokenCountWithChildren());
			assertEquals(4, term.getSiblingCount());

		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testGetTextSumAndSpaceWidth() {
		try {
			Term term = Term.createArithmetic(buildCommand("a = 3  *  (" + LINE_SEP + "4 + 5 ).", 2));
			assertEquals(15, term.getSumTextAndSpaceWidth());
			assertEquals(15, term.getSumTextAndSpaceWidth(false));
			assertEquals(13, term.getSumTextAndSpaceWidth(true));

		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testToErrorLogString() {
		try {
			Term term = Term.createArithmetic(buildCommand("a = 3  *  ( \"comment" + LINE_SEP + "4 + 5 ).", 2));
			assertEquals("3 * ( 4 + 5 )", term.toErrorLogString());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testIsOnSingleLine() {
		Token firstToken = buildCommand("a = 3  *  ( \"comment" + LINE_SEP + "4 + 5 ).", 2);
		try {
			Term term = Term.createSimple(firstToken);
			assertTrue(term.isOnSingleLine());

			term = Term.createArithmetic(firstToken);
			assertFalse(term.isOnSingleLine());

			firstToken = buildCommand("a = 3 * 5.", 2);
			term = Term.createArithmetic(firstToken);
			assertTrue(term.isOnSingleLine());
		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
	
	@Test
	void testMayHaveSideEffets() {
		try {
			Token firstToken = buildCommand("a = lts_table[ id = 1 ].", 2);
			Term term = Term.createSimple(firstToken);
			assertFalse(term.mayHaveSideEffects());

			firstToken = buildCommand("any_method( ).").firstToken;
			term = Term.createSimple(firstToken);
			assertTrue(term.mayHaveSideEffects());

			firstToken = buildCommand("a = NEW #( param = 1 ).", 2);
			term = Term.createSimple(firstToken);
			assertTrue(term.mayHaveSideEffects());

			firstToken = buildCommand("a = cl_any_factory=>get( )->get_utility( )->get_next_value( ).", 2);
			term = Term.createSimple(firstToken);
			assertTrue(term.mayHaveSideEffects());

			firstToken = buildCommand("a = 2 * ( 3 + lts_table[ id = 4 ]-component ).", 2);
			term = Term.createArithmetic(firstToken);
			assertFalse(term.mayHaveSideEffects());

			firstToken = buildCommand("a = 2 * ( 3 + lts_table[ id = get_next_id( ) ]-component ).", 2);
			term = Term.createArithmetic(firstToken);
			assertTrue(term.mayHaveSideEffects());

			firstToken = buildCommand("a = VALUE ty_s_any_struc( comp = 'abc' ).", 2);
			term = Term.createSimple(firstToken);
			assertFalse(term.mayHaveSideEffects());

			Command command = buildCommand("any_method( IMPORTING ev_result = DATA(lv_result) ).");
			firstToken = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "DATA(");
			term = Term.createSimple(firstToken);
			assertFalse(term.mayHaveSideEffects());

			command = buildCommand("any_method( IMPORTING ev_result = FINAL(lv_result) ).");
			firstToken = command.firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "FINAL(");
			term = Term.createSimple(firstToken);
			assertFalse(term.mayHaveSideEffects());

		} catch (UnexpectedSyntaxException ex) {
			fail();
		}
	}
}
