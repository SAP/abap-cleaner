package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class MarkdownBuilderTest {
	private static final int LEVEL_TOO_LOW = 0;
	private static final int LEVEL_TOO_HIGH = 9;
	private MarkdownBuilder mb;
	
	private void assertStringArrayEquals(String[] exp, String[] act) {
		assertEquals(exp.length, act.length);
		for (int i = 0; i < exp.length; ++i)
			assertEquals(exp[i], act[i]);
	}

	private void expect(String... expLines) {
		mb.finishBuild();
		String[] actLines = mb.toString().split("\r\n");
		assertStringArrayEquals(expLines, actLines);
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		mb = MarkdownBuilder.create();
	}
	
	@Test
	void testHeading() {
		mb.startNewHeading("H1", 1);
		mb.startNewHeading("H2", 2);
		mb.startNewHeading("H1", 1);
		
		expect("# H1", "", "## H2", "", "# H1");
	}

	@Test
	void testHeadingError() {
		try {
			mb.startNewHeading("H", LEVEL_TOO_LOW);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}

		try {
			mb.startNewHeading("H", LEVEL_TOO_HIGH);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	@Test
	void testParagraph() {
		mb.startNewParagraph();
		mb.appendText("abc");
		mb.startNewParagraph();
		mb.appendText("def");
		
		expect("abc", "", "def");
	}

	@Test
	void testEscapeChars() {
		mb.startNewParagraph();
		mb.appendText("a \\ b ` c * d _ e { f } g [ h ] i ( j ) k # l + m ! n");
		
		expect("a \\\\ b \\` c \\* d \\_ e \\{ f \\} g \\[ h \\] i \\( j \\) k \\# l \\+ m \\! n");
	}

	@Test
	void testFormattedText() {
		mb.startNewParagraph();
		mb.appendText("a ");
		mb.appendBoldText("bold");
		mb.appendText(" b ");
		mb.appendItalicText("italic");
		mb.appendText(" c ");
		mb.appendInlineCode("code");
		mb.appendText(".");
		
		expect("a **bold** b *italic* c `code`.");
	}

	@Test
	void testFormattedEscapeText() {
		mb.startNewParagraph();
		mb.appendText("a ");
		mb.appendBoldText("*");
		mb.appendText(" b ");
		mb.appendItalicText("*");
		mb.appendText(" c ");
		mb.appendInlineCode("*");
		mb.appendText(".");
		
		expect("a **\\*** b *\\** c `*`.");
	}

	@Test
	void testLinks() {
		mb.startNewParagraph();
		mb.appendText("a ");
		mb.appendLink("about:blank");
		mb.appendText(" b ");
		mb.appendLink("text", "about:blank");
		mb.appendText(" c ");
		mb.appendLink("{[]}", "about:blank");
		mb.appendText(".");
		
		expect("a about:blank b [text](about:blank) c [\\{\\[\\]\\}](about:blank).");
	}

	@Test
	void testBoldLinks() {
		mb.startNewParagraph();
		mb.appendText("a ");
		mb.appendBoldLink("bold", "about:blank");
		mb.appendText(" b ");
		mb.appendBoldLink("_*!", "about:blank");
		mb.appendText(".");
		
		expect("a [**bold**](about:blank) b [**\\_\\*\\!**](about:blank).");
	}

	@Test
	void testBlockQuote() {
		mb.startNewBlockQuote("a ", 1);
		mb.appendBoldText("bold");
		mb.startNewBlockQuote("b ", 2);
		mb.appendItalicText("italic");
		
		expect("> a **bold**", "", ">> b *italic*");
	}

	@Test
	void testBlockQuoteError() {
		try {
			mb.startNewBlockQuote("Q", LEVEL_TOO_LOW);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}

		try {
			mb.startNewBlockQuote("Q", LEVEL_TOO_HIGH);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	@Test
	void testBullet() {
		mb.startNewParagraph();
		mb.appendText("p");

		mb.startNewBullet(1);
		mb.appendText("a");
		mb.startNewBullet(1);
		mb.appendText("b");
		mb.startNewBullet(2);
		mb.appendText("c");
		mb.startNewBullet(3);
		mb.appendText("d");
		
		expect("p", "", "* a", "* b", "  * c", "    * d");
	}

	@Test
	void testBulletError() {
		try {
			mb.startNewBullet(LEVEL_TOO_LOW);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}

		try {
			mb.startNewBullet(LEVEL_TOO_HIGH);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	@Test
	void testOrderedList() {
		mb.startNewParagraph();
		mb.appendText("p");

		mb.startNewOrderedListItem(1);
		mb.appendText("a");
		mb.startNewOrderedListItem(1);
		mb.appendText("b");
		mb.startNewOrderedListItem(2);
		mb.appendText("c");
		mb.startNewOrderedListItem(3);
		mb.appendText("d");
		
		expect("p", "", "1. a", "1. b", "  1. c", "    1. d");
	}

	@Test
	void testOrderedListError() {
		try {
			mb.startNewOrderedListItem(LEVEL_TOO_LOW);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}

		try {
			mb.startNewOrderedListItem(LEVEL_TOO_HIGH);
			fail();
		} catch(IllegalArgumentException ex) {
			assertTrue(ex.getMessage().length() > 0);
		}
	}

	@Test
	void testImages() {
		mb.startNewParagraph();
		mb.appendImage("alt", "about:blank", "");
		mb.startNewParagraph();
		mb.appendImage("alt", "about:blank", "hover");
		
		expect("![alt](about:blank)", "", "![alt](about:blank \"hover\")");
	}

	@Test
	void testCodeBlock() {
		mb.startNewParagraph();
		mb.appendText("p");

		mb.startNewCodeBlock("METHODS any_method.\r\nENDMETHOD.", Language.ABAP);
		mb.startNewCodeBlock("define view AnyView", Language.DDL);

		expect("p", "", "```ABAP", "METHODS any_method.", "ENDMETHOD.", "```", "", "```ASDDLS", "define view AnyView", "```");
	}
}
