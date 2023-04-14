package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TextSettingsWriterTest {
	private int TECHNICAL_VERSION = 16;
	private int REQUIRED_VERSION = 1;

	private ISettingsWriter writer;
	private StringBuilder sbExp = new StringBuilder();
	
	@BeforeEach
	void setUp() throws IOException {
		writer = TextSettingsWriter.createForString(TECHNICAL_VERSION, REQUIRED_VERSION);
		buildExp("{");
		buildExp("  \"requiredVersion\": 1,");
		buildExp("  \"programVersion\": 16");
	}
	
	private void buildExp(String line) {
		if (sbExp.length() > 0)
			sbExp.append(ITextSettings.LF);
		sbExp.append(line);
	}
	private void buildExpAppendComma() {
		sbExp.append(",");
	}

	private void assertResult() throws IOException {
		writer.close();
		buildExp("}");

		String expResult = sbExp.toString();
		String actResult = writer.getStringResult();
		assertEquals(expResult, actResult);
	}

	@Test
	void testVersionInfoOnly() throws IOException {
		assertResult();
	}
	
	@Test
	void testCloseTwice() throws IOException {
		// expect that calling .close() several times does not cause an error 
		writer.close();
		writer.close();
		assertResult();
	}
	
	@Test
	void testEmptyObject() throws IOException {
		writer.startObject("anyObject");
		writer.closeObject();
		
		buildExpAppendComma();
		buildExp("  \"anyObject\": {}");

		assertResult();
	}
	
	@Test
	void testObjectWithOneAttribute() throws IOException {
		writer.startObject("anyObject");
		writer.write("anyBoolean", false);
		writer.closeObject();
		
		buildExpAppendComma();
		buildExp("  \"anyObject\": {");
		buildExp("    \"anyBoolean\": false");
		buildExp("  }");

		assertResult();
	}
	
	@Test
	void testObjectWithAttributes() throws IOException {
		writer.startObject("anyObject");
		writer.write("anyBoolean", true);
		writer.write("anyInteger", 42);
		writer.write("anyFloat", 3.14F);
		writer.write("anyDouble", 3.14159265358979);
		writer.write("anyString", "stringValue");
		writer.closeObject();
		
		buildExpAppendComma();
		buildExp("  \"anyObject\": {");
		buildExp("    \"anyBoolean\": true,");
		buildExp("    \"anyInteger\": 42,");
		buildExp("    \"anyFloat\": 3.14,");
		buildExp("    \"anyDouble\": 3.14159265358979,");
		buildExp("    \"anyString\": \"stringValue\"");
		buildExp("  }");

		assertResult();
	}
	
	@Test
	void testEmptyArray() throws IOException {
		writer.startArray("anyArray");
		writer.closeArray();
		
		buildExpAppendComma();
		buildExp("  \"anyArray\": []");

		assertResult();
	}
	
	@Test
	void testArrayWithOneItem() throws IOException {
		writer.startArray("anyArray");
		writer.writeArrayItem(true);
		writer.closeArray();
		
		buildExpAppendComma();
		buildExp("  \"anyArray\": [");
		buildExp("    true");
		buildExp("  ]");

		assertResult();
	}
	
	@Test
	void testArrayWithOneObject() throws IOException {
		writer.startArray("anyArray");
		writer.startObjectInArray();
		writer.write("anyInteger", 42);
		writer.closeObjectInArray();
		writer.closeArray();
		
		buildExpAppendComma();
		buildExp("  \"anyArray\": [");
		buildExp("    {");
		buildExp("      \"anyInteger\": 42");
		buildExp("    }");
		buildExp("  ]");

		assertResult();
	}
	
	@Test
	void testArrayWithFourItems() throws IOException {
		writer.startArray("anyArray");
		writer.writeArrayItem(42);
		writer.writeArrayItem(3.14F);
		writer.writeArrayItem(3.14159265358979);
		writer.writeArrayItem("anyText");
		writer.closeArray();
		
		buildExpAppendComma();
		buildExp("  \"anyArray\": [");
		buildExp("    42,");
		buildExp("    3.14,");
		buildExp("    3.14159265358979,");
		buildExp("    \"anyText\"");
		buildExp("  ]");

		assertResult();
	}
	
	@Test
	void testArrayWithThreeObjects() throws IOException {
		writer.startArray("anyArray");

		writer.startObjectInArray();
		writer.write("anyBoolean", true);
		writer.closeObjectInArray();
		
		writer.startObjectInArray();
		// keep element empty
		writer.closeObjectInArray();
		
		writer.startObjectInArray();
		writer.write("anyInteger", 42);
		writer.closeObjectInArray();
		
		writer.closeArray();
		
		buildExpAppendComma();
		buildExp("  \"anyArray\": [");
		buildExp("    {");
		buildExp("      \"anyBoolean\": true");
		buildExp("    },");
		buildExp("    {},");
		buildExp("    {");
		buildExp("      \"anyInteger\": 42");
		buildExp("    }");
		buildExp("  ]");

		assertResult();
	}
	
	@Test
	void testKeyValueWithEscapeChars() throws IOException {
		writer.writeKeyValue("a \" b \\ c", "a \t b \r c \n");
		
		buildExpAppendComma();
		buildExp("  \"a \\\" b \\\\ c\": \"a \t b \r c \n\"");

		assertResult();
	}
	
	@Test
	void testObjectInArrayErr() {
		boolean exceptionThrown = false;
		try {
			writer.startObjectInArray();
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected a key") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testArrayItemErr() {
		boolean exceptionThrown = false;
		try {
			writer.writeArrayItem("anyValue");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected a key") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testKeyInArrayErr() throws IOException {
		boolean exceptionThrown = false;
		writer.startArray("anyArray");
		try {
			writer.write("anyKey", true);
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("unexpected key") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testCloseWrongCollectionErr() {
		boolean exceptionThrown = false;
		try {
			writer.startObject("anyObject");
			writer.startArray("anyArray");
			writer.closeObject();
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected ']'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testObjectNotClosedErr() {
		boolean exceptionThrown = false;
		try {
			writer.startObject("anyObject");
			writer.close();
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected '}'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
}
