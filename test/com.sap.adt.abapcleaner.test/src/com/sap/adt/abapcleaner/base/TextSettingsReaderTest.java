package com.sap.adt.abapcleaner.base;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class TextSettingsReaderTest {
	private static final int TECHNICAL_VERSION = 16;
	private static final float FLOAT_DELTA = 0.0001F;
	private static final double DOUBLE_DELTA = 0.000000001F;
	
	private ISettingsReader reader;
	private StringBuilder sbSrc = new StringBuilder();
	
	@BeforeEach
	void setUp() throws IOException {
		buildSrc("{");
		buildSrc("  \"requiredVersion\": 1,");
		buildSrc("  \"programVersion\": 16");
	}
	
	void finishBuild() throws IOException {
		buildSrc("}");
		reader = TextSettingsReader.createFromString(sbSrc.toString(), TECHNICAL_VERSION);
	}

	private void buildSrc(String line) {
		if (sbSrc.length() > 0)
			sbSrc.append(ITextSettings.LF);
		sbSrc.append(line);
	}
	
	private void buildSrcAppendComma() {
		sbSrc.append(",");
	}

	private void buildSrcAppend(String text) {
		sbSrc.append(text);
	}

	private void assertCompleted() throws IOException {
		reader.close();
	}

	@Test
	void testVersionInfoOnly() throws IOException {
		finishBuild();
		assertCompleted();
	}
	
	@Test
	void testCloseTwice() throws IOException {
		// expect that calling .close() several times does not cause an error 
		finishBuild();
		reader.close();
		reader.close();
		
		assertCompleted();
	}
	
	@Test
	void testEmptyObject() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyObject\": {}");
		finishBuild();

		reader.startObject("anyObject");
		reader.closeObject();

		assertCompleted();
	}
	
	@Test
	void testEmptyObjectCondensed() throws IOException {
		buildSrc(",\"anyObject\":{}");
		finishBuild();

		reader.startObject("anyObject");
		reader.closeObject();

		assertCompleted();
	}
	
	@Test
	void testObjectWithOneAttribute() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyObject\": {");
		buildSrc("    \"anyBoolean\": false");
		buildSrc("  }");
		finishBuild();

		reader.startObject("anyObject");
		assertFalse(reader.readBool("anyBoolean"));
		reader.closeObject();
		
		assertCompleted();
	}
	
	@Test
	void testObjectWithAttributes() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyObject\": {");
		buildSrc("    \"anyBoolean\": true,");
		buildSrc("    \"anyInteger\": 42,");
		buildSrc("    \"anyFloat\": 3.14,");
		buildSrc("    \"anyDouble\": 3.14159265358979,");
		buildSrc("    \"anyString\": \"stringValue\"");
		buildSrc("  }");
		finishBuild();

		reader.startObject("anyObject");
		assertEquals(true, reader.readBool("anyBoolean"));
		assertEquals(42, reader.readInt32("anyInteger"));
		assertEquals(3.14F, reader.readFloat("anyFloat"), FLOAT_DELTA);
		assertEquals(3.14159265358979, reader.readDouble("anyDouble"));
		assertEquals("stringValue", reader.readString("anyString"));
		reader.closeObject();

		assertCompleted();
	}
	
	@Test
	void testObjectWithAttributesCondensed() throws IOException {
		buildSrcAppend(",\"anyObject\":{\"anyBoolean\":true,\"anyInteger\":42,\"anyFloat\":3.14,\"anyDouble\":3.14159265358979,\"anyString\":\"stringValue\"}");
		finishBuild();

		reader.startObject("anyObject");
		assertEquals(true, reader.readBool("anyBoolean"));
		assertEquals(42, reader.readInt32("anyInteger"));
		assertEquals(3.14F, reader.readFloat("anyFloat"), FLOAT_DELTA);
		assertEquals(3.14159265358979, reader.readDouble("anyDouble"));
		assertEquals("stringValue", reader.readString("anyString"));
		reader.closeObject();

		assertCompleted();
	}
	
	@Test
	void testEmptyArray() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyArray\": []");
		finishBuild();

		reader.startArray("anyArray");
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithOneItem() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyArray\": [");
		buildSrc("    true");
		buildSrc("  ]");
		finishBuild();

		reader.startArray("anyArray");
		assertEquals(true, reader.readArrayItemBool());
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithOneObject() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyArray\": [");
		buildSrc("    {");
		buildSrc("      \"anyInteger\": 42");
		buildSrc("    }");
		buildSrc("  ]");
		finishBuild();

		reader.startArray("anyArray");
		reader.startObjectInArray();
		assertEquals(42, reader.readInt32("anyInteger"));
		reader.closeObjectInArray();
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithOneObjectCondensed() throws IOException {
		buildSrcAppend(",\"anyArray\":[{\"anyInteger\":42}]");
		finishBuild();

		reader.startArray("anyArray");
		reader.startObjectInArray();
		assertEquals(42, reader.readInt32("anyInteger"));
		reader.closeObjectInArray();
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithFourItems() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyArray\": [");
		buildSrc("    42,");
		buildSrc("    3.14,");
		buildSrc("    3.14159265358979,");
		buildSrc("    \"anyText\"");
		buildSrc("  ]");
		finishBuild();

		reader.startArray("anyArray");
		assertEquals(42, reader.readArrayItemInt32());
		assertEquals(3.14F, reader.readArrayItemFloat(), FLOAT_DELTA);
		assertEquals(3.14159265358979, reader.readArrayItemDouble(), DOUBLE_DELTA);
		assertEquals("anyText", reader.readArrayItemString());
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithFourItemsCondensed() throws IOException {
		buildSrcAppend(",\"anyArray\":[42,3.14,3.14159265358979,\"anyText\"]");
		finishBuild();

		reader.startArray("anyArray");
		assertEquals(42, reader.readArrayItemInt32());
		assertEquals(3.14F, reader.readArrayItemFloat(), FLOAT_DELTA);
		assertEquals(3.14159265358979, reader.readArrayItemDouble(), DOUBLE_DELTA);
		assertEquals("anyText", reader.readArrayItemString());
		reader.closeArray();
		
		assertCompleted();
	}
	
	@Test
	void testArrayWithThreeObjects() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"anyArray\": [");
		buildSrc("    {");
		buildSrc("      \"anyBoolean\": true");
		buildSrc("    },");
		buildSrc("    {},");
		buildSrc("    {");
		buildSrc("      \"anyInteger\": 42");
		buildSrc("    }");
		buildSrc("  ]");
		finishBuild();

		reader.startArray("anyArray");

		reader.startObjectInArray();
		assertEquals(true, reader.readBool("anyBoolean"));
		reader.closeObjectInArray();
		
		reader.startObjectInArray();
		// object is empty
		reader.closeObjectInArray();
		
		reader.startObjectInArray();
		assertEquals(42, reader.readInt32("anyInteger"));
		reader.closeObjectInArray();
		
		reader.closeArray();

		assertCompleted();
	}
	
	@Test
	void testArrayWithThreeObjectsCondensed() throws IOException {
		buildSrcAppend(",\"anyArray\":[{\"anyBoolean\":true},{},{\"anyInteger\":42}]");
		finishBuild();

		reader.startArray("anyArray");

		reader.startObjectInArray();
		assertEquals(true, reader.readBool("anyBoolean"));
		reader.closeObjectInArray();
		
		reader.startObjectInArray();
		// object is empty
		reader.closeObjectInArray();
		
		reader.startObjectInArray();
		assertEquals(42, reader.readInt32("anyInteger"));
		reader.closeObjectInArray();
		
		reader.closeArray();

		assertCompleted();
	}
	
	@Test
	void testKeyValueWithEscapeChars() throws IOException {
		buildSrcAppendComma();
		buildSrc("  \"a \\\" b \\\\ c\": \"a \t b \r c \n\"");
		finishBuild();

		KeyValuePair kvp = reader.readKeyValue();
		assertEquals("a \" b \\ c", kvp.key);
		assertEquals("a \t b \r c \n", kvp.value);
		
		assertCompleted();
	}
	
	@Test 
	void testVersionErr() throws IOException {
		sbSrc = new StringBuilder();
		buildSrc("{");
		buildSrc("  \"requiredVersion\": 17,");
		buildSrc("  \"programVersion\": 16");
		buildSrc("}");
		
		boolean raisedException = false;
		try {
			reader = TextSettingsReader.createFromString(sbSrc.toString(), TECHNICAL_VERSION);
		} catch (IOException ex) {
			raisedException = true;
			assertTrue(ex.getMessage().indexOf("requires technical version 17") >= 0);
		}
		assertTrue(raisedException);
	}
	
	@Test
	void testParseBooleanErr() throws IOException {
		buildSrc("  \"anyBoolean\": ,");
		buildSrc("  \"otherBoolean\": true");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readBool("anyBoolean");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected 'true' or 'false'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testParseIntErr() throws IOException {
		buildSrc("  \"anyInteger\": true");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readInt32("anyInteger");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected integer") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testParseFloatErr() throws IOException {
		buildSrc("  \"anyFloat\": \"abc\"");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readFloat("anyFloat");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected float") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testParseDoubleErr() throws IOException {
		buildSrc("  \"anyDouble\": ??");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readDouble("anyDouble");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected double") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testKeyMissingErr() throws IOException {
		// keys are required outside of an array  
		buildSrc("  42");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readArrayItemInt32();
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected a key") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testKeyUnexpectedErr() throws IOException {
		// keys are unexpected inside an array  
		buildSrc("  \"anyArray\": [");
		buildSrc("    \"anyKey\": 42");
		buildSrc("  ]");
		finishBuild();

		boolean exceptionThrown = false;
		reader.startArray("anyArray");
		try {
			reader.readInt32("anyKey");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("unexpected key") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testKeyNotFoundErr() throws IOException {
		buildSrc("  \"anyKey\": 42");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.readInt32("otherKey");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected 'otherKey'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testContentIncompleteErr() throws IOException {
		buildSrc("  \"anyObject\": ");
		reader = TextSettingsReader.createFromString(sbSrc.toString(), TECHNICAL_VERSION);

		boolean exceptionThrown = false;
		try {
			reader.startObject("anyObject");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected '{'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testArrayNotFoundErr() throws IOException {
		buildSrc("  \"anyKey\": 42");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.startArray("anyKey");
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected '['") >= 0);
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testCloseWrongItemErr() throws IOException {
		buildSrc("  \"anyObject\": {");
		buildSrc("    \"anyArray\": []");
		buildSrc("  }");
		finishBuild();

		boolean exceptionThrown = false;
		try {
			reader.startObject("anyObject");
			reader.startArray("anyArray");
			reader.closeObject();
		} catch (IOException ex) {
			exceptionThrown = true;
			assertTrue(ex.getMessage().indexOf("expected ']'") >= 0);
		}
		assertTrue(exceptionThrown);
	}
}
