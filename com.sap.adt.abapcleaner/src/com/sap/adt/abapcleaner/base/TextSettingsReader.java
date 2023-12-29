package com.sap.adt.abapcleaner.base;

import java.io.IOException;
import java.util.Stack;

/**
 * Reader for sequential setting files in a JSON-like textual format. 
 */
public class TextSettingsReader extends SettingsReaderBase implements ISettingsReader, ITextSettings {
   private String content;
   private int pos;
   
   private Stack<Character> closingBracketStack = new Stack<>();

   private boolean disposed = false;

   public static ISettingsReader createFromFile(PersistencyBase persistency, String path, int programVersion) throws IOException {
   	return new TextSettingsReader(persistency, path, programVersion); 
   }
   
   public static ISettingsReader createFromString(String content, int programVersion) throws IOException {
   	return new TextSettingsReader(programVersion, content); 
   }
   
   private TextSettingsReader(PersistencyBase persistency, String path, int programVersion) throws IOException {
   	super(path);

   	content = persistency.readAllTextFromFile(path);

      readVersion(programVersion);
   }

   private TextSettingsReader(int programVersion, String content) throws IOException {
   	super("");
      this.content = content;
      readVersion(programVersion);
   }

   private final void readVersion(int programVersion) throws IOException {
      pos = 0;
      skipSign(OBJECT_OPEN, true);
   	closingBracketStack.push(OBJECT_CLOSE);
      
      requiredVersion = readInt32(ISettingsWriter.KEY_REQUIRED_VERSION);
      if (programVersion < requiredVersion)
         throw new IOException("'" + path + "' requires technical version " + Integer.toString(requiredVersion) + ", but installed version is " + Integer.toString(programVersion) + ".");
      fileVersion = readInt32(ISettingsWriter.KEY_PROGRAM_VERSION);
   }

   private boolean isKeyExpected() { 
   	return !closingBracketStack.isEmpty() && closingBracketStack.peek() == OBJECT_CLOSE; 
   }
   
   public final void startObject(String key) throws IOException {
   	start(key, OBJECT_OPEN, OBJECT_CLOSE);
   }
   public final void closeObject() throws IOException {
   	close(OBJECT_CLOSE);
   }

   public final void startArray(String key) throws IOException {
   	start(key, ARRAY_OPEN, ARRAY_CLOSE);
   }
   public final void closeArray() throws IOException {
   	close(ARRAY_CLOSE);
   }

   public final void startObjectInArray() throws IOException {
   	start(null, OBJECT_OPEN, OBJECT_CLOSE);
   }
   public final void closeObjectInArray() throws IOException {
   	close(OBJECT_CLOSE);
   }

   private final void start(String key, char openingBracket, char closingBracket) throws IOException {
   	skipSepAndKey(key); // even with key == null
   	skipSign(openingBracket, true);
   	closingBracketStack.push(closingBracket);
   }
   
   private final void close(char closingBracket) throws IOException {
   	// check against the expected closing bracket from the stack
   	char expClosingBracket = closingBracketStack.pop();
   	if (closingBracket != expClosingBracket) {
   		throw new IOException("expected '" + Character.toString(expClosingBracket) + "'");
   	}

   	skipSign(closingBracket, true);
   }

   public final boolean readArrayItemBool() throws IOException {
      return readBool(null);
   }
   public final int readArrayItemInt32() throws IOException {
      return readInt32(null);
   }
   public final float readArrayItemFloat() throws IOException {
      return readFloat(null);
   }
   public final double readArrayItemDouble() throws IOException {
      return readDouble(null);
   }
   public final String readArrayItemString() throws IOException {
   	return readString(null);
   }

   public final boolean readBool(String key) throws IOException {
   	skipSepAndKey(key);
   	String value = readNonString();
   	if (value.equals(VALUE_TRUE))
   		return true;
   	else if (value.equals(VALUE_FALSE))
   		return false;
   	else
   		throw new IOException(getErrorPos() + "expected '" + VALUE_TRUE + "' or '" + VALUE_FALSE + "', but found " + value);
   }
   
   public final int readInt32(String key) throws IOException {
   	skipSepAndKey(key);
   	String value = readNonString();
   	try {
   		return Integer.parseInt(value);
   	} catch (NumberFormatException ex) {
   		throw new IOException(getErrorPos() + "expected integer, but found '" + value + "'");
   	}
   }
   
   public final float readFloat(String key) throws IOException {
   	skipSepAndKey(key);
     	String value = readNonString();
   	try {
   		return Float.parseFloat(value);
   	} catch (NumberFormatException ex) {
   		throw new IOException(getErrorPos() + "expected float, but found '" + value + "'");
   	}
   }
   
   public final double readDouble(String key) throws IOException {
   	skipSepAndKey(key);
     	String value = readNonString();
   	try {
   		return Double.parseDouble(value);
   	} catch (NumberFormatException ex) {
   		throw new IOException(getErrorPos() + "expected double, but found '" + value + "'");
   	}
   }
   
   public final String readString(String key) throws IOException {
   	skipSepAndKey(key);
   	return readString();
   }
   
   public final KeyValuePair readKeyValue() throws IOException {
   	skipSign(ELEMENT_SEP, false);
   	String key = readString();
		skipSign(KEY_VALUE_SEP, true);
   	String value = readString();

   	return new KeyValuePair(key, value);
   }
   
   private final String readString() throws IOException {
   	skipWhiteSpace();
   	skipSign(STRING_DELIMITER, true);

		StringBuilder sb = new StringBuilder();
		while (!eof()) {
			char c = content.charAt(pos);
			++pos;
			if (c == STRING_DELIMITER) {
				break;
			} 
			if (c == '\\' && pos < content.length()) {
				c = content.charAt(pos);
				++pos;
			}
			sb.append(c);
		}
		return sb.toString();
   }

   private final String readNonString() throws IOException {
   	skipWhiteSpace();
   	int start = pos;
   	while (!eof() && NON_STRING_DELIMITERS.indexOf(content.charAt(pos)) < 0)
			++pos;
   	return content.substring(start, pos);
   }

   private final void skipSepAndKey(String key)  throws IOException {
   	skipSign(ELEMENT_SEP, false);

   	if (isKeyExpected()) {
   		if (key == null)
   			throw new IOException("expected a key");
   	} else {
   		if (key != null)
   			throw new IOException("unexpected key");
   	}
   	

   	if (key != null) {
   		skipWhiteSpace();
   		String foundKey = readString();
   		if (!key.equals(foundKey))
   			throw new IOException(getErrorPos() + "expected '" + key + "', but found '" + foundKey);
   		skipSign(KEY_VALUE_SEP, true);
   	}
   }

   private void skipSign(char sign, boolean mandatory) throws IOException {
   	skipWhiteSpace();
   	
		if (!eof() && content.charAt(pos) == sign) {
			++pos;
		} else if (mandatory) {
			String found = eof() ? "" : Character.toString(content.charAt(pos));
   		throw new IOException(getErrorPos() + "expected '" + Character.toString(sign) + "', but found '" + found + "'");
		}
   }

   private void skipWhiteSpace() {
   	while (!eof() && WHITESPACE_CHARS.indexOf(content.charAt(pos)) >= 0)
   		++pos;
   }

   private boolean eof() {
   	return (pos >= content.length());
   }

   public final void close() throws IOException {
   	if (!disposed) {
   		// we don't enforce the file to end at the current position, 
   		// thus allowing older versions to read at least the common beginning of files created with newer versions
   		// (i.e. REQUIRED_VERSION does not necessarily have to be increased if content is added at the end)
   		// readEnd();
   	}
      Dispose(true);
   }

   @SuppressWarnings("unused")
	private void readEnd() throws IOException {
		skipSign(OBJECT_CLOSE, true);
		skipWhiteSpace();
		if (!eof())
			throw new IOException(getErrorPos() + "expected end of file");

   }

   private String getErrorPos() {
   	int lineNum = 0;
   	int posInLine = pos;
   	boolean foundPosInLine = false;

   	int p = pos;
   	while (p > 0) {
   		--p;
   		char c = content.charAt(p);
   		if (!foundPosInLine && (c == '\r' || c == '\n')) {
   			posInLine = pos - p;
   			foundPosInLine = true;
   		} else if (c == '\n') { // only count \n, since otherwise, \r\n would count as 2 lines 
   			++lineNum;
   		}
   	}
   	return "line " + Cult.format(lineNum) + ", pos " + Cult.format(posInLine) + ": ";
   }
   
   protected void Dispose(boolean disposing) throws IOException {
      if (disposed)
         return;

      if (disposing) {
      	// nothing to do here 
      }
      disposed = true;
   }
}
