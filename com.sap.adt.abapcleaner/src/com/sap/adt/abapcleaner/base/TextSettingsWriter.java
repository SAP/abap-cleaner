package com.sap.adt.abapcleaner.base;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Stack;

/**
 * Writer for sequential setting files in a JSON-like textual format. 
 */
public class TextSettingsWriter implements ISettingsWriter, ITextSettings {
	private static final char INDENT_CHAR = ' ';
	private static final int INDENT_STEP = 2;

	private PersistencyBase persistency;
	private String path;
	private StringWriter stringOut;
   private BufferedWriter dataOut;

   private int indent;
   private boolean hasElement;
   private Stack<Character> closingBracketStack = new Stack<>();
   
   private boolean disposed = false;
   private String result;

   public String getStringResult() { return result; }
   
   public static ISettingsWriter createForFile(PersistencyBase persistency, String path, int programVersion, int requiredVersion) throws IOException {
   	return new TextSettingsWriter(persistency, path, programVersion, requiredVersion);
   }

   public static ISettingsWriter createForString(int programVersion, int requiredVersion) throws IOException {
   	return new TextSettingsWriter(programVersion, requiredVersion);
   }

   private TextSettingsWriter(PersistencyBase persistency, String path, int programVersion, int requiredVersion) throws IOException {
   	this.persistency = persistency; 
   	this.path = path;
		stringOut = new StringWriter();
      dataOut = new BufferedWriter(stringOut);

      writeVersion(programVersion, requiredVersion);
   }
   
   private TextSettingsWriter(int programVersion, int requiredVersion) throws IOException {
   	path = null;
		stringOut = new StringWriter();
      dataOut = new BufferedWriter(stringOut);
 
      writeVersion(programVersion, requiredVersion);
   }
   
   private final void writeVersion(int programVersion, int requiredVersion) throws IOException {
   	dataOut.write(OBJECT_OPEN);
   	closingBracketStack.push(OBJECT_CLOSE);
   	indent = 1;
   	hasElement = false;
      
      write(KEY_REQUIRED_VERSION, requiredVersion);
      write(KEY_PROGRAM_VERSION, programVersion);
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
   	writeSepAndKey(key); // even with key == null
   	dataOut.write(openingBracket);
   	closingBracketStack.push(closingBracket);
   	++indent;
   	hasElement = false;
   }
   
   private final void close(char closingBracket) throws IOException {
   	// check against the expected closing bracket from the stack
   	char expClosingBracket = closingBracketStack.pop();
   	if (closingBracket != expClosingBracket) {
   		throw new IOException("expected '" + Character.toString(expClosingBracket) + "', but found '" + Character.toString(closingBracket) + "'");
   	}

   	--indent;
   	if (hasElement)
   		nextLine();
   	dataOut.write(closingBracket);
   	hasElement = true;
   }

   private final void nextLine() throws IOException {
   	dataOut.write(LF);
   	dataOut.write(StringUtil.repeatChar(INDENT_CHAR, INDENT_STEP * indent));
   }

   public final void writeArrayItem(boolean value) throws IOException {
   	write(null, value);
   }
   public final void writeArrayItem(int value) throws IOException {
   	write(null, value);
   }
   public final void writeArrayItem(float value) throws IOException {
   	write(null, value);
   }
   public final void writeArrayItem(double value) throws IOException {
   	write(null, value);
   }
   public final void writeArrayItem(String value) throws IOException {
   	write(null, value);
   }

   public final void write(String key, boolean value) throws IOException {
   	writeSepAndKey(key);
      dataOut.write(value ? VALUE_TRUE : VALUE_FALSE);
   }
   public final void write(String key, int value) throws IOException {
   	writeSepAndKey(key);
      dataOut.write(Integer.toString(value));
   }
   public final void write(String key, float value) throws IOException {
   	writeSepAndKey(key);
      dataOut.write(Float.toString(value));
   }
   public final void write(String key, double value) throws IOException {
   	writeSepAndKey(key);
      dataOut.write(Double.toString(value));
   }
   public final void write(String key, String value) throws IOException {
   	writeSepAndKey(key);
   	writeString(value);
   }
   public final void writeKeyValue(String key, String value) throws IOException {
   	writeSepAndKey(key);
   	writeString(value);
   }

   private final void writeSepAndKey(String key)  throws IOException {
   	if (hasElement)
   		dataOut.write(ELEMENT_SEP);
   	nextLine();

   	if (isKeyExpected()) {
   		if (key == null)
   			throw new IOException("expected a key");
   	} else {
   		if (key != null)
   			throw new IOException("unexpected key");
   	}
   	
   	if (key != null) {
   		writeString(key);
      	dataOut.write(KEY_VALUE_SEP);
      	dataOut.write(" ");
   	}
   	hasElement = true;
   }

   private final void writeString(String value) throws IOException {
   	dataOut.write(STRING_DELIMITER);
   	dataOut.write(StringUtil.getEscapeText(value));
   	dataOut.write(STRING_DELIMITER);
   }

   public final void close() throws IOException {
   	if (!disposed) {
   		close(OBJECT_CLOSE);
   		if (!closingBracketStack.isEmpty())
   			throw new IOException("expected '" + Character.toString(closingBracketStack.peek()) + "', but file ended");
  		}
   	Dispose(true);
   }

   protected void Dispose(boolean disposing) throws IOException {
      if (disposed)
         return;

      if (disposing) {
         dataOut.flush();
      	dataOut.close();
  			result = stringOut.toString();
  			
   		if (persistency != null && path != null) {
   			boolean skipWrite = false;
   			String dir = persistency.getDirectoryName(path);
   			if (!persistency.directoryExists(dir)) {
   				persistency.createDirectory(dir);
   			} else if (persistency.fileExists(path)) {
   				String oldText = persistency.readAllTextFromFile(path);
   				if (oldText != null && oldText.equals(result)) {
   					// write can be skipped, because the content of the file was not changed
   					// (this helps to reduce synchronization overhead for profiles)
   					skipWrite = true;
   				}
   			}
   			if (!skipWrite) {
   				persistency.writeAllTextToFile(path, result);
   			}
   		}
      }
      disposed = true;
   }
}