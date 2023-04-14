package com.sap.adt.abapcleaner.base;

import java.io.Closeable;
import java.io.IOException;

public interface ISettingsReader extends Closeable {
   String getPath();
   int getRequiredVersion();
   int getFileVersion();
	String getFileNameWithoutExtension();
	
   void startObject(String key) throws IOException;
   void closeObject() throws IOException;

   void startArray(String key) throws IOException;
   void closeArray() throws IOException;

   void startObjectInArray() throws IOException;
   void closeObjectInArray() throws IOException;
   
   boolean readArrayItemBool() throws IOException;
   int readArrayItemInt32() throws IOException;
   float readArrayItemFloat() throws IOException;
   double readArrayItemDouble() throws IOException;
   String readArrayItemString() throws IOException;

   boolean readBool(String key) throws IOException;
   int readInt32(String key) throws IOException;
   float readFloat(String key) throws IOException;
   double readDouble(String key) throws IOException;
   String readString(String key) throws IOException;

   KeyValuePair readKeyValue() throws IOException;
}
