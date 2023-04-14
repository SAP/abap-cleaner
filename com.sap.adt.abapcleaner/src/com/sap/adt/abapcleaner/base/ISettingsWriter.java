package com.sap.adt.abapcleaner.base;

import java.io.Closeable;
import java.io.IOException;

public interface ISettingsWriter extends Closeable {
	static final String KEY_REQUIRED_VERSION = "requiredVersion";
	static final String KEY_PROGRAM_VERSION = "programVersion";

   void startObject(String key) throws IOException;
   void closeObject() throws IOException;

	void startArray(String key) throws IOException;
   void closeArray() throws IOException;

   void startObjectInArray() throws IOException;
   void closeObjectInArray() throws IOException;

   void writeArrayItem(boolean value) throws IOException;
   void writeArrayItem(int value) throws IOException;
   void writeArrayItem(float value) throws IOException;
   void writeArrayItem(double value) throws IOException;
   void writeArrayItem(String value) throws IOException;

   void write(String key, boolean value) throws IOException;
   void write(String key, int value) throws IOException;
   void write(String key, float value) throws IOException;
   void write(String key, double value) throws IOException;
   void write(String key, String value) throws IOException;
   void writeKeyValue(String key, String value) throws IOException;
   
   String getStringResult();
}
