package com.sap.adt.abapcleaner.base;

import java.io.File;

class SettingsReaderBase {
   protected final String path;
   protected int requiredVersion;
   protected int fileVersion;

   protected SettingsReaderBase(String path) {
   	this.path = path;
   }
   
   public final String getPath() { 
   	return path; 
   }
   
   public final int getRequiredVersion() { 
   	return requiredVersion; 
   }
   
   public final int getFileVersion() { 
   	return fileVersion; 
   }
   
   public final String getFileNameWithoutExtension() {
   	String name = new File(path).getName();
   	int dotPos = name.lastIndexOf('.');
   	return (dotPos <= 0) ? name : name.substring(0, dotPos);
   }
}
