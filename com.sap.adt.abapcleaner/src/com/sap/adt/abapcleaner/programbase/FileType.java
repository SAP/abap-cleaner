package com.sap.adt.abapcleaner.programbase;

/**
 * Enumeration of the various types of files that can be loaded, saved or displayed by the program. 
 * This helps to encapsulate the knowledge of concrete file names, extensions, folders etc. in {@link Persistency}. 
 */
public enum FileType  {
   CONFIG_TEXT,
   
   LAST_SESSION_BINARY,
	LAST_SESSION_TEXT,

	SETTINGS_MAIN_BINARY,
	SETTINGS_MAIN_TEXT,
   
	PROFILE_BINARY,
	PROFILE_TEXT,

	HELP,
   CODE,
   ERROR_LOG;

   public static final int SIZE = Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static FileType forValue(int value) {
      return values()[value];
   }
}