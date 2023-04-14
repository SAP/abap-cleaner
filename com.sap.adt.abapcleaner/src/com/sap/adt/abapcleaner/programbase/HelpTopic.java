package com.sap.adt.abapcleaner.programbase;

/**
 * Enumeration of the topics for which help pages are provided in the online documentation.
 */
public enum HelpTopic  {
	/** links to all other topics; contains short description and installation instructions */
   README("README"), 

   /** how-to descriptions for the most important use cases */
   USAGE("docs/usage"),  
   
   /** description of the main application window */
   MAIN("docs/main-window"),  
   
   /** description of the window to edit {@link Profile}s and {@link Rule}s */
   PROFILES("docs/profiles"),
   
   /** list of available cleanup {@link Rule}s with links to detail pages */
   RULES("docs/rules"),

   /** release notes with added rules and configuration, bugfixes etc. */
   RELEASE_NOTES("docs/release-notes");  
   
   public static final int SIZE = java.lang.Integer.SIZE;

   private String pageName;
   
   private HelpTopic(String pageName) {
   	this.pageName = pageName;
   }
   
   public String getPageName() { 
   	return pageName; 
   }

   public int getValue() {
      return this.ordinal();
   }

   public static HelpTopic forValue(int value) {
      return values()[value];
   }
}