package com.sap.adt.abapcleaner.base;

public enum Language {
	ABAP,
	DDL, // Data Definition Language (particularly CDS DDL)
	DCL, // Data Control Language for CDS roles (DEFINE ROLE) and CDS access policies (DEFINE ACCESSPOLICY)
	SQLSCRIPT,
	SQL, 
	GRAPH, 
	LLANG,
	OTHER,
	NOT_SUPPORTED; // esp. RAP Behavior Definition Language (BDL)
	
	public static Language preview(String text) {
		final String BDL_OR_DDL_COMMENT = "//";
		final char[] LINE_FEED = new char[] { 10, 13 };
		final char[] SPACE_OR_LINE_FEED = new char[] { 10, 13, ' ' };

		Language bestGuess = Language.ABAP;
		int pos = 0;
		
		// skip initial spaces, line feeds and // comments 
		while (pos < text.length()) {
			// skip spaces or line feeds (if any)
			pos = StringUtil.indexOfNotAnyOf(text, SPACE_OR_LINE_FEED, pos);
			if (pos < 0)
				return bestGuess;
			
			// skip "//" comments, which are used in both DDL and BDL (RAP behavior definition language)
			if (!StringUtil.containsAnyAt(text, pos, BDL_OR_DDL_COMMENT)) 
				break;
			bestGuess = Language.DDL; // only used if no code is found in the document
			pos = StringUtil.indexOfAny(text, LINE_FEED, pos);
			if (pos < 0) {
				return bestGuess;
			}
		}
		
		// distinguish between (CDS) DDL and ABAP code
		if (StringUtil.containsAnyAt(text, pos, "managed", "unmanaged", "abstract;", "projection;", "interface;")) {
			// cleanup of RAP behavior definition language (BDL) is not supported
			return Language.NOT_SUPPORTED;
			
			// cp. Command.getDdlOrDclEntityNameToken()
		} else if (StringUtil.containsAnyAt(text, pos, 
				com.sap.adt.abapcleaner.base.DDL.ANNOTATION_SIGN_STRING, 
				com.sap.adt.abapcleaner.base.DDL.LINE_END_COMMENT, 
				com.sap.adt.abapcleaner.base.DDL.LINE_END_MINUS_COMMENT, 
				com.sap.adt.abapcleaner.base.DDL.ASTERISK_COMMENT_START)
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "abstract", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "custom", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "view", "[entity]")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "table", "function")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "hierarchy")
			|| StringUtil.containsAtIgnoringCase(text, pos, "define", "transient", "view", "[entity]")

			// extension
			|| StringUtil.containsAtIgnoringCase(text, pos, "extend", "abstract", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "extend", "custom", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "extend", "view", "[entity]")
			|| StringUtil.containsAtIgnoringCase(text, pos, "annotate", "view")
			|| StringUtil.containsAtIgnoringCase(text, pos, "annotate", "entity")

			// ABAP Dictionary Objects in ADT
			|| StringUtil.containsAtIgnoringCase(text, pos, "define", "structure")
			|| StringUtil.containsAtIgnoringCase(text, pos, "define", "table")) {

			return Language.DDL; // may be corrected to .DCL later if Parser.parse() finds "[DEFINE] ROLE" or "[DEFINE] ACCESSPOLICY" after initial annotations or comments

		} else if (StringUtil.containsAtIgnoringCase(text, pos, "[define]", "role")
				  || StringUtil.containsAtIgnoringCase(text, pos, "[define]", "accesspolicy")) {
			
			return Language.DCL;

		} else {
			return bestGuess;
		}
	}
	
}
