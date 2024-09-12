package com.sap.adt.abapcleaner.base;

public enum Language {
	ABAP,
	DDL, // Data Definition Language (particularly CDS DDL)
	DCL, // Data Control Language for CDS roles (DEFINE ROLE) and CDS access policies (DEFINE ACCESSPOLICY)
	SQLSCRIPT,
	SQL, 
	GRAPH, 
	LLANG,
	OTHER;
	
	public static Language preview(String text) {
		int pos = 0;
		
		// skip initial spaces or line feeds 
		while (pos < text.length()) {
			char c = text.charAt(pos);
			if (c == 10 || c == 13 || c == ' ') {
				++pos;
				continue;
			}
			break;
		}
		
		// distinguish between (CDS) DDL and ABAP code
		// cp. Command.getDdlOrDclEntityNameToken()
		if (StringUtil.containsAnyAt(text, pos, 
				com.sap.adt.abapcleaner.base.DDL.ANNOTATION_SIGN_STRING, 
				com.sap.adt.abapcleaner.base.DDL.LINE_END_COMMENT, 
				com.sap.adt.abapcleaner.base.DDL.LINE_END_MINUS_COMMENT, 
				com.sap.adt.abapcleaner.base.DDL.ASTERISK_COMMENT_START)
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "abstract", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "custom", "entity")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "[root]", "view", "[entity]")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "table", "function")
			|| StringUtil.containsAtIgnoringCase(text, pos, "[define]", "hierarchy")
			|| StringUtil.containsAtIgnoringCase(text, pos, "define", "transient", "view")

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
			return Language.ABAP;
		}
	}
	
}
