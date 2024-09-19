package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.base.ABAP.SyField;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.RuleID;

import java.util.*;

/**
 * <p>From ABAP cleaner perspective, any ABAP {@link Code} consists of a sequence of Commands.
 * A Command contains a sequence of {@link Token}s up to the next '.' sign, plus possibly the final comment on the same line.
 * Chains (e.g. 'CLEAR: a, b, c.') are stored in one single Command.
 * Full-line comments are a Command, too, unless they are located between Tokens inside a Command.
 * {@link #toString()} exactly restores the source code from which the Command was parsed.</p> 
 * 
 * <p>The attributes {@link #prev} and {@link #next} provide the sequence of Commands in order of their appearance within the code; 
 * at the same time, a hierarchical view of the Commands is provided via the Command attributes 
 * {@link #parent}, {@link #firstChild}, {@link #lastChild}, {@link #prevSibling} and {@link #nextSibling}. 
 * In this hierarchical view, Commands that open a block (such as CLASS, METHOD, IF, LOOP, DO etc.) 
 * are the parent Command of the Commands inside that block; 
 * the closing Command (ENDCLASS, ENDMETHOD, ENDIF etc.) is the {@link #nextSibling} of the parent
 * and may itself open the next block (ELSEIF, ELSE, CATCH).   
 * </p>
 */
public class Command {
	public final static String[] declarationKeywords = new String[] { "CONSTANTS", "DATA", "FIELD-SYMBOLS", "TYPES", "CLASS-DATA", "STATICS" }; // "DATA(", "FINAL(" and "FIELD-SYMBOL(" do NOT belong here
	public final static String[] declarationKeywordsReservingMemory = new String[] { "CONSTANTS", "DATA", "FIELD-SYMBOLS", "CLASS-DATA", "STATICS" }; 
	public final static String[] declarationKeywordsOnlyInClassDef = new String[] { "ALIASES", "INTERFACES", "CLASS-DATA", "CLASS-EVENTS", "CLASS-METHODS", "METHODS", "EVENTS" };

	private static String getLevelOpenerKey(String text) {
		return AbapCult.toUpper(text);
	}

	private static String getLevelCloserKey(String text) {
		return AbapCult.toUpper(text);
	}

	private static HashMap<String, LevelCloser> levelClosers = new HashMap<String, LevelCloser>();
	private static HashMap<String, LevelOpener> levelOpeners = new HashMap<String, LevelOpener>();

	private static LevelOpener ddlLevelOpenerBrace;
	private static LevelCloser ddlLevelCloserBrace;
	private static LevelOpener ddlLevelOpenerParameters;
	private static LevelCloser ddlLevelCloserParameters;
	private static LevelOpener ddlLevelOpenerSelect;
	private static LevelCloser ddlLevelCloserSelect;

	// provides runtime-unique IDs of Command instances for serialization
	private static int globalID = 0;

	private Code parentCode;
	private final int iD;
	private Command parent;
	private Command prev;
	private Command next;
	private Command prevSibling;
	private Command nextSibling;
	private Command firstChild;
	private Command lastChild;

	public Command originalCommand;

	Token firstToken;
	Token lastToken;
	int tokenCount;

	/** contains the number of 'BEGIN OF' minus the number of 'END OF' in this Command */
	private int blockLevelDiff;  
	public int getBlockLevelDiff() { return blockLevelDiff; }

	/** contains the cumulated {@link #blockLevelDiff} of all previous Commands */
	private int initialBlockLevel;  
	public int getInitialBlockLevel() { return initialBlockLevel; }

	private int indentAdd;  

	/** the start index of this Command in the source text */
	private int sourceTextStart;
	/** the end index of this Command in the source text */
	private int sourceTextEnd;
	/** the original number of line breaks before this Command, as found in the source code from which this Command was parsed */
	private int sourceLineBreaksBefore; 
	/** the 1-based original start line number of this Command, as found in the source code from which this Command was parsed */
	private int sourceLineNumStart; 
	/** the 1-based original end line number of this Command, as found in the source code from which this Command was parsed */
	private int sourceLineNumLast; 
	private Language language; // may only be modified from .DDL to .DCL while the Command is built 
	public final Language getLanguage() { return language; }
	private ChangeControl changeControl;

	private LevelOpener usedLevelOpener;
	private LevelCloser usedLevelCloser;

	/** the number of chain colons ':' within this Command; if further colons are listed after the first colon of a 
	 * chained statement, they are handled like blanks (but do not provoke a syntax error) 
	 * see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenchained_statements.htm">Chained Statements</a> */
	private int chainColonCount;

	/** the number of tokens in this command that were rated with ErrorState.Erroneous by the RND Parser when the Command was first parsed */
	private int errorCountBeforeCleanup;
	
	public final Code getParentCode() { return parentCode; }
	final void setParentCode(Code value) { parentCode = value; }

	public final Command getParent() { return parent; }
	final void setParent(Command value) { parent = value; }

	public final Command getPrev() { return prev; }
	final void setPrev(Command value) { prev = value; }

	public final Command getNext() { return next; }
	final void setNext(Command value) { next = value; }

	public final Command getPrevSibling() { return prevSibling; }
	final void setPrevSibling(Command value) { prevSibling = value; }

	public final Command getNextSibling() { return nextSibling; }
	final void setNextSibling(Command value) { nextSibling = value; }

	public final Command getFirstChild() { return firstChild; }
	final void setFirstChild(Command value) { firstChild = value; }

	public final Command getLastChild() { return lastChild; }
	final void setLastChild(Command value) { lastChild = value; }

	public final Token getFirstToken() { return firstToken; }
	public final Token getLastToken() { return lastToken; }

	/** returns the first Token that is not a pragma or comment (or null if no such Token exists in the Command) */
	public final Token getFirstCodeToken() { return firstToken.getThisOrNextCodeToken(); }

	public final int getFirstTokenLineBreaks() { return firstToken.lineBreaks; }

	public final int getSourceTextStart() { return sourceTextStart; }
	public final int getSourceTextEnd() { return sourceTextEnd; }

	public final int getSourceLineBreaksBefore() { return sourceLineBreaksBefore; }
	
	public final int getSourceLineNumStart() { return sourceLineNumStart; }
	public final int getSourceLineNumLast() { return sourceLineNumLast; }
	public final boolean containsSourceLineNum(int sourceLineNum) { return sourceLineNumStart <= sourceLineNum && sourceLineNumLast >= sourceLineNum; }

	public final ChangeControl getChangeControl() { return changeControl; }

	public final int getErrorTokenCountBeforeCleanup() { return errorCountBeforeCleanup; }

	public final String getIdString() { return Integer.toHexString(iD) + ":"; }

	public final boolean getOpensLevel() { return (usedLevelOpener != null); }

	public final boolean getClosesLevel() { return (usedLevelCloser != null); }

	public final Token getLastCodeToken() { return (lastToken.isCode() ? lastToken : lastToken.getPrevCodeToken()); }

	public final boolean isAsteriskCommentLine() { return firstToken.isAsteriskCommentLine(); }

	public final boolean isCommentLine() { return firstToken.isCommentLine() && firstToken.getNext() == null; }

	final boolean isEmpty() { return (tokenCount == 1 && StringUtil.isNullOrEmpty(firstToken.text)); }

	public final boolean isQuotMarkCommentLine() { return firstToken.isQuotMarkCommentLine(); }

	public final boolean isAbapDoc() { return firstToken.isAbapDocCommentLine(); }

	private boolean isLevelOpener(String openingKeyword) {
		return (usedLevelOpener != null) && AbapCult.stringEquals(usedLevelOpener.text, openingKeyword, true);
	}

	private boolean isLevelCloser(String closingKeyword) {
		return (usedLevelCloser != null) && AbapCult.stringEquals(usedLevelCloser.text, closingKeyword, true);
	}

	public boolean isInterfaceStart() { return isLevelOpener("INTERFACE"); }

	public boolean isInterfaceEnd() { return isLevelCloser("ENDINTERFACE"); }

	public final boolean isClassStart() { return isLevelOpener("CLASS"); }

	public final boolean isClassEnd() { return isLevelCloser("ENDCLASS"); }

	public final boolean isClassOrInterfaceStart() { return isClassStart() || isInterfaceStart(); }

	public final boolean isClassOrInterfaceEnd() { return isClassEnd() || isInterfaceEnd(); }

	private boolean isClassDefinitionStart;
	public final boolean isClassDefinitionStart() { return isClassDefinitionStart; }

	private boolean isClassImplementationStart;
	public final boolean isClassImplementationStart() { return isClassImplementationStart; }
	
	public final boolean isMethodFunctionOrFormStart() { return isMethodStart() || isFunctionStart() || isFormStart(); }

	public final boolean isMethodFunctionFormOrEventBlockStart() { return isMethodFunctionOrFormStart() || startsEventBlock(); }

	public final boolean isDeclarationSectionStart() { return firstToken.matchesOnSiblings(true, "PRIVATE|PROTECTED|PUBLIC", "SECTION"); }

	public final boolean isReportSectionStart() { return (usedLevelOpener != null) && !usedLevelOpener.requiresCloser; }

	public final boolean isReportOrDeclarationSectionStart() { return isReportSectionStart() || isDeclarationSectionStart(); }

	public final boolean startsLocalVariableContext() { return (usedLevelOpener != null) && usedLevelOpener.startsLocalVariableContext; }
	
	public final boolean endsLocalVariableContext() { return (usedLevelCloser != null) && (prevSibling != null) && (prevSibling.usedLevelOpener != null) && prevSibling.usedLevelOpener.startsLocalVariableContext; }
	
	public final boolean startsEventBlock() { return (usedLevelOpener != null) && usedLevelOpener.startsEventBlock; }
	
	public final boolean endsEventBlock() { return (usedLevelCloser != null) && (prevSibling != null) && (prevSibling.usedLevelOpener != null) && prevSibling.usedLevelOpener.startsEventBlock; }
	
	public final boolean isMethodFunctionOrFormEnd() { return isMethodEnd() || isFunctionEnd() || isFormEnd(); }

	public final boolean isMethodFunctionFormOrEventBlockEnd() { return isMethodFunctionOrFormEnd() || endsEventBlock(); }

	public final boolean isMethodStart() { return isLevelOpener("METHOD"); }

	public final boolean isMethodEnd() { return isLevelCloser("ENDMETHOD"); }

	public final boolean isFunctionStart() { return isLevelOpener("FUNCTION"); }

	public final boolean isFunctionEnd() { return isLevelCloser("ENDFUNCTION"); }

	public final boolean isFormStart() { return isLevelOpener("FORM"); }

	public final boolean isFormEnd() { return isLevelCloser("ENDFORM"); }

	public final boolean isEndIf() { return isLevelCloser("ENDIF"); }

	public final boolean isDeclaration() { return firstCodeTokenIsAnyKeyword(declarationKeywords); }

	public final boolean isDeclarationInclude() { return getFirstCodeToken() != null && getFirstCodeToken().matchesOnSiblings(true, "INCLUDE", "TYPE|STRUCTURE"); }

	// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselection-screen_layout.htm
	public final boolean isSelectionScreenElement() { return firstCodeTokenIsAnyKeyword("SELECTION-SCREEN", "PARAMETERS", "SELECT-OPTIONS"); } 

	public final boolean isDeclarationInClassDef() { return firstCodeTokenIsAnyKeyword(declarationKeywordsOnlyInClassDef); }

	public final boolean containsChainColon() { return (chainColonCount > 0); }
	
	public final boolean containsComma() { return firstToken.matchesOnSiblings(true, TokenSearch.ASTERISK, ","); }
	
	public final boolean isLateChain() { return containsChainColon() && !isSimpleChain(); }

	public final boolean startsLoop() { return getOpensLevel() && firstCodeTokenIsAnyKeyword(ABAP.loopKeywords); }
	
	public final boolean endsLoop() { return getClosesLevel() && firstCodeTokenIsAnyKeyword(ABAP.loopEndKeywords); }
	
	public boolean isTryStart() { return isLevelOpener("TRY"); }

	public boolean isTryEnd() { return isLevelCloser("ENDTRY"); }

	public boolean isCatch() { return isLevelOpener("CATCH"); }

	private boolean isCleanup() { return isLevelOpener("CLEANUP"); } // in TRY ... CATCH ... CLEANUP ... ENDTRY

	// private boolean isCaseStart() { return isLevelOpener("CASE"); }

	private boolean isWhenStart() { return isLevelOpener("WHEN"); }

	public final boolean isFirstCommandInCode() { return (prev == null); }

	public final String getSourceName() { return (parentCode == null) ? null : parentCode.sourceName; }

	public final boolean isAbap() { return language == Language.ABAP; }
	
	public final boolean isDdl() { return language == Language.DDL; }
	
	public final boolean isDcl() { return language == Language.DCL; }
	
	public final boolean isDdlOrDcl() { return language == Language.DDL || language == Language.DCL; }
	
	public final boolean isSqlScript() { return language == Language.SQLSCRIPT; }
	
	public final boolean firstCodeTokenIsKeyword(String text) {
		Token token = getFirstCodeToken();
		return (token != null && token.isKeyword(text)); 
	}
	
	public final boolean firstCodeTokenIsAnyKeyword(String... texts) { 
		Token token = getFirstCodeToken();
		return (token != null && token.isAnyKeyword(texts)); 
	}
	
	public final boolean firstCodeTokenTextEqualsAny(String... texts) { 
		Token token = getFirstCodeToken();
		return (token != null && token.textEqualsAny(texts)); 
	}
	
	public final boolean lastCodeTokenIsKeyword(String text) {
		// only for DDL
		Token token = getLastCodeToken();
		return (token != null && token.isKeyword(text)); 
	}
	
	public final boolean lastCodeTokenIsAnyKeyword(String... texts) {
		// only for DDL
		Token token = getLastCodeToken();
		return (token != null && token.isAnyKeyword(texts)); 
	}
	
	public final boolean isIntroductoryStatement() {
		return firstToken.isAnyKeyword("CLASS-POOL", "FUNCTION-POOL", "INTERFACE-POOL", "PROGRAM", "REPORT", "TYPE-POOL");
	}

	public final boolean hasChildren() { return (firstChild != null); }

	public final boolean wasRemovedFromCode() { return (prev == null && parentCode.firstCommand != this); }
	
	private static class LevelOpener {
		final String text;
		final boolean startsEventBlock;
		final boolean startsLocalVariableContext;
		final boolean requiresCloser;
		private HashMap<String, LevelCloser> closers = new HashMap<String, LevelCloser>();

		LevelOpener(String text, boolean startsEventBlock, boolean startsLocalVariableContext, boolean requiresCloser) {
			this.text = text;
			this.startsEventBlock = startsEventBlock;
			this.startsLocalVariableContext = startsLocalVariableContext;
			this.requiresCloser = requiresCloser;
		}

		final void addCloser(LevelCloser closer) {
			String key = getLevelCloserKey(closer.text);
			closers.put(key, closer);
		}

		final String getClosersList() { return getClosersList(", "); }
		final String getClosersList(String separator) {
			StringBuilder result = new StringBuilder();
			for (LevelCloser closer : closers.values()) {
				if (result.length() > 0)
					result.append(separator);
				result.append(closer.text);
			}
			return result.toString();
		}
	}

	private static class LevelCloser {
		final String text;
		private HashMap<String, LevelOpener> openers = new HashMap<String, LevelOpener>();

		final HashMap<String, LevelOpener> getOpeners() { return openers; }

		boolean requiresOpener;

		LevelCloser(String text) {
			this.text = text;
			requiresOpener = true;
		}

		final void addOpener(LevelOpener opener) {
			String key = getLevelCloserKey(opener.text);
			openers.put(key, opener);
		}

		final String getOpenersList() { return getOpenersList("/"); }
		final String getOpenersList(String separator) {
			StringBuilder result = new StringBuilder();
			for (Map.Entry<String, LevelOpener> kvp : openers.entrySet()) {
				if (result.length() > 0)
					result.append("/");
				result.append(kvp.getValue().text);
			}
			return result.toString();
		}
		
		final boolean isCloserFor(LevelOpener levelOpener) {
			return getOpeners().containsKey(getLevelCloserKey(levelOpener.text));
		}
	}

	static {
		// see in ABAP Reference, chapter "ABAP Statements, Overview" all statements shown with "...":
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_statements_overview.htm
		
		// ----------------------------------------------------------------------
		// Modularization Statements

		// Procedures
		addLevelOpener("FUNCTION", false, true, true, "ENDFUNCTION");
		addLevelOpener("METHOD", false, true, true, "ENDMETHOD");
		
		// Dialog Modules
		addLevelOpener("MODULE", false, false, true, "ENDMODULE");
		
		// Event Blocks
		// events such as 'AT LINE-SELECTION' can appear multiple times in the same report, therefore all events - including 
		// 'AT LINE-SELECTION' itself! - can be closed with 'AT LINE-SELECTION' etc. 
		// obsolete event blocks are included: "START-OF-EDITING", "END-OF-SELECTION", "END-OF-EDITING"
		final String[] eventClosers = new String[] { "AT LINE-SELECTION", "AT SELECTION-SCREEN", "AT USER-COMMAND", "END-OF-PAGE", 
																	"INITIALIZATION", "LOAD-OF-PROGRAM", "START-OF-SELECTION", "START-OF-EDITING", "END-OF-SELECTION", "END-OF-EDITING", "TOP-OF-PAGE", "CLASS", "FORM", "MODULE" };
		// the parameter startsLocalVariableContext is usually false, because apart from FORM...ENDFORM and FUNCTION...ENDFUNCTION, 
		// only the event blocks 'AT SELECTION-SCREEN ...' and 'GET ...' (which are implemented internally as procedures) can contain local data, 
		// while declarative statements in all other event blocks are part of the global data declarations of the ABAP program, 
		// and are visible in all subsequent processing blocks 
		// (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenevent_blocks.htm)
		addLevelOpener("REPORT", false, false, false, eventClosers);
		addLevelOpener("AT LINE-SELECTION",   true, false, false, eventClosers);
		addLevelOpener("AT SELECTION-SCREEN", true, true,  false, eventClosers); // only this starts a local variable context!
		addLevelOpener("AT USER-COMMAND",     true, false, false, eventClosers);
		addLevelOpener("END-OF-PAGE",         true, false, false, eventClosers);
		addLevelOpener("INITIALIZATION",      true, false, false, eventClosers);
		addLevelOpener("LOAD-OF-PROGRAM",     true, false, false, eventClosers);
		addLevelOpener("START-OF-SELECTION",  true, false, false, eventClosers);
		addLevelOpener("START-OF-EDITING",    true, false, false, eventClosers); // obsolete
		addLevelOpener("END-OF-SELECTION",    true, false, false, eventClosers); // obsolete
		addLevelOpener("END-OF-EDITING",      true, false, false, eventClosers); // obsolete
		addLevelOpener("TOP-OF-PAGE",         true, false, false, eventClosers);

		// Source Code Modules
		addInnerLevelOpener("DEFINE", "END-OF-DEFINITION"); 
		
		// ----------------------------------------------------------------------
		// Classes and Interfaces
		
		addLevelOpener("CLASS", false, false, true, "PUBLIC", "PROTECTED", "PRIVATE", "ENDCLASS");
		addLevelOpener("PUBLIC", false, false, true, "PROTECTED", "PRIVATE", "ENDCLASS");
		addLevelOpener("PROTECTED", false, false, true, "PRIVATE", "ENDCLASS");
		addLevelOpener("PRIVATE", false, false, true, "ENDCLASS");
		addLevelOpener("INTERFACE", false, false, true, "ENDINTERFACE");
		
		// ----------------------------------------------------------------------
		// Program Flow Logic
		
		// Control Structures
		addInnerLevelOpener("DO", "ENDDO");

		addInnerLevelOpener("CASE", "WHEN", "ENDCASE"); // includes CASE TYPE OF ... WHEN TYPE ... ENDCASE
		addInnerLevelOpener("WHEN", "WHEN", "ENDCASE");

		addInnerLevelOpener("IF", "ELSEIF", "ELSE", "ENDIF");
		addInnerLevelOpener("ELSEIF", "ELSEIF", "ELSE", "ENDIF");
		addInnerLevelOpener("ELSE", "ENDIF");

		addInnerLevelOpener("WHILE", "ENDWHILE");

		// Exception Handling
		addInnerLevelOpener("TRY", "CATCH", "CLEANUP", "ENDTRY");
		addInnerLevelOpener("CATCH", "CATCH", "CLEANUP", "ENDTRY");
		addInnerLevelOpener("CLEANUP", "ENDTRY");

		// ----------------------------------------------------------------------
		// Processing Internal Data 
		
		// Internal Tables
		addInnerLevelOpener("LOOP", "ENDLOOP"); // includes LOOP AT ... ENDLOOP and LOOP AT GROUP ... ENDLOOP (as well as LOOP AT SCREEN ... ENDLOOP for Dynpros)
		addInnerLevelOpener("AT", "ENDAT");

		// ----------------------------------------------------------------------
		// Processing External Data 
		
		// ABAP SQL
		// under certain conditions, SELECT / WITH have no ENDSELECT / ENDWITH, see .finishBuild() and .opensSelectLoop()
		addInnerLevelOpener("SELECT", "ENDSELECT"); 
		addInnerLevelOpener("WITH", "ENDWITH");

		// Native SQL
		addInnerLevelOpener("EXEC", "ENDEXEC"); 
		
		// Data Consistency
		addInnerLevelOpener("AUTHORITY-CHECK DISABLE BEGIN", "AUTHORITY-CHECK DISABLE END");

		// ----------------------------------------------------------------------
		// (Miscellaneous)
		
		// Program Editing: Testing and Checking Programs
		addInnerLevelOpener("TEST-SEAM", "END-TEST-SEAM"); 
		addInnerLevelOpener("TEST-INJECTION", "END-TEST-INJECTION"); 
		
		// Enhancements: Source Code Enhancements
		addInnerLevelOpener("ENHANCEMENT", "ENDENHANCEMENT");
		addInnerLevelOpener("ENHANCEMENT-SECTION", "END-ENHANCEMENT-SECTION");

		// Statements for Experts 
		addInnerLevelOpener("PROVIDE", "ENDPROVIDE");

		// ----------------------------------------------------------------------
		// Obsolete Statements
		
		// Obsolete Modularization 
		addLevelOpener("FORM", false, true, true, "ENDFORM");

		// Obsolete Control Structure
		addInnerLevelOpener("ON CHANGE OF", "ELSE", "ENDON");
		addInnerLevelOpener("ELSE", "ENDON");
		
		// Obsolete Exception Handling
		addInnerLevelOpener("CATCH SYSTEM-EXCEPTIONS", "ENDCATCH");
		
		// CDS DDL: use dedicated level openers and closers for the { select list }  
		ddlLevelOpenerBrace = new LevelOpener(DDL.BRACE_OPEN_STRING, false, false, true);
		ddlLevelCloserBrace = new LevelCloser(DDL.BRACE_CLOSE_STRING);
		ddlLevelOpenerBrace.addCloser(ddlLevelCloserBrace);
		ddlLevelCloserBrace.addOpener(ddlLevelOpenerBrace);

		ddlLevelOpenerParameters = new LevelOpener("PARAMETERS", false, false, true);
		ddlLevelCloserParameters = new LevelCloser("AS"); // also used for "RETURNS", DDL.BRACE_OPEN_STRING
		ddlLevelOpenerParameters.addCloser(ddlLevelCloserParameters);
		ddlLevelCloserParameters.addOpener(ddlLevelOpenerParameters);

		ddlLevelOpenerSelect = new LevelOpener("SELECT", false, false, true);
		ddlLevelCloserSelect = new LevelCloser("FROM"); 
		ddlLevelOpenerSelect.addCloser(ddlLevelCloserSelect);
		ddlLevelCloserSelect.addOpener(ddlLevelOpenerSelect);
	}

	
	private static void addInnerLevelOpener(String openerText, String... closerTexts) {
		addLevelOpener(openerText, false, false, true, closerTexts);
	}

	private static void addLevelOpener(String openerText, boolean startsEventBlock, boolean startsLocalVariableContext, boolean requiresCloser, String... closerTexts) {
		String openerKey = getLevelOpenerKey(openerText);
		LevelOpener levelOpener;
		if (levelOpeners.containsKey(openerKey))
			levelOpener = levelOpeners.get(openerKey);
		else {
			levelOpener = new LevelOpener(openerText, startsEventBlock, startsLocalVariableContext, requiresCloser);
			levelOpeners.put(openerKey, levelOpener);
		}
		for (String closerText : closerTexts) {
			String closerKey = getLevelOpenerKey(closerText);
			LevelCloser levelCloser;
			if (levelClosers.containsKey(closerKey))
				levelCloser = levelClosers.get(closerKey);
			else {
				levelCloser = new LevelCloser(closerText);
				levelClosers.put(closerKey, levelCloser);
			}
			levelOpener.addCloser(levelCloser);
			if (!requiresCloser)
				levelCloser.requiresOpener = false;
			levelCloser.addOpener(levelOpener);
		}
	}

	// ----------------------------------------------------------------------

	/**
	 * Creates a new Command (while parsing) and appends it to the supplied {@link Code}.
	 */
	static Command create(Code parentCode, Token firstToken, Language language) {
		return new Command(parentCode, firstToken, true, null, language);
	}
	
	/**
	 * Creates a new Command (while executing a {@link Rule}) that belongs to the supplied {@link #originalCommand}
	 * (splitting out parts of its code, adding a comment etc.)
	 */
	public static Command create(Token firstToken, Command originalCommand) {
		return new Command(originalCommand.parentCode, firstToken, false, originalCommand, Language.ABAP);
	}
	
	/**
	 * Creates a new Command (while executing a {@link Rule}) that belongs to the supplied {@link #originalCommand}
	 * (splitting out parts of its code, adding a comment etc.)
	 */
	public static Command create(Token firstToken, Command originalCommand, Language language) {
		return new Command(originalCommand.parentCode, firstToken, false, originalCommand, language);
	}
	
	private Command(Code parentCode, Token firstToken, boolean appendToCode, Command originalCommand, Language language) {
		if (firstToken == null)
			throw new NullPointerException("firstToken");

		this.parentCode = parentCode;
		this.firstToken = firstToken;
		lastToken = firstToken;
		tokenCount = 1;
		iD = ++globalID;
		this.language = language;
		
		this.originalCommand = originalCommand;

		if (appendToCode)
			parentCode.appendCommand(this);
		firstToken.setParentCommand(this);
	}

	final void addNext(Command newCommand) throws UnexpectedSyntaxException {
		if (newCommand == null || parentCode == null || next != null || newCommand.prev != null || newCommand.firstToken == null)
			throw new NullPointerException("newCommand");

		newCommand.prev = this;
		next = newCommand;

		newCommand.initialBlockLevel = initialBlockLevel + blockLevelDiff; 

		if (getOpensLevel()) {
			if (!newCommand.getClosesLevel()) {
				addChild(newCommand);
			} else if (newCommand.usedLevelCloser.isCloserFor(usedLevelOpener)) {
				// newCommand directly closes this Command
				addSibling(newCommand);
			} else if (newCommand.usedLevelCloser.requiresOpener) {
				String msg = "expected " + usedLevelOpener.getClosersList() + ", but found " + newCommand.usedLevelCloser.text + ". ";
				msg += getHintOnMacroUsage();
				msg += System.lineSeparator() + System.lineSeparator() + "Opening command (line " + Cult.format(this.sourceLineNumStart) + "): " + this.toStringForErrorMessage(false);
				throw new UnexpectedSyntaxException(this, msg);
			} else {
				// remove an optional level closer that does not apply in this case, e.g. 'DEFINE any_macro. CLASS &1 DEFINITION.'
				newCommand.usedLevelCloser = null;
				addChild(newCommand);
			}

		} else if (newCommand.getClosesLevel()) {
			if (parent != null && newCommand.usedLevelCloser.isCloserFor(parent.usedLevelOpener)) {
				parent.addSibling(newCommand);
			} else if (newCommand.usedLevelCloser.requiresOpener) {
				String msg = newCommand.usedLevelCloser.text + " found in line " + Cult.format(newCommand.sourceLineNumStart); 
				msg += ", but no corresponding " + newCommand.usedLevelCloser.getOpenersList() + ". "; 
				msg += getHintOnMacroUsage();
				throw new UnexpectedSyntaxException(this, msg); 
			} else {
				newCommand.usedLevelCloser = null;
				addSibling(newCommand);
			}

		} else {
			addSibling(newCommand);
		}

		// -------------------------------------------------------------------
		// Determine indentAdd, which is increased if a previous declaration statement (TYPES, DATA, CLASS-DATA, CONSTANTS)
		// opens a block with BEGIN OF, but does not close it. Note that before the block is close with END OF, there may  
		// be further declaration commands, "INCLUDE TYPE|STRUCTURE ...", or comment lines. 

		newCommand.indentAdd = this.indentAdd;
		if (isSelectionScreenElement()) { // || not needed: isDeclaration()    
			// increase indentAdd if the existing Command starts more BEGIN OF blocks than it ends
			int beginOfCount = firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "BEGIN", "OF");
			int endOfCount = firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "END", "OF");
			if (beginOfCount > endOfCount) {
				newCommand.indentAdd += beginOfCount - endOfCount;
			}
		}
		if (newCommand.isSelectionScreenElement()) { // not needed: || newCommand.isDeclaration()  
			// decrease indentAdd if the new Command ends more blocks than it starts
			int beginOfCount = newCommand.firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "BEGIN", "OF");
			int endOfCount = newCommand.firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "END", "OF");
			if (endOfCount > beginOfCount) {
				newCommand.indentAdd -= (endOfCount - beginOfCount);
			}
		}
	}

	private String getHintOnMacroUsage() {
		final String LINE_SEP_2 = System.lineSeparator() + System.lineSeparator();
		final String hint = "If macros cannot be removed, consider hiding both opening and closing commands behind macros.";
		if (belongsToMacroDefinition()) { 
			return LINE_SEP_2 + "Macro definitions with incomplete code blocks are currently not supported by " + Program.PRODUCT_NAME + ". "
				  + LINE_SEP_2 + hint;
		} else {
			// create a list of used macros from the previous Commands (up to the start of the METHOD etc.)
			StringBuilder macroInfo = new StringBuilder();
			HashSet<String> usedMacros = new HashSet<>();
			Command command = this;
			while (command != null && !command.isMethodFunctionFormOrEventBlockStart() && !command.startsLocalVariableContext()) {
				if (command.usesMacro()) {
					String macroName = command.firstToken.getText();
					if (!usedMacros.contains(macroName)) {
						usedMacros.add(macroName);
						if (macroInfo.length() > 0) {
							macroInfo.insert(0, ", ");
						}
						macroInfo.insert(0, macroName + " (line " + Cult.format(command.sourceLineNumStart) + ")");
					}
				}
				command = command.getPrev();
			}
			if (macroInfo.length() > 0) {
				return LINE_SEP_2 + "Possibly, a macro definition contains incomplete code blocks, which is not supported by " + Program.PRODUCT_NAME + ". "
						+ "Used macro(s): " + macroInfo.toString() + ". " 
						+ LINE_SEP_2 + hint;
			}
		}
		return "";
	}
	
	private void addSibling(Command newCommand) throws UnexpectedSyntaxException {
		newCommand.parent = parent;
		newCommand.prevSibling = this;

		nextSibling = newCommand;
		if (parent != null)
			parent.lastChild = newCommand;
	}

	private void addChild(Command newCommand) throws UnexpectedSyntaxException {
		newCommand.parent = this;
		newCommand.prevSibling = null;

		firstChild = newCommand;
		lastChild = newCommand;
	}

	public final void insertFirstChild(Command child) throws UnexpectedSyntaxException, IntegrityBrokenException {
		insertFirstChild(child, false);
	}
	
	public final void insertFirstChild(Command child, boolean skipIntegrityTest) throws UnexpectedSyntaxException, IntegrityBrokenException {
		if (child == null)
			throw new NullPointerException("child");
		if (firstChild != null)
			throw new UnexpectedSyntaxException(this, "This command already has a child command!");
		if (parentCode == null || child.prev != null || child.getClosesLevel() || !getOpensLevel())
			throw new UnexpectedSyntaxException(this, "This command cannot add a child command!");
		if (child.hasChildren())
			throw new UnexpectedSyntaxException(this, "Inserting a child command which itself has child commands is not supported!");

		++parentCode.commandCount;

		child.parentCode = parentCode;
		child.parent = this;

		child.next = (nextSibling != null) ? nextSibling : next;
		child.next.prev = child;

		child.prev = this;
		next = child;

		child.prevSibling = null;
		child.nextSibling = null;
		child.indentAdd = indentAdd;

		firstChild = child;
		lastChild = child;

		if (!skipIntegrityTest) {
			testReferentialIntegrity(false);
		}
		child.testReferentialIntegrity(false);
		if (nextSibling != null)
			nextSibling.testReferentialIntegrity(false);
	}

	/**
	 * inserts the supplied Command (including its possible children) as a sibling after this Command
	 * 
	 * @param newCommand
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Command newCommand) throws IntegrityBrokenException {
		insertRightSibling(newCommand, false, false);
	}

	/**
	 * inserts the supplied Command (including its possible children) as a sibling after this Command
	 * 
	 * @param newCommand
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Command newCommand, boolean moveFollowingLinesRight, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (newCommand == null)
			throw new NullPointerException("newCommand");
		if (newCommand.hasChildren())
			throw new IntegrityBrokenException(this, "Inserting a sibling which has child commands is not supported!");
		if (hasChildren())
			throw new IntegrityBrokenException(this, "Inserting a right sibling to a command that has child commands is not supported!");

		++parentCode.commandCount;

		newCommand.parentCode = parentCode;
		if (parentCode.lastCommand == this)
			parentCode.lastCommand = newCommand;

		newCommand.parent = parent;
		if (parent != null && parent.lastChild == this)
			parent.lastChild = newCommand;

		if (nextSibling != null) {
			nextSibling.prevSibling = newCommand;
			nextSibling.prev = newCommand;
		}
		newCommand.nextSibling = nextSibling;

		newCommand.prevSibling = this;
		nextSibling = newCommand;

		Command next = this.next; // may be on a higher(!) level
		newCommand.next = next;
		if (next != null)
			next.prev = newCommand;

		newCommand.prev = this;
		newCommand.prev.next = newCommand;

		if (!skipIntegrityTest) {
			this.testReferentialIntegrity(false);
			newCommand.testReferentialIntegrity(false);
			if (newCommand.next != null) {
				newCommand.next.testReferentialIntegrity(false);
			}
			if (parent != null) {
				parent.testReferentialIntegrity(false);
			}
		}
	}

	/**
	 * inserts the supplied Section as a sibling before this Command
	 * 
	 * @param newSection
	 * @throws IntegrityBrokenException 
	 */
	public final void insertLeftSibling(Section newSection) throws IntegrityBrokenException {
		if (newSection == null)
			throw new NullPointerException("newSection");
		if (getClosesLevel())
			throw new IntegrityBrokenException(this, "Inserting a left sibling to a command that closes a block is not supported!");

		parentCode.commandCount += newSection.getCommandCountWithChildren();

		newSection.setParentCode(parentCode);
		if (parentCode.firstCommand == this)
			parentCode.firstCommand = newSection.firstCommand;

		newSection.setParent(parent);
		if (parent != null && parent.firstChild == this)
			parent.firstChild = newSection.firstCommand;

		if (prev != null)
			prev.next = newSection.firstCommand;
		newSection.setPrev(prev);

		prev = newSection.lastCommand; // newSection.lastCommand is sure to be childless
		prev.next = this;

		if (prevSibling != null) {
			prevSibling.nextSibling = newSection.firstCommand;
			prevSibling.next = newSection.firstCommand;
		}
		newSection.setPrevSibling(prevSibling);

		newSection.setNextSibling(this);
		prevSibling = newSection.lastCommand; // newSection.lastCommand is sure to be childless

		parentCode.testReferentialIntegrity(false);

		if (newSection.firstCommand.prev != null)
			newSection.firstCommand.prev.testReferentialIntegrity(false);
		newSection.firstCommand.testReferentialIntegrity(false);
		if (newSection.firstCommand != newSection.lastCommand)
			newSection.lastCommand.testReferentialIntegrity(false);
		this.testReferentialIntegrity(false);
		if (parent != null)
			parent.testReferentialIntegrity(false);
	}

	/**
	 * inserts the supplied Section as a sibling after this Command
	 * 
	 * @param newSection
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Section newSection) throws IntegrityBrokenException {
		insertRightSibling(newSection, false);
	}

	/**
	 * inserts the supplied Section as a sibling after this Command
	 * 
	 * @param newSection
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Section newSection, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		if (newSection == null)
			throw new NullPointerException("newSection");
		if (hasChildren())
			throw new IntegrityBrokenException(this, "Inserting a right sibling to a command that has child commands is not supported!");

		parentCode.commandCount += newSection.getCommandCountWithChildren();

		newSection.setParentCode(parentCode);
		if (parentCode.lastCommand == this)
			parentCode.lastCommand = newSection.lastCommand;

		newSection.setParent(parent);
		if (parent != null && parent.lastChild == this)
			parent.lastChild = newSection.lastCommand;

		if (nextSibling != null) {
			nextSibling.prevSibling = newSection.lastCommand;
			nextSibling.prev = newSection.lastCommand;
		}
		newSection.setNextSibling(nextSibling);

		newSection.setPrevSibling(this);
		nextSibling = newSection.firstCommand;

		Command next = this.next; // may be on a higher(!) level
		newSection.lastCommand.next = next;
		if (next != null)
			next.prev = newSection.lastCommand;

		newSection.setPrev(this);
		newSection.getPrev().next = newSection.firstCommand;

		parentCode.testReferentialIntegrity(false);

		this.testReferentialIntegrity(false);
		newSection.firstCommand.testReferentialIntegrity(false);
		if (newSection.firstCommand != newSection.lastCommand)
			newSection.lastCommand.testReferentialIntegrity(false);
		if (newSection.lastCommand.next != null)
			newSection.lastCommand.next.testReferentialIntegrity(false);
		if (parent != null)
			parent.testReferentialIntegrity(false);
	}

	/*
	private Command getLastInSubtree() {
		Command last = this;
		while (last.lastChild != null)
			last = last.lastChild;
		return last;
	}
	 */
	
	final void addToken(Token token) {
		if (token == null)
			throw new NullPointerException("token");
		if (firstToken == null)
			firstToken = token;
		lastToken = token;
		++tokenCount;
	}

	final boolean canAdd(Token newToken, Command prevCommand) {
		if (newToken == null)
			throw new NullPointerException("newToken");

		if (isDdlOrDcl()) {
			return canAddToDdl(newToken, prevCommand);
		}
		
		if (firstToken.isCommentLine())
			return false;
		
		// if the Command starts with one or several pragmas (plus potentially a line-end comment),
		// any new Token in a new line (whether it is a keyword, a comment, another pragma etc.) starts a new Command
		if (firstToken.isPragma() && newToken.lineBreaks > 0) {
			Token token = firstToken.getNext();
			while (token != null && token.isPragma())
				token = token.getNext();
			if (token == null || token.isComment())
				return false;
		}

		if (language == Language.ABAP) {
			Token lastNonCommentToken = getLastNonCommentToken();
			if (lastNonCommentToken == null || !lastNonCommentToken.isPeriod()) {
				return true;
			} else {
				return lastToken.isPeriod() && newToken.isComment() && (newToken.lineBreaks == 0);
			}
		
		} else {
			// non-ABAP sections can be closed by "ENDEXEC" or "ENDMETHOD", so if these keywords are found, 
			// they can NOT be added to this Command, but must start a new one with language = Language.ABAP 
			return !newToken.isAnyKeyword("ENDEXEC", "ENDMETHOD");
			
		}
	}

	private boolean canAddToDdl(Token newToken, Command prevCommand) {
		// a comment at line end can always be added to previous code
		if (newToken.isComment() && newToken.lineBreaks == 0)
			return true;
		// create a new Command for line breaks at document end
		if (newToken.getTextLength() == 0 && newToken.lineBreaks > 0)
			return false;
		// a comment line cannot add more lines
		if (firstToken.isComment() && firstToken == lastToken && newToken.lineBreaks > 0) 
			return false;
		// the base info comment must be a distinct Command (cp. Code.toString(String))
		if (newToken.textEquals(DDL.BASE_INFO_COMMENT_START))
			return false;
		
		if (firstToken.isDdlAnnotation()) {
			// determine whether the annotation is complete
			// note that there may be spaces and line feeds: "@ AnyAnno . Sub \r\n . SubSub" is a valid annotation
			
			// the "," or ";" after an element could be placed after an "@<" annotation following the element
			if (newToken.textEqualsAny(DDL.listElementSeparators))
				return true;
			
			Token lastNonCommentToken = getLastNonCommentToken();
			Token colon = findDdlAnnotationColon();
			if (colon == null) {
				if (lastNonCommentToken.textEquals(DDL.ANNOTATION_SIGN_STRING)) {
					return true;
				} else if (lastNonCommentToken.textEndsWith(DDL.DOT_SIGN_STRING)) {
					return true;
				} else if (newToken.textStartsWith(DDL.DOT_SIGN_STRING)) {
					return true;
				} else if (newToken.textEquals(DDL.COLON_SIGN_STRING)) {
					return true;
				} else { 
					// annotations may contain no ": value" if their default value is used; 
					// the new Token therefore belongs to the next Command 
					return false;
				}
			}
			Token value = colon.getNextCodeSibling();
			if (value != null && value.textEquals("#")) // unchecked expression #(...)
				value = value.getNextCodeToken();
			Token valueEnd = (value == null) ? null : (value.getOpensLevel() ? value.getNextCodeSibling() : value);
			return (valueEnd == null);
		}
		if (lastToken.getParent() != null)
			return true;
		
		// determine whether we are on top level of the Command hierarchy
		Token lastCodeToken = getLastNonCommentToken();
		Command prevCommandParent = (prevCommand == null) ? null : prevCommand.getParent();
		boolean isAtTopLevel = (prevCommand == null 
				|| prevCommandParent == null && !prevCommand.getOpensLevel() 
				|| prevCommandParent != null && prevCommandParent.lastCodeTokenIsKeyword("PARAMETERS") && firstCodeTokenTextEqualsAny(DDL.levelClosersAfterParameterList) 
				|| prevCommandParent != null && prevCommandParent.lastCodeTokenIsAnyKeyword("SELECT", "DISTINCT") && firstCodeTokenTextEqualsAny("FROM")); 

		// determine whether we are in a select list or parameter list
		boolean isInSelectListWithBraces = false;
		boolean isInSelectListWithoutBraces = false;
		boolean isInParameterList = false;
		if (prevCommand != null) {
			Command listOpener = prevCommand.getOpensLevel() ? prevCommand : prevCommandParent;
			if (listOpener != null) {
				Token lastCode = listOpener.getLastCodeToken();
				if (lastCode != null) {
					Token firstCode = getFirstCodeToken();
					isInSelectListWithBraces = lastCode.textEquals(DDL.BRACE_OPEN_STRING);
					isInSelectListWithoutBraces = lastCode.isAnyKeyword("SELECT", "DISTINCT");
					isInParameterList = lastCode.isKeyword("PARAMETERS") && (firstCode == null || !firstCode.textEqualsAny(DDL.levelClosersAfterParameterList));
				}
			}
		}
		
		// start a new Command for the parameter list
		if (isAtTopLevel && lastCodeToken.isKeyword("PARAMETERS")) {
			Token lastPrev = lastCodeToken.getPrevCodeToken();
			if (lastPrev != null && lastPrev.isKeyword("WITH")) {
				return false;
			}
		}
		
		// start a new Command for the select list
		if (isAtTopLevel && !newToken.isComment() && newToken.textEquals(DDL.BRACE_OPEN_STRING)) {
			return false;
		} else if (isAtTopLevel && lastCodeToken.textEquals(DDL.BRACE_OPEN_STRING)) {
			// the "{" that starts the select list is a distinct Command 
			return false;
		} else if (isAtTopLevel && lastCodeToken.textEqualsAny("SELECT", "DISTINCT") && !newToken.isComment() && !newToken.isAnyKeyword("DISTINCT", "FROM")) {
			// first form of DDIC-based views with no braces: 
			// [DEFINE] [ROOT] VIEW ddic_based_view [name_list] [parameter_list] AS SELECT [DISTINCT] element1, element2, ... FROM data_source ...
			return false;
		}
		
		// within in the select or parameter list, start a new Command after each comma (or semicolon, e.g. for "define table" and "define structure")
		// or if the new Token starts with "@<"
		if (isInSelectListWithBraces || isInSelectListWithoutBraces || isInParameterList) {
			if (lastCodeToken.textEqualsAny(DDL.listElementSeparators) && lastCodeToken.getParent() == null) {
				return false;
			} else if (newToken.textStartsWith(DDL.ANNOTATION_AFTER_LIST_ELEMENT_PREFIX)) {
				return false;
			}
		}
		
		// finish the select list or parameter list
		if (isInSelectListWithBraces && !newToken.isComment() && newToken.textEquals(DDL.BRACE_CLOSE_STRING)) {
			return false;
		} else if (isInSelectListWithoutBraces && !newToken.isComment() && newToken.isKeyword("FROM")) {
			return false;
		} else if (firstToken.textEquals(DDL.BRACE_CLOSE_STRING)) {
			// the "}" that ends the select list is a distinct Command 
			return false;
		} else if (isInParameterList && newToken.textEqualsAny(DDL.levelClosersAfterParameterList)) {
			// "AS" / "RETURNS" OR "{" ends the parameter list starts the next Command
			return false;
		}
		
		// start a new Command for a JOIN or an ASSOCIATION / COMPOSITION
		if (isAtTopLevel && newToken.isKeyword()) {
			Token firstCodeToken = getFirstCodeToken();
			// a join could either start with INNER, LEFT, RIGHT, CROSS, or directly with the keyword JOIN, or with a cardinality (after implicit INNER):
			// { [INNER] [cardinality] JOIN | LEFT OUTER [cardinality] JOIN | RIGHT OUTER JOIN | CROSS JOIN } data_source [ON cds_cond]
			// cardinality -> 
			//   EXACT ONE TO EXACT ONE | EXACT ONE TO MANY | EXACT ONE TO ONE | MANY TO EXACT ONE | MANY TO MANY | MANY TO ONE |
			//   ONE TO EXACT ONE | ONE TO MANY | ONE TO ONE | TO ONE | TO EXACT ONE | TO MANY
			if (firstCodeToken.isAnyKeyword("REDEFINE", "ASSOCIATION", "COMPOSITION")) {
				// skip this section, particularly to avoid that the keyword "TO" in "ASSOCIATION ... TO target" or "EXACT" in "COMPOSITION OF EXACT ONE ..." starts a new Command
			} else if (newToken.isKeyword("TO") && lastCodeToken.isAnyKeyword("REDIRECTED", "REFERENCE")) {
				// skip this section to avoid starting a new Command within "REDIRECTED TO" (in projection views) or "REFERENCE TO" (in structures)
			} else if (newToken.isAnyKeyword("INNER", "LEFT", "RIGHT", "CROSS")) {
				return false;
			} else if (newToken.isAnyKeyword("JOIN") && (lastCodeToken == null || !lastCodeToken.isAnyKeyword("INNER", "OUTER", "CROSS", "ONE", "MANY"))) {
				return false;
			} else if (newToken.isAnyKeyword("EXACT", "MANY", "ONE", "TO") && (lastCodeToken == null || !lastCodeToken.isAnyKeyword("INNER", "OUTER", "EXACT", "MANY", "ONE", "TO"))) {
				return false;
			}
			// an association either starts with "ASSOCIATION" or "REDEFINE ASSOCIATION" (in projection views):
			// - ASSOCIATION [cardinality] [TO] target [AS _assoc] ON cds_cond [WITH DEFAULT FILTER cds_cond]
			// - REDEFINE ASSOCIATION [source.]_ProjAssoc [filter] [AS _RedefinedName] redirection
			// for COMPOSITION, two cases must be excluded:
			// - "REDIRECTED TO COMPOSITION CHILD" 
			// - "EXTEND CUSTOM ENTITY ... _compos : COMPOSITION [cardinality] OF target"
			if (newToken.isKeyword("REDEFINE")) {
				return false;
			} else if (newToken.isKeyword("ASSOCIATION") && (lastCodeToken == null || !lastCodeToken.isKeyword("REDEFINE"))) {
				return false;
			} else if (newToken.isKeyword("COMPOSITION") && (lastCodeToken == null || !lastCodeToken.isKeyword("TO") && !lastCodeToken.textEquals(DDL.COLON_SIGN_STRING))) { 
				return false;
			}
		}
		
		// start a new Command for a WHERE / GROUP BY / HAVING clause, as well as for EXCEPT / INTERSECT / UNION [ALL]
		// (all these keywords never appear in a different context)
		if (isAtTopLevel && newToken.isKeyword()) {
			if (newToken.isAnyKeyword("WHERE", "GROUP", "HAVING")) {
				return false;
			} else if (newToken.isAnyKeyword("EXCEPT", "INTERSECT", "UNION")) {
				return false;
			}
		}
		
		// in all other cases, add the new Token to the current Command
		return true;
	}

	public final void finishBuild(int sourceTextStart, int sourceTextEnd) throws ParseException {
		finishBuild(sourceTextStart, sourceTextEnd, null);
	}
	
	public final void finishBuild(int sourceTextStart, int sourceTextEnd, Command prevCommand) throws ParseException {
		this.sourceTextStart = sourceTextStart;
		this.sourceTextEnd = sourceTextEnd;
		sourceLineBreaksBefore = firstToken.lineBreaks;
		sourceLineNumStart = firstToken.sourceLineNum;
		sourceLineNumLast = lastToken.sourceLineNum;
		changeControl = parentCode.getChangeControl(sourceTextStart, sourceTextEnd);

		if (isDdlOrDcl()) {
			finishBuildDdlOrDcl(prevCommand);
			return;
		}
		
		// code in non-ABAP sections (e.g. EXEC SQL ... ENDEXEC) is kept unchanged, and unless a Token is a COMMENT, is has TokenType NON_ABAP
		if (language != Language.ABAP)
			return;

		// invalidate the MemoryAccessType of all tokens
		invalidateMemoryAccessType();

		// correct some tricky cases in which field names may look like ABAP keywords (but are nevertheless identifiers)
		Program.getTokenTypeRefiner().refine(this);

		// -------------------------------------------------------------------
		// determine UsedLevelOpener/Closer

		// get preliminary result from the first keyword token(s)
		// In some cases like "AT LINE-SELECTION", "AT SELECTION-SCREEN", "AT USER-COMMAND", the levelOpener or levelCloser
		// must be concatenated from several consecutive keyword tokens, potentially with comments between them.  
		// Note that it is NOT enough to stop once a levelOpener or levelCloser is found, because otherwise, 
		// "AT LINE-SELECTION" would be misinterpreted as an "AT" that requires an "ENDAT"!
		usedLevelOpener = null;
		usedLevelCloser = null;
		Token keywordToken = getFirstCodeToken();
		String keywordText = null;
		while (keywordToken != null && keywordToken.isKeyword()) {
			keywordText = (keywordText == null) ? keywordToken.text : keywordText + " " + keywordToken.text;
			LevelOpener levelOpener = levelOpeners.get(getLevelOpenerKey(keywordText));
			if (levelOpener != null) {
				usedLevelOpener = levelOpener;
				// in a case like "CATCH SYSTEM-EXCEPTIONS", also reset the used level closer (which was determined from "CATCH" in a previous loop cycle) 
				usedLevelCloser = null;
			}
			LevelCloser levelCloser = levelClosers.get(getLevelCloserKey(keywordText));
			if (levelCloser != null)
				usedLevelCloser = levelCloser;
			// continue with next keyword, because "AT" may become "AT SELECTION-SCREEN", "CATCH" may become the obsolete "CATCH SYSTEM-EXCEPTIONS" etc.  
			keywordToken = keywordToken.getNextCodeSibling();
		}

		// correct pseudo CLASS opener
		Token firstCode = getFirstCodeToken();
		if (isClassStart() && firstCode != null) {
			if (firstCode.matchesOnSiblings(true, "CLASS", TokenSearch.makeOptional(":"), TokenSearch.ANY_IDENTIFIER, "DEFINITION", "DEFERRED")
					|| firstCode.matchesOnSiblings(true, "CLASS", TokenSearch.makeOptional(":"), TokenSearch.ANY_IDENTIFIER, "DEFINITION", "LOCAL", "FRIENDS")) {
				usedLevelOpener = null;
				// in a program, START-OF-SELECTION is NOT closed by CLASS ... DEFINITION DEFERRED: any Command after it is executed as part of START-OF-SELECTION
				usedLevelCloser = null;
				firstToken.setOpensLevel(false);
			}
		}
		// do NOT merge with previous "if" block!
		if (isClassStart()) {
			isClassDefinitionStart = firstCode != null && firstCode.matchesOnSiblings(true, "CLASS", TokenSearch.ASTERISK, "DEFINITION");
			isClassImplementationStart = firstCode != null && firstCode.matchesOnSiblings(true, "CLASS", TokenSearch.ASTERISK, "IMPLEMENTATION");
		}

		// in a SELECT or WITH statement, keep the LevelOpener only if an ENDSELECT or ENDWITH is required
		if (firstCode != null && firstCode.isAnyKeyword("SELECT", "WITH")) {
			boolean opensSelectLoop;
			try {
				opensSelectLoop = opensSelectLoop(); 
			} catch (NullPointerException ex) {
				// unexpected syntax
				opensSelectLoop = false;
			}
			if (!opensSelectLoop) { 
				usedLevelOpener = null;
			}
		}

		// the (obsolete) CLASS ... DEFINITION LOAD statement is NOT followed by ENDCLASS
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapclass_interface_load.htm
		if (firstCode != null &&  firstCode.isKeyword("CLASS") && firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "DEFINITION", "LOAD")) {
			usedLevelOpener = null;
			usedLevelCloser = null;
		}
		
		// the (obsolete) INTERFACE ... LOAD statement and the INTERFACE ... DEFERRED statement are NOT followed by ENDINTERFACE
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapclass_interface_load.htm
		if (firstCode != null && firstCode.isKeyword("INTERFACE") && firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "LOAD|DEFERRED")) {
			usedLevelOpener = null;
		}

		// determine the 'block level difference' in TYPES: BEGIN OF ... END OF etc. 
		if (isDeclaration()) {  
			// decrease block level if the new Command ends more blocks than it starts
			int beginOfCount = firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "BEGIN", "OF");
			int endOfCount = firstToken.getSequenceCount(true, true, TokenSearch.ASTERISK, "END", "OF");
			blockLevelDiff = beginOfCount - endOfCount;
		}
		// -------------------------------------------------------------------

		{
			// identify ABAP keyword collocations
			Token token = firstToken;
			while (token != null) {
				if (token.isKeyword()) {
					Token end = token;
					String collocation = token.text;

					Token testToken = token.getNextCodeToken();
					while (testToken != null && testToken.isKeyword()) {
						collocation += " " + testToken.text;
						if (ABAP.isAbapKeywordCollocation(collocation))
							end = testToken;
						else if (!ABAP.isAbapKeywordCollocationStart(collocation))
							break;
						testToken = testToken.getNextCodeToken();
					}
					while (token != end) {
						token.collocationContinues = true;
						token = token.getNextCodeToken();
					}
					// token is now the last Token of the collocation, so we can again move to the next token
				}
				token = token.getNextCodeToken();
			}
		}

		updateChainColonCount();

		// raise an exception in case of chained level openers or level closers such as 'IF a > : 1, 2.' or 'ENDIF:,.'
		// A chain colon : with NO comma can be tolerated, unless the : is inside parentheses; the latter case is already
		// prevented in Token.addNext()
		if ((getOpensLevel() || getClosesLevel()) && containsChainColon()) {
			if (firstToken.matchesDeep(true,  TokenSearch.ASTERISK, ABAP.COMMA_SIGN_STRING)) {
				throw new ParseException(parentCode, this.sourceLineNumStart, "Unsupported syntax: Chain of " + firstToken.getTextOfKeywordCollocation() + " commands cannot be processed by " + Program.PRODUCT_NAME + ". Please refactor this command first.");
			}
		}

		// distinguish assignment operators from comparison operators // this must be aligned with RuleForLogicalExpressions.executeOn()!
		if (firstCode != null && firstCode.isAnyKeyword("IF", "ELSEIF", "CHECK", "WHILE")) {
			distinguishOperators(true, firstCode, null);
		
		} else if (firstCode != null && firstCode.matchesOnSiblings(true, "LOOP AT|MODIFY|DELETE|FOR")) {
			// "LOOP AT ... [WHERE log_exp] [GROUP BY ...]"; "MODIFY itab ... WHERE log_exp ..." etc. // TODO: still missing: "PROVIDE", "ASSERT ... CONDITION", ...
			// see ABAP Reference: 
			// - "LOOP AT, itab", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaploop_at_itab.htm
			// - "MODIFY itab", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmodify_itab.htm
			// - "DELETE itab", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapdelete_itab.htm
			// - "FOR, Table Iterations", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_itab.htm
			// - "FOR ... IN GROUP", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_in_group.htm
			distinguishOperators(false, firstCode, null, "WHERE", "GROUP BY|USING KEY|TRANSPORTING|FROM");

		} else if (firstCode != null && firstCode.isKeyword("SELECT")) {
			distinguishOperators(false, firstCode, null, "WHERE", "GROUP BY|HAVING|ORDER BY|%_HINTS|UNION|INTO");

		} else if (firstCode != null && firstCode.isKeyword("WAIT")) {
         // WAIT FOR ASYNCHRONOUS TASKS [MESSAGING CHANNELS] [PUSH CHANNELS] UNTIL log_exp [UP TO sec SECONDS].
			distinguishOperators(false, firstCode, null, "UNTIL", "UP TO");
		
		} else {
			distinguishOperators(false, firstToken, null);
		}
	}

	private void finishBuildDdlOrDcl(Command prevCommand) {
		Token firstNonComment = getFirstCodeToken();
		Token lastNonComment = getLastNonCommentToken();
		
		if (lastNonComment != null && lastNonComment.textEquals(DDL.BRACE_OPEN_STRING)) {
			usedLevelOpener = ddlLevelOpenerBrace;
			lastNonComment.setOpensLevel(false);
		} else if (lastNonComment != null && lastNonComment.isAnyKeyword("SELECT", "DISTINCT")) {
			usedLevelOpener = ddlLevelOpenerSelect;
		} else if (lastNonComment != null && lastNonComment.isKeyword("PARAMETERS")) {
			usedLevelOpener = ddlLevelOpenerBrace;
		} // do NOT attach the next line with else! 
		
		Command prevCommandParent = (prevCommand == null) ? null : prevCommand.getParent(); 
		if (prevCommandParent != null && prevCommandParent.lastCodeTokenIsKeyword("PARAMETERS")) {
			if (firstNonComment != null && firstNonComment.textEqualsAny(DDL.levelClosersAfterParameterList)) {
				usedLevelCloser = ddlLevelCloserBrace;
			}
		} else if (firstNonComment != null && firstNonComment.textEquals(DDL.BRACE_CLOSE_STRING)) {
			usedLevelCloser = ddlLevelCloserBrace;
			firstNonComment.setClosesLevel(false);
			
		} else if (firstNonComment != null && firstNonComment.isKeyword("FROM")) {
			usedLevelCloser = ddlLevelCloserSelect;
			firstNonComment.setClosesLevel(false);
		}
		
		// correct TokenType.KEYWORD into IDENTIFIER in ambiguous cases (typical candidates: contract, depth, parent, period, name, root)
		boolean isAnnotation = isDdlAnnotation(); 
		Token token = firstToken;
		while (token != null) {
			Token prevSibling = token.getPrevCodeSibling();
			Token nextSibling = token.getNextCodeSibling();
			if (!token.isKeyword()) {
				token = token.getNext();
				continue;
			}

			if (isAnnotation) {
				// in annotations, change identifier "entity" from keyword to identifier
				token.type = TokenType.IDENTIFIER;
			
			} else if (!isAnnotation && token.getParent() != null && token.isAnyKeyword("VALUE", "WITH")){
				// in replace_regex( pcre => ... value => ... with => ... ), "value" and "with" are identifiers
				if (nextSibling != null && nextSibling.textEquals("=>")) {
					token.type = TokenType.IDENTIFIER;
				}
			
			} else if (!isAnnotation && token.isAnyKeyword("LEFT", "RIGHT") && nextSibling != null && nextSibling.textEquals("(")) {
				// "left(...)" and "right(...)" are built-in functions (unlike LEFT|RIGHT OUTER JOIN)
				token.type = TokenType.IDENTIFIER;
				
			} else if (prevSibling != null && prevSibling.textEndsWith(DDL.DOT_SIGN_STRING) 
					  || nextSibling != null && nextSibling.textStartsWith(DDL.DOT_SIGN_STRING)) {
				// "." is always followed and preceded by an identifier
				token.type = TokenType.IDENTIFIER;
				
			} else if ((prevSibling == null || prevSibling.isKeyword("KEY") || prevSibling.textEqualsAny(DDL.COMMA_SIGN_STRING, DDL.SEMICOLON_SIGN_STRING))
						&& nextSibling != null && nextSibling.isKeyword("AS")) {
				// simple selection list entry
				token.type = TokenType.IDENTIFIER;
				
			} else if (prevSibling != null && prevSibling.isKeyword("AS") && !token.isAnyKeyword("SELECT", "PROJECTION", "PARENT")) {
				// AS alias, AS type
				token.type = TokenType.IDENTIFIER;
			}
			
			// check if the keyword as part of a known collocation
			if (token.isKeyword()) {
				// list all keywords of the sequence (if any) to which this keyword belongs
				Token firstKeyword = token;
				Token keywordToken = firstKeyword.getPrevCodeSibling();
				while (keywordToken != null && keywordToken.isKeyword()) {
					firstKeyword = keywordToken;
					keywordToken = keywordToken.getPrevCodeSibling();
				}
				ArrayList<String> keywordSequence = new ArrayList<>();
				keywordToken = firstKeyword;
				int indexInList = 0;
				while (keywordToken != null && (keywordToken.isKeyword() || keywordToken.textEquals(DDL.PARENS_OPEN_STRING))) {
					if (keywordToken == token)
						indexInList = keywordSequence.size();
					keywordSequence.add(keywordToken.getText());
					if (keywordToken.textEquals(DDL.PARENS_OPEN_STRING)) // only possible at the end of a sequence
						break;
					keywordToken = keywordToken.getNextCodeSibling(); 
				}
				// determine the last keyword of the parent function, if any 
				// (e.g. "HIERARCHY" for the collocation "CHILD TO PARENT ASSOCIATION" inside of "PARENT CHILD HIERARCHY( ... )")
				String parentKeyword = null;
				if (token.getParent() != null) {
					Token parentPrev = token.getParent().getPrevCodeSibling();
					parentKeyword = (parentPrev == null) ? null : parentPrev.getText();
				}
				// if the keyword sequence around token is not a known DDL or DCL keyword collocation, token must be an identifier
				if (!DDL.isKnownCollocation(keywordSequence, indexInList, parentKeyword, language)) {
					token.type = TokenType.IDENTIFIER;
				}
			}
			token = token.getNext();
		}
	}

	public void invalidateMemoryAccessType() {
		Token token = firstToken;
		while (token != null) {
			token.invalidateMemoryAccessType();
			token = token.getNext();
		}
	}

	private void updateChainColonCount() {
		chainColonCount = 0;
		Token token = firstToken;
		while (token != null) {
			if (token.isChainColon()) 
				++chainColonCount;
			token = token.getNextCodeToken();
		}
	}
	
	private boolean opensSelectLoop() throws NullPointerException, ParseException {
		// determine whether a SELECT command requires an ENDSELECT and therefore opens a level
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect.htm 
		// similarly, determine whether WITH requires ENDWITH by analyzing its main SELECT query
		
		// "In the following cases, the statement SELECT opens a loop that must be closed using ENDSELECT.
		// 1. If an assignment is made to a non-table-like target range, that is, a SELECT statement without the 
		//    addition INTO|APPENDING ... TABLE, a loop closed by ENDSELECT always occurs, except in the following instances:
		//     1a) The addition SINGLE for reading a single row is specified after SELECT
		//     1b)   (i) The columns of the result set are specified statically in the SELECT list, 
		//          (ii) they contain only aggregate functions, and 
		//         (iii) the additions GROUP BY and UNION are not specified.
		//
		// 2. If an assignment is made to a table-like target range, that is, a SELECT statement with the addition 
		//    INTO|APPENDING ... TABLE, a loop closed by ENDSELECT occurs whenever the addition PACKAGE SIZE is used."

		Token selectToken = getFirstCodeToken();
		if (selectToken == null)
			return false;

		// for WITH commands, move to the main SELECT query (skipping parenthesized subquery SELECT clauses from table expression definitions)
		if (selectToken.isKeyword("WITH")) {
			selectToken = selectToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "SELECT");
			if (selectToken == null)
				return false;
		}

		// is the result put into a table? - then ENDSELECT is only required if PACKAGE SIZE is used (case 2)
		if (selectToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "INTO|APPENDING", TokenSearch.makeOptional("CORRESPONDING FIELDS OF"), "TABLE")) 
			return selectToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "PACKAGE SIZE");
		
		// is SELECT SINGLE used? - then ENDSELECT is not required (case 1a)
		if (selectToken.matchesOnSiblings(true, "SELECT", "SINGLE")) 
			return false;
		
		// process case 1b

		// is GROUP BY or UNION specified? - then ENDSELECT is required (case 1b.iii) 
		if (selectToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "GROUP BY"))
			return true;
		if (selectToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "UNION"))
			return true;
		
		// determine start and end of the select clause
		// (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_mainquery.htm)
		Token selectClauseStart = selectToken.getNextCodeSibling();
		boolean isSelectFromFields = false;
		if (selectClauseStart.isKeyword("FROM")) {
			isSelectFromFields = true;
			selectClauseStart = selectToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "FIELDS");
			selectClauseStart = selectClauseStart.getNextCodeSibling();
		}
		
		// skip the "DISTINCT" which the select clause may start with
		// (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_clause.htm)
		if (selectClauseStart.isKeyword("DISTINCT")) 
			selectClauseStart = selectClauseStart.getNextCodeSibling();

		// if the columns are specified dynamically, ENDSELECT is required (case 1b (i)); example: 
		// - SELECT DISTINCT (column) FROM spfli WHERE ...
		// (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_list.htm)
		Token selectListStart = selectClauseStart;
		if (selectListStart.textStartsWith("(") && selectListStart.getNext().isAttached())
			return true;

		// loop over the columns in the select list. Examples showing the [select list] in brackets:
		// - SELECT FROM scarr FIELDS [carrname, url] WHERE carrid = 'UA' ...
		// - SELECT FROM scarr FIELDS [carrid, carrname, url] INTO ... 
		// - SELECT FROM scarr FIELDS [COUNT( * )] INTO ... 
		// - SELECT [fldate, AVG( loccuram as DEC( 31,2 ) ) AS avg] FROM sbook ...
		// - SELECT FROM sflight FIELDS [carrid, connid, MAX( seatsmax - seatsocc ) AS seatsfree_max] GROUP BY ..FIELDS.
		// - SELECT DISTINCT [cityto] FROM spfli WHERE ...
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_list.htm)
		boolean aggregateFunctionFound = false;
		Token token = selectListStart;
		while (token != null) {
			// determine whether anywhere, a table field is found anywhere in the select list entry; this entry may contain 
			// - table fields 
			// - untyped literals like 123, 'abc', `abc`, 
			// - typed literals like int1`42`, decfloat16`3.14`, char`abc`, numc`012`, cuky`EUR`,  
			// - host variables or host constants (e.g. @lc_constant, @iv_variable),
			// - SQL functions func( arg1[, arg2] ... ) like abs(, ceil(, div(, floor(, mod(, round(, concat(, instr(, length( etc.
			// - an arithmetic expression combining any of these elements
			Term term;
			try {
				term = Term.createArithmetic(token);
			} catch (UnexpectedSyntaxException e) {
				throw new ParseException(parentCode, this.sourceLineNumStart, e);
			}
			// consider SELECT * already now, so the * does not get confused with multiplication 
			if (term.firstToken.textEquals("*"))
				return true;

			Token test = term.firstToken;
			boolean foundLast = false;
			while (test != null && !foundLast) {
				foundLast = (test == term.lastToken);
				if (test.isIdentifier() && !test.getOpensLevel() && !test.textStartsWith(ABAP.AT_SIGN_STRING) 
						&& !test.isSqlLiteralType() && !test.isSqlTypeInCast()) {
					// access to table field found outside of aggregate functions, therefore ENDSELECT is required
					return true;
				} 
				if (test.textEqualsAny(ABAP.abapSqlAggregateFunctions)) {
					aggregateFunctionFound = true;
					// table fields inside aggregate functions are fine, therefore skip the aggregate function
					test = test.getNextCodeSibling();
				} else {
					// move to the next Token - also looking into parentheses or SQL functions like concat(
					test = test.getNextCodeToken();
				}
			}

			// move behind the Term
			token = term.lastToken.getNextCodeToken();
			
			// skip "AS alias" expression
			if (token.isKeyword("AS")) 
				token = token.getNextCodeSibling().getNextCodeSibling();
			
			// skip the comma
			if (token.textEquals(",")) {
				token = token.getNextCodeSibling();
				continue;
			} 
			
			// check whether the select list ends
			if (!token.isKeyword()) {
				// list continues
			} else if (isSelectFromFields) {
				// the INTO clause may come earlier than specified in the ABAP reference
				if (token.matchesOnSiblings(true, "FOR ALL ENTRIES IN|WHERE|GROUP BY|HAVING|ORDER BY|%_HINTS|INTO")) {
					break;
				}
			} else {
				// the INTO clause or UP TO ... ROWS may come even before FROM 
				if (token.matchesOnSiblings(true, "FROM|INTO|UP TO")) {
					break;
				}
			}
		}
		// ENDSELECT is NOT required if at least one aggregate function was found (but no table field outside of it) 
		return !aggregateFunctionFound;
	}

	private void distinguishOperators(boolean isComparisonPositionAtStart, Token start, Token end) {
		distinguishOperators(isComparisonPositionAtStart, start, end, null, null);
	}
	private void distinguishOperators(boolean isComparisonPositionAtStart, Token start, Token end, String startCondition, String endCondition) {
		Token token = start;
		Token endOfLogicalExpression = null;
		boolean isComparisonPosition = isComparisonPositionAtStart;
		
		while (token != end) {
			if (startCondition != null && token.matchesOnSiblings(true, startCondition)) {
				isComparisonPosition = !isComparisonPositionAtStart;
				if (token.isAnyKeyword("UNTIL", "WHILE")) {
					// in case of "FOR, Conditional Iteration", the end of the logical expression after "UNTIL" or "WHILE" 
					// cannot always be identified with the endCondition; therefore, we must additionally determine the final token
					try {
						endOfLogicalExpression = Token.findEndOfLogicalExpression(token.getNextCodeSibling());
					} catch (UnexpectedSyntaxException e) {
						endOfLogicalExpression = null;
					}
				}
			} else if (token == endOfLogicalExpression || (endCondition != null && token.matchesOnSiblings(true, endCondition))) {
				isComparisonPosition = isComparisonPositionAtStart;
				endOfLogicalExpression = null;
			}

			if (token.textEquals("=") && (token.isAssignmentOperator() || token.isComparisonOperator()))
				token.type = isComparisonPosition ? TokenType.COMPARISON_OP : TokenType.ASSIGNMENT_OP;
			else if (token.textEqualsAny("IN", "BETWEEN") && !token.isIdentifier()) // excluded cases in which the token has already been classified as an identifier
				token.type = isComparisonPosition ? TokenType.COMPARISON_OP : TokenType.KEYWORD;

			if (token.getOpensLevel()) {
				// cp. Token.getEndOfLogicalExpression(), section 2. and 3. 
		      Token ctorKeyword = token.getPrevCodeSibling();
				if (token.textEqualsAny("xsdbool(", "boolc(")) { // TODO: boolx( not yet supported, that one has parameters...
					distinguishOperators(true, token.getNext(), token.getNextSibling());
					
				} else if (ctorKeyword != null && ctorKeyword.isKeyword("COND")) { 
					// COND type( [let_exp] WHEN log_exp THEN ... WHEN .. )
					distinguishOperators(false, token.getNext(), token.getNextSibling(), "WHEN", "THEN");
				
				} else if (ctorKeyword != null && ctorKeyword.isAnyKeyword("REDUCE", "NEW", "VALUE")) { 
					// "FOR var = rhs [THEN expr] UNTIL|WHILE log_exp [let_exp] ...", see ABAP Reference:
					// - "FOR, Iteration Expressions", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor.htm
					// - "FOR, Conditional Iteration", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_conditional.htm
					distinguishOperators(false, token.getNext(), token.getNextSibling(), "UNTIL|WHILE", "LET|NEXT|FOR");
				
				} else if (ctorKeyword != null && ctorKeyword.isKeyword("FILTER")) { 
		      	// FILTER type( itab [EXCEPT] [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
		      	// FILTER type( itab [EXCEPT] IN ftab [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
					distinguishOperators(false, token.getNext(), token.getNextSibling(), "WHERE", null);
				
				} else if (ctorKeyword != null && ctorKeyword.isKeyword("WHERE")) { 
					// "FOR ... WHERE ( log_exp )" in REDUCE|NEW|VALUE constructor expressions, see ABAP Reference:
		         // - "FOR, Table Iterations", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_itab.htm
					// - "FOR, cond", https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenfor_cond
					distinguishOperators(true, token.getNext(), token.getNextSibling());
				
				} else if (token.textEquals("(")) {
					// use the current value inside the parenthesis
					distinguishOperators(isComparisonPosition, token.getNext(), token.getNextSibling()); 
				
				} else { 
					// method call "identifier(" or table expression "table[" or string template "|text {" or "} text {"
					distinguishOperators(false, token.getNext(), token.getNextSibling());
				}
				token = token.getNextSibling();
			} else {
				token = token.getNextCodeToken();
			}
		}
	}

	public final boolean isAssignment(boolean allowInlineDeclaration, boolean allowTableExpr) {
		Token token = getFirstCodeToken();
		if (token == null) {
			return false;
		} else if (allowInlineDeclaration && token.opensInlineDeclaration()) {
			token = token.getNextSibling();
		} else if (!token.isIdentifier()) {
			return false;
		} else if (allowTableExpr && token.opensTableExpression()) {
			token = token.getEndOfTableExpression();
		}
		
		Token nextToken = token.getNextCodeToken();
		if (nextToken.isChainColon())
			nextToken = nextToken.getNextCodeToken();
		return nextToken != null && nextToken.isAssignmentOperator(); 
	}

	public final boolean splitOutLeadingCommentLines(Command originalCommand) throws UnexpectedSyntaxAfterChanges {
		boolean splitOut = false;
		Token firstCode = getFirstCodeToken();
		int firstLineBreaks = (firstCode == null) ? 1 : firstCode.lineBreaks;
		while (firstToken.isCommentLine() && tokenCount > 1) {
			Token commentLine = firstToken;
			commentLine.removeFromCommand(false, true); // skip the integrity test, as it will be performed below
			
			// the topmost comment line gets the number of line breaks from the first code Token, all others keep their number of line breaks
			commentLine.lineBreaks = Math.max(splitOut ? commentLine.lineBreaks : firstLineBreaks, 1);
			
			Command newCommand = Command.create(commentLine, originalCommand);
			try {
				newCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, commentLine, "parse error splitting out leading comment line");
			}
			insertPrev(newCommand, true); // skip the integrity test, as it will be performed below and there may be more comment lines
			if (!newCommand.isAsteriskCommentLine())
				newCommand.firstToken.spacesLeft = newCommand.getIndent();
			splitOut = true;
		}

		if (splitOut) {
			if (firstCode != null)
				firstCode.lineBreaks = 1;
			testReferentialIntegrity(true);
		}

		return splitOut;
	}

	public final boolean splitOutTrailingCommentLines(Command originalCommand, boolean adjustIndent) throws UnexpectedSyntaxAfterChanges {
		boolean splitOut = false;

		while (lastToken.isCommentLine() && tokenCount > 1) {
			Token commentLine = lastToken;
			commentLine.removeFromCommand(false, true); // skip the integrity test, as it will be performed below
			Command newCommand = Command.create(commentLine, originalCommand, language);
			try {
				newCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, commentLine, "parse error splitting out trailing comment line");
			}
			insertNext(newCommand, true); // skip the integrity test for this Command, as there may be more comment lines 
			if (adjustIndent && !newCommand.isAsteriskCommentLine())
				newCommand.firstToken.spacesLeft = newCommand.getIndent();
			splitOut = true;
		}

		if (splitOut)
			testReferentialIntegrity(true);

		return splitOut;
	}

	/** if the supplied token is followed by a line-end comment and/or further comment lines, this method moves these  
	 * comments into one or several new Commands above the Command of the supplied token */
	public final ArrayList<Command> splitOutCommentLinesAfter(Token token) throws UnexpectedSyntaxAfterChanges {
		Command useOriginalCommand = (originalCommand != null) ? originalCommand : this;
		ArrayList<Command> newCommentCommands = new ArrayList<Command>();
		int splitOutCount = 0;
		int basicIndent = firstToken.getStartIndexInLine();
		
		Token firstTokenInLine = token.getFirstTokenInLine();
		
		while (token.getNext() != null && (token.getNext().isCommentAfterCode() || token.getNext().isCommentLine())) {
			Token comment = token.getNext();
			comment.removeFromCommand();
			int lineBreaks = Math.max(comment.lineBreaks, 1);
			if (splitOutCount == 0) 
				lineBreaks = Math.max(lineBreaks, firstTokenInLine.lineBreaks);
			int indent = comment.isAsteriskCommentLine() ? 0 : basicIndent;
			comment.setWhitespace(lineBreaks, indent);

			Command newCommentCommand = Command.create(comment, useOriginalCommand);
			try {
				newCommentCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, useOriginalCommand, "parse error in extracted comment");
			}

			insertPrev(newCommentCommand, false);
			if (firstToken.lineBreaks > 1)
				firstToken.lineBreaks = 1;

			newCommentCommands.add(newCommentCommand);
			++splitOutCount;
		}
		return newCommentCommands;
	}

	@Override
	public String toString() {
		return toString(ABAP.LINE_SEPARATOR);
	}
	
	public String toString(String lineSeparator) {
		StringBuilder result = new StringBuilder();
		Token token = firstToken;
		while (token != null) {
			result.append(token.toString(lineSeparator));
			token = token.getNext();
		}
		return result.toString();
	}

	public String toStringForErrorMessage(boolean showLineBreaks) {
		StringBuilder result = new StringBuilder();
		Token token = firstToken;
		result.append(token.text);
		while (token.getNext() != null) {
			token = token.getNext();
			if (!token.isComment()) {
				String sep = token.isAttached() ? "" : (showLineBreaks && token.lineBreaks > 0 ? " | " : " ");
				result.append(sep + token.text);
			}
		}
		return result.toString();
	}

	public String toStringWithoutWhitespace() {
		StringBuilder result = new StringBuilder();
		Token token = firstToken;
		while (token != null) {
			result.append(token.text);
			token = token.getNext();
		}
		return result.toString();
	}

	public static String sectionToString(Token start, Token end, boolean replaceInnerLineBreaksWithSingleSpace) {
		StringBuilder result = new StringBuilder();
		Token token = start;
		do {
			if (replaceInnerLineBreaksWithSingleSpace && token.lineBreaks > 0) {
				if (token != start)
					result.append(" ");
				else if (token.spacesLeft > 0)
					result.append(StringUtil.repeatChar(' ', token.spacesLeft));
				result.append(token.text);
			} else {
				result.append(token.toString());
			}
			if (token == end)
				break;
			token = token.getNext();
		} while (true);
		return result.toString();
	}

	/**
	 * Derives the correct indentation from the Command's parent commands; assumes that the Command is NO asterisk comment line
	 * 
	 * @return
	 */
	public final int getIndent() {
		int result = 0;
		Command command = this;
		do {
			// add extra indentation for "WHEN", "CATCH" and "CLEANUP":
			// - CASE [TYPE OF] ... WHEN [TYPE] ... ENDCASE.
			// - TRY ... CATCH ... CLEANUP ... ENDTRY.
			if (command.isWhenStart() || command.isCatch() || command.isCleanup())
				result += ABAP.INDENT_STEP;

			if (command.parent == null)
				break;

			command = command.parent;
			if (command.isTryStart()) // CATCH and CLEANUP are siblings of CATCH, not its parentCommand!
				result += 2 * ABAP.INDENT_STEP;
			else if (!command.firstCodeTokenIsKeyword("REPORT")) // lines directly following REPORT do NOT have extra indentation 
				result += ABAP.INDENT_STEP;
		} while (true);

		Token firstCode = command.getFirstCodeToken();
		if (command.isClassStart() || command.isClassEnd() || command.firstCodeTokenIsKeyword("CLASS") || command.isInterfaceStart() || command.isInterfaceEnd()
				|| command.isIntroductoryStatement()) {
			// result remains unchanged
		} else if (firstCode != null && firstCode.matchesOnSiblings(true, "PUBLIC|PROTECTED|PRIVATE", "SECTION", ".")) {
			// CLASS is a sibling, not the parentCommand of ... SECTION, but these lines are indented by 2 chars
			result += ABAP.INDENT_STEP;
		} else if (!parentCode.hasIntroductoryStatement() && !parentCode.hasClassStart()) {
			// use the original indentation of the first command:
			result += parentCode.getIndentOfFirstCommand();
		}

		// add indent
		result += ABAP.INDENT_STEP * indentAdd;

		// in the special case of INCLUDE TYPE|STRUCTURE ... inside of a BEGIN OF ... END OF block, mimic PrettyPrinter 
		// behavior by making the indentation depend on the previous component / BEGIN OF / END OF line (note that 
		// PrettyPrinter only does this if the previous Command is a chain, otherwise it indents INCLUDE just like TYPES!)
		// This is especially important if AlignDeclarationsRule is deactivated. 
		if (isDeclarationInclude()) {
			command = getPrevNonCommentCommand();
			// skip previous INCLUDE TYPE|STRUCTURE lines
			while (command != null && command.isDeclarationInclude())
				command = command.getPrevNonCommentCommand();
			if (command != null && command.isDeclaration() && command.isSimpleChain()) {
				// make the indent of INCLUDE depend on the previous component / END OF ... / BEGIN OF ... 
				Token lastChainElemStart = command.getLastChainElementStart();
				result = lastChainElemStart.getStartIndexInLine();
				if (lastChainElemStart.matchesOnSiblings(true, "BEGIN", "OF")) {
					result += ABAP.INDENT_STEP;
				} // otherwise, use the same indent as the previous component or the 'END OF ...' line
			}
		}

		return result;
	}

	/** returns the first code Token of the last chain element, i.e. the code Token after : or the last , 
	 * (or returns null for a non-chained Command) */
	public Token getLastChainElementStart() {
		Token token = lastToken;
		while (token != null) {
			if (token.isChainColon() || token.isComma())
				return token.getNextCodeSibling();
			token = token.getPrevCodeSibling();
		}
		return null;
	}
	
	public final boolean addIndent(int addSpaceCount, int minSpacesLeft) {
		return addIndent(addSpaceCount, minSpacesLeft, null, null, false);
	}
	public final boolean addIndent(int addSpaceCount, int minSpacesLeft, Token startToken) {
		return addIndent(addSpaceCount, minSpacesLeft, startToken, null, false);
	}

	public final boolean addIndent(int addSpaceCount, int minSpacesLeft, Token startToken, Token endToken) {
		return addIndent(addSpaceCount, minSpacesLeft, startToken, endToken, false);
	}
	
	/**
	 * adds the provided (and possibly negative) number of spaces to the indent of all lines covered by this Command 
	 * (more precisely, to the first Token in each line), if their current indent is at least (minSpacesLeft).
	 * 
	 * @param addSpaceCount - the number of spaces to add to the indent of affected lines
	 * @param minSpacesLeft - lines with an indent lower than this value will be ignored
	 * @param startToken - the first Token to process (null to use the Command's start token)
	 * @param endToken - the Token (excl.) at which to stop processing (null to process up to the end of the Command)
	 * @param stopAtLowerIndent - stop processing once a line with an indent lower that addSpaceCount is encountered
	 */
	public final boolean addIndent(int addSpaceCount, int minSpacesLeft, Token startToken, Token endToken, boolean stopAtLowerIndent) {
		if (addSpaceCount == 0)
			return false;

		boolean changed = false;
		Token token = (startToken != null) ? startToken : firstToken;
		while (token != endToken) {
			if (token.isFirstTokenInLine() && (isDdlOrDcl() || !token.isAsteriskCommentLine())) {
				if (token.spacesLeft >= minSpacesLeft) {
					token.spacesLeft = Math.max(token.spacesLeft + addSpaceCount, 0);
					changed = true;
				} else if (stopAtLowerIndent) {
					break;
				}
			}
			token = token.getNext();
		}
		return changed;
	}

	/**
	 * inserts the supplied Command as the previous Command; if this Command closes a level, the new Command is inserted
	 * as the last child of the previous sibling
	 * 
	 * @param newCommand
	 */
	public final void insertPrev(Command newCommand, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (!getClosesLevel()) {
			insertLeftSibling(newCommand);
		} else if (prevSibling.hasChildren()) {
			prevSibling.lastChild.insertRightSibling(newCommand, false, skipIntegrityTest);
		} else {
			try {
				prevSibling.insertFirstChild(newCommand, skipIntegrityTest);
			} catch (UnexpectedSyntaxException e) {
				throw new IntegrityBrokenException(newCommand, e.getMessage());
			}
		}
	}

	/**
	 * inserts the supplied Token as the next sibling, or first child after this Token
	 * 
	 * @param newToken
	 * @throws IntegrityBrokenException 
	 */
	public final void insertNext(Command newCommand, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (!getOpensLevel()) {
			insertRightSibling(newCommand, false, skipIntegrityTest);
		} else if (hasChildren()) {
			firstChild.insertLeftSibling(newCommand);
		} else {
			try {
				insertFirstChild(newCommand, skipIntegrityTest);
			} catch (UnexpectedSyntaxException e) {
				throw new IntegrityBrokenException(newCommand, e.getMessage());
			}
		}
	}

	/**
	 * inserts the supplied Command (including its possible children) as a sibling before this Command
	 * 
	 * @param newCommand
	 */
	public final void insertLeftSibling(Command newCommand) throws IntegrityBrokenException {
		if (newCommand == null)
			throw new NullPointerException("newCommand");
		if (newCommand.hasChildren())
			throw new IntegrityBrokenException(this, "Inserting a sibling which has child commands is not supported!");
		if (getClosesLevel())
			throw new IntegrityBrokenException(this, "cannot insert a left sibling to a command that closes a level");

		++parentCode.commandCount;

		newCommand.parentCode = parentCode;
		if (parentCode.firstCommand == this)
			parentCode.firstCommand = newCommand;

		newCommand.parent = parent;
		if (parent != null && parent.firstChild == this)
			parent.firstChild = newCommand;

		if (prev != null)
			prev.next = newCommand;
		newCommand.prev = prev;

		prev = newCommand;
		prev.next = this;

		if (prevSibling != null)
			prevSibling.nextSibling = newCommand;
		newCommand.prevSibling = prevSibling;

		newCommand.nextSibling = this;
		prevSibling = newCommand;
	}

	public final void testReferentialIntegrity(boolean deep) throws IntegrityBrokenException {
		testReferentialIntegrity(deep, false);
	}
	public final void testReferentialIntegrity(boolean deep, boolean testCommentPositions) throws IntegrityBrokenException {
		check(prev == null || prev.next == this);
		check(next == null || next.prev == this);
		check(prevSibling == null || prevSibling.nextSibling == this);
		check(nextSibling == null || nextSibling.prevSibling == this);
		check(firstChild == null || usedLevelOpener != null, firstChild); // but not vice versa
		check(firstChild == null || firstChild == next, firstChild);
		check(firstChild == null || lastChild != null, firstChild);
		check(lastChild == null || firstChild != null);
		check(lastChild == null || !lastChild.getOpensLevel(), lastChild, true);
		check(lastChild == null || lastChild.next == nextSibling, lastChild, true);
		check(parent == null || parent.hasChildren());
		check(firstToken != null);
		check(lastToken != null);
		check(firstToken.getPrev() == null);
		check(lastToken.getNext() == null);
		check(usedLevelOpener == null || !usedLevelOpener.requiresCloser || (nextSibling != null && nextSibling.usedLevelCloser != null), this, true);
		check(usedLevelCloser == null || !usedLevelCloser.requiresOpener || (prevSibling != null && prevSibling.usedLevelOpener != null), this, false, true);
		check(!firstToken.isCommentLine() || tokenCount == 1);

		// ensure that the Command has the correct scope: either it is a stand-alone comment line,
		// or it only contains pragmas, or it ends with a period, only followed by a line-end comment
		if (firstToken.isComment()) {
			check(firstToken == lastToken);
			check(this.isFirstCommandInCode() || firstToken.lineBreaks > 0);
		} else if (isAbap() && !representsEmptyLinesAtCodeEnd()) {
			Token lastCodeToken = getLastCodeToken();
			if (lastCodeToken == null) {
				check(firstToken.isPragma());
			} else {
				check(lastCodeToken.isPeriod());
				Token finalComment = lastCodeToken.getNext();
				if (finalComment != null && finalComment.isPragma()) {
					// tolerate this case, which is frequently found in given code and cannot be corrected wherever it is encountered
				} else {
					check(finalComment == null || (finalComment == lastToken && finalComment.isCommentAfterCode()));
				}
			}
		}
		
		if (!deep)
			return;

		Token token = firstToken;
		while (token != null) {
			check(token.getParentCommand() == this);
			token.testReferentialIntegrity(testCommentPositions && !isDdlOrDcl());
			token = token.getNext();
		}
	}

	private void check(boolean value) throws IntegrityBrokenException {
		check(value, null, false, false);
	}
	private void check(boolean value, Command errorCommand) throws IntegrityBrokenException {
		check(value, errorCommand, false, false);
	}
	private void check(boolean value, Command errorCommand, boolean openingStatementNotClosed) throws IntegrityBrokenException {
		check(value, errorCommand, openingStatementNotClosed, false);
	}
	private void check(boolean value, Command errorCommand, boolean openingStatementNotClosed, boolean closingStatementNotOpened) throws IntegrityBrokenException {
		if (errorCommand == null)
			errorCommand = this;

		if (!value) {
			String explanation = "!";
			if (openingStatementNotClosed) {
				explanation = ": Opening '" + errorCommand.firstToken.text + "' not closed. Please do not process incomplete code blocks.";
			} else if (closingStatementNotOpened) {
				explanation = ": Closing '" + errorCommand.firstToken.text + "' was never opened. Please do not process incomplete code blocks.";
			}
			throw new IntegrityBrokenException(errorCommand,
				"Failed referential integrity test on command starting at source line " + Cult.format(errorCommand.getSourceLineNumStart()) + explanation);
		}
	}

	public final boolean containsInnerCommentLine() {
		Token token = firstToken;
		boolean isAtStartOrEnd = true;
		while (token != null) {
			if (token.isCommentLine()) {
				if (!isAtStartOrEnd)
					return true;
				// leave isAtStartOrEnd unchanged for consecutive comment lines
			} else {
				isAtStartOrEnd = token.textEqualsAny(".", ",", ":");
			}
			token = token.getNext();
		}
		return false;
	}

	public final boolean containsCommentBetween(Token startToken, Token endToken) {
		Token token = startToken;
		while (token != endToken) {
			if (token.isComment()) 
				return true;
			token = token.getNext();
		}
		return false;
	}

	public final void removeFromCode() throws UnexpectedSyntaxException, IntegrityBrokenException {
		if (hasChildren())
			throw new UnexpectedSyntaxException(this, "Removing a Command with its child Commands is not yet supported!");

		if (parentCode.firstCommand == this)
			parentCode.firstCommand = next;
		if (parentCode.lastCommand == this)
			parentCode.lastCommand = prev;
		--parentCode.commandCount;

		if (parent != null) {
			if (parent.firstChild == this && parent.lastChild == this) {
				parent.firstChild = null;
				parent.lastChild = null;
			} else if (parent.firstChild == this) {
				parent.firstChild = next;
			} else if (parent.lastChild == this) {
				parent.lastChild = prev;
			}
		}

		if (prev != null)
			prev.next = next;
		if (next != null)
			next.prev = prev;

		if (prevSibling != null)
			prevSibling.nextSibling = nextSibling;
		if (nextSibling != null)
			nextSibling.prevSibling = prevSibling;

		Command parentTemp = parent;
		parent = null;
		prev = null;
		next = null;
		prevSibling = null;
		nextSibling = null;

		if (parentTemp != null) {
			parentTemp.testReferentialIntegrity(false);
		}
	}

	/**
	 * Inserts the supplied comment text above this command without checking whether it already exists
	 * @param commentText
	 * @return
	 * @throws IntegrityBrokenException
	 */
	public final Command insertCommentLineAbove(String commentText) throws IntegrityBrokenException {
		int indent = isAsteriskCommentLine() ? getIndent() : firstToken.spacesLeft;
		if (getClosesLevel())
			indent += ABAP.INDENT_STEP;

		String insertText = ABAP.COMMENT_SIGN_STRING + " " + commentText.trim();
		Token newComment = Token.createForAbap(1, indent, insertText, getFirstToken().sourceLineNum);
		Command newCommand = Command.create(newComment, this);
		try {
			newCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
		} catch (ParseException e) {
			throw new IntegrityBrokenException(newCommand, "error while creating a new comment above an existing command");
		}

		insertPrev(newCommand, false);
		newCommand.testReferentialIntegrity(true);
		return newCommand;
	}

	/**
	 * Inserts the supplied comment text above the line of the supplied Token.
	 * 
	 * @param token
	 * @param commentText - text without comment sign
	 * @param cancelTexts - if any of these texts are found in an existing comment, supplied comment will not be inserted
	 * @return the Token that contains the new comment; null if no comment was created     
	 * @throws IntegrityBrokenException 
	 */
	public final Token putCommentAboveLineOf(Token token, String commentText, String... cancelTexts) throws UnexpectedSyntaxException, IntegrityBrokenException {
		if (commentText == null)
			throw new NullPointerException("commentText");
		if (token == null)
			throw new NullPointerException("token");
		else if (token.getParentCommand() != this)
			throw new UnexpectedSyntaxException(this, "the token instance '" + token.text + "' is not part of this command!");

		Token firstInLine = token.getFirstTokenInLine();
		// in rare cases, move further lines up, e.g. three lines "LOOP AT get_data(", "* comment", ") INTO DATA(ls_any)."
		while ((firstInLine.closesLevel() || firstInLine.isCommentLine()) && firstInLine.getPrev() != null) {
			firstInLine = firstInLine.getPrev().getFirstTokenInLine();
		}
		boolean isDdlOrDcl = (language == Language.DDL || language == Language.DCL);
		String insertText = (isDdlOrDcl ? DDL.LINE_END_COMMENT : ABAP.COMMENT_SIGN_STRING) + " " + commentText.trim();
		Token newComment = Token.create(firstInLine.lineBreaks, firstInLine.spacesLeft, insertText, firstInLine.sourceLineNum, language);

		// if this Command follows another Command on the same line, put both the comment and this Command to an own line
		if (firstInLine == firstToken && firstInLine.lineBreaks == 0 && !isFirstCommandInCode()) {
			int indent = getIndent();
			firstInLine.setWhitespace(1, indent);
			newComment.setWhitespace(1, indent);
		} else {
			firstInLine.lineBreaks = 1;
		}
		
		if (firstInLine.getPrev() == null) {
			// check whether the comment was already created in a previous cleanup run, potentially checking multiple attached comments
			Command prevCommand = prev;
			while (prevCommand != null && prevCommand.isCommentLine()) {
				String text = prevCommand.firstToken.text;
				if (text.contains(insertText.trim()))
					return null;
				if (cancelTexts != null) {
					for (String cancelText : cancelTexts) {
						if (text.contains(cancelText.trim())) {
							return null;
						}
					}
				}
				if (prevCommand.getFirstTokenLineBreaks() > 1)
					break;
				prevCommand = prevCommand.prev;
			}

			// fix indentation for special cases
			if (isAsteriskCommentLine()) {
				newComment.spacesLeft = getIndent();
			} else if (getClosesLevel()) {
				newComment.spacesLeft += (isDdlOrDcl ? DDL.INDENT_STEP : ABAP.INDENT_STEP);
			}

			// insert a new Command with the new comment Token before the current Command 
			Command newCommand = Command.create(newComment, this);
			try {
				newCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
			} catch (ParseException e) {
				throw new IntegrityBrokenException(newCommand, "error while creating a new comment above a variable declaration");
			}
			insertPrev(newCommand, false);
			newCommand.testReferentialIntegrity(true);
			
		} else {
			// check whether the comment was already created in a previous cleanup run, potentially checking multiple attached comments
			Token prevToken = firstInLine.getPrev();
			while (prevToken != null && prevToken.isCommentLine()) {
				String text = prevToken.text; 
				if (text.contains(insertText.trim()))
					return null;
				if (cancelTexts != null) {
					for (String cancelText : cancelTexts) {
						if (text.contains(cancelText.trim())) {
							return null;
						}
					}
				}
				if (prevToken.lineBreaks > 1)
					break;
				prevToken = prevToken.getPrev();
			}
			// insert the new comment Token before the first Token in line
			firstInLine.insertLeftSibling(newComment, false);
			testReferentialIntegrity(true);
		}
		return newComment;
	}

	/**
	 * Inserts the supplied comment text above the line of the supplied Token.
	 * 
	 * @param token
	 * @param commentText - text without comment sign
	 * @throws IntegrityBrokenException 
	 */
	public final boolean removeMatchingCommentAboveLineOf(Token token, String... commentTextsToMatch) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (token == null)
			throw new NullPointerException("token");
		else if (token.getParentCommand() != this)
			throw new UnexpectedSyntaxException(this, "the token instance '" + token.text + "' is not part of this command!");

		Token firstInLine = token.getFirstTokenInLine();
		
		if (firstInLine.getPrev() == null) {
			// check whether the comment was already created in a previous cleanup run
			if (prev == null || !prev.isCommentLine())
				return false;
			if (prev.firstToken.textEqualsAny(commentTextsToMatch)) {
				firstInLine.lineBreaks = prev.getFirstTokenLineBreaks();
				prev.removeFromCode();
				return true;
			} else {
				return false;
			}
			
		} else {
			// check whether the comment was already created in a previous cleanup run
			if (!firstInLine.getPrev().isComment())
				return false;
			if (firstInLine.getPrev().textEqualsAny(commentTextsToMatch)) {
				firstInLine.getPrev().removeFromCommand();
				return true;
			} else { 
				return false;
			}
		}
	}

	/**
	 * Appends the supplied comment text at the end of the line of the supplied Token.
	 * 
	 * @param token
	 * @param commentText - text without comment sign
	 * @param cancelTexts - if any of these texts are found in an existing comment, supplied comment will not be inserted
	 * @return the Token that contains the new comment; null if no comment was appended     
	 * @throws IntegrityBrokenException 
	 */
	public final Token appendCommentToLineOf(Token token, String commentText, String... cancelTexts) throws UnexpectedSyntaxException, IntegrityBrokenException {
		if (commentText == null)
			throw new NullPointerException("commentText");
		if (token == null)
			throw new NullPointerException("token");
		else if (token.getParentCommand() != this)
			throw new UnexpectedSyntaxException(this, "the token instance '" + token.text + "' is not part of this command!");

		Token lastInLine = token.getLastTokenInLine();
		String appendText = ABAP.COMMENT_SIGN_STRING + " " + commentText.trim();
		if (lastInLine.isComment()) {
			// do not add the comment if any of the cancel texts (old versions of the comment) are found
			if (cancelTexts != null) {
				for (String cancelText : cancelTexts) {
					if (lastInLine.text.contains(cancelText.trim())) {
						return null;
					}
				}
			}
			if (!lastInLine.text.contains(commentText.trim()))  // avoid adding the same comment twice
				lastInLine.text += " " + appendText;
			return lastInLine;
		} else {
			Token newComment = Token.create(0, 1, appendText, lastInLine.sourceLineNum, language);
			if (lastInLine.getNext() == null)
				lastInLine.insertRightSibling(newComment, false);
			else
				lastInLine.getNext().insertLeftSibling(newComment, false);
			testReferentialIntegrity(true);
			return newComment;
		}
	}

	public final boolean removeMatchingCommentFromLineOf(Token token, String... commentTextsToMatch) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (token == null)
			throw new NullPointerException("token");
		else if (token.getParentCommand() != this)
			throw new UnexpectedSyntaxException(this, "the token instance '" + token.text + "' is not part of this command!");
		
		Token lastInLine = token.getLastTokenInLine();
		if (lastInLine == null || !lastInLine.isCommentAfterCode())
			return false;
		
		// remove supplied comment texts from the comment, but expect the comment to possibly contain more text before or after that,
		// e.g. '" any comment " comment to be removed " other comment'
		String text = lastInLine.getText();
		boolean found = false;
		for (String commentTextToMatch : commentTextsToMatch) {
			int matchPos = text.indexOf(commentTextToMatch);
			if (matchPos < 0)
				continue;
			
			found = true;
			int matchEnd = matchPos + commentTextToMatch.length();
			// if the comment was added to an existing comment, an additional space was added by .appendCommentToLineOf()
			if (matchPos > 0 && text.charAt(matchPos - 1) == ' ') {
				--matchPos;
			}
			text = ((matchPos > 0) ? text.substring(0, matchPos) : "") + ((matchEnd < text.length()) ? text.substring(matchEnd) : "");
			text = text.trim();
			if (text.length() == 0) {
				break;
			} else if (!text.startsWith(ABAP.COMMENT_SIGN_STRING)) {
				text = ABAP.COMMENT_SIGN_STRING + " " + text;
			}
		}
		
		if (!found) {
			return false;

		} else if (text.length() == 0 || text.equals(ABAP.COMMENT_SIGN_STRING)) {
			// remove entire line-end comment
			lastInLine.removeFromCommand();
			return true;

		} else {
			// shorten the comment
			lastInLine.setText(text, false);
			return true;
		}
	}

	public final Token getLastNonCommentToken() {
		// DDL can have multiple comment Tokens at the end while the Command is being built
		Token token = lastToken;
		while (token != null && token.isComment())
			token = token.getPrev();
		return token;
	}

	public final Command getNextNonCommentCommand() {
		Command result = next;
		while (result != null && result.isCommentLine())
			result = result.next;
		return result;
	}

	public final Command getPrevNonCommentCommand() {
		Command result = prev;
		while (result != null && result.isCommentLine())
			result = result.prev;
		return result;
	}

	public final Command getNextNonCommentSibling() {
		Command sibling = nextSibling;
		while (sibling != null && sibling.isCommentLine())
			sibling = sibling.nextSibling;
		return sibling;
	}

	public final Command getPrevNonCommentSibling() {
		Command sibling = prevSibling;
		while (sibling != null && sibling.isCommentLine())
			sibling = sibling.prevSibling;
		return sibling;
	}

	public final int getLineBreakSumUpTo(Command endCommand) {
		int result = 0;
		Command command = this;
		while (command != endCommand) {
			result += command.getLineBreakSum();
			command = command.next;
		}
		return result;
	}

	public final int getLineBreakSum() {
		int result = 0;
		Token token = firstToken;
		while (token != null) {
			result += token.lineBreaks;
			token = token.getNext();
		}
		return result;
	}

	public boolean containsInnerLineBreaks(boolean ignoreLineBreakAfterComma) {
		return containsLineBreaksBetween(firstToken, lastToken, ignoreLineBreakAfterComma);
	}
	
	public boolean containsLineBreaksBetween(Token startToken, Token endToken, boolean ignoreLineBreakAfterComma) {
		if (startToken == null)
			throw new NullPointerException("startToken");
		
		Token token = startToken.getNext();
		while (token != null && token != endToken) {
			if (token.lineBreaks > 0) {
				if (!ignoreLineBreakAfterComma) 
					return true;
				Token prev = token.getPrevNonCommentToken();
				if (prev == null || !prev.isComma()) {
					return true;
				}
			}
			token = token.getNext();
		}
		return false;
	}
	
	public final boolean isAbapSqlOperation() {
		return isAbapSqlReadOperation() || isAbapSqlWriteOperation();
	}
	
	public final boolean isAbapSqlReadOperation() {
		Token firstCode = getFirstCodeToken();
		return (firstCode != null) && firstCode.matchesOnSiblings(true, "SELECT|WITH|OPEN CURSOR|FETCH NEXT CURSOR|CLOSE CURSOR");
	}
	
	public final boolean isAbapSqlWriteOperation() {
		Token firstCode = getFirstCodeToken();
		if (firstCode == null) {
			return false;

		} else if (firstCode.isKeyword("UPDATE")) {
			return true;
		
		} else if (firstCode.isKeyword("INSERT")) {
			Token intoKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "INTO");
			if (intoKeyword == null || intoKeyword == firstCode.getNextCodeSibling())
				return true;

		} else if (firstCode.isKeyword("DELETE")) {
			if (firstCode.matchesOnSiblings(true, "DELETE", "FROM", "MEMORY ID|DATABASE|SHARED MEMORY|SHARED BUFFER"))
				return false;
			else if (firstCode.matchesOnSiblings(true, "DELETE", "FROM"))
				return true;
			else if (firstCode.matchesOnSiblings(true, "DELETE", "TABLE|ADJACENT") || firstCode.matchesOnSiblings(true, "DELETE", TokenSearch.ANY_IDENTIFIER, "INDEX|WHERE"))
				return false;
			else if (firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "USING", "KEY"))
				return false;
			// TODO: there remain, however, unclear cases of "DELETE dtab FROM wa" versus "DELETE itab FROM idx1"

		} else if (firstCode.isKeyword("MODIFY")) {
			if (firstCode.matchesOnSiblings(true, "MODIFY", "TABLE") || firstCode.matchesOnSiblings(true, "MODIFY", TokenSearch.ANY_IDENTIFIER, "INDEX"))
				return false;
			else if (firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "USING", "KEY")
					|| firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "TRANSPORTING"))
				return false;
			// TODO: there remain, however, unclear cases of "MODIFY dtab FROM wa" versus "MODIFY itab FROM wa"
		}
		return false;
	}

	public final boolean isInClassDefinition() {
		Command command = this;
		while (command != null) {
			if (command.isClassDefinitionStart)
				return true;

			if (command.parent != null)
				command = command.parent;
			else if (command.isClassEnd() || command.isDeclarationSectionStart()) // ENDCLASS as well as PRIVATE/PROTECTED/PUBLIC SECTION etc. are considered siblings of CLASS
				command = command.prevSibling;
			else
				break;
		}
		return false;
	}

	public final boolean isInClassImplementation() {
		Command command = this;
		while (command != null) {
			if (command.isClassImplementationStart)
				return true;
			
			if (command.parent != null)
				command = command.parent;
			else if (command.isClassEnd()) 
				command = command.prevSibling;
			else
				break;
		}
		return false;
	}

	public final boolean isInInterfaceDefinition() {
		Command command = this;
		while (command != null) {
			if (command.isInterfaceStart())
				return true;
			command = command.parent;
		}
		return false;
	}

	public final String[] getAllKeywordsWithCollocations() {
		ArrayList<String> result = new ArrayList<String>();
		Token token = getFirstCodeToken();
		while (token != null) {
			if (token.isKeyword()) {
				if (!token.collocationContinues)
					result.add(AbapCult.toUpper(token.text));
				else {
					StringBuilder collocation = new StringBuilder(AbapCult.toUpper(token.text));
					do {
						token = token.getNextCodeToken();
						collocation.append(" ").append(AbapCult.toUpper(token.text));
					} while (token.collocationContinues);
					result.add(collocation.toString());
				}
			}
			token = token.getNextCodeToken();
		}
		return result.toArray(new String[0]);
	}

	public final boolean isFunctionalCallOrCallChain() {
		Token token = getFirstCodeToken();
		if (token == null)
			return false;
		if (token.isLiteral() || !token.getOpensLevel())
			return false;
		while (token != null && token.getOpensLevel())
			token = token.getNextSibling();
		token = token.getNextCodeToken();
		return (token != null && token.isPeriod());
	}
	
	public boolean isBlocked(RuleID ruleID, Language[] supportedLanguages) {
		// non-ABAP sections are excluded from processing and changing
		boolean isLanguageSupportedByRule = false;
		for (Language supportedLanguage : supportedLanguages) {
			if (supportedLanguage == language) {
				isLanguageSupportedByRule = true;
				break;
			}
		}
		return !isLanguageSupportedByRule || getChangeControl().isRuleBlocked(ruleID) || !isInCleanupRange();
	}

	public boolean isInCleanupRange() {
		if (!parentCode.hasCleanupRange()) {
			// the whole code is being cleaned
			return true;
		} else { 
			// Determine whether this Command is part of the cleanup. Note that this depends on the original position 
			// of the Command which may have changed in the meantime, e.g. with IfBlockAtLoopEndRule and IfBlockAtMethodEndRule.
			// When applying a Rule, we therefore always visit all Commands.

			// both CleanupRange and sourceLineNum.. use 1-based, inclusive values  
			CleanupRange cleanupRange = parentCode.getCleanupRange();
			int rangeStart = cleanupRange.startLine;
			int rangeLast = cleanupRange.lastLine;

			if (rangeStart <= sourceLineNumLast && rangeLast >= sourceLineNumStart) {
				// the cleanup range intersects with the text of the command (NOT just with the empty lines above it)
				return true;
			} else if (rangeStart > sourceLineNumStart - firstToken.lineBreaks && rangeLast < sourceLineNumStart) {
				// the whole(!) cleanup range is within the empty lines above a Command (note that .lineBreaks == 4 means 3 empty lines)
				return true;
			} else {
				return false;
			}
			
			// return (cleanupRange.startLine < sourceLineNumLast && cleanupRange.endLine >= sourceLineNumStart)
			// 		|| (cleanupRange.startLine >= sourceLineNumStart - firstToken.lineBreaks && cleanupRange.endLine < sourceLineNumStart);
		}
	}

	/**
	 * Assuming this Command starts the definition of an INTERFACE, CLASS or METHOD, 
	 * this method returns the interface, class or method name.
	 * @return
	 */
	public final String getDefinedName() {
		Token firstCode = getFirstCodeToken();				
		if (firstCode == null || !firstCode.isKeyword())
			return null;
		Token token = firstCode.getNextCodeSibling();
		while (token.isChainColon())
			token = token.getNextCodeSibling();
		return (token == null || !token.isIdentifier()) ? null : token.getText(); 
	}
	
	void onTermInserted(Term term) {
		updateChainColonCount();
	}

	void onTokenInserted(Token token) {
		if (token.isChainColon()) {
			++chainColonCount;
		}
	}

	void onTokenRemoved(Token token) {
		if (chainColonCount > 0 && token.isChainColon()) {
			--chainColonCount;
		}
	}
	
	public void removeAllChainColons() throws UnexpectedSyntaxAfterChanges {
		// after the first chain colon : was removed from a Command (by the caller), all further colons must be removed, too: 
		// such additional colons are syntactically correct and are simply ignored while there is another colon before them: 
		// 'If further colons are listed after the first colon of a chained statement, they are handled like blanks' 
		// (https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenchained_statements.htm)
		// After removing the first colon, however, the next colon would change the behavior. 
		if (!containsChainColon()) 
			return;
		
		Token token = firstToken;
		while (token != null) {
			if (token.isChainColon()) {
				Token colon = token;
				
				// ensure the next Token has at least one space before it
				Token next = colon.getNext();
				if (next != null && next.isAttached()) 
					next.setWhitespace();

				colon.removeFromCommand();
				token = next;
			} else {
				token = token.getNext();
			}
		}
		chainColonCount = 0;
	}
	
	public Command copyTokenRangeToNewCommand(Token startToken, Token endToken, int startLineBreaks, int startSpacesLeft) throws UnexpectedSyntaxException {
		Token sourceToken = startToken;
		Command newCommand = null;
		while (sourceToken != null && sourceToken != endToken) {
			if (newCommand == null) {
				Token newToken = Token.create(startLineBreaks, startSpacesLeft, sourceToken.getText(), sourceToken.getNext().sourceLineNum, language);
				newCommand = Command.create(newToken, originalCommand);
			} else {
				Token newToken = Token.create(sourceToken.lineBreaks, sourceToken.spacesLeft, sourceToken.getText(), sourceToken.getNext().sourceLineNum, language);
				newCommand.getLastToken().addNext(newToken);
			}
			sourceToken = sourceToken.getNext(); 
		} 
		return newCommand;
	}

	public final Language getLanguageOfNextCommand() {
		if (language != Language.ABAP) {
			return language;
		} 
		
		Token firstCode = getFirstCodeToken();
		if (firstCode == null) {
			return language;
		} else if (firstCode.isKeyword("EXEC")) {
			return Language.SQL;
		
		} else if (startsAMDPMethod()) {
			// for METHOD <identifier> BY DATABASE PROCEDURE|FUNCTION|GRAPH WORKSPACE, see 
			// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapmethod_by_db_proc.htm
			Token languageToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "LANGUAGE");
			if (languageToken != null)
				languageToken = languageToken.getNextCodeSibling();
			if (languageToken == null) 
				return Language.OTHER;
			if (languageToken.isKeyword("SQLSCRIPT"))
				return Language.SQLSCRIPT;
			else if (languageToken.isKeyword("SQL"))
				return Language.SQL;
			else if (languageToken.isKeyword("GRAPH"))
				return Language.GRAPH;
			else if (languageToken.isKeyword("LLANG"))
				return Language.LLANG;
			else
				return Language.OTHER;
		} else
			return language;
	}
	
	public final boolean startsAMDPMethod() {
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.matchesOnSiblings(true, "METHOD", TokenSearch.ANY_IDENTIFIER, "BY", "DATABASE", "PROCEDURE|FUNCTION|GRAPH");
	}

	public final Command getStartOfAttachedComments() {
		Command command = this;
		while (command.getFirstTokenLineBreaks() == 1 && command.getPrev() != null && command.getPrev().isCommentLine())
			command = command.getPrev();
		return command;
	}
	
	public final boolean isPublicClassDefinitionStart() { 
		if (!isClassDefinitionStart())
			return false;
		Token token = getFirstCodeToken();
		while (token != null) {
			if (token.isKeyword("CREATE")) {
				// skip the next token (PUBLIC / PROTECTED / PRIVATE)
				token = token.getNextCodeSibling();
			} else if (token.isKeyword("PUBLIC")) {
				return true;
			}
			token = token.getNextCodeSibling();
		}
		return false;
	}

	public final boolean isInsideBeginOfEnumBlock() {
		if (initialBlockLevel != 1) 
			return false;
		
		Command command = getPrev();
		
		while (command != null) {
			Token start = command.getFirstCodeToken();
			
			// find the last BEGIN OF ENUM or END OF ENUM in this Command (note that TYPES: may chain all sorts of blocks) 
			Token lastEnum = null;
			while (start != null) {
				start = start.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "BEGIN|END", "OF", "ENUM");
				if (start != null)
					lastEnum = start;
			}
			if (lastEnum != null) {
				return lastEnum.getPrevCodeSibling().getPrevCodeSibling().isKeyword("BEGIN");
			} else if (command.initialBlockLevel != 1) {
				return false;
			}
			
			command = command.getPrev();
		}
		return false;
	}
	
	/**
	 * returns true if the Command only consists of one line with one or several pragmas (and potentially a line-end comment) 
	 */
	public final boolean isPragmaLine() {
		if (!firstToken.isPragma())
			return false;
		// starting from the second Token, check for pragmas and line breaks  
		Token token = firstToken.getNext();
		while (token != null && token.isPragma() && token.lineBreaks == 0) {
			token = token.getNext();
		}
		return (token == null || token.isComment());
	}

	public final boolean isSimpleChain() { 
		Token token = getFirstCodeToken();
		if (token == null || !token.isKeyword())
			return false;
		token = token.getNextCodeToken();
		return (token != null) && token.isChainColon();
	}

	/** returns true if this Command uses a macro */
	public final boolean usesMacro() {
		// a macro starts with an identifier which 
		// - does NOT end with an opening parentheses (as a functional method call does), and  
		// - is NOT followed by an assignment operator (as an assignment does, including calculation assignments with += etc.) 
		Token next = firstToken.getNextCodeSibling();
		return (firstToken.isIdentifier() && !firstToken.getOpensLevel() && next != null && next.type != TokenType.ASSIGNMENT_OP);
	}

	/** returns the corresponding opening Command for ENDMETHOD, ENDLOOP, ENDIF, ENDFORM, CATCH etc. (skipping ELSEIF etc.) */
	public Command getOpeningCommand() {
		if (usedLevelCloser == null || !usedLevelCloser.requiresOpener)
			return null;
		Command command = this;
		while (command.usedLevelCloser != null && command.usedLevelCloser.requiresOpener && command.prevSibling != null)
			command = command.prevSibling;
		return command;
	}

	/** returns the corresponding closing Command for METHOD, LOOP, IF, FORM, CATCH etc. (skipping ELSEIF etc.) */
	public Command getClosingCommand() {
		if (usedLevelOpener == null || !usedLevelOpener.requiresCloser)
			return null;
		Command command = this;
		while (command.usedLevelOpener != null && command.usedLevelOpener.requiresCloser && command.nextSibling != null)
			command = command.nextSibling;
		return command;
	}

	public final Token findTokenOfType(TokenType tokenType, String... texts) {
		if (firstToken.type == tokenType && firstToken.textEqualsAny(texts))
			return firstToken;
		else 
			return firstToken.getNextTokenOfTypeAndText(tokenType, texts);
	}

	public boolean isInLoop() {
		Command test = parent;
		while (test != null) {
			if (test.startsLoop()) {
				return true;
			}
			test = test.parent;
		}
		return false;
	}

	/**
	 * Returns true if the Command reads the specified system field
	 * @param fieldName - field name without prefix SY- or SYST-, e.g. "subrc" or "tabix"
	 */
	public final boolean readsSyField(ABAP.SyField syField) {
		Token test = getFirstCodeToken();
		while (test != null) {
			// both SY-... and SYST-... could be used to access the same system structure
			if (test.textEqualsAny(syField.syField, syField.systField) && test.getMemoryAccessType().mayRead) {
				return true;
			} else if (syField == SyField.SUBRC && test.isIdentifier() && test.textEqualsAny("cl_abap_unit_assert=>assert_subrc(", "cl_abap_unit_assert=>assert_return_code(")) {
				// in this special case, it is accepted that SY-SUBRC is checked in a different method
				return true;
			}
			test = test.getNextCodeToken();
		}
		return false;
	}

	/**
	 * Returns true if the Command explicitly sets the specified system field (having it in a write position)
	 */
	public final boolean changesSyField(ABAP.SyField syField) {
		switch (syField) {
			case SUBRC:
				return changesSySubrc();
			case TABIX:
				return changesSyTabix();
			case INDEX:
				return changesSyIndex();
			case TFILL: 
			case TLENG:
				return changesSyTFillOrTLeng();
			default:
				throw new IllegalArgumentException();
		}
	}

	/**
	 * Returns true if the Command explicitly sets the specified system field (having it in a write position)
	 */
	private final boolean writesToSyField(ABAP.SyField syField) {
		Token test = getFirstCodeToken();
		while (test != null) {
			// both SY-... and SYST-... could be used to access the same system structure
			if (test.textEqualsAny(syField.syField, syField.systField) && test.getMemoryAccessType().mayWrite) {
				return true;
			}
			test = test.getNextCodeToken();
		}
		return false;
	}

	/** Returns true if the Command changes sy-subrc */
	public final boolean changesSySubrc() {
		// the following is structured like the ABAP statements overview in   
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenabap_statements_overview.htm
		// and considers all places in the ABAP keyword documentation that explicitly mention sy-subrc
		
		Token token = getFirstCodeToken();
		if (token == null)
			return false;

		// direct assignments to sy-subrc
		if (writesToSyField(ABAP.SyField.SUBRC))
			return true;
		
		// ----------------------------------------------------------------------
		// Object Creation

		if (token.matchesOnSiblings(true, "CREATE", "OBJECT")) {
			// true, because this is a method call to the instance constructor
			return true;
		} else if (!token.matchesOnSiblings(true, "AT", "NEW") && !token.matchesOnSiblings(true, "SELECT")) {
			// the instance operator NEW always sets 0, except when anonymous data objects are created (not set)
			Token newToken = findTokenOfType(TokenType.KEYWORD, "NEW");
			while (newToken != null) {
				if (newToken.getNext() != null && newToken.getNext().isIdentifier()) {
					return true;
				}
				newToken = newToken.getNextTokenOfTypeAndText(TokenType.KEYWORD, "NEW");
			}
		}

		// ----------------------------------------------------------------------
		// Calling and Exiting Program Units

		// Calling Programs
		if (token.isKeyword("SUBMIT"))
			return true;
		
		// Calling Processing Blocks
		// - PERFORM / FORM ... ENDFORM itself does NOT change sy-subrc (but inside the FORM, something may happen, of course) 
		if (token.matchesOnSiblings(true, "CALL", "FUNCTION|METHOD")) {
			// for both static and dynamic calls, sy-subrc is set to 0 upon calling; later, it may be set to a non-class-based exception value
			// however, after CALL FUNCTION ... IN UPDATE TASK, sy-subrc is undefined
			// CALL METHOD/FUNCTION ... EXCEPTION-TABLE can contain NAME / VALUE pairs for exception values
			return true;
		} else if (token.matchesOnSiblings(true, "SET", "HANDLER")) {
			return true;
		} else {
			// search for a functional method call, which sets sy-subrc = 0 when returning to the caller with ENDMETHOD;
			// built-in functions such as xsdbool( ... ), line_exists( ... ), cos( ... ) etc. do NOT set sy-subrc 
			// and are therefore excluded here; 
			// TODO: however, if a class happens to have a method of the same name as a built-in functions, 
			// this method will be called instead of the built-in function (it 'shadows' the built-in function even 
			// if it has a different signature); this distinction is not yet considered
			Token test = token;
			boolean considerBuiltInFunctionsAsMethodCalls = false;
			while (test != null) {
				if (test.startsFunctionalMethodCall(considerBuiltInFunctionsAsMethodCalls)) {
					return true;
				}
				test = test.getNextCodeToken();
			}
		}

		// ----------------------------------------------------------------------
		// Program Flow Logic

		// Program Interruption
		if (token.matchesOnSiblings(true, "WAIT", "UP", "TO")) {
			// always sets sy-subrc = 0
			return true;
		}

		// Exception Handling
		if (token.isKeyword("RAISE")) {
			return true;
		}
		
		// ----------------------------------------------------------------------
		// Assignments

		// Setting References
		if (token.isKeyword("ASSIGN")) {
			// however: NOT for the static_dobj variant: ASSIGN dobj[+off][(len)]
			return true;
		}

		// ----------------------------------------------------------------------
		// Processing Internal Data

		// Character String and Byte String Processing
		if (token.isAnyKeyword("CONCATENATE", "FIND", "OVERLAY", "REPLACE", "SPLIT")) {
			// including REPLACE SECTION ... OF
			return true;
		} else if (token.isKeyword("SHIFT") && token.matchesOnSiblings(true, TokenSearch.ASTERISK, "UP", "TO")) {
			return true;
		} else if (token.matchesOnSiblings(true, "GET|SET", "BIT")) {
			return true;
		} else if (token.matchesOnSiblings(true, "WRITE", TokenSearch.ASTERISK, "TO")) {
			return true;
		}

		// Date and Time Processing
		if (token.matchesOnSiblings(true, "CONVERT", TokenSearch.ASTERISK, "INTO", "TIME", "STAMP")) {
			return true;
		} else if (token.matchesOnSiblings(true, "CONVERT", "TIME", "STAMP")) {
			return true;
		}

		// Internal Tables
		// - FIND|REPLACE ... IN TABLE see above
		if (token.isAnyKeyword("DELETE", "INSERT", "MODIFY") && !token.matchesOnSiblings(true, "DELETE", "DYNPRO|REPORT|TEXTPOOL")) {
			// same for DELETE|INSERT|MODIFY mesh_path (and dbtab, see below)
			return true;
		} else if (token.isKeyword("ENDLOOP")) {
			// note that sy-subrc is set by ENDLOOP, not by 'LOOP AT'
			return true;
		} else if (token.matchesOnSiblings(true, "LOOP", "AT", "GROUP")) {
			return true;
		} else if (token.matchesOnSiblings(true, "READ", "TABLE")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// Processing External Data

		// ABAP SQL
		// - DELETE, INSERT, MODIFY see above
		if (token.isAnyKeyword("FETCH", "SELECT", "UPDATE")) {
			return true;
		} else if (token.matchesOnSiblings(true, "OPEN", "CURSOR")) {
			return true;
		}
		
		// Native SQL
		if (token.isKeyword("ENDEXEC")) {
			return true;
		} 

		// Data Clusters
		// - DELETE FROM see above
		if (token.isKeyword("IMPORT")) {
			// including "IMPORT DIRECTORY" (and "IMPORT DYNPRO")
			return true;
		}

		// File Interface
		if (token.matchesOnSiblings(true, "CLOSE|DELETE|GET|OPEN|READ|SET|TRUNCATE", "DATASET")) {
			return true;
		} else if (token.isKeyword("TRANSFER")) {
			return true;
		}

		// Data Consistency
		if (token.isKeyword("AUTHORITY-CHECK")) {
			return true;
		} else if (token.matchesOnSiblings(true, "COMMIT|ROLLBACK", "WORK")) {
			return true;
		} else if (token.matchesOnSiblings(true, "SET", "UPDATE", "TASK", "LOCAL")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// ABAP for RAP Business Objects

		// ABAP EML
		if (token.matchesOnSiblings(true, "COMMIT", "ENTITIES")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// Program Parameters
		
		// User Memory
		if (token.matchesOnSiblings(true, "GET", "PARAMETER")) {
			return true;
		}

		// Language Environment
		if (token.matchesOnSiblings(true, "SET", "COUNTRY|LANGUAGE")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// Program Editing
		
		// Testing and Checking Programs
		if (token.matchesOnSiblings(true, "SET", "RUN", "TIME", "ANALYZER")) {
			return true;
		}

		// Dynamic Program Development
		if (token.matchesOnSiblings(true, "GENERATE", "SUBROUTINE", "POOL")) {
			return true;
		} else if (token.matchesOnSiblings(true, "INSERT|READ", "REPORT|TEXTPOOL")) {
			return true;
		} else if (token.isKeyword("SYNTAX-CHECK")) {
			// including SYNTAX-CHECK FOR PROGRAM and SYNTAX-CHECK FOR DYNPRO
			return true;
		} 

		// ----------------------------------------------------------------------
		// ABAP Data and Communication Interfaces

		// Remote Function Call
		if (token.matchesOnSiblings(true, "RECEIVE", "RESULTS", "FROM", "FUNCTION")) {
			return true;
		} else if (token.matchesOnSiblings(true, "WAIT", "FOR", "ASYNCHRONOUS", "TASKS")) {
			return true;
		} else if (token.matchesOnSiblings(true, "WAIT", "FOR", "MESSAGING|PUSH", "CHANNELS")) {
			return true;
		} 

		// OLE Interface
		// - CALL METHOD and CREATE OBJECT see above
		if (token.matchesOnSiblings(true, "FREE", "OBJECT")) {
			return true;
		} else if (token.matchesOnSiblings(true, "GET|SET", "PROPERTY")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// User Dialogs

		// Dynpros
		if (token.matchesOnSiblings(true, "GET", "CURSOR")) {
			return true;
		} else if (token.matchesOnSiblings(true, "SET", "TITLEBAR")) {
			return true;
		}

		// Selection Screens
		if (token.matchesOnSiblings(true, "CALL", "SELECTION-SCREEN")) {
			return true;
		}

		// Lists
		// - MODIFY [CURRENT] LINE, GET CURSOR see above
		if (token.matchesOnSiblings(true, "DESCRIBE", "LIST")) {
			return true;
		} else if (token.matchesOnSiblings(true, "READ", "LINE")) {
			return true;
		} else if (token.matchesOnSiblings(true, "SCROLL", "LIST")) {
			return true;
		}

		// ----------------------------------------------------------------------
		// Enhancements
		
		// Enhancements Using BAdIs
		if (token.matchesOnSiblings(true, "CALL", "BADI")) {
			return true;
		}
		
		// ----------------------------------------------------------------------
		// Statements for Experts

		if (token.isAnyKeyword("PROVIDE", "ENDPROVIDE")) {
			return true;
		} 

		// ----------------------------------------------------------------------
		// Obsolete Statements

		// Obsolete Calls
		if (token.matchesOnSiblings(true, "CALL", "CUSTOMER-FUNCTION")) {
			return true;
		} else if (token.matchesOnSiblings(true, "CALL", "DIALOG", TokenSearch.ASTERISK, "IMPORTING")) {
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapcall_dialog.htm
			// TODO: more conditions to this?
			return true;
		}

		// Obsolete Exception Handling
		if (token.matchesOnSiblings(true, "CATCH", "SYSTEM-EXCEPTIONS")) {
			return true;
		}

		
		// Obsolete Character String and Byte String Processing
		// - REPLACE ... WITH see above (more conditions to this?)
		if (token.isKeyword("SEARCH")) {
			return true;
		} 
		
		// Obsolete Internal Table Processing
		// - SEARCH itab: see SEARCH above
		// - WRITE ... TO: see above
		if (token.matchesOnSiblings(true, "REFRESH", TokenSearch.ASTERISK, "FROM", "TABLE")) {
			return true;
		} 

		// Obsolete Extracts
		// see ENDLOOP above

		// Obsolete Database Access
		// see LOOP AT / READ TABLE above

		// Contexts
		if (token.isKeyword("DEMAND")) {
			return true;
		} 
		
		// Obsolete Editor Calls
		if (token.matchesOnSiblings(true, "EDITOR-CALL", "FOR")) {
			return true;
		} 

		// Obsolete External Programming Interface
		if (token.isKeyword("COMMUNICATION")) {
			return true;
		} 

		// ----------------------------------------------------------------------
		// Internal Statements
		
		// Program Editing
		// - IMPORT DYNPRO, SYNTAX-CHECK FOR DYNPRO: see above
		if (token.matchesOnSiblings(true, "DELETE|GENERATE", "DYNPRO|REPORT")) {
			return true;
		} else if (token.matchesOnSiblings(true, "DELETE", "TEXTPOOL", TokenSearch.ASTERISK, "STATE")) {
			// apparently, sy-subrc is only set with the STATE addition
			return true;
		} else if (token.matchesOnSiblings(true, "LOAD", "REPORT")) {
			return true;
		} else if (token.isKeyword("SCAN")) {
			return true;
		}

		// External Interface
		if (token.matchesOnSiblings(true, "CALL", TokenSearch.ASTERISK, "ID")) {
			// sy-subrc may be set by a System Function Call
			return true;
		}
		
		return false;
	}

	/** Returns true if the Command changes SY-TABIX */
	public final boolean changesSyTabix() {
		// the following is structured like the ABAP statements overview in   
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenabap_statements_overview.htm
		// and considers all places in the ABAP keyword documentation that explicitly mention SY-TABIX

		// SY-TABIX is explicitly NOT changed by
		// - INSERT/MODIFY/DELETE itab
		// - FIND/REPLACE ... IN TABLE
		// - table expressions unless ASSIGN is used
		// - FOR, Table Iteration (instead, 'INDEX INTO idx' can be used)

		Token token = getFirstCodeToken();
		if (token == null)
			return false;

		// direct assignments to SY-TABIX
		if (writesToSyField(ABAP.SyField.TABIX))
			return true;
		
		// ----------------------------------------------------------------------
		// Assignments

		// Setting References
		if (token.isKeyword("ASSIGN")) {
			// determine whether a table expressions is assigned, which sets SY-TABIX and SY-SUBRC just like 
			// READ TABLE ... ASSIGNING does; otherwise, SY-TABIX is not changed
			Token test = token;
			while (test != null) {
				if (test.getOpensLevel() && test.textEndsWith("[")) {
					return true;
				}
				test = test.getNextCodeSibling();
			}
			return false;
		}

		// ----------------------------------------------------------------------
		// Processing Internal Data

		// Internal Tables
		if (token.matchesOnSiblings(true, "LOOP", "AT")) { 
			// - for standard or sorted tables, LOOP AT sets SY-TABIX to the line number in the used (primary or secondary) index;  
			// - for hashed tables, it sets SY-TABIX = 0
			// - LOOP AT ... GROUP BY / LOOP AT GROUP ... / LOOP AT mesh_path also set SY-TABIX. 
			// - note that LOOP AT (like DESCRIBE TABLE and READ TABLE) also sets SY-TFILL (number of lines) and SY-TLENG (line size) 
			return true;
		} else if (token.isKeyword("ENDLOOP")) {
			// sets SY-TABIX to the value before entering the LOOP
			return true;

		} else if (token.matchesOnSiblings(true, "READ", "TABLE")) {
			// - for sy-subrc = 0, sets SY-TABIX to the line number in the primary or secondary table index where the entry was found (or 0 if a hash key was used)
			// - for sy-subrc = 4, sets SY-TABIX to the line number before the position where the entry would need to be
			// - for sy-subrc = 8, sets SY-TABIX to the number of table lines + 1
			// - also, see detailed documentation for READ TABLE, table_key / READ TABLE, free_key
			// - note that READ TABLE (like DESCRIBE TABLE and LOOP AT) also sets SY-TFILL (number of lines) and SY-TLENG (line size)
			return true;
			
		} else if (token.isKeyword("COLLECT")) {
			// for standard or sorted tables, sets SY-TABIX to the line number of the inserted or existing line in the primary index
			// for hashed tables, sets SY-TABIX = 0
			return true;

		} else if (token.isKeyword("APPEND")) {
			// sets SY-TABIX to the line number of the last appended line in the primary index
			return true;
		}

		// ----------------------------------------------------------------------
		// Statements for Experts

		if (token.isAnyKeyword("PROVIDE", "ENDPROVIDE")) {
			// PROVIDE sets SY-TABIX to 0 before every loop pass; ENDPROVIDE also sets it to 0
			return true;
		} 
		
		return false;
	}

	/** Returns true if the Command changes SY-INDEX */
	public final boolean changesSyIndex() {
		// the following is structured like the ABAP statements overview in   
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenabap_statements_overview.htm
		// and considers all places in the ABAP keyword documentation that explicitly mention SY-INDEX

		// SY-INDEX is explicitly NOT changed by "FOR, Conditional Iteration"

		Token token = getFirstCodeToken();
		if (token == null)
			return false;

		// direct assignments to SY-INDEX
		if (writesToSyField(ABAP.SyField.INDEX))
			return true;
		
		// ----------------------------------------------------------------------
		// Program Flow Logic

		// Control Structures
		if (token.isAnyKeyword("DO", "WHILE", "ENDDO", "ENDWHILE")) {
			// - DO and WHILE set SY-INDEX to the number of previous loop passes, including the current one
			//   (this is done immediately, so a condition like 'WHILE sy-index <= 3.' can already evaluate it)
			// - ENDDO and ENDWHILE restore it to its previous value, so it always refers to the current loop 
			return true;
		}
		
		return false;
	}

	/** Returns true if the Command changes SY-TFILL or SY-TLENG */
	public final boolean changesSyTFillOrTLeng() {
		Token token = getFirstCodeToken();
		if (token == null)
			return false;

		// direct assignments to SY-TFILL or SY-TLENG
		if (writesToSyField(ABAP.SyField.TFILL) || writesToSyField(ABAP.SyField.TLENG))
			return true;
		
		// ----------------------------------------------------------------------
		// Processing Internal Data

		// Internal Tables
		if (token.matchesOnSiblings(true, "LOOP", "AT") && !token.matchesOnSiblings(true, "LOOP", "AT", "GROUP")) { 
			return true;
		} else if (token.matchesOnSiblings(true, "READ", "TABLE")) {
			return true;
		}
		
		// Properties of Data Objects
		if (token.matchesOnSiblings(true, "DESCRIBE", "TABLE")) {
			return true;
		}

		return false;
	}

	public final boolean containsMethodCallInsideConstructorExp() {
		Token token = firstToken;
		while (token != null) {
			if (token.isAnyKeyword(ABAP.constructorOperators)) {
				token = token.getNextCodeSibling();
				if (token.getOpensLevel() && !token.textEquals("(")) {
					if (containsFunctionalMethodCallBetween(token.getFirstChild(), token.getNextCodeSibling())) {
						return true;
					}
					token = token.getNextSibling();
				}
			}
			token = token.getNextCodeToken();
		}
		return false;
	}

	public final boolean containsFunctionalMethodCallBetween(Token startToken, Token endToken) {
		Token token = startToken;
		while (token != null && token != endToken) {
			if (token.startsFunctionalMethodCall(false)) {
				return true;
			}
			token = token.getNextCodeToken();
		}
		return false;
	}
	
	public final boolean containsChainColonInsideParentheses() {
		Token token = firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, ":");
		return (token != null && token.getParent() != null);
	}

	public final boolean insertStressTestTokenAt(int tokenIndex, StressTestType stressTestType) throws IntegrityBrokenException {
		if (!isAbap() || representsEmptyLinesAtCodeEnd() || isPragmaLine())
			return false;

		// if chain colon shall be inserted, this can be done
		// - anywhere if no chain colon exists in the Command (thus creating a "chain of one"), or otherwise
		// - somewhere after an existing one chain colon (so the new chain colon has no effect on the chain);
		//   in this case, tokenIndex = 0 refers to the existing colon  
		// - but never in ABAP SQL Commands like SELECT, UPDATE, WITH etc. (in which "," has a different meaning)
		Token token = firstToken;
		if (stressTestType == StressTestType.COLON) {
			if (isAbapSqlOperation())
				return false;
			token = token.getLastTokenDeep(true, TokenSearch.ASTERISK, ":");
			if (token == null) {
				token = firstToken;
			}
		}
		
		int index = 0;
		while (token != null) {
			boolean canInsert = token.canInsertStressTestTokenAfter(stressTestType); 
			if (canInsert) {
				if (index >= tokenIndex && token.insertStressTestTokenAfter(stressTestType)) 
					return true;
				++index;
			}
			token = token.getNext();
		}
		return false;
	}

	public final boolean representsEmptyLinesAtCodeEnd() {
		return (this == parentCode.lastCommand && firstToken == lastToken && firstToken.lineBreaks > 0 && firstToken.getTextLength() == 0);
	}

	public final boolean belongsToMacroDefinition() {
		Command command = this;
		while (command != null) {
			if (command.firstCodeTokenIsKeyword("DEFINE"))
				return true;
			command = command.parent;
		}
		return false;
	}
	
	/**
	 * Handles the specified chain element (or, if the Command is not a chain, the entire Command) with the specified action.
	 * @param start - the Token with which the chain element starts; in non-chains, the Token after the ABAP keyword
	 * @param action - whether to add a comment, comment out the chain element, or remove it 
	 * @param commentText - comment text (without comment sign) in case of ChainElementAction.ADD_TODO_COMMENT
	 * @return
	 * @throws UnexpectedSyntaxException
	 * @throws UnexpectedSyntaxAfterChanges
	 * @throws IntegrityBrokenException
	 */
	public final boolean handleChainElement(Token start, ChainElementAction action, String commentText) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (action == ChainElementAction.IGNORE) {
			return false;

		} else if (action == ChainElementAction.ADD_TODO_COMMENT) {
			boolean commentAdded = false;
			if (getClosesLevel()) { // in case of 'CATCH ... INTO DATA(...)', the comment cannot be added as a 'previous sibling', because that must be the TRY statement 
				commentAdded = (appendCommentToLineOf(start, commentText) != null);
			} else {
				commentAdded = (putCommentAboveLineOf(start, commentText) != null);
			}
			return commentAdded;

		} else if (action == ChainElementAction.ADD_PRAGMA_NEEDED) {
			Token commaOrPeriod = start.getLastTokenDeep(true, TokenSearch.ASTERISK, ".|,");
			Token neededPragma = Token.createForAbap(0, 1, "##NEEDED", commaOrPeriod.sourceLineNum);
			if (commaOrPeriod.lineBreaks > 0) {
				neededPragma.copyWhitespaceFrom(commaOrPeriod);
				commaOrPeriod.setWhitespace(0, 0);
			}
			commaOrPeriod.insertLeftSibling(neededPragma);
			return true;
		}
		
		// get information on the line to be deleted / commented out and the surrounding lines
		Command originalCommand = (this.originalCommand != null) ? this.originalCommand : this;
		Token keyword = getFirstCodeToken();
		Token colon = keyword.getNextCodeToken().isChainColon() ? keyword.getNextCodeToken() : null;
		// boolean isKeywordOnSameLine = (identifier.lineBreaks == 0);
		Token commaOrPeriod = start.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ".|,");
		Token lastTokenInLine = (commaOrPeriod.getNext() != null && commaOrPeriod.getNext().isCommentAfterCode()) ? commaOrPeriod.getNext() : commaOrPeriod;
		boolean isInChain = isSimpleChain();
		boolean isFirstInChain = isInChain && start.getPrevCodeToken().isChainColon();
		boolean isLastInChain = isInChain && commaOrPeriod.isPeriod();
		boolean isOnlyOneInChain = isFirstInChain && isLastInChain;
		Token prevComma = (isInChain && !isFirstInChain) ? start.getPrevCodeSibling() : null;
		Token nextIdentifier = (isInChain && !isLastInChain) ? lastTokenInLine.getNextCodeSibling() : null;
		Token firstTokenInLine = (isInChain && !isFirstInChain) ? start : keyword;

		// make the surrounding chain work without the line to be deleted / commented out
		if (isInChain && !isOnlyOneInChain) {
			if (isLastInChain) {
				prevComma.setText(ABAP.DOT_SIGN_STRING, false);
				prevComma.type = TokenType.PERIOD;
			} else if (isFirstInChain) {
				nextIdentifier.copyWhitespaceFrom(start);
				int lineBreaks = (action == ChainElementAction.DELETE) ? keyword.lineBreaks : 1;
				nextIdentifier.insertLeftSibling(Token.createForAbap(lineBreaks, keyword.spacesLeft, keyword.getText(), TokenType.KEYWORD, keyword.sourceLineNum));
				if (colon != null) {
					nextIdentifier.insertLeftSibling(Token.createForAbap(0, colon.spacesLeft, colon.getText(), TokenType.COLON, colon.sourceLineNum));
				}
			} else if (nextIdentifier != null && nextIdentifier.lineBreaks == 0) {
				nextIdentifier.copyWhitespaceFrom(start);
			}
		}

		if (action == ChainElementAction.COMMENT_OUT_WITH_ASTERISK || action == ChainElementAction.COMMENT_OUT_WITH_QUOT) {
			// create a comment line
			String lineText = Command.sectionToString(firstTokenInLine, lastTokenInLine, true);
			int indent = 0;
			if (action == ChainElementAction.COMMENT_OUT_WITH_QUOT) {
				indent = firstTokenInLine.getStartIndexInLine();
				lineText = ABAP.COMMENT_SIGN_STRING + " " + StringUtil.trimStart(lineText);
			} else {
				lineText = ABAP.LINE_COMMENT_SIGN_STRING + lineText;
			}
			Token newComment = Token.createForAbap(Math.max(firstTokenInLine.lineBreaks, 1), indent, lineText, TokenType.COMMENT, firstTokenInLine.sourceLineNum);

			// insert the comment line, possibly as a new Command before or after the current one
			if ((isFirstInChain || isLastInChain) && !isOnlyOneInChain) {
				Command newCommand = Command.create(newComment, originalCommand);
				try {
					newCommand.finishBuild(getSourceTextStart(), getSourceTextEnd());
				} catch (ParseException e) {
					throw new UnexpectedSyntaxException(this, "parse error in commented-out line");
				}

				// code.addRuleUse(this, newCommand); is not required, because it will have the same effect as "code.addRuleUse(this, command)" below
				if (isFirstInChain)
					insertLeftSibling(newCommand);
				else
					getNext().insertLeftSibling(newCommand);
				this.originalCommand = originalCommand;
			} else {
				firstTokenInLine.insertLeftSibling(newComment, false, true);
			}

			// remove the old declaration code
			// referential integrity cannot be checked at this point, because we may still need to split out leading comment lines
			Term.createForTokenRange(firstTokenInLine, lastTokenInLine).removeFromCommand(true);

		} else if (action == ChainElementAction.DELETE) {
			// remove the declaration code (and possibly the whole Command)
			if (isInChain && !isOnlyOneInChain) {
				Term termToDelete = Term.createForTokenRange(firstTokenInLine, lastTokenInLine); 
				termToDelete.removeFromCommand(true);
			} else {
				// transfer line breaks to next command to keep sections separate
				if (next != null && getFirstTokenLineBreaks() > next.getFirstTokenLineBreaks())
					next.getFirstToken().lineBreaks = getFirstTokenLineBreaks();
				removeFromCode();
			}

		} else {
			throw new IllegalArgumentException("Unknown Action!");
		}

		// if the (remaining) Command starts or ends with one or several comment lines, create separate Commands from it;
		// this may happen if the first/last line of the Command was deleted or commented out, and the adjacent line(s) already were comment lines
		if (splitOutLeadingCommentLines(originalCommand))
			this.originalCommand = originalCommand;
		if (splitOutTrailingCommentLines(originalCommand, true))
			this.originalCommand = originalCommand;

		testReferentialIntegrity(true, true);
		return true;
	}

	public void setErrorStateBeforeCleanup(int errorCount) {
		this.errorCountBeforeCleanup = errorCount;
	}
	
	public boolean containsKeywordOnSiblings(String keyword) {
		if (firstToken.isKeyword(keyword)) {
			return true;
		} else { 
			return (firstToken.getNextSiblingOfTypeAndText(TokenType.KEYWORD, keyword) != null);
		}
	}
	
	public boolean containsKeywordDeep(String keyword) {
		if (firstToken.isKeyword(keyword)) {
			return true;
		} else { 
			return (firstToken.getNextTokenOfTypeAndText(TokenType.KEYWORD, keyword) != null);
		}
	}

	public boolean isInOOContext() {
		Command command = this;
		while (command != null) {
			if (command.firstCodeTokenIsAnyKeyword("CLASS", "INTERFACE", "METHOD")) {
				return true;
			}
			command = command.getParent();
		}
		return false;
	}

	public boolean isDdlAnnotation() { 
		if (!isDdlOrDcl()) 
			return false;
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.textStartsWith(DDL.ANNOTATION_SIGN_STRING); 
	}

	public boolean isDdlAnnotationBeforeListElement() { 
		return isDdlAnnotation() && !isDdlAnnotationAfterListElement(); 
	}

	public boolean isDdlAnnotationAfterListElement() { 
		if (!isDdl()) 
			return false;
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.textStartsWith(DDL.ANNOTATION_AFTER_LIST_ELEMENT_PREFIX); 
	}

	public boolean startsDdlClause() {
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.startsDdlClause(); 
	}

	public boolean startsDdlUnionEtc() {
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.startsDdlUnionEtc(); 
	}

	public String getCondensedDdlAnnotationName(boolean considerCommentedOutAnnotations) { 
		if (isDdlAnnotation()) {
			StringBuilder sbAnnoName = new StringBuilder();
			Token token = getFirstCodeToken();
			while (token != null && !token.textEquals(DDL.COLON_SIGN_STRING)) {
				// do not add spaces or line feeds
				sbAnnoName.append(token.getText()); 
				token = token.getNextCodeToken();
			}
			return sbAnnoName.toString();
			
		} else if (considerCommentedOutAnnotations && isCommentedOutDdlAnnotation()) {
			String annoName = getFirstToken().getText().substring(DDL.LINE_END_COMMENT.length());
			int colonPos = annoName.indexOf(DDL.COLON_SIGN);
			annoName = (colonPos < 0) ? annoName : annoName.substring(0, colonPos);
			return annoName.replace(" ", "");

		} else {
			return null;
		}
	}

	public Token findDdlAnnotationColon() {
		Token token = getFirstCodeToken();
		if (!token.isDdlAnnotation())
			return null;

		// move to the first name bit
		if (token.textEquals(DDL.ANNOTATION_SIGN_STRING)) {
			// "@ AnyAnno" is also valid, therefore move to the "AnyAnno" Token
			token = token.getNextCodeToken();
		}
		
		// find the ":", considering that there might be spaces, line feeds and even comments in between,  
		// e.g. "@Annotation . element /* comment */ . subelement  :"
		while (token != null) {
			if (token.textEquals(DDL.COLON_SIGN_STRING))
				return token;
			// move to the next annotation name element
			token = token.getNextCodeToken();
		}
		return null;
	}

	public boolean isDdlListElement() {
		return isDdlSelectElement() || isDdlParametersElement();
	}

	public boolean isDdlSelectElement() { 
		return isDdlListElement(new String[] { DDL.BRACE_OPEN_STRING, "SELECT", "DISTINCT" }, 
										new String[] { DDL.BRACE_CLOSE_STRING, "FROM" });
	}

	public boolean isDdlParametersElement() {
		return isDdlListElement(new String[] { "PARAMETERS" }, DDL.levelClosersAfterParameterList);
	}

	private boolean isDdlListElement(String[] openingTexts, String[] closingTexts) { 
		if (!isDdl()) 
			return false;
		
		// the select list is inside of { ... }, and the "{" must be on top level
		if (parent == null || parent.parent != null || !parent.getLastCodeToken().textEqualsAny(openingTexts))
			return false;
		
		// exclude annotations and comment lines
		if (isDdlAnnotation() || isCommentLine())
			return false;
		
		// the Command is a field 
		// a) if it is at the end of the parameter or select list
		Command nextNonComment = getNextNonCommentCommand();
		if (nextNonComment != null && nextNonComment.firstToken.textEqualsAny(closingTexts))
			return true;

		// b) if it ends with a comma or semicolon 
		// (note that @< annotation can also end with a semicolon, but isDdlAnnotation() was already excluded above)
		Token lastCode = getLastCodeToken();
		if (lastCode != null && lastCode.textEqualsAny(DDL.COMMA_SIGN_STRING, DDL.SEMICOLON_SIGN_STRING))
			return true;

		// c) if it is followed by an @< annotation 
		if (nextNonComment != null && nextNonComment.isDdlAnnotationAfterListElement())
			return true;

		return false;
	}

	public boolean isCommentedOutDdlAnnotation() { 
		return isDdlOrDcl() && isCommentLine() && firstToken.isCommentedOutDdlAnnotation();
	}
	
	public boolean startsMultiLineDdlComment() {
		return isDdlOrDcl() && isCommentLine() && firstToken.textStartsWith(DDL.ASTERISK_COMMENT_START) && !endsMultiLineDdlComment();
	}

	public Command getLastOfMultiLineDdlComment() {
		if (!isCommentLine() || !firstToken.startsMultiLineDdlComment()) 
			return null;

		Command command = this;
		do {
			if (command.endsMultiLineDdlComment())
				return command;
			command = command.getNextSibling();
		} while (command != null && command.isCommentLine());

		return null;
	}

	public boolean endsMultiLineDdlComment() {
		return isDdlOrDcl() && isCommentLine() && firstToken.getText().indexOf(DDL.ASTERISK_COMMENT_END) >= 0;
	}

	public boolean startsDdlOrDclDefinition() {
		return (getDdlOrDclEntityNameToken() != null);
	}

	public Token getDdlOrDclEntityNameToken() {
		if (!isDdlOrDcl() || parent != null)
			return null;
		Token start = getFirstCodeToken();
		if (start == null || !start.isKeyword())
			return null;
		
		// cp. Language.preview()
		Token identifier = null;
		if (identifier == null) 
			start.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), TokenSearch.makeOptional("ROOT"), "ASTRACT|CUSTOM", "ENTITY", TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), TokenSearch.makeOptional("ROOT"), "VIEW", TokenSearch.makeOptional("ENTITY"), TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), "TABLE", "FUNCTION", TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), "HIERARCHY", TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "DEFINE", "TRANSIENT", "VIEW", TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "DEFINE", "TRANSIENT", "VIEW", TokenSearch.ANY_IDENTIFIER);

		// extension
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "EXTEND", "ABSTRACT|CUSTOM", "VIEW", TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "EXTEND", "VIEW", TokenSearch.makeOptional("ENTITY"), TokenSearch.ANY_IDENTIFIER);
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "ANNOTATE", "VIEW|ENTITY", TokenSearch.ANY_IDENTIFIER);

		// ABAP Dictionary Objects in ADT
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, "DEFINE", "STRUCTURE|TABLE", TokenSearch.ANY_IDENTIFIER);

		// Data Control Language (DCL)
		if (identifier == null) 
			identifier = start.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), "ROLE|ACCESSPOLICY", TokenSearch.ANY_IDENTIFIER);

		return identifier;
	}
	
	void setLanguage(Language newLanguage) {
		this.language = newLanguage;
	}
	
	public final boolean startsDdlJoin() {
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.startsDdlJoin();
	}
		
	public final boolean startsDdlAssociation() {
		Token firstCode = getFirstCodeToken();
		return firstCode != null && firstCode.startsDdlAssociation();
	}

	/** for a DDL select list element, returns the first of its preceding annotations (if any), or the element itself */
	public final Command getStartOfPrecedingDdlAnnotations() {
		Command command = this;
		do {
			Command prevNonComment = command.getPrevNonCommentSibling();
			if (prevNonComment == null || !prevNonComment.isDdlAnnotation())
				return command;
			command = prevNonComment;
		} while (true);
	}
	
	public final boolean startsDdlEntityParameters() {
		return getOpensLevel() && getLastCodeToken() != null && getLastCodeToken().isKeyword("PARAMETERS"); // "!= null" pro forma
	}
		
	public final boolean startsDdlSelectList() {
		return startsDdlSelectListWithBrace() || startsDdlSelectListBeforeFrom(); 
	}
		
	public final boolean startsDdlSelectListWithBrace() {
		return getOpensLevel() && getLastCodeToken() != null && getLastCodeToken().textEquals(DDL.BRACE_OPEN_STRING); // "!= null" pro forma
	}
		
	/** returns true if this Command starts the select list of a DDIC-based CDS View with the syntax 'SELECT [DISTINCT] select_list FROM' */
	public final boolean startsDdlSelectListBeforeFrom() {
		return getOpensLevel() && getLastCodeToken() != null && getLastCodeToken().isAnyKeyword("SELECT", "DISTINCT"); // "!= null" pro forma
	}
		
	public final boolean endsDdlSelectListWithBrace() {
		return getClosesLevel() && getFirstCodeToken() != null && getFirstCodeToken().textEquals(DDL.BRACE_CLOSE_STRING); // "!= null" pro forma
	}
		
	/** Returns true if the Command matches a hard-coded pattern or condition.
	 * This method can be used during development to search for examples in all sample code files. */
	public final boolean matchesPattern() {
      // useful snippets:
      // - is this a certain ABAP command?
      //   return firstCodeTokenIsKeyword("ABAP_KEYWORD");
      // - is a certain Token found anywhere?
      //   Token token = firstToken.getLastTokenDeep(true, TokenSearch.ASTERISK, "SEARCH_TEXT|ALTERNATIVE|...");
      //   return (token != null && ...);
      // - was a certain cleanup rule used?
		//   return changeControl.wasRuleUsed(RuleID....);
		// - is a certain system field modified with at least 2 reads on this system field in subsequent program flow?
		//   return changesSyField(ABAP.SyField.SUBRC) && SyFieldAnalyzer.getSyFieldReadersFor(ABAP.SyField.SUBRC, this).size() >= 2;
		//   - getCommandsRelatedToPatternMatch() can then return SyFieldAnalyzer.getSyFieldReadersFor(ABAP.SyField.SUBRC, this);

		if (!parentCode.isDdlOrDcl() || isDdlAnnotation())
			return false;
		Token token = firstToken;
		while (token != null) {
			if (!token.isComment() && !token.textEqualsAny("{", "}") && StringUtil.containsAny(token.text, new String[] { "{", "}", "[", "]" })) {
				return true;
			}
			token = token.getNext();
		}
      return false;
	}
	
	public final ArrayList<Command> getCommandsRelatedToPatternMatch() {
		return null;
	}

}