package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.IProgress;
import com.sap.adt.abapcleaner.programbase.TaskType;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

/**
 * <p>Creates a sequence of {@link Token}s from the code string that is supplied to the 
 * {@link #Token(String, int, int)} constructor, returning the next {@link Token} at each call of {@link #getNext()} 
 * (or null if the code string is exhausted).</p>   
 * 
 * <p>If the caller detects that the new Token already belongs to a code section in a non-ABAP language, it must call  
 * {@link #changeToNonAbapLanguage(Token, Language, String)}.</p>
 */
public class Tokenizer {
	/** regardless of ABAP.LINE_SEPARATOR, contains all possible chars with which lines could be separated */ 
	private final static String lineSeparatorChars = "\r\n";
	private final static String spaceChars = " \u00A0"; // treat non-breaking space like a space (thus making it vanish already at this stage)
	private final static char[] lineFeedChars = new char[] { '\r', '\n' };
	
	private final static char[] abapTokenEndChars = new char[] { ' ', '\u00A0', '\r', '\n', ABAP.COMMA_SIGN, ABAP.DOT_SIGN, ABAP.QUOT_MARK, ABAP.QUOT_MARK2, ABAP.COMMENT_SIGN, ABAP.COLON_SIGN, '(', ')' };
	private final static String abapTokenEndCharsToIncludeInToken = "("; 
	private final static char[] ddlNumericTokenEndChars = new char[] { ' ', '\u00A0', '\r', '\n', DDL.COLON_SIGN, DDL.QUOT_MARK, DDL.COMMA_SIGN, DDL.SEMICOLON_SIGN, DDL.PARENS_OPEN, DDL.PARENS_CLOSE, DDL.BRACE_OPEN, DDL.BRACE_CLOSE, DDL.BRACKET_OPEN, DDL.BRACKET_CLOSE, '<', '>', '=', '+', '-', '*', '/' };
	private final static char[] ddlIdentifierEndChars   = new char[] { ' ', '\u00A0', '\r', '\n', DDL.COLON_SIGN, DDL.QUOT_MARK, DDL.COMMA_SIGN, DDL.SEMICOLON_SIGN, DDL.PARENS_OPEN, DDL.PARENS_CLOSE, DDL.BRACE_OPEN, DDL.BRACE_CLOSE, DDL.BRACKET_OPEN, DDL.BRACKET_CLOSE, '<', '>', '=', '+', '-', '*' };
	private final static String[] ddlIdentifierEndStrings = new String[] { DDL.LINE_END_COMMENT, DDL.ASTERISK_COMMENT_START };
	private final static String ddlComparisonOpChars = "<>!=";
	private final static char ddlArithmeticOpDiv = '/';
	private final static String ddlArithmeticOpCharsWithoutDiv = "+-*";
	private final static char[] nonAbapTokenEndChars = new char[] { '\r', '\n', ABAP.QUOT_MARK, ABAP.COMMENT_SIGN };
	
	private final static char[] stringTemplateEndChars = new char[] { ABAP.BRACE_OPEN, ABAP.PIPE };
	private final static String[] stringTemplateEscapeSequences = new String[] { "\\|", "\\{", "\\}", "\\\\", "\\r", "\\n", "\\t" };

	private final static TokenType UNKNOWN_TOKEN_TYPE = TokenType.NON_ABAP;
	
	static String removeTabs(String text) { 
		// TODO: clarify whether '\u00a0' should be considered, too 
		// in some places, a TAB is found; however, both SAP GUI and ADT display this as a single space
		return (text == null) ? null : text.replace('\t', ' ');
	}

	private final String text;
	private int lineNum;
	private int readPos;
	private boolean isInMultiLineComment;
	
	private Language previewLanguage;
	private Language curLanguage;
	/** the ABAP keyword "ENDEXEC" or "ENDMETHOD" that will end the current non-ABAP section */
	private String abapKeywordEndingNonAbapSection = null;

	/** interface for progress display (may be null) */
	private final IProgress progress; 
	private int lastReportedPos;
	private int reportSpan;

	boolean isLanguageSupported() { return previewLanguage != Language.NOT_SUPPORTED; }
	int getReadPos() { return readPos; }
	int getLineNum() { return lineNum; }
	Language getCurLanguage() { return curLanguage; }
	
	Tokenizer(String text, int lineNumOffset, IProgress progress) {
		this.text = removeTabs(text);
		this.lineNum = lineNumOffset;
		this.readPos = 0;
		this.isInMultiLineComment = false;
		
		this.previewLanguage = Language.preview(text);
		this.curLanguage = previewLanguage;
		
		this.progress = progress;
		this.lastReportedPos = readPos;
		// don't report progress too often, since tokenization and parsing works at > 1 MB per second
		this.reportSpan = Math.max(text.length() / 100 + 1, 100000);
	}
	
	/** returns the next Token, or null if the code string is exhausted */
	Token getNext() throws UnexpectedSyntaxException {
		if (!isLanguageSupported() || readPos >= text.length())
			return null;

		// identify whitespace
		int lineFeedCount = 0;
		int spaceCount;
		do {
			// count line separators (tolerating both \r\n and \n)
			while (readPos < text.length() && lineSeparatorChars.indexOf(text.charAt(readPos)) >= 0) {
				if (text.charAt(readPos) == '\n') {
					++lineFeedCount;
					++lineNum;
				}
				++readPos;
			}

			// count spaces (but only those after the last line feed)
			spaceCount = 0;
			while (readPos < text.length() && spaceChars.indexOf(text.charAt(readPos)) >= 0) {
				++spaceCount;
				++readPos;
			}

			// in sample code, lines never end with spaces or tabs; if such a line is encountered, ignore such trailing spaces
		} while (readPos < text.length() && lineSeparatorChars.indexOf(text.charAt(readPos)) >= 0);

		// ignore final line feed
		if (readPos == text.length() && lineFeedCount == 1 && spaceCount == 0)
			return null;

		// determine the next Token
		char curChar = (readPos < text.length()) ? text.charAt(readPos) : ' ';
		boolean isAtLineStart = (readPos == 0 || (lineFeedCount > 0 && spaceCount == 0));

		// common tokenization for ABAP and non-ABAP code: comments with " and *   
		String tokenText;
		TokenType overrideTokenType = UNKNOWN_TOKEN_TYPE;
		if (readPos == text.length()) {
			tokenText = "";

		} else if (curLanguage == Language.DDL || curLanguage == Language.DCL) {
			if (isInMultiLineComment || curChar == DDL.COMMENT_SIGN && StringUtil.containsAt(text, readPos, DDL.ASTERISK_COMMENT_START)) {
				// asterisk comment /*...*/: read up to */ or the next line break
				tokenText = readAsteriskComment();
				isInMultiLineComment = !tokenText.endsWith(DDL.ASTERISK_COMMENT_END);
				overrideTokenType = TokenType.COMMENT;
				
			} else if (StringUtil.containsAt(text, readPos, DDL.LINE_END_COMMENT) || StringUtil.containsAt(text, readPos, DDL.LINE_END_MINUS_COMMENT)) {
				// line end comment: the rest of the line
				tokenText = readDdlUntil(lineFeedChars, null);
				
			} else if (curChar == DDL.QUOT_MARK) {
				// read text literal '...', considering escape char ''
				tokenText = readLiteralUntil(curChar, false);

			} else if (curChar == DDL.COLON_SIGN || curChar == DDL.COMMA_SIGN 
					|| curChar == DDL.PARENS_OPEN || curChar == DDL.BRACE_OPEN || curChar == DDL.BRACKET_OPEN 
					|| curChar == DDL.PARENS_CLOSE || curChar == DDL.BRACE_CLOSE || curChar == DDL.BRACKET_CLOSE) {
				// one-char Tokens (which are all part of ddlTokenEndChars as well)
				tokenText = text.substring(readPos, readPos + 1);

			} else if (ddlComparisonOpChars.indexOf(curChar) >= 0) {
				// comparison operator or => operator
				tokenText = StringUtil.readTillEndOfAllowedChars(text, readPos, ddlComparisonOpChars);

			} else if (Character.isDigit(curChar)) { // || curChar == '-' && readPos + 1 < text.length() && Character.isDigit(text.charAt(readPos + 1))) {
				// number, which may start with - and may include . (however, -.5 is not possible)
				// unlike identifiers, a number stops at / (and thus automatically at // and /*, therefore no need to supply ddlIdentifierEndStrings)
				tokenText = readDdlUntil(ddlNumericTokenEndChars, null);
				
			} else if (curChar == ddlArithmeticOpDiv && !DDL.isCharAllowedForIdentifier(text, readPos + 1, true)) {
				// '/' is only a division operator if it is NOT directly followed by a char that could start an identifier
				tokenText = Character.toString(curChar);

			} else if (ddlArithmeticOpCharsWithoutDiv.indexOf(curChar) >= 0) {
				// arithmetic operator (just one char)
				tokenText = Character.toString(curChar);

			} else {
				// normal word, which includes chars from 0-9, a-z, A-Z, / for namespaces, ._@# and < after initial @
				tokenText = readDdlUntil(ddlIdentifierEndChars, ddlIdentifierEndStrings);
			}
			
		} else if (curChar == ABAP.LINE_COMMENT_SIGN && isAtLineStart) {
			// line comment: the whole line
			tokenText = readUntil(lineFeedChars, null);

		} else if (curChar == ABAP.COMMENT_SIGN) {
			// normal comment: the rest of the line
			tokenText = readUntil(lineFeedChars, null);

		} else if (curLanguage == Language.ABAP) {
			// ABAP-specific tokenization
			if (curChar == ABAP.QUOT_MARK) {
				// text field literal with ' (data type c): read the whole literal, considering escape char ''
				// and possible text symbol IDs at the end of the literal, e.g. 'literal text'(001)
				tokenText = readLiteralUntil(curChar, true);

			} else if (curChar == ABAP.QUOT_MARK2) {
				// text string literal with ` (data type string): read the whole literal, considering escape char ``
				tokenText = readLiteralUntil(curChar, false);

			} else if (curChar == ABAP.PIPE || text.charAt(readPos) == ABAP.BRACE_CLOSE) {
				// string template with |: the whole template until | or {
				tokenText = readStringTemplate();

			} else if (curChar == '(' || curChar == ABAP.DOT_SIGN || curChar == ABAP.COMMA_SIGN || curChar == ABAP.COLON_SIGN) {
				// one-char Tokens for . , : or ( as a delimiter
				tokenText = text.substring(readPos, readPos + 1);

			} else {
				// normal word, including pragma ##...
				tokenText = readUntil(abapTokenEndChars, abapTokenEndCharsToIncludeInToken);
			}

		} else {
			// non-ABAP tokenization: the Token constructor will regard everything except comments as TokenType.NON_ABAP, 
			// and these Commands will always remain unchanged (see Command.isInCleanupRange()).
			tokenText = readNonAbap();
			if (AbapCult.stringEquals(tokenText, abapKeywordEndingNonAbapSection, true)) {
				curLanguage = Language.ABAP;
			}
		}
		
		if (tokenText != null) {
			readPos += tokenText.length();
			lineNum += StringUtil.instrCount(tokenText, '\n');
		}
		
		if (progress != null && (lastReportedPos == 0 || readPos >= lastReportedPos + reportSpan)) {
			progress.report(TaskType.PARSER, readPos / (double) text.length());
			lastReportedPos = readPos;
		}

		Token nextToken = Token.create(lineFeedCount, spaceCount, tokenText, lineNum, curLanguage);
		if (overrideTokenType != UNKNOWN_TOKEN_TYPE) {
			// for a single line of a multi-line comment, the TokenType cannot be inferred 
			nextToken.type = overrideTokenType;
			nextToken.setOpensLevel(false);
			nextToken.setClosesLevel(false);
		}
		return nextToken;
	}

	/** changes to a non-ABAP language and instantiates the supplied Token again with that language */
	Token changeToNonAbapLanguage(Token token, Language nonAbapLanguage, String abapKeywordEndingNonAbapSection) {
		this.curLanguage = nonAbapLanguage;
		this.abapKeywordEndingNonAbapSection = abapKeywordEndingNonAbapSection.toUpperCase();
		
		return Token.create(token.lineBreaks, token.spacesLeft, token.text, token.sourceLineNum, curLanguage);
	}
	
	void setLanguage(Language newLanguage) {
		this.curLanguage = newLanguage;
	}
	
	private String readLiteralUntil(char delimiterChar, boolean includeTextSymbolID) throws UnexpectedSyntaxException {
		int searchPos = readPos + 1;
		int literalEnd;
		do {
			searchPos = text.indexOf(delimiterChar, searchPos); // skip the char at readPos
			if (searchPos < 0) { // pro forma
				literalEnd = text.length();
				break;
			}
			if (searchPos + 1 == text.length() || text.charAt(searchPos + 1) != delimiterChar) {
				literalEnd = searchPos + 1;
				break;
			}
			// delimiterChar is used as an escape char, e.g. 'text with ''quotation'' marks' and `string with ``backquote`` marks``
			searchPos += 2;
		} while (true);

		
	   // consider the case 'literal text'(001): text field literals that are delimited with '...' (not those delimited with `...`) 
		// may be linked to a text symbol ID, where the 'literal text' is overridden if the text symbol TEXT-001 is defined; 
		// for our purpose, we simply consider this addition to be part of the literal, because the whole construct will never be changed
		if (includeTextSymbolID && literalEnd < text.length() && text.charAt(literalEnd) == ABAP.TEXT_SYMBOL_ID_OPEN) {
			int closePos = text.indexOf(ABAP.TEXT_SYMBOL_ID_CLOSE, literalEnd);
			if (closePos >= 0) {
				literalEnd = closePos + 1;
			}
		}

		return text.substring(readPos, literalEnd);
	}

	private String readStringTemplate() throws UnexpectedSyntaxException {
		int tokenEnd = StringUtil.indexOfAny(text, stringTemplateEndChars, readPos + 1, stringTemplateEscapeSequences); // skip the char at readPos
		tokenEnd = (tokenEnd < 0) ? text.length() : tokenEnd + 1;

		return text.substring(readPos, tokenEnd);
	}

	private String readUntil(char[] delimiterChars, String includeDelimiters) throws UnexpectedSyntaxException {
		// read until the first delimiter is found
		int tokenEnd = StringUtil.indexOfAny(text, delimiterChars, readPos + 1); // skip the char at readPos

		// continue reading in case of "identifier(##)" with no spaces as in "DATA lv_chars(20) TYPE c."
		if (tokenEnd >= 0 && text.charAt(tokenEnd) == '(' && tokenEnd + 1 < text.length() && Character.isDigit(text.charAt(tokenEnd + 1))) {
			tokenEnd = text.indexOf(')', tokenEnd);
			if (tokenEnd < 0)
				throw new UnexpectedSyntaxException("closing parenthesis not found");
			++tokenEnd;
		} else {
			if (tokenEnd < 0)
				tokenEnd = text.length();
			else if (includeDelimiters != null && includeDelimiters.indexOf(text.charAt(tokenEnd)) >= 0)
				++tokenEnd;
		}

		return text.substring(readPos, tokenEnd);
	}
	
	private String readDdlUntil(char[] delimiterChars, String[] delimiterStrings) throws UnexpectedSyntaxException {
		// read until the first delimiter is found
		// in the special case of an initial @, < is allowed afterwards, because annotations for CDS entities 
		// (except for CDS view entities) may start with @< for the annotation after a list element in a comma-separated or semicolon-separated list.
		int start = readPos + 1;
		if (text.charAt(readPos) == DDL.ANNOTATION_SIGN && start < text.length() && text.charAt(start) == '<') {
			++start;
		}
		int tokenEnd = StringUtil.indexOfAny(text, delimiterChars, delimiterStrings, start); 

		if (tokenEnd < 0)
			tokenEnd = text.length();

		// in the special case of association cardinality, do not return "0..", "1.." or "n.." but only "0", "1", or "n"
		if (tokenEnd - readPos >= 3) {
			char firstChar = text.charAt(readPos);
			if ((firstChar == '0' || firstChar == '1' || firstChar == 'n') && text.charAt(readPos + 1) == '.' && text.charAt(readPos + 2) == '.') {
				tokenEnd = readPos + 1;
			}
		}
		
		return text.substring(readPos, tokenEnd); 
	}
	
	private String readAsteriskComment() throws UnexpectedSyntaxException {
		// determine line end position
		int lineEnd = StringUtil.indexOfAny(text, lineFeedChars, readPos);
		if (lineEnd < 0)
			lineEnd = text.length();
		
		// determine comment end position
		int searchStart = readPos + (StringUtil.containsAt(text, readPos, DDL.ASTERISK_COMMENT_START) ? DDL.ASTERISK_COMMENT_START.length() : 0);  
		int commentEnd = text.indexOf(DDL.ASTERISK_COMMENT_END, searchStart);
		if (commentEnd < 0)
			throw new UnexpectedSyntaxException("comment end '" + DDL.ASTERISK_COMMENT_END + "' not found");
		commentEnd += DDL.ASTERISK_COMMENT_END.length();

		// return the next bit
		return text.substring(readPos, Math.min(lineEnd, commentEnd));
	}
	
	private String readNonAbap() {
		int tokenEnd = StringUtil.indexOfAny(text, nonAbapTokenEndChars, readPos + 1); // skip the char at readPos
		if (tokenEnd < 0)
			tokenEnd = text.length();
		String nonAbapText = text.substring(readPos, tokenEnd);

		return splitEndOfNonAbapSection(nonAbapText);
	}

	private String splitEndOfNonAbapSection(String text) {
		// determine whether the text ends the current non-ABAP section: 
		// - if the abapKeywordEndingNonAbapSection ("ENDEXEC" or "ENDMETHOD") is at text start, then only return this keyword
		// - if it is later in the text, only return the non-ABAP text before the keyword (leaving the keyword to the next Token)
		// - otherwise, return the whole text, which is then completely non-ABAP
		
		String textUpper = text.toUpperCase();
		int searchPos = 0;
		
		while (searchPos < textUpper.length()) {
			searchPos = textUpper.indexOf(abapKeywordEndingNonAbapSection, searchPos);
			if (searchPos < 0) 
				break;

			// was "ENDEXEC" OR "ENDMETHOD" found as a whole word (not as part of a different word)?
			int posAfterKeyword = searchPos + abapKeywordEndingNonAbapSection.length();
			if ((searchPos == 0 || textUpper.charAt(searchPos - 1) == ' ') && (posAfterKeyword >= textUpper.length() || " .\"".indexOf(textUpper.charAt(posAfterKeyword)) >= 0)) {
				if (searchPos > 0) {
					// return only the non-ABAP code up to (but excluding) the ABAP keyword "ENDEXEC" or "ENDMETHOD" 
					return StringUtil.trimEnd(text.substring(0, searchPos));
				
				} else {
					// return the ABAP keyword "ENDEXEC" or "ENDMETHOD" only (without any space or "." after it)
					return text.substring(0, abapKeywordEndingNonAbapSection.length());
				}			
			}

			searchPos = posAfterKeyword;
		}

		// the whole text is non-ABAP code
		return text;
	}
}
