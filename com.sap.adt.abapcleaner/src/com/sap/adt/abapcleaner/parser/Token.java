package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleID;

import java.security.InvalidParameterException;
import java.util.*;

/**
 * <p>From ABAP cleaner perspective, a Token is the smallest relevant unit of ABAP code: 
 * A {@link Command} consists of either a single Token or a sequence of Tokens.
 * {@link #toString()} exactly restores the ABAP source code from which a Token was parsed.</p>
 * 
 * <p>Each Token stores the number of {@link #lineBreaks} and {@link #spacesLeft} that separates it from the previous Token, 
 * allowing ABAP cleaner to easily modify whitespace. 
 * In cleaning, a Token may be moved around, but its {@link #text} is usually not changed. </p>
 * 
 * <p>The attributes {@link #prev} and {@link #next} provide the sequence of Tokens in order of their appearance within the code; 
 * at the same time, a hierarchical view of the Tokens inside a {@link Command} is provided via the Token attributes 
 * {@link #parent}, {@link #firstChild}, {@link #lastChild}, {@link #prevSibling} and {@link #nextSibling}: 
 * Any Tokens inside parentheses ( ... ) and brackets [ ... ] - or inside braces { ... } in string templates - are considered 'child' Tokens; 
 * in such cases, the Token that ends with '(', '[' or '{' is the {@link #parent}, while the '}', ')' or ']' Token is the parent's {@link #nextSibling}.  
 * However, parentheses with no spaces and only an integer literal inside them - such as 'char(8)' - are considered a single Token 
 * (with no parent/child relationship).</p>    
 * 
 * <p>More detailed description of what a Token is:</p>
 * <ul>
 *   <li> a Token is everything separated with whitespace (or until . , ' ` "); its text includes final ( [, but not . or , or :</li>
 *   <li> expressions linked with - ~ -&gt; =&gt; are one Token</li>
 *   <li> string literals etc. such as 'text' or |text{ or }text| or `text` are one Token each</li>
 *   <li> a comment line (or the " comment at the end of a code line) is one single Token, including the comment sign</li>
 *   <li> ) and ] are one Token respectively</li>
 *   <li> = ?= += -= *= /= is one Token each</li>
 *   <li> a pragma such as ##NO_TEXT is one Token</li>
 *   <li> the final . or , as well as the chain colon : is a separate Token, even if it is attached to the previous Token</li>
 * </ul>
 */
public class Token {
	private static String[] levelOpeners = new String[] { "(", "[" };
	private static String[] levelClosers = new String[] { ")", "]" };

	private static String[] ddlLevelOpeners = new String[] { "(", "[", "{", "case" };
	private static String[] ddlLevelClosers = new String[] { ")", "]", "}", "end" };

	// place and content
	public int lineBreaks;
	public int spacesLeft;
	String text;
	public final int sourceLineNum; // 1-based

	// references to the Command and to other Tokens
	private Command parentCommand;
	private Token parent;
	private Token prev;
	private Token next;
	private Token prevSibling;
	private Token nextSibling;
	private Token firstChild;
	private Token lastChild;
	private MemoryAccessType memoryAccessType = MemoryAccessType.UNKNOWN; // use lazy evaluation
	
	public final Command getParentCommand() { return parentCommand; }
	final void setParentCommand(Command value) { parentCommand = value; }

	public final Token getParent() { return parent; }
	final void setParent(Token value) { parent = value; }

	public final Token getPrev() { return prev; }
	final void setPrev(Token value) { prev = value; }

	public final Token getNext() { return next; }
	final void setNext(Token value) { next = value; }

	public final Token getPrevSibling() { return prevSibling; }
	final void setPrevSibling(Token value) { prevSibling = value; }

	public final Token getNextSibling() { return nextSibling; }
	final void setNextSibling(Token value) { nextSibling = value; }

	public final Token getFirstChild() { return firstChild; }
	final void setFirstChild(Token value) { firstChild = value; }

	public final Token getLastChild() { return lastChild; }
	final void setLastChild(Token value) { lastChild = value; }

	public final Token getFirstCodeChild() { return (firstChild == null || firstChild.isCode()) ? firstChild : firstChild.getNextCodeSibling(); }
	public final Token getLastCodeChild() { return (lastChild == null || lastChild.isCode()) ? lastChild : lastChild.getPrevCodeSibling(); }

	public final String getText() { return text; }
	
	// type and effect of this Token
	public TokenType type = TokenType.values()[0];
	private boolean closesLevel;
	private boolean opensLevel;
	boolean collocationContinues;

	public final boolean closesLevel() { return closesLevel; }
	final void setClosesLevel(boolean value) { closesLevel = value; }

	public final boolean getOpensLevel() { return opensLevel; }
	final void setOpensLevel(boolean value) { opensLevel = value; }

	public final boolean isComment() { return (type == TokenType.COMMENT); }

	public final boolean isQuotMarkComment() { return isComment() && AbapCult.stringStartsWith(text, ABAP.COMMENT_SIGN_STRING); }

	public final boolean isCommentLine() {
		// in DDL, a line may contain multiple comments, e.g. /* comment */ // comment
		// however, unlike Command.isCommentLine(), Token.isCommentLine() only returns true if this is the only Token on this line
		return isComment() && isFirstTokenInLine() && (next == null || next.lineBreaks > 0);
	}

	public final boolean isQuotMarkCommentLine() { return isCommentLine() && isQuotMarkComment(); }

	public final boolean isDdlLineEndComment() { return isComment() && textStartsWithAny(DDL.LINE_END_COMMENT, DDL.LINE_END_MINUS_COMMENT); }

	public final boolean isAbapDocComment() {
		// ABAP Doc comments are expected to be comment lines, however, it is syntactically possible to put them as 
		// line-end comments after METHODS: (only getting a warning, but no error) 
		return AbapCult.stringStartsWith(text, ABAP.ABAP_DOC_SIGN); 
	}

	public final boolean isAbapDocCommentLine() { return isCommentLine() && AbapCult.stringStartsWith(text, ABAP.ABAP_DOC_SIGN); }

	public final boolean isAsteriskCommentLine() { return isCommentLine() && AbapCult.stringStartsWith(text, ABAP.LINE_COMMENT_SIGN_STRING); }

	public final boolean isCommentAfterCode() { return isComment() && (prev != null) && (lineBreaks == 0); }

	public final boolean isCommentAfterPeriod() { return isComment() && (prev != null) && (lineBreaks == 0) && (prev.isPeriod()); }

	public final boolean isPseudoComment() {
		// returns true if the comment starts with "#EC (this is NOT case-sensitive, i.e. Extended Check also accepts "#ec and "#Ec;
		// however, the following word must be upper case: Extended Check accepts "#ec NEEDED but it does NOT accept "#EC needed ) 
		return isComment() && AbapCult.stringStartsWith(text, ABAP.PSEUDO_COMMENT_EC_PREFIX); 
	}

	public final boolean isPseudoCommentAfterCode() { return isPseudoComment() && (prev != null) && (lineBreaks == 0); }

	public final boolean isLiteral() { return (type == TokenType.LITERAL); }

	public final boolean isIntegerLiteral() { return isLiteral() && !isStringLiteral(); }

	public final boolean isFloatLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(0) == ABAP.QUOT_MARK && text.charAt(text.length() - 1) == ABAP.QUOT_MARK
				&& ABAP.isNumeric(text.substring(1, text.length() - 1));
	}

	public final boolean isIdentifier() { return (type == TokenType.IDENTIFIER); }

	public final boolean isOtherOp() { return (type == TokenType.OTHER_OP); }

	public final boolean isClosingParenthesisOrBracket() { return (type == TokenType.OTHER_OP && (text.equals(")") || text.equals("]"))); }

	public final boolean isKeyword() { return (type == TokenType.KEYWORD); }

	public final boolean isKeyword(String text) { return isKeyword() && textEquals(text); }

	public final boolean isAnyKeyword(String... compareTexts) { return isKeyword() && textEqualsAny(compareTexts); }
	
	public final boolean isTextualComparisonOp() { return (type == TokenType.COMPARISON_OP) && !StringUtil.isNullOrEmpty(text) && Character.isLetter(text.charAt(0)); }

	public final boolean isPragma() { return (type == TokenType.PRAGMA); }

	public final boolean isPragmaOrComment() { return isPragma() || isComment(); }

	/** returns true if the Token is NOT a comment or pragma */
	public final boolean isCode() { return (type != TokenType.COMMENT) && (type != TokenType.PRAGMA); }

	public final boolean isPeriod() { return (type == TokenType.PERIOD); }

	public final boolean isComma() { return (type == TokenType.COMMA); }

	public final boolean isCommaOrPeriod() { return isComma() || isPeriod(); }

	public final boolean isChainColon() { return (type == TokenType.COLON); }

	public final boolean isTextSymbol() { return (type == TokenType.KEYWORD && ABAP.mayBeTextSymbol(text)); }

	final boolean getMayBeIdentifier() { return (type == TokenType.IDENTIFIER) || isKeyword() || isTextualComparisonOp(); }

	public final boolean isTextFieldLiteral() {
		// also ensure that the literal ends with ' to exclude literals with text element IDs: 'text'(idf)
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.length() >=2 && text.charAt(0) == ABAP.QUOT_MARK && text.charAt(text.length() - 1) == ABAP.QUOT_MARK;
	}

	public final boolean isTextStringLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(0) == ABAP.QUOT_MARK2;
	}

	public final boolean isCharacterLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && (text.charAt(0) == ABAP.QUOT_MARK || text.charAt(0) == ABAP.QUOT_MARK2);
	}

	public final boolean isStringLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text)
				&& (text.charAt(0) == ABAP.QUOT_MARK || text.charAt(0) == ABAP.QUOT_MARK2 || text.charAt(0) == ABAP.PIPE || text.charAt(0) == ABAP.BRACE_CLOSE);
	}

	public final boolean isStringTemplate() {
		return (isLiteral() && !StringUtil.isNullOrEmpty(text) 
				&& (text.charAt(0) == ABAP.PIPE || text.charAt(0) == ABAP.BRACE_CLOSE)
				&& (text.charAt(text.length() - 1) == ABAP.PIPE || text.charAt(text.length() - 1) == ABAP.BRACE_OPEN)); 
	}

	public final boolean isDdlStringLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(0) == DDL.QUOT_MARK && text.charAt(text.length() - 1) == DDL.QUOT_MARK;
	}

	final boolean startsStringTemplate() { return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(0) == ABAP.PIPE; }
	
	final boolean endsStringTemplate() { return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(text.length() - 1) == ABAP.PIPE; }
	
	final boolean startsWithLetter() { return !StringUtil.isNullOrEmpty(text) && Character.isLetter(text.charAt(0)); }

	public final boolean startsEmbeddedExpression() { return isStringLiteral() && text.charAt(text.length() - 1) == ABAP.BRACE_OPEN; }

	public final boolean endsEmbeddedExpression() { return isStringLiteral() && text.charAt(0) == ABAP.BRACE_CLOSE; }

	public final boolean isAssignmentOperator() { return (type == TokenType.ASSIGNMENT_OP); }

	public final boolean isComparisonOperator() { return (type == TokenType.COMPARISON_OP); }

	public final boolean isComparisonOperator(String text) { return isComparisonOperator() && textEquals(text); }

	public final boolean isAnyComparisonOperator(String... compareTexts) { return isComparisonOperator() && textEqualsAny(compareTexts); }

	public final int getTextLength() { return text.length(); }

	public final boolean hasChildren() { return (firstChild != null); }

	public final boolean hasCodeChildren() { return (firstChild != null && (firstChild.isCode() || firstChild.getNextCodeSibling() != null)); }

	public final boolean isPrecededByWhitespace() { return (lineBreaks > 0 || spacesLeft > 0); }

	public final boolean isFirstTokenInCommand() { return (parentCommand != null && parentCommand.firstToken == this); }

	public final boolean isFirstTokenInCode() { return isFirstTokenInCommand() && parentCommand.isFirstCommandInCode(); }

	public final boolean isLastTokenInCommand() { return (parentCommand != null && parentCommand.lastToken == this); }

	public final boolean isFirstTokenInLine() { return lineBreaks > 0 || (sourceLineNum == 1 && isFirstTokenInCommand()); }

	public final boolean isLastTokenInLineExceptComment() { return next == null || (next.lineBreaks > 0 || next.isCommentAfterCode()); }

	public final boolean isOnlyTokenInLine() { return isFirstTokenInLine() && (next == null || next.lineBreaks > 0); }

	public final boolean isOnlyTokenInCommand() { return isFirstTokenInCommand() && isLastTokenInCommand(); }

	public final boolean opensInlineDeclaration() { return isAnyKeyword("DATA(", "FINAL(", "FIELD-SYMBOL(", "@DATA(", "@FINAL(") && next != null && next.isAttached(); } // by contrast, 'REF data( lv_company_code )' is NOT an inline declaration, because the next Token is NOT attached!

	public final boolean opensTableExpression() { return opensLevel && isIdentifier() && textEndsWith("["); } 

	public final boolean opensInlineDeclarationForFieldSymbol() { return isAnyKeyword("FIELD-SYMBOL(") && next != null && next.isAttached(); }; 
	
	public final boolean isAttached() { return lineBreaks == 0 && spacesLeft == 0; }
	
	public final String getTypeAndTextForErrorMessage() { return type.toString() + " '" + text + "'"; }

	public final String getSourceName() { return (parentCommand == null) ? null : parentCommand.getSourceName(); }
	
	public final boolean isDdlAnnotation() { return text.length() > 0 && text.charAt(0) == DDL.ANNOTATION_SIGN; }
		
	/**
	 * Returns true if the Token text equals the supplied comparison text.  
	 * To check the Token type at the same time, use {@link #isKeyword(String)} or {@link #isComparisonOperator(String)} 
	 * (see list of tokens classified as comparison operators in {@link ABAP#isComparisonOperator(String, boolean)}) 
	 */
	public final boolean textEquals(String compareText) {
		return AbapCult.stringEquals(text, compareText, true);
	}

	public final boolean textStartsWith(String prefix) {
		return AbapCult.stringStartsWith(text, prefix, true);
	}

	public final boolean textEndsWith(String suffix) {
		return AbapCult.stringEndsWith(text, suffix, true);
	}

	// ----------------------------------------------------------------------

	/**
	 * <p>Creates a new ABAP Token with the supplied {@link TokenType}. This overload should be preferred,  
	 * especially in cases where the {@link TokenType} is known, but ambiguous:</p>
	 * <ul><li>{@link TokenType#IDENTIFIER} if the identifier might look like an ABAP keyword;</li> 
	 * <li>{@link TokenType#ASSIGNMENT_OP} and {@link TokenType#COMPARISON_OP} for the equals sign '='.</li></ul>
	 * <p>Rules that use this method to create an ABAP keyword or an identifier should override
	 * {@link Rule#getDependentRules()} and return {@link RuleID#UPPER_AND_LOWER_CASE}.</p>
	 */
	public static Token createForAbap(int lineBreaks, int spacesLeft, String text, TokenType type, int sourceLineNum) {
		Token token = new Token(lineBreaks, spacesLeft, text, sourceLineNum, Language.ABAP);

		// in typical ambiguous cases, prefer the supplied TokenType
		if (token.type != type) {
			if ((token.type == TokenType.KEYWORD && type == TokenType.IDENTIFIER) 
			 || (token.type == TokenType.IDENTIFIER && type == TokenType.KEYWORD)
			 || (token.type == TokenType.OTHER_OP && type == TokenType.IDENTIFIER && ABAP.isPlaceholderInMacroDefinition(token.text))) {
				token.type = type;
			} else if ((token.type == TokenType.ASSIGNMENT_OP && type == TokenType.COMPARISON_OP) 
					  || (token.type == TokenType.COMPARISON_OP && type == TokenType.ASSIGNMENT_OP)) {
				
				token.type = type;
			} else {
				// this case should NOT be ambiguous; possibly, a wrong TokenType was supplied by the rule
				throw new IllegalArgumentException("Supplied TokenType " + type.toString() + " unexpected for Token text '" + text + "'; using inferred TokenType " + token.type + "!");
			}
		}
		return token;
	}
	
	/**
	 * Create a new ABAP Token. With this overload, the {@link TokenType} is automatically inferred from the text. 
	 * Rules that use this method to create an ABAP keyword or an identifier 
	 * should override {@link Rule#getDependentRules()} and return {@link RuleID#UPPER_AND_LOWER_CASE}. 
	 */
	public static Token createForAbap(int lineBreaks, int spacesLeft, String text, int sourceLineNum) {
		return new Token(lineBreaks, spacesLeft, text, sourceLineNum, Language.ABAP);
	}
	
	/**
	 * Create a new Token for the specified programming language  
	 *  
	 * @param lineBreaks
	 * @param spacesLeft
	 * @param text
	 * @param sourceLineNum
	 */
	public static Token create(int lineBreaks, int spacesLeft, String text, int sourceLineNum, Language language) {
		return new Token(lineBreaks, spacesLeft, text, sourceLineNum, language);
	}
	
	private Token(int lineBreaks, int spacesLeft, String text, int sourceLineNum, Language language) {
		if (text == null)
			throw new NullPointerException("text");

		this.lineBreaks = lineBreaks;
		this.spacesLeft = spacesLeft;
		this.text = text;
		this.sourceLineNum = sourceLineNum;

		// preliminarily determine the type; Command.finishBuild() may correct this choice depending on the context of this Token
		// (at this point, we only need to identify comments and tell whether the Token opens or closes a level)
		boolean isAtLineStart = ((this.sourceLineNum == 1 || this.lineBreaks > 0) && this.spacesLeft == 0);

		if (language == Language.DDL || language == Language.DCL) {
			type = inferTypeFromDdlToken(text, language);
			if (type == TokenType.KEYWORD || type == TokenType.OTHER_OP) {
				opensLevel = textEndsWithAny(ddlLevelOpeners);
				closesLevel = textStartsWithAny(ddlLevelClosers);
			}			

		} else {
			// even in non-ABAP sections, comments are started with * at line start
			if (AbapCult.stringStartsWith(text, ABAP.LINE_COMMENT_SIGN_STRING) && isAtLineStart) {
				type = TokenType.COMMENT;
	
			} else {			
				if (language == Language.ABAP)  {
					type = inferTypeFromAbapToken(text);
				} else if (language == Language.SQLSCRIPT) { 
					type = inferTypeFromSqlScriptToken(text);
				} else { 
					type = inferTypeFromOtherToken(text);
				}
			}
			
			if (type == TokenType.KEYWORD || type == TokenType.IDENTIFIER || type == TokenType.OTHER_OP) {
				closesLevel = textStartsWithAny(levelClosers);
				opensLevel = textEndsWithAny(levelOpeners);
			} else if (type == TokenType.LITERAL) {
				closesLevel = textStartsWith("}");
				opensLevel = textEndsWith("{");
			}
		}
	}

	private static TokenType inferTypeFromAbapToken(String text) {
		if (AbapCult.stringStartsWith(text, ABAP.COMMENT_SIGN_STRING)) { 
			return TokenType.COMMENT;
		
		} else if (AbapCult.stringStartsWith(text, ABAP.PRAGMA_SIGN)) {
			return TokenType.PRAGMA;

		} else if (text.length() > 0 && (text.charAt(0) == ABAP.QUOT_MARK || text.charAt(0) == ABAP.QUOT_MARK2 || text.charAt(0) == ABAP.PIPE || text.charAt(0) == ABAP.BRACE_CLOSE)) {
			// text literal, incl. floating point literals like '3.14'
			return TokenType.LITERAL; 
		} else if (ABAP.isNumeric(text, false, false)) {
			// integer literal
			return TokenType.LITERAL; 
		} else if (ABAP.isPositionAndLength(text)) { 
			// e.g. "1(75)" as in "SELECTION-SCREEN PUSHBUTTON 1(75) ..."
			return TokenType.LITERAL; 
		
		} else if (ABAP.isAssignmentOperator(text)) { 
			// "=" will later be differentiated between assignment and comparison operator in Command.finishBuild() / .distinguishOperators()
			return TokenType.ASSIGNMENT_OP;
		
		} else if (ABAP.isComparisonOperator(text, true)) {
			return TokenType.COMPARISON_OP;
		
		} else if (ABAP.isAbapUpperCaseKeyword(text)) {
			// some identifiers may be preliminarily categorized as keywords here, see Command.finishBuild()
			return TokenType.KEYWORD; 
		
		} else if (ABAP.COLON_SIGN_STRING.equals(text)) {
			return TokenType.COLON;
		
		} else if (ABAP.COMMA_SIGN_STRING.equals(text)) {
			return TokenType.COMMA;
		
		} else if (ABAP.DOT_SIGN_STRING.equals(text)) {
			return TokenType.PERIOD;
		
		} else if (ABAP.mayBeTextSymbol(text)) {
			// text symbols such as 'TEXT-001' and 'TEXT-a01' are stored as one Token of type KEYWORD and get special handling where needed
			return TokenType.KEYWORD;  
			
		} else {
			// Assume .IDENTIFIER if at least one letter a-z A-Z or an underscore _ is found, otherwise assume .OTHER_OP. 
			// This first assumption may be corrected in TokenTypeRefinerRnd.refine(Command, Token, Token), esp. if the  
			// RND Parser categorizes the Token as .CAT_IDENTIFIER: in non-object-oriented contexts, even the single  
			// characters $ % ?, as well as sequences like &%$?*#> and <%$?*# could be valid variable names,
			// cp. ABAP.isCharAllowedForVariableNames() and ABAP.isCharAllowedForTypeNames()! However, all the above 
			// categorizations as .COMMENT, .PRAGMA, .LITERAL etc. (except .KEYWORD) are always correct, even in 
			// non-object-oriented contexts.
			char[] textChars = text.toCharArray();
			for (char c : textChars) {
				if (Character.isLetter(c) || c == '_') {
					return TokenType.IDENTIFIER;
				}
			}
			return TokenType.OTHER_OP; // e.g. ")", "#("
		}
	}

	private static TokenType inferTypeFromDdlToken(String text, Language language) {
		if (AbapCult.stringStartsWith(text, DDL.LINE_END_COMMENT) 
				|| AbapCult.stringStartsWith(text, DDL.LINE_END_MINUS_COMMENT)
				|| AbapCult.stringStartsWith(text, DDL.ASTERISK_COMMENT_START)) {
			return TokenType.COMMENT;
		
		} else if (AbapCult.stringStartsWith(text, DDL.QUOT_MARK_STRING) || DDL.isNumeric(text, true)) {
			return TokenType.LITERAL;

		} else if (DDL.isComparisonOperator(text)) {
			return TokenType.COMPARISON_OP;

		} else if (language == Language.DDL && DDL.isDdlKeyword(text)) {
			return TokenType.KEYWORD;

		} else if (language == Language.DCL && DDL.isDclKeyword(text)) {
			return TokenType.KEYWORD;

		} else if (ABAP.COLON_SIGN_STRING.equals(text)) {
			return TokenType.COLON;
		
		} else if (ABAP.COMMA_SIGN_STRING.equals(text)) {
			return TokenType.COMMA;
		
		} else {
			// Assume .IDENTIFIER if at least one letter a-z A-Z or an underscore _ is found, otherwise assume .OTHER_OP. 
			char[] textChars = text.toCharArray();
			for (char c : textChars) {
				if (Character.isLetter(c) || c == '_' || c == '$' || c == '#' || c == '@') {
					return TokenType.IDENTIFIER;
				}
			}

			return TokenType.OTHER_OP; // e.g. ")", "#("
		}
	}
	private static TokenType inferTypeFromSqlScriptToken(String text) {
		// in non-ABAP sections (e.g. EXEC SQL ... ENDEXEC), everything except comments gets TokenType.NON_ABAP (including literals)  
		return TokenType.NON_ABAP;
	}

	private static TokenType inferTypeFromOtherToken(String text) {
		// in non-ABAP sections (e.g. EXEC SQL ... ENDEXEC), everything except comments gets TokenType.NON_ABAP (including literals)  
		return TokenType.NON_ABAP;
	}

	public final void addNext(Token newToken) throws UnexpectedSyntaxException {
		if (newToken == null || parentCommand == null || next != null || newToken.prev != null)
			throw new NullPointerException("newToken");

		// the following check is deactivated, because a colon inside of parentheses or brackets can be accepted, 
		// as long as parentheses are NOT closed twice (which is checked below with 'if (parent == null)') 
		// if (parent != null && newToken.isChainColon()) 
		// 	throw new UnexpectedSyntaxException(this, "Unsupported syntax: Chain colon ':' inside parentheses ( ... ) or brackets [ ... ] cannot be processed by " + Program.PRODUCT_NAME + ". Please refactor this command first.");
		
		newToken.parentCommand = parentCommand;
		parentCommand.addToken(newToken);

		newToken.prev = this;
		next = newToken;

		if (opensLevel && !newToken.closesLevel) {
			// correct the token type of '*' in 'lv_any+4(*) = ...', which was initially identified as a comment 
			// in the Token constructor, if it appeared on the first code line
			if (newToken.isAttached() && newToken.textEquals("*")) {
				newToken.type = TokenType.OTHER_OP;
			}
			addChild(newToken);
		} else if (!opensLevel && newToken.closesLevel) {
			if (parent == null) {
				String msg ;
				if (parentCommand.containsChainColonInsideParentheses()) {
					msg = "Chain colons inside parentheses or brackets are currently not supported by " + Program.PRODUCT_NAME + ". ";
					msg += "Please rewrite this ABAP statement manually before running " + Program.PRODUCT_NAME + " on this code.";
				} else {
					msg = "The Token '" + newToken.text + "' cannot be added to '" + text + "', because the latter has no parent Token.";
				}
				throw new UnexpectedSyntaxException(this, msg);
			}
			parent.addSibling(newToken);
		} else {
			addSibling(newToken);
		}
	}

	private void addSibling(Token newToken) {
		newToken.parent = parent;
		newToken.prevSibling = this;

		nextSibling = newToken;
		if (parent != null)
			parent.lastChild = newToken;
	}

	private void addChild(Token newToken) {
		newToken.parent = this;
		newToken.prevSibling = null;

		firstChild = newToken;
		lastChild = newToken;
	}

	@Override
	public String toString() {
		return toString(ABAP.LINE_SEPARATOR);
	}
	
	public String toString(String lineSeparator) {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < lineBreaks; ++i)
			result.append(lineSeparator);
		if (spacesLeft > 0)
			result.append(StringUtil.repeatChar(' ', spacesLeft));
		result.append(text);

		return result.toString();
	}

	/**
	 * Checks whether this Token and the following siblings match the supplied sequence of texts.
	 * Supplied texts may contain alternatives (separated with the TokenSearch.TEXT_MATCH_VARIANT_SEPARATOR |, e.g. "a|b|c"),
	 * use the {@link TokenSearch} constants ANY_LITERAL, ANY_IDENTIFIER etc. or its static method {@link TokenSearch#makeOptional(String)}.
	 */
	public final boolean matchesOnSiblings(boolean skipCommentsAndPragmas, String... texts) {
		return (getLastTokenOfSequence(true, skipCommentsAndPragmas, null, texts) != null);
	}

	/**
	 * Checks whether this Token and the following (child, sibling, or parent) Tokens match the supplied sequence of texts.
	 * Supplied texts may contain alternatives (separated with the TokenSearch.TEXT_MATCH_VARIANT_SEPARATOR |, e.g. "a|b|c"),
	 * use the {@link TokenSearch} constants ANY_LITERAL, ANY_IDENTIFIER etc. or its static method {@link TokenSearch#makeOptional(String)}.
	 */
	public final boolean matchesDeep(boolean skipCommentsAndPragmas, String... texts) {
		return (getLastTokenOfSequence(false, skipCommentsAndPragmas, null, texts) != null);
	}

	public final Token getLastTokenOnSiblings(boolean skipCommentsAndPragmas, String... texts) {
		return getLastTokenOfSequence(true, skipCommentsAndPragmas, null, texts);
	}

	public final Token getLastTokenDeep(boolean skipCommentsAndPragmas, String... texts) {
		return getLastTokenOfSequence(false, skipCommentsAndPragmas, null, texts);
	}

	public final Token getLastTokenOfSequence(boolean siblingsOnly, boolean skipCommentsAndPragmas, Token endToken, String... texts) {
		Token token = this;
		Token lastToken = null;
		boolean skipMode = false;
		boolean skipTokensMustBeComments = false;
		int skipTokensLeft = Integer.MAX_VALUE;
		int resumeSkipIndex = -1;
		boolean optionalMode = false;

		int textIndex = 0;
		while (textIndex < texts.length) {
			String text = texts[textIndex];
			if (token == null || token == endToken)
				return null;

			// check whether this token is marked as optional
			if (text.endsWith(TokenSearch.OPTIONAL_TOKEN_SUFFIX)) {
				optionalMode = true;
				text = text.substring(0, text.length() - TokenSearch.OPTIONAL_TOKEN_SUFFIX.length());
			}

			boolean match;
			if (TokenSearch.ASTERISK.equals(text) || TokenSearch.ANY_NUMBER_OF_COMMENTS.equals(text) || TokenSearch.MAX_ONE_NON_MATCHING_TOKEN.equals(text)) {
				skipMode = true;
				skipTokensMustBeComments = (TokenSearch.ANY_NUMBER_OF_COMMENTS.equals(text));
				skipTokensLeft = (TokenSearch.MAX_ONE_NON_MATCHING_TOKEN.equals(text)) ? 1 : Integer.MAX_VALUE;
				++textIndex;
				if (TokenSearch.ASTERISK.equals(text))
					resumeSkipIndex = textIndex;
				continue;

			} else if (TokenSearch.ANY_LITERAL.equals(text)) {
				match = (token.type == TokenType.LITERAL);

			} else if (TokenSearch.ANY_IDENTIFIER.equals(text)) {
				match = (token.type == TokenType.IDENTIFIER);

			} else if (TokenSearch.ANY_IDENTIFIER_OR_LITERAL.equals(text)) {
				match = (token.type == TokenType.IDENTIFIER || token.type == TokenType.LITERAL);

			} else if (TokenSearch.ANY_COMPARISON_OPERATOR.equals(text)) {
   			// TODO: "|| ..." is required because the differentiation between assignment and comparison operator in Command.finishBuild() / .distinguishOperators() is not yet perfect:
				match = token.isComparisonOperator() || (token.isAssignmentOperator() && token.textEquals("=")); 

			} else if (TokenSearch.ANY_TERM.equals(text) || TokenSearch.ANY_ARITHMETIC_EXPRESSION.equals(text)) {
				match = Term.isFirstTokenAllowed(token);

			} else if (TokenSearch.ANY_NON_COMMENT.equals(text)) {
				match = (token.type != TokenType.COMMENT);

			} else if (text.indexOf(TokenSearch.TEXT_MATCH_VARIANT_SEPARATOR) >= 0) {
				match = false;
				String[] variants = StringUtil.split(text, TokenSearch.TEXT_MATCH_VARIANT_SEPARATOR, false);
				for (String variant : variants) {
					Token testToken = token.getLastTokenOfPlainSequence(siblingsOnly, skipCommentsAndPragmas, variant);
					if (testToken != null) {
						match = true;
						token = testToken;
						break;
					}
				}
			} else {
				Token testToken = token.getLastTokenOfPlainSequence(siblingsOnly, skipCommentsAndPragmas, text);
				match = (testToken != null);
				if (match) {
					token = testToken;
				}
			}

			if (match) {
				skipMode = false;
				optionalMode = false;
				if (TokenSearch.ANY_TERM.equals(text)) {
					Term term;
					try {
						term = Term.createSimple(token);
					} catch (UnexpectedSyntaxException e) {
						return null;
					}
					lastToken = term.lastToken;
				} else if (TokenSearch.ANY_ARITHMETIC_EXPRESSION.equals(text)) {
					Term term;
					try {
						term = Term.createArithmetic(token);
					} catch (UnexpectedSyntaxException e) {
						return null;
					}
					lastToken = term.lastToken;
				} else {
					lastToken = token;
				}
				if (siblingsOnly)
					token = skipCommentsAndPragmas ? lastToken.getNextCodeSibling() : lastToken.nextSibling;
				else
					token = skipCommentsAndPragmas ? lastToken.getNextCodeToken() : lastToken.next;
				++textIndex;
			} else {
				if (optionalMode) {
					// the optional text bit was not found; continue with the next text bit WITHOUT proceeding to the next Token
					optionalMode = false;
					++textIndex;

				} else if (skipMode && skipTokensLeft > 0) {
					if (skipTokensMustBeComments && !token.isComment())
						return null;
					--skipTokensLeft;
					if (siblingsOnly)
						token = skipCommentsAndPragmas ? token.getNextCodeSibling() : token.nextSibling;
					else
						token = skipCommentsAndPragmas ? token.getNextCodeToken() : token.next;
					// textIndex remains unchanged!

				} else if (resumeSkipIndex >= 0 && resumeSkipIndex < textIndex) {
					// If we are searching for texts = { "END", "OF" }, and the code reads "end TYPE ... END OF", then a mismatch
					// will be detected between texts[textIndex] = "OF" and Token.text = "TYPE". In such a case, resume the matching  
					// from the 'resumeSkipIndex', which points to texts[resumeSkipIndex] = "END", but do NOT advance the token.
					// (Endless loops are prevented by the 'resumeSkipIndex < textIndex' condition.)
					textIndex = resumeSkipIndex;
					// a 'resumeSkipIndex' is only used if TokenSearch.ANY_NUMBER_OF_NON_MATCHING_TOKENS is encountered:
					skipMode = true;
					skipTokensLeft = Integer.MAX_VALUE;
					
				} else {
					return null;
				}
			}
		}
		return skipMode ? null : lastToken;
	}

	private Token getLastTokenOfPlainSequence(boolean siblingsOnly, boolean skipCommentsAndPragmas, String textWithSpaces) {
		// textWithSpaces may contain spaces for several Tokens, e.g. "TRANSPORTING NO FIELDS"; 
		// however, if it starts with a comment sign, it is treated as one text, e.g. ""#EC CHAIN_DECL_USAG" 
		final char space = ' ';
		String[] textBits = textWithSpaces.startsWith(ABAP.COMMENT_SIGN_STRING) ? new String[] { textWithSpaces } : StringUtil.split(textWithSpaces, space, false);
		return getLastTokenOfPlainSequence(siblingsOnly, skipCommentsAndPragmas, textBits);
	}
	
	private Token getLastTokenOfPlainSequence(boolean siblingsOnly, boolean skipCommentsAndPragmas, String[] texts) {
		Token lastToken = null;
		Token token = this;
		for (String text : texts) {
			if (token == null || !token.textEquals(text))
				return null;
			lastToken = token;
			if (siblingsOnly)
				token = skipCommentsAndPragmas ? token.getNextCodeSibling() : token.nextSibling;
			else
				token = skipCommentsAndPragmas ? token.getNextCodeToken() : token.next;
		}
		return lastToken;
	}

	public final void removeFromCommand() throws UnexpectedSyntaxAfterChanges {
		removeFromCommand(false, false);
	}
	public final void removeFromCommand(boolean mayMoveFollowingLinesLeft) throws UnexpectedSyntaxAfterChanges {
		removeFromCommand(mayMoveFollowingLinesLeft, false);
	}
	public final void removeFromCommand(boolean mayMoveFollowingLinesLeft, boolean skipIntegrityTest) throws UnexpectedSyntaxAfterChanges {
		if (hasChildren())
			throw new UnexpectedSyntaxAfterChanges(null, this, "Removing the Token '" + text + "' with its child Tokens is not yet supported!");
		if (parentCommand.firstToken == this && parentCommand.lastToken == this)
			throw new UnexpectedSyntaxAfterChanges(null, this, "A Command must contain at least one Token!");

		// before removing the Token, get some relationships and information for the .addIndent() call below
		// if (isFirstTokenInLine)
		//    mayMoveFollowingLinesLeft = false;
		Command command = parentCommand;
		Token nextToken = next;
		int length = spacesLeft + getTextLength();
		int rightPos = mayMoveFollowingLinesLeft ? getEndIndexInLine() : 0; // save performance if variable is not needed

		// prevent the next Token in the line from being moved up to the previous line, esp. if the previous line ends 
		// with a comment; if the to-be-removed Token is afterwards moved to a different place, its new lineBreaks / spacesLeft
		// must therefore be set only AFTER calling .removeFromCommand()
		if (lineBreaks > 0 && next != null && next.lineBreaks == 0) {
			int newSpacesLeft = spacesLeft;
			if (mayMoveFollowingLinesLeft) {
				// for now, keep the next Token at its position, and adjust the length for the .addIndent() call below
				newSpacesLeft = rightPos + next.spacesLeft;
				length = getTextLength() + next.spacesLeft;
			}
			next.setWhitespace(lineBreaks, newSpacesLeft);
		}

		if (parentCommand.firstToken == this)
			parentCommand.firstToken = next;
		if (parentCommand.lastToken == this)
			parentCommand.lastToken = prev;
		--parentCommand.tokenCount;

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

		parent = null;
		prev = null;
		next = null;
		prevSibling = null;
		nextSibling = null;
		parentCommand = null;
		
		if (mayMoveFollowingLinesLeft && nextToken != null)
			command.addIndent(-length, rightPos, nextToken);

		command.onTokenRemoved(this);
		if (!skipIntegrityTest) {
			command.testReferentialIntegrity(true);
		}
	}

	public final boolean wasRemovedFromCommand() {
		return (parentCommand == null); // alternative: return (prev == null && parentCommand.firstToken != this);
	}
	
	public final void insertParenthesesUpTo(Token tokenAfterParentheses) throws IntegrityBrokenException {
		insertParenthesesUpTo(tokenAfterParentheses, "(", ")");
	}
	
	public final void insertParenthesesUpTo(Token tokenAfterParentheses, String openingParenthesisText, String closingParenthesisText) throws IntegrityBrokenException {
		if (tokenAfterParentheses == null)
			throw new NullPointerException("tokenAfterParentheses");
		if (openingParenthesisText == null)
			throw new NullPointerException("openingParenthesisText");
		if (closingParenthesisText == null)
			throw new NullPointerException("closingParenthesisText");

		int startIndexInLine = getStartIndexInLine();

		// if the tokenAfterParentheses is itself a closing parenthesis, insert a temporary right sibling to the Token before it: 
		// e.g., the Command is "CHECK a = 1 AND ( b = 1 OR b = 2 AND c = 3 ).", and parentheses shall be added around "b = 2 AND c = 3"; 
		// then insert "T" as temporary Token: "CHECK a = 1 AND ( b = 1 OR b = 2 AND c = 3 T )." and remove it later
		Token endToken = tokenAfterParentheses;
		boolean usingTempToken = false;
		if (tokenAfterParentheses.closesLevel) {
			Token tempToken = Token.create(0, 1, "T", tokenAfterParentheses.sourceLineNum, parentCommand.getLanguage()); 
			tokenAfterParentheses.getPrev().insertRightSibling(tempToken);
			endToken = tempToken;
			usingTempToken = true;
		}
		
		// first insert opening parenthesis as if it was a normal sibling (skipping the integrity test)
		Token openingParenthesis = Token.create(lineBreaks, spacesLeft, openingParenthesisText, sourceLineNum, parentCommand.getLanguage());
		insertLeftSibling(openingParenthesis, false, true);
		setWhitespace();
		parentCommand.addIndent(openingParenthesisText.length() + 1, startIndexInLine, this, endToken);

		// move this Token and its siblings to child level
		if (this != endToken) {
			openingParenthesis.firstChild = this;
			openingParenthesis.lastChild = endToken.prev;

			Token prevChild = null;
			Token child = this;
			while (child != null && child != endToken) {
				child.parent = openingParenthesis;
				child.prevSibling = prevChild;
				prevChild = child;
				child = child.nextSibling;
			}
			prevChild.nextSibling = null;
		}

		// temporarily set siblings, before the closing parenthesis is inserted
		openingParenthesis.nextSibling = endToken;
		endToken.prevSibling = openingParenthesis;

		// insert closing parenthesis
		// int spacesLeft = tokenAfterParentheses.isPrecededByWhitespace() ? tokenAfterParentheses.spacesLeft : 1;
		Token closingParenthesis = Token.create(0, 1, closingParenthesisText, tokenAfterParentheses.sourceLineNum, parentCommand.getLanguage());
		endToken.insertLeftSibling(closingParenthesis);
		if (tokenAfterParentheses.lineBreaks == 0 && !tokenAfterParentheses.textEqualsAny(".", ","))
			tokenAfterParentheses.spacesLeft = Math.max(tokenAfterParentheses.spacesLeft, 1);

		// if a temporary Token was inserted above, remove it again
		if (usingTempToken) {
			try {
				endToken.removeFromCommand(false, true);
			} catch (UnexpectedSyntaxAfterChanges e) {
				throw new IntegrityBrokenException(tokenAfterParentheses, "error removing temporary token");
			}
		}
		
		parentCommand.testReferentialIntegrity(true);
	}

	public final void removeParentheses() throws IntegrityBrokenException, UnexpectedSyntaxException {
		if (!textEquals("(") || !hasChildren() || nextSibling == null || !nextSibling.textEquals(")"))
			throw new UnexpectedSyntaxException("expected '( ... )'");
		
		Term innerExpr = Term.createForTokenRange(firstChild, lastChild);
		innerExpr.removeFromCommand(true);
		innerExpr.firstToken.copyWhitespaceFrom(this);
		insertLeftSibling(innerExpr);
		
		// remove the parentheses, copying the preceding whitespace to the next Token
		Term parentheses = Term.createForTokenRange(this, nextSibling);
		Token nextAfterParentheses = parentheses.lastToken.next;
		if (nextAfterParentheses != null)
			nextAfterParentheses.copyWhitespaceFrom(parentheses.lastToken);
		parentheses.removeFromCommand(true);
		// if the next Token is a period or comma, remove whitespace if possible
		if (nextAfterParentheses != null && nextAfterParentheses.isCommaOrPeriod() && !nextAfterParentheses.prev.isComment())
			nextAfterParentheses.setWhitespace(0, 0);
		
		parentCommand.testReferentialIntegrity(true);
	}
	
	public final void appendParenthesesUpTo(Token tokenAfterParentheses, boolean keepPragmasBehindParens) throws UnexpectedSyntaxAfterChanges {
		if (tokenAfterParentheses == null)
			throw new NullPointerException("tokenAfterParentheses");
		// TODO: if needed at some point, get rid of this restriction:
		if (hasChildren() || opensLevel || textEndsWith("(") || next == null)
			throw new UnexpectedSyntaxAfterChanges(null, this, "Insertion of parentheses not supported for the Token '" + text + "' because it has child Tokens!");

		// keep pragmas (and comments between those pragmas) at the end behind the parentheses?
		if (keepPragmasBehindParens) {
			do {
				Token testToken = tokenAfterParentheses.getPrevNonCommentSibling();
				if (testToken != null && testToken.isPragma())
					tokenAfterParentheses = testToken;
				else
					break;
			} while (true);
		}
		
		text += "(";
		parentCommand.addIndent("(".length(), getEndIndexInLine(), this, tokenAfterParentheses);
		opensLevel = true;

		// move siblings to child level
		if (next != tokenAfterParentheses) {
			firstChild = next;
			lastChild = tokenAfterParentheses.prevSibling;

			Token prevChild = null;
			Token child = next;
			while (child != tokenAfterParentheses) {
				child.parent = this;
				child.prevSibling = prevChild;
				prevChild = child;
				child = child.nextSibling;
			}
			prevChild.nextSibling = null;
		}

		// temporarily set siblings, before " )" is inserted
		nextSibling = tokenAfterParentheses;
		tokenAfterParentheses.prevSibling = this;

		// insert ")"
		int spacesLeft = tokenAfterParentheses.isPrecededByWhitespace() ? tokenAfterParentheses.spacesLeft : 1;
		Token closingParenthesis = Token.create(tokenAfterParentheses.lineBreaks, spacesLeft, ")", tokenAfterParentheses.sourceLineNum, parentCommand.getLanguage());
		tokenAfterParentheses.insertLeftSibling(closingParenthesis);
		tokenAfterParentheses.lineBreaks = 0;
		tokenAfterParentheses.spacesLeft = tokenAfterParentheses.isCommaOrPeriod() ? 0 : 1;
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling before this Token
	 * 
	 * @param newToken
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertLeftSibling(Token newToken) throws IntegrityBrokenException {
		return insertLeftSibling(newToken, false, null, false);
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling before this Token
	 * 
	 * @param newToken
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertLeftSibling(Token newToken, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		return insertLeftSibling(newToken, moveFollowingLinesRight, null, false);
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling before this Token
	 * 
	 * @param newToken
	 * @param moveFollowingLinesRight
	 * @param skipIntegrityTest
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertLeftSibling(Token newToken, boolean moveFollowingLinesRight, boolean skipIntegrityTest) throws IntegrityBrokenException {
		return insertLeftSibling(newToken, moveFollowingLinesRight, null, skipIntegrityTest);
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling before this Token
	 * 
	 * @param newToken
	 * @param moveFollowingLinesRight
	 * @param addIndentEndToken
	 * @param skipIntegrityTest
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertLeftSibling(Token newToken, boolean moveFollowingLinesRight, Token addIndentEndToken, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (newToken == null)
			throw new NullPointerException("newToken");
		if (closesLevel)
			throw new IntegrityBrokenException(this, "cannot insert a left sibling to a token that closes a level");
		
		int oldStartIndex = moveFollowingLinesRight ? getStartIndexInLine() : 0; // save performance if information is not needed below

		// ensure newToken is not placed behind a comment
		if (prev != null && prev.isComment() && newToken.lineBreaks == 0 && lineBreaks > 0) {
			newToken.copyWhitespaceFrom(this);
			setWhitespace();
		}
		// if newToken is a comment, ensure a line break before this Token
		if (newToken.isComment() && lineBreaks == 0) {
			ensureLineBreak();
		}
		
		++parentCommand.tokenCount;

		newToken.parentCommand = parentCommand;
		if (parentCommand.firstToken == this)
			parentCommand.firstToken = newToken;

		if (prev != null)
			prev.next = newToken;
		newToken.prev = prev;

		prev = newToken.lastChild != null ? newToken.lastChild : newToken;
		prev.next = this;

		newToken.parent = parent;
		if (parent != null && parent.firstChild == this)
			parent.firstChild = newToken;

		if (prevSibling != null)
			prevSibling.nextSibling = newToken;
		newToken.prevSibling = prevSibling;

		newToken.nextSibling = this;
		prevSibling = newToken;

		if (moveFollowingLinesRight)
			parentCommand.addIndent(newToken.spacesLeft + newToken.getTextLength(), oldStartIndex, this, addIndentEndToken, true);

		parentCommand.onTokenInserted(newToken);
		
		if (!skipIntegrityTest)
			parentCommand.testReferentialIntegrity(true);
		
		return newToken;
	}

	private void ensureLineBreak() {
		if (lineBreaks == 0) {
			Token prevCode = getPrevCodeSibling();
			if (prevCode != null && prevCode.isComma()) {
				setWhitespace(1, parentCommand.firstToken.getStartIndexInLine() + ABAP.INDENT_STEP);
			} else {
				setWhitespace(1, getStartIndexInLine());
			}
		}
	}
	
	/**
	 * inserts the supplied Token as the next sibling, or first child after this Token
	 * 
	 * @param newToken
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertNext(Token newToken) throws IntegrityBrokenException {
		if (!opensLevel) {
			return insertRightSibling(newToken, false);
		} else if (hasChildren()) {
			return firstChild.insertLeftSibling(newToken);
		} else {
			return insertFirstChild(newToken);
		}
	}

	/**
	 * inserts the supplied Term as the next sibling, or first child after this Token
	 */
	public final void insertNext(Term newTerm) throws IntegrityBrokenException {
		insertNext(newTerm, false);
	}

	/**
	 * inserts the supplied Term as the next sibling, or first child after this Token
	 */
	public final void insertNext(Term newTerm, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (!opensLevel) {
			insertRightSibling(newTerm, false, skipIntegrityTest);
		} else if (hasChildren()) {
			firstChild.insertLeftSibling(newTerm, skipIntegrityTest);
		} else {
			insertFirstChild(newTerm, skipIntegrityTest);
		}
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling after this Token
	 * 
	 * @param newToken
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertRightSibling(Token newToken) throws IntegrityBrokenException {
		return insertRightSibling(newToken, false, false);
	}

	/** inserts the supplied Token (including its possible children) as a sibling after this Token */
	public final Token insertRightSibling(Token newToken, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		return insertRightSibling(newToken, moveFollowingLinesRight, false);
	}

	/** inserts the supplied Token (including its possible children) as a sibling after this Token */
	public final Token insertRightSibling(Token newToken, boolean moveFollowingLinesRight, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (newToken == null)
			throw new NullPointerException("newToken");
		if (opensLevel)
			throw new IntegrityBrokenException(this, "cannot insert a right sibling to a token that opens a level");

		int oldStartIndex = moveFollowingLinesRight ? getEndIndexInLine() + 1 : 0; // save performance if information is not needed below

		// ensure newToken is not placed behind a comment
		if (isComment() && newToken.lineBreaks == 0) {
			if (next == null) {
				throw new IntegrityBrokenException(this, "cannot insert a right sibling after the final comment in a command");
			} else {
				newToken.copyWhitespaceFrom(next);
				if (newToken.isComment()) {
					next.lineBreaks = 1;
				} else {
					next.setWhitespace();
				}
			}
		}
		// if newToken is a comment, ensure a line break before the next Token
		if (newToken.isComment() && next != null && next.lineBreaks == 0) {
			next.ensureLineBreak();
		}

		++parentCommand.tokenCount;

		newToken.parentCommand = parentCommand;
		if (parentCommand.lastToken == (lastChild != null ? lastChild : this))
			parentCommand.lastToken = newToken.lastChild != null ? newToken.lastChild : newToken;

		newToken.parent = parent;
		if (parent != null && parent.lastChild == this)
			parent.lastChild = newToken;

		if (nextSibling != null) {
			nextSibling.prevSibling = newToken;
			nextSibling.prev = newToken.lastChild != null ? newToken.lastChild : newToken;
		}
		newToken.nextSibling = nextSibling;

		newToken.prevSibling = this;
		nextSibling = newToken;

		Token next = (lastChild != null ? lastChild : this).next; // may be on a higher(!) level
		(newToken.lastChild != null ? newToken.lastChild : newToken).next = next;
		if (next != null)
			next.prev = (newToken.lastChild != null) ? newToken.lastChild : newToken;

		newToken.prev = (lastChild != null ? lastChild : this);
		newToken.prev.next = newToken;

		if (moveFollowingLinesRight)
			parentCommand.addIndent(newToken.spacesLeft + newToken.getTextLength(), oldStartIndex, newToken, null, true);

		parentCommand.onTokenInserted(newToken);
		if (!skipIntegrityTest)
			parentCommand.testReferentialIntegrity(true);
		
		return newToken;
	}

	/**
	 * inserts the supplied Term (including its possible children) as a sibling after this Token
	 * 
	 * @param newTerm
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Term newTerm) throws IntegrityBrokenException {
		insertRightSibling(newTerm, false, false);
	}

	/**
	 * inserts the supplied Term (including its possible children) as a sibling after this Token
	 * 
	 * @param newTerm
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Term newTerm, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		insertRightSibling(newTerm, moveFollowingLinesRight, false);
	}

	/**
	 * inserts the supplied Term (including its possible children) as a sibling after this Token
	 * 
	 * @param newTerm
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Term newTerm, boolean moveFollowingLinesRight, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (newTerm == null)
			throw new NullPointerException("newTerm");
		if (opensLevel)
			throw new IntegrityBrokenException(this, "cannot insert a right sibling to a token that opens a level");

		int oldStartIndex = moveFollowingLinesRight ? getEndIndexInLine() + 1 : 0; // save performance if information is not needed below
		int newTermWidth = newTerm.getCurrentWidth(false);

		Token lastTokenInNewTerm = newTerm.lastToken.lastChild != null ? newTerm.lastToken.lastChild : newTerm.lastToken; // TODO: can newTerm.lastToken have child Tokens at all?

		// ensure newTerm is not placed behind a comment
		if (isComment() && newTerm.firstToken.lineBreaks == 0) {
			if (next == null) {
				throw new IntegrityBrokenException(this, "cannot insert a right sibling after the final comment in a command");
			} else {
				newTerm.firstToken.copyWhitespaceFrom(next);
				if (newTerm.lastToken.isComment()) {
					next.lineBreaks = 1; // keep next.spacesLeft
				} else {
					next.setWhitespace();
				}
			}
		}
		// if newTerm ends with a comment, ensure a line break before the next Token
		if (newTerm.lastToken.isComment() && next != null && next.lineBreaks == 0) {
			next.ensureLineBreak();
		}

		parentCommand.tokenCount += newTerm.getTokenCountWithChildren();

		newTerm.setParentCommand(parentCommand);
		if (parentCommand.lastToken == (lastChild != null ? lastChild : this))
			parentCommand.lastToken = lastTokenInNewTerm;

		newTerm.setParent(parent);
		if (parent != null && parent.lastChild == this)
			parent.lastChild = newTerm.lastToken;

		if (nextSibling != null) {
			nextSibling.prevSibling = newTerm.lastToken;
			nextSibling.prev = lastTokenInNewTerm;
		}
		newTerm.lastToken.nextSibling = nextSibling;

		newTerm.firstToken.prevSibling = this;
		nextSibling = newTerm.firstToken;

		Token next = (lastChild != null ? lastChild : this).next; // may be on a higher(!) level
		lastTokenInNewTerm.next = next;
		if (next != null)
			next.prev = lastTokenInNewTerm;

		newTerm.setPrev((lastChild != null) ? lastChild : this);
		newTerm.getPrev().next = newTerm.firstToken;

		if (moveFollowingLinesRight)
			parentCommand.addIndent(newTerm.firstToken.spacesLeft + newTermWidth, oldStartIndex, newTerm.lastToken, null);

		parentCommand.onTermInserted(newTerm);
		if (!skipIntegrityTest) {
			parentCommand.testReferentialIntegrity(true);
		}
	}

	public final void insertLeftSibling(Term newTerm) throws IntegrityBrokenException {
		insertLeftSibling(newTerm, false);
	}
	
	public final void insertLeftSibling(Term newTerm, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (newTerm == null)
			throw new NullPointerException("newTerm");
		if (closesLevel)
			throw new IntegrityBrokenException(this, "cannot insert a left sibling to a token that closes a level");

		// ensure newTerm is not placed behind a comment
		if (prev != null && prev.isComment() && newTerm.firstToken.lineBreaks == 0 && lineBreaks > 0) {
			newTerm.firstToken.copyWhitespaceFrom(this);
			if (newTerm.lastToken.isComment()) {
				lineBreaks = 1; // keep spacesLeft
			} else {
				setWhitespace();
			}
		}
		// if newTerm ends with a comment, ensure a line break before this Token
		if (newTerm.lastToken.isComment() && lineBreaks == 0) {
			setWhitespace(1, getStartIndexInLine());
		}
		
		parentCommand.tokenCount += newTerm.getTokenCountWithChildren();

		newTerm.setParentCommand(parentCommand);
		if (parentCommand.firstToken == this)
			parentCommand.firstToken = newTerm.firstToken;

		newTerm.setParent(parent);
		if (parent != null && parent.firstChild == this)
			parent.firstChild = newTerm.firstToken;

		
		if (prev != null)
			prev.next = newTerm.firstToken;
		newTerm.setPrev(prev);

		prev = newTerm.lastToken; // newTerm.lastToken is sure to be childless
		prev.next = this;

		if (prevSibling != null)
			prevSibling.nextSibling = newTerm.firstToken;
		newTerm.setPrevSibling(prevSibling);

		newTerm.setNextSibling(this);
		prevSibling = newTerm.lastToken; // newTerm.lastToken is sure to be childless

		parentCommand.onTermInserted(newTerm);
		
		if (!skipIntegrityTest) {
			parentCommand.testReferentialIntegrity(true);
		}
	}

	public final boolean copyWhitespaceFrom(Token token) {
		if (token == null)
			throw new NullPointerException("token");
		return setWhitespace(token.lineBreaks, token.spacesLeft);
	}

	public final void ensureWhitespace() {
		if (lineBreaks == 0 && spacesLeft == 0)
			spacesLeft = 1;
	}

	/**
	 * Returns true if the Token text equals any of the supplied comparison texts.  
	 * To check the Token type at the same time, use {@link #isAnyKeyword(String...)} or {@link #isAnyComparisonOperator(String...)} 
	 * (see list of tokens classified as comparison operators in {@link ABAP#isComparisonOperator(String, boolean)}) 
	 */
	public final boolean textEqualsAny(String... compareTexts) {
		for (String compareText : compareTexts) {
			if (textEquals(compareText))
				return true;
		}
		return false;
	}

	public final boolean textStartsWithAny(String... prefixes) {
		for (String prefix : prefixes) {
			if (textStartsWith(prefix))
				return true;
		}
		return false;
	}

	public final boolean textEndsWithAny(String... suffixes) {
		for (String suffix : suffixes) {
			if (textEndsWith(suffix))
				return true;
		}
		return false;
	}

	/**
	 * sets one space (and no line breaks); returns true if whitespace was changed
	 * 
	 * @param lineBreaks
	 * @param spacesLeft
	 * @return
	 */
	public final boolean setWhitespace() {
		return setWhitespace(0, 1);
	}
	/**
	 * returns true if whitespace was changed
	 * 
	 * @param lineBreaks
	 * @param spacesLeft
	 * @return
	 */
	public final boolean setWhitespace(int lineBreaks, int spacesLeft) {
		if (this.lineBreaks == lineBreaks && this.spacesLeft == spacesLeft)
			return false;
		this.lineBreaks = lineBreaks;
		this.spacesLeft = spacesLeft;
		return true;
	}

	/**
	 * returns true if whitespace was changed
	 * 
	 * @param lineBreaks
	 * @return
	 */
	public final boolean setLineBreaks(int lineBreaks) {
		if (this.lineBreaks == lineBreaks)
			return false;
		this.lineBreaks = lineBreaks;
		return true;
	}

	public final boolean setSpacesLeftAdjustingIndent(int newSpacesLeft, boolean adjustIfDirectlyBelow) {
		if (newSpacesLeft == spacesLeft)
			return false;
		
		if (next == null) {
			// nothing to adjust
			spacesLeft = newSpacesLeft; 
		} else {
			int addSpaceCount = newSpacesLeft - spacesLeft;
			int minSpacesLeft;
			if (adjustIfDirectlyBelow) {
				minSpacesLeft = this.getStartIndexInLine();
			} else {
				minSpacesLeft = (next.lineBreaks == 0) ? next.getStartIndexInLine() : this.getStartIndexInLine() + 1;
			}
			spacesLeft = newSpacesLeft;
			parentCommand.addIndent(addSpaceCount, minSpacesLeft, next, null, true);
		}
		return true;
	}

	final ColorType getMainColorType() {
		boolean isDdlOrDcl = parentCommand.isDdlOrDcl();
		
		switch (type) {
			case COMMENT:
			case PRAGMA:
				return ColorType.COMMENT;

			case LITERAL:
				return isStringLiteral() ? ColorType.STRING_LITERAL : ColorType.NUMBER;

			case ASSIGNMENT_OP:
				if (isDdlOrDcl) {
					return ColorType.DDL_KEYWORD;
				} else {
					return ColorType.USUAL_OPERATOR;
				}

			case OTHER_OP:
				if (isDdlOrDcl) {
					return ColorType.DDL_KEYWORD;
				} else {
					return textEquals(")") && isAttached() ?  ColorType.TOKEN_OPERATOR : ColorType.USUAL_OPERATOR;
				}

			case COMPARISON_OP:
				if (isDdlOrDcl) {
					return ColorType.DDL_KEYWORD;
				} else if (!StringUtil.isNullOrEmpty(text) && Character.isLetter(text.charAt(0))) { // GE, GT, EQ, NE etc.
					return ColorType.KEYWORD;
				} else {
					return ColorType.USUAL_OPERATOR;
				}

			case KEYWORD:
				if (isDdlOrDcl) {
					return ColorType.DDL_KEYWORD;
				} else {
					return isDeclarationKeyword() ? ColorType.DECLARATION_KEYWORD : ColorType.KEYWORD;
				}

			case IDENTIFIER:
				if (isDdlOrDcl) {
					if (isChildOf("cast") && getNextCodeSibling() == null) {
						return ColorType.DDL_IDENTIFIER_DATA_ELEMENT;
					} else {
						return ColorType.IDENTIFIER;
					}
				} else {
					MemoryAccessType accessType = getMemoryAccessType(); 
					return accessType.displayAsWritePos ? ColorType.IDENTIFIER_WRITE_POS : ColorType.IDENTIFIER;
				}
				
			case NON_ABAP:
				return ColorType.NON_ABAP;

			default:
				return ColorType.IDENTIFIER;
		}
	}

	public final boolean isDeclarationKeyword() {
		if (!isKeyword())
			return false;
		
		if (opensInlineDeclaration())
			return true;
		else if (this != parentCommand.getFirstCodeToken())
			return false;
		
		if (textEqualsAny(Command.declarationKeywords))
			return true;
		if (textEqualsAny(Command.declarationKeywordsOnlyInClassDef))
			return true;
		
		return false;
	}

	final TextBit[] toTextBits(int startIndex, boolean isInOOContext) {
		boolean isAbap = parentCommand.isAbap();
		boolean isDdlOrDcl = parentCommand.isDdlOrDcl();
		boolean isSimpleCase = false;
		
		// in most cases, the whole Token is of one ColorType
		
		ColorType colType;
		if (isDdlOrDcl && parentCommand.isDdlAnnotation() && type != TokenType.COMMENT) { 
			// in CDS DDL annotations, everything gets the same color
			colType = ColorType.DDL_ANNOTATION;
			isSimpleCase = true;
		} else {
			colType = getMainColorType();
		}
		
		if (type != TokenType.KEYWORD && type != TokenType.IDENTIFIER && type != TokenType.LITERAL && type != TokenType.OTHER_OP) {
			isSimpleCase = true;
		} else if (isAbap) {
			if (isAbap && type == TokenType.IDENTIFIER && text.charAt(0) == ABAP.FIELD_SYMBOL_START_SIGN) {
				isSimpleCase = true;
			} else if (isAbap && type == TokenType.OTHER_OP && !textEndsWithAny("(", ")")) { // "ULINE AT /(20)."
				isSimpleCase = true;
			}
		} else if (isDdlOrDcl) {
			if (type == TokenType.LITERAL) {
				isSimpleCase = true;
			} else if (type == TokenType.IDENTIFIER && textStartsWith(DDL.SESSION_PREFIX + DDL.DOT_SIGN_STRING)) {
				// session variables are functionally used like variables (and therefore should have TokenType.IDENTIFIER), 
				// but are displayed like keywords
				isSimpleCase = true;
				colType = ColorType.DDL_KEYWORD;
			}
		}
		
		// literals may be linked to a text symbol ID, having the form 'literal text'(idf), where 'idf' is always 3 characters long
		if (isAbap && type == TokenType.LITERAL) {
			if (text.charAt(0) == ABAP.QUOT_MARK && text.length() >= 7 
					&& text.charAt(text.length() - 5) == ABAP.TEXT_SYMBOL_ID_OPEN
					&& text.charAt(text.length() - 1) == ABAP.TEXT_SYMBOL_ID_CLOSE ) {
				
				int literalLength = text.length() - 5;
				return new TextBit[] { TextBit.create(startIndex, literalLength, colType), 
											  TextBit.create(startIndex + literalLength, 1, ColorType.TOKEN_OPERATOR), 
											  TextBit.create(startIndex + literalLength + 1, 3, ColorType.NUMBER), 
											  TextBit.create(startIndex + literalLength + 4, 1, ColorType.TOKEN_OPERATOR)};
			} else if (!textEndsWith(")")) { // "ULINE AT 10(20)."
				isSimpleCase = true;
			}
		}

		if (isSimpleCase)
			return new TextBit[] { TextBit.create(startIndex, text.length(), colType) };

		if (isAbap && isTextSymbol()) {
			// text symbol IDs may be numeric (TEXT-001) or contain letters or _ (TEXT-a01, TEXT-a_2)
			int prefixLength = ABAP.TEXT_SYMBOL_PREFIX.length();
			ColorType idColorType = ABAP.consistsOfDigitsOnly(text.substring(prefixLength)) ? ColorType.NUMBER : ColorType.IDENTIFIER;
			return new TextBit[] { TextBit.create(startIndex, prefixLength, ColorType.KEYWORD), 
										  TextBit.create(startIndex + prefixLength, ABAP.TEXT_SYMBOL_ID_LENGTH, idColorType) };
		}

		ArrayList<TextBit> result = new ArrayList<TextBit>();
		int writtenPos = 0;
		boolean lastWasIdentifier = false;
		ColorType opColorType = isDdlOrDcl ? ColorType.DDL_KEYWORD : ColorType.TOKEN_OPERATOR;
		for (int i = 0; i < text.length(); ++i) {
			char c = text.charAt(i);
			boolean isIdentifier;
			if (isDdlOrDcl) {
				isIdentifier = (type == TokenType.KEYWORD) ? DDL.isCharAllowedForAnyKeyword(c, (i == 0)) 
						 												 : DDL.isCharAllowedForIdentifier(text, i, (i == 0));
			} else {
				isIdentifier = (type == TokenType.KEYWORD) ? ABAP.isCharAllowedForAnyKeyword(c, (i == 0)) 
																		 : ABAP.isCharAllowedForVariableNames(text, i, (i == 0), false, isInOOContext);
				// in some cases, initial '/' does NOT start an identifier with a namespace, e.g. "ULINE AT /.", "ULINE AT /10(20)." or "ULINE at /pos(20)."
				if (i == 0 && c == '/' && (text.length() <= 1 || text.indexOf('/', 1) < 0)) {
					isIdentifier = false;
				}
			}
			if (i > 0 && isIdentifier != lastWasIdentifier) {
				// if the text bit is not part of the keyword or the identifier, it is considered a token operator (e.g. "->", "=>", "(" etc.)
				ColorType bitType = lastWasIdentifier ? colType : opColorType;
				if (isDdlOrDcl && AbapCult.stringEqualsAny(true, text.substring(writtenPos, i), DDL.PROJECTION_PREFIX, DDL.PARAMETER_PREFIX)) 
					bitType = ColorType.DDL_KEYWORD;
				if (isAbap && ABAP.isInteger(text.substring(writtenPos, i)))
					bitType = ColorType.NUMBER;
				result.add(TextBit.create(startIndex + writtenPos, i - writtenPos, bitType));
				writtenPos = i;
			}
			lastWasIdentifier = isIdentifier;
		}
		// process the last bit
		if (writtenPos < text.length()) {
			ColorType bitType = lastWasIdentifier ? colType : opColorType;
			// in some cases, a Token of type IDENTIFIER contains a number, e.g. "ULINE AT /10." 
			if (ABAP.isInteger(text.substring(writtenPos)))
				bitType = ColorType.NUMBER;
			result.add(TextBit.create(startIndex + writtenPos, text.length() - writtenPos, bitType));
		}

		return result.toArray(new TextBit[0]);
	}

	public final int getStartIndexInLine() {
		Token token = this;
		int result = token.spacesLeft;
		while (token.lineBreaks == 0 && token.prev != null) {
			token = token.prev;
			result += token.text.length() + token.spacesLeft;
		}
		// does this Command continue the line of the previous Command? (parentCommand may be null if this Token is being moved)
		if (token.lineBreaks == 0 && token.prev == null && parentCommand != null && parentCommand.getPrev() != null) {
			result += parentCommand.getPrev().lastToken.getEndIndexInLine();
		}
		return result;
	}

	public final int getEndIndexInLine() {
		Token token = this;
		int result = token.spacesLeft + token.text.length();
		while (token.lineBreaks == 0 && token.prev != null) {
			token = token.prev;
			result += token.spacesLeft + token.text.length();
		}
		// does this Command continue the line of the previous Command? (parentCommand may be null if this Token is being moved)
		if (token.lineBreaks == 0 && token.prev == null && parentCommand != null && parentCommand.getPrev() != null) {
			result += parentCommand.getPrev().lastToken.getEndIndexInLine();
		}
		return result;
	}

	public final int getMinIndexInLine(Token endToken) {
		int minIndex = this.getStartIndexInLine();
		Token token = this;
		while (token != endToken) {
			if (token.lineBreaks > 0)
				minIndex = Math.min(minIndex, token.spacesLeft);
			token = token.next;
		}
		return minIndex;
	}

	public final int getMaxIndexInLine(Token endToken) {
		int maxIndex = 0;
		Token token = this;
		while (token != endToken) {
			Token nextToken = token.next;
			if (nextToken == null || nextToken == endToken || nextToken.lineBreaks > 0)
				maxIndex = Math.max(maxIndex, token.getEndIndexInLine());
			token = token.next;
		}
		return maxIndex;
	}

	public final int getLineInCommand() {
		if (this == parentCommand.firstToken)
			return 0;
		Token token = parentCommand.firstToken.next;
		int line = 0; // (token.lineBreaks == 0) ? 0 : -1;
		while (token != null && token != this) {
			line += token.lineBreaks;
			token = token.next;
		}
		if (token == this) {
			line += lineBreaks;
		}
		return line;
	}
	
	/**
	 * if the Token is an identifier of type "structure-component" or "&lt;field-symbol&gt;-component",
	 * the structure or field-symbol variable is returned
	 * 
	 * @return
	 */
	public final String getStructureVariable() {
		if (!isIdentifier())
			return null;
		int selectorPos = text.lastIndexOf(ABAP.COMPONENT_SELECTOR); // *last*IndexOf() is used for cases of complex statements that have components at their end
		if (selectorPos <= 0)
			return null;
		else
			return text.substring(0, selectorPos);
	}

	final int getSequenceCount(boolean siblingsOnly, boolean skipComments, String... texts) {
		int result = 0;
		Token token = this;
		while (token != null) {
			token = token.getLastTokenOfSequence(siblingsOnly, skipComments, null, texts);
			if (token != null) {
				++result;
				token = token.next;
			}
		}
		return result;
	}

	public final Token getNextWhileComment() { return isComment() ? getNextNonCommentToken() : this; }

	public final Token getPrevWhileComment() { return isComment() ? getPrevNonCommentToken() : this; }

	/** returns this Token unless it is a pragma or a comment; otherwise the next 'code' Token */
	public final Token getThisOrNextCodeToken() { 
		return isComment() || isPragma() ? getNextCodeToken() : this; 
	}

	/** returns this Token unless it is a pragma or a comment; otherwise the next 'code' Token */
	public final Token getThisOrPrevCodeToken() { 
		return isComment() || isPragma() ? getPrevCodeToken() : this; 
	}

	/** returns the next Token that is not a comment (but may be a pragma) */
	public final Token getNextNonCommentToken() {
		Token result = next;
		while (result != null && result.isComment())
			result = result.next;
		return result;
	}

	/** returns the next Token that is not a comment or pragma */
	public final Token getNextCodeToken() {
		Token result = next;
		while (result != null && !result.isCode())
			result = result.next;
		return result;
	}
	
	/** returns the previous Token that is not a comment (but may be a pragma) */
	public final Token getPrevNonCommentToken() {
		Token result = prev;
		while (result != null && result.isComment())
			result = result.prev;
		return result;
	}

	/** returns the previous Token that is not a comment or pragma */
	public final Token getPrevCodeToken() {
		Token result = prev;
		while (result != null && !result.isCode())
			result = result.prev;
		return result;
	}

	/** returns the next sibling Token that is not a comment (but may be a pragma) */
	public final Token getNextNonCommentSibling() {
		Token sibling = nextSibling;
		while (sibling != null && sibling.isComment())
			sibling = sibling.nextSibling;
		return sibling;
	}

	/** returns the next sibling Token that is not a comment or pragma */
	public final Token getNextCodeSibling() {
		Token sibling = nextSibling;
		while (sibling != null && !sibling.isCode())
			sibling = sibling.nextSibling;
		return sibling;
	}

	/** returns the previous sibling Token that is not a comment (but may be a pragma) */
	public final Token getPrevNonCommentSibling() {
		Token sibling = prevSibling;
		while (sibling != null && sibling.isComment())
			sibling = sibling.prevSibling;
		return sibling;
	}

	/** returns the previous sibling Token that is not a comment or pragma */
	public final Token getPrevCodeSibling() {
		Token sibling = prevSibling;
		while (sibling != null && !sibling.isCode())
			sibling = sibling.prevSibling;
		return sibling;
	}

	public final Token getNextTokenOfType(TokenType tokenType) {
		Token result = next;
		while (result != null && result.type != tokenType)
			result = result.next;
		return result;
	}

	public final Token getNextTokenOfTypeAndText(TokenType tokenType, String... texts) {
		Token result = next;
		while (result != null) {
			if (result.type == tokenType && result.textEqualsAny(texts)) {
				return result;
			}
			result = result.next;
		}
		return null;
	}

	public final Token getNextTokenOfTypes(TokenType... tokenTypes) {
		Token result = next;
		while (result != null) {
			for (TokenType tokenType : tokenTypes) {
				if (result.type == tokenType) {
					return result;
				}
			}
			result = result.next;
		}
		return result;
	}

	public final Token getNextSiblingOfType(TokenType tokenType) {
		Token result = nextSibling;
		while (result != null && result.type != tokenType)
			result = result.nextSibling;
		return result;
	}

	public final Token getNextSiblingOfTypeAndText(TokenType tokenType, String... texts) {
		Token result = nextSibling;
		while (result != null) {
			if (result.type == tokenType && result.textEqualsAny(texts)) {
				return result;
			}
			result = result.nextSibling;
		}
		return result;
	}

	public final Token getNextSiblingOfTypeAndTextBefore(Token end, TokenType tokenType, String... texts) {
		Token result = nextSibling;
		while (result != null && result != end) {
			if (result.type == tokenType && result.textEqualsAny(texts)) {
				return result;
			}
			result = result.nextSibling;
		}
		return null;
	}
	
	public final Token getNextSiblingWhileLevelOpener() {
		Token token = this;
		while (token != null && token.opensLevel)
			token = token.nextSibling;
		return token;
	}


	public final Token getPrevSiblingWhileLevelCloser() {
		Token token = this;
		while (token != null && token.closesLevel)
			token = token.prevSibling;
		return token;
	}


	public final Token getPrevTokenOfType(TokenType tokenType) {
		Token result = prev;
		while (result != null && result.type != tokenType)
			result = result.prev;
		return result;
	}

	public final Token getPrevSiblingOfType(TokenType tokenType) {
		Token result = prevSibling;
		while (result != null && result.type != tokenType)
			result = result.prevSibling;
		return result;
	}

	final void testReferentialIntegrity(boolean testCommentPositions) throws IntegrityBrokenException {
		check(parentCommand != null);
		check(prev == null || prev.next == this);
		check(next == null || next.prev == this);
		check(prevSibling == null || prevSibling.nextSibling == this);
		check(nextSibling == null || nextSibling.prevSibling == this);
		check(firstChild == null || opensLevel == true); // but not vice versa
		check(firstChild == null || firstChild == next);
		check(firstChild == null || lastChild != null);
		check(lastChild == null || firstChild != null);
		check(lastChild == null || lastChild.next == nextSibling);
		check(opensLevel || nextSibling == null || !nextSibling.closesLevel);
		check(!opensLevel || nextSibling != null && nextSibling.closesLevel);
		check(closesLevel || prevSibling == null || !prevSibling.opensLevel);
		check(!closesLevel || prevSibling != null && prevSibling.opensLevel);
		if (testCommentPositions && !parentCommand.isDdlOrDcl()) {
			// there can be no further Token behind a comment (except in the next line)
			check(prev == null || !prev.isComment() || lineBreaks > 0);
			// asterisk comment lines must be at line start
			check(!isAsteriskCommentLine() || isFirstTokenInCode() || lineBreaks > 0);
			check(!isAsteriskCommentLine() || spacesLeft == 0);
		}
	}

	private void check(boolean value) throws IntegrityBrokenException {
		if (!value) {
			if (parentCommand == null) {
				throw new IntegrityBrokenException(this,
						"Failed referential integrity test on Token '" + text + "': parent Command unknown!");
			} else {
				throw new IntegrityBrokenException(this,
						"Failed referential integrity test on Token '" + text + "' in Command starting at source line " + Cult.format(parentCommand.getSourceLineNumStart()) + "!");
			}
		}
	}

	public final Token getFirstTokenInLine() {
		Token token = this;
		while (token.lineBreaks == 0 && token.prev != null )
			token = token.prev;
		return token;
	}
	
	public final Token getLastTokenInLine() {
		Token token = this;
		while (token.next != null && token.next.lineBreaks == 0)
			token = token.next;
		return token;
	}
	
	public final Token getLastTokenOfKeywordCollocation() {
		Token token = this;
		while (token != null && token.collocationContinues)
			token = token.getNextCodeToken();
		return token;
	}
	
	/**
	 * Changes the Token's {@link #text}. Rules that use this method to change an ABAP keyword or an identifier  
	 * should override {@link Rule#getDependentRules()} and return {@link RuleID#UPPER_AND_LOWER_CASE}. 
	 *  
	 * @param newText
	 * @param adjustIndent
	 */
	public final boolean setText(String newText, boolean adjustIndent) {
		if (newText == null)
			throw new NullPointerException("newText");
		
		if (text.equals(newText))
			return false;

		if (!adjustIndent || next == null || next.lineBreaks > 0) {
			// nothing to adjust
			this.text = newText;
		} else {
			int addSpaceCount = newText.length() - text.length();
			int minSpacesLeft = next.getStartIndexInLine();
			this.text = newText;
			if (addSpaceCount != 0) {
				parentCommand.addIndent(addSpaceCount, minSpacesLeft, next, null, true);
			}
		}
		return true;
	}

	public final String getTextOfKeywordCollocation() {
		StringBuilder result = new StringBuilder();
		Token token = this;
		while (token != null) {
			if (result.length() > 0)
				result.append(" ");
			result.append(token.text);
			if (!token.isKeyword() || !token.collocationContinues)
				break;
			token = token.getNextCodeToken();
		} 
		return result.toString(); 
	}

	public final Token getStartOfAttachedComments() {
		Token token = this;
		while (token.lineBreaks == 1 && token.getPrev() != null && token.getPrev().isCommentLine())
			token = token.getPrev();
		return token;
	}

	public static Token findEndOfLogicalExpression(Token firstTokenOfLogExpr) throws UnexpectedSyntaxException {
		if (firstTokenOfLogExpr == null)
			throw new NullPointerException("firstTokenOfLogExpr");

		// see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenlogexp
		Token token = firstTokenOfLogExpr;
		do {
			// skip leading NOT (multiple are possible)
			while (token.textEndsWith("NOT")) {
				token = token.getNextCodeSibling();
			}
			
			// skip relational expression
			token = findEndOfRelationalExpression(token);
			
			if (!token.isAnyKeyword("AND", "OR", "EQUIV")) 
				break;

			// continue with next logical expression
			token = token.getNextCodeSibling();
		} while(true);
		
		return token;
	}
	
	public static Token findEndOfRelationalExpression(Token firstTokenOfRelExpr) throws UnexpectedSyntaxException {
		if (firstTokenOfRelExpr == null)
			throw new NullPointerException("firstTokenOfRelExpr");

		Token token = firstTokenOfRelExpr;

		// Predicate Functions 
		// - for Character-Like Arguments, see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenpredicate_functions_strgs
		if (token.textEqualsAny("contains(", "contains_any_of(", "contains_any_not_of(", "matches(")) 
			return token.getNextSibling().getNextCodeSibling();
		// - for Table-Like Arguments, see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenpredicate_functions_tabs	
		if (token.textEqualsAny("line_exists(")) 
			return token.getNextSibling().getNextCodeSibling();

		// read predicative method call, operand of predicate expression, or arithmetic expression  
		Term term1 = Term.createArithmetic(token);
		token = term1.getNextCodeToken();
		
		if (token.isKeyword("IS")) {
			// Predicate Expressions, see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenpredicate_expressions
			token = token.getNextCodeSibling();
			if (token.isKeyword("NOT"))
				token = token.getNextCodeSibling();
			if (token.isAnyKeyword("INITIAL", "BOUND", "ASSIGNED", "SUPPLIED"))
				token = token.getNextCodeSibling();
			else if (token.matchesOnSiblings(true, "INSTANCE", "OF")) {
				token = token.getLastTokenOnSiblings(true, "INSTANCE", "OF", TokenSearch.ANY_IDENTIFIER);
				token = token.getNextCodeSibling();
			} else {
				throw new UnexpectedSyntaxException(token, "predicate expression expected");
			}
			
		} else if (ABAP.isComparisonOperator(token.getText(), true)) { // includes BETWEEN and IN
			// Comparison Expressions (rel_exp), see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenlogexp_comp
			boolean isTernaryOp = token.isComparisonOperator("BETWEEN");
			Term term2 = Term.createArithmetic(token.getNextCodeSibling());
			token = term2.getNextCodeToken();
			if (isTernaryOp) {
				if (!token.isKeyword("AND"))
					throw new UnexpectedSyntaxException(token, "AND expected");
				Term term3 = Term.createArithmetic(token.getNextCodeSibling());
				token = term3.getNextCodeToken();
			}

		} else {
			// assume Predicative Method Call, see https://ldcier1.wdf.sap.corp:44300/sap/public/bc/abap/docu?object=abenpredicative_method_calls
			// - nothing to do
		}
		return token;
	}
	
	public boolean textContainsAnyLetter() {
		char[] chars = text.toCharArray();
		for (char c : chars) {
			if (Character.isLetter(c)) {
				return true;
			}
		}
		return false;
	}

	public final MemoryAccessType getMemoryAccessType() {
		// lazy evaluation
		if (memoryAccessType == MemoryAccessType.UNKNOWN)
			memoryAccessType = determineMemoryAccessType();
		return memoryAccessType; 
	}

	public final void invalidateMemoryAccessType() {
		// use lazy evaluation
		memoryAccessType = MemoryAccessType.UNKNOWN;
	}

	private MemoryAccessType determineMemoryAccessType() {
		// Checks whether the token is at a write (or read-write) position, depending on the ABAP commands; restrictions: 
		// - does not consider declaration positions 
		// - only explicitly considers write / read-write, because read is the default result. 
		// - 'TODO' comments in this method show sections or commands that are not yet considered
		
		// This function is ordered by sections in:
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_statements_overview.htm

		if (!isIdentifier() || this.opensLevel && this.textEndsWith("("))
			return MemoryAccessType.NONE;
		
		Command command = getParentCommand();
		Token firstToken = command.getFirstCodeToken();
		Token prevToken = getPrevCodeToken();
		if (prevToken != null && prevToken.isChainColon())
			prevToken = prevToken.getPrevCodeSibling();
		Token prevPrevToken = (prevToken == null) ? null : prevToken.getPrevCodeSibling();
		Token prevPrevPrevToken = (prevPrevToken == null) ? null : prevPrevToken.getPrevCodeSibling();
		Token nextToken = getEndOfTableExpression().getNextCodeToken();
		if (nextToken == null)
			return MemoryAccessType.NONE;
		if (nextToken.isChainColon())
			nextToken = nextToken.getNextCodeToken();
		
		// inline declarations
		if (prev != null && prev.opensInlineDeclaration()) {
			if (prev.opensInlineDeclarationForFieldSymbol()) 
				return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;

			// adjust previous and next Token
			prevToken = prev.getPrevCodeToken();
			prevPrevToken = (prevToken == null) ? null : prevToken.getPrevCodeSibling();
			prevPrevPrevToken = (prevPrevToken == null) ? null : prevPrevToken.getPrevCodeSibling();
			nextToken = prev.getNextSibling().getNextCodeToken();

			// determine whether this is an inline declaration of a data reference
			if (nextToken != null && nextToken.matchesOnSiblings(true, "=", "REF")) {
				return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
			} else if (prevToken == null) {
				// continue below
			} else if (firstToken.matchesOnSiblings(true, "GET", "REFERENCE") && prevToken.isKeyword("INTO")) {
				return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
			} else if (prevToken.isKeyword("INTO") && prevPrevToken != null && prevPrevToken.isKeyword("REFERENCE")) {
				return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
			}
			
			// otherwise, it is a write to the memory area of a variable
			return MemoryAccessType.WRITE;
		} 
		
		// non-inline declarations
		if (firstToken.isAnyKeyword(Command.declarationKeywords)) {
			if (prevToken.isAnyKeyword(Command.declarationKeywordsReservingMemory) 
					|| firstToken.isAnyKeyword(Command.declarationKeywordsReservingMemory) && prevToken.isComma()) {
				// except for TYPES, the declared identifier is in a write or assignment position
				return ABAP.isFieldSymbol(text) ? MemoryAccessType.ASSIGN_TO_FS_OR_DREF : MemoryAccessType.WRITE;
			} else if (prevToken.isIdentifier() && prevToken.getOpensLevel() && prevPrevToken != null && (prevPrevToken.isAnyKeyword(Command.declarationKeywords) || prevPrevToken.isComma() || prevPrevToken.isChainColon())) { 
				// e.g. "lc_length" in "DATA lv_text(lc_length) VALUE 'abcde'."
				return MemoryAccessType.READ;
			} else if (prevToken.isAnyKeyword("LENGTH", "VALUE")) { // constants can be used for both
				// e.g. "lc_length" and "lc_text" in "DATA lv_text TYPE c LENGTH lc_length VALUE lc_text."
				return MemoryAccessType.READ;
			} else { // e.g. a class name after TYPE REF TO or a type name after TYPES
				return MemoryAccessType.NONE;
			}
		}
			
		// assignments of type 'var = ...' or 'itab[ ... ] = ...' (without inline declarations)
		if (command.isAssignment(false, true) && nextToken.isAssignmentOperator()) {
			// in case of equals sign chaining 'a = b = c = 1.', move to the first receiving variable
			Token firstReceiver = this.getStartOfTableExpression();
			do {
				Token prevAssignmentOp = firstReceiver.getPrevCodeSibling();
				if (prevAssignmentOp == null || !prevAssignmentOp.isAssignmentOperator())
					break;
				Token prevIdentifier = prevAssignmentOp.getPrevCodeSibling();
				if (prevIdentifier == null || !prevIdentifier.isIdentifier())
					break;
				firstReceiver = prevIdentifier.getStartOfTableExpression();
			} while(true);
			if (firstReceiver == command.getFirstCodeToken()) {
				Token nextNext = nextToken.getNextCodeSibling();
				if (nextNext != null && nextNext.isKeyword("REF"))
					return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
				else 
					return MemoryAccessType.WRITE;
			}
		} 

		// avoid having to check prevToken == null in the rest of the method
		if (prevToken == null) { 
			return MemoryAccessType.NONE;
		}

		// assignments to field symbols; REFERENCE INTO dref
		if (firstToken.isKeyword("ASSIGN") && prevToken.isKeyword("TO")) {
			return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
		} else if (firstToken.isAnyKeyword("UNASSIGN", "UNASSIGN:")) {
			return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
		} else if (prevToken.isKeyword("ASSIGNING")) {
			return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
		} else if (firstToken.matchesOnSiblings(true, "GET", "REFERENCE") && prevToken.isKeyword("INTO")) {
			return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
		} else if (prevToken.isKeyword("INTO") && prevPrevToken != null && prevPrevToken.isKeyword("REFERENCE")) {
			return MemoryAccessType.ASSIGN_TO_FS_OR_DREF;
		}

		// Object Creation
		if (firstToken.matchesOnSiblings(true, "CREATE", "DATA|OBJECT") && prevToken.isAnyKeyword("DATA", "OBJECT")) {
			// CREATE DATA dref ...
			// CREATE OBJECT oref ...
			// CREATE OBJECT ole class [NO FLUSH] [QUEUE-ONLY].
			return MemoryAccessType.WRITE;
		} 
		
		// Calling and Exiting Program Units: Calling Processing Blocks
		// - see below (end of this function)
		
		// Program Flow Logic: Exception Handling
		if (firstToken.isAnyKeyword("CATCH", "CLEANUP") && prevToken.isKeyword("INTO")) {
			// [CATCH [BEFORE UNWIND] cx_class1 cx_class2 ... [INTO oref]. 
			// [CLEANUP [INTO oref]. [cleanup_block]] 
			return MemoryAccessType.WRITE;
		} 

		// Assignments
		if (firstToken.isKeyword("MOVE-CORRESPONDING") && prevToken.isKeyword("TO")) {
			// MOVE-CORRESPONDING [EXACT] struc1 TO struc2 [EXPANDING NESTED TABLES] [KEEPING TARGET LINES].
			// MOVE-CORRESPONDING [EXACT] itab1 TO itab2 [EXPANDING NESTED TABLES] [KEEPING TARGET LINES]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("UNPACK") && prevToken.isKeyword("TO")) {
			// UNPACK source TO destination. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "REFERENCE", "OF") && prevToken.isKeyword("INTO")) {
			// GET REFERENCE OF dobj INTO dref. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isAnyKeyword("CLEAR", "CLEAR:", "FREE", "FREE:") && parent == null && !prevToken.isKeyword("WITH")) {
			// CLEAR dobj [ {WITH val [IN {CHARACTER|BYTE} MODE] } | {WITH NULL} ].
			// FREE dobj. 
			return MemoryAccessType.WRITE;
		} 
		
		// Processing Internal Data: Character String and Byte String Processing 
		if (firstToken.isKeyword("CONCATENATE") && prevToken.isKeyword("INTO")) {
			// CONCATENATE {dobj1 dobj2 ...}|{LINES OF itab} INTO result [IN {CHARACTER|BYTE} MODE] [SEPARATED BY sep] [RESPECTING BLANKS]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("CONDENSE") && prevToken.isKeyword("CONDENSE")) {
			// CONDENSE text [NO-GAPS]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "CONVERT", "TEXT") && prevToken.isKeyword("CODE")) { 
			// CONVERT TEXT text INTO SORTABLE CODE hex. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("FIND") && (prevToken.isKeyword("RESULTS") || (prevToken.isAnyKeyword("COUNT", "OFFSET", "LENGTH") && prevPrevToken != null && prevPrevToken.isKeyword("MATCH")))) { 
			// MATCH COUNT mcnt, MATCH OFFSET moff, MATCH LENGTH mlen, RESULTS result_tab|result_wa
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapfind_options.htm
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("FIND")) { 
			// move back until SUBMATCHES keyword may be found
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapfind_options.htm
			Token test = prevToken;
			while (test != null && (!test.isKeyword() || test.opensInlineDeclaration()))
				test = test.getPrevCodeSibling();
			if (test != null && test.isKeyword("SUBMATCHES"))
				return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "BIT") && prevToken.isKeyword("INTO")) {
			// GET BIT bitpos OF byte_string INTO val. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("OVERLAY") && prevToken.isKeyword("OVERLAY")) {
			// OVERLAY text1 WITH text2 [ONLY mask]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("REPLACE")) {
			if (firstToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "INTO")) {
				// obsolete syntax: REPLACE substring WITH new INTO dobj [IN {BYTE|CHARACTER} MODE] [LENGTH len].
				if (prevToken.isKeyword("INTO")) {
					return MemoryAccessType.WRITE;
				}
			} else if (nextToken.isKeyword("WITH") || prevToken.isKeyword("RESULTS") || (prevToken.isAnyKeyword("COUNT", "OFFSET", "LENGTH") && prevPrevToken != null && prevPrevToken.isKeyword("REPLACEMENT"))) { 
				// REPLACE [{FIRST OCCURRENCE}|{ALL OCCURRENCES} OF] pattern IN [section_of] dobj WITH new  [IN {CHARACTER|BYTE} MODE] REPLACEMENT COUNT rcnt, REPLACEMENT OFFSET roff, REPLACEMENT LENGTH rlen
				// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapreplace_options.htm
				return MemoryAccessType.WRITE;
			}

		} else if (firstToken.matchesOnSiblings(true, "SET", "BIT") && prevToken.isKeyword("OF")) { 
			// SET BIT bitpos OF byte_string [TO val]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("SHIFT") && prevToken.isKeyword("SHIFT")) {
			// SHIFT dobj [ {[ places][ direction]} | deleting ] [IN {CHARACTER|BYTE} MODE]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("SPLIT") && prevToken.isKeyword("TABLE")) { 
			// SPLIT ... INTO TABLE result_tab
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("SPLIT")) { 
			// SPLIT ... INTO result1 result2 [...]
			// move back until INTO keyword may be found
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsplit.htm
			Token test = prevToken;
			while (test != null && (!test.isKeyword() || test.opensInlineDeclaration()))
				test = test.getPrevCodeSibling();
			if (test != null && test.isKeyword("INTO"))
				return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("TRANSLATE") && prevToken.isKeyword("TRANSLATE")) {
			// TRANSLATE text {TO {UPPER|LOWER} CASE} | {USING mask}. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("WRITE") && prevToken.isKeyword("TO")) {
			// WRITE {source|(source_name)} TO destination [format_options]. 
			return MemoryAccessType.WRITE;
		}		

		// Processing Internal Data: Date and Time Processing 
		if (firstToken.matchesOnSiblings(true, "CONVERT", "DATE") && prevToken.isAnyKeyword("UTCLONG")) {
			// CONVERT DATE dat TIME tim [FRACTIONAL SECONDS fs] [DAYLIGHT SAVING TIME dst] TIME ZONE tz INTO UTCLONG time_stamp. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "CONVERT", "DATE") && prevToken.isAnyKeyword("STAMP")) {
			// CONVERT DATE dat [TIME tim [DAYLIGHT SAVING TIME dst]] INTO TIME STAMP time_stamp TIME ZONE tz.
			// note that TIME ZONE is NOT a write position, although it is behind the INTO keyword
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "CONVERT", "UTCLONG") && prevToken.isAnyKeyword("DATE", "TIME", "SECONDS", "ZONE")) {
			// CONVERT UTCLONG time_stamp INTO [DATE dat] [TIME tim [FRACTIONAL SECONDS fs]] [DAYLIGHT SAVING TIME dst] TIME ZONE tz. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "CONVERT", "TIME", "STAMP") && prevToken.isAnyKeyword("DATE", "TIME")) {
			 // CONVERT TIME STAMP time_stamp TIME ZONE tz INTO [DATE dat] [TIME tim] [DAYLIGHT SAVING TIME dst]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "TIME") && prevToken.isKeyword("FIELD")) {
			// GET TIME [FIELD tim]. 
			// GET TIME STAMP FIELD time_stamp. 
			return MemoryAccessType.WRITE;
		}

		// Processing Internal Data: Internal Tables 
		// - FIND IN TABLE: see above for FIND
		// - SUM: not considered: this occurs inside AT NEW / AT END OF and changes the wa of the current LOOP  
		// - TODO: SET ASSOCIATION not yet considered
		if (firstToken.isKeyword("APPEND") && prevToken.isAnyKeyword("TO", "INTO")) {
			// APPEND {wa | INITIAL LINE | LINES OF jtab [FROM idx1] [TO idx2] [STEP n] } TO itab [SORTED BY comp] ... { ASSIGNING <fs> [CASTING]} | { REFERENCE INTO dref }.
			if (prevToken.isKeyword("TO") && firstToken.matchesOnSiblings(true, "APPEND", "LINES", "OF") && this.matchesDeep(true, TokenSearch.ASTERISK, "TO")) {
				// this is the idx2 token in 'LINES OF jtab [FROM idx1] [TO idx2] [STEP n]'
				return MemoryAccessType.READ;
			} else {
				return MemoryAccessType.WRITE;
			}
		} else if (firstToken.isKeyword("COLLECT") && prevToken.isKeyword("INTO")) {
			// COLLECT wa INTO itab [result].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DELETE", "TABLE") && prevToken.isKeyword("TABLE")) {
			// DELETE TABLE itab ... (but NOT 'DELETE dtab FROM TABLE @itab'!)
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("DELETE") && prevToken == firstToken) {
			// DELETE itab ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DELETE", "ADJACENT", "DUPLICATES", "FROM") && prevToken.isKeyword("FROM")) {
			// DELETE ADJACENT DUPLICATES FROM itab ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("INSERT") && (prevToken.isKeyword("INTO") || prevPrevToken != null && prevPrevToken.matchesOnSiblings(true, "INTO", "TABLE"))) {
			// INSERT line_spec INTO {{TABLE itab} | {itab INDEX idx} | {itab}} (but NOT 'INSERT dtab FROM TABLE @itab'!) 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "LOOP", "AT") && prevToken.isKeyword("INTO")) {
			// LOOP AT ...  { {INTO wa } | { ASSIGNING <fs> [CASTING] [ELSE UNASSIGN] } | { REFERENCE INTO dref }
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "MODIFY", "TABLE") && prevToken.isAnyKeyword("TABLE", "INTO")) {
			// MODIFY TABLE itab ... (but NOT 'MODIFY dtab FROM TABLE @itab'!)
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("MODIFY") && prevToken == firstToken) {
			// MODIFY itab ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "READ", "TABLE") && prevToken.isKeyword("INTO")) {
			// READ TABLE ...  { {INTO wa } | { ASSIGNING <fs> [CASTING] [ELSE UNASSIGN] } | { REFERENCE INTO dref }
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("REPLACE") && prevToken.isKeyword("TABLE")) {
			// REPLACE [{FIRST OCCURRENCE}|{ALL OCCURRENCES} OF] pattern IN TABLE itab
			// (for [replace_options], see above for "REPLACE")
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("SORT") && prevToken == firstToken) {
			// SORT itab ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DESCRIBE", "FIELD") && prevToken.isAnyKeyword("TYPE", "COMPONENTS", "LENGTH", "DECIMALS", "OUTPUT-LENGTH", "HELP-ID", "MASK")) {
			// DESCRIBE FIELD dobj [TYPE typ [COMPONENTS com]] [LENGTH ilen IN {BYTE|CHARACTER} MODE] [DECIMALS dec] [OUTPUT-LENGTH olen] [HELP-ID hlp] [EDIT MASK mask].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DESCRIBE", "TABLE") && prevToken.isAnyKeyword("KIND", "LINES", "OCCURS")) {
			// DESCRIBE TABLE itab [KIND knd] [LINES lin] [OCCURS n].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DESCRIBE", "DISTANCE", "BETWEEN") && prevToken.isKeyword("INTO")) {
			// DESCRIBE DISTANCE BETWEEN dobj1 AND dobj2 INTO dst IN {BYTE|CHARACTER} MODE.
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "DESCRIBE", "FIELD") && prevToken.isKeyword("INTO")) {
			// DESCRIBE FIELD dobj INTO td.
			return MemoryAccessType.WRITE;
		}
		
		// Processing External Data: ABAP SQL
		if (firstToken.matchesOnSiblings(true, "CLOSE", "CURSOR") && prevToken.isKeyword("CURSOR")) {
			// CLOSE CURSOR @dbcur.
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "FETCH", "NEXT", "CURSOR") && prevToken.isAnyKeyword("INTO", "OF", "TABLE", "RESULT")) {
			// FETCH NEXT CURSOR dbcur { { INTO ( elem1, elem2,  ...) } | { INTO [CORRESPONDING FIELDS OF] wa [indicators] } | { INTO|APPENDING [CORRESPONDING FIELDS OF] TABLE itab [indicators] [PACKAGE SIZE n] } }
			//   EXTENDED RESULT @oref 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "OPEN", "CURSOR") && prevToken.isAnyKeyword("CURSOR", "HOLD")) {
			// OPEN CURSOR [WITH HOLD] @dbcur|@DATA(dbcur) FOR ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.isAnyKeyword("SELECT", "WITH")) {
			Token parentPrev = (parent != null && parent.textEquals("(")) ? parent.getPrevCodeToken() : null;
			boolean isParentPrevInto = (parentPrev != null && parentPrev.isKeyword("INTO"));
			if (prevToken.isAnyKeyword("INTO", "OF", "TABLE", "RESULT") || isParentPrevInto) {
				// SELECT ... { { INTO ( elem1, elem2,  ...) } | { INTO [CORRESPONDING FIELDS OF] wa [indicators] } | { INTO|APPENDING [CORRESPONDING FIELDS OF] TABLE itab [indicators] [PACKAGE SIZE n] } }
				//   EXTENDED RESULT @oref 
				// WITH: cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwith.htm
				return MemoryAccessType.WRITE;
			} else if (prevToken.isKeyword("NEW") && (prevPrevToken != null && prevPrevToken.isAnyKeyword("INTO", "OF", "TABLE") || isParentPrevInto)) {
				// SELECT .. .INTO|APPENDING [CORRESPONDING FIELDS OF] [TABLE] { NEW @dref | NEW @DATA(dref) | NEW @FINAL(dref) }
				// SELECT ... INTO ( { NEW @dref1 | NEW @DATA(dref1) | NEW @FINAL(dref1) }, ... )
				// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapselect_into_target.htm
				return MemoryAccessType.WRITE;
			}
		}
		
		// Processing External Data: ABAP and HANA
		// see below for CALL DATABASE PROCEDURE ... [EXPORTING  p1 = a1 p2 = a2 ...] [IMPORTING  p1 = a1 p2 = a2 ...].

		// Processing External Data: Data Clusters
		if (firstToken.isKeyword("IMPORT") && (prevToken.textEquals("=") || prevToken.isKeyword("TO"))) {
			// IMPORT { {p1 = dobj1 p2 = dobj2 ...} | {p1 TO dobj1  p2 TO dobj2 ...} | (ptab) ... } FROM ...
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "IMPORT", "DIRECTORY", "INTO") && prevToken.isAnyKeyword("INTO", "TO")) {
			// IMPORT DIRECTORY INTO itab FROM DATABASE dbtab(ar) [TO wa] [CLIENT cl] ID id.
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("EXPORT") && prevPrevPrevToken != null && prevPrevPrevToken.isKeyword("TO") && 
				(prevPrevToken.isKeyword("DATA") && prevToken.isKeyword("BUFFER") || prevPrevToken.isKeyword("INTERNAL") && prevToken.isKeyword("TABLE"))) {
			// EXPORT parameter_list TO { DATA BUFFER xstr | INTERNAL TABLE itab | ... }
			return MemoryAccessType.WRITE;
		}

		// Processing External Data: File Interface
		if (firstToken.matchesOnSiblings(true, "GET", "DATASET") && prevToken.isAnyKeyword("POSITION", "ATTRIBUTES")) {
			// GET DATASET dset [POSITION pos] [ATTRIBUTES attr].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "READ", "DATASET") && prevToken.isAnyKeyword("INTO", "LENGTH")) {
			// READ DATASET dset INTO dobj [MAXIMUM LENGTH mlen] [[ACTUAL] LENGTH alen].
			return MemoryAccessType.WRITE;
		} 
		
		// ABAP for RAP Business Objects
		if (firstToken.matchesOnSiblings(true, "COMMIT", "ENTITIES") && prevToken.isAnyKeyword("FAILED", "REPORTED")) {
			// COMMIT ENTITIES ...  [FAILED failed_resp] [REPORTED reported_resp]
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "PERMISSIONS") && prevToken.isAnyKeyword("RESULT", "FAILED", "REPORTED")) {
			// GET PERMISSIONS ... RESULT result_tab [FAILED failed_resp] [REPORTED reported_resp]
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "MODIFY", "ENTITY|ENTITIES") && prevToken.isAnyKeyword("RESULT", "FAILED", "MAPPED", "REPORTED")) {
			// MODIFY ENTITY, ENTITIES ...  [FAILED failed_resp] [REPORTED reported_resp]
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "MODIFY", "ENTITY|ENTITIES") && prevToken.isAnyKeyword("FROM", "WITH")) {
			// MODIFY ENTITY, ENTITIES ...  FROM fields_tab / AUTO FILL CID WITH fields_tab /  
			// [AUTO FILL CID] FIELDS ( comp1 comp2 ... ) WITH fields_tab / [AUTO FILL CID] SET FIELDS WITH fields_tab:
			// A syntax check on static read-only fields is not possible for all variants, therefore using READ_WRITE to prevent 
			// introduction of FINAL for the fields_tab
			return MemoryAccessType.READ_WRITE;
		} else if (firstToken.matchesOnSiblings(true, "READ", "ENTITY|ENTITIES") && prevToken.isAnyKeyword("RESULT", "FAILED", "REPORTED")) {
			// READ ENTITY, ENTITIES ... [FAILED failed_resp] [REPORTED reported_resp]
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "SET", "LOCKS") && prevToken.isAnyKeyword("FAILED", "REPORTED")) {
			// SET LOCKS ENTITY bdef FROM inst [FAILED failed_resp] [REPORTED reported_resp]
			return MemoryAccessType.WRITE;
		}		 
		
		// Program Parameters
		if (firstToken.matchesOnSiblings(true, "GET", "PARAMETER") && prevToken.isKeyword("FIELD")) { 
			// GET PARAMETER ID pid FIELD dobj. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "LOCALE") && prevToken.isAnyKeyword("LANGUAGE", "COUNTRY", "MODIFIER")) {
			// GET LOCALE LANGUAGE lang obsolete_parameters. 
			return MemoryAccessType.WRITE;
		}
		
		// Program Editing
		if (firstToken.matchesOnSiblings(true, "GET", "RUN", "TIME") && prevToken.isKeyword("FIELD")) {
			// GET RUN TIME FIELD rtime. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "READ", "REPORT|TEXTPOOL") && prevToken.isKeyword("INTO")) {
			// READ REPORT prog INTO itab [MAXIMUM WIDTH INTO wid]. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "SYNTAX-CHECK", "FOR") && prevToken.isAnyKeyword("MESSAGE", "LINE", "WORD", "INCLUDE", "OFFSET", "MESSAGE-ID")) {
			// SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd [PROGRAM prog] [DIRECTORY ENTRY dir] [WITH CURRENT SWITCHSTATES] [INCLUDE incl] [OFFSET off] [MESSAGE-ID mid]. 
			return MemoryAccessType.WRITE;
		}		
		
		// ABAP Data and Communication Interfaces: Remote Function Call
		// - CALL FUNCTION DESTINATION and RECEIVE RESULTS FROM FUNCTION: see below
		if (firstToken.matchesOnSiblings(true, "CALL", "TRANSFORMATION") && (prevToken.isKeyword("XML") || prevToken.textEquals("=")) && !(isAttached() && prev.textEquals("("))) {
			// CALL TRANSFORMATION ID | trans|(name) 
	      //    [PARAMETERS {p1 = e1 p2 = e2 ...}|(ptab)] 
	      //    [OPTIONS options] 
	      //    SOURCE {XML src_xml}  | {{bn1 = e1 bn2 = e2 ...}|(stab)} 
	      //    RESULT {XML rslt_xml} | {{bn1 = f1 bn2 = f2 ...}|(rtab)}.
			Token testToken = prevToken;
			while (testToken != null && !testToken.isAnyKeyword("PARAMETERS", "OPTIONS", "SOURCE", "RESULT")) {
				testToken = testToken.getPrevCodeSibling();
			}
			if (testToken != null && testToken.isKeyword("RESULT")) {
				return MemoryAccessType.WRITE;
			}
		}

		// ABAP Data and Communication Interfaces: OLE Interface
		// - CREATE OBJECT ole class ...: already covered by "CREATE OBJECT" above
		// - FREE OBJECT ole [NO FLUSH]: it seems that 'ole' is not written to
		if ((firstToken.matchesOnSiblings(true, "CALL", "METHOD", "OF") 
   		|| firstToken.matchesOnSiblings(true, "GET", "PROPERTY", "OF")) && prevToken.textEquals("=")) {
			// CALL METHOD OF ole meth [= rc] [EXPORTING p1 = f1 p2 = f2 ...] [NO FLUSH] [QUEUE-ONLY].
			// GET PROPERTY OF ole prop = dobj [NO FLUSH] [QUEUE-ONLY] [EXPORTING p1 = f1 p2 = f2 ...].
			// token is only in a write position if the keyword EXPORTING is NOT found before it 
			Token testToken = prevToken;
			while (testToken != null && !testToken.isKeyword("EXPORTING")) {
				testToken = testToken.getPrevCodeSibling();
			}
			if (testToken == null) {
				return MemoryAccessType.WRITE;
			}
		} 

		// User Dialogs: Dynpros
		if (firstToken.matchesOnSiblings(true, "GET", "CURSOR") && prevToken.isAnyKeyword("FIELD", "VALUE", "LENGTH", "OFFSET", "LINE", "AREA")) {
			// GET CURSOR { { FIELD  field [VALUE val] [LENGTH len] [OFFSET off] [LINE lin] [AREA area] } | { LINE line } }.
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "GET", "PF-STATUS") && prevToken.isAnyKeyword("PF-STATUS", "PROGRAM", "EXCLUDING")) {
			// GET PF-STATUS status [PROGRAM prog] [EXCLUDING fcode].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "LOOP", "AT", "SCREEN", "INTO") && prevToken.isKeyword("INTO")) {
			// LOOP AT SCREEN INTO wa (this branch is probably never reached, because it is already handled with LOOP AT ... INTO)
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "REFRESH", "CONTROL") && prevToken.isKeyword("CONTROL")) {
			// REFRESH CONTROL contrl FROM SCREEN dynnr.
			return MemoryAccessType.WRITE;
		}		
		
		// User Dialogs: Lists
		if (firstToken.matchesOnSiblings(true, "DESCRIBE", "LIST") && prevToken.isAnyKeyword("LINES", "PAGES", "LINE", "PAGE", "LINE-SIZE", "LINE-COUNT", "FIRST-LINE", "TOP-LINES", "TITLE-LINES", "HEAD-LINES", "END-LINES", "INDEX")) {
			// DESCRIBE LIST { {NUMBER OF  {LINES|PAGES} n} | {LINE linno PAGE page} | {PAGE pagno [LINE-SIZE width] [LINE-COUNT page_lines] [LINES lines]
			//   [FIRST-LINE first_line] [TOP-LINES top_lines] [TITLE-LINES title_lines] [HEAD-LINES header_lines] [END-LINES footer_lines] } } [INDEX idx].
			return MemoryAccessType.WRITE;
		} else if (firstToken.matchesOnSiblings(true, "READ", TokenSearch.makeOptional("CURRENT"), "LINE") && prevToken.isKeyword("INTO")) {
			// READ { {LINE line [{OF PAGE page}|{OF CURRENT PAGE}] [INDEX idx]} | {CURRENT LINE} } 
			//   [LINE VALUE INTO wa] [FIELD VALUE dobj1 [INTO wa1] dobj2 [INTO wa2] ...].
			return MemoryAccessType.WRITE;
		} 
		
		// User Dialogs: Messages
		if (firstToken.isKeyword("MESSAGE") && prevToken.isKeyword("INTO")) {
			// MESSAGE { msg  | text  } ... INTO text. 
			return MemoryAccessType.WRITE;
		}
		
		// Enhancements
		// - CALL BADI ...: see below
		if (firstToken.matchesOnSiblings(true, "GET", "BADI") && prevToken.isKeyword("BADI")) {
			// GET BADI badi ...
			return MemoryAccessType.WRITE;
		} 
		
		// TODO: further Obsolete Statements missing (except those already covered below)

		// Obsolete Statements: Obsolete Assignments
		if (firstToken.isKeyword("MOVE") && prevToken.isAnyKeyword("TO", "?TO")) {
			// MOVE {[EXACT] source  TO destination} | {source ?TO destination}. 
			return MemoryAccessType.WRITE;
		} else if (firstToken.isKeyword("MOVE") && (prevToken.isChainColon() || prevToken.isComma())) {
			// MOVE [EXACT] source TO: destination1, destination2, ...
			Token chainColon = command.getFirstToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "TO", ":");
			if (chainColon != null) {
				return MemoryAccessType.WRITE;
			}
		} else if (firstToken.isKeyword("PACK") && prevToken.isKeyword("TO")) {
			// PACK source TO destination. 
			return MemoryAccessType.WRITE;
		}		
		
		// Obsolete Statements: Obsolete Calculation Statements
		if (firstToken.isKeyword("COMPUTE") && prevToken.isAnyKeyword("COMPUTE", "EXACT")) { 
			// COMPUTE [EXACT] lhs =|?= rhs.
			return MemoryAccessType.WRITE;
		} else if (firstToken.isAnyKeyword("ADD", "ADD-CORRESPONDING") && prevToken.isKeyword("TO")) {
			// ADD dobj1 TO dobj2.
			// ADD-CORRESPONDING      struc1 TO struc2.
			return MemoryAccessType.WRITE;
		} else if (firstToken.isAnyKeyword("SUBTRACT", "SUBTRACT-CORRESPONDING") && prevToken.isKeyword("FROM")) {
			// SUBTRACT dobj1 FROM dobj2.
			// SUBTRACT-CORRESPONDING struc1 FROM struc2.
			return MemoryAccessType.WRITE;
		} else if (firstToken.isAnyKeyword("MULTIPLY", "DIVIDE", "MULTIPLY-CORRESPONDING", "DIVIDE-CORRESPONDING") && prevToken == firstToken) {
			// MULTIPLY dobj2 BY dobj1.
			// DIVIDE   dobj2 BY dobj1.
			// MULTIPLY-CORRESPONDING struc1 BY struc2.
			// DIVIDE-CORRESPONDING   struc1 BY struc2.
			return MemoryAccessType.WRITE;
		} 
		
		// ----------------------------------------------------------------------
		
		// actual parameter in a method or function call, applicable for:
		// - Calling and Existing Program Units: Calling Processing Blocks
		// - ABAP Data and Communication Interfaces: Remote Function Call
		// - Processing External Data: ABAP and HANA
		// - Enhancements: Enhancements Using BAdIs
		// TODO: CALL TRANSFORMATION missing
		Token parentToken = getParent();
		if (prevToken.textEquals("=") && 
				(  firstToken.matchesOnSiblings(true, "CALL", "METHOD|FUNCTION|BADI")
				|| firstToken.matchesOnSiblings(true, "CALL", "DATABASE", "PROCEDURE")
				|| firstToken.matchesOnSiblings(true, "RECEIVE", "RESULTS", "FROM", "FUNCTION")
				|| (parentToken != null && parentToken.textEndsWith("(") && !parentToken.textEquals("(")))) {
			// for method calls, find corresponding access token
			Token accessKeyword = prevToken;
			while (accessKeyword != null && !accessKeyword.isAnyKeyword("EXPORTING", "IMPORTING", "TABLES", "CHANGING", "RECEIVING", "EXCEPTIONS")) {
				accessKeyword = accessKeyword.getPrevCodeSibling();
			}
			// if no access keyword is found, we use the default result .READ below
			if (accessKeyword != null) {
				if (accessKeyword.isAnyKeyword("EXPORTING")) {
					return MemoryAccessType.READ;
				} else if (accessKeyword.isAnyKeyword("IMPORTING", "RECEIVING")) {
					return MemoryAccessType.WRITE;
				} else if (accessKeyword.isAnyKeyword("CHANGING", "TABLES")) { 
					return MemoryAccessType.READ_WRITE;
				}
			}
		}

		// actual parameter in a PERFORM call
		if (firstToken.isKeyword("PERFORM")) {
			// find corresponding access token
			Token accessKeyword = prevToken;
			while (accessKeyword != null && !accessKeyword.isAnyKeyword("TABLES", "USING", "CHANGING")) {
				accessKeyword = accessKeyword.getPrevCodeSibling();
			}
			// if no access keyword is found, we use the default result .READ below
			if (accessKeyword != null) {
				// variables after USING are in a read position, but the syntax check does not prevent them to be changed inside the FORM 
				if (accessKeyword.isKeyword("USING"))
					return MemoryAccessType.READ_WRITE_POSSIBLE;
				else if (accessKeyword.isAnyKeyword("TABLES", "CHANGING")) 
					return MemoryAccessType.READ_WRITE;
			}
		}
		
		return MemoryAccessType.READ_OR_NONE;
	}

	/**
	 * If this Token starts a list of parameters or components, the corresponding end Token is returned; otherwise null.
	 * @return
	 */
	public Token getEndOfParamsOrComponentsList() {
		if (!getOpensLevel() || !hasChildren() || isLiteral()) {
			return null;
		} else if (next.isAttached() && (next.isIdentifier() || next.isLiteral() || next.textEqualsAny("*", "**"))) {
			// e.g. DATA(lv_variable) or lv_any(5) or lv_any+5(*) or ULINE AT /10(**).
			return null; 
		}
		
		boolean isSqlOperation = parentCommand.isAbapSqlOperation();
		if (textEqualsAny("boolc(", "boolx(", "xsdbool(")) {
			// skip built-in functions for logical expressions; also skip "boolx( bool = log_exp bit = bit )", 
			// because parameters that expect a log_exp are not yet considered in AlignParametersRule. 
			return null;

			// By contrast, NO need to skip the other ABAP.builtInFunctionsWithoutNamedParams, because they have the same 
			// syntax as a method call "any_method( arg )" that omits the parameter name. After excluding log_exp, the  
			// argument could be of type table expression, numeric, string / byte string, or table, e.g.
			// line_index( table_exp ), sin( arg ), strlen( arg ), lines( itab ). 
			// Also note that a class may define methods with the same name as a built-in function; in such a case,   
			// the built-in function is 'shadowed' (even if the method has a different signature) and the method is called.

		} else if (isSqlOperation && isAnyKeyword("OVER(", "HIERARCHY(", "HIERARCHY_SIBLINGS(", "HIERARCHY_ANCESTORS(", "HIERARCHY_DESCENDANTS(", "HIERARCHY_ANCESTORS_AGGREGATE(", "HIERARCHY_DESCENDANTS_AGGREGATE(")) {
			// skip win_func OVER( ... ) and SELECT FROM hierarchy_data( ... ) 
			return null;

		} else if (isSqlOperation && textEqualsAny(ABAP.abapSqlFunctions)) {
			// skip ABAP SQL table functions, unless they use assignments to formal parameters
			return textEqualsAny(ABAP.abapSqlFunctionsWithAssignments) ? getNextCodeSibling() : null;
			
		} else if (isSqlOperation && textEquals("@(")) {
			// skip host expression
			return null;
			
		} else if (isSqlOperation && textEndsWith("[")) {
			// skip ABAP SQL path expressions
			return null;
			
		} else if (textEquals("(")) {
			// stand-alone " ( " only starts a component list in case of LOOP AT ... GROUP BY ( ... ) or if it is a row  
			// inside a VALUE or NEW constructor expression, not if it is part of arithmetic expressions etc.
			
			Token prev = getPrevCodeSibling();  
			if (parent == null && this.opensGroupKey()) {
				// LOOP AT ... GROUP BY ( key1 = dobj1 key2 = dobj2 ... [gs = GROUP SIZE] [gi = GROUP INDEX] ) ...
				return getNextCodeSibling();
			} else if (isSqlOperation && prev != null && prev.isKeyword()) {
				// e.g. SELECT FROM ( ... ) ... JOIN ( ... ), MODIFY ... FROM ( SELECT ...),
				// GROUP BY GROUPING SETS ( ... ), UNION ALL|INTERSECT ( SELECT ... ),
				// WHERE ... IN ( SELECT ... ), WHERE ... IN ( ..., ... ), WHERE ... EXISTS ( SELECT ... ),
				// WHERE operand {=|...} ALL|ANY|SOME ( SELECT ... ), INTO ( ...,  ... )
				return null;
			} else if (parent == null) {
				// otherwise, stand-alone " ( " on top level must be part of an arithmetic expression
				return null;
			} else if (parent.textEquals("(") || !parent.textEndsWith("(")) {
				return null;
			} 
			Token parentPrev = parent.getPrevCodeToken();
			if (parentPrev == null || !parentPrev.isAnyKeyword("VALUE", "NEW")) {
				return null;
			} 
			// the stand-alone " ( " may still be part of an arithmetic expression, as in "VALUE #( param = a * ( b + c ) )";
			// such brackets must be preceded by " = " or an arithmetic operator or by keywords like 
			// - "UNTIL" / "WHILE" (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_conditional.htm) or 
			// - "WHERE (see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_conditional.htm)
			// The above conditions already excluded the case " ( ( ".  
			// do NOT use getPrevCodeToken() here, as it may return the "#(" of the VALUE or NEW constructor, which has TokenType.OTHER_OP
			if (prev != null && !prev.textEquals(")") && (prev.isAssignmentOperator() || prev.isComparisonOperator() || prev.isOtherOp() || prev.isAnyKeyword("UNTIL", "WHILE", "WHERE"))) {
				return null;
			}

		} else {
			// as the token opens a level but is not a stand-alone "(", it must be a method call or a constructor expression,  
			// because the Tokens "method_name(", "VALUE #(" etc. have text immediately attached to the top level parenthesis
			Token prev = getPrevCodeToken();
			if (prev != null && prev.isAnyKeyword("NEW", "VALUE")) {
				// continue below
			} else if (prev != null && prev.isAnyKeyword(ABAP.constructorOperators)) {
				// do not align inside these constructor expressions (exception: NEW and VALUE, see above)
				return null;
			}
		}
		return getNextSibling();
	}
	
	public boolean opensGroupKey() {
		if (!textEquals("(") || !hasChildren() || next.isAttached())
			return false;
		// LOOP AT ... GROUP BY ( key1 = dobj1 key2 = dobj2 ... [gs = GROUP SIZE] [gi = GROUP INDEX] ) ...
		Token prev = getPrevCodeSibling(); 
		Token prevPrev = (prev == null) ? null : prev.getPrevCodeSibling();
		return prevPrev != null && prevPrev.isKeyword("GROUP") && prev.isKeyword("BY") && parentCommand.firstCodeTokenIsKeyword("LOOP");
	}

	/**
	 * If this Token starts a logical expression, the last code Token of the logical expression is returned; otherwise null.
	 * @return
	 */
	public Token getLastTokenOfLogicalExpression() {
      if (parentCommand.isDdlOrDcl()) {
      	return getLastTokenOfDdlLogicalExpression();
      }

      Token firstCode = parentCommand.getFirstCodeToken();
      if (firstCode == null)
      	return null;
      
      // 1. non-SQL cases on top level (parent == null)
      if (this == firstCode && firstCode.isAnyKeyword("IF", "ELSEIF", "CHECK", "WHILE")) {
      	return parentCommand.getLastNonCommentToken().getPrevCodeToken();

      } else if (isKeyword("WHERE") && parent == null && firstCode.matchesOnSiblings(true, "LOOP AT|MODIFY|DELETE|FOR") 
      		&& !parentCommand.isAbapSqlOperation()) { // for ABAP SQL, see below
         // LOOP AT ... WHERE <logical_expression> [GROUP BY ...].
         // MODIFY itab ... WHERE log_exp ... etc. 
         Token end = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "GROUP BY|USING KEY|TRANSPORTING|FROM");
         if (end == null) {
            return parentCommand.getLastCodeToken().getPrevCodeToken();
         } else if (end.isAnyKeyword("BY", "KEY")) { // if "GROUP BY" or "USING KEY" was found
            return end.getPrevCodeSibling().getPrevCodeToken();
         } else {
         	return end.getPrevCodeToken();
         }

      } else if (isKeyword("UNTIL") && parent == null && firstCode.isKeyword("WAIT")) {
         // WAIT FOR ASYNCHRONOUS TASKS [MESSAGING CHANNELS] [PUSH CHANNELS] UNTIL log_exp [UP TO sec SECONDS].
         Token end = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "UP", "TO");
         if (end == null) {
            return parentCommand.getLastCodeToken().getPrevCodeToken();
         } else {
            return end.getPrevCodeSibling().getPrevCodeToken(); // previous to "UP"
         }

      } else if (firstCode.isKeyword("ASSERT")) {
      	// ASSERT [ [ID group [SUBKEY sub]] [FIELDS val1 val2 ...] CONDITION ] log_exp.
      	if (isKeyword("CONDITION")) {
      		return parentCommand.getLastCodeToken().getPrevCodeToken();
      	} else if (this == firstCode && !firstCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "CONDITION")) {
      		return parentCommand.getLastCodeToken().getPrevCodeToken();
      	} else {
      		return null;
      	}
      } 
      
      // 2. xsdbool( ... ) and boolc( ... )
      if (textEqualsAny("xsdbool(", "boolc(")) {
      	return getNextSibling().getPrevCodeToken();
      }
      
      
      // 3. WHEN / UNTIL / WHILE / WHERE clauses inside constructor expressions
      // the following must be aligned with Command.finishBuild()!
      Token ctorKeyword = (parent == null) ? null : parent.getPrevCodeSibling();
      if (ctorKeyword == null || !ctorKeyword.isKeyword()) {
      	// continue below
      
      } else if (isAnyKeyword("WHEN") && ctorKeyword.isAnyKeyword("COND")) {
      	// "FOR, Iteration Expressions", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor.htm, context:
      	// - COND type( [let_exp] WHEN log_exp THEN ... WHEN .. )
      	Token thenToken = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "THEN");
      	return (thenToken == null) ? null : thenToken.getPrevCodeToken();
      
      } else if (isAnyKeyword("UNTIL", "WHILE") && ctorKeyword.isAnyKeyword("REDUCE", "NEW", "VALUE")) {
      	// "FOR, Conditional Iteration", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_conditional.htm, context:
         // - REDUCE|NEW|VALUE identifier( ... FOR var = rhs [THEN expr] UNTIL|WHILE log_exp [let_exp] ... ) 
      	Token letNextOrFor = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "LET|NEXT|FOR");
      	return (letNextOrFor == null) ? null : letNextOrFor.getPrevCodeToken();

      } else if (isKeyword("WHERE") && ctorKeyword.isAnyKeyword("FILTER")) {
      	// FILTER type( itab [EXCEPT] [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
      	// FILTER type( itab [EXCEPT] IN ftab [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
        	return parent.getNextSibling().getPrevCodeToken();
         
      } else if (isKeyword("WHERE") && ctorKeyword.isAnyKeyword("REDUCE", "NEW", "VALUE") && getNextCodeSibling() != null && getNextCodeSibling().textEquals("(")) {
         // "FOR, Table Iterations", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_itab.htm
      	// Context:
      	// - VALUE|NEW type( [let_exp] [BASE itab] [FOR for_exp1 FOR for_exp2 ... ] ( line_spec1 ) ( line_spec2 ) ... ) ...
      	// - REDUCE type( [let_exp] INIT ... FOR for_exp1 FOR for_exp2 ... NEXT ... ) 
      	// FOR clause:
      	// - FOR wa|<fs> IN itab [INDEX INTO idx] [ cond] [let_exp]
      	// - FOR GROUPS [group|<group>] OF wa|<fs> IN itab [INDEX INTO idx] [cond] GROUP BY group_key [ASCENDING|DESCENDING [AS TEXT]] [WITHOUT MEMBERS] [let_exp] ...
      	// - FOR { wa|<fs> IN GROUP group [INDEX INTO idx] [ WHERE ( log_exp )] }
      	//     | { GROUPS OF wa|<fs> IN GROUP group [INDEX INTO idx] [ WHERE ( log_exp )] GROUP BY group_key [ASCENDING|DESCENDING [AS TEXT]] [WITHOUT MEMBERS] } [ let_exp] ...
      	// Condition [cond]: in this case, the condition is in parentheses:
      	// - ... [USING KEY keyname] [FROM idx1] [TO idx2] [STEP n] [WHERE ( log_exp )|(cond_syntax)] ...
         return getNextCodeSibling().getNextSibling();
      }
      
      // 4. ABAP SQL conditions (sql_cond)
      if (parentCommand.isAbapSqlOperation()) {
      	if (isKeyword("WHEN")) {
      		Token thenToken = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "THEN"); 
      		return (thenToken == null) ? null : thenToken.getPrevCodeToken();
      		
      	} else if (isAnyKeyword("WHERE", "HAVING", "ON")) {
      		// find the next keyword that does NOT fit into a sql condition (but instead starts a new clause, join, case etc.)
      		Token end = getNextCodeSibling();
      		while (end != null && !end.isPeriod()) {
      			if (!end.isKeyword()) {
      				// the condition can only end with a keyword, a period or a closing parenthesis (see while condition above)
   				} else if (end.isAnyKeyword("NOT", "AND", "OR")) {
   					// sql_cond -> rel_exp | [NOT] sql_cond [AND|OR sql_cond] ...
   				} else if (end.isAnyKeyword("IS", "BETWEEN", "IN", "LIKE")) {
   					// operand IS [NOT] NULL, operand IS [NOT] INITIAL
      				// however, EQ, NE etc. as well as BETWEEN, IN, LIKE should be categorized as TokenType.COMPARISON_OP
      			} else if (end.isAnyKeyword("ALL", "ANY", "SOME")) {
      				// operand1 {=|EQ|<>|NE|>|GT|<|LT|>=|GE|<=|LE} [ALL|ANY|SOME] ( SELECT subquery_clauses ... )
   				} else if (end.isAnyKeyword("NULL", "INITIAL", "ESCAPE")) {
   					// operand IS [NOT] NULL, operand IS [NOT] INITIAL, operand1 [NOT] LIKE operand2 [ESCAPE esc]
   				} else if (end.isAnyKeyword("EXISTS")) {
   					// EXISTS ( SELECT subquery_clauses ... )
   				} else if (end.isAnyKeyword("CASE", "WHEN", "THEN", "ELSE", "END")) {
   					// CASE as an sql expression at operand position
   				} else if (end.isAnyKeyword(ABAP.abapSqlAggregateFunctions)) { // or .abapSqlFunctions?
   					// sql_agg
   				} else if (end.isAnyKeyword(ABAP.constructorOperators)) {
   					// if constructor operators could ever be used, they would be part of the logical expression
   				} else {
   					break;
      			}
      			// the logical expression may be at the end of parentheses, 
      			// e.g. the ON condition in 'SELECT FROM ( dtab1 AS t1 INNER JOIN dtab2 AS t2 ON t2~d = t1~d ) ...'
      			if (end.getNextCodeSibling() == null) 
      				return end;
      			end = end.getNextCodeSibling();
      		}
      		// check whether the end meets the expectations, otherwise ignore this logical expressions by returning null
      		if (end != null && !end.isPeriod()) {
      			if (end.startsDdlUnionEtc()) {
      				// end of SELECT
      			} else if (end.isAnyKeyword("FIELDS", "FOR", "WHERE", "GROUP", "HAVING", "ORDER", "INTO", "APPENDING")) {
      				// next SQL clause
      			} else if (end.isAnyKeyword("USING", "UP", "%_HINTS", "PRIVILEGED", "OFFSET", "BYPASSING", "CONNECTION")) {
      				// USING CLIENT / DB hints / ABAP options
      			} else if (end.isAnyKeyword("INNER", "LEFT", "RIGHT", "JOIN", "CROSS", "ON", "CLIENT")) {
      				// next join, or ON condition from surrounding join without parentheses 
   				} else if (end.isAnyKeyword("SOURCE", "CHILD", "PERIOD", "SIBLINGS", "DEPTH", "MULTIPLE", "ORPHANS", "CYCLES", "LOAD", "GENERATE", "DISTANCE", "MEASURES", "WHERE", "WITH")) {
   					// additions of HIERARCHY( ... ), HIERARCHY_DESCENDANTS|HIERARCHY_ANCESTORS|HIERARCHY_SIBLINGS( ... ),
   					// HIERARCHY_ANCESTORS_AGGREGATE( ... )
      			} else {
      				if (Program.showDevFeatures()) 
      					throw new IllegalArgumentException("Unexpected end '" + end.getText() + "' of SQL condition: " + parentCommand.toString());
      				// ignore this unexpected syntax by returning null as if no logical expression was found
      				end = null;
      			}
      		}
      		return (end == null) ? null : end.getPrevCodeToken();
      	}
      }
      return null;
	}
	
	/**
	 * If this Token starts a logical expression in DDL, the last code Token of the logical expression is returned; otherwise null.
	 * @return
	 */
	public Token getLastTokenOfDdlLogicalExpression() {
      Token firstCode = parentCommand.getFirstCodeToken();
      if (firstCode == null)
      	return null;
      Token prevCode = getPrevCodeToken();
      
   	if (isKeyword("WHEN")) {
   		Token thenToken = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "THEN"); 
   		return (thenToken == null) ? null : thenToken.getPrevCodeToken();
   		
   	} else if (parent != null && parent.textEquals(DDL.BRACKET_OPEN_STRING) && !parentCommand.isDdlAnnotation()
   			&& (    textEquals(DDL.COLON_SIGN_STRING) && getNextSibling() != null && !getNextSibling().isAnyKeyword("INNER", "LEFT", "WHERE")
   			     || isAnyKeyword("INNER", "OUTER") && getNextSibling() != null && !getNextSibling().isKeyword("WHERE")
   				  || isKeyword("WHERE"))) {

   		// path expression: ... [ [cardinality] [INNER|{LEFT OUTER}] [WHERE] [cds_cond] ] ...
   		return parent.getNextCodeSibling().getPrevCodeToken();
   		
   	} else if (isAnyKeyword("WHERE", "HAVING") 
   			|| isKeyword("ON") && (prevCode == null || !prevCode.isKeyword("PROJECTION")) // exclude "AS PROJECTION ON"
   			|| isKeyword("FILTER") && prevCode != null && prevCode.isKeyword("DEFAULT")) { // "ASSOCIATION ... WITH DEFAULT FILTER"

   		// find the next keyword that does NOT fit into a DDL condition (but instead starts a new clause, join, case, list element etc.)
   		Token end = getNextCodeSibling();
   		while (end != null) {
   			if (end.textEqualsAny(DDL.BRACE_OPEN_STRING, DDL.COMMA_SIGN_STRING, DDL.SEMICOLON_SIGN_STRING)) {
   				break; // start of select list or end of list element
   			} else if (!end.isKeyword()) {
   				// the condition can only end with a keyword or with , ; { (see while condition above)
				} else if (end.isAnyKeyword("NOT", "AND", "OR")) {
					// sql_cond -> rel_exp | [NOT] sql_cond [AND|OR sql_cond] ...
				} else if (end.isAnyKeyword("IS", "BETWEEN", "IN", "LIKE")) {
					// operand IS [NOT] NULL, operand IS [NOT] INITIAL
   				// however, EQ, NE etc. as well as BETWEEN, IN, LIKE should be categorized as TokenType.COMPARISON_OP
				} else if (end.isAnyKeyword("NULL", "INITIAL", "ESCAPE")) {
					// operand IS [NOT] NULL, operand IS [NOT] INITIAL, operand1 [NOT] LIKE operand2 [ESCAPE esc]
				} else if (end.isAnyKeyword("CASE", "WHEN", "THEN", "ELSE", "END")) {
					// CASE as a DDL expression at operand position
				} else if (end.isAnyKeyword(DDL.aggregationFunctions) && isKeyword("HAVING")) { 
					// aggregate functions are only allowed in HAVING (and WHEN ... THEN), but not in WHERE / ON / DEFAULT FILTER, 
					// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abencds_cond_expr_operands_v2.htm
				} else {
					break;
   			}
   			end = end.getNextCodeSibling();
   		}
   		// check whether the end meets the expectations, otherwise ignore this logical expressions by returning null
   		if (end != null) {
				if (end.textEqualsAny(DDL.BRACE_OPEN_STRING, DDL.COMMA_SIGN_STRING, DDL.SEMICOLON_SIGN_STRING)) {
					// start of select list or end of list element 
				} else if (end.isAnyKeyword("UNION", "INTERSECT", "EXCEPT")) {
   				// end of SELECT
   			} else if (end.isAnyKeyword("GROUP", "HAVING")) {
   				// next SELECT clause
   			} else if (end.isAnyKeyword("INNER", "LEFT", "RIGHT", "JOIN", "CROSS", "EXACT", "MANY", "ONE", "TO", "ASSOCIATION", "COMPOSITION")) {
   				// next join or association 
   			} else if (end.isAnyKeyword("WITH")) {
   				// ASSOCIATION [cardinality] TO target [AS assoc] ON cds_cond [WITH DEFAULT FILTER cds_cond] 
   			} else {
   				if (Program.showDevFeatures()) 
   					throw new IllegalArgumentException("Unexpected end '" + end.getText() + "' of DDL condition: " + parentCommand.toString());
   				// ignore this unexpected syntax by returning null as if no logical expression was found
   				end = null;
   			}
   		}
   		return (end == null) ? parentCommand.getLastCodeToken() : end.getPrevCodeToken();
   	}
      return null;
	}
	
	public boolean startsFunctionalMethodCall(boolean considerBuiltInFunctionsAsMethodCalls) {
		// exclude all cases that don't have the syntax of a functional method call
		if (!isIdentifier() || !getOpensLevel() || !textEndsWith("("))
			return false;
		else if (next == null || next.isAttached()) // e.g. DATA(lv_variable) or lv_any(5)
			return false;
		
		// if applicable, exclude built-in functions (which do NOT set sy-subrc, as opposed to ENDMETHOD in functional calls)
		if (!considerBuiltInFunctionsAsMethodCalls && textEqualsAny(ABAP.builtInFunctions))
			return false;

		// exclude constructor expressions
		Token prev = getPrevCodeToken();
		if (prev != null && prev.isAnyKeyword(ABAP.constructorOperators)) {
			return false;
		}
		return true;
	}
	
	public boolean startsConstructorExpression() {
		if (textEquals("#(") || isIdentifier() && getOpensLevel() && textEndsWith("(")) {
			if (next == null || next.isAttached()) // e.g. lv_any(5)
				return false;
			Token prev = getPrevCodeToken();
			return (prev != null && prev.isAnyKeyword(ABAP.constructorOperators));
		} else {
			return false;
		}
	}
	
	public int getCondensedWidthUpTo(Token endToken, boolean considerEndOfComments) {
		int result = 0;
		Token token = this;
		while (token != null) {
			if (considerEndOfComments || !token.isCommentAfterCode()) {
				if (token != this && !token.isAttached())
					result += 1;
				result += token.getTextLength();
			}
			if (token == endToken)
				break;
			token = token.getNext();
		}
		return result;
	}
	
	/**
	 * Returns true if this Token represents a type (dtype, abap_type, enum_type, mesh_type, ref_type, struct_type, table_type),
	 * not a data object (dobj).
	 * This could either be in the definition with TYPES type ... (this is only checked for includeTypeDef == true),   
	 * or when the type is used for declaring a data object (e.g. DATA ... TYPE type) or another type (e.g. TYPES ... TYPE STANDARD TABLE OF type)
	 * or after a constructor operator (e.g. ... = VALUE type( ... )) 
	 */
	public boolean isTypeIdentifier(boolean includeTypeDef) {
		if (type != TokenType.IDENTIFIER)
			return false;

		Token prevSibling = this.getPrevCodeSibling();
		
		// if the identifier is preceded by keywords, determine the first of these keywords (skipping chain colons)
		Token firstKeyword = null;
		Token token = prevSibling;
		while (token != null) {
			if (token.isKeyword()) {
				firstKeyword = token;
			} else if (!token.isChainColon()) {
				break;
			}
			token = token.getPrevCodeSibling();
		}
		
		// a) definition of a type (always in a TYPES statement)
		// TYPES [:] [BEGIN OF|BEGIN OF ENUM|BEGIN OF MESH] identifier1 ..., identifier2 ... 
		if (includeTypeDef && parentCommand.firstCodeTokenIsKeyword("TYPES")) {
			if (firstKeyword != null && firstKeyword.isKeyword("TYPES")) { 
				// the Token is the identifier directly after TYPES [:] [BEGIN OF|BEGIN OF ENUM|BEGIN OF MESH] 
				return true;
			} else if (prevSibling != null && prevSibling.isComma()) {
				// the Token is an identifier after a chain comma
				return true;
			}
			// otherwise continue below to check for usage
		}
		
		// b) usage of a type
		// - in a constructor expression, e.g. VALUE type( ), COND type( ), or any other constructor operator
		if (this.textEndsWith("(") && prevSibling != null && prevSibling.isAnyKeyword(ABAP.constructorOperators)) {
			return true;
		}

		// - in a declaration, e.g. DATA dobj TYPE STANDARD TABLE OF REF TO type (including declaration of a dependent type with TYPES)
		if (parentCommand.isDeclaration() || parentCommand.isDeclarationInClassDef()) { // CONSTANTS, DATA, FIELD-SYMBOLS, TYPES, CLASS-DATA, STATICS
			// the identifier represents a type if it is preceded by a sequence of keywords that starts with TYPE (NOT with LIKE)
			return (firstKeyword != null && firstKeyword.isKeyword("TYPE"));
		} else if (parentCommand.firstCodeTokenIsKeyword("INCLUDE")) {
			// the identifier represents a type if it is preceded by "INCLUDE TYPE" (NOT by "INCLUDE STRUCTURE", which is followed by a data object)
			return (firstKeyword != null && firstKeyword.matchesOnSiblings(true, "INCLUDE", "TYPE"));
		} else if (parentCommand.firstCodeTokenIsKeyword("CATCH")) {
			// CATCH [BEFORE UNWIND] cx_class1 cx_class2 ... [INTO oref].
			// for DATA(lx_any), prevSibling will be null (so it is excluded); for cx_class2, firstKeyword will be null; 
			return prevSibling != null && (firstKeyword == null || firstKeyword.isKeyword("CATCH"));
		}
		
		
		return false;
	}
	
	public Token getStartOfTableExpression() {
		Token token = this;
		while (token != null && token.closesLevel && token.textStartsWith("]")) {
			token = token.getPrevSibling();
		}
		return token;
	}
	
	public Token getEndOfTableExpression() {
		Token token = this;
		while (token != null && token.opensTableExpression()) {
			token = token.getNextSibling();
		}
		return token;
	}
	
	public boolean canInsertStressTestTokenAfter(StressTestType stressTestType) {
		// nothing can be inserted between attached tokens like DATA(lv_any) or lv_text+5(lv_length)
		if (next != null && next.isAttached() && !next.isCommaOrPeriod() && !next.isChainColon())
			return false;

		/*
		// do not insert a comment, pragma (or even line break) between "OPEN CURSOR", otherwise RND Parser will show all
		// commas in the SELECT field list as erroneous 
		if (isKeyword("OPEN") && next != null && next.isKeyword("CURSOR"))
			return false;
		*/
		
		// if a pragma follows on the same line after a period, it must be considered as the "nextToken" here
		Token nextCommandToken = null;
		if (next == null && parentCommand.getNext() != null && parentCommand.getNext().firstToken.lineBreaks == 0)
			nextCommandToken = parentCommand.getNext().firstToken;
		
		switch(stressTestType) {
			case LINE_END_COMMENT:
				return !isPragmaOrComment() && (next == null || !next.isPragmaOrComment()) && (nextCommandToken == null || !nextCommandToken.isPragma());

			case COMMENT_LINE:
				return !isPragma() && (next != null) && !next.isPragmaOrComment() && (nextCommandToken == null || !nextCommandToken.isPragma());
			
			case PRAGMA:
				return !isComment() && !isPeriod();

			case COLON:
				return !isComment() && !isPeriod() && (next != null);

			default:
				throw new InvalidParameterException();
		}
		
	}
	
	public boolean insertStressTestTokenAfter(StressTestType stressTestType) throws IntegrityBrokenException {
		if (!canInsertStressTestTokenAfter(stressTestType))
			return false;
		
		Token newToken;
		boolean isNewComment = false;
		switch(stressTestType) {
			case LINE_END_COMMENT:
				newToken = Token.createForAbap(0, 1, ABAP.COMMENT_SIGN_STRING + " test", sourceLineNum);
				isNewComment = true;
				break;
			case COMMENT_LINE:
				newToken = Token.createForAbap(1, 0, ABAP.LINE_COMMENT_SIGN_STRING + " test", sourceLineNum);
				isNewComment = true;
				break;
			case PRAGMA:
				newToken = Token.createForAbap(0, 1, ABAP.PRAGMA_SIGN + "PRAGMA", sourceLineNum); 
				break;
			case COLON:
				newToken = Token.createForAbap(0, 0, ABAP.COLON_SIGN_STRING, sourceLineNum); 
				break;
			default:
				throw new InvalidParameterException();
		}
												 
		if (isNewComment && next != null && next.lineBreaks == 0)
			next.setWhitespace(1, next.getStartIndexInLine());

		if (opensLevel) {
			if (firstChild == null) {
				insertFirstChild(newToken);
			} else {
				firstChild.insertLeftSibling(newToken);
			}
		} else if (next == null || next.closesLevel) {
			insertRightSibling(newToken);
		} else {
			next.insertLeftSibling(newToken);
		}
		return true;
	}

	private Token insertFirstChild(Token newToken) throws IntegrityBrokenException {
		if (firstChild != null)
			throw new IntegrityBrokenException(this, "child token already exists");
		
		// since 'this' Token opens a level, it cannot be a comment; however, newToken may be: 
		if (newToken.isComment() && next != null && next.lineBreaks == 0) {
			next.ensureLineBreak();
		}

		++parentCommand.tokenCount;

		newToken.parentCommand = parentCommand;
		newToken.parent = this;
		newToken.prev = this;
		newToken.next = next;
		newToken.prevSibling = null;
		newToken.nextSibling = null;

		if (next != null) 
			next.prev = newToken;
		next = newToken;
		firstChild = newToken;
		lastChild = newToken;

		parentCommand.onTokenInserted(newToken);
		parentCommand.testReferentialIntegrity(true);
		return newToken;
	}

	private void insertFirstChild(Term newTerm, boolean skipIntegrityTest) throws IntegrityBrokenException {
		if (firstChild != null)
			throw new IntegrityBrokenException(this, "child token already exists");

		// since 'this' Token opens a level, it cannot be a comment; however, newToken may end with a comment: 
		if (newTerm.lastToken.isComment() && next != null && next.lineBreaks == 0) {
			next.ensureLineBreak();
		}

		parentCommand.tokenCount += newTerm.getTokenCountWithChildren();
		newTerm.setParentCommand(parentCommand);
		newTerm.setParent(this);
		newTerm.firstToken.prev = this;
		newTerm.lastToken.next = next;
		newTerm.firstToken.prevSibling = null;
		newTerm.lastToken.nextSibling = null;

		if (next != null) 
			next.prev = newTerm.lastToken;
		next = newTerm.firstToken;
		firstChild = newTerm.firstToken;
		lastChild = newTerm.lastToken;

		parentCommand.onTermInserted(newTerm);
		if (!skipIntegrityTest) {
			parentCommand.testReferentialIntegrity(true);
		}
	}

	public boolean isSqlLiteralType() {
		return (isIdentifier() && next != null && next.isAttached() && this.textEqualsAny(ABAP.abapSqlLiteralTypes));
	}
	
	public boolean isSqlTypeInCast() {
		return textEqualsAny(ABAP.abapSqlLiteralTypes) && parent != null && parent.textEquals("CAST(") && getPrevCodeSibling() != null && getPrevCodeSibling().isKeyword("AS");
	}
	
	public boolean condenseUpTo(Token last, int maxLineLength, int indent, boolean keepCommentLines) {
		Token token = this;
		if (token == last)
			return false;
		
		boolean changed = false;
		int indexInLine = token.getEndIndexInLine();
		token = token.getNext();
		Token lastMovableToken = null;
		while (token != null) {
			// determine whether the Token is (or should be) attached; otherwise, it can be moved to the next line
			int spacesLeft = (token.isAttached() || token.isCommaOrPeriod() || token.isChainColon()) ? 0 : 1;
			if (spacesLeft > 0)
				lastMovableToken = token;

			// if needed, move Token(s) to the next line, otherwise move it directly behind the previous Token
			boolean isLineEndComment = parentCommand.isDdlOrDcl() ? token.isDdlLineEndComment() : token.isQuotMarkCommentLine(); 
			if (token.isAsteriskCommentLine() || keepCommentLines && isLineEndComment) {
				// do nothing

			} else if (token.getPrev().isComment()) {
				// after a comment, even a comma or period must be kept on the next line
				if (token.setWhitespace(1, indent)) {
					changed = true;
				}
				lastMovableToken = null;
				indexInLine = token.getEndIndexInLine();
			
			} else if (lastMovableToken != null && indexInLine + spacesLeft + token.getTextLength() > maxLineLength) {
				// break the line before the last movable (= the last non-attached) Token
				if (lastMovableToken.setWhitespace(1, indent)) {
					changed = true;
					if (token != lastMovableToken) {
						token.setWhitespace(0, spacesLeft);
					}
				}
				lastMovableToken = null;
				indexInLine = token.getEndIndexInLine();
			
			} else {
				if (token.setWhitespace(0, spacesLeft)) {
					changed = true;
				}
				indexInLine += spacesLeft + token.getTextLength();
			}
			
			if (token == last) {
				break;
			}
			token = token.getNext();
		}
		return changed;
	}

	/** returns true if this Token is the NEW keyword in SELECT ... INTO(NEW ...) and is attached to the opening parenthesis */
	boolean isAttachedNewKeywordInSelectInto() {
		if (isKeyword("NEW") && isAttached() && prev.textEquals("(")) {
			Token prevPrev = prev.getPrevCodeSibling();
			return (prevPrev != null && prevPrev.isKeyword("INTO") && parentCommand.isAbapSqlReadOperation());
		} else {
			return false;
		}
			
	}

	public void convertToStartEmbeddedExpression() {
		text = text.substring(0, text.length() - 1) + ABAP.BRACE_OPEN_STRING;
		opensLevel = true; 
	}

	public void convertToEndEmbeddedExpression() { 
		text = ABAP.BRACE_CLOSE_STRING + text.substring(1);
		closesLevel = true; 
	}
	
	/** returns a StrucInfo instance if the Token as a whole is the name of a structure or one of its components;
	 *  for asterisk comments, only the most frequent case of a commented-out assignment inside a VALUE or NEW constructor is considered */
	public StrucElement getStrucInfo() {
		boolean isIdentifier = isIdentifier();
		boolean isComment = isAsteriskCommentLine();
		
		if (!isIdentifier && !isComment) {
			return null;

		} else if (isIdentifier && text.indexOf(ABAP.COMPONENT_SELECTOR) > 0) {
			// this includes cases in ABAP SQL like '@ls_any-comp' 
			return StrucElement.createPartialForExprWithComponent(this);
		}
		
		// fields inside table expressions: itab[ comp = ... ]
		Token nextCode = getNextCodeSibling();
		Token prevCode = getPrevCodeSibling();
		if (isIdentifier && parent != null && parent.textEndsWith("[") && nextCode != null && nextCode.textEquals("=")) {
			return textEquals(ABAP.TABLE_LINE) ? null : StrucElement.createForComponentName(this, parent);
		}

		// fields inside constructor expressions: 
		boolean prevIsAssign = (prevCode != null && prevCode.textEquals("="));
		boolean nextIsAssign = (nextCode != null && nextCode.textEquals("="));
		
		Token prevKeyword = getPrevSiblingOfType(TokenType.KEYWORD);
		if (parent != null && parent.textEndsWith("(")) {
			// the constructor may start one level higher, e.g. VALUE #( ( c1 = ... ) )  
			Token ctorStart = parent.textEquals("(") ? parent.parent : parent;
			if (ctorStart != null && (ctorStart.isIdentifier() || ctorStart.textEquals("#("))) {
				Token ctorKeyword = ctorStart.getPrevCodeSibling();
				if (ctorKeyword == null) {
					// continue below

				} else if (ctorKeyword.isAnyKeyword("VALUE", "NEW")) {
					// VALUE #( comp = ... ) or VALUE #( ( comp = ... ) )
					if (isIdentifier && nextIsAssign) {
						return StrucElement.createForComponentName(this, ctorStart);
					} else if (isComment && getBitsOfCommentedOutAssignment() != null) {
						return StrucElement.createForComponentName(this, ctorStart);
					}
				
				} else if (isIdentifier && ctorKeyword.isAnyKeyword("FILTER")) {
					// FILTER type( itab [EXCEPT] [IN ftab] [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
					if (nextCode != null && nextCode.isComparisonOperator()) { // this could also be <= etc.
						return StrucElement.createForComponentName(this, ctorStart);
					}
				
				} else if (isIdentifier && ctorKeyword.isKeyword("CORRESPONDING")) {
					// CORRESPONDING #( ... [MAPPING   { t1 = s1 [DISCARDING DUPLICATES] }
					//                               | { ( t1 = s1 [DISCARDING DUPLICATES] [MAPPING ...] [EXCEPT ...] ) }
					//                               | { t1 = [s1] DEFAULT expr1 } ... ]  
					//                      [EXCEPT ti tj ...] )

					if (prevKeyword != null && prevKeyword.isKeyword("EXCEPT")) {
						// use the constructor type for the target structure fields
						return StrucElement.createForComponentName(this, ctorStart);
					}
					// determine whether this Token is part of a MAPPING ... assignment, possibly inside parentheses: MAPPING ( t1 = s1 ... )
					Token prevKeyword2 = (prevKeyword == null && parent.textEquals("(")) ? parent : prevKeyword;
					while (prevKeyword2 != null && (!prevKeyword2.isKeyword() || prevKeyword2.isAnyKeyword("DEFAULT", "DISCARDING", "DUPLICATES")))
						prevKeyword2 = prevKeyword2.prevSibling;
					if (prevKeyword2 != null && prevKeyword2.isKeyword("MAPPING")) {
						if (nextIsAssign) {
							// use the constructor type for the target structure fields
							return StrucElement.createForComponentName(this, ctorStart); 
						} else if (prevIsAssign) {
							// use the 'CORRESPONDING' Token for the source structure fields
							return StrucElement.createForComponentName(this, ctorKeyword); 
						}
					} 
				}
			}			
		}
		// for comments, only the most frequent case of a commented-out assignment inside a VALUE or NEW constructor is considered  
		if (isComment())
			return null;
		
		Token firstCode = parentCommand.getFirstCodeToken();
		Token prevPrevCode = (prevCode == null) ? null : prevCode.getPrevCodeSibling();
		Token prevPrevPrevCode = (prevPrevCode == null) ? null : prevPrevCode.getPrevCodeSibling();
		Token prevPrevKeyword = (prevKeyword == null) ? null : prevKeyword.getPrevCodeSibling();

		// exclude the case of "KEY keyname COMPONENTS" for multiple of the following cases
		if (prevKeyword != null && prevKeyword.isKeyword("KEY") && nextCode != null && nextCode.isKeyword("COMPONENTS"))
			return null;
		
		if (parentCommand.isAbapSqlOperation()) {
			if (text.indexOf(ABAP.TILDE) >= 0) {
				return StrucElement.createPartialForStructureAliasAndField(this, firstCode);

			} else if (textStartsWith(ABAP.AT_SIGN_STRING)) {
				// it was already checked above whether the text contains no ABAP.COMPONENT_SELECTOR, e.g. '@ls_any-comp' 
				return null;

			} else if (isAttached() && prev != null && (prev.opensInlineDeclaration() || prev.textEquals("("))) {
				// @DATA(...), dynamic list etc.
				return null;
			
			} else if (nextCode != null && (nextCode.isComparisonOperator() || nextCode.isAssignmentOperator())) {
				if (parent != null && parent.getPrevCodeSibling() != null && parent.getPrevCodeSibling().isKeyword("FROM")) {
					// for parameters, use the view name (i.e. this.parent) as the parent Token, not firstCode, so they form a context 
					// of their own: thus, if no CamelCase is known for the parameters, they will not prevent the field names from being changed 
					return StrucElement.createForParameterName(this, parent);
				} else {
					return StrucElement.createForComponentName(this, firstCode);
				}
			
			} else if (prevCode != null && (prevCode.isComparisonOperator() || prevCode.isAssignmentOperator()) && textEqualsAny(ABAP.ABAP_TRUE, ABAP.ABAP_FALSE, ABAP.ABAP_SPACE)) {
				return null;
			
			} else if (isSqlLiteralType() || isSqlTypeInCast()) { // e.g. int1`255`
				return null;
			
			} else if (firstCode.isAnyKeyword("INSERT", "UPDATE", "DELETE") && prevKeyword == firstCode) {
				return StrucElement.createForStructureName(this);
			
			} else if (prevKeyword != null && prevKeyword.isAnyKeyword("FROM", "JOIN", "TABLE", "IN")) {
				return StrucElement.createForStructureName(this);
			
			} else if (prevCode != null && prevCode.isKeyword("AS") && nextCode != null && nextCode.isKeyword("ON")) {
				return StrucElement.createForStructureAlias(this, firstCode);
			
			} else if (nextCode != null && nextCode.isKeyword("ON")) {
				return StrucElement.createForStructureName(this); 
			
			} else if (prevPrevPrevCode != null && prevPrevPrevCode.isKeyword("FROM") && prevCode.isKeyword("AS")) {
				return StrucElement.createForStructureAlias(this, firstCode);
			
			} else if (prevCode != null && prevCode.isKeyword("AS")) {
				return StrucElement.createForComponentAlias(this, firstCode); 
			
			} else {
				return StrucElement.createForComponentName(this, firstCode);
			}
			
		}
		
		// Internal Tables 
		if (firstCode.isKeyword("APPEND") && prevPrevKeyword != null && prevPrevKeyword.isKeyword("SORTED") && prevKeyword.isKeyword("BY")) {
			// APPEND ... TO itab SORTED BY comp.
			return StrucElement.createForComponentName(this, firstCode);
		
		} else if (firstCode.matchesOnSiblings(true, "DELETE|READ", "TABLE") && nextCode != null && nextCode.textEquals("=")
				&& prevKeyword != null && prevKeyword.isAnyKeyword("KEY", "COMPONENTS")) {
			// DELETE TABLE itab WITH TABLE KEY [keyname COMPONENTS] {comp_name1|(name1)} = operand1 ...
			// READ TABLE itab WITH [TABLE] KEY [keyname COMPONENTS] {comp_name1|(name1)} = operand1 ... 
			return textEquals(ABAP.TABLE_LINE) ? null : StrucElement.createForComponentName(this, prevKeyword);
		
		} else if (firstCode.matchesOnSiblings(true, "READ", "TABLE") && prevKeyword != null && prevKeyword.isAnyKeyword("COMPARING", "TRANSPORTING")) {
			// READ TABLE itab INTO ... [COMPARING { { comp1 comp2 ...}|{ALL FIELDS}|{NO FIELDS} }] [TRANSPORTING { { comp1 comp2 ...}|{ALL FIELDS} }] ...
			return StrucElement.createForComponentName(this, firstCode);
		
		} else if (firstCode.matchesOnSiblings(true, "LOOP", "AT") && nextCode != null && (nextCode.isComparisonOperator() || nextCode.isKeyword("IS"))) {
			// LOOP AT itab ... WHERE comp1 = ... AND comp2 IS ... AND comp3 OP ...
			// LOOP AT GROUP ... INTO FINAL(wa) WHERE comp1 = ... - no need to consider 'GROUP BY ( key1 = wa-comp1 key2 = wa-comp2 ... indx = ... size = ... )'
			return textEquals(ABAP.TABLE_LINE) ? null : StrucElement.createForComponentName(this, firstCode);
		
		} else if (firstCode.matchesOnSiblings(true, "AT", "NEW") || firstCode.matchesOnSiblings(true, "AT", "END", "OF")) {
			// AT NEW comp1. AT END OF comp2.
			return StrucElement.createForComponentName(this, firstCode);
		
		} else if (firstCode.isKeyword("SORT") && prevKeyword != null && !prevKeyword.isKeyword("SORT")) {
			// SORT itab ... BY {comp1 [ASCENDING|DESCENDING] [AS TEXT]}...
			return StrucElement.createForComponentName(this, firstCode);
		}

		// Declaration
		if (parentCommand.isDeclaration() || parentCommand.isDeclarationInClassDef()) { // CONSTANTS, DATA, FIELD-SYMBOLS, TYPES, CLASS-DATA, STATICS
			if (prevKeyword != null && prevKeyword.isAnyKeyword("KEY", "COMPONENTS")) {
				// DATA ... TYPE ... TABLE OF ... WITH ... KEY ... COMPONENTS comp
				return StrucElement.createForComponentName(this, prevKeyword); // do not use firstCode here, because the declaration might be chained

			} else if (prevPrevKeyword != null && prevPrevKeyword.isKeyword("TABLE") && prevKeyword.isKeyword("OF")) {
				return StrucElement.createForStructureName(this);

			}
		} 
		
		if (isTypeIdentifier(false)) { // also checks constructor expressions like 'VALUE type( ... )'
			return StrucElement.createForStructureName(this);
		}

		return null;
	}
	
	public boolean isChildOf(String parentText) {
		return isChildOfAny(parentText);
	}
	
	public boolean isChildOfAny(String... parentTexts) {
		Token token = parent;
		if (token != null && parentCommand.isDdlOrDcl()) { //  && token.textEqualsAny("(", "{", "[", "#(")
			token = token.getPrevCodeToken();
		}
		if (token == null)
			return false;
		return token.textEqualsAny(parentTexts);
	}

	public boolean startsMultiLineDdlComment() {
		return parentCommand.isDdlOrDcl() && isCommentLine() && textStartsWith(DDL.ASTERISK_COMMENT_START) && !endsMultiLineDdlComment();
	}

	public boolean endsMultiLineDdlComment() {
		return parentCommand.isDdlOrDcl() && isCommentLine() && textEndsWith(DDL.ASTERISK_COMMENT_END);
	}

	public boolean isDdlInlineComment() {
		return parentCommand.isDdlOrDcl() && textStartsWith(DDL.ASTERISK_COMMENT_START) && textEndsWith(DDL.ASTERISK_COMMENT_END);
	}

	public boolean isCommentedOutDdlAnnotation() { 
		if (parentCommand.isDdlOrDcl() && isCommentLine() && textStartsWithAny(DDL.LINE_END_COMMENT, DDL.LINE_END_MINUS_COMMENT)) {
			String textAfterCommentSign = text.substring(DDL.LINE_END_COMMENT.length()).trim();
			return textAfterCommentSign.startsWith(DDL.ANNOTATION_SIGN_STRING); 
		} else {
			return false;
		}
	}
	
	public ArrayList<Command> setWhitespaceInclAttachedComments(int newLineBreaksMin, int newLineBreaksMax, int newSpacesLeft, boolean includeDdlAnnoComments) {
		ArrayList<Command> changedCommands = new ArrayList<>();
		
		if (prev == null && lineBreaks == 1 && parentCommand.getPrev() != null && parentCommand.getPrev().isCommentLine()) {
			// move to the first attached comment Command (possibly except for commented-out annotations)
			Command comment = null;
			Command testCommand = parentCommand;
			while (testCommand.getFirstTokenLineBreaks() == 1) {
				Command prev = testCommand.getPrev();
				 if (prev == null || !prev.isCommentLine() || !includeDdlAnnoComments && prev.isCommentedOutDdlAnnotation())
					 break;
				 comment = prev;
				 testCommand = prev;
			}

			// set the supplied spacesLeft to all attached Commands, but the supplied lineBreaks only to the first one
			boolean isFirst = true;
			if (comment != null) {
				if (comment.getPrev() == null) {
					newLineBreaksMin = comment.getFirstTokenLineBreaks();
					newLineBreaksMax = newLineBreaksMin;
				}
				
				do {
					int addIndent = newSpacesLeft - comment.getFirstToken().spacesLeft;
					int newLineBreaks = determineLineBreaks(isFirst, comment.getFirstTokenLineBreaks(), newLineBreaksMin, newLineBreaksMax);
					if (comment.getFirstToken().setWhitespace(newLineBreaks, newSpacesLeft))
						changedCommands.add(comment);

					if (comment.startsMultiLineDdlComment()) {
						// change indent of the remaining Commands that belong to this multi-line comment
						while (!comment.endsMultiLineDdlComment()) {
							comment = comment.getNext();
							if (comment == null || !comment.isCommentLine()) // pro forma
								break;
							Token commentToken = comment.getFirstToken();
							if (commentToken.setWhitespace(commentToken.lineBreaks, commentToken.spacesLeft + addIndent)) { 
								changedCommands.add(comment);
							}
						} 
					}
					if (comment != null) // pro forma
						comment = comment.getNext();
					isFirst = false;
				} while (comment != null && comment.isCommentLine());
			}
			int newLineBreaks = determineLineBreaks(isFirst, this.lineBreaks, newLineBreaksMin, newLineBreaksMax);
			if (setWhitespace(newLineBreaks, newSpacesLeft)) {
				changedCommands.add(parentCommand);
			}

		} else if (prev != null && lineBreaks == 1 && prev.isCommentLine()) {
			// move to the first attached comment Token (possibly except for commented-out annotations)
			Token commentToken = null;
			Token testToken = this;
			while (testToken.lineBreaks == 1) {
				Token prev = testToken.prev;
				 if (prev == null || !prev.isCommentLine() || !includeDdlAnnoComments && prev.isCommentedOutDdlAnnotation())
					 break;
				 commentToken = prev;
				 testToken = prev;
			}
			
			// set the supplied spacesLeft to all attached Tokens, but the supplied lineBreaks only to the first one
			boolean isFirst = true;
			boolean changed = false;
			if (commentToken != null) {
				do {
					int addIndent = newSpacesLeft - commentToken.spacesLeft;
					int newLineBreaks = determineLineBreaks(isFirst, commentToken.lineBreaks, newLineBreaksMin, newLineBreaksMax);
					changed |= commentToken.setWhitespace(newLineBreaks, newSpacesLeft); 

					if (commentToken.startsMultiLineDdlComment()) {
						// change indent of the remaining Tokens that belong to this multi-line comment
						while (!commentToken.endsMultiLineDdlComment()) {
							commentToken = commentToken.getNext();
							if (commentToken == null || !commentToken.isCommentLine()) // pro forma
								break;
							changed |= commentToken.setWhitespace(commentToken.lineBreaks, commentToken.spacesLeft + addIndent); 
						} 
					}
					if (commentToken != null) // pro forma
						commentToken = commentToken.getNext();
					isFirst = false;
				} while (commentToken != null && commentToken.isComment());
			}
			int newLineBreaks = determineLineBreaks(isFirst, this.lineBreaks, newLineBreaksMin, newLineBreaksMax);
			changed |= setWhitespace(newLineBreaks, newSpacesLeft);
			if (changed) {
				changedCommands.add(parentCommand);
			}

		} else {
			// no attached comments - only change this Token
			int newLineBreaks = determineLineBreaks(true, this.lineBreaks, newLineBreaksMin, newLineBreaksMax);
			if (setWhitespace(newLineBreaks, newSpacesLeft)) {
				changedCommands.add(parentCommand);
			}
		}	
		
		return changedCommands;
	}

	private int determineLineBreaks(boolean change, int curLineBreaks, int newLineBreaksMin, int newLineBreaksMax) {
		return change ? Math.max(Math.min(curLineBreaks, newLineBreaksMax), newLineBreaksMin) : curLineBreaks;
	}
	
	public boolean startsDdlJoin() {
		if (!parentCommand.isDdlOrDcl())
			return false;

		return isAnyKeyword("INNER", "LEFT", "RIGHT", "CROSS", "JOIN")
			 || matchesOnSiblings(true, "EXACT", "ONE", "TO") 
			 || matchesOnSiblings(true, "MANY|ONE", "TO")
			 || matchesOnSiblings(true, "TO", "ONE|EXACT|MANY");
	}

	public boolean startsDdlAssociation() {
		if (!parentCommand.isDdlOrDcl())
			return false;
		return isAnyKeyword("ASSOCIATION", "COMPOSITION") || matchesOnSiblings(true, "REDEFINE", "ASSOCIATION");
	}

	public boolean startsDdlClause() {
		return isAnyKeyword("WHERE", "HAVING") || matchesOnSiblings(true, "GROUP", "BY");
	}

	public boolean startsDdlUnionEtc() {
		return isAnyKeyword("UNION", "INTERSECT", "EXCEPT");
	}

	/** return true if the Token is a colon ":" that belongs to a parameter name like ":P_Any" (only possible in DDIC-based views) */
	public boolean isDdlParameterColon() {
		if (textEquals(DDL.COLON_SIGN_STRING) && next != null && next.isAttached() && next.isIdentifier()) {
			// in cases like 'select from I_Any(P_Any::P_Other)', only the second colon belongs to a parameter  
			String prevChar = (prev == null) ? " " : StringUtil.getLastCharAsString(prev.text);
			if (!DDL.isCharAllowedForIdentifier(prevChar, 0, false))
				return true;

			// if the colon is detached from the previous Token, it probably starts a parameter name; since this cannot be 
			// safely determined without the syntax of the context, we rather assume it does, so it is not detached  
			return !isAttached();

		} else {
			return false;
		}
	}
	
	public boolean isDdlEntityName() {
		if (!parentCommand.isDdl() || parentCommand.isDdlAnnotation() || !isIdentifier())
			return false;
		return (this == parentCommand.getDdlOrDclEntityNameToken());	
	}
	
	public boolean isDdlDataSourceName() {
		if (!parentCommand.isDdl() || parentCommand.isDdlAnnotation() || !isIdentifier())
			return false;
		
		Token prevCode = getPrevCodeSibling();
		if (parentCommand.getParent() == null && prevCode != null && prevCode.isKeyword("FROM")) {
			// primary data source of the view
			return true;
			
		} else if (parentCommand.startsDdlJoin()) { 
			// join target
			return (prevCode != null && prevCode.isAnyKeyword("JOIN"));

		} else if (parentCommand.startsDdlAssociation()) { 
			// association target
			if (prevCode != null && prevCode.isKeyword("AS")) {
				return false;
			} else {
				Token nextCode = getNextCodeSibling();
				return (nextCode != null && nextCode.isAnyKeyword("AS", "ON"));
			}
		}
		return false;
	}
	
	public boolean isDdlSourceAliasDefinition() {
		if (!parentCommand.isDdl() || parentCommand.isDdlAnnotation() || !isIdentifier())
			return false;
		
		Token prevCode = getPrevCodeSibling();
		return (prevCode != null && prevCode.isKeyword("AS") && !parentCommand.isDdlSelectElement());
	}

	/** returns true if this Token represents the definition (but not usage) of an entity parameter,
	 * or the formal parameter of a data source */
	public boolean isDdlParameterName() {
		if (!parentCommand.isDdl() || parentCommand.isDdlAnnotation() || !isIdentifier())
			return false;
		
		Token prevCode = getPrevCodeSibling();
		Token nextCode = getNextCodeSibling();
		Token parentPrev = (parent == null) ? null : parent.getPrevCodeSibling();

		if (parentCommand.isDdlParametersElement()) {
			// name of an entity parameter
			return (prevCode == null);
			
		} else if (parentPrev != null && parentPrev.isDdlDataSourceName()) {
			// formal parameter name of a data source
			return (nextCode != null && nextCode.textEquals(DDL.COLON_SIGN_STRING));
		}
		
		return false;
	}
	
	public boolean isDdlTypeName() {
		if (!parentCommand.isDdl() || parentCommand.isDdlAnnotation() || !isIdentifier())
			return false;

		Token parentPrev = (parent == null) ? null : parent.getPrevCodeSibling();
		Token prevCode = getPrevCodeSibling();

		if (parentCommand.isDdlParametersElement()) {
			// type of an entity parameter
			return (prevCode != null) && prevCode.textEquals(DDL.COLON_SIGN_STRING);
			
		} else if (parentPrev != null && parentPrev.isKeyword("CAST") && prevCode != null && prevCode.isKeyword("AS") && getNextCodeSibling() == null) {
			// target type of "cast( ... as <type>)"
			return true;
		}
		
		return false;
	}
	
	/** Returns true if this Token can be followed by an arithmetic operator. 
	 * Note that the '-' for negative numbers in NOT considered an arithmetic operator here. */
	public boolean mayBeFollowedByArithmeticOp() {
		if (textEqualsAny(DDL.PARENS_CLOSE_STRING, DDL.BRACKET_CLOSE_STRING)) {
			return true;
		} else if (type == TokenType.IDENTIFIER || type == TokenType.LITERAL) {
			return true;
		} else { // KEYWORDS, ASSIGNMENT_OP, OTHER_OP, COMMA, "("
			return false;
		}
	}
	
	/** check whether the Token is an asterisk comment line in the format "***  parameter_or_component  =  ..." 
	 *  (with any number of spaces and asterisks); if so, returns a String[4], e.g.  
	 *  { "***", "parameter_or_component_name", "=", "any_term" } */
	public String[] getBitsOfCommentedOutAssignment() {
		if (!isAsteriskCommentLine())
			return null;

		// check whether the asterisk comment line has the format "***   parameter   =   ..." (with any number of spaces and asterisks)
		String commentText = getText();
		int equalsSignPos = commentText.indexOf('=');
		if (equalsSignPos < 0)
			return null;

		String[] asterisksAndParamName = StringUtil.split(commentText.substring(0, equalsSignPos), ' ', true);
		if (asterisksAndParamName.length != 2)
			return null;
		else if (!ABAP.mayBeVariableName(asterisksAndParamName[1], false, getParentCommand().isInOOContext()))
			return null;

		return new String[] { asterisksAndParamName[0], asterisksAndParamName[1], "=", commentText.substring(equalsSignPos + 1).trim() };
	}
}
