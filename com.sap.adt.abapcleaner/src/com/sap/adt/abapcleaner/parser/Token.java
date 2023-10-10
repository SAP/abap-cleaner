package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleID;

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

	public final String getText() { return text; }
	
	// type and effect of this Token
	public TokenType type = TokenType.values()[0];
	private boolean closesLevel;
	private boolean opensLevel;
	boolean collocationContinues;

	public final boolean closesLevel() { return closesLevel; }

	public final boolean getOpensLevel() { return opensLevel; }
	final void setOpensLevel(boolean value) { opensLevel = value; }

	public final boolean isComment() { return (type == TokenType.COMMENT); }

	public final boolean isQuotMarkComment() { return isComment() && AbapCult.stringStartsWith(text, ABAP.COMMENT_SIGN_STRING); }

	public final boolean isCommentLine() { return isComment() && isFirstTokenInLine(); }

	public final boolean isQuotMarkCommentLine() { return isCommentLine() && isQuotMarkComment(); }

	public final boolean isAbapDocComment() {
		// ABAP Doc comments are expected to be comment lines, however, it is syntactically possible to put them as 
		// line-end comments after METHODS: (only getting a warning, but no error) 
		return AbapCult.stringStartsWith(text, ABAP.ABAP_DOC_SIGN); 
	}

	public final boolean isAbapDocCommentLine() { return isCommentLine() && AbapCult.stringStartsWith(text, ABAP.ABAP_DOC_SIGN); }

	public final boolean isAsteriskCommentLine() { return isCommentLine() && AbapCult.stringStartsWith(text, ABAP.LINE_COMMENT_SIGN_STRING); }

	public final boolean isCommentAfterCode() { return isComment() && (prev != null) && (lineBreaks == 0); }

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

	public final boolean isCharacterLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text) && (text.charAt(0) == ABAP.QUOT_MARK || text.charAt(0) == ABAP.QUOT_MARK2);
	}

	public final boolean isStringLiteral() {
		return isLiteral() && !StringUtil.isNullOrEmpty(text)
				&& (text.charAt(0) == ABAP.QUOT_MARK || text.charAt(0) == ABAP.QUOT_MARK2 || text.charAt(0) == ABAP.PIPE || text.charAt(0) == ABAP.BRACE_CLOSE);
	}

	final boolean startsStringTemplate() { return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(0) == ABAP.PIPE; }
	
	final boolean endsStringTemplate() { return isLiteral() && !StringUtil.isNullOrEmpty(text) && text.charAt(text.length() - 1) == ABAP.PIPE; }
	
	final boolean startsWithLetter() { return !StringUtil.isNullOrEmpty(text) && Character.isLetter(text.charAt(0)); }

	final boolean startsEmbeddedExpression() { return isStringLiteral() && text.charAt(text.length() - 1) == ABAP.BRACE_OPEN; }

	final boolean endsEmbeddedExpression() { return isStringLiteral() && text.charAt(0) == ABAP.BRACE_CLOSE; }

	public final boolean isAssignmentOperator() { return (type == TokenType.ASSIGNMENT_OP); }

	public final boolean isComparisonOperator() { return (type == TokenType.COMPARISON_OP); }

	public final boolean isComparisonOperator(String text) { return isComparisonOperator() && textEquals(text); }

	public final boolean isAnyComparisonOperator(String... compareTexts) { return isComparisonOperator() && textEqualsAny(compareTexts); }

	public final int getTextLength() { return text.length(); }

	public final boolean hasChildren() { return (firstChild != null); }

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
	
	/**
	 * Returns true if the Token text equals the supplied comparison text.  
	 * To check the Token type at the same time, use {@link #isKeyword(String)} or {@link #isComparisonOperator(String)} 
	 * (see list of tokens classified as comparison operators in {@link ABAP#isComparisonOperator(String)}) 
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
			 || (token.type == TokenType.IDENTIFIER && type == TokenType.KEYWORD)) {
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

		// even in non-ABAP sections, comments are started with * at line start
		if (AbapCult.stringStartsWith(text, ABAP.LINE_COMMENT_SIGN_STRING) && isAtLineStart) {
			type = TokenType.COMMENT;

		} else {			
			if (language == Language.ABAP) 
				type = inferTypeFromAbapToken(text);
			else if (language == Language.SQLSCRIPT) 
				type = inferTypeFromSqlScriptToken(text);
			else 
				type = inferTypeFromOtherToken(text);
		}
		
		if (type == TokenType.KEYWORD || type == TokenType.IDENTIFIER || type == TokenType.OTHER_OP) {
			closesLevel = textStartsWithAny(levelClosers);
			opensLevel = textEndsWithAny(levelOpeners);
		} else if (type == TokenType.LITERAL) {
			closesLevel = textStartsWith("}");
			opensLevel = textEndsWith("{");
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
		
		} else if (ABAP.isComparisonOperator(text)) {
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
			char[] textChars = text.toCharArray();
			for (char c : textChars) {
				if (Character.isLetter(c)) 
					return TokenType.IDENTIFIER;
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
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < lineBreaks; ++i)
			result.append(ABAP.LINE_SEPARATOR);
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
		final char space = ' ';

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
					// text may still contain spaces for several Tokens, e.g. "TRANSPORTING NO FIELDS"
					Token testToken = token.getLastTokenOfPlainSequence(siblingsOnly, skipCommentsAndPragmas, StringUtil.split(variant, space, false));
					if (testToken != null) {
						match = true;
						token = testToken;
						break;
					}
				}
			} else {
				// text may still contain spaces for several Tokens, e.g. "TRANSPORTING NO FIELDS"
				Token testToken = token.getLastTokenOfPlainSequence(siblingsOnly, skipCommentsAndPragmas, StringUtil.split(text, space, false));
				match = (testToken != null);
				if (match)
					token = testToken;
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
		if (!skipIntegrityTest)
			command.testReferentialIntegrity(true);
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
		return insertLeftSibling(newToken, false, false);
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling before this Token
	 * 
	 * @param newToken
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertLeftSibling(Token newToken, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		return insertLeftSibling(newToken, moveFollowingLinesRight, false);
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
		if (newToken == null)
			throw new NullPointerException("newToken");
		if (closesLevel)
			throw new IntegrityBrokenException(this, "cannot insert a left sibling to a token that closes a level");
		
		int oldStartIndex = moveFollowingLinesRight ? getStartIndexInLine() : 0; // save performance if information is not needed below

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
			parentCommand.addIndent(newToken.spacesLeft + newToken.getTextLength(), oldStartIndex);

		parentCommand.onTokenInserted(newToken);
		
		if (!skipIntegrityTest)
			parentCommand.testReferentialIntegrity(true);
		
		return newToken;
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling after this Token
	 * 
	 * @param newToken
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertRightSibling(Token newToken) throws IntegrityBrokenException {
		return insertRightSibling(newToken, false);
	}

	/**
	 * inserts the supplied Token (including its possible children) as a sibling after this Token
	 * 
	 * @param newToken
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final Token insertRightSibling(Token newToken, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		if (newToken == null)
			throw new NullPointerException("newToken");

		int oldStartIndex = moveFollowingLinesRight ? this.next.getEndIndexInLine() + 1 : 0; // save performance if information is not needed below

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
			parentCommand.addIndent(newToken.spacesLeft + newToken.getTextLength(), oldStartIndex);

		parentCommand.onTokenInserted(newToken);
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
		insertRightSibling(newTerm, false);
	}

	/**
	 * inserts the supplied Term (including its possible children) as a sibling after this Token
	 * 
	 * @param newTerm
	 * @param moveFollowingLinesRight
	 * @throws IntegrityBrokenException 
	 */
	public final void insertRightSibling(Term newTerm, boolean moveFollowingLinesRight) throws IntegrityBrokenException {
		if (newTerm == null)
			throw new NullPointerException("newTerm");

		int oldStartIndex = moveFollowingLinesRight ? this.next.getEndIndexInLine() + 1 : 0; // save performance if information is not needed below
		int newTermWidth = newTerm.getCurrentWidth();

		Token lastTokenInNewTerm = newTerm.lastToken.lastChild != null ? newTerm.lastToken.lastChild : newTerm.lastToken; // TODO: can newTerm.lastToken have child Tokens at all?

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
			parentCommand.addIndent(newTerm.firstToken.spacesLeft + newTermWidth, oldStartIndex);

		parentCommand.onTermInserted(newTerm);
		parentCommand.testReferentialIntegrity(true);
	}

	public final void insertLeftSibling(Term newTerm) throws IntegrityBrokenException {
		if (newTerm == null)
			throw new NullPointerException("newTerm");

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
		
		parentCommand.testReferentialIntegrity(true);
	}

	public final void copyWhitespaceFrom(Token token) {
		if (token == null)
			throw new NullPointerException("token");
		lineBreaks = token.lineBreaks;
		spacesLeft = token.spacesLeft;
	}

	public final void ensureWhitespace() {
		if (lineBreaks == 0 && spacesLeft == 0)
			spacesLeft = 1;
	}

	/**
	 * Returns true if the Token text equals any of the supplied comparison texts.  
	 * To check the Token type at the same time, use {@link #isAnyKeyword(String...)} or {@link #isAnyComparisonOperator(String...)} 
	 * (see list of tokens classified as comparison operators in {@link ABAP#isComparisonOperator(String)}) 
	 */
	public final boolean textEqualsAny(String... compareTexts) {
		for (String compareText : compareTexts) {
			if (textEquals(compareText))
				return true;
		}
		return false;
	}

	final boolean textStartsWithAny(String... prefixes) {
		for (String prefix : prefixes) {
			if (textStartsWith(prefix))
				return true;
		}
		return false;
	}

	final boolean textEndsWithAny(String... suffixes) {
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

	public final boolean setSpacesLeftAdjustingIndent(int newSpacesLeft) {
		if (newSpacesLeft == spacesLeft)
			return false;
		
		if (next == null) {
			// nothing to adjust
			spacesLeft = newSpacesLeft; 
		} else {
			int addSpaceCount = newSpacesLeft - spacesLeft;
			int minSpacesLeft = (next.lineBreaks == 0) ? next.getStartIndexInLine() : this.getStartIndexInLine() + 1;
			spacesLeft = newSpacesLeft;
			parentCommand.addIndent(addSpaceCount, minSpacesLeft, next, null, true);
		}
		return true;
	}

	final ColorType getMainColorType() {
		switch (type) {
			case COMMENT:
			case PRAGMA:
				return ColorType.COMMENT;

			case LITERAL:
				return isStringLiteral() ? ColorType.STRING_LITERAL : ColorType.NUMBER;

			case ASSIGNMENT_OP:
				return ColorType.USUAL_OPERATOR;

			case OTHER_OP:
				return textEquals(")") && isAttached() ?  ColorType.TOKEN_OPERATOR : ColorType.USUAL_OPERATOR;

			case COMPARISON_OP:
				if (!StringUtil.isNullOrEmpty(text) && Character.isLetter(text.charAt(0))) // GE, GT, EQ, NE etc.
					return ColorType.KEYWORD;
				else
					return ColorType.USUAL_OPERATOR;

			case KEYWORD:
				return isDeclarationKeyword() ? ColorType.DECLARATION_KEYWORD : ColorType.KEYWORD;

			case IDENTIFIER:
				MemoryAccessType accessType = getMemoryAccessType(); 
				return accessType.displayAsWritePos ? ColorType.IDENTIFIER_WRITE_POS : ColorType.IDENTIFIER;

			case NON_ABAP:
				return ColorType.NON_ABAP;

			default:
				return ColorType.IDENTIFIER;
		}
	}

	private final boolean isDeclarationKeyword() {
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

	final TextBit[] toTextBits(int startIndex) {
		boolean isSimpleCase = false;
		
		// in most cases, the whole Token is of one ColorType
		ColorType colType = getMainColorType();
		if (type != TokenType.KEYWORD && type != TokenType.IDENTIFIER && type != TokenType.LITERAL && type != TokenType.OTHER_OP)
			isSimpleCase = true;
		if (type == TokenType.IDENTIFIER && text.charAt(0) == ABAP.FIELD_SYMBOL_START_SIGN)
			isSimpleCase = true;
		if (type == TokenType.OTHER_OP && !textEndsWithAny("(", ")")) // "ULINE AT /(20)."
			isSimpleCase = true;

		// literals may be linked to a text symbol ID, having the form 'literal text'(idf), where 'idf' is always 3 characters long
		if (type == TokenType.LITERAL) {
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

		if (isTextSymbol()) {
			// text symbol IDs may be numeric (TEXT-001) or contain letters or _ (TEXT-a01, TEXT-a_2)
			int prefixLength = ABAP.TEXT_SYMBOL_PREFIX.length();
			ColorType idColorType = ABAP.consistsOfDigitsOnly(text.substring(prefixLength)) ? ColorType.NUMBER : ColorType.IDENTIFIER;
			return new TextBit[] { TextBit.create(startIndex, prefixLength, ColorType.KEYWORD), 
										  TextBit.create(startIndex + prefixLength, ABAP.TEXT_SYMBOL_ID_LENGTH, idColorType) };
		}

		ArrayList<TextBit> result = new ArrayList<TextBit>();
		int writtenPos = 0;
		boolean lastWasIdentifier = false;
		for (int i = 0; i < text.length(); ++i) {
			char c = text.charAt(i);
			boolean isIdentifier = (type == TokenType.KEYWORD) ? ABAP.isCharAllowedForAnyKeyword(c) : ABAP.isCharAllowedForVariableNames(c);
			// in some cases, initial '/' does NOT start an identifier with a namespace, e.g. "ULINE AT /.", "ULINE AT /10(20)." or "ULINE at /pos(20)."
			if (i == 0 && c == '/' && (text.length() <= 1 || text.indexOf('/', 1) < 0)) {
				isIdentifier = false;
			}
			if (i > 0 && isIdentifier != lastWasIdentifier) {
				// if the text bit is not part of the keyword or the identifier, it is considered a token operator (e.g. "->", "=>", "(" etc.)
				ColorType bitType = lastWasIdentifier ? colType : ColorType.TOKEN_OPERATOR;
				// in some cases, a Token of type OTHER_OP contains a number, e.g. "ULINE AT /10(20)." and "ULINE AT 10(**)." 
				if (ABAP.isInteger(text.substring(writtenPos, i)))
					bitType = ColorType.NUMBER;
				result.add(TextBit.create(startIndex + writtenPos, i - writtenPos, bitType));
				writtenPos = i;
			}
			lastWasIdentifier = isIdentifier;
		}
		// process the last bit
		if (writtenPos < text.length()) {
			ColorType bitType = lastWasIdentifier ? colType : ColorType.TOKEN_OPERATOR;
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
		return result;
	}

	public final int getEndIndexInLine() {
		Token token = this;
		int result = token.spacesLeft + token.text.length();
		while (token.lineBreaks == 0 && token.prev != null) {
			token = token.prev;
			result += token.spacesLeft + token.text.length();
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
		Token result = next;
		while (result != null && result.type != tokenType)
			result = result.nextSibling;
		return result;
	}

	public final Token getPrevTokenOfType(TokenType tokenType) {
		Token result = prev;
		while (result != null && result.type != tokenType)
			result = result.prev;
		return result;
	}

	public final Token getPrevSiblingOfType(TokenType tokenType) {
		Token result = prev;
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
		check(!closesLevel || prevSibling != null);
		check(!closesLevel || prevSibling.opensLevel);
		if (testCommentPositions) {
			// there can be no further Token behind a comment (except in the next line)
			check(prev == null || !prev.isComment() || lineBreaks > 0); 
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
	public final void setText(String newText, boolean adjustIndent) {
		if (newText == null)
			throw new NullPointerException("newText");
		
		if (!adjustIndent || next == null || next.lineBreaks > 0) {
			// nothing to adjust
			this.text = newText;
		} else {
			int addSpaceCount = newText.length() - text.length();
			int minSpacesLeft = next.getStartIndexInLine();
			this.text = newText;
			if (addSpaceCount != 0)
				parentCommand.addIndent(addSpaceCount, minSpacesLeft, next, null, true);
		}
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
			
		} else if (ABAP.isComparisonOperator(token.getText())) { // includes BETWEEN and IN
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
		if (firstToken.isAnyKeyword(Command.declarationKeywordsReservingMemory)) {
			if  (prevToken.isAnyKeyword(Command.declarationKeywords) || prevToken.isComma()) {
				return ABAP.isFieldSymbol(text) ? MemoryAccessType.ASSIGN_TO_FS_OR_DREF : MemoryAccessType.WRITE;
			} else { // e.g. a class name after TYPE REF TO
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
		} else if (prevToken == null) { 
			// avoid having to check prevToken == null in the rest of the method
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
		} else if (firstToken.isAnyKeyword("CLEAR", "CLEAR:", "FREE", "FREE:") && !prevToken.isKeyword("WITH")) {
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
		} else if (firstToken.isKeyword("REPLACE") && (nextToken.isKeyword("WITH") || prevToken.isKeyword("RESULTS") || (prevToken.isAnyKeyword("COUNT", "OFFSET", "LENGTH") && prevPrevToken != null && prevPrevToken.isKeyword("REPLACEMENT")))) { 
			// REPLACE [{FIRST OCCURRENCE}|{ALL OCCURRENCES} OF] pattern IN [section_of] dobj WITH new  [IN {CHARACTER|BYTE} MODE] REPLACEMENT COUNT rcnt, REPLACEMENT OFFSET roff, REPLACEMENT LENGTH rlen
			// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapreplace_options.htm
			return MemoryAccessType.WRITE;

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
		} else if (firstToken.matchesOnSiblings(true, "CONVERT", "DATE") && prevToken.isAnyKeyword("STAMP", "ZONE")) {
			// CONVERT DATE dat [TIME tim [DAYLIGHT SAVING TIME dst]] INTO TIME STAMP time_stamp TIME ZONE tz.
			// but NOT for the time zone in 'CONVERT DATE ... TIME ZONE tz INTO UTCLONG time_stamp'!
			if (!firstToken.matchesOnSiblings(true,  TokenSearch.ASTERISK, "INTO", "UTCLONG"))
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
		} else if (firstToken.isAnyKeyword("SELECT", "WITH") && prevToken.isAnyKeyword("INTO", "OF", "TABLE", "RESULT")) {
			// SELECT ... { { INTO ( elem1, elem2,  ...) } | { INTO [CORRESPONDING FIELDS OF] wa [indicators] } | { INTO|APPENDING [CORRESPONDING FIELDS OF] TABLE itab [indicators] [PACKAGE SIZE n] } }
			//   EXTENDED RESULT @oref 
			// WITH: cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapwith.htm
			return MemoryAccessType.WRITE;
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
		// - TODO: CALL TRANSFORMATION

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
			if (testToken == null)
				return MemoryAccessType.WRITE;
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
			// find corresponding access token
			Token accessKeyword = prevToken;
			while (accessKeyword != null && !accessKeyword.isAnyKeyword("EXPORTING", "IMPORTING", "TABLES", "CHANGING", "RECEIVING", "EXCEPTIONS")) {
				accessKeyword = accessKeyword.getPrevCodeSibling();
			}
			// if no access keyword is found, we use the default result .READ below
			if (accessKeyword != null) {
				if (accessKeyword.isAnyKeyword("TABLES", "EXPORTING"))
					return MemoryAccessType.READ;
				else if (accessKeyword.isAnyKeyword("IMPORTING", "RECEIVING"))
					return MemoryAccessType.WRITE;
				else if (accessKeyword.isAnyKeyword("CHANGING")) 
					return MemoryAccessType.READ_WRITE;
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
		} else if (next.isAttached() && (next.isIdentifier() || next.isLiteral())) {
			// e.g. DATA(lv_variable) or lv_any(5)
			return null; 

		} else if (textEqualsAny("boolc(", " boolx(", " xsdbool(")) {
			// skip built-in functions for logical expressions; also skip "boolx( bool = log_exp bit = bit )", 
			// because parameters that expect a log_exp are not yet considered in AlignParametersRule. 
			return null;

			// By contrast, NO need to skip the other ABAP.builtInFunctionsWithoutNamedParams, because they have the same 
			// syntax as a method call "any_method( arg )" that omits the parameter name. After excluding log_exp, the  
			// argument could be of type table expression, numeric, string / byte string, or table, e.g.
			// line_index( table_exp ), sin( arg ), strlen( arg ), lines( itab ). 
			// Also note that a class may define methods with the same name as a built-in function; in such a case,   
			// the built-in function is 'shadowed' (even if the method has a different signature) and the method is called.

		} else if (textEquals("(")) {
			// stand-alone " ( " only starts a component list if it is a row inside a VALUE or NEW constructor expression, 
			// not if it is part of arithmetic expressions etc.
			if (parent == null) {
				// stand-alone " ( " on top level must be part of an arithmetic expression
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
			Token prev = getPrevCodeSibling();  
			if (prev != null && !prev.textEquals(")") && (prev.isAssignmentOperator() || prev.isComparisonOperator() || prev.isOtherOp() || prev.isAnyKeyword("UNTIL", "WHILE", "WHERE")))
				return null;

		} else {
			// as the token opens a level but is not a stand-alone "(", it must be a method call or a constructor expression,  
			// because the Tokens "method_name(", "VALUE #(" etc. have text immediately attached to the top level parenthesis
			Token prev = getPrevCodeToken();
			if (prev != null && prev.isAnyKeyword("CONV", "CORRESPONDING", "CAST", "REF", "EXACT", "REDUCE", "FILTER", "COND", "SWITCH")) {
				// do not align inside these constructor expressions
				return null;
			}
		}
		return getNextSibling();
	}
	
	
	/**
	 * If this Token starts a logical expression, the corresponding end Token is returned; otherwise null.
	 * @return
	 */
	public Token getEndOfLogicalExpression() {
      Token firstCode = parentCommand.getFirstCodeToken();

      // 1. cases on top level (parent == null)
      if (this == firstCode && firstCode.isAnyKeyword("IF", "ELSEIF", "CHECK", "WHILE")) {
      	return parentCommand.getLastNonCommentToken();

      } else if (isKeyword("WHERE") && parent == null && firstCode.matchesOnSiblings(true, "LOOP AT|MODIFY|DELETE|FOR")) {
         // LOOP AT ... WHERE <logical_expression> [GROUP BY ...].
         // MODIFY itab ... WHERE log_exp ... etc. 
         Token end = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "GROUP BY|USING KEY|TRANSPORTING|FROM");
         if (end == null) {
            return parentCommand.getLastCodeToken();
         } else if (end.isAnyKeyword("BY", "KEY")) { // if "GROUP BY" or "USING KEY" was found
            return end.getPrevCodeSibling();
         } else {
         	return end;
         }

      } else if (isKeyword("UNTIL") && parent == null && firstCode.isKeyword("WAIT")) {
         // WAIT FOR ASYNCHRONOUS TASKS [MESSAGING CHANNELS] [PUSH CHANNELS] UNTIL log_exp [UP TO sec SECONDS].
         Token end = getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "UP", "TO");
         if (end == null) {
            return parentCommand.getLastCodeToken();
         } else {
            return end.getPrevCodeSibling(); // "UP"
         }
      } 
      
      // 2. xsdbool( ... ) and boolc( ... )
      if (textEqualsAny("xsdbool(", "boolc(")) {
      	return getNextSibling();
      }
      
      
      // 3. WHEN / UNTIL / WHILE / WHERE clauses inside constructor expressions
      // the following must be aligned with Command.finishBuild()!
      Token ctorKeyword = (parent == null) ? null : parent.getPrevCodeSibling();
      if (ctorKeyword == null || !ctorKeyword.isKeyword()) {
      	return null;
      
      } else if (isAnyKeyword("WHEN") && ctorKeyword.isAnyKeyword("COND")) {
      	// "FOR, Iteration Expressions", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor.htm, context:
      	// - COND type( [let_exp] WHEN log_exp THEN ... WHEN .. )
      	return getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "THEN");
      
      } else if (isAnyKeyword("UNTIL", "WHILE") && ctorKeyword.isAnyKeyword("REDUCE", "NEW", "VALUE")) {
      	// "FOR, Conditional Iteration", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfor_conditional.htm, context:
         // - REDUCE|NEW|VALUE identifier( ... FOR var = rhs [THEN expr] UNTIL|WHILE log_exp [let_exp] ... ) 
      	return getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "LET|NEXT|FOR");

      } else if (isKeyword("WHERE") && ctorKeyword.isAnyKeyword("FILTER")) {
      	// FILTER type( itab [EXCEPT] [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
      	// FILTER type( itab [EXCEPT] IN ftab [USING KEY keyname] WHERE c1 op f1 [AND c2 op f2 [...]] ) ...
        	return parent.getNextSibling();
         
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
         return getNextCodeSibling().getNextSibling().getNext();
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
		if (prev != null && prev.isAnyKeyword("NEW", "VALUE", "CONV", "CORRESPONDING", "CAST", "REF", "EXACT", "REDUCE", "FILTER", "COND", "SWITCH")) {
			return false;
		}
		return true;
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
	 * This could either be in the definition with TYPES type ...,   
	 * or when the type is used for declaring a data object (e.g. DATA ... TYPE type) or another type (e.g. TYPES ... TYPE STANDARD TABLE OF type)
	 * or after a constructor operator (e.g. ... = VALUE type( ... )) 
	 */
	public boolean isTypeIdentifier() {
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
		if (parentCommand.firstCodeTokenIsKeyword("TYPES")) {
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
		if (parentCommand.isDeclaration()) { // CONSTANTS, DATA, FIELD-SYMBOLS, TYPES, CLASS-DATA, STATICS
			// the identifier represents a type if it is preceded by a sequence of keywords that starts with TYPE (NOT with LIKE)
			return (firstKeyword != null && firstKeyword.isKeyword("TYPE"));
		} else if (parentCommand.firstCodeTokenIsKeyword("INCLUDE")) {
			// the identifier represents a type if it is preceded by "INCLUDE TYPE" (NOT by "INCLUDE STRUCTURE", which is followed by a data object)
			return (firstKeyword != null && firstKeyword.matchesOnSiblings(true, "INCLUDE", "TYPE"));
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
}
