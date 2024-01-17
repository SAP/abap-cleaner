package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.programbase.*;

/**
 * <p>A Term may be ...</p>
 * <ul>
 * <li>a single {@link Token} of type LITERAL or IDENTIFIER, including 'structure-component' 
 * (which together is one {@link Token} of type IDENTIFIER)</li>
 * <li>a table expression like 'identifier[ ... ]' or 'identifier[ ... ]-component',</li>
 * <li>a method call or even call chain like 'identifier( ... )=&gt;identifier( ... )-&gt;identifier( ...)';</li>
 * <li>an arithmetic expression like 'a + b * ( c + d )'; however, if the Term is constructed with {@link Term#Term(Token, boolean)} 
 * and 'expandWithArithmeticOps = false', there will be no space within it, except for contents in parentheses or brackets</li>
 * <li>any range of {@link Token}s that are (possibly remote) siblings, if the Term is constructed with {@link Term#Term(Token, Token)}</li>
 * </ul>
 * 
 * <p>The Term's {@link #firstToken} and {@link #lastToken} are siblings; the {@link #lastToken} is always childless.
 * To move a Term to another place, it can be removed with {@link #removeFromCommand()} and inserted elsewhere 
 * with {@link Token#insertLeftSibling(Term)} or {@link Token#insertRightSibling(Term, boolean)}.</p>
 */
public class Term {
	// for binary operators see ABAP Reference, "Operators", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenoperators.htm
	private final static String[] binaryOperators = new String[] { "+", "-", "*", "/", "DIV", "MOD", "**", "BIT-AND", "BIT-OR", "BIT-XOR", "&&", "&" }; 

	public final Token firstToken;
	public final Token lastToken;

	public final boolean isSingleToken() { return (firstToken == lastToken); }

	public final boolean isSingleLiteral() { return isSingleToken() && firstToken.isLiteral(); }

	public final boolean isSingleIntegerLiteral() { return isSingleToken() && firstToken.isIntegerLiteral(); }

	public final boolean isSingleFloatLiteral() { return isSingleToken() && firstToken.isFloatLiteral(); }

	public final boolean isSingleStringLiteral() { return isSingleToken() && firstToken.isStringLiteral(); }

	public final boolean isSingleIdentifier() { return isSingleToken() && firstToken.isIdentifier(); }

	final Command getParentCommand() { return firstToken.getParentCommand(); }

	final void setParentCommand(Command value) {
		Token token = firstToken;
		token.setParentCommand(value);
		while (token != lastToken) {
			token = token.getNext();
			token.setParentCommand(value);
		}
	}

	final Token getParent() { 
		return firstToken.getParent();
	}
	final void setParent(Token value) {
		Token token = firstToken;
		token.setParent(value);
		while (token != lastToken) {
			token = token.getNextSibling();
			token.setParent(value);
		}
	}

	public final Token getPrev() { 
		return firstToken.getPrev(); 
	}
	final void setPrev(Token value) {
		firstToken.setPrev(value);
	}

	public final Token getNext() { 
		return lastToken.getNext(); 
	}

	public final Token getNextNonCommentToken() { 
		return lastToken.getNextNonCommentToken(); 
	}

	public final Token getNextCodeToken() { 
		return lastToken.getNextCodeToken(); 
	}

	public final Token getNextCodeSibling() { 
		return lastToken.getNextCodeSibling(); 
	}

	final Token getPrevSibling() { 
		return firstToken.getPrevSibling(); 
	}
	final void setPrevSibling(Token value) {
		firstToken.setPrevSibling(value);
	}

	public final Token getNextSibling() { 
		return lastToken.getNextSibling(); 
	}
	final void setNextSibling(Token value) {
		lastToken.setNextSibling(value);
	}

	public static boolean isFirstTokenAllowed(Token token) {
		switch (token.type) {
			case IDENTIFIER:
			case LITERAL:
			case OTHER_OP:
				return true;
			case KEYWORD:
				return token.isAnyKeyword(ABAP.constructorOperators) 
					 || token.isAnyKeyword("DATA(", "FINAL(") 
					 || token.textStartsWith("TEXT-");
			default:
				return false;
		}
	}

	public static Term createSimple(Token firstToken) throws UnexpectedSyntaxException {
		if (firstToken == null)
			throw new NullPointerException("firstToken");

		return new Term(firstToken, false);
	}
	
	public static Term createArithmetic(Token firstToken) throws UnexpectedSyntaxException {
		if (firstToken == null)
			throw new NullPointerException("firstToken");
	
		return new Term(firstToken, true);
	}
	
	public static Term createForTokenRange(Token firstToken, Token lastToken) throws UnexpectedSyntaxException {
		if (firstToken == null)
			throw new NullPointerException("firstToken");
		if (lastToken == null)
			throw new NullPointerException("lastToken");
		
		return new Term(firstToken, lastToken);
	}

	private Term(Token firstToken, boolean expandWithArithmeticOps) throws UnexpectedSyntaxException {
		this.firstToken = firstToken;
		Token token = firstToken;
		// cp. for the following: Term.isFirstTokenAllowed(Token)
		if (firstToken.isAnyKeyword(ABAP.constructorOperators)) {
			// constructor expression, see ABAP Reference, "Constructor Operators for Constructor Expressions", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenconstructor_expressions.htm
			token = token.getNextCodeToken();
		} else if (firstToken.isAnyKeyword("DATA(", "FINAL(", "@DATA(", "@FINAL(")) {
			// continue below
		} else if (firstToken.isKeyword() && firstToken.textStartsWith("TEXT-")) {
			// continue below
		} else if (firstToken.isKeyword("CASE")) {
			// continue below
		} else if (firstToken.isStringLiteral() || firstToken.isLiteral() || firstToken.isIdentifier() || firstToken.isOtherOp()) {
			// continue below
		} else if (firstToken.getOpensLevel() && firstToken.textEqualsAny(ABAP.abapSqlFunctions)) {
			// continue below
		} else {
			throw new UnexpectedSyntaxException(firstToken, "First token '" + firstToken.text + "' unexpected for a Term");
		}

		// determine the lastToken of this Term
		if (token.startsStringTemplate()) {
			while (!token.endsStringTemplate()) {
				token = token.getNextSibling();
			}
		} else if (token.isKeyword("CASE")) {
			// move to the end of the CASE expression in ABAP SQL: 'CASE WHEN sql_cond1 THEN result1 [WHEN sql_cond2 THEN result2] [ELSE resultn|sql_null] END'
			int nestingDepth = 1;
			while (nestingDepth > 0) {
				token = token.getNextCodeSibling();
				if (token == null) { // pro forma
					break;
				} else if (token.isKeyword("CASE")) {
					++nestingDepth;
				} else if (token.isKeyword("END")) {
					--nestingDepth;
				}
			}
		} else {
			while (token.getOpensLevel()) {
				token = token.getNextSibling();
				if (!token.isClosingParenthesisOrBracket() && !token.isIdentifier() && !token.textEquals("][")) {
					throw new UnexpectedSyntaxException(token, "Expected an identifier or a closing bracket, but found " + token.getTypeAndTextForErrorMessage() + "!");
				}
			}
		}

		// optionally, continue reading if token is followed by a binary operator (e.g. if called from ValueStatementRule.createTableFromAssignmentSequence())
		// like in the parameter assignment "amount = <ls_tdc_ev_pob>-event_based-list_price_per_unit * <ls_tdc_ev_pob>-event_based-qty / 2"
		if (expandWithArithmeticOps) {
			while (token.getNextCodeToken() != null && token.getNextCodeToken().textEqualsAny(binaryOperators)) {
				token = token.getNextCodeToken().getNextCodeToken();
				if (token.startsStringTemplate()) {
					while (!token.endsStringTemplate()) {
						token = token.getNextSibling();
					}
				} else {
					if (token.isAnyKeyword(ABAP.constructorOperators)) // e.g. "... + COND any_data_element( WHEN ... THEN ... )
						token = token.getNextCodeToken();
					while (token.getOpensLevel()) {
						token = token.getNextSibling(); // the next sibling closes the level again, but might open another level
					}
				}
			}
		}

		lastToken = token;

		if (lastToken.hasChildren()) {
			throw new UnexpectedSyntaxException(lastToken, "Token '" + lastToken.text + "' unexpected as last token of a Term, since it has child tokens!");
		}
	}

	private Term(Token firstToken, Token lastToken) throws UnexpectedSyntaxException {
		this.firstToken = firstToken;
		this.lastToken = lastToken;

		if (this.lastToken.hasChildren())
			throw new UnexpectedSyntaxException(lastToken, "Token '" + this.lastToken.text + "' unexpected as last token of a Term, since it has child tokens!");

		// firstToken and lastToken must be siblings
		Token token = this.firstToken;
		while (token != this.lastToken) {
			token = token.getNextSibling();
			if (token == null) {
				throw new UnexpectedSyntaxException(this.lastToken,
						"The first and last Token of a Term must be siblings, but '" + this.firstToken.text + "' and '" + this.lastToken.text + "' are not.");
			}
		}
	}

	final int getTokenCountWithChildren() {
		Token token = firstToken;
		int result = 1;
		while (token != lastToken) {
			token = token.getNext();
			++result;
		}
		return result;
	}

	final int getSiblingCount() {
		Token token = firstToken;
		int result = 1;
		while (token != lastToken) {
			token = token.getNextSibling();
			++result;
		}
		return result;
	}

	public final void removeFromCommand(boolean skipReferentialIntegrityTest) throws IntegrityBrokenException {
		// prevent the next Token in the line from being moved up to the previous line, esp. if the previous line ends 
		// with a comment; if the to-be-removed Token is afterwards moved to a different place, its new lineBreaks / spacesLeft
		// must therefore be set only AFTER calling .removeFromCommand()
		Token next = getNext();
		if (firstToken.lineBreaks > 0 && next != null && next.lineBreaks == 0) {
			next.copyWhitespaceFrom(firstToken);
		}

		Command parentCommand = getParentCommand();
		if (parentCommand.firstToken == firstToken)
			parentCommand.firstToken = getNext();
		if (parentCommand.lastToken == lastToken)
			parentCommand.lastToken = getPrev();
		parentCommand.tokenCount -= getTokenCountWithChildren();

		if (getParent() != null) {
			if (getParent().getFirstChild() == firstToken && getParent().getLastChild() == lastToken) {
				getParent().setFirstChild(null);
				getParent().setLastChild(null);
			} else if (getParent().getFirstChild() == firstToken) {
				getParent().setFirstChild(getNext());
			} else if (getParent().getLastChild() == lastToken) {
				getParent().setLastChild(getPrev());
			}
		}

		if (getPrev() != null)
			getPrev().setNext(getNext());
		if (getNext() != null)
			getNext().setPrev(getPrev());

		if (getPrevSibling() != null)
			getPrevSibling().setNextSibling(getNextSibling());
		if (getNextSibling() != null)
			getNextSibling().setPrevSibling(getPrevSibling());

		if (!skipReferentialIntegrityTest)
			parentCommand.testReferentialIntegrity(true, true);
	}

	/**
	 * adds the provided (and possibly negative) number of spaces to the indent of all lines covered by this Term, starting from the second line;
	 * returns true if whitespace was changed
	 * 
	 * @param spaceCount
	 */
	public final boolean addIndent(int spaceCount) {
		if (spaceCount == 0)
			return false;

		boolean result = false;
		Token token = firstToken;
		while (token != lastToken && token.getNext() != null) {
			token = token.getNext();
			if (token.lineBreaks > 0 && !token.isAsteriskCommentLine()) {
				token.spacesLeft = Math.max(token.spacesLeft + spaceCount, 0);
				result = true;
			}
		}
		return result;
	}

	public final int getMaxEndIndexInAnyLine(boolean considerComments) {
		int result = 0;
		Token token = firstToken;

		// if the Term only consists of one Token, return its end index, even if it is a comment 
		if (token == lastToken) 
			return token.getEndIndexInLine();

		while (token != null) {
			// determine the next Token, unless it is a comment and comments shall be ignored
			Token next = token.getNext();
			if (considerComments || !token.isComment()) {
				boolean isLastInLine = (next == null || next.lineBreaks > 0 || (!considerComments && next.isComment()));
				if (token == lastToken || isLastInLine) {
					result = Math.max(result, token.getEndIndexInLine());
				}
			}

			if (token == lastToken)
				break;
			token = token.getNext();
		}
		return result;
	}

	public final int getCurrentWidth(boolean considerComments) { 
		return getMaxEndIndexInAnyLine(considerComments) - firstToken.getStartIndexInLine(); 
	}

	public final int getSumTextAndSpaceWidth() { return getSumTextAndSpaceWidth(false); }
	public final int getSumTextAndSpaceWidth(boolean addOneSpaceOnly) {
		Token token = firstToken;
		int result = token.getTextLength();
		while (token != lastToken) {
			token = token.getNext();
			if (token.lineBreaks > 0 || token.spacesLeft > 0) // for attached tokens (e.g. ":"), do not add anything
				result += (addOneSpaceOnly || token.lineBreaks > 0) ? 1 : token.spacesLeft;
			result += token.getTextLength();
		}
		return result;
	}

	public final boolean hasInnerComment() {
		Token token = firstToken; 
		while (token != lastToken) {
			if (token.isComment())
				return true;
			token = token.getNext();
		}
		return false;
	}

	public final boolean hasCommentAtAnyLineEnd() {
		Token token = firstToken; // the first Token itself cannot be a comment
		while (token != lastToken) {
			token = token.getNext();
			if (token.isCommentAfterCode())
				return true;
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();
		Token token = firstToken;
		result.append(token.text);
		while (token != lastToken) {
			token = token.getNext();
			result.append(token.toString());
		}
		return result.toString();
	}

	public final String toErrorLogString() {
		StringBuilder result = new StringBuilder();
		Token token = firstToken;
		result.append(token.text);
		while (token != lastToken) {
			token = token.getNextNonCommentToken();
			result.append(" " + token.text);
		}
		return result.toString();
	}
	
	/**
	 * returns true if evaluating the expression may have side effects, which could cause repeated evaluation to create different results.
	 * (i.e. if the expression contains a method call or a NEW constructor) 
	 * @return
	 */
	public final boolean mayHaveSideEffects() {
		Token token = firstToken;
		while (token != null) {
			if (token.getOpensLevel() && !token.textEquals("(") && token.textEndsWith("(") && !token.opensInlineDeclaration()) {
				Token prev = token.getPrevCodeSibling();
				boolean isMethodCall = (prev == null) || !prev.isAnyKeyword(ABAP.constructorOperators);
				boolean isConstructorExpr = (prev != null) && prev.isKeyword("NEW");
				if (isMethodCall || isConstructorExpr) 
					return true;
			}
			if (token == lastToken)
				break;
			token = token.getNextCodeToken();
		}
		return false;
	}
	
	public final boolean isOnSingleLine() {
		if (firstToken == lastToken)
			return true;
		Token token = firstToken.getNext();
		while (token != null) {
			if (token.lineBreaks > 0)
				return false;
			if (token == lastToken)
				break;
			token = token.getNext();
		}
		return true;
	}
	
	public boolean contains(Token searchToken) {
		Token token = firstToken;
		do {
			if (token == searchToken)
				return true;
			if (token == lastToken)
				break;
			token = token.getNext();
		} while (true);
		return false;
	}

	public boolean condense() {
		return firstToken.condenseUpTo(lastToken, ABAP.MAX_LINE_LENGTH, firstToken.getStartIndexInLine(), false);
	}
	
	public boolean setFirstTokenWhitespace(int lineBreaks, int spacesLeft) {
		return setTokenWhitespace(firstToken, lineBreaks, spacesLeft);
	}
	
	public boolean setTokenWhitespace(Token startTokenInTerm, int lineBreaks, int spacesLeft) {
		int oldStartIndex = startTokenInTerm.getStartIndexInLine();
		if (!startTokenInTerm.setWhitespace(lineBreaks, spacesLeft)) 
			return false;
		if (startTokenInTerm == lastToken)
			return true;
		int addSpaceCount = startTokenInTerm.getStartIndexInLine() - oldStartIndex;
		startTokenInTerm.getParentCommand().addIndent(addSpaceCount, oldStartIndex, startTokenInTerm.getNext(), lastToken.getNext(), false);
		return true; 
	}
	
	public Token findSiblingOfTypeAndTexts(TokenType tokenType, String... texts) {
		return firstToken.getNextSiblingOfTypeAndTextBefore(getNextCodeSibling(), tokenType, texts);
	}
}
