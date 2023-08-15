package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;

import java.util.*;

public class LogicalExpression {
	public static final int BINDING_LEVEL_COUNT = 5;

	private final static String[] keywordOfBindingLevel = new String[] { "", "EQUIV", "OR", "AND", "NOT" };

	// complexity of negating a whole (sub-)expression with 'NOT ( ... )'
	private final static int COMPLEXITY_OF_OUTER_NOT = 3;

	// complexity of Boolean operators: "AND" is a bit easier to understand than "OR";
	// "NOT" is difficult to understand
	private final static int COMPLEXITY_OF_AND = 0;       // exp1 AND exp2
	private final static int COMPLEXITY_OF_OR = 0;        // exp1 OR exp2
	private final static int COMPLEXITY_OF_INNER_NOT = 3; // NOT EQUIV, NOT exp, NOT BETWEEN, NOT BOUND, NOT line_exists( )

	// complexity of comparisons: "=" is easier to understand than "<>"; 
	// similarly, "= abap_true" is easier to understand than "= abap_false" etc.
	private final static int COMPLEXITY_OF_EQ = 0;       // =, EQ
	private final static int COMPLEXITY_OF_LT_GT = 1;    // <, <=, >=, >, LT, LE, GE, GT
	private final static int COMPLEXITY_OF_NE = 2;       // <>, NE
	private final static int COMPLEXITY_OF_EQ_TRUE = 0;  // = abap_true
	private final static int COMPLEXITY_OF_EQ_FALSE = 2; // = abap_false
	

	private static BindingLevel getBindingLevelOfToken(Token token, boolean isFirstTokenInExpression) {
		if (token.isKeyword()) {
			if (token.isKeyword("EQUIV"))
				return BindingLevel.EQUIV;
			else if (token.isKeyword("OR"))
				return BindingLevel.OR;
			else if (token.isKeyword("AND"))
				return BindingLevel.AND;
			else if (isFirstTokenInExpression && token.isKeyword("NOT"))
				return BindingLevel.NOT;
		}
		return BindingLevel.UNKNOWN;
	}

	// binding: EQUIV < OR < AND < NOT (XOR seems to be replaced by NOT EQUIV), see
	// - https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenvalue_constructor_params_lspc.htm
	// - implicit parentheses: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_boole.htm
	// - relational expression: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenrelational_expression_glosry.htm
	// - Comparison Operators (lots of them!): https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_op.htm
	// - Relational/Predicate Operator IS: used in predicate expressions: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpredicate_expressions.htm
	// - Predicate Functions: line_exists(), matches(), contains(), contains_any_of(), contains_any_not_of()

	private LogicalExpression parentExpression;
	private Token firstToken; // possibly the opening parenthesis "("
	private Token lastToken; // possibly the closing parenthesis ")"
	private Token endToken; // the Token after lastToken (in a LogicalExpression there certainly is one, at least the final period .)
	private boolean isInParentheses;
	private BindingLevel bindingLevel = BindingLevel.values()[0];

	private ArrayList<Token> keywords = new ArrayList<Token>(); // the keyword(s) that correspond to the BindingLevel of this expression: EQUIV (possibly preceded by NOT), OR, AND,
																				 // NOT, comparison operator / IS / the predicate function
	private ArrayList<LogicalExpression> innerExpressions = new ArrayList<LogicalExpression>(); // null for COMPARISON_OR_PREDICATE
	private final RelationalExpressionType relExprType;
	private boolean isSupported;
	private boolean isValid;

	/** current complexity of this logical expression and all its inner expressions */
	private int currentComplexity;
	
	/** complexity of this logical expression and all its inner expressions 
	 *  if the expression were negated by propagating negation to its inner expressions (if possible) */
	private int negatedComplexity;

	public final boolean isSupported() { return isSupported; }

	final Token getFirstToken() { return firstToken; }

	private Token getFirstTokenExceptOpeningParenthesis() { return isInParentheses ? firstToken.getNextCodeToken() : firstToken; }

	private Token getLastTokenExceptClosingParenthesis() { return isInParentheses ? lastToken.getPrevCodeToken() : lastToken; }

	private Token getEndTokenExceptClosingParenthesis() { return isInParentheses ? lastToken : endToken; }

	@Override
	public String toString() {
		return getDebuggingText();
	}

	public static LogicalExpression create(Token firstToken, Token lastToken) throws UnexpectedSyntaxException {
		return new LogicalExpression(null, firstToken, lastToken);
	}
	
	static LogicalExpression createInner(LogicalExpression parentExpression, Token firstToken, Token lastToken) throws UnexpectedSyntaxException {
		return new LogicalExpression(parentExpression, firstToken, lastToken);
	}
	
	private LogicalExpression(LogicalExpression parentExpression, Token firstToken_, Token lastToken_) throws UnexpectedSyntaxException {
		this.parentExpression = parentExpression;
		this.firstToken = firstToken_.getThisOrNextCodeToken();
		this.lastToken = lastToken_.getThisOrPrevCodeToken();

		endToken = lastToken.getNextCodeToken();
		isInParentheses = (firstToken.getOpensLevel() && lastToken.closesLevel() && firstToken.textEquals("(") && lastToken.textEquals(")") && lastToken == firstToken.getNextCodeSibling());

		// determine BindingLevel
		BindingLevel minBindingLevel = BindingLevel.COMPARISON_OR_PREDICATE;
		Token token = getFirstTokenExceptOpeningParenthesis();
		Token end = getEndTokenExceptClosingParenthesis();
		boolean isFirst = true;
		boolean ignoreNextAnd = false;
		while (token != null && token != end) {
			// if the ternary comparison operator "BETWEEN" is found, ignore the next "AND", since it is NOT a logical operator: "operand [NOT] BETWEEN operand1 AND operand2"
			if (token.isComparisonOperator("BETWEEN"))
				ignoreNextAnd = true;
			else if (ignoreNextAnd && token.textEquals("AND"))
				ignoreNextAnd = false;
			else if (token.isKeyword()) {
				BindingLevel tokenBindingLevel = getBindingLevelOfToken(token, isFirst);
				if (tokenBindingLevel != BindingLevel.UNKNOWN && tokenBindingLevel.getValue() < minBindingLevel.getValue())
					minBindingLevel = tokenBindingLevel;
			}
			token = token.getNextCodeSibling();
			isFirst = false;
		}
		bindingLevel = minBindingLevel;

		isSupported = true; // may be set to false in .addInnerExpression()
		if (bindingLevel.getValue() < BindingLevel.COMPARISON_OR_PREDICATE.getValue()) {
			// find Keywords "EQUIV", "OR", "AND" or "NOT" (depending on the BindingLevel) and the InnerExpresions separated by them
			String keyword = keywordOfBindingLevel[bindingLevel.getValue()];
			Token start = getFirstTokenExceptOpeningParenthesis();
			ignoreNextAnd = false;
			token = start;
			while (token != null && token != end) {
				// if the ternary comparison operator "BETWEEN" is found, ignore the next "AND", since it is NOT a logical operator
				if (token.isComparisonOperator("BETWEEN"))
					ignoreNextAnd = true;
				else if (ignoreNextAnd && token.textEquals("AND"))
					ignoreNextAnd = false;

				else if (token.isKeyword(keyword)) {
					if (token == start && bindingLevel != BindingLevel.NOT)
						throw new UnexpectedSyntaxException(token, "unexpected position of ABAP keyword '" + keyword + "'");
					if (token != start)
						addInnerExpression(start, token.getPrevCodeToken());
					keywords.add(token);
					start = token.getNextCodeSibling();
				}
				if (bindingLevel == BindingLevel.NOT)
					break;
				token = token.getNextCodeSibling();
			}
			if (start != end)
				addInnerExpression(start, end.getPrevCodeToken());
			relExprType = RelationalExpressionType.NONE;

		} else {
			// relational expression: find comparison or predicate keyword and determine type
			Token keyword = findComparisonOrPredicateKeyword();
			if (keyword == null) {
				isSupported = false;
				relExprType = RelationalExpressionType.NONE;
			} else {
				keywords.add(keyword);
				if (keyword.isKeyword("IS"))
					relExprType = RelationalExpressionType.PREDICATE_EXPRESSION;
				else if (keyword.isIdentifier())
					relExprType = RelationalExpressionType.PREDICATE_FUNCTION;
				else
					relExprType = RelationalExpressionType.COMPARISON;
			}
		}
		isValid = isSupported;
	}

	private void addInnerExpression(Token start, Token end) throws UnexpectedSyntaxException {
		LogicalExpression newInnerExpression = LogicalExpression.createInner(this, start, end);
		innerExpressions.add(newInnerExpression); // for debugging, add the newInnerExpression even if it is not supported
		if (!newInnerExpression.isSupported)
			isSupported = false;
	}

	private Token findComparisonOrPredicateKeyword() {
		Token start = getFirstTokenExceptOpeningParenthesis();
		Token end = getEndTokenExceptClosingParenthesis(); // prevent getLastTokenOfSequence() from reading further than this LogicalExpression goes

		// search for Relational/Predicate Operator IS
		Token isToken = start.getLastTokenOfSequence(true, false, end, TokenSearch.ASTERISK, "IS");
		if (isToken != null) {
			// relationalExpressionType = RelationalExpressionType.PREDICATE_EXPRESSION;
			return isToken;
		}

		// search for comparison operator; in case of "BETWEEN", all the rest (i.e. "operand1 AND operand2") 
		// will simply be put on the right-hand side of the "BETWEEN" comparison operator
		Token compOp = start.getLastTokenOfSequence(true, false, end, TokenSearch.ASTERISK, TokenSearch.ANY_COMPARISON_OPERATOR);
		if (compOp != null) {
			// relationalExpressionType = RelationalExpressionType.COMPARISON;
			return compOp;
		}

		// determine predicate function
		if (start.isIdentifier() && start.textEqualsAny("line_exists(", "matches(", "contains(", "contains_any_of(", "contains_any_not_of(")) {
			// relationalExpressionType = RelationalExpressionType.PREDICATE_FUNCTION;
			return start;
		}

		// determine predicative method calls; the result of such method calls can have any data type (not necessarily abap_bool)
		// and is implicitly evaluated by ABAP with IS NOT INITIAL; negation should therefore use NOT  
		// (cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenpredicative_method_calls.htm)
		if (start.isIdentifier() && start.textEndsWith("(") && start.getNextCodeSibling() != null && start.getNextCodeSibling().getNextCodeToken() == end) {
			// relationalExpressionType = RelationalExpressionType.PREDICATE_FUNCTION;
			return start;
		}
		
		// relationalExpressionType = RelationalExpressionType.NONE;
		return null;
	}

	private String getDebuggingText() {
		StringBuilder result = new StringBuilder();

		
		if (!isSupported)
			result.append(" ??:");

		// show complexity in debugger:
		// calculateComplexity(true);
		// result.append(" [" + Cult.format(currentComplexity) + "!" + Cult.format(negatedComplexity) + "]");
		
		Token token = firstToken;
		int keywordIndex = 0;
		int innerExpressionIndex = 0;
		while (token != endToken) {
			if (keywordIndex < keywords.size()) {
				Token nextKeyword = keywords.get(keywordIndex);
				if (token == nextKeyword) {
					if (nextKeyword.isIdentifier() && nextKeyword.textEndsWith("(")) {
						result.append(" !" + nextKeyword.getText() + " )!");
						token = nextKeyword.getNextCodeSibling().getNextCodeToken();
					} else {
						result.append(" !" + nextKeyword.getText() + "!");
						token = nextKeyword.getNextCodeToken();
					}
					++keywordIndex;
					continue;
				}
			}
			if (innerExpressionIndex < innerExpressions.size()) {
				LogicalExpression nextInnerExpression = innerExpressions.get(innerExpressionIndex);
				if (token == nextInnerExpression.firstToken) {
					result.append(" { " + nextInnerExpression.toString() + " }");
					token = nextInnerExpression.endToken;
					++innerExpressionIndex;
					continue;
				}
			}
			result.append(" " + token.getText());
			token = token.getNextCodeToken();
		}
		return result.toString().trim();
	}

	public final void negate(NegationStyle negationStyle, boolean convertAbapFalseAndAbapTrue) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges {
		if (!isValid)
			throw new IllegalStateException("This logical expression instance is no longer valid!");

		// for the whole LogicalExpression and all of its inner expressions, calculate the .currentComplexity  
		// as well as the .negatedComplexity of a negated expression
		if (parentExpression == null && negationStyle == NegationStyle.AVOID_INNER_NEGATIONS)
			calculateComplexity(convertAbapFalseAndAbapTrue);

		// decide whether to put NOT ( ... ) around the whole (sub-)expression
		boolean negateWithOuterNot = false;
		switch (negationStyle) {
			case ALWAYS_WITH_AND_OR:
				negateWithOuterNot = (bindingLevel == BindingLevel.AND || bindingLevel == BindingLevel.OR);
				break;
			case AVOID_INNER_NEGATIONS:
				negateWithOuterNot = (currentComplexity + COMPLEXITY_OF_OUTER_NOT < negatedComplexity);
				break;
			case NEVER:
				negateWithOuterNot = false;
				break;
		}
		if (negateWithOuterNot) {
			Token notToken = Token.createForAbap(firstToken.lineBreaks, firstToken.spacesLeft, "NOT", TokenType.KEYWORD, firstToken.sourceLineNum);
			firstToken.insertLeftSibling(notToken, false);
			firstToken.setWhitespace();

			// NOT shall refer to the whole logical (sub-)expression; therefore, decide whether parentheses are required
			if (firstToken.textEquals("(") && firstToken.getNextCodeSibling() == lastToken) {
				// the whole expression is already inside a pair(!) of parentheses; note, however, that a non-pair  
				// such as '( a = 1 ) AND ( b = 2 )' would require insertion of parentheses: 'NOT ( ( a = 1 ) AND ( b = 2 ) )' 
			} else if (bindingLevel != BindingLevel.COMPARISON_OR_PREDICATE) {
				// AND / OR / EQUIV is involved (which bind weaker than NOT), therefore put the whole expression in parentheses
				firstToken.insertParenthesesUpTo(lastToken.getNext());
			}
			return;
		}
		
		if (bindingLevel == BindingLevel.EQUIV) {
			Token equivToken = keywords.get(0);
			if (equivToken.getPrevCodeToken().isKeyword("NOT"))
				equivToken.getPrevCodeToken().removeFromCommand();
			else
				insertNotBefore(equivToken, false);

		} else if (bindingLevel == BindingLevel.OR) {
			for (Token orToken : keywords)
				orToken.setText("AND", false);
			for (LogicalExpression innerExpression : innerExpressions) {
				innerExpression.negate(negationStyle, convertAbapFalseAndAbapTrue);
				isValid &= innerExpression.isValid;
			}
			isValid = false; // the object structure of this logical expression no longer matches the code!

		} else if (bindingLevel == BindingLevel.AND) {
			for (Token andToken : keywords)
				andToken.setText("OR", false);
			for (LogicalExpression innerExpression : innerExpressions) {
				innerExpression.negate(negationStyle, convertAbapFalseAndAbapTrue);
				isValid &= innerExpression.isValid;
			}
			// since transforming "AND" into "OR" diminishes the binding level, parentheses may have to be added, 
			// if the parentExpression used OR, now transformed to AND (note that BindingLevel is not changed)
			if (parentExpression != null && (parentExpression.bindingLevel == BindingLevel.OR) && !isInParentheses)
				firstToken.insertParenthesesUpTo(lastToken.getNext());
			isValid = false; // the object structure of this logical expression no longer matches the code!

		} else if (bindingLevel == BindingLevel.NOT) {
			removeLeadingNot(keywords.get(0));
			// TODO: any parentheses to remove under certain conditions?

			// BindingLevel.COMPARISON_OR_PREDICATE
		} else if (relExprType == RelationalExpressionType.COMPARISON) {
			Token compOp = keywords.get(0);
			Token nextCode = compOp.getNextCodeToken();
			if (convertAbapFalseAndAbapTrue && compOp.textEquals("=") && nextCode.textEqualsAny(ABAP.ABAP_FALSE, ABAP.ABAP_TRUE))
				nextCode.setText(nextCode.textEquals(ABAP.ABAP_FALSE) ? ABAP.ABAP_TRUE : ABAP.ABAP_FALSE, false);
			else if (compOp.isAnyComparisonOperator("IN", "BETWEEN")) {
				Token prevCode = compOp.getPrevCodeToken();
				if (prevCode.isKeyword("NOT")) {
					prevCode.removeFromCommand();
				} else {
					Token notToken = Token.createForAbap(compOp.lineBreaks, compOp.spacesLeft, "NOT", TokenType.KEYWORD, compOp.sourceLineNum);
					compOp.insertLeftSibling(notToken, false);
					compOp.setWhitespace();
				}
			} else {
				try {
					compOp.setText(ABAP.negateComparisonOperator(compOp.getText()), false);
				} catch (IndexOutOfBoundsException ex) {
					throw new UnexpectedSyntaxException(compOp, ex.getMessage());
				}
			}

		} else if (relExprType == RelationalExpressionType.PREDICATE_EXPRESSION) {
			Token isToken = keywords.get(0);
			Token nextCode = isToken.getNextCodeToken();
			if (nextCode.isKeyword("NOT")) {
				nextCode.removeFromCommand();
			} else {
				insertNotBefore(nextCode, false);
			}
			
		} else if (relExprType == RelationalExpressionType.PREDICATE_FUNCTION) {
			Token functionCall = keywords.get(0);
			if (functionCall != firstToken && functionCall.getPrevCodeToken().isKeyword("NOT")) {
				// this branch is probably impossible to reach, because code like "NOT line_exists( ... )" would be handled above with BindingLevel.NOT
				removeLeadingNot(functionCall.getPrevCodeToken());
			} else {
				insertNotBefore(functionCall, true);
			}

		} else {
			throw new IndexOutOfBoundsException("unexpected BindingLevel or RelationalExpressionType!");
		}
	}

	/**
	 * Calculates the {@link #currentComplexity} of this LogicalExpression and all its inner expressions, 
	 * as well as the {@link #negatedComplexity} of a negated expression. This helps to decide whether it would be 
	 * better to propagate negation to inner expressions, or to negate a whole (sub-)expression with NOT ( ... ). 
	 * E.g., 'IF NOT ( a = b AND c = d )' is easier to understand than 'IF a <> b OR c <> d.'
	 *  
	 * @param convertAbapFalseAndAbapTrue
	 * @throws UnexpectedSyntaxAfterChanges
	 */
	private final void calculateComplexity(boolean convertAbapFalseAndAbapTrue) {
		// calculate the complexity of inner expressions first and cumulate the result
		currentComplexity = 0;
		negatedComplexity = 0;

		for (LogicalExpression innerExpression : innerExpressions) { 
			innerExpression.calculateComplexity(convertAbapFalseAndAbapTrue);
			currentComplexity += innerExpression.currentComplexity;
			negatedComplexity += innerExpression.negatedComplexity;
		}
		
		if (bindingLevel == BindingLevel.EQUIV) {
			// EQUIV is negated with NOT EQUIV and vice versa, so inner expressions always remain unchanged
			negatedComplexity = currentComplexity;
			Token equivToken = keywords.get(0);
			if (equivToken.getPrevCodeToken().isKeyword("NOT")) {
				currentComplexity += COMPLEXITY_OF_INNER_NOT;
			} else {
				negatedComplexity += COMPLEXITY_OF_INNER_NOT;
			}

		} else if (bindingLevel == BindingLevel.OR) {
			currentComplexity += COMPLEXITY_OF_OR;
			negatedComplexity += COMPLEXITY_OF_AND;
			
		} else if (bindingLevel == BindingLevel.AND) {
			currentComplexity += COMPLEXITY_OF_AND;
			negatedComplexity += COMPLEXITY_OF_OR;
			
		} else if (bindingLevel == BindingLevel.NOT) {
			negatedComplexity = currentComplexity;
			currentComplexity += COMPLEXITY_OF_INNER_NOT;

			// BindingLevel.COMPARISON_OR_PREDICATE
		} else if (relExprType == RelationalExpressionType.COMPARISON) {
			Token compOp = keywords.get(0);
			if (convertAbapFalseAndAbapTrue && compOp.textEquals("=") && compOp.getNextCodeToken().textEquals(ABAP.ABAP_FALSE)) {
				currentComplexity += COMPLEXITY_OF_EQ_FALSE;
				negatedComplexity += COMPLEXITY_OF_EQ_TRUE;

			} else if (convertAbapFalseAndAbapTrue && compOp.textEquals("=") && compOp.getNextCodeToken().textEquals(ABAP.ABAP_TRUE)) {
				currentComplexity += COMPLEXITY_OF_EQ_TRUE;
				negatedComplexity += COMPLEXITY_OF_EQ_FALSE;
				
			} else if (compOp.isAnyComparisonOperator("IN", "BETWEEN")) {
				if (compOp.getPrevCodeToken().isKeyword("NOT")) {
					currentComplexity += COMPLEXITY_OF_INNER_NOT;
				} else {
					negatedComplexity += COMPLEXITY_OF_INNER_NOT;
				}
				
			} else if (compOp.isAnyComparisonOperator("=", "EQ", "CO", "CA", "CS", "CP", "BYTE-CO", "BYTE-CA", "BYTE-CS")) { // CO = contains only, CA = contains any, CS = contains string, CP = covers pattern
				currentComplexity += COMPLEXITY_OF_EQ;
				negatedComplexity += COMPLEXITY_OF_NE;
			} else if (compOp.isAnyComparisonOperator("<>", "NE", "CN", "NA", "NS", "NP", "BYTE-CN", "BYTE-NA", "BYTE-NS")) { // CN = contains not only, NA = contains not any, NS = contains no string, NP = no pattern
				currentComplexity += COMPLEXITY_OF_NE;
				negatedComplexity += COMPLEXITY_OF_EQ;
			} else { // compOp.isAnyComparisonOperator("<", "<=", ">=", ">", "LT", "LE", "GE", "GT")
				currentComplexity += COMPLEXITY_OF_LT_GT;
				negatedComplexity += COMPLEXITY_OF_LT_GT;
			}

		} else if (relExprType == RelationalExpressionType.PREDICATE_EXPRESSION) {
			negatedComplexity = currentComplexity;
			Token isToken = keywords.get(0);
			if (isToken.getNextCodeToken().isKeyword("NOT")) {
				currentComplexity += COMPLEXITY_OF_INNER_NOT;
			} else {
				negatedComplexity += COMPLEXITY_OF_INNER_NOT;
			}
			
		} else if (relExprType == RelationalExpressionType.PREDICATE_FUNCTION) {
			negatedComplexity = currentComplexity;
			Token functionCall = keywords.get(0);
			if (functionCall != firstToken && functionCall.getPrevCodeToken().isKeyword("NOT")) {
				// this branch is probably impossible to reach, because code like "NOT line_exists( ... )" would be handled above with BindingLevel.NOT
				currentComplexity += COMPLEXITY_OF_INNER_NOT;
			} else {
				negatedComplexity += COMPLEXITY_OF_INNER_NOT;
			}

		} else {
			throw new IndexOutOfBoundsException("unexpected BindingLevel or RelationalExpressionType!");
		}		
	}
	
	/**
	 * returns true if the logical expression was changed.
	 * 
	 * @return
	 * @throws IntegrityBrokenException 
	 */
	public final boolean transformIsNotToNotIs() throws UnexpectedSyntaxAfterChanges {
		if (!isValid)
			throw new IllegalStateException("This logical expression instance is no longer valid!");

		// descend to deeper levels
		if (bindingLevel.getValue() < BindingLevel.COMPARISON_OR_PREDICATE.getValue()) {
			boolean changed = false;
			for (LogicalExpression innerExpression : innerExpressions) {
				if (innerExpression.transformIsNotToNotIs())
					changed = true;
				isValid &= innerExpression.isValid;
			}
			return changed;
		}

		// only predicate expressions inside NOT expressions (e.g. "NOT lv_value IS ASSIGNED") are relevant
		if (relExprType != RelationalExpressionType.PREDICATE_EXPRESSION)
			return false;
		if (parentExpression == null || parentExpression.bindingLevel != BindingLevel.NOT)
			return false;

		// exclude unsupported cases
		if (isInParentheses) // this case (e.g. "IF NOT ( lv_value IS INITIAL )" is correct ABAP syntax, but not (yet) supported
			return false;
		Token isToken = keywords.get(0);
		Token next = isToken.getNextCodeSibling();
		if (next.isKeyword("NOT")) // this case (e.g. "IF NOT <x> IS NOT BOUND") is correct ABAP syntax, but not (yet) supported
			return false;

		// insert "NOT" after "IS" and remove "NOT" from before the predicate expression (e.g. "NOT lv_value IS INITIAL" -> "lv_value IS NOT INITIAL")
		next.insertLeftSibling(Token.createForAbap(0, 1, "NOT", TokenType.KEYWORD, isToken.sourceLineNum));
		Token oldNotToken = parentExpression.keywords.get(0);
		if (oldNotToken.isFirstTokenInLine() && !oldNotToken.getNext().isFirstTokenInLine())
			oldNotToken.getNext().copyWhitespaceFrom(oldNotToken);
		oldNotToken.removeFromCommand();
		// the object structure of this logical expression no longer matches the code; nevertheless we can continue and potentially process other places in this expression
		isValid = false; 
		return true;
	}

	private void insertNotBefore(Token token, boolean invalidateExpression) throws IntegrityBrokenException {
		Token notToken = Token.createForAbap(token.lineBreaks, token.spacesLeft, "NOT", TokenType.KEYWORD, token.sourceLineNum);
		token.setWhitespace();
		token.insertLeftSibling(notToken, true);

		// adjust first token, including for parent expressions
		LogicalExpression adjustLogExp = this;
		while (adjustLogExp != null) {
			if (adjustLogExp.firstToken == token)
				adjustLogExp.firstToken = notToken;
			adjustLogExp = adjustLogExp.parentExpression; 
		}
		
		if (invalidateExpression) {
			// the object structure of this logical expression no longer matches the code
			isValid = false; 
		}
	}

	private void removeLeadingNot(Token notToken) throws UnexpectedSyntaxAfterChanges {
		// adjust the first token, including for parent expressions, esp. for cases where the first token is required 
		// later to insert an opening parenthesis before it. E.g., when negating 
		// 'is_valid( ) OR NOT is_valid( ) AND is_valid( )' into 'NOT is_valid( ) AND ( is_valid( ) OR NOT is_valid( ) )',
		// the existing NOT is removed and only afterwards, parentheses are introduced
		LogicalExpression adjustLogExp = this;
		while (adjustLogExp != null && adjustLogExp.firstToken == notToken) {
			adjustLogExp.firstToken = notToken.getNextCodeToken();
			adjustLogExp = adjustLogExp.parentExpression; 
		}

		notToken.removeFromCommand();

		// the object structure of this logical expression no longer matches the code
		isValid = false; 
	}
	
	final void toTreeAlign(TreeAlign tree) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxException {
		switch (bindingLevel) {
			case EQUIV:
			case OR:
			case AND:
				tree.add(TreeAlignColumnType.OPEN_BRACKET_FOR_BOOL_OP, isInParentheses ? firstToken : null);
				for (int inner = 0; inner < innerExpressions.size(); ++inner) {
					tree.add(TreeAlignColumnType.BOOL_OPERATOR, (inner > 0) ? keywords.get(inner - 1) : null);
					innerExpressions.get(inner).toTreeAlign(tree);
				}
				tree.add(TreeAlignColumnType.CLOSE_BRACKET_FOR_BOOL_OP, isInParentheses ? lastToken : null);
				break;

			case NOT:
				tree.add(TreeAlignColumnType.OPEN_BRACKET_FOR_NOT, isInParentheses ? firstToken : null);
				tree.add(TreeAlignColumnType.NOT_KEYWORD, keywords.get(0));
				innerExpressions.get(0).toTreeAlign(tree);
				tree.add(TreeAlignColumnType.CLOSE_BRACKET_FOR_NOT, isInParentheses ? lastToken : null);
				break;

			case COMPARISON_OR_PREDICATE:
				// if this comparison or predicate is not negated with a NOT (as the parent expression), nevertheless leave room for one:
				boolean hasParentNot = (parentExpression != null && parentExpression.bindingLevel == BindingLevel.NOT);
				if (!hasParentNot) {
					tree.add(TreeAlignColumnType.OPEN_BRACKET_FOR_NOT, null);
					tree.add(TreeAlignColumnType.NOT_KEYWORD, null);
				}

				tree.add(TreeAlignColumnType.OPEN_BRACKET_FOR_REL, isInParentheses ? firstToken : null);
				if (relExprType == RelationalExpressionType.PREDICATE_FUNCTION)
					tree.add(TreeAlignColumnType.REL_FUNCTION, getFirstTokenExceptOpeningParenthesis(),
							getLastTokenExceptClosingParenthesis());
				else {
					tree.add(TreeAlignColumnType.REL_TERM1, getFirstTokenExceptOpeningParenthesis(), keywords.get(0).getPrevCodeToken());
					tree.add(TreeAlignColumnType.REL_OPERATOR, keywords.get(0));
					tree.add(TreeAlignColumnType.REL_TERM2, keywords.get(0).getNextCodeToken(), getLastTokenExceptClosingParenthesis());
				}
				tree.add(TreeAlignColumnType.CLOSE_BRACKET_FOR_REL, isInParentheses ? lastToken : null);

				if (!hasParentNot)
					tree.add(TreeAlignColumnType.CLOSE_BRACKET_FOR_NOT, null);
				break;
			default:
				throw new IndexOutOfBoundsException("unexpected BindingLevel!");
		}

	}
}
