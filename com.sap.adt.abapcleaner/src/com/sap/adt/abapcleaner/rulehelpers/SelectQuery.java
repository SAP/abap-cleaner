package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class SelectQuery {
	/** the start query in a 'chain' of UNION / INTERSECT / EXCEPT queries; null if this is the start query */
	public final SelectQuery startQuery;
	/** the previous query in a 'chain' of UNION / INTERSECT / EXCEPT queries */
	public final SelectQuery prevQuery;
	/** the next query in a 'chain' of UNION / INTERSECT / EXCEPT queries */
	private SelectQuery nextQuery;
	
	private Token startToken;
	private Token endToken;
	/** true if the clauses of a main query are split between a beginning and final INTO etc. clauses, 
	 * with one or several SELECTs between them */
	private boolean areClausesSplit;
	
	/** the SELECT clauses that were found for this query, regardless of their sequence */
	private Term[] clauses = new Term[SelectClause.length];
	private ArrayList<Term> clauseTermsInOrder = new ArrayList<>();
	private ArrayList<SelectClause> clauseTypesInOrder = new ArrayList<>();
	
	public Token getSelectToken() { return getSelectClause().firstToken; }
	public Term getSelectClause() { return getClause(SelectClause.SELECT); }

	public Term getClause(SelectClause clause) { return clauses[clause.getValue()]; }
	public boolean hasClause(SelectClause clause) { return (clauses[clause.getValue()] != null); }
	public ArrayList<SelectClause> getClausTypesInOrder() { return clauseTypesInOrder; }

	public boolean isMainQuery() { return isStartQuery() && getSelectClause() != null && getSelectClause().firstToken.getParent() == null; }
	public boolean isStartQuery() { return (startQuery == null); }
	public SelectQuery getNextQuery() { return nextQuery; }

	/**
	 * Returns one or several 'chains' of queries found in the supplied Command. 
	 * A chain 
	 * @param command
	 * @return
	 * @throws UnexpectedSyntaxException
	 */
	public static ArrayList<SelectQuery> createQueryChainsFrom(Command command) throws UnexpectedSyntaxException {
		ArrayList<SelectQuery> queryChains = new ArrayList<>();
		HashSet<Token> selectTokens = new HashSet<>();
		
		Token token = command.getFirstCodeToken();
		while (token != null) {
			// process the beginning(!) of a new SELECT query chain
			if (token.isKeyword("SELECT") && !selectTokens.contains(token)) {
				// read the start query and all following UNION / INTERSECT / EXCEPT queries
				SelectQuery startQuery = null;
				SelectQuery prevQuery = null;
				Token queryStartToken = token;
				HashSet<Token> openingParentheses = new HashSet<>();
				do {
					SelectQuery newQuery = new SelectQuery(startQuery, prevQuery);
					queryStartToken = newQuery.read(queryStartToken, openingParentheses);
						
					if (newQuery.hasClause(SelectClause.SELECT)) {
						// remember the SELECT keyword to prevent queries after UNION etc. from being added as new chains later
						selectTokens.add(newQuery.getSelectClause().firstToken);

						if (startQuery == null) {
							startQuery = newQuery;
							queryChains.add(newQuery);
						}
						if (prevQuery != null) {
							prevQuery.nextQuery = newQuery;
						}
						prevQuery = newQuery;
					}
					
					// final ORDER BY, INTO|APPENDING, and abap_options belong to the start query (if that is a main query)
					if (queryStartToken != null && queryStartToken.isAnyKeyword("ORDER", "INTO", "APPENDING", "PRIVILEGED", "BYPASSING", "CONNECTION") && startQuery != null)
						queryStartToken = startQuery.read(queryStartToken, openingParentheses);
					
				} while (queryStartToken != null && !queryStartToken.isPeriod());
			}
			// move inside parentheses etc. to find subqueries, too 
			token = token.getNextCodeToken();
		}
		
		return queryChains;
	}
	
	private SelectQuery(SelectQuery startQuery, SelectQuery prevQuery) throws UnexpectedSyntaxException {
		this.startQuery = startQuery;
		this.prevQuery = prevQuery;
		this.areClausesSplit = false;
	}
	
	private Token read(Token start, HashSet<Token> openingParentheses) throws UnexpectedSyntaxException {
		if (this.startToken == null) {
			this.startToken = start;
		} else {
			areClausesSplit = true;
		}
		
		Token token = start;
		Token lastToken = start;
		Token curClauseStart = null;
		SelectClause curClause = SelectClause.NONE; 
		
		while (token != null) {
			SelectClause nextClause = SelectClause.NONE;
			if (token.startsDdlUnionEtc()) {
				if (!hasClause(SelectClause.UNION) && !hasClause(SelectClause.SELECT)) {
					nextClause = SelectClause.UNION;
				} else {
					break;
				}
				
			} else if (token.textEquals("(") && curClause == SelectClause.UNION) {
				// only in this case, move into the parentheses; use 'continue' so lastToken is NOT updated
				openingParentheses.add(token);
				token = token.getNextCodeToken();
				continue; 
			} else if (token.getOpensLevel()) {
				while (token.getOpensLevel()) {
					token = token.getNextSibling();
				}

			} else if (token.closesLevel()) {
				break;

			} else if (token.isPeriod()) {
				break;
			} else if (!token.isKeyword()) {
				// continue reading
				
			} else if (!hasClause(SelectClause.SELECT) && token.isKeyword("SELECT")) {
				nextClause = SelectClause.SELECT;

			} else if (!hasClause(SelectClause.FROM) && token.isKeyword("FROM")) {
				nextClause = SelectClause.FROM;
			
			} else if (token.matchesOnSiblings(true, "CORRESPONDING", "FIELDS")) {
				// skip this usage of 'FIELDS'
				token = token.getNextCodeSibling();
			} else if (!hasClause(SelectClause.FIELDS) && token.isKeyword("FIELDS")) {
				nextClause = SelectClause.FIELDS;

			} else if (!hasClause(SelectClause.FOR_ALL_ENTRIES) && token.matchesOnSiblings(true, "FOR", "ALL", "ENTRIES", "IN"))  {
				nextClause = SelectClause.FOR_ALL_ENTRIES;
			
			} else if (!hasClause(SelectClause.WHERE) && token.isKeyword("WHERE")) {
				nextClause = SelectClause.WHERE;
			
			} else if (!hasClause(SelectClause.GROUP_BY) && token.matchesOnSiblings(true, "GROUP", "BY"))  {
				nextClause = SelectClause.GROUP_BY;
			
			} else if (!hasClause(SelectClause.HAVING) && token.isKeyword("HAVING")) {
				nextClause = SelectClause.HAVING;
			
			} else if (token.matchesOnSiblings(true, "ORDER", "BY")) {
				// it seems ORDER BY can appear both as a 'query clause' and (again) as a 'mainquery clause'
				if (!hasClause(SelectClause.ORDER_BY)) {
					nextClause = SelectClause.ORDER_BY;
				} else {
					break;
				}
			
			} else if (!hasClause(SelectClause.DB_HINTS) && token.isKeyword("%_HINTS")) {
				nextClause = SelectClause.DB_HINTS;

			} else if (token.isAnyKeyword("INTO", "APPENDING")) {
				if (!hasClause(SelectClause.INTO) && isStartQuery()) {
					nextClause = SelectClause.INTO;
				} else {
					break;
				}

			} else if (!hasClause(SelectClause.UP_TO_OFFSET) && (token.matchesOnSiblings(true, "UP", "TO") || token.isKeyword("OFFSET")))  {
				nextClause = SelectClause.UP_TO_OFFSET;
			
			} else if (!hasClause(SelectClause.ABAP_OPTIONS) && (token.matchesOnSiblings(true, "PRIVILEGED", "ACCESS") || token.matchesOnSiblings(true, "BYPASSING", "BUFFER") || token.isKeyword("CONNECTION")))  {
				if (isStartQuery()) {
					nextClause = SelectClause.ABAP_OPTIONS;
				} else {
					break;
				}
			}

			if (nextClause != SelectClause.NONE) {
				addClause(curClause, curClauseStart, lastToken);
				// start new clause
				curClause = nextClause;
				curClauseStart = token;
			}
			
			lastToken = token;
			token = token.getNextCodeSibling();
		}

		// skip closing parentheses that were opened earlier
		if (token == null) {
			token = lastToken.getNextCodeToken();
			while (token != null && token.textEquals(")") && openingParentheses.contains(token.getPrevSibling())) {
				token = token.getNextCodeToken();
			}
		}

		addClause(curClause, curClauseStart, lastToken);
		endToken = token;

		// if no progress was made in this .read(), return null to prevent endless loops
		return (token == start) ? null : token;
	}
	
	private void addClause(SelectClause curClause, Token curClauseStart, Token lastToken) throws UnexpectedSyntaxException {
		if (curClause != SelectClause.NONE && curClauseStart != null) { 
			clauses[curClause.getValue()] = Term.createForTokenRange(curClauseStart, lastToken);
			clauseTermsInOrder.add(clauses[curClause.getValue()]);
			clauseTypesInOrder.add(curClause);
		}
	}

	/** returns true if the select list follows after SELECT (non-strict mode), NOT after FIELDS */
	public boolean hasSelectListInSelectClause() {
		Term selectClause = getSelectClause();
		Token testToken = selectClause.firstToken;
		while (testToken != null && testToken.isAnyKeyword("SELECT", "SINGLE", "FOR", "UPDATE"))
			testToken = testToken.getNextCodeSibling();
		return (testToken != selectClause.lastToken.getNextCodeSibling());
	}

	/** returns true if the FROM clause contains at least one JOIN or CDS View parameters */
	public boolean hasJoinOrParameters() {
		Term fromClause = getClause(SelectClause.FROM);
		if (fromClause == null)
			return false;
		Token joinToken = fromClause.findSiblingOfTypeAndTexts(TokenType.KEYWORD, "JOIN");
		Token closingParenthesis = fromClause.findSiblingOfTypeAndTexts(TokenType.OTHER_OP, ")");
		return (joinToken != null || closingParenthesis != null);
	}

	/** returns true if the INTO clause has the form INTO (dobj1, dobj2, ...) */
	public boolean hasIntoList() {
		Term intoClause = getClause(SelectClause.INTO);
		if (intoClause == null)
			return false;
		Token openingParenthesis = intoClause.findSiblingOfTypeAndTexts(TokenType.OTHER_OP, "(");
		return (openingParenthesis != null);
	}

	/** returns true if the query currently is a one-liner */
	public boolean isOneLiner() {
		if (startToken == endToken)
			return true;
		Token token = startToken.getNext();
		while (token != null && token != endToken) {
			if (token.lineBreaks > 0) 
				return false;
			token = token.getNext();
		}
		return true;
	}

	/** returns the (potential) length of this query as a one-liner, starting from the SELECT keyword, 
	 * or -1 if the query contains comments, inner queries, or exceeds 255 chars */
	public int getLengthOnOneLine() {
		if (areClausesSplit)
			return -1;
		
		int sumLen = 0;
		Token token = getSelectToken();
		while (token != null && token != endToken) {
			if (token.isComment())
				return -1;
			// assume only 1 space between non-attached Tokens
			if (token != startToken && !token.isAttached())
				++sumLen;
			sumLen += token.getTextLength();
			if (sumLen > ABAP.MAX_LINE_LENGTH)
				return -1;
			token = token.getNext();
		}
		return sumLen;
	}
	
	/** condenses to one line, starting from the SELECT keyword; returns true if any whitespace was changed */
	public boolean condense() {
		if (areClausesSplit)
			return false;
		
		boolean changed = false;
		Token token = getSelectToken();
		while (token != null && token != endToken) {
			if (token.getPrev() != null && token.getPrev().isComment()) {
				break;
			} else if (token == startToken || token.isAttached()) {
				// keep whitespace
			} else if (token.setWhitespace()) {
				changed = true;
			}
			token = token.getNext();
		}
		return changed;
	}

	public SelectClause getNextClauseInOrder(SelectClause clause) {
		int index = clauseTypesInOrder.indexOf(clause);
		return (index >= 0 && index + 1 < clauseTypesInOrder.size()) ? clauseTypesInOrder.get(index + 1) : SelectClause.NONE; 
	}
}
