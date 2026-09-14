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
			
			} else if (curClause == SelectClause.FROM && token.matchesOnSiblings(true, "WITH", "PRIVILEGED", "ACCESS")) {
				// skip 'WITH PRIVILEGED ACCESS' in the FROM clause to avoid mixing it up with the 'PRIVILEGED ACCESS' clause and starting SelectClause.ABAP_OPTIONS
				token = token.getNextCodeSibling().getNextCodeSibling();
				
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
	
	/** Tries to determine whether the query uses strict mode. If this cannot be determined (because not all rules are 
	 * covered here), AbapSqlMode.UNCERTAIN is returned. */
	public AbapSqlMode determineAbapSqlMode() {
		// maxRelease (not yet unused): if new syntax (such as the FINAL keyword) shall be introduced to the code, it should   
		// be ensured that the code already complies with the 'strict mode requirements' of that release. maxRelease contains
		// the release up to which requirements are sure to be met (because corresponding syntax is found in the statement). 
		// However, as of now, we only try to find out whether the query is definitely NOT in strict mode.
		int maxRelease = 0;
		
		// (7.60) The INTO clause must be specified as the last clause of a SELECT statement and the ABAP-specific additions OPTIONS of the SELECT statement must be specified after the INTO clause.
		// (7.63) In the statement OPEN CURSOR, the additions grouped under UP TO, OFFSET, and OPTIONS must also be specified at the end of the SELECT statement.
		ArrayList<SelectClause> clauseTypes = getClausTypesInOrder();
		int intoIndex = clauseTypes.indexOf(SelectClause.INTO);
		int uptoIndex = clauseTypes.indexOf(SelectClause.UP_TO_OFFSET);
		int abapOptionsIndex = clauseTypes.indexOf(SelectClause.ABAP_OPTIONS);
		if (intoIndex >= 0) {
			if (uptoIndex >= 0 && uptoIndex < intoIndex || abapOptionsIndex >= 0 && abapOptionsIndex < intoIndex) {
				return AbapSqlMode.NOT_STRICT;
			}
			for (int i = intoIndex + 1; i < clauseTypes.size(); ++i) {
				SelectClause clauseType = clauseTypes.get(i);
				if (clauseType != SelectClause.UP_TO_OFFSET && clauseType != SelectClause.ABAP_OPTIONS) {
					return AbapSqlMode.NOT_STRICT;
				}
			}
			// (7.40, SP08) Arrangement of the INTO clause as the final clause of a SELECT statement. => strict mode
			maxRelease = Math.max(maxRelease, 740);
		}

		// (7.96) explicit usage of addition OPTIONS => strict mode
		Term abapOptionsClause = getClause(SelectClause.ABAP_OPTIONS);
		if (abapOptionsClause != null && !abapOptionsClause.firstToken.isKeyword("OPTIONS")) {
			return AbapSqlMode.NOT_STRICT;
		}

		for (Term clause : clauseTermsInOrder) {
			// (7.40, SP05) Lists of operands as comma-separated lists => strict mode
			if (clause.findSiblingOfTypeAndTexts(TokenType.OTHER_OP, ",") != null) {
				maxRelease = Math.max(maxRelease, 740);
			}
			// (7.40, SP05) Addition USING CLIENT => strict mode
			if (clause.findSiblingOfTypeAndTexts(TokenType.KEYWORD, "USING", "CLIENT") != null) {
				maxRelease = Math.max(maxRelease, 740);
			}
			// (7.77) The obsolete use of the addition CLIENT SPECIFIED is forbidden in the statement SELECT in queries and subqueries.
			// (7.77) The obsolete use of the addition CLIENT SPECIFIED in write statements is forbidden.
			// (7.67) The addition CLIENT SPECIFIED cannot be specified for a data source specified using a path expression.
			if (clause.findSiblingOfTypeAndTexts(TokenType.KEYWORD, "CLIENT", "SPECIFIED") != null) {
				return AbapSqlMode.NOT_STRICT;
			}

			Token token = clause.firstToken;
			while (token != null && token != clause.lastToken.getNext()) {
				// (7.40, SP05) Escape character @ in front of host variables => strict mode
				if (token.isIdentifier() && token.textStartsWith("@")) {
					maxRelease = Math.max(maxRelease, 740);
				}
				// (7.60) Use of host expressions
				if (token.textEquals("@(")) {
					maxRelease = Math.max(maxRelease, 760);
				}

				if (token.isIdentifier() && token.getOpensLevel()) {
					if (token.textEqualsAny("SERIES_GENERATE_DATE(", "SERIES_GENERATE_INTEGER(", "SERIES_GENERATE_TIME(", "SERIES_GENERATE_TIMESTAMP(", "SUBSTRING_REGEXPR(")) {
						// (7.84) Use of the CDS system table functions SERIES_GENERATE_DATE, SERIES_GENERATE_INTEGER, SERIES_GENERATE_TIME,
						// and SERIES_GENERATE_TIMESTAMP.
						// (7.84) Use of the string function SUBSTRING_REGEXPR.
						maxRelease = Math.max(maxRelease, 784);

					} else if (token.textEqualsAny("INITCAP(", "IS_VALID(", "EXTRACT_YEAR(", "EXTRACT_MONTH(", "EXTRACT_DAY(", "EXTRACT_HOUR(", "EXTRACT_MINUTE(", "EXTRACT_SECOND(",
							"DAYNAME(", "MONTHNAME(", "WEEKDAY(", "DAYS_BETWEEN(", "ADD_DAYS(", "ADD_MONTHS(", "UNIT_CONVERSION(")) {
						// (7.83) Use of the string function INITCAP.
						// (7.83) Use of one of the new date/time functions IS_VALID, EXTRACT_YEAR, EXTRACT_MONTH, EXTRACT_DAY, EXTRACT_HOUR, EXTRACT_MINUTE,
						// EXTRACT_SECOND, DAYNAME, MONTHNAME, WEEKDAY, DAYS_BETWEEN, ADD_DAYS, and ADD_MONTHS.
						// (7.83) Use of the function UNIT_CONVERSION.
						maxRelease = Math.max(maxRelease, 783);
					
					} else if (token.textEqualsAny("REPLACE_REGEXPR(", "LIKE_REGEXPR(", "OCCURRENCES_REGEXPR(")) {
						// (7.81) Use of the string functions REPLACE_REGEXPR, LIKE_REGEXPR, and OCCURRENCES_REGEXPR.
						// (7.81) Use,
						maxRelease = Math.max(maxRelease, 781);
					
					} else if (token.textEqualsAny("TO_BLOB(")) {
						// (7.80) Use of the SQL function TO_BLOB
						maxRelease = Math.max(maxRelease, 780);
					
					} else if (token.textEqualsAny("NTILE(", "TO_CLOB(")) {
						// (7.79) Use of the window function NTILE
						// (7.79) Use of the SQL function TO_CLOB

						maxRelease = Math.max(maxRelease, 779);
					
					} else if (token.textEqualsAny("ALLOW_PRECISION_LOSS(", "FIRST_VALUE(", "LAST_VALUE(", "TSTMPL_TO_UTCL(", "TSTMPL_FROM_UTCL(", "DATS_TO_DATN(", "DATS_FROM_DATN(",
							"TIMS_TO_TIMN(", "TIMS_FROM_TIMN(", "UTCL_CURRENT(", "UTCL_ADD_SECONDS(", "UTCL_SECONDS_BETWEEN(", "DATN_ADD_DAYS(", "DATN_ADD_MONTHS(", "DATN_DAYS_BETWEEN(",
							"MEDIAN(", "STDDEV(", "VAR(", "CORR(", "CORR_SPEARMAN(", "as_geo_json(")) {
						// (7.78) Use of the function ALLOW_PRECISION_LOSS
						// (7.78) Window functions FIRST_VALUE and LAST_VALUE.
						// (7.78) Use of one of the new date/time functions TSTMPL_TO_UTCL, TSTMPL_FROM_UTCL, DATS_TO_DATN, DATS_FROM_DATN, TIMS_TO_TIMN,
						// TIMS_FROM_TIMN, UTCL_CURRENT, UTCL_ADD_SECONDS, UTCL_SECONDS_BETWEEN, DATN_ADD_DAYS, DATN_ADD_MONTHS, and DATN_DAYS_BETWEEN.
						// (7.78) Use of one of the new aggregate functions MEDIAN, STDDEV, VAR, CORR, and CORR_SPEARMAN.
						// (7.78) New geometry conversion function as_geo_json.
						maxRelease = Math.max(maxRelease, 778);
					
					} else if (token.textEqualsAny("UUID(", "LEAD(", "LAG(")) {
						// (7.77) Use of the built-in function UUID.
						// (7.77) Use of the window functions LEAD and LAG.
						maxRelease = Math.max(maxRelease, 777);
					
					} else if (token.textEqualsAny("STRING_AGG(", "HIERARCHY_DESCENDANTS_AGGREGATE(")) {
						// (7.75) Use of the aggregate function STRING_AGG.
						// (7.75) Use of the hierarchy navigator HIERARCHY_DESCENDANTS_AGGREGATE.
						maxRelease = Math.max(maxRelease, 775);
					
					} else if (token.textEqualsAny("ABAP_SYSTEM_TIMEZONE(", "ABAP_USER_TIMEZONE(")) {
						// (7.72) Use of one of the new time zone functions ABAP_SYSTEM_TIMEZONE or ABAP_USER_TIMEZONE.
						maxRelease = Math.max(maxRelease, 772);
					
					} else if (token.textEqualsAny("TIMS_IS_VALID(", "TSTMP_IS_VALID(", "TSTMP_CURRENT_UTCTIMESTAMP(", "TSTMP_SECONDS_BETWEEN(", "TSTMP_ADD_SECONDS(", "TSTMP_TO_DATS(",
							"TSTMP_TO_TIMS(", "TSTMP_TO_DST(", "DATS_TIMS_TO_TSTMP(")) {
						// (7.71) Use of one of the new date/time functions TIMS_IS_VALID, TSTMP_IS_VALID, TSTMP_CURRENT_UTCTIMESTAMP, TSTMP_SECONDS_BETWEEN,
						// TSTMP_ADD_SECONDS, TSTMP_TO_DATS, TSTMP_TO_TIMS, TSTMP_TO_DST, or DATS_TIMS_TO_TSTMP.
						maxRelease = Math.max(maxRelease, 771);
					
					} else if (token.textEqualsAny("BINTOHEX(", "HEXTOBIN(")) {
						// (7.68) Use of the type conversion functions BINTOHEX and HEXTOBIN
						maxRelease = Math.max(maxRelease, 768);
					
					} else if (token.textEqualsAny("DIVISION(", "LOWER(", "UPPER(", "DATS_IS_VALID(", "DATS_DAYS_BETWEEN(", "DATS_ADD_DAYS(", "DATS_ADD_MONTHS(")) {
						// (7.65) Use of new SQL functions: Numeric function DIVISION, String functions LOWER and UPPER, Date functions DATS_IS_VALID,
						// DATS_DAYS_BETWEEN, DATS_ADD_DAYS and DATS_ADD_MONTHS
						maxRelease = Math.max(maxRelease, 765);
					
					} else if (token.textEqualsAny("LEFT(", "CONCAT_WITH_SPACE(", "INSTR(", "RPAD(")) {
						// (7.63) Use of the string functions LEFT, CONCAT_WITH_SPACE, INSTR, and RPAD
						maxRelease = Math.max(maxRelease, 763);

					} else if (token.textEqualsAny("ROUND(", "CONCAT(", "LPAD(", "LENGTH(", "LTRIM(", "REPLACE(", "RIGHT(", "RTRIM(", "SUBSTRING")) {
						// (7.62) Use of the numeric function ROUND or the string functions CONCAT, LPAD, LENGTH, LTRIM, REPLACE, RIGHT, RTRIM, and SUBSTRING.
						maxRelease = Math.max(maxRelease, 762);
					}
				}

				token = token.getNext();
			}
		}

		return AbapSqlMode.UNCERTAIN;
	}
}
