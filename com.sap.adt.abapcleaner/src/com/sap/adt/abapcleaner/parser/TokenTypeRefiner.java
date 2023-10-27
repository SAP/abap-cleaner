package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.ABAP;

/**
 * <p>Implementation of the {@link ITokenTypeRefiner} interface which uses ABAP cleaner-internal means of 
 * refining the {@link TokenType}s that were preliminary determined in {@link Token#Token(int, int, String, int)}, 
 * especially by differentiating between {@link TokenType#KEYWORD}s and {@link TokenType#IDENTIFIER}s.</p> 
 *
 * <p>This implementation only covers some common cases and is neither perfect nor complete; 
 * its results are therefore mostly overwritten by the results of the {@link TokenTypeRefinerRnd},
 * but are kept for code that is considered erroneous by the RND Parser.</p> 
 */
public class TokenTypeRefiner implements ITokenTypeRefiner {
	public static TokenTypeRefiner create() {
		return new TokenTypeRefiner();
	}
	
	@Override
	public void refine(Command command) {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null || !command.isAbap())
			return;
		
		{
			Token token = firstCode;
			while (token != null) {
				if (token.type == TokenType.KEYWORD || (token.type == TokenType.COMPARISON_OP && token.startsWithLetter())) {
					// use TokenType "keyword" for "DATA(", "FINAL(", "FIELD-SYMBOL(" and "VALUE(", but not for "lines(" (as opposed to "LINES OF")
					if (token.textEndsWith("(") && !token.opensInlineDeclaration() && !token.textEquals("VALUE(") &&
							(!firstCode.isKeyword("SELECT") || !token.textEqualsAny(ABAP.abapSqlFunctions))) {
						token.type = TokenType.IDENTIFIER;
					}
					
					// Tokens followed by assignment operators, comparison operators or "TYPE" cannot be ABAP keywords
					Token nextToken = token.getNext();
					if (nextToken != null) {
						if (nextToken.isAssignmentOperator() && !token.isKeyword("OTHERS")) { 
							// exception: "OTHERS = ..." in "CALL METHOD ... EXCEPTIONS ..."
							token.type = TokenType.IDENTIFIER;
						}
						if (nextToken.isComparisonOperator() && !token.matchesOnSiblings(true, "NOT", "IN|BETWEEN")) { 
							// exception: "NOT" in the logical expression "<identifier> NOT IN <range table>" or "operand NOT BETWEEN operand1 AND operand2"
							token.type = TokenType.IDENTIFIER;
						}
						// the following should be handled later:
						// else if (token.getNext().textEquals("TYPE") && !token.textEqualsAny("EXCEPTION", "INCLUDE", "READ-ONLY"))
						//    token.type = TokenType.IDENTIFIER;
					}
					Token prevToken = token.getPrev();
					if (prevToken != null) {
						if (prevToken.isChainColon())
							prevToken = prevToken.getPrev();
						if (prevToken != null && prevToken.getMayBeIdentifier() && prevToken.isAnyKeyword("METHOD", "METHODS", "CLASS-METHODS", "TYPE-POOLS"))
							token.type = TokenType.IDENTIFIER;
					}
				}
				token = token.getNext();
			}
		}
		
		// - TYPES ... TABLE OF ... WITH ... KEY [<identifier> [ALIAS <identifier>] COMPONENTS] <identifiers> [WITH ...] [INITIAL SIZE n]
		if (firstCode.isAnyKeyword("TYPES", "TYPES:")) {
			Token withKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "TABLE", "OF", TokenSearch.ASTERISK, "WITH");
			while (withKeyword != null) {
				// WITH [UNIQUE, NON-UNIQUE, DEFAULT, EMPTY, HASHED, SORTED] KEY
				Token keyKeyword = withKeyword.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "KEY");
				if (keyKeyword == null) // e.g. "WITH UNIQUE KEY ... WITH FURTHER SECONDARY KEYS."
					break;
				Token token = keyKeyword.getNext();
				while (token != null && token.getMayBeIdentifier() && !token.isCommaOrPeriod() && !token.textEquals("WITH")) {
					if (!token.isAnyKeyword("ALIAS", "COMPONENTS", "INITIAL", "SIZE"))
						token.type = TokenType.IDENTIFIER;
					token = token.getNext();
				}
				withKeyword = token.isKeyword("WITH") ? token : null;
			}
		}

		// - APPEND / LOOP / DELETE / INSERT / READ ... USING KEY <identifier>
		// TODO: also consider WITH TABLE KEY [key_name|(name) COMPONENTS] { comp_name1 | (name1)} = dobj1 { comp_name2 | (name2)} = dobj2
		if (firstCode.isAnyKeyword("APPEND", "DELETE", "INSERT", "LOOP", "READ")) {
			Token usingKey = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "USING", "KEY");
			Token identifier = (usingKey == null) ? null : usingKey.getNextCodeSibling();
			if (identifier != null && identifier.getMayBeIdentifier())
				identifier.type = TokenType.IDENTIFIER;
		}

		// - SORT ... BY <identifier> [ASCENDING | DESCENDING] [AS TEXT]
		if (firstCode.isKeyword("SORT")) {
			Token byKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "BY");
			Token token = (byKeyword == null) ? null : byKeyword.getNextCodeSibling();
			while (token != null && token.getMayBeIdentifier() && !token.isCommaOrPeriod()) {
				if (!token.isAnyKeyword("ASCENDING", "DESCENDING", "AS", "TEXT"))
					token.type = TokenType.IDENTIFIER;
				token = token.getNextCodeSibling();
			}
		}

		// CLEAR <identifier> ..., <identifier> ..., ...
		if (firstCode.isKeyword("CLEAR")) {
			Token token = firstCode.getNextCodeSibling();
			boolean isChain = (token != null && token.isChainColon());
			if (isChain)
				token = token.getNextCodeSibling();
			while (token != null) {
				if (token.getMayBeIdentifier())
					token.type = TokenType.IDENTIFIER;
				if (!isChain)
					break;
				token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",");
				token = (token == null) ? null : token.getNextCodeSibling();
			}
		}

		// - DELETE ADJACENT DUPLICATES FROM ... COMPARING <identifiers>.
		if (firstCode.matchesOnSiblings(true, "DELETE", "ADJACENT", "DUPLICATES", "FROM")) {
			Token comparingKeyword = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "COMPARING");
			Token token = (comparingKeyword == null) ? null : comparingKeyword.getNextCodeSibling();
			if (token != null && !token.matchesOnSiblings(true, "ALL", "FIELDS")) {
				while (token != null && token.getMayBeIdentifier() && !token.isCommaOrPeriod()) {
					token.type = TokenType.IDENTIFIER;
					token = token.getNextCodeSibling();
				}
			}
		}

		// the ABAP keyword "RESULT" is used in very specific contexts only (and then only once within the command); 
		// in all other cases, it is an identifier.  
		if (!command.isCommentLine() && firstCode.matchesDeep(true, TokenSearch.ASTERISK, "RESULT")) {
			// identify the "RESULT" keyword in the statement (if there is any) 
			Token resultKeyword = null;
			resultKeyword = firstCode.getLastTokenOnSiblings(true, "CALL TRANSFORMATION", TokenSearch.ASTERISK, "RESULT");
			if (resultKeyword == null)
				resultKeyword = firstCode.getLastTokenOnSiblings(true, "TYPES", TokenSearch.ASTERISK, "FOR READ RESULT|FOR ACTION RESULT");
			if (resultKeyword == null)
				resultKeyword = firstCode.getLastTokenOnSiblings(true, "METHODS", TokenSearch.ASTERISK, "FOR", TokenSearch.ASTERISK, "RESULT");
			if (resultKeyword == null)
				resultKeyword = firstCode.getLastTokenOnSiblings(true, "MODIFY|READ", TokenSearch.ASTERISK, "ENTITY", TokenSearch.ASTERISK, "RESULT");
			if (resultKeyword == null)
				resultKeyword = firstCode.getLastTokenOnSiblings(true, "SELECT|FETCH NEXT CURSOR", TokenSearch.ASTERISK, "EXTENDED RESULT");
			if (resultKeyword == null) {
				resultKeyword = firstCode.getLastTokenOnSiblings(true, "SCAN", TokenSearch.ASTERISK, "RESULT INTO");
				if (resultKeyword != null)
					resultKeyword = resultKeyword.getPrevCodeSibling(); // move back from the "INTO" keyword to the "RESULT" keyword
			}
			// set any Token "result" to type .IDENTIFIER, except the resultKeyword (if any was found) 
			Token token = firstCode;
			while (token != null) {
				if (token.isKeyword("RESULT") && token != resultKeyword)
					token.type = TokenType.IDENTIFIER;
				token = token.getNext();
			}
		}
		
		Token nextNext = (firstCode.getNext() == null) ? null : firstCode.getNext().getNext(); 
		if (firstCode.isKeyword("ALIASES") && nextNext != null && nextNext.getNext() != null && nextNext.isKeyword("FOR")) {
			Token identifier = firstCode.getNext();
			if (identifier.getMayBeIdentifier())
				identifier.type = TokenType.IDENTIFIER;
			identifier = identifier.getNext().getNext();
			if (identifier.getMayBeIdentifier())
				identifier.type = TokenType.IDENTIFIER;
		}

		// use TokenType.IDENTIFIER for "var" / "dtype" as well as "type" / "struc_type" / "abap_type" in declarations (CONSTANTS, DATA, FIELD-SYMBOLS, TYPES, CLASS-DATA)
		if (firstCode.isAnyKeyword(Command.declarationKeywords)) {
			boolean isFieldSymbol = firstCode.isKeyword("FIELD-SYMBOLS");
			boolean isChain = command.isSimpleChain();
			Token token = firstCode.getNext();
			if (token.isChainColon())
				token = token.getNext();
			do {
				if (!isFieldSymbol && token.matchesOnSiblings(true, "BEGIN|END", "OF")) {
					token = token.getNextCodeSibling().getNextCodeSibling();
					if (token.isAnyKeyword("ENUM", "MESH"))
						token = token.getNextCodeSibling();
				} else {
					token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "TYPE|LIKE");
					if (token == null)
						break;

					// 1. the Token before TYPE or LIKE is an identifier
					Token identifier = token.getPrevCodeSibling();
					if (identifier.getMayBeIdentifier())
						identifier.type = TokenType.IDENTIFIER;

					token = token.getNextCodeSibling();
					if (token.matchesOnSiblings(true, "LINE", "OF"))
						token = token.getNextCodeSibling().getNextCodeSibling();
					else if (!isFieldSymbol && token.matchesOnSiblings(true, "RANGE", "OF"))
						token = token.getNextCodeSibling().getNextCodeSibling();
					else if (token.matchesOnSiblings(true, "TABLE|RESPONSE", "FOR")) { // cp. ABAP Reference, "Type of Output Parameter"
						token = token.getNextCodeSibling().getNextCodeSibling();
						if (token.isAnyKeyword("FAILED", "MAPPED", "REPORTED")) {
							token = token.getNext();
							if (token.isKeyword("LATE"))
								token = token.getNext();
						} else if (token.matchesOnSiblings(true, "READ|ACTION", "RESULT"))
							token = token.getNextCodeSibling().getNextCodeSibling();
					} else {
						if (!isFieldSymbol) {
							Token ofToken = token.getLastTokenOnSiblings(true, "STANDARD TABLE|SORTED TABLE|HASHED TABLE|TABLE", "OF");
							if (ofToken != null)
								token = ofToken.getNextCodeSibling();
						}
						Token toToken = (token == null) ? null : token.getLastTokenOnSiblings(true, "REF", "TO"); // may appear as "TYPE REF TO" or as "TYPE ... TABLE OF REF TO"
						if (toToken != null)
							token = toToken.getNextCodeSibling();
					}
				}
				//
				if (token != null && token.getMayBeIdentifier())
					token.type = TokenType.IDENTIFIER;

				if (isChain && token != null)
					token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",");
				else
					token = null;
			} while (token != null);
		}

		// the only keywords possible after an assignment operator = are constructor operators (NEW, VALUE, CONV, ...), "DATA(" and "FINAL(",
		// except inside an embedded expression in a string template (see ABAP Reference, "string_tmpl - embedded_expressions", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenstring_templates_expressions.htm)
		// such as "rv_srcdoc_output = |{ is_srcdoc-srcdoc_type }/{ is_srcdoc-srcdoc_id ALPHA = OUT }|."
		{
			boolean isInsideEmbeddedExpression = false;
			Token token = firstCode;
			while (token != null) {
				if (token.endsEmbeddedExpression())
					isInsideEmbeddedExpression = false;
				// do NOT append with "else if", as the same string literal "}...{" may end an embedded expression and start the next one
				if (token.startsEmbeddedExpression())
					isInsideEmbeddedExpression = true;

				if (!isInsideEmbeddedExpression && token.getMayBeIdentifier() && token.getPrev() != null && token.getPrev().isAssignmentOperator()
						&& !token.isAnyKeyword(ABAP.constructorOperators) && !token.opensInlineDeclaration())
					token.type = TokenType.IDENTIFIER;
				token = token.getNext();
			}
		}

		// correct "HIGH" and "LOW" in "SET RUN TIME CLOCK RESOLUTION HIGH|LOW" from .IDENTIFIER to .KEYWORD
		Token highLowToken = firstCode.getLastTokenOnSiblings(true, "SET", "RUN", "TIME", "CLOCK", "RESOLUTION", "HIGH|LOW");
		if (highLowToken != null)
			highLowToken.type = TokenType.KEYWORD;
	}
}
