package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class ReadTableCommand {
	// READ TABLE itab { table_key | free_key | index } result.
	// 
	// table_key ->   { FROM wa [USING KEY keyname] }
	//              | { WITH TABLE KEY [keyname COMPONENTS]
	//                  {comp_name1|(name1)} = operand1
	//                  {comp_name2|(name2)} = operand2 ... } 
	// free_key  -> WITH KEY   { comp1 = operand1 comp2 = operand2 ... [BINARY SEARCH] }
	//                       | { keyname COMPONENTS comp1 = operand1 comp2 = operand2 ... } 
	// index     -> INDEX idx [USING KEY keyname]
	// 
	// result ->   { INTO wa [transport_options] } 
	//           | { ASSIGNING <fs> [CASTING] [ELSE UNASSIGN] } 
	//           | { REFERENCE INTO dref } 
	//           | { TRANSPORTING NO FIELDS }.
	// 
	// transport_options -> [COMPARING { {comp1 comp2 ...}|{ALL FIELDS}|{NO FIELDS} }]
	//                      [TRANSPORTING { {comp1 comp2 ...}|{ALL FIELDS} }]

	// -------------------------------------------------------------------------
	
	private static final String exceptionMessage = "Unexpected syntax of READ TABLE";

	private Term readTableKeywords; // 'READ TABLE'
	private Term table;	// identifier | identifier[] | <field_symbol> | method( ) | ctor_expr | table_expr

	private ReadTableSelectType selectType = ReadTableSelectType.NONE;
	private Token fromKeyword; // FROM
	private Term workArea;     // wa
	private Token indexKeyword; // INDEX
	private Term indexNum;      // idx
	
	private Term keyStartKeywords; // WITH [TABLE] KEY | USING KEY [the latter is optional after for ReadTableSelectType.INDEX and ReadTableSelectType.TABLE_KEY_FROM_WA]
	private Term keyName;          // key_name | (name)
	private Token componentsKeyword;   // COMPONENTS [is found between keyName and componentAssignments if(!) both are specified]
	private Term componentAssignments; // {comp_name1|(name1)} = operand1 {comp_name2|(name2)} = operand2 ...
	private Term binarySearchKeywords; // BINARY SEARCH - only with ReadTableSelectType.FREE_KEY

	private ReadTableResultType resultType = ReadTableResultType.NONE;
	private Term resultStartKeywords; // INTO / ASSIGNING / REFERENCE INTO / TRANSPORTING NO FIELDS
	private Term resultVariable;   // wa, <fs>, dref - except for ReadTableResultType.TRANSPORTING_NO_FIELDS
	private Term assignOptions;    // [CASTING] [ELSE UNASSIGN] - only with ReadTableResultType.ASSIGNING_FS
	private Term compareOptions;   // COMPARING    { {comp1 comp2 ...}|{ALL FIELDS}|{NO FIELDS} } - only with ReadTableResultType.INTO_WA 
	private Term transportOptions; // TRANSPORTING { {comp1 comp2 ...}|{ALL FIELDS} }             - only with ReadTableResultType.INTO_WA

	// -------------------------------------------------------------------------
	
	public Term getTable() { return table; }

	public ReadTableSelectType getSelectType() { return selectType; }
	public Term getWorkArea() { return workArea; }
	public Term getIndexNum() { return indexNum; }
	public Term getKeyName() { return keyName; }
	public Token getComponentsKeyword() { return componentsKeyword; }
	public Term getComponentAssignments() { return componentAssignments; }
	public boolean hasBinarySearchKeywords() { return binarySearchKeywords != null; }
	
	public ReadTableResultType getResultType() { return resultType; }
	public Term getResultVariable() { return resultVariable; }
	public Term getAssignOptions() { return assignOptions; }
	public Term getCompareOptions() { return compareOptions; }
	public Term getTransportOptions() { return transportOptions; }

	// -------------------------------------------------------------------------
	
	public static ReadTableCommand create(Command command) throws UnexpectedSyntaxException {
		return new ReadTableCommand(command);
	}

	/** returns the Terms that must be transferred to a new Command if the READ TABLE Command is changed into a table expression */
	private Term[] getTermsToBeMoved() {
		return new Term[] { table, workArea, indexNum, keyName, componentAssignments, resultVariable, assignOptions, compareOptions, transportOptions };
	}
	
	/**
	 * returns the Tokens of all comments and pragmas which are NOT transferred to a new Command 
	 * (because they are not found inside of a Term that would be transferred) 
	 * @return
	 */
	public ArrayList<Token> getCommentOrPragmaOutsideOfTerms() {
		// collect all comments and pragmas that are found inside of those Terms which would be transferred to a new Command 
		// if this READ TABLE Command was changed (i.e. do NOT consider keywords or Terms made of keywords)
		HashSet<Token> commentsAndPragmasInTerms = new HashSet<>();
		Term[] terms = getTermsToBeMoved();
		for (Term term : terms) {
			if (term == null)
				continue;
			Token token = term.firstToken;
			do {
				if (token.isPragmaOrComment())
					commentsAndPragmasInTerms.add(token);
				if (token == term.lastToken)
					break;
				token = token.getNext();
			} while (token != null);
		}

		// collect all comments and pragmas that are NOT part of the above list
		ArrayList<Token> commentOrPragmaOutsideOfTerms = new ArrayList<>();
		Token token = readTableKeywords.firstToken.getParentCommand().getFirstToken();
		while (token != null) {
			if (token.isPragmaOrComment() && !commentsAndPragmasInTerms.contains(token)) {
				commentOrPragmaOutsideOfTerms.add(token);
			}
			token = token.getNext();
		}
		return commentOrPragmaOutsideOfTerms;
	}
	
	private ReadTableCommand(Command command) throws UnexpectedSyntaxException {
		Token token = command.getFirstCodeToken();

		// 1. READ TABLE itab
		token = readReadTable(token);
		Token prevReadToken = token;
		
		// predetermine whether the result type is INTO_WA; in this case, "TRANSPORTING NO FIELDS" is part of the transport 
		// options, NOT the resultType. Outside of classes, the compiler tolerates it in any position (before or after INTO) 
		// if "COMPARING" is also found, or issues a warning if "COMPARING" is not found. 
		Token intoToken = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "INTO");
		boolean hasIntoToken = (intoToken != null && intoToken.isKeyword() && intoToken.getPrevCodeSibling() != null 
				&& !intoToken.getPrevCodeSibling().isKeyword("REFERENCE"));
		
		while (token != null) {
			// 2. selection: { index | table_key | free_key }
			if (selectType != ReadTableSelectType.NONE) { 
				// skip this section
				
			} else if (token.isKeyword("INDEX")) {
				selectType = ReadTableSelectType.INDEX;
				token = readIndex(token);

			} else if (token.isKeyword("FROM")) {
				selectType = ReadTableSelectType.TABLE_KEY_FROM_WA;
				token = readFromWorkArea(token);

			} else if (token.matchesOnSiblings(true, "WITH", "TABLE", "KEY")) {
				selectType = ReadTableSelectType.TABLE_KEY;
				token = readKey(token);
				
			} else if (token.matchesOnSiblings(true, "WITH", "KEY")) {
				selectType = ReadTableSelectType.FREE_KEY;
				token = readKey(token);
			} // do NOT attach with else if! 
			
			if (token.matchesOnSiblings(true, "BINARY", "SEARCH")) {
				// BINARY SEARCH - actually part of a FREE_KEY, but sometimes found in unexpected places, esp. after TRANSPORTING NO FIELDS
				binarySearchKeywords = Term.createForTokenRange(token, token.getNextCodeSibling());
				token = binarySearchKeywords.getNextCodeSibling();
			}
			
			// 3. result: {INTO wa ... | ASSIGNING <fs> ... | REFERENCE INTO dref | TRANSPORTING NO FIELDS}
			if (resultType != ReadTableResultType.NONE) {
				// skip this section

			} else if (token.isKeyword("INTO")) {
				resultType = ReadTableResultType.INTO_WA;
				token = readIntoWa(token);

			} else if (token.isKeyword("ASSIGNING")) {
				resultType = ReadTableResultType.ASSIGNING_FS;
				token = readAssigningFieldSymbol(token);

			} else if (token.matchesOnSiblings(true, "REFERENCE", "INTO")) {
				resultType = ReadTableResultType.REFERENCE_INTO_DREF;
				token = readReferenceIntoDref(token);

			} else if (token.matchesOnSiblings(true, "TRANSPORTING", "NO", "FIELDS") && !hasIntoToken) {
				resultType = ReadTableResultType.TRANSPORTING_NO_FIELDS;
				token = readTransportingNoFields(token);
			} // do NOT attach with else if!
			
			// transport options - actually part of a INTO_WA, but sometimes found in unexpected places, esp. before INTO 
			if (token.isKeyword("COMPARING")) {
				token = readCompareOptions(token);
				
			} else if (token.isKeyword("TRANSPORTING") && hasIntoToken) {
				// hasIntoToken ensures that this Command contains "INTO wa" (possibly at a later point); therefore, even 
				// "TRANSPORTING NO FIELDS" will be considered as a transport option (rather than as the result type)
				token = readTransportOptions(token);
			}

			// end the loop?
			if (prevReadToken == token) {
				// no reading progress was made in this loop cycle - unexpected syntax!
				throw new UnexpectedSyntaxException(token, exceptionMessage);

			} else if (token.isPeriod()) {
				if (getSelectType() == ReadTableSelectType.NONE) { // resultType == ReadTableResultType.UNKNOWN is tolerated by ABAP syntax
					throw new UnexpectedSyntaxException(token, exceptionMessage);
				} 
				break;
			}
			prevReadToken = token;
		}
	}

	private Token readReadTable(Token token) throws UnexpectedSyntaxException {
		// READ TABLE
		if (!token.matchesOnSiblings(true, "READ", "TABLE")) 
			throw new UnexpectedSyntaxException(token, exceptionMessage);
		readTableKeywords = Term.createForTokenRange(token, token.getNextCodeSibling());
		Token tableNameStart = readTableKeywords.getNextCodeSibling();
		token = tableNameStart;
		if (tableNameStart.isAnyKeyword(ABAP.constructorOperators) && tableNameStart.getNextCodeSibling().startsConstructorExpression())
			token = token.getNextCodeSibling();
		table = Term.createForTokenRange(tableNameStart, token.getNextSiblingWhileLevelOpener());
		return table.getNextCodeSibling();
	}

	private Token readIndex(Token token) throws UnexpectedSyntaxException {
		indexKeyword = token;
		token = indexKeyword.getNextCodeSibling();
		indexNum = Term.createArithmetic(token); // this may indeed be a calculation!
		return readUsingKey(indexNum.getNextCodeSibling());
	}

	private Token readFromWorkArea(Token token) throws UnexpectedSyntaxException {
		fromKeyword = token;
		token = fromKeyword.getNextCodeSibling();
		workArea = Term.createForTokenRange(token, token.getNextSiblingWhileLevelOpener());
		return readUsingKey(workArea.getNextCodeSibling());
	}

	private Token readUsingKey(Token token) throws UnexpectedSyntaxException {
		if (keyStartKeywords == null && token.matchesOnSiblings(true, "USING", "KEY", TokenSearch.ANY_IDENTIFIER)) {
			keyStartKeywords = Term.createForTokenRange(token, token.getNextCodeSibling());
			token = keyStartKeywords.getNextCodeSibling();
			keyName = Term.createForTokenRange(token, token);
			return keyName.getNextCodeSibling();
		} else {
			return token;
		}
	}
	
	private Token readKey(Token token) throws UnexpectedSyntaxException {
		Token keyKeyword = token.getLastTokenOnSiblings(true, "WITH", TokenSearch.makeOptional("TABLE"), "KEY");
		keyStartKeywords = Term.createForTokenRange(token, keyKeyword);
		token = keyStartKeywords.getNextCodeSibling();

		// the (potential) key name could be 'key_name' or '(name)'
		Token keyIdentifierStart = token;
		Token keyIdentifierEnd = keyIdentifierStart.getNextSiblingWhileLevelOpener();  
		boolean mayBeKeyName = keyIdentifierStart.isIdentifier() 
									|| keyIdentifierStart.textEquals("(") && keyIdentifierStart.hasChildren() && keyIdentifierStart.getFirstChild().isAttached() && keyIdentifierEnd.textEquals(")");  

		// either the key name is provided, or the component assignments immediately start with 'comp1 = ...';
		// note that there might be no components listed after 'WITH KEY keyname'
		Token nextCode = keyIdentifierEnd.getNextCodeSibling();
		if (mayBeKeyName && nextCode != null && !nextCode.textEquals("=")) { 
			keyName = Term.createForTokenRange(keyIdentifierStart, keyIdentifierEnd);
			// if no list of component assignments follows, we are done
			if (!nextCode.isKeyword("COMPONENTS")) 
				return nextCode;
			componentsKeyword = nextCode;
			token = componentsKeyword.getNextCodeSibling();
		}

		// read component assignment list
		Token componentStart = token;
		Token componentLast = null;
		if (token.textEquals("=")) {
			// 'WITH KEY = operator1' is possible for tables that have only one anonymous component (e.g. 'STANDARD TABLE OF char30')
			Term term = Term.createArithmetic(token.getNextCodeSibling());
			componentLast = term.lastToken;
			token = term.getNextCodeSibling();
		} else {
			while (token.matchesOnSiblings(true, TokenSearch.ANY_IDENTIFIER, "=") || token.matchesOnSiblings(true, "(", ")", "=")) {
				token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "=");
				Term term = Term.createArithmetic(token.getNextCodeSibling());
				componentLast = term.lastToken;
				token = term.getNextCodeSibling();
			}
		}
		if (componentLast != null) {
			componentAssignments = Term.createForTokenRange(componentStart, componentLast);
			token = componentAssignments.getNextCodeSibling();
		}
		return token;
	}
	
	private Token readIntoWa(Token token) throws UnexpectedSyntaxException {
		// INTO wa
		resultStartKeywords = Term.createForTokenRange(token, token);
		token = resultStartKeywords.getNextCodeSibling();
		resultVariable = Term.createForTokenRange(token, token.getNextSiblingWhileLevelOpener());
		return resultVariable.getNextCodeSibling();

		// the optional transport_options are handled in readCompareOptions() and readTransportOptions(), 
		// because they might be detached from the INTO section
	}
	
	private Token readCompareOptions(Token token) throws UnexpectedSyntaxException {
		compareOptions = readCompareOrTransportOptions(token);
		return compareOptions.getNextCodeSibling();
	}

	private Token readTransportOptions(Token token) throws UnexpectedSyntaxException {
		transportOptions = readCompareOrTransportOptions(token);
		return transportOptions.getNextCodeSibling();
	}

	private Term readCompareOrTransportOptions(Token token) throws UnexpectedSyntaxException {
		// [transport_options]:
		// - [COMPARING    { {comp1 comp2 ...}|{ALL FIELDS}|{NO FIELDS} }] 
		// - [TRANSPORTING { {comp1 comp2 ...}|{ALL FIELDS} }] - see remark below for NO FIELDS!

		Token resultStart = token;
		token = token.getNextCodeSibling();

		Token resultLast;
		if (token.matchesOnSiblings(true, "ALL", "FIELDS")) {
			resultLast = token.getNextCodeSibling();

		} else if (token.matchesOnSiblings(true, "NO", "FIELDS")) {
			// outside of classes, if the Command contains "INTO wa", "TRANSPORTING NO FIELDS" is also considered as a 
			// transport option (not as the result type). This is already checked by the caller, so at this point, we 
			// can assume that the result type is .INTO_WA (even if it only comes after TRANSPORTING NO FIELDS)
			resultLast = token.getNextCodeSibling();
		
		} else if (token.isIdentifier()) {
			do  {
				resultLast = token;
				token = token.getNextCodeSibling();
			} while (token.isIdentifier());
		} else {
			throw new UnexpectedSyntaxException(token, exceptionMessage);
		}
		return Term.createForTokenRange(resultStart, resultLast);
	}

	private Token readAssigningFieldSymbol(Token token) throws UnexpectedSyntaxException {
		// ASSIGNING <fs>
		resultStartKeywords = Term.createForTokenRange(token, token);
		token = resultStartKeywords.getNextCodeSibling();
		resultVariable = Term.createForTokenRange(token, token.getNextSiblingWhileLevelOpener());
		token = resultVariable.getNextCodeSibling();

		// [CASTING] [ELSE UNASSIGN]
		Token resultStart = token;
		if (token.isKeyword("CASTING")) {
			token = token.getNextCodeSibling();
		}
		if (token.matchesOnSiblings(true, "ELSE", "UNASSIGN")) {
			token = token.getNextCodeSibling().getNextCodeSibling();
		}
		if (token != resultStart) {
			Token resultLast = token.getPrevCodeSibling();
			assignOptions = Term.createForTokenRange(resultStart, resultLast);
			token = assignOptions.getNextCodeSibling();
		}
		return token;
	}
	
	private Token readReferenceIntoDref(Token token) throws UnexpectedSyntaxException {
		// REFERENCE INTO dref
		resultStartKeywords = Term.createForTokenRange(token, token.getNextCodeSibling());
		token = resultStartKeywords.getNextCodeSibling();
		resultVariable = Term.createForTokenRange(token, token.getNextSiblingWhileLevelOpener());
		return resultVariable.getNextCodeSibling();
	}
	
	private Token readTransportingNoFields(Token token) throws UnexpectedSyntaxException {
		// TRANSPORTING NO FIELDS
		resultStartKeywords = Term.createForTokenRange(token, token.getNextCodeSibling().getNextCodeSibling());
		// resultVariable remains null
		return resultStartKeywords.getNextCodeSibling();
	}

	/**
	 * ensures that after READ TABLE was changed to a table expression, all relevant Terms were moved to the new Command, 
	 * and none is left on a now-deleted Command
	 * @return true if the change was consistent; false if a relevant Term is still part of the now-deleted READ TABLE Command 
	 */
	public boolean wereAllRelevantTermsMoved() {
		Term[] terms = getTermsToBeMoved();
		for (Term term : terms) {
			if (term != null && term.firstToken.getParentCommand().wasRemovedFromCode()) {
				return false;
			}
		}
		return true;
	}
}
