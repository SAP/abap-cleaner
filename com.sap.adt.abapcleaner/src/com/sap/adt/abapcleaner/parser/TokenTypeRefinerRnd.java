package com.sap.adt.abapcleaner.parser;

import java.util.List;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.PadResourceResolver;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.ParseException;

import com.sap.adt.tools.abapsource.parser.ABAPRndParser;
import com.sap.rnd.rndrt.Category;
import com.sap.rnd.rndrt.ErrorState;

/**
 * <p>Implementation of the {@link ITokenTypeRefiner} interface which uses the ABAP RND Parser 
 * to refine the {@link TokenType}s that were preliminary determined in {@link Token#Token(int, int, String, int)}.</p> 
 *
 * <p>The only 'support' required from the RND Parser is to securely distinguish 
 * ABAP keywords ({@link TokenType#KEYWORD}) from identifiers ({@link TokenType#IDENTIFIER}). 
 * This is required because ABAP keywords are not reserved, but can be used 
 * as identifiers as well (e.g. variable or component names 'min', 'max', 'key', 'result').</p>  
 * 
 * <p>Note that for the special purpose of ABAP cleaner, the {@link TokenType} enumeration differs 
 * from the RND Parser's {@link Category}, e.g. different types of operators are distinguished 
 * ({@link TokenType#ASSIGNMENT_OP}, {@link TokenType#COMPARISON_OP}, {@link TokenType#OTHER_OP}).
 * Also, an ABAP cleaner Token may contain multiple RND Parser tokens, e.g.</p>
 * 
 * <ul>
 * <li>"DATA(", "TEXT-001", "READ-ONLY" (all of {@link TokenType#KEYWORD}),</li>
 * <li>"lo_object->method(", "lt_table[", "lv_text(20)" (all of {@link TokenType#IDENTIFIER})</li>
 * <li>"1(70)" ({@link TokenType#LITERAL})</li>
 * </ul>
 * 
 * <p>... because the text of such 'tokens' will never need to be changed in the context of ABAP cleaner, 
 * so it is easier to keep them together in one {@link Token} instance.</p>
 */
@SuppressWarnings("restriction")
public class TokenTypeRefinerRnd implements ITokenTypeRefiner {
	/*
	 Examples of RND token categories found in 20 MB ABAP sample code (space-separated): 
		CAT_OPERATOR:        - # ( ) * , . / : @ [ ]         + < = >
		CAT_TOKEN_OPERATOR:  -   ( ) *     /   @ [ ] { | } ~ + <   > => ->
		CAT_MAYBE_KEYWORD:   # = AND APPENDING AS c CORRESPONDING DIV EMPTY EQ EQUIV EXTENDED FOR FROM IN INITIAL INNER INTO IS JOIN KEY LIKE MOD name NOT NULL OR RESULT space SUPPLIED TABLE TO USING VALUE WHERE WITH
		CAT_STRICT_KEYWORD:  APPENDING AS CHANGING FROM INTO LIKE PACKAGE SCREEN TABLES TO TYPE UP USING VALUE
		CAT_ANNOTATION:      (none found)
		CAT_INCOMPLETE:      (none found)
		CAT_PRIMITIVE:       (none found)
		CAT_UNDEF:           (none found)
		CAT_WS:              <NL> #EOF#
	 */

	private PadResourceResolver padFileResolver;
	private ABAPRndParser rndParser;
	private TokenTypeRefiner internalRefiner;

	public static TokenTypeRefinerRnd create() {
		return new TokenTypeRefinerRnd();
	}

	private TokenTypeRefinerRnd() {
		padFileResolver = new PadResourceResolver();
		rndParser = new ABAPRndParser();
		internalRefiner = TokenTypeRefiner.create();
	}

	@Override
	public void refine(Command command) throws ParseException {
		if (command.isCommentLine() || command.isEmpty() || !command.isAbap())
			return;

		// start with the results of the internal refiner that does not rely on the RND Parser; its results may then be 
		// overwritten below by the results of the RND Parser, but are kept for code considered erroneous by the RND Parser
		internalRefiner.refine(command);

		// get a string for this command, omitting comments and reducing whitespace to single line breaks / spaces
		StringBuilder code = new StringBuilder();
		Token token = command.getFirstToken();
		while (token != null) {
			if (!token.isComment()) {
				if (token.lineBreaks > 0)
					code.append('\n');
				// even after a line break, a space may be added, because the next line may start with the multiplication 
				// operator "*" (which would be misinterpreted as a asterisk comment sign without a leading space)
				if (token.spacesLeft > 0) 
					code.append(' ');
				code.append(token.text);
			}
			token = token.getNext();
		}

		// provide a current .pad file from the resources to the RND Parser, since otherwise, it will fall back to 
		// an older .pad file version, in which newer ABAP syntax is not yet considered
		List<com.sap.rnd.rndrt.Token> rndTokens = rndParser.parseSource(padFileResolver, code.toString());
		
		token = command.getFirstToken();
		int textOffset = 0;
		for (com.sap.rnd.rndrt.Token rndToken : rndTokens) {
			// skip line breaks in RND Parser Tokens
			if (rndToken.m_category == Category.CAT_WS && rndToken.m_lexem == "<NL>")
				continue;

			// skip comments in ABAP cleaner Tokens
			while (token != null && token.isComment())
				token = token.getNext();

			// if this is the last rndToken, check whether any ABAP cleaner Token is left
			if (rndToken.m_category == Category.CAT_WS && rndToken.m_lexem == "#EOF#") {
				if (token != null)
					throw new ParseException(command.getParentCode(), command.getSourceLineNumStart(), "no further token expected");
				continue; // #EOF# should be the last rndToken, so "continue" should have the effect of "break"
			} 
			
			// ensure there is an ABAP cleaner Token left to match the rndToken
			if (token == null) 
				throw new ParseException(command.getParentCode(), command.getSourceLineNumStart(), "no further token expected");

			refine(command, token, rndToken);

			// skip the "!" escape character for identifiers such as !min, !its_table and !ls_struc-component, because the 
			// RND token simply omits the "!". The escape char is typically found in method declarations, including with 
			// the keywords !VALUE(...) and !REFERENCE(...), but could be used as the first character of any identifier 
			// (including method names) 
			if (textOffset == 0 && token.getTextLength() > 1 && token.textStartsWith("!") && !StringUtil.startsWith(rndToken.m_lexem, "!", false)
					 && (rndToken.m_category == Category.CAT_IDENTIFIER || rndToken.m_category == Category.CAT_KEYWORD)) {
				++textOffset;
			}
			
			// move to the next ABAP cleaner Token, or advance the textOffset of the current Token if it contains multiple RND tokens  
			int lexemLength = rndToken.m_lexem.length();
			if (textOffset == 0 && token.getTextLength() == lexemLength && token.text.equals(rndToken.m_lexem)) {
				token = token.getNext();
				textOffset = 0; // pro forma
			} else if (textOffset + lexemLength <= token.getTextLength() && token.text.substring(textOffset, textOffset + lexemLength).equals(rndToken.m_lexem)) {
				// in some cases, ABAP cleaner Tokens contain several RND Tokens, e.g. "DATA(", "#(", "+=", "lv_var(20)", "obj->method(", ")->method(", "interface~method(" etc.
				textOffset += lexemLength;
				if (textOffset == token.getTextLength()) {
					token = token.getNext();
					textOffset = 0;
				}
			} else {
				throw new ParseException(command.getParentCode(), command.getSourceLineNumStart(), "code mismatch with RND Parser output");
			}
		}
	}

	private void refine(Command command, Token token, com.sap.rnd.rndrt.Token rndToken) throws ParseException {
		// only use the RND Parser result if is classified as being correct -
		// e.g., RND Parser considers multiple :: to be erroneous 
		if (rndToken.m_err_state == ErrorState.Erroneous)
			return;

		// apart from the main task of distinguishing ABAP keywords from identifiers, exceptions are thrown in case
		// the RND Parser token category does not seem to match the expectations based on the token.type
		boolean throwException = false;
		switch (rndToken.m_category) {
			case CAT_IDENTIFIER:
				if (token.type == TokenType.LITERAL && token.textStartsWith(ABAP.QUOT_MARK_STRING) && token.textEndsWith(ABAP.CLOSING_PARENTHESIS_STRING)) {
					// keep TokenType.LITERAL: in cases like 'text'(a01) (text field literals with attached alphanumeric text symbols), 
					// RND Parser considers a01 to be an identifier, but ABAP cleaner regards the whole 'text'(a01) as a literal
				} else if (token.isAnyKeyword("FIELD-SYMBOL(", "READ-ONLY")) { 
					// keep TokenType.KEYWORD - RND parser classifies 'FIELD' as an identifier and as 'suspicious' 
				} else if (token.isTextSymbol()) { 
					// keep TokenType.KEYWORD, which was assigned earlier when rndToken "TEXT" was processed
				} else if (token.type != TokenType.IDENTIFIER) {
					token.type = TokenType.IDENTIFIER;
				}
				break;

			case CAT_KEYWORD:
			case CAT_STRICT_KEYWORD:
				// keep TokenType.COMPARISON_OP for "EQ", "NE" etc. (as required by ComparisonOperatorRule)
				// and keep TokenType.ASSIGNMENT_OP for "?=" (as required by AlignParametersRule)
				if (token.type != TokenType.KEYWORD && token.type != TokenType.COMPARISON_OP && token.type != TokenType.ASSIGNMENT_OP)
					token.type = TokenType.KEYWORD;
				break;

			case CAT_MAYBE_KEYWORD:
				// in case of a "maybe keyword", keep the token type determined by ABAP cleaner
				// if (token.type != TokenType.KEYWORD && token.type != TokenType.COMPARISON_OP && token.type != TokenType.ASSIGNMENT_OP)
				//    token.type = TokenType.KEYWORD;
				break;

			case CAT_COMMENT:
				// no comment expected, as we created the code string which was passed to the RND Parser without them; 
				// however, RND Parser categorizes pragmas like ##NO_TEXT as comments, too, so only non-pragmas are unexpected here
				throwException = !StringUtil.startsWith(rndToken.m_lexem, ABAP.PRAGMA_SIGN, false);
				break;

			case CAT_PRAGMA:
				throwException = (token.type != TokenType.PRAGMA);
				break;

			case CAT_LITERAL:
			case CAT_NUMERICAL_LITERAL:
				switch (token.type) {
					case LITERAL:
					case IDENTIFIER:
						// identifiers may contain literals, e.g. "lv_var(20)" as in "DATA lv_var(20) TYPE c."
						break;
					case KEYWORD:
						// keywords may contain literals in the special case of Text Elements such as TEXT-001
						throwException = !token.textStartsWith("TEXT-");
						break;
					case OTHER_OP:
						// other operators may contain literals in the special case of "ULINE AT /10(20).", "ULINE AT 10(20)." etc. 
						throwException = !token.textStartsWith("/") && !token.textEndsWith("(");
						break;
					default:
						throwException = true;
						break;
				}
				break;

			case CAT_OPERATOR:
				switch (token.type) {
					case ASSIGNMENT_OP:
					case COMPARISON_OP:
					case COLON:
					case PERIOD:
					case COMMA:
					case OTHER_OP:
						break;
					default:
						// in SELECT statements, the prefix \ (ABAP.ASSOCIATION_PREFIX_STRING) is used for association names, 
						// and the tilde ~ after the data source; the ABAP cleaner parser regards both as part of the identifier; 
						// examples: SELECT FROM ... FIELDS \_Carrier-carrname, c~\_Carrier-currcode, \_productversion[ (*) ]-active  
						throwException = !rndToken.m_lexem.equals(ABAP.AT_SIGN_STRING) 
										  && !rndToken.m_lexem.equals("[") && !rndToken.m_lexem.equals("]") 
										  && !rndToken.m_lexem.equals(ABAP.ASSOCIATION_PREFIX_STRING)
										  && !rndToken.m_lexem.equals(ABAP.TILDE_STRING);
				}
				break;

			case CAT_TOKEN_OPERATOR:
				switch (token.type) {
					case ASSIGNMENT_OP: // e.g. *=
					case COMPARISON_OP: // e.g. >=, see CodeTest->testSelectWithComparisonOps()
					case OTHER_OP:
					case LITERAL:  // e.g. -500, where "-" is considered a token operator by the RND Parser
						break;
					case IDENTIFIER:
					case KEYWORD: // e.g. READ-ONLY
						// in ABAP cleaner, token operators are usually part of identifiers to which they are attached
						// or part of keywords ("READ-ONLY" etc.)
						throwException = (rndToken.m_lexem.length() == token.getTextLength());
						break;
					default:
						throwException = true;
						break;
				}
				break;

			case CAT_ANNOTATION: // TODO: annotations may not yet be considered in the ABAP cleaner Parser class
			case CAT_INCOMPLETE:
			case CAT_PRIMITIVE:
			case CAT_UNDEF:
			case CAT_WS: 
				break;
				
			default:
				break;
		}

		if (throwException)
			throw new ParseException(command.getParentCode(), command.getSourceLineNumStart(), "RND Parser categorizes '" + rndToken.m_lexem + "' as a " + rndToken.m_category.name());
	}

}
