package com.sap.adt.abapcleaner.parser;

/**
 * Contains wildcards that can be used as String parameters when searching for code patterns with 
 * {@link Token#matchesOnSiblings(boolean, String...)},  
 * {@link Token#matchesDeep(boolean, String...)},  
 * {@link Token#getLastTokenOnSiblings(boolean, String...)},
 * {@link Token#getLastTokenDeep(boolean, String...)}, and
 * {@link Token#getLastTokenOfSequence(boolean, boolean, Token, String...)}. 
 */
public class TokenSearch {
	/** allows skipping any number of non-matching Tokens */
	public static final String ASTERISK = "[*]";
	public static final String MAX_ONE_NON_MATCHING_TOKEN = "[1]";
	static final String OPTIONAL_TOKEN_SUFFIX = "[?]";
	public static final String ANY_NUMBER_OF_COMMENTS = "[comments]";
	public static final String ANY_LITERAL = "[literal]";
	public static final String ANY_IDENTIFIER = "[identifier]";
	public static final String ANY_NON_COMMENT = "[non_comment]";
	public static final String ANY_IDENTIFIER_OR_LITERAL = "[identifier_or_literal]";
	public static final String ANY_TERM = "[term]";
	public static final String ANY_ARITHMETIC_EXPRESSION = "[arithmetic_expression]";
	public static final String ANY_COMPARISON_OPERATOR = "[comparison_operator]";
	public static final char TEXT_MATCH_VARIANT_SEPARATOR = '|';

	public static String makeOptional(String text) {
		return text + TokenSearch.OPTIONAL_TOKEN_SUFFIX;
	}
}
