package com.sap.adt.abapcleaner.parser;

/**
 * <p>Enumeration of {@link Token} types which are roughly predetermined in {@link Token#Token(int, int, String, int)}
 * and then refined in {@link Command#finishBuild(int, int)} with the help of a {@link ITokenTypeRefiner}.</p>
 * 
 * <p>This enumeration is tailored to meet the needs of ABAP cleaner 
 * and therefore not identical to the token types used by the RND Parser (see {@link TokenTypeRefinerRnd}).</p> 
 */
public enum TokenType  {
   COMMENT,
   PRAGMA,
   LITERAL,
   ASSIGNMENT_OP, // "=" is first categorized as an assignment operator (even if it is a comparison operator); Command.finishBuild() -> .distinguishOperators() improves that to some extent
   COMPARISON_OP,
   KEYWORD,
   IDENTIFIER, // including identifier chains like "lo_any_instance->any_method("
   PERIOD,
   COMMA,
   COLON,
   OTHER_OP, // "+", ")" etc.
   NON_ABAP; 

	public static final int SIZE = java.lang.Integer.SIZE;
}