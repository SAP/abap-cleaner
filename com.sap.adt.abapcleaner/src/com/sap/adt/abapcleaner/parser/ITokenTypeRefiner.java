package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.programbase.ParseException;

/**
 * <p>Implementations of this interface refine the initial categorization of {@link Token}s into {@link TokenType}s, 
 * especially by differentiating between {@link TokenType#KEYWORD}s and {@link TokenType#IDENTIFIER}s.
 * This is not a trivial task, since ABAP keywords are not reserved, 
 * but can be used as identifiers as well (e.g. variable or component names 'min', 'max', 'key', 'result').</p>
 * 
 * <p>When parsing ABAP code, ABAP cleaner first performs an initial categorization in {@link Token#Token(int, int, String, int)}
 * and then calls the ITokenTypeRefiner implementation in {@link Command#finishBuild(int, int)}, 
 * when all information is available to decide on the type of each {@link Token} in the {@link Command}.</p>
 */
public interface ITokenTypeRefiner {
	void refine(Command command) throws ParseException;
}
