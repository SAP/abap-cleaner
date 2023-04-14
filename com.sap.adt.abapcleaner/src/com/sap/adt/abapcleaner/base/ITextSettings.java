package com.sap.adt.abapcleaner.base;

public interface ITextSettings {
	static final String LF = "\n";

	static final char OBJECT_OPEN = '{';
	static final char OBJECT_CLOSE = '}';
	static final char ARRAY_OPEN = '[';
	static final char ARRAY_CLOSE = ']';
	static final char KEY_VALUE_SEP = ':';
	static final char ELEMENT_SEP = ',';
	static final char STRING_DELIMITER = '\"';

	static final String VALUE_TRUE = "true";
	static final String VALUE_FALSE = "false";

	static final String WHITESPACE_CHARS = " \t\r\n";
	static final String NON_STRING_DELIMITERS = ",}]" + WHITESPACE_CHARS;
}
