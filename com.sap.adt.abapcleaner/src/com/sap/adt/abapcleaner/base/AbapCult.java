package com.sap.adt.abapcleaner.base;

import java.util.Locale;

/**
 * Represents the Culture used by ABAP code and provides helper methods for String handling. 
 */
public final class AbapCult {
	private final static Locale locale = Locale.ENGLISH;

	public static boolean stringEquals(String value1, String value2, boolean ignoreCase) {
		return ignoreCase ? value1.equalsIgnoreCase(value2) : value1.equals(value2);
	}

	public static boolean stringEqualsAny(boolean ignoreCase, String value1, String... values2) {
		for (String value2 : values2) {
			if (ignoreCase ? value1.equalsIgnoreCase(value2) : value1.equals(value2))
				return true;
		}
		return false;
	}

	public static boolean stringStartsWithAny(String text, String... substrings) {
		for (String substring : substrings) {
			if (stringStartsWith(text, substring, true)) {
				return true;
			}
		}
		return false;
	}

	public static boolean stringStartsWith(String text, String substring) {
		// as casing is not explicitly specified by the caller, ignore the case
		return stringStartsWith(text, substring, true);
	}

	public static boolean stringStartsWith(String text, String substring, boolean ignoreCase) {
		if (ignoreCase)
			return (substring.length() <= text.length() && text.substring(0, substring.length()).equalsIgnoreCase(substring));
		else
			return text.startsWith(substring);
	}

	public static boolean stringEndsWithAny(String text, String... substrings) {
		for (String substring : substrings) {
			if (stringEndsWith(text, substring, true)) {
				return true;
			}
		}
		return false;
	}

	public static boolean stringEndsWith(String text, String substring) {
		// as casing is not explicitly specified by the caller, ignore the case
		return stringEndsWith(text, substring, true);
	}

	public static boolean stringEndsWith(String text, String substring, boolean ignoreCase) {
		if (ignoreCase)
			return (substring.length() <= text.length() && text.substring(text.length() - substring.length()).equalsIgnoreCase(substring));
		else
			return text.endsWith(substring);
	}

	public static int indexOf(String text, String substring, int startIndex, boolean ignoreCase) {
		if (ignoreCase)
			return text.toUpperCase().indexOf(substring.toUpperCase(), startIndex);
		else
			return text.indexOf(substring, startIndex);
	}

	public static int lastIndexOf(String text, String substring, int startIndex, boolean ignoreCase) {
		if (ignoreCase)
			return text.toUpperCase().lastIndexOf(substring.toUpperCase(), startIndex);
		else
			return text.lastIndexOf(substring, startIndex);
	}

	public static boolean stringContainsAt(String text, int start, String textBit, boolean ignoreCase) {
		if (text == null || textBit == null)
			return false;
		return (start + textBit.length() <= text.length()) && stringEquals(text.substring(start, start + textBit.length()), textBit, ignoreCase);
	}

	public static String toLower(String value) {
		return (value == null) ? null : value.toLowerCase(locale);
	}

	public static String toUpper(String value) {
		return (value == null) ? null : value.toUpperCase(locale);
	}

	public static String ToString(int value) {
		return String.valueOf(value);
	}

	public static boolean isMixedCase(String text) {
		return !toUpper(text).equals(text) && !toLower(text).equals(text);
	}
}