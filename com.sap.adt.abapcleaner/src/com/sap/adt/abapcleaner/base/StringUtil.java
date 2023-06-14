package com.sap.adt.abapcleaner.base;

import java.util.ArrayList;
import java.util.List;

/**
 * Provides static helper methods for String handling and manipulation. 
 */
public final class StringUtil {
	public static int instrCount(String text, char textBit) {
		if (text == null)
			return 0;
		int count = 0;
		int pos = 0;
		do {
			pos = text.indexOf(textBit, pos);
			if (pos < 0)
				break;
			++count;
			++pos;
		} while (pos < text.length());
		return count;
	}

	public static int spaceCountAtStartOf(String text) {
		if (StringUtil.isNullOrEmpty(text))
			return 0;
		int result = 0;
		char[] textChars = text.toCharArray();
		for (char c : textChars) {
			if (c == ' ' || c == '\t')
				++result;
			else
				break;
		}
		return result;
	}

	/**
	 * returns null only if text == null, otherwise a String array that may have .length == 0 if empty entries shall be removed
	 */
	public static String[] split(String text, String separator, boolean removeEmptyEntries) {
		if (text == null) 
			return null;
		else if (text.length() == 0) 
			return removeEmptyEntries ? new String[0] : new String[] { "" };

		ArrayList<String> results = new ArrayList<String>();
		int start = 0;
		do {
			int sepPos = text.indexOf(separator, start);
			if (sepPos < 0) {
				if (start < text.length())
					results.add(text.substring(start, text.length()));
				else if (!removeEmptyEntries)
					results.add("");
				break;
			}
			if (sepPos > start) 
				results.add(text.substring(start, sepPos));
			else if (!removeEmptyEntries)
				results.add("");
			start = sepPos + separator.length();
		} while(true);

		return toStringArray(results);
	}

	/**
	 * returns null only if text == null, otherwise a String array that may have .length == 0 if empty entries shall be removed
	 */
	public static String[] split(String text, char separator, boolean removeEmptyEntries) {
		if (text == null) 
			return null;
		else if (text.length() == 0) 
			return removeEmptyEntries ? new String[0] : new String[] { "" };

		ArrayList<String> results = new ArrayList<String>();
		int start = 0;
		do {
			int sepPos = text.indexOf(separator, start);
			if (sepPos < 0) {
				if (start < text.length())
					results.add(text.substring(start, text.length()));
				else if (!removeEmptyEntries)
					results.add("");
				break;
			}
			if (sepPos > start) 
				results.add(text.substring(start, sepPos));
			else if (!removeEmptyEntries)
				results.add("");
			start = sepPos + 1;
		} while(true);

		return toStringArray(results);
	}

	/**
	 * returns null only if text == null, otherwise a String array that may have .length == 0 if empty entries shall be removed
	 */
	public static String[] split(String text, char[] separators, boolean removeEmptyEntries) {
		if (text == null) 
			return null;
		else if (text.length() == 0) 
			return removeEmptyEntries ? new String[0] : new String[] { "" };

		ArrayList<String> results = new ArrayList<String>();
		int start = 0;
		for (int i = 0; i < text.length(); ++i) {
			char c = text.charAt(i);
			for (char sep : separators) {
				if (sep == c) {
					if (i > start) 
						results.add(text.substring(start, i));
					else if (!removeEmptyEntries)
						results.add("");
					start = i + 1;
					break;
				}
			}
		}
		if (start < text.length())
			results.add(text.substring(start, text.length()));
		else if (!removeEmptyEntries)
			results.add("");

		return toStringArray(results);
	}

	public static boolean startsWith(String text, String prefix, boolean ignoreCase) {
		if (ignoreCase)
			return (prefix.length() <= text.length() && text.substring(0, prefix.length()).equalsIgnoreCase(prefix));
		else
			return text.startsWith(prefix);
	}

	public static boolean endsWith(String text, String suffix, boolean ignoreCase) {
		if (ignoreCase)
			return (suffix.length() <= text.length() && text.substring(text.length() - suffix.length()).equalsIgnoreCase(suffix));
		else
			return text.endsWith(suffix);
	}

	public static String removePrefix(String text, String prefix, boolean ignoreCase) {
		if (startsWith(text, prefix, ignoreCase))
			return text.substring(prefix.length());
		else
			return text;
	}
	
	public static String removeSuffix(String text, String suffix, boolean ignoreCase) {
		if (endsWith(text, suffix, ignoreCase))
			return text.substring(0, text.length() - suffix.length());
		else
			return text;
	}
	
	public static String removePrefixRecursively(String text, String prefix, boolean ignoreCase) {
		String result = text;
		while (startsWith(result, prefix, ignoreCase))
			result = result.substring(prefix.length());
		return result;
	}
	
	public static String removeSuffixRecursively(String text, String suffix, boolean ignoreCase) {
		String result = text;
		while (endsWith(result, suffix, ignoreCase))
			result = result.substring(0, result.length() - suffix.length());
		return result;
	}
	
	public static int suffixCount(String text, String suffix, boolean ignoreCase) {
		if (text == null || text.length() == 0 || suffix == null || suffix.length() == 0)
			return 0;
		
		int result = 0;
		int pos = text.length() - suffix.length();
		while (pos >= 0) {
			String substring = text.substring(pos, pos + suffix.length());
			boolean match = ignoreCase ? substring.equalsIgnoreCase(suffix) : substring.equals(suffix);
			if (!match)
				break;
			++result;
			pos -= suffix.length();
		}
		return result;
	}

	public static String repeatChar(char charToRepeat, int count) {
		StringBuilder sb = new StringBuilder(count);
		for (int i = 1; i <= count; i++)
			sb.append(charToRepeat);
		return sb.toString();
	}

	public static String trimStart(String string) {
		if (string == null)
			return string;

		int startIndex = 0;
		while (startIndex < string.length() && Character.isWhitespace(string.charAt(startIndex)))
			++startIndex;
		return string.substring(startIndex);
	}

	public static String trimStart(String string, Character trimChar) {
		if (string == null)
			return string;

		int startIndex = 0;
		while (startIndex < string.length() && string.charAt(startIndex) == trimChar)
			++startIndex;
		return string.substring(startIndex);
	}

	public static String trimEnd(String string) {
		if (string == null)
			return string;

		int length = string.length();
		while (length - 1 >= 0 && Character.isWhitespace(string.charAt(length - 1)))
			--length;
		return string.substring(0, length);
	}

	public static String trimEnd(String string, Character trimChar) {
		if (string == null)
			return string;

		int length = string.length();
		while (length - 1 >= 0 && string.charAt(length - 1) == trimChar)
			--length;
		return string.substring(0, length);
	}

	public static String trimEnd(String string, String trimText) {
		if (string == null)
			return string;

		int length = string.length();
		int trimTextLength = trimText.length();
		while (length - trimTextLength >= 0 && string.substring(length - trimTextLength, length).equals(trimText))
			length -= trimTextLength;
		return string.substring(0, length);
	}

	public static boolean isNullOrEmpty(String string) {
		return string == null || string.length() == 0;
	}

	public static String join(String separator, String[] stringArray) {
		if (stringArray == null)
			return null;

		StringBuilder sb = new StringBuilder();
		for (int index = 0; index < stringArray.length; index++) {
			if (separator != null && index > 0)
				sb.append(separator);

			if (stringArray[index] != null)
				sb.append(stringArray[index]);
		}

		return sb.toString();
	}

	public static int indexOfAny(String string, char[] anyOf, int startIndex) {
		if (string == null)
			return -1;
		
		int index = startIndex;
		while (index < string.length()) {
			char c = string.charAt(index);
			for (char match : anyOf) {
				if (c == match)
					return index;
			}
			++index;
		}
		return -1;
	}
	
	public static int indexOfAny(String string, char[] anyOf, int startIndex, String[] skipTexts) {
		if (string == null)
			return -1;
		
		int index = startIndex;
		while (index < string.length()) {
			// find out whether this position should be skipped (typically, because an escape sequence like \\, \r, \n, \t etc. was found)
			boolean skip = false;
			for (String skipText : skipTexts) {
				int skipLen = skipText.length();
				if (index + skipLen <= string.length() && skipText.equals(string.substring(index, index + skipLen))) {
					skip = true;
					index += skipLen;
					break;
				}
			}
			if (!skip) {
				char c = string.charAt(index);
				for (char match : anyOf) {
					if (c == match)
						return index;
				}
				++index;
			}
		}
		return -1;
	}
	
	public static int findWholeWord(String text, String word, boolean ignoreCase, String nonDelimiterChars) {
		if (ignoreCase) {
			text = text.toUpperCase();
			word = word.toUpperCase();
		}
		int pos = -1;
		while (pos + 1 < text.length()) {
			pos = text.indexOf(word, pos + 1);
			if (pos < 0)
				break;
			if (pos > 0) {
				char prevChar = text.charAt(pos - 1);
				if (Character.isLetterOrDigit(prevChar))
					continue;
				if (nonDelimiterChars != null && nonDelimiterChars.indexOf(prevChar) >= 0)
					continue;
			}
			int end = pos + word.length();
			if (end < text.length()) {
				char nextChar = text.charAt(end);
				if (Character.isLetterOrDigit(nextChar))
					continue;
				if (nonDelimiterChars != null && nonDelimiterChars.indexOf(nextChar) >= 0)
					continue;
			}
			return pos;
		}
		return -1;
	}

	public static String[] toStringArray(ArrayList<String> arrayList) {
		if (arrayList == null || arrayList.size() == 0)
			return null;
		String[] stringArray = new String[arrayList.size()];
		arrayList.toArray(stringArray);
		return stringArray;
	}
	
	public static String[] toStringArray(List<String> list) {
		if (list == null || list.size() == 0)
			return null;
		String[] stringArray = new String[list.size()];
		list.toArray(stringArray);
		return stringArray;
	}

	public static boolean equalsCheckingForNull(String string1, String string2) {
		if (string1 == null && string2 == null)
			return true;
		else if (string1 == null || string2 == null)
			return false;
		else
			return string1.equals(string2);
	}

	public static boolean equalsIgnoreCaseCheckingForNull(String string1, String string2) {
		if (string1 == null && string2 == null)
			return true;
		else if (string1 == null || string2 == null)
			return false;
		else
			return string1.equalsIgnoreCase(string2);
	}
	
	public static String inferLineSeparator(String text) {
		if (text == null || text.length() == 0)
			return System.lineSeparator();

		int lfPos = text.indexOf('\n');
		if (lfPos > 0 && text.charAt(lfPos - 1) == '\r')
			return "\r\n";
		else if (lfPos >= 0)
			return "\n";
		else
			return System.lineSeparator();
	}
	
	public static String getEscapeText(String text) {
		if (text == null)
			return null;
		
		StringBuilder sb = new StringBuilder();
		char[] chars = text.toCharArray();
		for (char c : chars) {
			if (c == '\"' || c == '\\')
				sb.append("\\");
			sb.append(c);
		}
		return sb.toString();
	}
	
	public static String getUnescapedText(String text) {
		if (text == null)
			return null;
		
		StringBuilder sb = new StringBuilder();
		char[] chars = text.toCharArray();
		for (int i = 0; i < chars.length; ++i) {
			if (chars[i] == '\\' && i + 1 < chars.length) {
				// skip char and instead add the next char, i.e. the escaped \ or " char
				++i;
			}
			sb.append(chars[i]);
		}
		return sb.toString();
	}

	public static String readTillEndOfAllowedChars(String line, int start, String allowedChars) {
		int end = start;
		if (line == null)
			return null;
		if (start >= line.length())
			return null;
		while (end < line.length() && allowedChars.indexOf(line.charAt(end)) >= 0)
			++end;
		return line.substring(start, end);
	}

	public static int findFirstNonSpace(String line) {
		return findFirstNonSpace(line, 0);
	}

	public static int findFirstNonSpace(String line, int start) {
		if (line == null)
			return 0;
		if (start >= line.length())
			return line.length();
		int pos = start;
		while (pos < line.length() && (line.charAt(pos) == ' '))
			++pos;
		return pos;
	}

	public static boolean containsAny(String string, String[] parts) {
		if (string == null || parts == null || parts.length == 0)
			return false;

		for (String part : parts) {
			if (string.indexOf(part) >= 0) {
				return true;
			}
		}
		return false;
	}
	
	public static boolean contains(String text, String subtext) {
		if (text == null || subtext == null)
			return false;
		return (text.indexOf(subtext) >= 0);
	}
	
	public static boolean containsIgnoringCase(String text, String subtext) {
		if (text == null || subtext == null)
			return false;
		return (text.toUpperCase().indexOf(subtext.toUpperCase()) >= 0);
	}
	
	public static String removeTags(String text) {
		if (text == null)
			return null;
		StringBuilder sb = new StringBuilder();
		int start = 0;
		while (start < text.length()) {
			int tagStart = text.indexOf('<', start);
			if (tagStart < 0) {
				sb.append(text.substring(start));
				break;
			}
			if (tagStart > start) {
				sb.append(text.substring(start, tagStart));
				start = tagStart;
			}

			// determine whether '<' is really the beginning of a tag
			if (tagStart + 1 == text.length() || (!Character.isLetter(text.charAt(tagStart+1)) && text.charAt(tagStart + 1) != '/')) {
				++tagStart;
				sb.append(text.substring(start, tagStart));
				start = tagStart;
				continue;
			}

			int tagEnd = text.indexOf('>', tagStart);
			if (tagEnd < 0) {
				sb.append(text.substring(start));
				break;
			}
			// skip the <tag> from tagStart to tagEnd
			start = tagEnd + 1;
		} 
		return sb.toString();
	}
}