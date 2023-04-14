package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.StringUtil;

/**
 * Helper class to identify words within comments or literals, also allowing to replace typos with corrected words.
 * Words are usually textual (e.g. within a textual comment or a literal), but identifiers commented-out code 
 * such as lv_variable_name or /namespace/identifier will be returned in one piece
 * (while the brackets of field-symbols will not be considered as part of the word)
 */
class TextReader {
	private final String text;
	private final int length;
	private final boolean isLiteral;
	private final char escapeChar;
	
	// state changed by getNextWord()
	private int start;
	private String lastWord;
	private int lastWordStart;
	
	// correction
	private int correctionStart;
	private StringBuilder correctedText;
	private ArrayList<String> corrections;
	
	/**
	 *  
	 * @param text - the comment to find words in (may include comment signs)
	 */
	public static TextReader createForComment(String text) {
		return new TextReader(text, false, '\0');
	}

	/**
	 *  
	 * @param text - the literal to find words in, excluding the literal's delimiters ' ` | etc.
	 * @param escapeChar - the escape char used for this literal
	 */
	public static TextReader createForLiteral(String text, char escapeChar) {
		return new TextReader(text, true, escapeChar);
	}

	private TextReader(String text, boolean isLiteral, char escapeChar) {
		this.text = text;
		this.length = text.length();
		this.isLiteral = isLiteral;
		this.escapeChar = escapeChar;
		start = 0;
		lastWordStart = 0;
		correctionStart = 0;
	}

	public String getNextWord() {
		boolean isFirst = true;
		StringBuilder word = new StringBuilder();
   	lastWordStart = start;
		
   	int lastCharStart = start;
   	int step = 0;
		while (start < length) {
			step = 1;
			char c = text.charAt(start);
			if (isLiteral && c == escapeChar && start + 1 < length) {
				c = text.charAt(start + 1);
				step = 2;
			}
			if (isCharAllowedForWord(c, isFirst)) {
				lastCharStart = start;
				word.append(c);
				start += step;
				isFirst = false;
			} else if (isFirst) {
				// skip character and try starting the word with the next character
				start += step;
         	lastWordStart = start;
				lastCharStart = start;
				continue;
			} else {
				break;
			}
		}

		// only accept ' inside the word, but not at the end
		if (word.length() > 0 && word.charAt(word.length() - 1) == '\'') {
			word.deleteCharAt(word.length() - 1);
			start = lastCharStart;
		}
		lastWord = word.toString();
		return (word.length() == 0) ? null : lastWord;
	}

	private boolean isCharAllowedForWord(char c, boolean isFirst) {
		// always accept hyphens 
		return Character.isLetterOrDigit(c) || c == '-' || (!isFirst && c == '\'') 
				|| (!isLiteral && (c == '_' || c == '/'));
	}
	
	public void correctLastWord(String newWord, String diagnosis) {
		// if the new word is empty or even null, do nothing
		if (StringUtil.isNullOrEmpty(newWord))
			return;
		
		// use lazy instantiation for the corrected text
		if (correctedText == null)
			correctedText = new StringBuilder();
		if (correctionStart < lastWordStart) {
			correctedText.append(text.substring(correctionStart, lastWordStart));
		}
		if (isLiteral) {
			for (int i = 0; i < newWord.length(); ++i) {
				char c = newWord.charAt(i);
				if (c == escapeChar)
					correctedText.append(escapeChar);
				correctedText.append(c);
			}
		} else {
			correctedText.append(newWord);
		}
		correctionStart = start;
		
		if (corrections == null)
			corrections = new ArrayList<String>();
		
		String correction = lastWord;
		if (!StringUtil.isNullOrEmpty(diagnosis))
			correction += " (" + diagnosis + ")";
		correction += " -> " + newWord;
		corrections.add(correction);
	}
	
	public String getCorrectedText() {
		if (correctionStart == 0)
			return null;
		if (correctionStart < length) {
			correctedText.append(text.substring(correctionStart, length));
		}
		return correctedText.toString();
	}
	
	public ArrayList<String> getCorrections() {
		return corrections;
	}
}
