package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import java.util.*;

/**
 * <p>Represents a single line in a {@link CompareDoc}, i.e. one version of a single text line, 
 * which is split into {@link CompareWord}s 
 * and can be compared with another {@link CompareLine} (i.e. a changed version of the line) 
 * using the {@link #compareTo(CompareLine) compareTo()} method.</p>
 *  
 * <p>The result of the comparison is a {@link DiffLine}, which is created in {@link DiffDoc#add(CompareLine, CompareLine, LineStatus) DiffDoc.add()}</p>
 */
class CompareLine {
	// behavior of text simplification
	private static final boolean simplifyIndent = true;
	private static final boolean simplifyCasing = true;
	private static final boolean simplifyWhitespace = true;

	static final String WHITESPACE_CHARS = " \t\u00a0";
	private static final String CHARS_CONSIDERED_PART_OF_WORD = "_-";

	private enum WordType {
		WHITESPACE, 
		LETTERS_AND_DIGITS, 
		SPECIAL_CHARS;

		/*
		public int getValue() { return this.ordinal(); }

		public static WordType forValue(int value) {
			return values()[value];
		}
		*/
	}

	static String simplify(String text, boolean simplifyCasing, boolean simplifyIndent, boolean simplifyWhitespace) {
		if (text == null)
			throw new NullPointerException("text");

		if (simplifyCasing)
			text = AbapCult.toUpper(text);
		if (simplifyIndent)
			text = text.trim();
		if (simplifyWhitespace) {
			StringBuilder result = new StringBuilder(text.length());
			boolean skipWhitespace = false;
			char[] textChars = text.toCharArray();
			for (char c : textChars) {
				if (WHITESPACE_CHARS.indexOf(c) < 0) {
					// always insert a space before the period . and the comment sign "
					// (otherwise the rules SpaceBeforePeriodRule and SpaceBeforeCommentSignRule will be considered more than a whitespace change)
					if ((c == ABAP.DOT_SIGN || c == ABAP.COMMENT_SIGN) && !skipWhitespace)
						result.append(' ');

					result.append(c);

					// always insert a space after the comment sign " (otherwise the rule SpaceAfterCommentSignRule will be considered more than a whitespace change)
					if (c == ABAP.COMMENT_SIGN) {
						result.append(' ');
						skipWhitespace = true;
					} else {
						skipWhitespace = false;
					}
				} else if (!skipWhitespace) {
					result.append(' ');
					skipWhitespace = true;
				}
			}
			return result.toString();
		} else {
			return text;
		}
	}

	// ----------------------------------------------------------------------
	// instance attributes 
	
	final DisplayLine displayLine;
	final String simplifiedText;

	private ArrayList<CompareWord> words; // must initially be null

	private ArrayList<String> sortedWords; // must initially be null
	private int sortedWordsTotalLength;

	private CompareLine match;

	final CompareLine getMatch() { return match; }

	final boolean hasMatch() { return (match != null); }

	final String getText() { return displayLine.getText(); }

	final Command getParentCommand() { return (displayLine == null) ? null : displayLine.parentCommand; }

	Command getOriginalParentCommand() { return (displayLine == null) ? null : displayLine.parentCommand.originalCommand; }

	final String getCompareString() {
		String idString = (displayLine != null && displayLine.parentCommand != null) ? displayLine.parentCommand.getIdString() : "";
   	return idString + simplifiedText;
	}

	@Override
	public String toString() { // for debugging
		return getText();
	}

	// ----------------------------------------------------------------------

	CompareLine(DisplayLine displayLine) {
		this.displayLine = displayLine;
		simplifiedText = simplify(displayLine.getText(), simplifyCasing, simplifyIndent, simplifyWhitespace);
	}

	final void setMatch(CompareLine other) {
		match = other;
		other.match = this;
	}

	private void createWords() {
		if (words != null)
			return;

		// to increase performance, use local variables
		String text = getText();
		int textLength = text.length();

		words = new ArrayList<CompareWord>();
		WordType lastWordType = WordType.WHITESPACE;
		int writePos = 0;
		int readPos = 0;

		// special treatment for * comments: the first * is a separate word, even if more * signs (or other special chars) follow directly
		if (!StringUtil.isNullOrEmpty(text) && text.charAt(0) == ABAP.LINE_COMMENT_SIGN) {
			words.add(new CompareWord(this, 0, 1));
			writePos = 1;
			readPos = 1;
			// leave lastWordType = WordType.whitespace, since text may continue with further * signs which shall, however, be added as a separate Word
		}

		while (readPos < textLength) {
			char readChar = text.charAt(readPos);
			WordType wordType;
			if (WHITESPACE_CHARS.indexOf(readChar) >= 0)
				wordType = WordType.WHITESPACE;
			else if (Character.isLetterOrDigit(readChar) || (CHARS_CONSIDERED_PART_OF_WORD.indexOf(readChar) >= 0))
				wordType = WordType.LETTERS_AND_DIGITS;
			else
				wordType = WordType.SPECIAL_CHARS;

			if (readPos == 0)
				lastWordType = wordType;
			else if (lastWordType != wordType) {
				if (lastWordType != WordType.WHITESPACE)
					words.add(new CompareWord(this, writePos, readPos - writePos));
				writePos = readPos;
				lastWordType = wordType;
			}
			++readPos;
		}
		// add last word
		if (writePos < textLength)
			words.add(new CompareWord(this, writePos, textLength - writePos));
	}

	private void createSortedWords() {
		if (words == null)
			createWords();

		sortedWords = new ArrayList<String>();
		for (CompareWord compareWord : words) {
			String text = compareWord.text;
			if (!compareWord.isWhitespaceWord()) {
				if (simplifyCasing)
					text = AbapCult.toUpper(text);
				sortedWords.add(text);
				sortedWordsTotalLength += text.length();
			}
		}
		Collections.sort(sortedWords);
	}

	final double calcMatchRatioWith(CompareLine other) {
		if (other == null)
			throw new NullPointerException("other");

		// since CompareDoc.compareTo() does not match empty lines out of sequence, this must be checked here
		if (StringUtil.isNullOrEmpty(simplifiedText) && StringUtil.isNullOrEmpty(other.simplifiedText))
			return 1.0;

		if (sortedWords == null)
			createSortedWords();
		if (other.sortedWords == null)
			other.createSortedWords();

		int index = 0;
		int otherIndex = 0;
		// int commonWordCount = 0;
		int commonCharCount = 0;
		while (index < sortedWords.size() && otherIndex < other.sortedWords.size()) {
			String word1 = sortedWords.get(index);
			String word2 = other.sortedWords.get(otherIndex);

			int compareResult = word1.compareTo(word2); // ignoring case is NOT required because sortedWords are in upper case
			if (compareResult < 0)
				++index;
			else if (compareResult > 0)
				++otherIndex;
			else {
				// ++commonWordCount;
				commonCharCount += word1.length();
				++index;
				++otherIndex;
			}
		}
		double avgTotalLength = (double) (sortedWordsTotalLength + other.sortedWordsTotalLength) / 2.0;
		return commonCharCount / avgTotalLength;
	}

	/*
	private int countInstancesOf(String wordText, boolean ignoreCase) {
		if (words == null)
			createWords();
		int result = 0;
		for (CompareWord word : words) {
			if (word.textEquals(wordText, ignoreCase))
				++result;
		}
		return result;
	}
	*/
	
	private HashMap<String, Integer> getCountOfSpecialTexts() {
		if (words == null)
			createWords();

		HashMap<String, Integer> countOfSpecialTexts = new HashMap<String, Integer>();
		for (CompareWord word : words) {
			if (!word.simplifiedTextContainsAnyLetterOrDigit()) {
				String text = word.simplifiedText;
				if (countOfSpecialTexts.containsKey(text))
					countOfSpecialTexts.put(text, countOfSpecialTexts.get(text) + 1);
				else
					countOfSpecialTexts.put(text, 1);
			}
		}
		return countOfSpecialTexts;
	}

	final void compareTo(CompareLine other) {
		if (other == null)
			throw new NullPointerException("other");

		if (words == null)
			createWords();
		if (other.words == null)
			other.createWords();

		int maxA = words.size();
		int maxB = other.words.size();
		HashMap<String, Integer> wordScopeA = new HashMap<String, Integer>();
		HashMap<String, Integer> wordScopeB = new HashMap<String, Integer>();

		HashMap<String, Integer> countOfSpecialTexts = getCountOfSpecialTexts();
		HashMap<String, Integer> otherCountOfSpecialTexts = other.getCountOfSpecialTexts();

		// bool hasNumberOfParenthesesChanged = (countInstancesOf("(") != other.countInstancesOf("("));
		// bool hasNumberOfEqualsSignsChanged = (countInstancesOf("=") != other.countInstancesOf("="));

		// find pairs of matching (or otherwise, at least similar) words
		int indexA = 0;
		int indexB = 0;
		while (indexA < maxA && indexB < maxB) {
			// is the next word pair in the sequence a direct match?
			CompareWord wordA = words.get(indexA);
			CompareWord wordB = other.words.get(indexB);
			if (wordA.simplifiedText.equals(wordB.simplifiedText)) {
				wordA.setMatch(wordB);
				++indexA;
				++indexB;
				continue;
			}

			// gradually increase search distance to find the next word pair with a direct match;
			// with the HashMap's hash algorithm, this only requires linear runtime
			boolean found = false;
			int testIndexA = indexA + 1;
			int testIndexB = indexB + 1;
			wordScopeA.put(wordA.simplifiedText, indexA);
			wordScopeB.put(wordB.simplifiedText, indexB);
			while (testIndexA < maxA || testIndexB < maxB) {
				if (testIndexA < maxA) {
					wordA = words.get(testIndexA);
					if (wordScopeB.containsKey(wordA.simplifiedText)) {
						int matchIndexB = wordScopeB.get(wordA.simplifiedText);
						// if the number of special texts like "(", "=", "=>" etc. in this line has changed,
						// then do not match two of these special texts, because they may not be a real match,
						// and matching keywords may be skipped
						if (wordA.simplifiedTextContainsAnyLetterOrDigit() || countOfSpecialTexts.get(wordA.simplifiedText).equals(otherCountOfSpecialTexts.get(wordA.simplifiedText))) {
							// a "findSimilarWords" method (analogous to CompareDoc.findSimilarLines) does not make sense here,
							// since the rules do not change words (but rather, whitespace, word order, etc.)
							wordA.setMatch(other.words.get(matchIndexB));
							indexA = testIndexA + 1;
							indexB = matchIndexB + 1;
							found = true;
							break;
						}
					}
					// if two words have the same text, it is enough to enter the first one
					if (!wordScopeA.containsKey(wordA.simplifiedText))
						wordScopeA.put(wordA.simplifiedText, testIndexA);
				}
				if (testIndexB < maxB) {
					wordB = other.words.get(testIndexB);
					if (wordScopeA.containsKey(wordB.simplifiedText)) {
						int matchIndexA = wordScopeA.get(wordB.simplifiedText);
						// if the number of special texts like "(", "=", "=>" etc. in this line has changed,
						// then do not match two of these special texts, because they may not be a real match,
						// and matching keywords may be skipped
						if (wordB.simplifiedTextContainsAnyLetterOrDigit() || countOfSpecialTexts.get(wordB.simplifiedText).equals(otherCountOfSpecialTexts.get(wordB.simplifiedText))) {
							// a "findSimilarWords" method (analogous to CompareDoc.findSimilarLines) does not make sense here,
							// since the rules do not change words (but rather, whitespace, word order, etc.)
							words.get(matchIndexA).setMatch(wordB);
							indexA = matchIndexA + 1;
							indexB = testIndexB + 1;
							found = true;
							break;
						}
					}
					// if two words have the same text, it is enough to enter the first one
					if (!wordScopeB.containsKey(wordB.simplifiedText))
						wordScopeB.put(wordB.simplifiedText, testIndexB);
				}
				++testIndexA;
				++testIndexB;
			}

			// clear HashMaps for next usage, also freeing memory
			wordScopeA.clear();
			wordScopeB.clear();

			if (!found)
				break;
		}
	}

	final ArrayList<HighlightBit> getHighlightBits() {
		ArrayList<HighlightBit> result = new ArrayList<HighlightBit>();

		boolean isFirst = true;
		CompareWord lastMatch = null;
		int lastWordEndIndex = 0;
		int lastMatchEndIndex = 0;
		for (CompareWord word : words) {
			CompareWord match = word.getMatch();
			if (match == null || !word.text.equals(match.text)) {
				// mark the whole word (because it was deleted / added);
				// if multiple consecutive words were added or deleted, include the separating space in between these words in the highlighting
				HighlightBitType type;
				if (word.isWhitespaceWord())
					type = (words.get(0).equals(word)) ? HighlightBitType.INDENT_CHANGE : HighlightBitType.WHITESPACE_CHANGE;
				else if (match != null && AbapCult.toUpper(word.text).equals(AbapCult.toUpper(match.text)))
					type = HighlightBitType.CASE_CHANGE;
				else
					type = HighlightBitType.CONTENT_CHANGE;
				if (!isFirst && match == null && lastMatch == null && word.startIndex == lastWordEndIndex + 1)
					result.add(new HighlightBit(word.startIndex - 1, word.length + 1, type));
				else
					result.add(new HighlightBit(word.startIndex, word.length, type));

			} else if (isFirst || lastMatch != null) {
				// check whether the whitespace on the left-hand side of the word was extended;
				// (if it was shrinked, the extra spaces will be shown in the other document)
				// this only makes sense if this word and the last one could be matched to consecutive words in the other document
				int spaceCount = word.startIndex - lastWordEndIndex;
				int spaceCountMatch = match.startIndex - lastMatchEndIndex;
				if (spaceCount > spaceCountMatch) {
					int diff = spaceCount - spaceCountMatch;
					result.add(new HighlightBit(word.startIndex - diff, diff, isFirst ? HighlightBitType.INDENT_CHANGE : HighlightBitType.WHITESPACE_CHANGE));
				}
				isFirst = false;
			}

			// update variables for next loop cycle
			lastWordEndIndex = word.getEndIndex();
			lastMatch = match;
			if (lastMatch != null)
				lastMatchEndIndex = lastMatch.getEndIndex();
		}

		// condense attached HighlightBit entries
		ArrayList<HighlightBit> condensedResult = new ArrayList<HighlightBit>();
		HighlightBit lastEntry = null;
		for (HighlightBit highlightBit : result) {
			if (lastEntry != null && lastEntry.getEnd() == highlightBit.start && lastEntry.type == highlightBit.type)
				lastEntry.length += highlightBit.length;
			else {
				lastEntry = new HighlightBit(highlightBit.start, highlightBit.length, highlightBit.type);
				condensedResult.add(lastEntry);
			}
		}
		return condensedResult;
	}

	final String getTextWithHighlightedChanges() {
		int writtenCharCount = 0;
		StringBuilder result = new StringBuilder();
		for (CompareWord word : words) {
			if (word.startIndex > writtenCharCount) {
				result.append(StringUtil.repeatChar(' ', word.startIndex - writtenCharCount));
				writtenCharCount = word.startIndex;
			}
			String text = word.text;
			if (word.getMatch() == null)
				result.append("[").append(text).append("]");
			else if (!word.getMatch().text.equals(text))
				result.append("{").append(text).append("}");
			else
				result.append(text);
			writtenCharCount += word.length;
		}
		return result.toString();
	}

	final boolean matchesCurrentOrOriginalCommand(CompareLine other) {
		return (getParentCommand() == other.getParentCommand()) || (getOriginalParentCommand() != null && getOriginalParentCommand() == other.getOriginalParentCommand());
	}
}
