package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.*;

/**
 * <p>Represents a single word in a {@link CompareLine}, which can be matched 
 * with a CompareWord of another {@link CompareLine} (i.e. a changed version of the line).</p> 
 * 
 * <p>In this context, whitespace (e.g. a certain number of spaces between two words) is also considered a CompareWord. 
 * This allows to identify whitespace changes which occur frequently in formatting of code.</p>
 */
class CompareWord {
	// behavior of text simplification
	private static final boolean SIMPLIFY_CASING = true;

	private static String simplify(String text) {
		if (text == null)
			throw new NullPointerException("text");

		if (SIMPLIFY_CASING)
			text = AbapCult.toUpper(text);
		return text;
	}

	// ----------------------------------------------------------------------

	private CompareLine parentLine;
	final int startIndex;
	final int length;
	final String simplifiedText;

	final boolean textEquals(String text, boolean ignoreCase) {
		return AbapCult.stringEquals(this.text, text, ignoreCase);
	}

	private CompareWord match;

	final String text;

	final CompareWord getMatch() { return match; }

	final int getEndIndex() { return startIndex + length; }

	final boolean isWhitespaceWord() { return (CompareLine.WHITESPACE_CHARS.indexOf(parentLine.getText().charAt(startIndex)) >= 0); }

	@Override
	public String toString() { // for debugging
		return text;
	}

	CompareWord(CompareLine parentLine, int startIndex, int length) {
		this.parentLine = parentLine;
		this.startIndex = startIndex;
		this.length = length;
		text = parentLine.getText().substring(startIndex, startIndex + length);
		simplifiedText = simplify(text);
	}

	final void setMatch(CompareWord other) {
		match = other;
		other.match = this;
	}

	final boolean simplifiedTextContainsAnyLetterOrDigit() {
		String text = simplifiedText;
		char[] textChars = text.toCharArray();
		for (char c : textChars) {
			if (Character.isLetterOrDigit(c))
				return true;
		}
		return false;
	}

	/*
   String sortedChars;
   int getSortedCharsTotalLength() { return sortedChars.length(); };
   String getSortedChars() { return sortedChars; };
   
   private void createChars() {
   	// creates a sorted list of characters in this word (only needed for words that could not be matched using the SimplifiedText)
      if (this.sortedChars != null)
         return;
      
      ArrayList<char> sortedChars = new ArrayList<char>();
      char[] simplifiedTextChars = simplifiedText.toCharArray();
      for (char c : simplifiedTextChars) 
         sortedChars.add(c);
      sortedChars.sort();
      this.sortedChars = new String(sortedChars.toArray());
   }

   private double calcMatchRatioWith(CompareWord other) {
      String otherSortedChars = other.sortedChars;
      int index = 0;
      int otherIndex = 0;
      int commonCharCount = 0;
      while (index < sortedChars.length() && otherIndex < otherSortedChars.length()) {
         char char1 = sortedChars.charAt(index);
         char char2 = otherSortedChars.charAt(otherIndex);
         if (char1 < char2)  // ignoring case is NOT required because sortedChars are in upper case
            ++index;
         else if (char1 == char2)
            ++commonCharCount;
         else
            ++otherIndex;
      }
      double avgTotalLength = (double)(getSortedCharsTotalLength() + other.getSortedCharsTotalLength()) / 2.0;
      return commonCharCount / avgTotalLength;
   }
   */
}