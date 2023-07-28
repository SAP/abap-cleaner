package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.Token;

public class AlignOverlengthAction {
	public final Token lineBreakToken;
	private Token fallbackToken1;
	private Token fallbackToken2;
	private int requiredWidth;
	private int maxLineLength;
	
	public AlignOverlengthAction(Token lineBreakToken, Token fallbackToken1, Token fallbackToken2, int requiredWidth, int maxLineLength) {
		if (lineBreakToken == null || fallbackToken1 == null)
			throw new IllegalArgumentException();
		this.lineBreakToken = lineBreakToken;
		this.fallbackToken1 = fallbackToken1;
		this.fallbackToken2 = fallbackToken2;
		this.requiredWidth = requiredWidth;
		this.maxLineLength = maxLineLength;
	}
	
	public int getSpacesLeft() {
		if (fallbackToken2 == null || fallbackToken1.getStartIndexInLine() + requiredWidth <= maxLineLength) {
			return fallbackToken1.getStartIndexInLine();
		} else { 
			return fallbackToken2.getStartIndexInLine();
		}
	}
}
