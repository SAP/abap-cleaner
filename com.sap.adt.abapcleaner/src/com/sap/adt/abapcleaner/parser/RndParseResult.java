package com.sap.adt.abapcleaner.parser;

import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.Cult;

class RndParseResult {
	final ArrayList<RndTokenPair> tokenPairs;
	final int errorTokenCount;
	final Token firstErrorToken;

	RndParseResult(ArrayList<RndTokenPair> tokenPairs, int errorTokenCount, Token firstErrorToken) {
		this.tokenPairs = tokenPairs;
		this.errorTokenCount = errorTokenCount;
		this.firstErrorToken = firstErrorToken;
	}
	
	String getFirstErrorDetails() {
		if (errorTokenCount == 0 || firstErrorToken == null) {
			return "";
		} else { 
			return "at token '" + firstErrorToken.getText() + "'"
			+ " (line " + Cult.format(firstErrorToken.getParentCommand().getSourceLineNumStart()) 
			+ " + " + Cult.format(firstErrorToken.getLineInCommand()) 
			+ ", col " + Cult.format(firstErrorToken.getStartIndexInLine()) + ")";
		}
	}
}
