package com.sap.adt.abapcleaner.rulehelpers;

public class WordFrequency { 
	public int count;
	public int countUpperAndLower;
	public final boolean isGerman;
	public final String firstContext;
	
	public WordFrequency(boolean isGerman, String firstContext) {
		count = 1;
		countUpperAndLower = 0;
		this.isGerman = isGerman;
		this.firstContext = firstContext;
	}
}
