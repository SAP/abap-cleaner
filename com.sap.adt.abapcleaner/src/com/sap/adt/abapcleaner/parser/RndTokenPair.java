package com.sap.adt.abapcleaner.parser;

@SuppressWarnings("restriction")
class RndTokenPair {
	final com.sap.rnd.rndrt.Token rndToken;
	final Token token;
	
	RndTokenPair(com.sap.rnd.rndrt.Token rndToken, Token token) {
		this.rndToken = rndToken;
		this.token = token;
	}
}
