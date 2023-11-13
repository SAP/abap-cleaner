package com.sap.adt.abapcleaner.base;

import java.io.InputStream;

import com.sap.rnd.rndrt.IPadFileResolver;

@SuppressWarnings("restriction")
public class PadResourceResolver implements IPadFileResolver {
	// The resources should always contain the latest ABAP grammar .pad file; due to downward compatibility of ABAP, 
	// this should work for code from older releases, too, because the grammar is not used for a syntax check, but 
	// only to differentiate keywords from identifiers.
	// 
	// To update the resource to the latest ABAP grammar,
	// - download the current grammar file 
	// - update \com.sap.adt.abapcleaner\resources\grammar.txt from that file (keeping the name), and
	// - adjust the release number returned by getRelease() below
	
	@Override
	public InputStream getPadFileContent() {
		return this.getClass().getClassLoader().getResourceAsStream("grammar.txt"); 
	}

	@Override
	public String getRelease() { 
		return "758"; 
	}
}
