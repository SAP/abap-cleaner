package com.sap.adt.abapcleaner.rulehelpers;

public class DdlAnnotationLayout {
	// spaces - some settings are not exposed to the user
	public final int spacesAfterAnnoSign = 0;

	public final int spacesBeforeDot = 0;
	public final int spacesAfterDot = 0;
	
	public final int spacesBeforeColon = 0;
	public final int spacesAfterColon;
	
	public final int spacesInsideBraces;
	public final int spacesInsideBrackets;
	
	public final int spacesBeforeComma = 0;
	public final int spacesAfterComma = 1;

	// one-liners
	public int maxOneLinerElemCountMain;
	public int maxOneLinerElemCountList;
	public int maxLineLength;

	// alignment
	public boolean alignValues;
	public boolean alignTablesInArrays;
	
	// context
	public int initialLineBreaks;
	public int basicIndent;
	
	// empty lines
	public DdlAnnotationEmptyLines emptyLines;

	public DdlAnnotationLayout(
			int spacesAfterColon, int spacesInsideBraces, int spacesInsideBrackets,
			int maxLineLength, int maxOneLinerElemCountMain, int maxOneLinerElemCountList,
			boolean alignValues, boolean alignTablesInArrays) {

		// spaces
		this.spacesAfterColon = spacesAfterColon;
		this.spacesInsideBraces = spacesInsideBraces;
		this.spacesInsideBrackets = spacesInsideBrackets;

		// one-liners
		this.maxLineLength = maxLineLength;
		this.maxOneLinerElemCountMain = maxOneLinerElemCountMain;
		this.maxOneLinerElemCountList = maxOneLinerElemCountList;

		// alignment
		this.alignValues = alignValues;
		this.alignTablesInArrays = alignTablesInArrays;
	}

	public void setContext(int initialLineBreaks, int basicIndent) {
		this.initialLineBreaks = initialLineBreaks;
		this.basicIndent = basicIndent;
	}
	
	public void setEmptyLines(DdlAnnotationEmptyLines emptyLines) {
		this.emptyLines = emptyLines;
	}
}
