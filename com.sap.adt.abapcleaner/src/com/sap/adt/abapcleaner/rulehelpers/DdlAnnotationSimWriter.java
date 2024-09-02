package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

/** simulates writing of DDL annotations for the purpose of retrieving layout data, 
 * particularly DdlAnnotationElement.endIndexInLine and DdlAnnotation.valueEndIndexInLine */
public class DdlAnnotationSimWriter extends DdlAnnotationWriter {

	public DdlAnnotationSimWriter(DdlAnnotationLayout layout, int maxLevel) {
		super(layout, maxLevel);
	}

	@Override
	protected void addToken(int lineBreaks, int spacesLeft, String text, Language language) throws UnexpectedSyntaxException {
		// do nothing
	}

	@Override
	protected void addToLastTokenText(String text) {
		// do nothing
	}

	@Override
	public String toText() {
		// do nothing
		return null;
	}

}
