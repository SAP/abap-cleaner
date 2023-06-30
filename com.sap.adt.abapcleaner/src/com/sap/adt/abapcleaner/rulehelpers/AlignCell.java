package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;

public abstract class AlignCell {
	AlignColumn parentColumn;

	protected int oldStartIndexInLine;
	protected int additionalIndent;
	protected int overrideTextWidth = -1;

	public abstract Token getFirstToken();

	public abstract Token getLastToken();

	abstract boolean hasInnerComment();

	abstract boolean hasCommentAtEnd();

	abstract int getMonoLineWidth();

	abstract int getMultiLineWidth();

	abstract int getStartIndexInFirstLine();

	abstract int getMaxEndIndexInAnyLine();

	abstract int getEndIndexInLastLine();

	abstract boolean setWhitespace(int lineBreaks, int spacesLeft, boolean keepMultiline, boolean condenseInnerSpaces);

	final String getSimplifiedText() { return getSimplifiedText("|"); }
	final String getSimplifiedText(String lineBreakSign) {
		StringBuilder text = new StringBuilder();
		Token token = getFirstToken();
		do {
			if (text.length() > 0)
				text.append(token.lineBreaks > 0 ? lineBreakSign : " ");

			String tokenText;
			if (token.isKeyword() || token.isPragma())
				tokenText = AbapCult.toUpper(token.getText());
			else if (token.isIdentifier())
				tokenText = AbapCult.toLower(token.getText());
			else // comment, literal etc.
				tokenText = token.getText();

			text.append(tokenText);
			if (token == getLastToken())
				break;
			token = token.getNext();
		} while (true);
		return text.toString();
	}

	final boolean startsWithSameObjectAs(AlignCell other) {
		Token tokenA = getFirstToken();
		Token tokenB = other.getFirstToken();
		String tokenAObject = ABAP.readTillEndOfVariableName(tokenA.getText(), 0, true);
		String tokenBObject = ABAP.readTillEndOfVariableName(tokenB.getText(), 0, true);
		return AbapCult.stringEquals(tokenAObject, tokenBObject, true);
	}

	final int getActualMultiLineWidth() {
		return additionalIndent + getEndIndexInLastLine() - getStartIndexInFirstLine(); 
	}
	
	public final void setOverrideTextWidth(int overrideTextWidth) {
		this.overrideTextWidth = overrideTextWidth;
		if (parentColumn != null)
			parentColumn.invalidate();
	}
	
	public final boolean overridesTextWidth() {
		return (overrideTextWidth >= 0);
	}
	
	public void setAdditionalIndent(int additionalIndent) {
		this.additionalIndent = additionalIndent;
		parentColumn.invalidate();
	}
}