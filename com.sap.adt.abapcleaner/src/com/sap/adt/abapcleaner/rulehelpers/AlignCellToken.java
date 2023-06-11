package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.*;

public class AlignCellToken extends AlignCell {
	private final Token token;

	@Override
	public Token getFirstToken() { return token; }

	@Override
	public Token getLastToken() { return token; }

	@Override
	boolean hasInnerComment() { return false; }

	@Override
	boolean hasCommentAtEnd() { return token.isComment(); }

	@Override
	int getMonoLineWidth() { 
		return additionalIndent + ((overrideTextWidth < 0) ? token.getTextLength() : overrideTextWidth); 
	}

	@Override
	int getMultiLineWidth() { 
		return additionalIndent + ((overrideTextWidth < 0) ? token.getTextLength() : overrideTextWidth); 
	}

	@Override
	int getStartIndexInFirstLine() { return token.getStartIndexInLine(); }

	@Override
	int getMaxEndIndexInAnyLine() { return token.getEndIndexInLine(); }

	@Override
	int getEndIndexInLastLine() { return token.getEndIndexInLine(); }

	public AlignCellToken(Token token) {
		if (token == null)
			throw new NullPointerException("token");

		this.token = token;
		oldStartIndexInLine = token.getStartIndexInLine();
	}

	public static AlignCellToken createSpecial(Token Token, int additionalIndent, boolean overrideToTextWidth1) {
		AlignCellToken alignCellToken = new AlignCellToken(Token);
		alignCellToken.additionalIndent = additionalIndent;
		if (overrideToTextWidth1)
			alignCellToken.overrideTextWidth = 1;
		return alignCellToken;
	}

	public AlignCellToken(Token token, int overrideTextWidth, int additionalIndent) {
		if (token == null)
			throw new NullPointerException("token");

		this.token = token;
		this.overrideTextWidth = overrideTextWidth;
		this.additionalIndent = additionalIndent;
		oldStartIndexInLine = this.token.getStartIndexInLine();
	}

	@Override
	boolean setWhitespace(int lineBreaks, int spacesLeft, boolean keepMultiline, boolean condenseInnerSpaces) {
		return token.setWhitespace(lineBreaks, additionalIndent + spacesLeft);
	}
}