package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.*;

public class AlignCellTerm extends AlignCell {
	private Term term;

	public final Term getTerm() { return term; }

	@Override
	public Token getFirstToken() { return term.firstToken; }

	@Override
	public Token getLastToken() { return term.lastToken; }

	@Override
	boolean hasInnerComment() { return term.hasInnerComment(); }

	@Override
	boolean hasCommentAtEnd() { return term.lastToken.isComment(); }

	@Override
	int getMonoLineWidth() { 
		return additionalIndent + ((overrideTextWidth < 0) ? term.getSumTextAndSpaceWidth(true) : overrideTextWidth); 
	}

	@Override
	int getMultiLineWidth() { 
		return additionalIndent + ((overrideTextWidth < 0) ? term.getMaxEndIndexInAnyLine(false) - term.firstToken.getStartIndexInLine() : overrideTextWidth); 
	}

	@Override
	int getStartIndexInFirstLine() { return term.firstToken.getStartIndexInLine(); }

	@Override
	int getMaxEndIndexInAnyLine() { return term.getMaxEndIndexInAnyLine(false); }

	@Override
	int getEndIndexInLastLine() { return term.lastToken.getEndIndexInLine(); }

	@Override
	boolean contains(Token searchToken) { return term.contains(searchToken); }
	
	public AlignCellTerm(Term term) {
		if (term == null)
			throw new NullPointerException("term");
		this.term = term;
		oldStartIndexInLine = term.firstToken.getStartIndexInLine();
	}

	public static AlignCellTerm createSpecial(Term term, int additionalIndent, boolean overrideToTextWidth1) {
		AlignCellTerm alignCellTerm = new AlignCellTerm(term);
		alignCellTerm.additionalIndent = additionalIndent;
		if (overrideToTextWidth1)
			alignCellTerm.overrideTextWidth = 1;
		return alignCellTerm;
	}

	@Override
	boolean setWhitespace(int lineBreaks, int spacesLeft, boolean keepMultiline, boolean condenseInnerSpaces, AlignOverlengthAction overlengthAction) {
		boolean changed = false;
		if (term.firstToken.setWhitespace(lineBreaks, additionalIndent + spacesLeft))
			changed = true;

		if (keepMultiline) {
			if (term.addIndent(term.firstToken.getStartIndexInLine() - oldStartIndexInLine)) {
				changed = true;
			}
		} else {
			Token token = term.firstToken;
			while (token != term.lastToken) {
				token = token.getNext();
				if (token.lineBreaks == 0 && !condenseInnerSpaces)
					continue;
				if (token.lineBreaks > 0 && token.getPrev() != null && token.getPrev().isComment())
					continue;
				// if an 'emergency line break' was introduced to avoid exceeding line length, keep the line break  
				// on this Token (e.g. VALUE) and align it with the reference token (e.g. TYPE) 
				if (overlengthAction != null && token.lineBreaks > 0 && token == overlengthAction.lineBreakToken) {
					if (token.setWhitespace(token.lineBreaks, overlengthAction.getSpacesLeft())) {
						changed = true;
					}
					continue;
				}
				// keep comment lines unchanged and do not merge into comments 
				if (token.isCommentLine() || token.getPrev() != null && token.getPrev().isComment())
					continue;
				// if the Token is attached to the previous Token, keep it that way as in "DATA lv_textdat1(20) TYPE c."
				int newSpacesLeft = token.isAttached() ? 0 : 1;
				if (token.setWhitespace(0, newSpacesLeft)) { 
					changed = true;
				}
			}
		}
		return changed;
	}
}
