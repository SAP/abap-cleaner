package com.sap.adt.abapcleaner.comparer;

import java.util.ArrayList;

/**
 * Is used both to show what kind(s) of changes are found in a {@link DiffLine}, 
 * and as a filter for highlighting, or navigating to, a certain selection of change types.
 */
public class ChangeTypes {
	public final boolean indentChanges;
	public final boolean innerSpaceChanges;
	public final boolean caseChanges;
	public final boolean textOrLineChanges;

	public static ChangeTypes createNoChanges() {
		return new ChangeTypes(false, false, false, false);
	}

	public static ChangeTypes createAllChanges() {
		return new ChangeTypes(true, true, true, true);
	}

	public static ChangeTypes create(boolean indentChanges, boolean innerSpaceChanges, boolean caseChanges, boolean textOrLineChanges) {
		return new ChangeTypes(indentChanges, innerSpaceChanges, caseChanges, textOrLineChanges);
	}

	public static ChangeTypes createFrom(ArrayList<HighlightBit> leftHighlightBits, ArrayList<HighlightBit> rightHighlightBits) {
		boolean indentChanges = false;
		boolean innerSpaceChanges = false;
		boolean caseChanges = false;
		boolean textOrLineChanges = false;

		for (HighlightBit highlightBit : leftHighlightBits) {
			switch (highlightBit.type) {
				case INDENT_CHANGE:
					indentChanges = true;
					break;
				case INNER_SPACE_CHANGE:
					innerSpaceChanges = true;
					break;
				case CASE_CHANGE:
					caseChanges = true;
					break;
				case TEXT_CHANGE:
					textOrLineChanges = true;
					break;
			}
		}
		for (HighlightBit highlightBit : rightHighlightBits) {
			switch (highlightBit.type) {
				case INDENT_CHANGE:
					indentChanges = true;
					break;
				case INNER_SPACE_CHANGE:
					innerSpaceChanges = true;
					break;
				case CASE_CHANGE:
					caseChanges = true;
					break;
				case TEXT_CHANGE:
					textOrLineChanges = true;
					break;
			}
		}
		return new ChangeTypes(indentChanges, innerSpaceChanges, caseChanges, textOrLineChanges);
	}

	private ChangeTypes(boolean indentChanges, boolean innerSpaceChanges, boolean caseChanges, boolean textOrLineChanges) {
		this.indentChanges = indentChanges;
		this.innerSpaceChanges = innerSpaceChanges;
		this.caseChanges = caseChanges;
		this.textOrLineChanges = textOrLineChanges;
	}
	
	public boolean anyChanges() {
		return indentChanges || innerSpaceChanges || caseChanges || textOrLineChanges;
	}
	
	public boolean matchesFilter(ChangeTypes filter) {
		return filter.indentChanges && this.indentChanges
			|| filter.innerSpaceChanges && this.innerSpaceChanges
			|| filter.caseChanges && this.caseChanges
			|| filter.textOrLineChanges && this.textOrLineChanges; 
	}
}
