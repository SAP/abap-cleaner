package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.Comparator;

public class DdlAnnotationNestingRange {
	static class NestingRangeComparator implements Comparator<DdlAnnotationNestingRange> {
		@Override
		public int compare(DdlAnnotationNestingRange o1, DdlAnnotationNestingRange o2) {
			if (o1.nestAfterLevel > o2.nestAfterLevel) {
				return -1;
			} else if (o1.nestAfterLevel < o2.nestAfterLevel) {
				return 1;

			} else if (o1.sortedAnnoIndexStart < o2.sortedAnnoIndexStart) {
				return -1;
			} else if (o1.sortedAnnoIndexStart > o2.sortedAnnoIndexStart) {
				return 1;
				
			} else if (o1.sortedAnnoIndexEnd < o2.sortedAnnoIndexEnd) {
				return -1;
			} else if (o1.sortedAnnoIndexEnd > o2.sortedAnnoIndexEnd) {
				return 1;

			} else {
				return 0;
			}
		}
	}

	/** the element level common to all nested annotations; the opening brace { is placed after this element */
	final int nestAfterLevel;
	
	final boolean isArray;
	
	/** the annotations affected by this nesting */
	ArrayList<DdlAnnotation> annotations = new ArrayList<>();
	
	// the temporary index range of the alphabetically sorted annotations   
	private int sortedAnnoIndexStart = Integer.MAX_VALUE;
	private int sortedAnnoIndexEnd = Integer.MIN_VALUE;

	/** true if nesting was used in the original code for at least some of the involved annotations */
	boolean wasActiveInOriginalCode;

	/** the length of the longest element name in this nesting, which can be used for alignment; 
	 * 0 if the nesting contains an element with a dot (and therefore, no alignment is done) */
	int columnWidth;
	
	/** the maximum depth of elements inside this nesting; 
	 * 1 if this nesting only contains one level of elements (and their values) */
	int maxInnerElemDepth;

	/** the maximum depth of nestings or arrays inside this nesting; 0 if this nesting contains no other nestings or arrays */
	int maxInnerNestingDepth;
	
	/** true if the elements of this nesting shall be put on one line */
	boolean writeAsOneLiner; 
	
	public int getAnnotationCount() { return annotations.size(); }
	
	DdlAnnotationNestingRange(int nestAfterLevel, boolean isArray) {
		this.nestAfterLevel = nestAfterLevel;
		this.isArray = isArray;
		this.wasActiveInOriginalCode = isArray;
	}
	
	void addAnnotation(DdlAnnotation annotation, int sortedIndex) {
		annotations.add(annotation);
		annotation.setNestingRange(nestAfterLevel, this);
		
		sortedAnnoIndexStart = Math.min(sortedAnnoIndexStart, sortedIndex);
		sortedAnnoIndexEnd = Math.max(sortedAnnoIndexEnd, sortedIndex + 1);
	}
	
	/** determines the (maximum) column width and set it to all involved elements; returns false if no column alignment 
	 * is possible, because one of the elements is followed by a dot (and another element) */
	void finishBuild() {
		int nestedLevel = nestAfterLevel + 1;
		boolean containsElemFollowedByDot = false;

		wasActiveInOriginalCode = isArray;
		columnWidth = 0;
		maxInnerElemDepth = 0;
		maxInnerNestingDepth = 0;
		
		for (DdlAnnotation annotation : annotations) {
			if (annotation.isElemFollowedDot(nestedLevel)) 
				containsElemFollowedByDot = true;
			wasActiveInOriginalCode |= annotation.originalCodeStartedNestingAfterLevel(nestAfterLevel);
			if (!isArray)
				columnWidth = Math.max(columnWidth, annotation.getElementName(nestedLevel).length());
			maxInnerElemDepth = Math.max(maxInnerElemDepth, annotation.getLevelCount() - nestedLevel);
			maxInnerNestingDepth = Math.max(maxInnerNestingDepth, annotation.getInnerNestingDepthAfterLevel(nestedLevel));
		}

		if (containsElemFollowedByDot) {
			// no alignment possible
			columnWidth = 0;
		}
	}

	boolean isOneLinerCandidate(int maxOneLinerElemCount) {
		return maxInnerNestingDepth == 0 && getAnnotationCount() <= maxOneLinerElemCount;
	}
}
