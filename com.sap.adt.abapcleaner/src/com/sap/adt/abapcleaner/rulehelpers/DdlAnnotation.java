package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.Comparator;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Token;

public class DdlAnnotation {
	static class AlphabeticComparator implements Comparator<DdlAnnotation> {
		@Override
		public int compare(DdlAnnotation o1, DdlAnnotation o2) {
			return o1.abcSortKey.compareTo(o2.abcSortKey);
		}
	}

	static class SequenceComparator implements Comparator<DdlAnnotation> {
		@Override
		public int compare(DdlAnnotation o1, DdlAnnotation o2) {
			return o1.seqSortKey.compareTo(o2.seqSortKey);
		}
	}

	// -------------------------------------------------------------------------

	static final String ARRAY_SIGN = "[]";
	
	/** the original position of this annotation in the DdlAnnotationScope */
   private int posInSequence;
   
   /** elements contain 'Anno', 'subAnno' or 'subAnno[]', but no '@' or '.' */
   private DdlAnnotationElement[] elements;  

   /** the sort key for alphabetical sorting */
   private String abcSortKey;

   /** the annotation value, if any */
   private String value;         
   
   /** the Token that contains the annotation value or alternatively, the last part of the annotation name */
   private Token valueToken; 
   
   /** the number of line breaks found at Command start or after the previous comma */
   private int originalLineBreaks; 
   
   // variables used for nesting
   private String seqSortKey;

   /** last position of the value when last written (or simulated) with DdlAnnotationScope.writeTo() */
   public int valueEndIndexInLine;  

	/** the length of the longest value amongst all annotations that belong to (structure-identical) anonymous nestings in an array; 
	 * 0 if no alignment of values is possible */
	public int valueColumnWidth;

	// -------------------------------------------------------------------------
   
   int getLevelCount() { return elements.length; }

   public int getPosInSequence() { return posInSequence; }
   public String getValue() { return value; }
   public Token getValueToken() { return valueToken; }
   public int getOriginalLineBreaks() { return originalLineBreaks; }
   
   static DdlAnnotation create(int posInSequence, ArrayList<DdlAnnotationElement> elements, boolean lastIsValue, Token lastToken, int lineBreaks) {
   	return new DdlAnnotation(posInSequence, elements, lastIsValue, lastToken, lineBreaks);
   }
   
   private DdlAnnotation(int posInSequence, ArrayList<DdlAnnotationElement> elements, boolean lastIsValue, Token lastToken, int lineBreaks) {
   	this.posInSequence = posInSequence;

   	StringBuilder abcSortKeyBuilder = new StringBuilder();
   	int elementCount = lastIsValue ? elements.size() - 1 : elements.size();
   	this.elements = new DdlAnnotationElement[elementCount];
   	for (int level = 0; level < elementCount; ++level) {
   		this.elements[level] = new DdlAnnotationElement(elements.get(level));
   		
   		if (abcSortKeyBuilder.length() > 0)
   			abcSortKeyBuilder.append(DDL.DOT_SIGN_STRING);
   		abcSortKeyBuilder.append(elements.get(level).getNameKey());
   	}
   	this.value = lastIsValue ? elements.get(elementCount).name : null;
   	this.valueToken = lastToken;
   	this.originalLineBreaks = lineBreaks;
   	this.abcSortKey = abcSortKeyBuilder.toString();
   	this.seqSortKey = "";
   }
   
   @Override
   public String toString() { // only for debugging
   	// cp. DdlAnnotationScope.writeTo(...)
   	StringBuilder sb = new StringBuilder();
   	sb.append(seqSortKey).append(": ");
   	
   	boolean addDot = false;
   	for (int index = 0; index < elements.length; ++index) {
   		if (index == 0) 
   			sb.append(DDL.ANNOTATION_SIGN_STRING);
   		else if (addDot)
   			sb.append(DDL.DOT_SIGN_STRING);
  			sb.append(getElementName(index));

  			addDot = false;
  			if (startsNestingAfterLevel(index)) {
				if (!hasAnonymousArrayObjectAt(index)) 
					sb.append(DDL.COLON_SIGN_STRING + " ");
				if (hasArrayStartAt(index)) {
					sb.append(DDL.BRACKET_OPEN_STRING + " ");
				} else {
					sb.append(DDL.BRACE_OPEN_STRING + " ");
				}
  			} else {
  				addDot = true;
  			}
   	}
   	if (value != null) {
   		if (!isValueInArray()) 
  				sb.append(DDL.COLON_SIGN_STRING + " ");
			sb.append(value);
   	}
   	return sb.toString();
   }
   
	void initializeSortKeys(int digits) {
		seqSortKey = StringUtil.getNumberWithLeadingZeros(posInSequence, digits);
		// also append the seqSortKey to the alphabetical sort key, which is otherwise identical for multiple array values
		abcSortKey += DDL.DOT_SIGN_STRING + seqSortKey;
	}
	
	void addPrefixToSeqSortKey(String prefix) {
		seqSortKey = prefix + seqSortKey;
	}
	
	int getLevelOfFirstDiff(DdlAnnotation other) {
		int levelCountMin = Math.min(elements.length, other.elements.length);
		int commonLevelCount = 0;
		while (commonLevelCount < levelCountMin && elements[commonLevelCount].matches(other.elements[commonLevelCount])) {
			++commonLevelCount;
		}
		return commonLevelCount; 
	}
	
	boolean hasArrayStartAt(int level) {
		return (level >= 0 && level < elements.length) && elements[level].startsArray;
	}

	boolean startsNestingAfterLevel(int level) {
		return (level >= 0 && level < elements.length) && elements[level].nestingRange != null;
	}

	boolean originalCodeStartedNestingAfterLevel(int level) {
		return (level >= 0 && level < elements.length) && elements[level].startedNestingInOriginalCode;
	}

	int getInnerNestingDepthAfterLevel(int level) {
		int count = 0;
		for (int i = level; i < elements.length; ++i) {
			if (startsNestingAfterLevel(i) || hasAnonymousArrayObjectAt(i)) {
				++count;
			}
		}
		return count;
	}

	void setNestingRange(int level, DdlAnnotationNestingRange nestingRange) {
		elements[level].nestingRange = nestingRange;
	}

	boolean hasAnonymousArrayObjectAt(int level) {
		return (level >= 0 && level < elements.length) && elements[level].isAnonymousArrayObject();
	}

	String getElementName(int level) {
		return (level >= 0 && level < elements.length) ? elements[level].name : "";
	}
	
	int getColumnWidth(int level) {
		if (level == 0)
			return 0;
		DdlAnnotationNestingRange nestingRange = elements[level - 1].nestingRange; 
		return (nestingRange == null || nestingRange.writeAsOneLiner) ? 0 : nestingRange.columnWidth; 
	}
	
	int getValueColumnWidth() {
		// only return the valueColumnWidth if the last element belongs to a one-liner nesting inside an array
		int nestedAfterLevel = elements.length - 2;
		if (startsNestingAfterLevel(nestedAfterLevel) && hasArrayStartAt(nestedAfterLevel - 1)) {
			DdlAnnotationNestingRange nestingRange =  elements[nestedAfterLevel].nestingRange; 
			return (nestingRange == null || !nestingRange.writeAsOneLiner) ? 0 : valueColumnWidth; 
		} else {
			return 0;
		}
	}

	boolean isValueInArray() {
		return hasArrayStartAt(elements.length - 1);
	}
	
	boolean startsNestingOrArrayBefore(int level) {
		for (int i = 0; i < level; ++i) {
			if (startsNestingAfterLevel(i)) {
				return true;
			}
		}
		return false;
	}
	
   /** returns true if the annotation element at the supplied level is followed by a dot (not by a colon) */
   boolean isElemFollowedDot(int level) {
   	return !startsNestingAfterLevel(level) && level + 1 < getLevelCount(); 
   }

	/** checks whether the starting elements of this annotation match any entry in the list; 
	 * and returns the number of elements of the shortest list entry (or -1 if no match was found) */
	public int getMatchElemCountMin(String[] annotationList) {
		int elemCountMin = Integer.MAX_VALUE;
		for (String checkAnnotation : annotationList) {
			if (StringUtil.startsWith(abcSortKey, checkAnnotation, true)
				 && (abcSortKey.length() == checkAnnotation.length() || abcSortKey.charAt(checkAnnotation.length()) == DDL.DOT_SIGN)) {
				int elemCount = StringUtil.instrCount(checkAnnotation, DDL.DOT_SIGN) + 1;
				elemCountMin = Math.min(elemCountMin, elemCount);
			}
		}
		return (elemCountMin == Integer.MAX_VALUE) ? -1 : elemCountMin;
	}
	
	public int getEndIndexInLine(int level) {
		return (level < elements.length) ? elements[level].endIndexInLine : 0;
	}
	public void setEndIndexInLine(int level, int endIndex) {
		elements[level].endIndexInLine = endIndex;
	}

	public int getValueEndIndexInLine() {
		return valueEndIndexInLine;
	}
	public void setValueEndIndexInLine(int endIndex) {
		valueEndIndexInLine = endIndex;
	}
	
	public boolean continueOnSameLine(int nestingStartLevel) {
		return startsNestingAfterLevel(nestingStartLevel - 1) && elements[nestingStartLevel - 1].nestingRange.writeAsOneLiner;
	}
	
	public boolean canNestingInclude(DdlAnnotation other, int nestAfterLevel, boolean fillUpExisting) {
		return getLevelOfFirstDiff(other) > nestAfterLevel 
				&& (fillUpExisting || other.originalCodeStartedNestingAfterLevel(nestAfterLevel));
	}
	
	public boolean isNestedWith(DdlAnnotation other, int nestAfterLevel) {
		return startsNestingAfterLevel(nestAfterLevel) && elements[nestAfterLevel].nestingRange == other.elements[nestAfterLevel].nestingRange;
	}
	
	/** returns a path like "Hierarchy.parentChild[].recurse.child[]" that can be compared with DDL.elementRefAnnotations etc. */
	public String getPath() {
		StringBuilder sb = new StringBuilder();
		for (DdlAnnotationElement element : elements) {
			if (element.isAnonymousArrayObject())
				continue;

			if (sb.length() > 0)
				sb.append(".");
			sb.append(element.name);
			
			if (element.startsArray) {
				sb.append("[]");
			}
		}
		return sb.toString();
	}
	
	public boolean lastElementContains(String textAtStart, String textAfterStart) {
		if (elements.length < 1)
			return false;
		String lastElementName = elements[elements.length - 1].name;
		return lastElementName.startsWith(textAtStart) || lastElementName.indexOf(textAfterStart) > 0;
	}
}
