package com.sap.adt.abapcleaner.rulehelpers;

public class DdlAnnotationElement {
	private static final String ANONYMOUS_OBJECT_START = "*";

	public final String name;
	public boolean startedNestingInOriginalCode;
	public boolean startsArray;

	/** used temporarily if startsArray == true */
	public int anonymousObjectCount;
	
	public DdlAnnotationNestingRange nestingRange;
	
	/** end position when last written (or simulated) with DdlAnnotationScope.writeTo() */
	public int endIndexInLine;  
	
	// -------------------------------------------------------------------------

	public String getNameKey() { return name.toUpperCase(); }
	
	public static DdlAnnotationElement create(String name) {
		return new DdlAnnotationElement(name);
	}
	
	public static DdlAnnotationElement createAnonymousObject(int number) {
		// use '1000 + number' to ensure that the alphabetical sort key (DdlAnnotation.abcSortKey) does not put *12 before *2 
		return new DdlAnnotationElement(ANONYMOUS_OBJECT_START + String.valueOf(1000 + number));
	}
	
	private DdlAnnotationElement(String name) {
		this.name = name;
	}
	
	public DdlAnnotationElement(DdlAnnotationElement model) {
		this.name = model.name;
		this.startedNestingInOriginalCode = model.startedNestingInOriginalCode;
		this.startsArray = model.startsArray;
		// .nestingRange is not yet filled at this point
		// .anonymousObjectCount is not copied, since its use is not relevant after this point
	}
	
	public boolean isAnonymousArrayObject() { 
		return name.startsWith(ANONYMOUS_OBJECT_START); 
	}

	public boolean matches(DdlAnnotationElement other) {
		return name.equalsIgnoreCase(other.name) && (startsArray == other.startsArray);
	}
}
