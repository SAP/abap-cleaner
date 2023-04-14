package com.sap.adt.abapcleaner.comparer;


/**
 * <p>Enumeration that expresses the two versions of text documents in a {@link DiffDoc}: 
 * {@link DisplaySide#LEFT} for the old version, {@link DisplaySide#RIGHT} for the changed version.
 * In this sense, each {@link DiffLine} in a {@link DiffDoc} can contain a 'left' {@link DisplayLine}
 * and a 'right' {@link DisplayLine} (or just one of them, if a line was deleted or added). 
 * </p>   
 */
public enum DisplaySide {
	LEFT, 
	RIGHT;

	public static final int SIZE = java.lang.Integer.SIZE;

	public int getValue() { return this.ordinal(); }

	public static DisplaySide forValue(int value) {
		return values()[value];
	}
}