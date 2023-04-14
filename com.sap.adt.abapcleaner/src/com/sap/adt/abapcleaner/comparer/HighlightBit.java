package com.sap.adt.abapcleaner.comparer;

/**
 * <p>Represents a character range ({@link #start} position, {@link #length}) inside a {@link DisplayLine} 
 * for which a certain type of change ({@link HighlightBitType}) was detected 
 * when comparing two versions of the line.</p>
 */
public class HighlightBit {
	public final int start;
	public int length;
	final HighlightBitType type;

	final int getEnd() { return start + length; }

	HighlightBit(int start, int length, HighlightBitType type) {
		this.start = start;
		this.length = length;
		this.type = type;
	}
}