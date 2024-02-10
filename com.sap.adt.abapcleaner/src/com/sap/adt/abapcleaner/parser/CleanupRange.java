package com.sap.adt.abapcleaner.parser;

public class CleanupRange {
	/** the 1-based start line (incl.) at which cleanup shall begin, leaving all lines before it untouched */
	public final int startLine;

	/** the 0-based end line (excl.) at which cleanup shall end, leaving this line and all lines after it untouched */
	public final int lastLine;

	/** true if {@link #startLine} and {@link #endLine} shall still be expanded to cover a whole statement, method/section/form/function or class */
	public final boolean expandRange;

	/**
	 * Creates a CleanupRange instance.
	 * @param startLine - the 1-based start line (incl.) at which cleanup shall begin, leaving all lines before it untouched
	 * @param lastLine - the 1-based last line (incl.) at which cleanup shall end, leaving all lines after it untouched
	 * @param expandRange - true if {@link #startLine} and {@link #endLine} shall still be expanded to cover a whole statement, method/section/form/function or class
	 * @return
	 */
	public static CleanupRange create(int startLine, int lastLine, boolean expandRange) {
		return new CleanupRange(startLine, lastLine, expandRange); 
	}
	
	private CleanupRange(int startLine, int lastLine, boolean expandRange) {
		this.startLine = startLine;
		this.lastLine = lastLine;
		this.expandRange = expandRange;
	}
}
