package com.sap.adt.abapcleaner.parser;

/**
 * Encapsulates all information required to parse ABAP code, calling {@link Code#parse(com.sap.adt.abapcleaner.programbase.IProgress, ParseParams)}.
 */
public class ParseParams {
	public final String sourceName;
	public final String codeText;

	/** the ABAP release (e.g. "757" for release 7.57) against which this code must compile, 
	 * see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews.htm">ABAP - Release News</a> */
	public final String abapRelease;

	public final CleanupRange cleanupRange;
	public final CleanupRangeExpandMode cleanupRangeExpandMode;
	public final int lineNumOffset;
	public final int surroundingTextOffset;
	public final Code surroundingCode;

	/** Create parse parameters for parsing a new ABAP code document */
	public static ParseParams createForWholeCode(String sourceName, String codeText, String abapRelease) {
		return new ParseParams(sourceName, codeText, abapRelease, null, CleanupRangeExpandMode.FULL_DOCUMENT, 1, 0, null);
	}

	/** Create parse parameters for parsing a new ABAP code document */
	public static ParseParams createForCleanupRange(String sourceName, String codeText, String abapRelease, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode) {
		return new ParseParams(sourceName, codeText, abapRelease, cleanupRange, cleanupRangeExpandMode, 1, 0, null);
	}

	/** Create parse parameters for parsing a new ABAP code document inside a Unit Test */
	public static ParseParams createForTest(String codeText, String abapRelease) {
		return new ParseParams("test", codeText, abapRelease, null, CleanupRangeExpandMode.FULL_DOCUMENT, 1, 0, null);
	}

	/** Create parse parameters for parsing a new ABAP code document inside a Unit Test */
	public static ParseParams createForTest(String codeText, String abapRelease, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode) {
		return new ParseParams("test", codeText, abapRelease, cleanupRange, cleanupRangeExpandMode, 1, 0, null);
	}

	/** Create parse parameters for re-parsing only a part of an ABAP code document, in order to replace a part of an existing {@link Code} */
	public static ParseParams createForReprocessing(String sourceName, String codeText, String abapRelease, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode, Code surroundingCode) {
		return new ParseParams(sourceName, codeText, abapRelease, cleanupRange, cleanupRangeExpandMode, 1, 0, surroundingCode);
	}
	
	private ParseParams(String sourceName, String codeText, String abapRelease, CleanupRange cleanupRange, CleanupRangeExpandMode cleanupRangeExpandMode, int lineNumOffset, int surroundingTextOffset, Code surroundingCode) {
		this.sourceName = sourceName;
		this.codeText = codeText;
		this.abapRelease = abapRelease;
		this.cleanupRange = cleanupRange;
		this.cleanupRangeExpandMode = cleanupRangeExpandMode;
		this.lineNumOffset = lineNumOffset;
		this.surroundingTextOffset = surroundingTextOffset;
		this.surroundingCode = surroundingCode;
	}
}