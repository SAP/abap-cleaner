package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.CompareDoc;
import com.sap.adt.abapcleaner.comparer.DiffDoc;

/**
 * <p>Represents an exception that is thrown while comparing two versions of a code document ({@link CompareDoc})
 * and creating a {@link DiffDoc}.</p>
 */
public class CompareException extends ExceptionBase {
	private static final long serialVersionUID = 1L;

	// public final int indexA;
	// public final int indexB;

	public CompareException(int indexA, int indexB, RuntimeException inner) {
		super(ExceptionSeverity.S3_STOP_JOB, null, indexA, "Error comparing lines " + Cult.format(indexA) + " and " + Cult.format(indexB)
				+ (StringUtil.isNullOrEmpty(inner.getMessage()) ? "!" : ": " + inner.getMessage()));
		// this.indexA = indexA;
		// this.indexB = indexB;
	}
}