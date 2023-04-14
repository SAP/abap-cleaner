package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.sap.adt.abapcleaner.programbase.IProgress;
import com.sap.adt.abapcleaner.programbase.TaskType;

public class ProgressDouble implements IProgress {
	// values passed to the constructor
	private final int callsUntilCancellationPending;
	private final TaskType expTaskType;

	// values tracked during test
	private int callCountIsCancellationPending;
	private boolean wasReportCalled;
	
	/**
	 * 
	 * @param callsUntilCancellationPending - determines after how many calls to {@link #isCancellationPending()} the method shall return true (0 = never)
	 * @param expTaskType - the TaskType expected in calls to {@link #report(TaskType, double)}
	 */
	ProgressDouble(int callsUntilCancellationPending, TaskType expTaskType) {
		this.callsUntilCancellationPending = callsUntilCancellationPending;
		this.expTaskType = expTaskType;
	}
	
	@Override
	public void report(TaskType task, double progressRatio) {
		wasReportCalled = true;
		if (expTaskType != TaskType.NONE) {
			assertEquals(expTaskType, task);
		}
		assertTrue(progressRatio >= 0.0);
		assertTrue(progressRatio <= 100.0);
	}

	@Override
	public boolean isCancellationPending() { 
		++callCountIsCancellationPending;
		return (callsUntilCancellationPending == callCountIsCancellationPending);
	}

	boolean wasReportCalled() {
		return wasReportCalled;
	}

	int getCallCountIsCancellationPending() {
		return callCountIsCancellationPending;
	}
}
