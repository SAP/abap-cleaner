package com.sap.adt.abapcleaner.programbase;

public class JobDouble implements ICancelable {
	// values passed to the constructor
	private final int callsUntilCancellationPending;

	// values tracked during test
	private int callCountIsCancellationPending;

	/**
	 * 
	 * @param callsUntilCancellationPending - determines after how many calls to {@link #isCancellationPending()} the method shall return true (0 = never)
	 */
	public JobDouble(int callsUntilCancellationPending) {
		this.callsUntilCancellationPending = callsUntilCancellationPending;
	}

	@Override
	public boolean isCancellationPending(boolean wasMainStepCompleted) { 
		if (wasMainStepCompleted) {
			++callCountIsCancellationPending;
			return (callsUntilCancellationPending == callCountIsCancellationPending);
		} else {
			return false;
		}
	}

	int getCallCountIsCancellationPending() {
		return callCountIsCancellationPending;
	}
}
