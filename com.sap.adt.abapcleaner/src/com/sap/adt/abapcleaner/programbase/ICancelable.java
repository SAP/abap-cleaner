package com.sap.adt.abapcleaner.programbase;

public interface ICancelable {
	boolean isCancellationPending(boolean wasMainStepCompleted);
} 
