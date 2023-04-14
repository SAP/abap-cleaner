package com.sap.adt.abapcleaner.programbase;

/**
 * <p>Interface to be implemented by the UI to show the progress of a task that is currently being executed 
 * (parsing, comparing, cleaning, testing of referential integrity).</p> 
 */
public interface IProgress {
   void report(TaskType task, double progressRatio);
	boolean isCancellationPending();
}