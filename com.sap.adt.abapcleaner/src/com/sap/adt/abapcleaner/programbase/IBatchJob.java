package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.parser.StressTestParams;
import com.sap.adt.abapcleaner.rulebase.CleanupParams;

public interface IBatchJob {
	String getDescription();
	String getTitle(String codeFileInfo);
	StressTestParams getStressTestParams();
	CleanupParams getCleanupParams();
	
	void initialize();
	void addTaskResult(String sourceCode, String sourceName, Task task);
	void finish(int duration_ms, boolean wasCancelled);
	
	String getSummary();
	String getDetails();
}
