package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.programbase.IBatchJob;
import com.sap.adt.abapcleaner.programbase.Task;

public class CleanupBatchJob implements IBatchJob {
	private final CleanupParams cleanupParams;
	private final CleanupMetrics batchCleanupMetrics;

	public CleanupBatchJob(CleanupParams cleanupParams) {
		this.cleanupParams = cleanupParams;
		this.batchCleanupMetrics = new CleanupMetrics(cleanupParams);
	}
	
	@Override
	public String getDescription() { 
		return (cleanupParams.executeCleanup() ? "Test Parser, Cleaner and Comparer" : "Test Parser" ) + ", applying all rules to all code files in folder"; 
	}
	
	@Override
	public String getTitle(String codeFileInfo) { 
		if (cleanupParams.executeCleanup())
			return "Parser, Cleaner (" + String.valueOf(cleanupParams.getRules().length) + " rules), and Comparer result for " + codeFileInfo;
		else
			return "Parser result for " + codeFileInfo;
	 }


	@Override
	public CleanupParams getCleanupParams() { 
		return cleanupParams; 
	}

	@Override
	public void initialize() {
	}

	@Override
	public void addTaskResult(String sourceCode, String sourceName, Task task) {
		batchCleanupMetrics.buildFromTaskResult(sourceCode, sourceName, task);
	}

	@Override
	public void finish(int duration_ms) {
		batchCleanupMetrics.buildFinish(duration_ms);
	}

	@Override
	public String getSummary() { 
		return batchCleanupMetrics.getSummary(); 
	}

	@Override
	public String getDetails() { 
		return batchCleanupMetrics.getDetails(); 
	}
}
