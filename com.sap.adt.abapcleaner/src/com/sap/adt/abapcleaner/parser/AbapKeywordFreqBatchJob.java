package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.programbase.IBatchJob;
import com.sap.adt.abapcleaner.programbase.Task;
import com.sap.adt.abapcleaner.rulebase.CleanupParams;

public class AbapKeywordFreqBatchJob implements IBatchJob {
	private CodeMetrics codeMetrics; 
	private int parseExceptionCount;
	private int duration_ms;
	private boolean wasCancelled;
	
	public AbapKeywordFreqBatchJob() {
	}

	@Override
	public String getDescription() {
		return "Get ABAP keyword frequency for all text files in folder"; 
	}
	
	@Override
	public String getTitle(String codeFileInfo) {
		return "ABAP keyword frequency for " + codeFileInfo; 
	}

	@Override
	public StressTestParams getStressTestParams() { 
		return null; 
	}

	@Override
	public CleanupParams getCleanupParams() { 
		return null; 
	}

	@Override
	public void initialize() {
		codeMetrics = new CodeMetrics();
		parseExceptionCount = 0;
	}

	@Override
	public void addTaskResult(String sourceCode, String sourceName, Task task) {
		if (task.getParseSuccess()) {
			codeMetrics.addCode(sourceCode, sourceName, task.getResultingCode());
		} else {
			++parseExceptionCount;
		}
	}

	@Override
	public void finish(int duration_ms, boolean wasCancelled) {
		this.wasCancelled = wasCancelled;
		this.duration_ms = duration_ms;
	}

	@Override
	public String getSummary() { 
		return codeMetrics.getSummary(duration_ms, parseExceptionCount, wasCancelled); 
	}

	@Override
	public String getDetails() { 
		return codeMetrics.getKeywordMetricsDetails(wasCancelled); 
	}

}
