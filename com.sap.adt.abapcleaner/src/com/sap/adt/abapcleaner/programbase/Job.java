package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class Job implements ICancelable {
	public static final int CODE_LENGTH_TO_SHOW_PROGRESS_FORM = 1024 * 1024;

	private boolean cancellationPending;

	// data provided to the constructor
	// - for single file processing only:
	private final ParseParams parseParams;
	// - for batch processing only:
	private final IBatchJob batchJob;
	private final String batchDir;
	private final String[] batchPaths;
	// - for both single file and batch processing:
	private final StressTestParams stressTestParams;
	private final CleanupParams cleanupParams;

	public final int getCodeTextLength() { return (parseParams == null) ? 0 : parseParams.codeText.length(); }

	// result data
	// - for single file processing:
	private Task result;
	// - for batch processing:
	private String batchSummary;
	private String batchDetails;
	// - for both single file and batch processing:
	private boolean wasCancelled;
	private boolean done;

	public final Task getResult() { return result; }

	public final String getBatchSummary() { return batchSummary; }

	public final String getBatchDetails() { return batchDetails; }

	public final boolean wasCancelled() { return wasCancelled; }

	@Override
	public final boolean isCancellationPending(boolean wasMainStepCompleted) { return cancellationPending; }

	public final boolean isDone() { return done; }
	
	public final JobProgress getInitialProgress() {
		String sourceName = (parseParams == null) ? "" : parseParams.sourceName;
		int bulkFileCount = (batchPaths == null) ? 0 : batchPaths.length;
		return new JobProgress(sourceName, TaskType.NONE, 0.0, 0, bulkFileCount, 0, 0);
	}

	public static Job createForSingleCodeDocument(ParseParams parseParams, CleanupParams cleanupParams) {
		return new Job(parseParams, cleanupParams);
	}

	public static Job createForRuleExample(String sourceName, String codeText, Rule rule) {
		return new Job(ParseParams.createForWholeCode(sourceName, codeText, ABAP.NEWEST_RELEASE), 
				         CleanupParams.createForRule(rule, ABAP.NO_RELEASE_RESTRICTION));
	}

	public static Job createForBatch(IBatchJob batchJob, String batchDir, String[] batchPaths) {
		return new Job(batchJob, batchDir, batchPaths);
	}

	/**
	 * Creates a Job for processing of a single code document
	 */
	protected Job(ParseParams parseParams, CleanupParams cleanupParams) {
		this.parseParams = parseParams;
		this.stressTestParams = null; 
		this.cleanupParams = cleanupParams;
		
		batchJob = null;
		batchDir = null;
		batchPaths = null;
	}

	/**
	 * Creates a test Job for batch-processing of all provided files
	 */
	protected Job(IBatchJob batchJob, String batchDir, String[] batchPaths) {
		this.parseParams = null;
		this.stressTestParams = batchJob.getStressTestParams();
		this.cleanupParams = batchJob.getCleanupParams();

		this.batchJob = batchJob;
		this.batchDir = batchDir;
		this.batchPaths = batchPaths;
	}

	public final void cancel() {
		cancellationPending = true;
	}

	public final Task run() {
		wasCancelled = false;
		done = false;
		if (batchPaths == null) {
			result = runSingleCodeDocument();
		} else {
			runBatchJob();
			result = null;
		}
		done = true;
		return result;
	}

	private Task runSingleCodeDocument() {
		Task task = createTask(parseParams);
		task.run(stressTestParams, cleanupParams);
		task.readAndFlushLog();
		wasCancelled = task.wasCancelled();
		return task;
	}

	protected Task createTask(ParseParams parseParams) {
		return Task.create(this, parseParams);
	}

	private void runBatchJob() {
		Persistency persistency = Persistency.get();
		final String extension = persistency.getExtension(FileType.CODE); // e.g. ".txt"

		Stopwatch stopwatch = Stopwatch.createAndStart();
		batchJob.initialize(); 

		Program.getLog().flush();

		for (int batchPathIndex = 0; batchPathIndex < batchPaths.length; ++batchPathIndex) {
			String path = batchPaths[batchPathIndex];
			String sourceCode = persistency.readAllTextFromFile(path);
			String sourceName = StringUtil.removeSuffix(path.substring(batchDir.length()), extension, true);

			Task task = createTask(ParseParams.createForWholeCode(sourceName, sourceCode, ABAP.NEWEST_RELEASE), batchPathIndex, batchPaths.length);
			task.run(batchJob.getStressTestParams(), batchJob.getCleanupParams(), true);
			task.readAndFlushLog();
			if (task.wasCancelled()) {
				wasCancelled = true; 
				return;
			} 
			batchJob.addTaskResult(sourceCode, sourceName, task);
		}

		batchJob.finish(stopwatch.getElapsedTimeMs()); 

		batchSummary = batchJob.getSummary(); 
		batchDetails = batchJob.getDetails();
	}

	protected Task createTask(ParseParams parseParams, int batchIndex, int batchCount) {
		return Task.createForBatch(this, parseParams, batchIndex, batchCount);
	}
}