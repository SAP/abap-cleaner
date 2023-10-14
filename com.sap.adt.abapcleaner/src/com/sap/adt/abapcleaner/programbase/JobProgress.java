package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;

public class JobProgress {
	private final String sourceName;
	private final TaskType task;
	private final double progressRatio;
	private final int bulkFileIndex;
	private final int bulkFileCount;
	private final int stressTestIndex;
	private final int stressTestCount;

	public final int getProgressPercentage() { return (int) (progressRatio * 100.0 + 0.5); }

	public final String getTitle() {
		if (StringUtil.isNullOrEmpty(sourceName))
			return "Progress";
		
		String bulkFileProgress = (bulkFileCount < 2) ? "" : Cult.format(bulkFileIndex + 1) + " / " + Cult.format(bulkFileCount) + ": "; 
		String stressTestProgress = (stressTestCount == 0) ? "" : ": " + Cult.format(stressTestIndex + 1) + " / " + Cult.format(stressTestCount);
		return "Processing " + bulkFileProgress + sourceName + stressTestProgress;
	}

	public final int getPercentageOf(TaskType task) {
		if (this.task.getValue() < task.getValue())
			return 0;
		else if (this.task == task)
			return getProgressPercentage();
		else
			return 100;
	}

	JobProgress(String fileName, TaskType task, double progressRatio) {
		this(fileName, task, progressRatio, 0, 0, 0, 0);
	}
	JobProgress(String fileName, TaskType task, double progressRatio, int bulkFileIndex, int bulkFileCount, int stressTestIndex, int stressTestCount) {
		sourceName = fileName;
		this.task = task;
		this.progressRatio = progressRatio;
		this.bulkFileIndex = bulkFileIndex;
		this.bulkFileCount = bulkFileCount;
		this.stressTestIndex = stressTestIndex;
		this.stressTestCount = stressTestCount;
	}
}