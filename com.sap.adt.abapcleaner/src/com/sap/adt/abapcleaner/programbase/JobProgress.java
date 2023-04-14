package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;

public class JobProgress {
	private final String sourceName;
	private final TaskType task;
	private final double progressRatio;
	private final int bulkFileCount;
	private final int bulkFileNum;

	public final int getProgressPercentage() { return (int) (progressRatio * 100.0 + 0.5); }

	public final String getTitle() {
		if (StringUtil.isNullOrEmpty(sourceName))
			return "Progress";
		else if (bulkFileCount > 1)
			return "Processing " + Cult.format(bulkFileNum) + " / " + Cult.format(bulkFileCount) + ": " + sourceName;
		else
			return "Processing " + sourceName;
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
		this(fileName, task, progressRatio, 0, 0);
	}
	JobProgress(String fileName, TaskType task, double progressRatio, int bulkFileCount, int bulkFileNum) {
		sourceName = fileName;
		this.task = task;
		this.progressRatio = progressRatio;
		this.bulkFileCount = bulkFileCount;
		this.bulkFileNum = bulkFileNum;
	}
}