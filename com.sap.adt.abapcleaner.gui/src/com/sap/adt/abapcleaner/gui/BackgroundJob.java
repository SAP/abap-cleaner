package com.sap.adt.abapcleaner.gui;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

class BackgroundJob extends Job {
	private JobProgress latestProgress;

	BackgroundJob(ParseParams parseParams, CleanupParams cleanupParams) {
      super(parseParams, cleanupParams);
   }
   BackgroundJob(IBatchJob batchJob, String batchDir, String[] batchPaths) {
      super(batchJob, batchDir, batchPaths);
   }

   @Override
   protected Task createTask(ParseParams parseParams) {
      return createTask(parseParams, 0, 0);
   }
   @Override
   protected Task createTask(ParseParams parseParams, int batchIndex, int batchCount) {
      return new BackgroundTask(this, parseParams, batchIndex, batchCount);
   }

   final void reportProgress(JobProgress progress) {
   	latestProgress = progress;
   }
   
   final JobProgress consumeLatestProgress() {
   	JobProgress result = latestProgress;
   	latestProgress = null;
   	return result;
   }
}
