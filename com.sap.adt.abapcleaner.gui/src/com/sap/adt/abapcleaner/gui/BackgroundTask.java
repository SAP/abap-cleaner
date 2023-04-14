package com.sap.adt.abapcleaner.gui;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;

class BackgroundTask extends Task {

   BackgroundTask(Job parentJob, ParseParams parseParams) {
      this(parentJob, parseParams, 0, 0);
   }
   BackgroundTask(Job parentJob, ParseParams parseParams, int batchIndex, int batchCount) {
      super(parentJob, parseParams, batchIndex, batchCount);
   }

   @Override
   protected void reportProgress(JobProgress progress) {
      ((BackgroundJob)parentJob).reportProgress(progress);
   }
}
