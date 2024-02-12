package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.rulebase.CleanupBatchJob;
import com.sap.adt.abapcleaner.rulebase.CleanupParams;
import com.sap.adt.abapcleaner.rulebase.Profile;

public class JobTest {
	private PersistencyDouble persistency;
	private String codeDir; 
	private String code1Path;
	private String code2Path;
	private String codeParseErrPath; 
	private String errorLogPath;
	private final String code1File = "code1.txt";
	private final String code2File = "code2.txt";
	private final String codeParseErrFile = "codeParseErr.txt";
	private final String code1Text = "CLASS any_class IMPLEMENTATION. ENDCLASS.";
	private final String code2Text = "CLASS other_class IMPLEMENTATION. ENDCLASS.";
	private final String codeParseErrText = "METHOD any_method. DO 5 TIMES. ENDLOOP. ENDCLASS.";

	@BeforeEach
	void setup() {
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");

		String workDir = persistency.getWorkDir();
		String userDir = persistency.prepareDirectory(workDir, "user");
		codeDir = persistency.prepareDirectory(userDir, "code");
		code1Path = persistency.prepareFile(codeDir, code1File, code1Text);
		code2Path = persistency.prepareFile(codeDir, code2File, code2Text);
		codeParseErrPath = persistency.prepareFile(codeDir, codeParseErrFile, codeParseErrText);
		
		errorLogPath = persistency.getSavePath(FileType.ERROR_LOG);
	}

	@Test
	void testCleanupBatch() {
		Profile profile = Profile.createDefault();
		IBatchJob batchJob = new CleanupBatchJob(CleanupParams.createForProfile(profile, false, ABAP.NO_RELEASE_RESTRICTION));
		Job job = Job.createForBatch(batchJob, codeDir, new String[] { code1Path, code2Path });

		assertEquals(0.0, job.getInitialProgress().getProgressPercentage());
		
		job.run();

		assertTrue(job.isDone());
		assertFalse(job.wasCancelled());

		// expect the job summary to report that all processes worked 
		String summary = job.getBatchSummary();
		assertTrue(StringUtil.contains(summary, "Processed 2 files with 12 tokens in 4 commands"));
		assertTrue(StringUtil.contains(summary, "Parser: OK"));
		assertTrue(StringUtil.contains(summary, "Cleaner: OK"));
		assertTrue(StringUtil.contains(summary, "Comparer: OK"));

		// expect batch details to mention both code files
		String batchDetails = job.getBatchDetails();
		assertFalse(StringUtil.isNullOrEmpty(batchDetails));
		assertTrue(StringUtil.contains(batchDetails, persistency.getFileNameWithoutExtension(code1File)));
		assertTrue(StringUtil.contains(batchDetails, persistency.getFileNameWithoutExtension(code2File)));
		
		// expect getResult() to be null, because multiple files were processed 
		assertNull(job.getResult());
		// expect getCodeTextLength() to be null because no ParseParams were determined 
		assertEquals(0, job.getCodeTextLength());
	}

	@Test
	void testCleanupBatchCancelled() {
		Profile profile = Profile.createDefault();
		IBatchJob batchJob = new CleanupBatchJob(CleanupParams.createForProfile(profile, false, ABAP.NO_RELEASE_RESTRICTION));
		Job job = Job.createForBatch(batchJob, codeDir, new String[] { code1Path, code2Path });

		job.cancel();
		job.run();
	
		assertTrue(job.wasCancelled());
	}

	@Test
	void testCleanupBatchWithParseError() {
		Profile profile = Profile.createDefault();
		IBatchJob batchJob = new CleanupBatchJob(CleanupParams.createForProfile(profile, false, ABAP.NO_RELEASE_RESTRICTION));
		Job job = Job.createForBatch(batchJob, codeDir, new String[] { code1Path, code2Path, codeParseErrPath });

		job.run();
	
		assertTrue(job.isDone());
		assertFalse(job.wasCancelled());

		// ensure that the batch summary reports the parse error
		String summary = job.getBatchSummary();
		assertTrue(StringUtil.contains(summary, "Processed 2 files with 12 tokens in 4 commands"));
		assertTrue(StringUtil.contains(summary, "Parser: 1 exceptions"));
		assertTrue(StringUtil.contains(summary, "Cleaner: 1 warnings, 2 OK"));
		assertTrue(StringUtil.contains(summary, "Comparer: 1 exceptions, 2 OK"));

		// expect batch details to mention all three code files and to report the parse error
		String batchDetails = job.getBatchDetails();
		assertFalse(StringUtil.isNullOrEmpty(batchDetails));
		assertTrue(StringUtil.contains(batchDetails, persistency.getFileNameWithoutExtension(code1File)));
		assertTrue(StringUtil.contains(batchDetails, persistency.getFileNameWithoutExtension(code2File)));
		assertTrue(StringUtil.contains(batchDetails, persistency.getFileNameWithoutExtension(codeParseErrFile)));
		assertTrue(StringUtil.contains(batchDetails, "Parse error in line 1: expected ENDDO, but found ENDLOOP."));
		assertTrue(StringUtil.contains(batchDetails, "Opening command (line 1): DO 5 TIMES."));

		// ensure that the log file reports the parse error
		assertTrue(persistency.fileExists(errorLogPath));
		String errorLog = persistency.readAllTextFromFile(errorLogPath);
		assertTrue(StringUtil.contains(errorLog, "Parse error in line 1: expected ENDDO, but found ENDLOOP."));
		assertTrue(StringUtil.contains(errorLog, "Opening command (line 1): DO 5 TIMES."));
	}


	/*
	// start the job in a separate thread until it is done or cancelled 
	new Thread() {
		@Override
		public void run() {
			job.run();
		}
	}.start();
	 */
}
