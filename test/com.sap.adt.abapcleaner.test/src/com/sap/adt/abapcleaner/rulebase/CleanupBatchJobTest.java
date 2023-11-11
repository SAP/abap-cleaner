package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.parser.StressTestParams;
import com.sap.adt.abapcleaner.parser.StressTestType;
import com.sap.adt.abapcleaner.programbase.JobDouble;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Task;

public class CleanupBatchJobTest {
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	private CleanupParams cleanupParams;
	private StressTestParams stressTestParams;
	private CleanupBatchJob batchJob;

	@BeforeEach
	void setup() {
		Program.setLogForTesting();
	}

	void createJob(boolean includeCleanup) {
		Profile profile = Profile.createDefault();
		cleanupParams = includeCleanup ? CleanupParams.createForProfile(profile, true) : CleanupParams.createForParseOnly();
		batchJob = new CleanupBatchJob(cleanupParams);
		batchJob.initialize();
	}
	
	void createJobWithStressTest() {
		Profile profile = Profile.createDefault();
		cleanupParams = CleanupParams.createForProfile(profile, true);
		stressTestParams = StressTestParams.create(0, 3, StressTestType.getAll());
		assertEquals(16, stressTestParams.getCount());
		batchJob = new CleanupBatchJob(cleanupParams, stressTestParams);
		batchJob.initialize();
	}
	
	@Test
	void testInfoWithCleanupAndStressTest() {
		createJobWithStressTest();
		assertTrue(batchJob.getDescription().length() > 0);
		assertTrue(batchJob.getTitle("info").length() > 0);
		assertEquals(cleanupParams, batchJob.getCleanupParams());
		assertEquals(stressTestParams, batchJob.getStressTestParams());
		assertEquals(null, batchJob.getSummary());
		assertEquals(null, batchJob.getDetails());
	}
	
	@Test
	void testInfoWithCleanup() {
		createJob(true);
		assertTrue(batchJob.getDescription().length() > 0);
		assertTrue(batchJob.getTitle("info").length() > 0);
		assertEquals(cleanupParams, batchJob.getCleanupParams());
		assertEquals(null, batchJob.getSummary());
		assertEquals(null, batchJob.getDetails());
	}
	
	@Test
	void testInfoWithoutCleanup() {
		createJob(false);
		assertTrue(batchJob.getDescription().length() > 0);
		assertTrue(batchJob.getTitle("info").length() > 0);
		assertEquals(cleanupParams, batchJob.getCleanupParams());
		assertEquals(null, batchJob.getSummary());
		assertEquals(null, batchJob.getDetails());
	}
	
	void addSource(String sourceName, String code) {
		String sourceCode = "method any_method." + LINE_SEP + code + LINE_SEP + "endmethod.";
		JobDouble jobDouble = new JobDouble(0); 
		ParseParams parseParams = ParseParams.createForWholeCode(sourceName, sourceCode, ABAP.NEWEST_RELEASE); 
		Task task = Task.createForBatch(jobDouble, parseParams, 0, 2);
		task.run(null, batchJob.getCleanupParams(), true);

		batchJob.addTaskResult(sourceCode, sourceName, task);
	}
	
	private void assertHasSummaryAndDetails(String... sourceNames) {
		batchJob.finish(1, false);

		String summary = batchJob.getSummary();
		String details = batchJob.getDetails();

		// expect summary and details to contain text
		assertTrue(summary.length() > 0);
		assertTrue(details.length() > 0);

		// expect the sourceNames to be mentioned in the details
		for (String sourceName : sourceNames) {
			assertTrue(details.indexOf(sourceName) >= 0);
		}

		// try adding another source and expect results to be unchanged
		addSource("sourceAfterFinish", "* comment");
		assertEquals(summary, batchJob.getSummary());
		assertEquals(details, batchJob.getDetails());
		assertTrue(details.indexOf("sourceAfterFinish") < 0);
	}

	@Test
	void testParseSuccess() {
		// create a job for parsing only
		createJob(false);
		
		addSource("source1", "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		addSource("source2", "clear ev_value." + LINE_SEP + "* comment" + LINE_SEP + "ev_value = get_value( ).");

		assertHasSummaryAndDetails("source1", "source2");
	}
	
	@Test
	void testCleanupSuccess() {
		// create a job that includes cleanup
		createJob(true);
		
		addSource("source1", "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		addSource("source2", "clear ev_value." + LINE_SEP + "* comment" + LINE_SEP + "ev_value = get_value( ).");

		assertHasSummaryAndDetails("source1", "source2");
	}
	
	@Test
	void testParseError() {
		// create a job for parsing only
		createJob(false);

		// add a source that provokes parse errors (opening parenthesis not closed; do ... endclass)
		addSource("source1", "do 5 times." + LINE_SEP + "a += 2 * ( 3 + ." + LINE_SEP + "endclass.");

		assertHasSummaryAndDetails("source1");
	}
	
	@Test
	void testCleanupError() {
		// create a job that includes cleanup
		createJob(true);
		
		// add a source that provokes parse errors (opening parenthesis not closed; do ... endclass)
		addSource("source1", "do 5 times." + LINE_SEP + "a += 2 * ( 3 + ." + LINE_SEP + "endclass.");

		assertHasSummaryAndDetails("source1");
	}
}
