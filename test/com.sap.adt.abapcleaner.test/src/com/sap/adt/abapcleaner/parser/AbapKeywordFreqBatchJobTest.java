package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.programbase.JobDouble;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Task;

public class AbapKeywordFreqBatchJobTest {

	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	private AbapKeywordFreqBatchJob batchJob;

	@BeforeEach
	void setup() {
		Program.setLogForTesting();
	}

	void createJob(boolean includeCleanup) {
		batchJob = new AbapKeywordFreqBatchJob();
		batchJob.initialize();
	}
	
	@Test
	void testInfo() {
		createJob(true);
		assertTrue(batchJob.getDescription().length() > 0);
		assertTrue(batchJob.getTitle("info").length() > 0);
		assertNull(batchJob.getStressTestParams());
		assertEquals(null, batchJob.getCleanupParams());
	}
	
	void addSource(String sourceName, String code) {
		String sourceCode = "method any_method." + LINE_SEP + code + LINE_SEP + "endmethod.";
		JobDouble jobDouble = new JobDouble(0); 
		ParseParams parseParams = ParseParams.createForWholeCode(sourceName, sourceCode, ABAP.NEWEST_RELEASE); 
		Task task = Task.createForBatch(jobDouble, parseParams, 0, 2);
		task.run(null, batchJob.getCleanupParams(), true);

		batchJob.addTaskResult(sourceCode, sourceName, task);
	}
	
	private void assertHasSummaryAndDetails(boolean expAnySourceName, String... sourceNames) {
		batchJob.finish(1, false);

		String summary = batchJob.getSummary();
		String details = batchJob.getDetails();

		// expect summary and details to contain text
		assertTrue(summary.length() > 0);
		assertTrue(details.length() > 0);

		if (expAnySourceName) {
			// expect at least one of the sourceNames to be mentioned in the details
			boolean foundAny = false;
			for (String sourceName : sourceNames) {
				if (details.indexOf(sourceName) >= 0)
					foundAny = true;
			}
			assertTrue(foundAny);
		}
	}

	@Test
	void testParseSuccess() {
		// create a job for parsing only
		createJob(false);
		
		addSource("source1", "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		addSource("source2", "clear ev_value." + LINE_SEP + "* comment" + LINE_SEP + "other_method( ).");

		assertHasSummaryAndDetails(true, "source1", "source2");
	}
	
	@Test
	void testParseError() {
		// create a job for parsing only
		createJob(false);

		// add a source that provokes parse errors (opening parenthesis not closed; do ... endclass)
		addSource("source1", "do 5 times." + LINE_SEP + "a += 2 * ( 3 + ." + LINE_SEP + "endclass.");

		assertHasSummaryAndDetails(false, "source1");
	}
}
