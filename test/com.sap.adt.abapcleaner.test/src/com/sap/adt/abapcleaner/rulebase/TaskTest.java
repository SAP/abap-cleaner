package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.programbase.JobDouble;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Task;

public class TaskTest {
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	private CleanupParams cleanupParams;

	@BeforeEach
	void setup() {
		Program.setLogForTesting();
		Profile profile = Profile.createDefault();
		cleanupParams = CleanupParams.createForProfile(profile, true);
	}

	Task runTask(int callsUntilCancellationPending, String code) {
		String sourceCode = "method any_method." + LINE_SEP + code + LINE_SEP + "endmethod.";
		JobDouble jobDouble = new JobDouble(callsUntilCancellationPending); 
		ParseParams parseParams = ParseParams.createForWholeCode("anyName", sourceCode, ABAP.NEWEST_RELEASE); 
		Task task = Task.createForBatch(jobDouble, parseParams, 0, 2);
		task.run(cleanupParams, true);
		return task;
	}
	
	@Test
	void testCleanupSuccess() {
		Task task = runTask(0, "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		
		assertFalse(task.wasCancelled());
		assertNotNull(task.getResultingCode());
		assertNotNull(task.getResultingDiffDoc());
		assertEquals(null, task.getParseCheckErrorsInTestMode());
		
		assertEquals(5, task.getLineCountInCleanupRange());
		assertTrue(task.getAppliedRuleCount() >= 0);
		assertEquals(5, task.getChangedLineCount());

		assertEquals(null, task.getParseError());
		assertEquals(null, task.getCleanupError());
		assertEquals(null, task.getCompareError());
		assertEquals(null, task.getIntegrityTestError());
		assertEquals("", task.getErrorMessage());
		
		assertTrue(task.getParseTimeMs() >= 0);
		assertTrue(task.getCleanupTimeMs() >= 0);
		assertTrue(task.getCompareTimeMs() >= 0);
		assertTrue(task.getIntegrityTestTimeMs() >= 0);

		assertTrue(task.getParseSuccess());
		assertTrue(task.getCleanupSuccess(false));
		assertTrue(task.getCleanupSuccess(true));
		assertTrue(task.getCompareSuccess());
		assertTrue(task.getIntegrityTestSuccess());
		assertTrue(task.getSuccess());
		
		assertNull(task.getLogSummary());
		assertNull(task.getLogText());
		
		assertTrue(task.getCalculationTimeInfo().length() > 0);
	}

	@Test
	void testCancelAfterParse() {
		Task task = runTask(1, "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		
		assertTrue(task.wasCancelled());
		assertNotNull(task.getResultingCode());
		assertEquals(null, task.getResultingDiffDoc());
		
		assertEquals(0, task.getParseTimeMs());
		assertEquals(0, task.getCleanupTimeMs());
		assertEquals(0, task.getCompareTimeMs());
		assertEquals(0, task.getIntegrityTestTimeMs());

		assertFalse(task.getSuccess());
	}

	@Test
	void testCancelAfterCleanup() {
		Task task = runTask(2, "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		
		assertTrue(task.wasCancelled());
		assertNotNull(task.getResultingCode());
		assertEquals(null, task.getResultingDiffDoc());
		
		assertTrue(task.getParseTimeMs() >= 0);
		assertEquals(0, task.getCleanupTimeMs());
		// compare time includes retrieval of 'oldCodeDisplayLines' and is therefore already used after parsing
		assertTrue(task.getCompareTimeMs() >= 0);
		assertEquals(0, task.getIntegrityTestTimeMs());

		assertFalse(task.getSuccess());
	}

	@Test
	void testCancelAfterCompare() {
		Task task = runTask(3, "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		
		assertTrue(task.wasCancelled());
		assertNotNull(task.getResultingCode());
		assertNotNull(task.getResultingDiffDoc());
		
		assertTrue(task.getParseTimeMs() >= 0);
		assertTrue(task.getCleanupTimeMs() >= 0);
		// compare time includes retrieval of 'oldCodeDisplayLines' and is therefore already used after parsing
		assertTrue(task.getCompareTimeMs() >= 0);
		assertEquals(0, task.getIntegrityTestTimeMs());

		assertFalse(task.getSuccess());
	}

	@Test
	void testCancelAfterIntegrityTest() {
		Task task = runTask(4, "do 5 times." + LINE_SEP + "a += 1." + LINE_SEP + "enddo.");
		
		assertTrue(task.wasCancelled());
		assertNotNull(task.getResultingCode());
		assertNotNull(task.getResultingDiffDoc());
		
		assertTrue(task.getParseTimeMs() >= 0);
		assertTrue(task.getCleanupTimeMs() >= 0);
		assertTrue(task.getCompareTimeMs() >= 0);
		assertEquals(0, task.getIntegrityTestTimeMs());

		assertFalse(task.getSuccess());
	}
}
