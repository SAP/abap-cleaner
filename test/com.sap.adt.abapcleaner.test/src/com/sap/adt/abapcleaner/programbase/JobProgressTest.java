package com.sap.adt.abapcleaner.programbase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

public class JobProgressTest {
	@Test 
	void testGetTitle() {
		JobProgress jobProgress = new JobProgress(null, TaskType.PARSER, 0.2);
		assertEquals("Progress", jobProgress.getTitle());
		
		jobProgress = new JobProgress("abc", TaskType.PARSER, 0.2);
		assertEquals("Processing abc", jobProgress.getTitle());

		jobProgress = new JobProgress("abc", TaskType.PARSER, 0.2, 0, 100, 42, 64);
		assertEquals("Processing 1 / 100: abc: 43 / 64", jobProgress.getTitle());
	}
	
	@Test
	void testGetPercentageOf() {
		JobProgress jobProgress = new JobProgress("abc", TaskType.PARSER, 0.1);
		assertEquals(10, jobProgress.getPercentageOf(TaskType.PARSER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.CLEANER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.COMPARER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.INTEGRITY_TEST));
		
		jobProgress = new JobProgress("abc", TaskType.CLEANER, 0.2);
		assertEquals(100, jobProgress.getPercentageOf(TaskType.PARSER));
		assertEquals(20, jobProgress.getPercentageOf(TaskType.CLEANER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.COMPARER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.INTEGRITY_TEST));

		jobProgress = new JobProgress("abc", TaskType.COMPARER, 0.3);
		assertEquals(100, jobProgress.getPercentageOf(TaskType.PARSER));
		assertEquals(100, jobProgress.getPercentageOf(TaskType.CLEANER));
		assertEquals(30, jobProgress.getPercentageOf(TaskType.COMPARER));
		assertEquals(0, jobProgress.getPercentageOf(TaskType.INTEGRITY_TEST));

		jobProgress = new JobProgress("abc", TaskType.INTEGRITY_TEST, 0.4);
		assertEquals(100, jobProgress.getPercentageOf(TaskType.PARSER));
		assertEquals(100, jobProgress.getPercentageOf(TaskType.CLEANER));
		assertEquals(100, jobProgress.getPercentageOf(TaskType.COMPARER));
		assertEquals(40, jobProgress.getPercentageOf(TaskType.INTEGRITY_TEST));
	}
}
