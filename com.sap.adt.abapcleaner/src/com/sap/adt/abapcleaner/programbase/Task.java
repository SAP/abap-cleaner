package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.comparer.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;
import java.util.*;

public class Task implements IProgress {
	protected final ICancelable parentJob;
	private final String sourceName;
	private final ParseParams parseParams;

	private final int batchIndex;
	private final int batchCount;
	private int stressTestIndex;
	private int stressTestCount;

	private TaskType lastReportedTask = TaskType.NONE;
	private int lastReportedPercentage = -1;

	private boolean wasCancelled;
	private Code resultingCode;
	private DiffDoc resultingDiffDoc;
	private String parseCheckErrorsInTestMode;

	private int lineCountInCleanupRange;
	private int appliedRuleCount;
	private int changedLineCount;

	private String parseError;
	private String cleanupError;
	private String compareError;
	private String checkError;

	private int parseTimeMs;
	private int cleanupTimeMs;
	private int compareTimeMs;
	private int checkTimeMs;
	private boolean success;

	public final boolean wasCancelled() { return wasCancelled; }

	public final Code getResultingCode() { return resultingCode; }

	public final DiffDoc getResultingDiffDoc() { return resultingDiffDoc; }

	public final String getParseCheckErrorsInTestMode() { return parseCheckErrorsInTestMode; }

	public final int getLineCountInCleanupRange() { return lineCountInCleanupRange; }
	
	public final int getAppliedRuleCount() { return appliedRuleCount; }

	public final int getChangedLineCount() { return changedLineCount; }
	
	public final String getParseError() { return parseError; }

	public final String getCleanupError() { return cleanupError; }

	public final String getCheckError() { return checkError; }

	public final String getCompareError() { return compareError; }

	private String logSummary; // may contain text even with success == true

	private String logText; 

	public final int getParseTimeMs() { return parseTimeMs; }

	public final int getCleanupTimeMs() { return cleanupTimeMs; }

	public final int getCompareTimeMs() { return compareTimeMs; }

	public final int getCheckTimeMs() { return checkTimeMs; }

	public final boolean getParseSuccess() { return (parseError == null); }

	public final boolean getCleanupSuccess(boolean allowWarnings) { return getParseSuccess() && (cleanupError == null) && (allowWarnings || StringUtil.isNullOrEmpty(logText)); }

	public final boolean getCheckSuccess() { return getCleanupSuccess(true) && (checkError == null); }

	public final boolean getCompareSuccess() { return getCheckSuccess() && (compareError == null); }

	public final boolean getSuccess() { return success; }
	
	public final String getLogSummary() { return logSummary; }

	public final String getLogText() { return logText; }

	public final String getCalculationTimeInfo() {
		return "parse: " + Cult.fromMillisec(parseTimeMs) 
			+ " + clean: " + Cult.fromMillisec(cleanupTimeMs) 
			+ ((checkTimeMs > 0) ? " + check: " + Cult.fromMillisec(checkTimeMs) : "")
			+ " + compare: " + Cult.fromMillisec(compareTimeMs) 
			+ " = " + Cult.fromMillisec(getTotalProcessingTime_ms());
	}

	public final int getTotalProcessingTime_ms() {
		return parseTimeMs + cleanupTimeMs + checkTimeMs + compareTimeMs;
	}

	@Override
	public boolean isCancellationPending() { return parentJob.isCancellationPending(false); }

	// -------------------------------------------------------------------------
	
	public static Task create(ICancelable parentJob, ParseParams parseParams) {
		return new Task(parentJob, parseParams, 0, 0);
	}

	public static Task createForBatch(ICancelable parentJob, ParseParams parseParams, int batchIndex, int batchCount) {
		return new Task(parentJob, parseParams, batchIndex, batchCount);
	}

	protected Task(ICancelable parentJob, ParseParams parseParams, int batchIndex, int batchCount) {
		this.parentJob = parentJob;
		this.sourceName = parseParams.sourceName;
		this.parseParams = parseParams;
		this.batchIndex = batchIndex;
		this.batchCount = batchCount;
		this.stressTestIndex = 0;
		this.stressTestCount = 0;
	}

	final void run(StressTestParams stressTestParams, CleanupParams cleanupParams) {
		run(stressTestParams, cleanupParams, false);
	}
	public final void run(StressTestParams stressTestParams, CleanupParams cleanupParams, boolean testMode) {
		if (stressTestParams == null || cleanupParams == null) {
			run(StressTestType.NONE, -1, cleanupParams, testMode);
	
		} else {
			// run a stress test by inserting line-end comments, comment lines, pragmas or chain colons after different Tokens in each Command
			stressTestCount = stressTestParams.getCount();
			stressTestIndex = 0;
			for (StressTestType stressTestType : stressTestParams.stressTestTypes) {
				for (int index = stressTestParams.insertAfterTokenIndexMin; index <= stressTestParams.insertAfterTokenIndexMax; ++index) {
					boolean continueStressTestType = run(stressTestType, index, cleanupParams, testMode);
					if (wasCancelled || !getCheckSuccess()) {
						return;
					}
					// if no stress test Tokens of the current StressTestType could be inserted at the current index, 
					// continue with the next StressTestType 
					if (!continueStressTestType) {
						break;
					}
					++stressTestIndex;
				}
			}
		}
	}
	
	public final boolean run(StressTestType stressTestType, int insertAfterTokenIndex, CleanupParams cleanupParams, boolean testMode) {
		success = false;

		lastReportedTask = TaskType.NONE;

		// parse
		Stopwatch stopwatch = Stopwatch.createAndStart();
		try {
			resultingCode = Code.parse(this, parseParams);
			lineCountInCleanupRange = resultingCode.getLineCountInCleanupRange();
		} catch (ParseException ex) {
			ex.addToLog();
			parseError = ex.getLineAndMessage(null);
			return false;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return false;
		}

		// test referential integrity to fail early, e.g. if a block is not closed
		try {
			resultingCode.testReferentialIntegrity(true);
			// .checkSyntax() can NOT be called here, because the RND Parser marks some Tokens as erroneous although 
			// they do not provoke a syntax error:
			// resultingCode.checkSyntax(false);
		} catch (IntegrityBrokenException ex) {
			ex.addToLog();
			parseError = ex.getLineAndMessage(null);
			return false;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return false;
		}
		parseTimeMs += stopwatch.getElapsedTimeMs();

		if (testMode)
			parseCheckErrorsInTestMode = resultingCode.compareWithSource(parseParams.codeText, 10); // null if recompiled code matches source text

		// parse only?
		if (cleanupParams == null || !cleanupParams.executeCleanup()) {
			success = true;
			return true;
		}

		// oldCodeDisplayLines must be retrieved now, before Rules are executed
		stopwatch.resetAndStart();
		ArrayList<DisplayLine> oldCodeDisplayLines = resultingCode.toDisplayLines(parseParams.lineNumOffset - 1);
		compareTimeMs += stopwatch.getElapsedTimeMs();

		// stress test: in each Command, insert a comment, pragma, or colon after the Token with the given index 
		String stressTestInfo = "";
		if (insertAfterTokenIndex >= 0) {
			stressTestInfo = " [stress test: inserted " + stressTestType.description + " after token #" + String.valueOf(insertAfterTokenIndex) + "]";
			try {
				if (!resultingCode.insertStressTestTokentAt(insertAfterTokenIndex, stressTestType)) {
					success = true;
					return false;
				}
			} catch (IntegrityBrokenException ex) {
				cleanupError = ex.getLineAndMessage(stressTestInfo);
				return false;
			}
		}
		
		// clean: execute active rules
		if (parseParams.surroundingCode != null)
			resultingCode.clearUsedRulesInCleanupRange(); // the ChangeControls are shared between code and codePart
		stopwatch.resetAndStart();
		try {
			if (cleanupParams.executeSingleRuleOnly()) {
				appliedRuleCount = 1;
				cleanupParams.rule.executeIfAllowedOn(resultingCode, cleanupParams.releaseRestriction);
			} else {
				appliedRuleCount = cleanupParams.profile.getActiveRuleCount();
				cleanupParams.profile.executeRules(resultingCode, cleanupParams.releaseRestriction, cleanupParams.executeAllRules, this);
			}
		} catch (CleanException ex) {
			Rule rule = cleanupParams.rule;
			if (rule != null)
				ex.enhanceIfMissing(rule, rule.commandForErrorMsg);
			ex.addToLog(stressTestInfo);
			if (ex.severity.getValue() > ExceptionSeverity.S1_STOP_RULE.getValue()) {
				cleanupError = ex.getLineAndMessage(stressTestInfo);
				return false;
			}
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return false;
		}
		cleanupTimeMs += stopwatch.getElapsedTimeMs();
		
		// check: test referential integrity and syntax
		stopwatch.resetAndStart();
		try {
			resultingCode.testReferentialIntegrity(true, this);
			if (stressTestType != StressTestType.COLON) {
				resultingCode.checkSyntax(true);
			}
		} catch (IntegrityBrokenException ex) {
			ex.addToLog(stressTestInfo);
			checkError = ex.getLineAndMessage(stressTestInfo);
			return false;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return false;
		}
		checkTimeMs += stopwatch.getElapsedTimeMs();

		// compare (not necessary during stress test, which focuses on cleanup and integrity)
		if (stressTestType == StressTestType.NONE) {
			stopwatch.resetAndStart();
			ArrayList<DisplayLine> newCodeDisplayLines = resultingCode.toDisplayLines(parseParams.lineNumOffset - 1);
			CompareDoc doc1 = CompareDoc.createFromDisplayLines(oldCodeDisplayLines);
			CompareDoc doc2 = CompareDoc.createFromDisplayLines(newCodeDisplayLines);
			try {
				resultingDiffDoc = doc1.compareTo(doc2, this);
				changedLineCount = resultingDiffDoc.getChangedLineCount(); 
			} catch (CompareException ex) {
				ex.addToLog(stressTestInfo);
				compareError = ex.getLineAndMessage(stressTestInfo);
				return false;
			}
			if (parentJob.isCancellationPending(true)) {
				wasCancelled = true;
				return false;
			}
			compareTimeMs += stopwatch.getElapsedTimeMs();
		}

		success = true;
		return true;
	}

	@Override
	public void report(TaskType task, double progressRatio) {
		int progressPercent = (int) (progressRatio * 100.0);
		if (task != lastReportedTask || progressPercent != lastReportedPercentage) {
			reportProgress(new JobProgress(sourceName, task, progressRatio, batchIndex, batchCount, stressTestIndex, stressTestCount));
			lastReportedTask = task;
			lastReportedPercentage = progressPercent;
		}
	}

	protected void reportProgress(JobProgress progress) {
	}
	
	void readAndFlushLog() {
		Log log = Program.getLog();
		logSummary = log.getSummary(false);
		logText = log.toString();
		log.flush();
	}

	public final String getErrorMessage() {
		String msg  = getParseError();
		if (msg == null) {
			msg = getCleanupError();
			if (msg == null) {
				msg = getCompareError();
				if (msg == null) {
					msg = getCheckError();
					if (msg == null) {
						msg = "";
					}
				}
			}
		}
		return msg + (StringUtil.isNullOrEmpty(logSummary) ? "" : System.lineSeparator() + System.lineSeparator() + logSummary);
	}

	/**
	 * 
	 * @param maxLength - maximum length in characters, or 0 for no length restriction
	 * @param lineSep - line separator
	 * @return
	 */
	public final String getLogText(int maxLength, String lineSep) {
		if (StringUtil.isNullOrEmpty(logText))
			return "";

		StringBuilder result = new StringBuilder();
		String[] lines = StringUtil.split(logText, System.lineSeparator(), true);
		int linesAdded = 0;

		for (String line : lines ) {
			if (result.length() > 0) {
				result.append(lineSep);
				if (maxLength > 0 && result.length() + line.length() > maxLength) {
					int linesMissing = lines.length - linesAdded;
					result.append("(+ " + Cult.format(linesMissing) + " further log lines)");
					break;
				}
			}
			result.append(line);
			++linesAdded;
		}
		return result.toString(); 
	}
}
