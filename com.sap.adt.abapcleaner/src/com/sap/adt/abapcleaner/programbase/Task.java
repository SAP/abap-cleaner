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
	private String integrityTestError;

	private int parseTimeMs;
	private int cleanupTimeMs;
	private int compareTimeMs;
	private int integrityTestTimeMs;
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

	public final String getCompareError() { return compareError; }

	public final String getIntegrityTestError() { return integrityTestError; }

	private String logSummary; // may contain text even with success == true

	private String logText; 

	public final int getParseTimeMs() { return parseTimeMs; }

	public final int getCleanupTimeMs() { return cleanupTimeMs; }

	public final int getCompareTimeMs() { return compareTimeMs; }

	public final int getIntegrityTestTimeMs() { return integrityTestTimeMs; }

	public final boolean getParseSuccess() { return (parseError == null); }

	public final boolean getCleanupSuccess(boolean allowWarnings) { return getParseSuccess() && (cleanupError == null) && (allowWarnings || StringUtil.isNullOrEmpty(logText)); }

	public final boolean getCompareSuccess() { return getCleanupSuccess(true) && (compareError == null); }

	public final boolean getIntegrityTestSuccess() { return getCompareSuccess() && (integrityTestError == null); }

	public final boolean getSuccess() { return success; }
	
	public final String getLogSummary() { return logSummary; }

	public final String getLogText() { return logText; }

	public final String getCalculationTimeInfo() {
		return "parser: " + Cult.fromMillisec(parseTimeMs) + " + cleaner: " + Cult.fromMillisec(cleanupTimeMs) + " + comparer: " + Cult.fromMillisec(compareTimeMs)
				+ ((integrityTestTimeMs > 0) ? " + integrity test: " + Cult.fromMillisec(integrityTestTimeMs) : "") + " = "
				+ Cult.fromMillisec(getTotalProcessingTime_ms());
	}

	public final int getTotalProcessingTime_ms() {
		return parseTimeMs + cleanupTimeMs + compareTimeMs + integrityTestTimeMs;
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
	}

	final void run(CleanupParams cleanupParams) {
		run(cleanupParams, false);
	}
	public final void run(CleanupParams cleanupParams, boolean testMode) {
		success = false;

		lastReportedTask = TaskType.NONE;

		// parse
		Stopwatch stopwatch = Stopwatch.createAndStart();
		try {
			resultingCode = Code.parse(this, parseParams);
			resultingCode.testReferentialIntegrity(true); // fail early, e.g. if a block is not closed
			lineCountInCleanupRange = resultingCode.getLineCountInCleanupRange();
		} catch (ParseException | IntegrityBrokenException ex) {
			ex.addToLog();
			parseError = ex.getMessage();
			return;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return;
		}
		parseTimeMs = stopwatch.getElapsedTimeMs();

		if (testMode)
			parseCheckErrorsInTestMode = resultingCode.compareWithSource(parseParams.codeText, 10); // null if recompiled code matches source text

		// parse only?
		if (cleanupParams == null || !cleanupParams.executeCleanup()) {
			success = true;
			return;
		}

		// oldCodeDisplayLines must be retrieved now, before Rules are executed
		stopwatch.resetAndStart();
		ArrayList<DisplayLine> oldCodeDisplayLines = resultingCode.toDisplayLines(parseParams.lineNumOffset - 1);
		compareTimeMs = stopwatch.getElapsedTimeMs();

		// clean: execute active rules
		if (parseParams.surroundingCode != null)
			resultingCode.clearUsedRules(); // the ChangeControls are shared between code and codePart
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
			ex.addToLog();
			if (ex.severity.getValue() > ExceptionSeverity.S1_STOP_RULE.getValue()) {
				cleanupError = ex.getMessage();
				return;
			}
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return;
		}
		cleanupTimeMs = stopwatch.getElapsedTimeMs();

		// compare
		stopwatch.resetAndStart();
		ArrayList<DisplayLine> newCodeDisplayLines = resultingCode.toDisplayLines(parseParams.lineNumOffset - 1);
		CompareDoc doc1 = CompareDoc.createFromDisplayLines(oldCodeDisplayLines);
		CompareDoc doc2 = CompareDoc.createFromDisplayLines(newCodeDisplayLines);
		try {
			resultingDiffDoc = doc1.compareTo(doc2, this);
			changedLineCount = resultingDiffDoc.getChangedLineCount(); 
		} catch (CompareException ex) {
			ex.addToLog();
			compareError = ex.getMessage();
			return;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return;
		}
		compareTimeMs += stopwatch.getElapsedTimeMs();

		// test referential integrity
		stopwatch.resetAndStart();
		try {
			resultingCode.testReferentialIntegrity(true, this);
		} catch (IntegrityBrokenException ex) {
			ex.addToLog();
			integrityTestError = ex.getMessage();
			return;
		}
		if (parentJob.isCancellationPending(true)) {
			wasCancelled = true;
			return;
		}
		integrityTestTimeMs = stopwatch.getElapsedTimeMs();

		success = true;
	}

	@Override
	public void report(TaskType task, double progressRatio) {
		int progressPercent = (int) (progressRatio * 100.0);
		if (task != lastReportedTask || progressPercent != lastReportedPercentage) {
			reportProgress(new JobProgress(sourceName, task, progressRatio, batchCount, batchIndex));
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
					msg = getIntegrityTestError();
					if (msg == null)
						msg = "";
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
