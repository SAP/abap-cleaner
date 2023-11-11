package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.CodeMetrics;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.programbase.Task;

class CleanupMetrics {
	private final String lineSep = System.lineSeparator();

	// data provided to the constructor
	private final CleanupParams cleanupParams;
	
	// textual results  
	private final StringBuilder summary = new StringBuilder();
	private final StringBuilder details = new StringBuilder();
	
	// statistics updated in .addFile() and evaluated in .finish()
	private final CodeMetrics codeMetrics = new CodeMetrics();

	private int parseSuccessCount;
	private int parseExceptionCount;
	private int parseErrorCount;

	private int cleanupSuccessCount;
	private int cleanupExceptionCount;
	private int cleanupWarningCount;

	private int checkSuccessCount;
	private int checkExceptionCount;

	private int compareSuccessCount;
	private int compareExceptionCount;

	private double parseSum_ms;
	private double cleanupSum_ms;
	private double checkSum_ms;
	private double compareSum_ms;

	private int[] totalRuleUseCount = new int[Rule.RULE_COUNT];

	// results can only be retrieved after .buildFinish() was called
	private boolean wasBuildFinished;

	// result retrieval
	String getSummary() { return wasBuildFinished ? summary.toString() : null; }
	String getDetails() { return wasBuildFinished ? details.toString() : null; }

	
	public CleanupMetrics(CleanupParams cleanupParams) {
		this.cleanupParams = cleanupParams;
		
		appendHeaderLine();
	}
	
	private void appendHeaderLine() {
		// header categories
		details.append("\tCode metrics");
		details.append("\t\t\t");
		details.append("\tProcessing time and result\t");
		if (cleanupParams.executeCleanup()) {
			details.append("\t\t");
			details.append("\t\t");
		
			details.append("\t");
			RuleGroupID lastRuleGroupID = RuleGroupID.ALIGNMENT; // any value
			for (int i = 0; i < Rule.RULE_COUNT; ++i) {
				Rule rule = cleanupParams.profile.getRule(RuleID.forValue(i));
				details.append("\t");
				if (i == 0 || rule.getGroupID() != lastRuleGroupID) {
					details.append(rule.getGroupID().toString());
					lastRuleGroupID = rule.getGroupID();
				}
			}
		}
		details.append(lineSep);

		// main header
		details.append("File\tCharCount");
		details.append("\tTokenCount\tLineCount\tCommandCount");
		details.append("\tParse duration (ms)\tParse result");
		if (cleanupParams.executeCleanup()) {
			details.append("\tCleanup duration (ms)\tCleanup result");
			details.append("\tCheck duration (ms)\tCheck result");
			details.append("\tCompare duration (ms)\tCompare result");

			details.append("\t");
			for (int i = 0; i < Rule.RULE_COUNT; ++i) {
				details.append("\t").append(cleanupParams.profile.getRule(RuleID.forValue(i)).getDisplayName());
			}
		}
		details.append(lineSep);
	}
	
	void buildFromTaskResult(String sourceCode, String sourceName, Task task) {
		if (wasBuildFinished)
			return;
		
		appendSourceInfo(sourceName, sourceCode.length());

		Code code = task.getResultingCode();

		// parser result
		if (task.getParseSuccess()) {
			int tokenCount = code.getTotalTokenCount();
			String parseErrors = task.getParseCheckErrorsInTestMode(); // null if recompiled code matches source text
			int parse_ms = task.getParseTimeMs();
			int lineCount = StringUtil.instrCount(sourceCode, '\n');
			
			codeMetrics.addFile(lineCount, code.commandCount, tokenCount, sourceCode.length());
			parseSum_ms += parse_ms;

			appendParseResult(tokenCount, lineCount, code.commandCount);
			appendDurationAndMessage((int) parse_ms, (parseErrors != null) ? parseErrors : "OK");
			if (parseErrors == null)
				++parseSuccessCount;
			else
				++parseErrorCount;

		} else {
			appendParseResult(0, 0, 0);
			appendDurationAndMessage(-1, task.getParseError());
			++parseExceptionCount;
		}
		
		if (cleanupParams.executeCleanup()) {
			// cleanup result
			if (task.getCleanupSuccess(false)) {
				int rules_ms = task.getCleanupTimeMs();
				cleanupSum_ms += rules_ms;
				appendDurationAndMessage(rules_ms, "OK");
				++cleanupSuccessCount;
			} else if (task.getCleanupError() != null) {
				appendDurationAndMessage(-1, task.getCleanupError());
				++cleanupExceptionCount;
			} else {
				appendDurationAndMessage(-1, task.getLogText(2000, " // "));
				++cleanupWarningCount;
			}
	
			// (integrity and syntax) check result
			if (task.getCheckSuccess()) {
				int check_ms = task.getCheckTimeMs();
				checkSum_ms += check_ms;
				appendDurationAndMessage(check_ms, "OK");
				++checkSuccessCount;
			} else {
				appendDurationAndMessage(-1, task.getCheckError());
				++checkExceptionCount;
			}
	
			// comparer result
			if (cleanupParams.executeCleanup() && task.getCompareSuccess()) {
				int compare_ms = task.getCompareTimeMs();
				compareSum_ms += compare_ms;
				appendDurationAndMessage(compare_ms, "OK");
				++compareSuccessCount;
			} else {
				appendDurationAndMessage(-1, task.getCompareError());
				++compareExceptionCount;
			}
	
			// rule use count
			if (cleanupParams.executeCleanup() && task.getCleanupSuccess(true)) {
				int[] ruleUseCount = new int[Rule.RULE_COUNT];
				int[] ruleBlockedCount = new int[Rule.RULE_COUNT];
				
				Command command = code.firstCommand;
				while (command != null) {
					command.getChangeControl().addToRuleStats(ruleUseCount, ruleBlockedCount);
					command = command.getNext();
				}
	
				appendRuleUseCount(ruleUseCount, "");
				for (int i = 0; i < Rule.RULE_COUNT; ++i) {
					totalRuleUseCount[i] += ruleUseCount[i];
				}
			}
		}
		details.append(lineSep);
	}
	
	void buildFinish(int duration_ms, boolean wasCancelled) {
		StringBuilder parseSummary = new StringBuilder();
		StringBuilder cleanupSummary = new StringBuilder();
		StringBuilder checkSummary = new StringBuilder();
		StringBuilder compareSummary = new StringBuilder();

		// parse result
		if (parseExceptionCount == 0 && parseErrorCount == 0) {
			parseSummary.append("OK!");
		} else {
			parseSummary.append(Cult.format(parseExceptionCount) + " exceptions");
			parseSummary.append(", " + Cult.format(parseErrorCount) + " parse errors");
			parseSummary.append(", " + Cult.format(parseSuccessCount) + " OK.");
		}

		if (cleanupParams.executeCleanup()) {
			// cleanup result
			if (cleanupExceptionCount == 0 && cleanupWarningCount == 0) {
				cleanupSummary.append("OK!");
			} else {
				if (cleanupExceptionCount > 0)
					cleanupSummary.append(Cult.format(cleanupExceptionCount) + " exceptions, ");
				if (cleanupWarningCount > 0)
					cleanupSummary.append(Cult.format(cleanupWarningCount) + " warnings, ");
				cleanupSummary.append(Cult.format(cleanupSuccessCount) + " OK.");
			}
			if (cleanupParams.executeAllRules) {
				cleanupSummary.append(" (all rules");
			} else {
				cleanupSummary.append(" (" + Cult.format(cleanupParams.profile.getActiveRuleCount()) + " rules");
			}
			cleanupSummary.append(" with profile '" + cleanupParams.getProfileName() + "')");
			
			// check result
			if (checkExceptionCount == 0) {
				checkSummary.append("OK!");
			} else {
				checkSummary.append(Cult.format(checkExceptionCount) + " exceptions");
				checkSummary.append(", " + Cult.format(checkSuccessCount) + " OK.");
			}
	
			// compare result
			if (compareExceptionCount == 0) {
				compareSummary.append("OK!");
			} else {
				compareSummary.append(Cult.format(compareExceptionCount) + " exceptions");
				compareSummary.append(", " + Cult.format(compareSuccessCount) + " OK.");
			}
		}
			
		summary.append(codeMetrics.getSummary(duration_ms, 0, wasCancelled));

		// add summary of three steps to output
		summary.append(lineSep);
		summary.append("Parser: ").append(parseSummary.toString()).append(lineSep);
		if (cleanupParams.executeCleanup()) {
			summary.append("Cleaner: ").append(cleanupSummary.toString()).append(lineSep);
			summary.append("Checker: ").append(checkSummary.toString()).append(lineSep);
			summary.append("Comparer: ").append(compareSummary.toString()).append(lineSep);
		}
		
		appendSumLine(parseSummary.toString(), cleanupSummary.toString(), checkSummary.toString(), compareSummary.toString());
		
		wasBuildFinished = true;
	}

	private void appendSumLine(String parseSummary, String cleanupSummary, String checkSummary, String compareSummary) {
		// total code metrics
		appendSourceInfo("Sum", codeMetrics.getByteSum());
		
		appendParseResult(codeMetrics.getTokenSum(), codeMetrics.getLineSum(), codeMetrics.getCommandSum());
		appendDurationAndMessage((int) parseSum_ms, " " + parseSummary);
		if (cleanupParams.executeCleanup()) {
			appendDurationAndMessage((int) cleanupSum_ms, " " + cleanupSummary);
			appendDurationAndMessage((int) checkSum_ms, " " + checkSummary);
			appendDurationAndMessage((int) compareSum_ms, " " + compareSummary);

			appendRuleUseCount(totalRuleUseCount, "inactive");
		}

		details.append(lineSep).append(lineSep).append(summary.toString()).append(lineSep);
	}

	private void appendSourceInfo(String sourceName, int sourceCodeLength) {
		details.append(sourceName);
		details.append("\t").append(sourceCodeLength);
	}
		
	private void appendParseResult(int tokenCount, int lineCount, int commandCount) {
		details.append("\t").append(tokenCount);
		details.append("\t").append(lineCount);
		details.append("\t").append(commandCount);
	}
	
	private void appendDurationAndMessage(int duration_ms, String message) {
		details.append("\t").append(duration_ms < 0 ? " " : (int) duration_ms);
		details.append("\t").append((message == null) ? "" : message);
	}
	
	private void appendRuleUseCount(int[] ruleUseCount, String inactiveText) {
		details.append("\t");
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			Rule rule = cleanupParams.profile.getRule(RuleID.forValue(i));
			String ruleUseText = (rule.isActive || cleanupParams.executeAllRules) ? String.valueOf(ruleUseCount[i]) : inactiveText;
			details.append("\t").append(ruleUseText);
		}
	}
}
