package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Task;

public class CleanupResult {
	public final String cleanedCode;
	public final int startLine;
	public final int endLine;
	public final int offset;
	public final int length;
	public final String errorMessage;
	
	public int lineCountInCleanupRange;
	public int appliedRuleCount;
	public int changedLineCount;
	public int totalPocessingTime_ms;
	public String ruleStats;
	
	public boolean hasCleanedCode() { return (cleanedCode != null); }

	public String getCleanedCode() { return (cleanedCode == null) ? null : cleanedCode; }
	
	public boolean hasLineSelection() { return (startLine >= 0); }
	
	public boolean hasErrorMessage() { return !StringUtil.isNullOrEmpty(errorMessage); }
	
	public String getSelectedText() { return (length == 0) ? "" : cleanedCode.substring(offset, offset + length); }

	// statistics are only available if setStats() was called:
	public int getLineCountInCleanupRange() { return lineCountInCleanupRange; }
	public int getAppliedRuleCount() { return appliedRuleCount; }
	public int getChangedLineCount() { return changedLineCount; }
	public int getTotalProcessingTime_ms() { return totalPocessingTime_ms; }

	public String getRuleStats() { return ruleStats; }
	
	static CleanupResult createForRange(String cleanedCode, int startLine, int endLine, int offset, int length) {
		return new CleanupResult(cleanedCode, startLine, endLine, offset, length, null); 
	}
	
	static CleanupResult createWithoutRange(String cleanedCode) {
		return new CleanupResult(cleanedCode, -1, -1, 0, 0, null); 
	}

	public static CleanupResult createError(String errorMessage) {
		return new CleanupResult(null, -1, -1, 0, 0, errorMessage); 
	}
	
	private CleanupResult(String cleanedCode, int startLine, int endLine, int offset, int length, String errorMessage) {
		this.cleanedCode = cleanedCode;
		this.startLine = startLine;
		this.endLine = endLine;
		this.offset = offset;
		this.length = length;
		this.errorMessage = errorMessage;
	}

	public void setStats(Task result, String ruleStats) { 
		this.lineCountInCleanupRange = result.getLineCountInCleanupRange();
		this.appliedRuleCount = result.getAppliedRuleCount();
		this.changedLineCount = result.getChangedLineCount();
		this.totalPocessingTime_ms = result.getTotalProcessingTime_ms();
		
		this.ruleStats = ruleStats; 
	}

	public String getStatsSummary() {
		StringBuilder sb = new StringBuilder();
		sb.append((appliedRuleCount == 1) ? "1 cleanup rule" : Cult.format(appliedRuleCount) + " cleanup rules");
		sb.append(" applied to " + Cult.format(lineCountInCleanupRange) + " code lines"); 
		sb.append(" in " + Cult.fromMillisec(totalPocessingTime_ms) + ". ");
		sb.append((changedLineCount == 1) ? "1 line changed." : Cult.format(changedLineCount) + " lines changed.");
		
		return sb.toString();
	}
}
