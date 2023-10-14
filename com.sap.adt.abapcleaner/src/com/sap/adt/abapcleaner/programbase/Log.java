package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;

public class Log {
	public final String path;

	private int entryCount;
	private int[] countOfSeverities = new int[ExceptionBase.SEVERITY_COUNT];

	private final StringBuilder text = new StringBuilder();

	static Log create(String path) {
		return new Log(path);
	}

	static Log createNonPersistentForTesting() {
		return new Log(null);
	}

	private Log(String path) {
		this.path = path;
	}

	final void add(ExceptionBase ex, String addMessage) {
		// do NOT log unexpected syntax before changes, because this may simply be caused by a comment in an unsupported place,  
		// e.g. at the beginning of a Term
		if (ex instanceof UnexpectedSyntaxBeforeChanges)
			return;
		
		text.append(Cult.getReverseDateTime(ex.raiseTime, true));
		text.append(" " + ex.sourceName);
		// the Rule name is already provided in ex.getMessage()
		text.append(", line " + Cult.format(ex.sourceLineNum));
		text.append(": " + ex.getMessage());
		if (!StringUtil.isNullOrEmpty(addMessage))
			text.append(addMessage);
		text.append(System.lineSeparator());
		++countOfSeverities[ex.severity.getValue()];
		++entryCount;
	}

	final String getSummary(boolean wereMultipleFilesProcessed) {
		if (entryCount == 0)
			return null;

		StringBuilder result = new StringBuilder();
		result.append(Cult.format(entryCount) + (entryCount == 1 ? " entry was " : " entries were ") + "added to the error log '" + path + "': ");
		if (entryCount > 1)
			result.append(System.lineSeparator());
		for (int i = 0; i < ExceptionBase.SEVERITY_COUNT; ++i) {
			if (countOfSeverities[i] == 0)
				continue;
			if (entryCount > 1)
				result.append(" - in " + Cult.format(countOfSeverities[i]) + (countOfSeverities[i] == 1 ? " case " : " cases "));
			String action;
			switch (ExceptionSeverity.forValue(i)) {
				case S0_STOP_COMMAND:
					action = "a command was skipped due to unexpected syntax";
					break;
				case S1_STOP_RULE:
					action = "the execution of a rule was stopped";
					break;
				case S2_STOP_TASK:
					action = (wereMultipleFilesProcessed ? "the cleanup was cancelled" : "the cleanup of a code file was cancelled");
					break;
				case S3_STOP_JOB:
					action = "the whole operation was cancelled";
					break;
				default:
					throw new IndexOutOfBoundsException();
			}
			result.append(action).append(System.lineSeparator());
		}
		return result.toString();
	}

	final void flush() {
		if (text.length() > 0) {
			if (path != null) { 
				Persistency.get().appendToFile(path, text.toString());
			}
			clear();
		}
	}

	private void clear() {
		text.setLength(0);
		entryCount = 0;
		for (int i = 0; i < ExceptionBase.SEVERITY_COUNT; ++i)
			countOfSeverities[i] = 0;
	}

	@Override
	public String toString() {
		return text.toString();
	}
}