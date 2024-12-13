package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;

public class ParseException extends ExceptionBase {
	private static final long serialVersionUID = 1L;

	public ParseException(Code code, int lineNum, ExceptionBase inner) {
		super(ExceptionSeverity.S2_STOP_TASK, code.sourceName, lineNum,
				"Parse error in line " + Cult.format(lineNum) + (StringUtil.isNullOrEmpty(inner.getMessage()) ? "!" : ": " + inner.getMessage()));
	}

	public ParseException(Code code, int lineNum, String message) {
		super(ExceptionSeverity.S2_STOP_TASK, code.sourceName, lineNum, "Parse error in line " + Cult.format(lineNum) + ": " + message);
	}

	public ParseException(Code code, String message, boolean skipLog) {
		super(ExceptionSeverity.S2_STOP_TASK, code.sourceName, 0, message, skipLog);
	}
}