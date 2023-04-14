package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.parser.*;

/**
 * <p>Represents an exception that is thrown while cleaning {@link Code} with a {@link Rule}.</p>
 */
public abstract class CleanException extends ExceptionBase {
	/* 
	 * // currently not needed:
	 * private Rule rule;
	 * private Command command;
	 * private Token token;
	 * 
	 * Rule getRule() { return rule; }
	 * Command getCommand() { return command; }
	 * Token getToken() { return token; }
	 */

	private static final long serialVersionUID = 1L;

	protected CleanException(ExceptionSeverity severity, Rule rule, Command command, Token token, ExceptionBase inner) {
		super(severity, (token != null) ? token.getSourceName() : ((command == null) ? null : command.getSourceName()),
				(token != null) ? token.sourceLineNum : ((command == null) ? 0 : command.getSourceLineNumStart()),
				(rule == null ? inner.getMessage() : "Error executing rule '" + rule.getDisplayName() + "'" + (StringUtil.isNullOrEmpty(inner.getMessage()) ? "!" : ": " + inner.getMessage())));
		// this.rule = rule;
		// this.command = command;
		// this.token = token;
	}

	protected CleanException(ExceptionSeverity severity, Rule rule, Token token, String message) {
		super(severity, (token == null) ? null : token.getSourceName(), (token == null) ? 0 : token.sourceLineNum, 
				(rule == null ? message : "Error executing rule '" + rule.getDisplayName() + "': " + message));
		// this.rule = rule;
		// this.command = token.parentCommand;
		// this.token = token;
	}

	protected CleanException(ExceptionSeverity severity, Rule rule, Command command, String message) {
		super(severity, command.getSourceName(), command.getSourceLineNumStart(), 
				(rule == null ? message : "Error executing rule '" + rule.getDisplayName() + "': " + message));
		// this.rule = rule;
		// this.command = token.parentCommand;
		// this.token = null;
	}

	protected CleanException(ExceptionSeverity severity, Rule rule, Code code, String message) {
		super(severity, code.sourceName, 0, 
				(rule == null ? message : "Error executing rule '" + rule.getDisplayName() + "': " + message));
		// this.rule = rule;
		// this.command = token.parentCommand;
		// this.token = null;
	}

	public final void enhanceIfMissing(Rule rule, Command command) {
		// if (this.rule == null) 
		//    this.rule = rule;
		// if (this.command == null)
		//    this.command = command;
	}
}