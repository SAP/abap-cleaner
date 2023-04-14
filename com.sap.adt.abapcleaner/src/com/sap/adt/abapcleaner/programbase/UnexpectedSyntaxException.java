package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.Rule;

/**
 * Is thrown when a {@link Rule} detects an unexpected syntax in the ABAP {@link Code} 
 * at a point within in call chain that is unaware of whether changes have already been made.
 * Must be caught and rethrown as either {@link UnexpectedSyntaxBeforeChanges} or {@link UnexpectedSyntaxAfterChanges}. 
 */
public class UnexpectedSyntaxException extends ExceptionBase {
	private static final long serialVersionUID = 1L;

	public final Token token;
   public final Command command;

   public UnexpectedSyntaxException(Token token, String message) {
      super(ExceptionSeverity.S0_STOP_COMMAND, token.getSourceName(), token.sourceLineNum, message);
      this.token = token;
      command = this.token.getParentCommand();
   }

   public UnexpectedSyntaxException(Command command, String message) {
   	super(ExceptionSeverity.S0_STOP_COMMAND, command.getSourceName(), command.getSourceLineNumStart(), message);
      this.token = null;
      this.command = command;
   }

   public UnexpectedSyntaxException(String message) {
   	super(ExceptionSeverity.S0_STOP_COMMAND, "", 0, message);
      this.token = null;
      this.command = null;
   }

}