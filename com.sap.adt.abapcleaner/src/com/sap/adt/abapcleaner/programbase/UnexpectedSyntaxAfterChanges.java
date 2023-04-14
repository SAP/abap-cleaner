package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.parser.*;

/** 
 * Is thrown when a {@link Rule} detects an unexpected syntax in the ABAP {@link Code} 
 * after changes have been made to the current {@link Command}.
 * Requires the whole {@link Task} to stop and discard all changes made to the {@link Code}. 
*/
public class UnexpectedSyntaxAfterChanges extends CleanException {
	private static final long serialVersionUID = 1L;

	public UnexpectedSyntaxAfterChanges(Rule rule, Token token, String message) {
      super(ExceptionSeverity.S2_STOP_TASK, rule, token, message);
   }
	public UnexpectedSyntaxAfterChanges(Rule rule, Command command, String message) {
      super(ExceptionSeverity.S2_STOP_TASK, rule, command, message);
   }
	public UnexpectedSyntaxAfterChanges(Rule rule, Code code, String message) {
      super(ExceptionSeverity.S2_STOP_TASK, rule, code, message);
   }
   public UnexpectedSyntaxAfterChanges(Rule rule, UnexpectedSyntaxException inner) {
      super(ExceptionSeverity.S2_STOP_TASK, rule, inner.command, inner.token, inner);
   }
}