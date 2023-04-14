package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.parser.*;

/** 
 * Is thrown when a {@link Rule} detects an unexpected syntax in the ABAP {@link Code} 
 * before changes have been made to the current {@link Command}.
 * Allows to proceed with the next {@link Command}.
*/
public class UnexpectedSyntaxBeforeChanges extends CleanException {
	private static final long serialVersionUID = 1L;

	public UnexpectedSyntaxBeforeChanges(Rule rule, Token token, String message) {
      super(ExceptionSeverity.S0_STOP_COMMAND, rule, token, message);
   }
   public UnexpectedSyntaxBeforeChanges(Rule rule, UnexpectedSyntaxException inner) {
      super(ExceptionSeverity.S0_STOP_COMMAND, rule, inner.command, inner.token, inner);
   }
}