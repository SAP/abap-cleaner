package com.sap.adt.abapcleaner.programbase;

import java.time.*;

/** 
 * <p>Base class for all exceptions to be caught and logged in the error {@link Log}, 
 * possibly after being enriched with the current {@link Rule} and rethrown.</p>
 * 
 * <p>Other Java exceptions like ArgumentException, ArgumentNullException, ArgumentOutOfRangeException, 
 * and InvalidOperationException are NOT caught, since they do result from wrong calls 
 * (independent of the specific code data) and are therefore programming errors.</p>
*/
public abstract class ExceptionBase extends Throwable { // make this a checked exception  
	private static final long serialVersionUID = 1L;

	static final int SEVERITY_COUNT = 4;

   public final ExceptionSeverity severity;
   final LocalDateTime raiseTime;
   final String sourceName;
   final int sourceLineNum;

   protected ExceptionBase(ExceptionSeverity severity, String sourceName, int sourceLineNum, String message) {
      super(message);
      this.raiseTime = LocalDateTime.now();
      this.severity = severity;
      this.sourceName = sourceName != null ? sourceName : "";
      this.sourceLineNum = sourceLineNum;
   }

   public final void addToLog() {
      Program.getLog().add(this);
   }
}