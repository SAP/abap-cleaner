package com.sap.adt.abapcleaner.programbase;

/** 
 * <p>Exception class indicating that the command line arguments are invalid.</p>
*/
public class CommandLineException extends IllegalArgumentException {
	private static final long serialVersionUID = 1L;

   public CommandLineException(String message) {
      super(message);
   }
}