package com.sap.adt.abapcleaner.programbase;

/**
 * <p>Enumeration specifying the severity of an exception that inherits from {@link ExceptionBase}.</p>
 * 
 * <p>The ExceptionSeverity shows which level of processing must be stopped, e.g.</p>
 * <ul>
 * <li>{@link #S0_STOP_COMMAND} means that the processing of the current {@link Command} must be stopped, 
 * but the current {@link Rule} may continue processing the next {@link Command};</li>
 * <li>{@link #S2_STOP_TASK2} means that the processing of the current {@link Task} must be stopped, 
 * but the {@link Job} may continue processing the next {@link Task}.</li>
 * </ul>
 */
public enum ExceptionSeverity  {
	/** the processing of the current {@link Command} must be stopped, 
	 * but the current {@link Rule} may continue processing the next {@link Command} */
   S0_STOP_COMMAND,
	
   /** the processing of the current {@link Rule} must be stopped, 
	 * but the current {@link Task} may continue processing the next {@link Rule} */
   S1_STOP_RULE, // currently unused
	
   /** the processing of the current {@link Task} must be stopped, 
	 * but the {@link Job} may continue processing the next {@link Task} */
   S2_STOP_TASK,

   /** the processing of the whole {@link Job} must be stopped */
   S3_STOP_JOB; // currently unused

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ExceptionSeverity forValue(int value) {
      return values()[value];
   }
}