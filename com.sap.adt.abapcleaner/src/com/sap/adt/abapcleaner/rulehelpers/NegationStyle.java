package com.sap.adt.abapcleaner.rulehelpers;

/** When to negate logical expressions with NOT ( ... ) */
public enum NegationStyle {
	/** always use NOT ( ... ) to negate logical expressions with AND / OR */
   ALWAYS_WITH_AND_OR,
   /** negate with NOT ( ... ) if multiple inner negations (IS NOT, <>, ...) can be avoided */
   AVOID_INNER_NEGATIONS,
   /** never negate with NOT ( ... ) */
   NEVER;

	public static final int SIZE = java.lang.Integer.SIZE;

	public static final String description = "Negate logical expressions with NOT ( ... ):";
	public static final String[] selectionTexts = new String[] { 
			"always with AND / OR ", "if multiple inner negations (IS NOT, <>, ...) can be avoided", "never" };

   public int getValue() {
      return this.ordinal();
   }

   public static NegationStyle forValue(int value) {
      return values()[value];
   }
}