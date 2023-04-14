package com.sap.adt.abapcleaner.rules.declarations;

public enum UnusedVariableMeasureIfAssigned  {
   ADD_TODO_COMMENT,
   IGNORE;

	public int getValue() { return this.ordinal(); }

   public static UnusedVariableMeasureIfAssigned forValue(int value) {
      return values()[value];
   }
}