package com.sap.adt.abapcleaner.rules.declarations;

public enum UnusedVariableActionIfAssigned  {
   ADD_TODO_COMMENT,
   IGNORE;

	public int getValue() { return this.ordinal(); }

   public static UnusedVariableActionIfAssigned forValue(int value) {
      return values()[value];
   }
}