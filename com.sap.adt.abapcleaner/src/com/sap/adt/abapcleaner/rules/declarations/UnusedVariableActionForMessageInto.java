package com.sap.adt.abapcleaner.rules.declarations;

import com.sap.adt.abapcleaner.parser.ChainElementAction;

public enum UnusedVariableActionForMessageInto {
	ADD_PRAGMA_NEEDED(ChainElementAction.ADD_PRAGMA_NEEDED),
   ADD_TODO_COMMENT(ChainElementAction.ADD_TODO_COMMENT),
   IGNORE(ChainElementAction.IGNORE);

	private ChainElementAction chainElementAction;
	
	private UnusedVariableActionForMessageInto(ChainElementAction chainElementAction) {
		this.chainElementAction = chainElementAction;
	}

	public int getValue() { return this.ordinal(); }
	public ChainElementAction getCorrespondingChainElementAction() { return chainElementAction; }

   public static UnusedVariableActionForMessageInto forValue(int value) {
      return values()[value];
   }
}