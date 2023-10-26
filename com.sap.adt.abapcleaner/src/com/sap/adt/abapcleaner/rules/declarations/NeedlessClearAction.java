package com.sap.adt.abapcleaner.rules.declarations;

import com.sap.adt.abapcleaner.parser.ChainElementAction;

public enum NeedlessClearAction {
   DELETE(ChainElementAction.DELETE),
   ADD_TODO_COMMENT(ChainElementAction.ADD_TODO_COMMENT),
   IGNORE(ChainElementAction.IGNORE);

	private ChainElementAction chainElementAction;
	
	private NeedlessClearAction(ChainElementAction chainElementAction) {
		this.chainElementAction = chainElementAction;
	}

	public int getValue() { return this.ordinal(); }
	public ChainElementAction getCorrespondingChainElementAction() { return chainElementAction; }
	
	public static NeedlessClearAction forValue(int value) {
		return values()[value];
	}
}