package com.sap.adt.abapcleaner.rules.declarations;

public enum ClassDefIndentStyle {
	PLUS_2(2), 
	PLUS_4(4), 
	BELOW_NAME(6), 
	BELOW_DEFINITION(-1);
	
	public final int indent;
	
	private ClassDefIndentStyle(int indent) {
		this.indent = indent;
	}
	
	public int getValue() { 
		return this.ordinal(); 
	}

	public static ClassDefIndentStyle forValue(int value) { 
		return values()[value]; 
	}
}
