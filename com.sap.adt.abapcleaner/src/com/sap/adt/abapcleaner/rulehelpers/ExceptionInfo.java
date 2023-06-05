package com.sap.adt.abapcleaner.rulehelpers;

public class ExceptionInfo {
	public final String name;
	public final boolean isClassBased;
	public final boolean isResumable;
	
	public static ExceptionInfo createClassBased(String name, boolean isResumable) {
		return new ExceptionInfo(name, true, isResumable);
	}
	
	public static ExceptionInfo createNonClassBased(String name) {
		return new ExceptionInfo(name, false, false);
	}
	
	private ExceptionInfo(String name, boolean isClassBased, boolean isResumable) {
		this.name = name;
		this.isClassBased = isClassBased;
		this.isResumable = isResumable;
	}
}
