package com.sap.adt.abapcleaner.rulehelpers;

public class ParameterInfo {
	public final String name;
	public final ParameterAccessType accessType;

	public ParameterInfo(String name, ParameterAccessType accessType) {
		this.name = name;
		this.accessType = accessType;
	}
}
