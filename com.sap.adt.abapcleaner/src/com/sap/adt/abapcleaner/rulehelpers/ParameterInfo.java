package com.sap.adt.abapcleaner.rulehelpers;

public class ParameterInfo {
	public String name;
	public ParameterAccessType accessType;
	public ParameterInfo(String name, ParameterAccessType accessType) {
		this.name = name;
		this.accessType = accessType;
	}
}
