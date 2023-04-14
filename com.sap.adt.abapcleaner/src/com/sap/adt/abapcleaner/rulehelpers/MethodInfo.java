package com.sap.adt.abapcleaner.rulehelpers;

import java.util.HashMap;

import com.sap.adt.abapcleaner.base.AbapCult;

public class MethodInfo {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

	public final String name;
	public final MethodVisibility visiblity;
	public boolean isRedefinition;
	public HashMap<String, ParameterInfo> parameters;

	public MethodInfo(String name, MethodVisibility visibility) {
		this.name = name;
		this.visiblity = visibility;
		this.isRedefinition = false;
		parameters = new HashMap<>();
	}
	public void addParameter(ParameterInfo parameter) {
		parameters.put(getNameKey(parameter.name), parameter);
	}
	public boolean hasParameter(String parameterName) {
		return parameters.containsKey(getNameKey(parameterName));
	}
	public ParameterInfo getParameter(String parameterName) {
		return parameters.get(getNameKey(parameterName));
	}
}
