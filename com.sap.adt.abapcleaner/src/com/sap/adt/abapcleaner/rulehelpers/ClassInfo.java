package com.sap.adt.abapcleaner.rulehelpers;

import java.util.HashMap;

import com.sap.adt.abapcleaner.base.AbapCult;

public class ClassInfo {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

	public final String name;
	public HashMap<String, MethodInfo> methods;

	public ClassInfo(String name) {
		this.name = name;
		methods = new HashMap<>();
	}
	
	public void addMethod(MethodInfo method) {
		methods.put(getNameKey(method.name), method);
	}
	
	public MethodInfo getMethod(String methodName) {
		return methods.get(getNameKey(methodName));
	}
}
