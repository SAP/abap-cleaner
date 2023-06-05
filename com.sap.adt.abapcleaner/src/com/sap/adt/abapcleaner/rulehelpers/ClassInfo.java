package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.AbapCult;

public class ClassInfo {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

	public final String name;

	private HashMap<String, MethodInfo> methods;
	private ArrayList<MethodInfo> methodsInOrder;

	public ClassInfo(String name) {
		this.name = name;
		methods = new HashMap<>();
		methodsInOrder = new ArrayList<>();
	}
	
	public void addMethod(MethodInfo method) {
		methods.put(getNameKey(method.name), method);
		methodsInOrder.add(method);
	}
	
	public MethodInfo getMethod(String methodName) {
		return methods.get(getNameKey(methodName));
	}
	
	public Iterable<MethodInfo> getMethodsInOrder() {
		return methodsInOrder;
	}
}
