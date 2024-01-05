package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.parser.Token;

public class MethodInfo {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

   public final Token declarationToken;
	public final String name;
	public final MethodVisibility visibility;

	// possible combinations of the qualifiers: a method may be 
	// - ABSTRACT 
	// - FINAL
	// - REDEFINITION
	// - FINAL REDEFINITION 
	// - FOR TESTING
	// - (none of these)
	public final boolean isAbstract;
	public final boolean isFinal;
	public final boolean isRedefinition;
	public final boolean isForTesting;
	public final boolean isInOOContext;
	
	private HashMap<String, ParameterInfo> parameters;
	private ArrayList<ParameterInfo> parametersInOrder;
	
	private HashMap<String, ExceptionInfo> exceptions;
	private ArrayList<ExceptionInfo> exceptionsInOrder;
	
	public MethodInfo(Token declarationToken, MethodVisibility visibility, boolean isAbstract, boolean isFinal, boolean isRedefinition, boolean isForTesting) {
		this.declarationToken = declarationToken;
		this.name = declarationToken.getText();
		this.visibility = visibility;
		
		this.isAbstract = isAbstract;
		this.isFinal = isFinal;
		this.isRedefinition = isRedefinition;
		this.isForTesting = isForTesting;
		this.isInOOContext = declarationToken.getParentCommand().isInOOContext();
		
		parameters = new HashMap<>();
		parametersInOrder = new ArrayList<>();
		
		exceptions = new HashMap<>();
		exceptionsInOrder = new ArrayList<>();
	}

	public void addParameter(ParameterInfo parameter) {
		parameters.put(getNameKey(parameter.name), parameter);
		parametersInOrder.add(parameter);
	}
	
	public boolean hasParameter(String parameterName) {
		return parameters.containsKey(getNameKey(parameterName));
	}
	
	public ParameterInfo getParameter(String parameterName) {
		return parameters.get(getNameKey(parameterName));
	}
	
	public Iterable<ParameterInfo> getParametersInOrder() {
		return parametersInOrder;
	}

	public void addException(ExceptionInfo Exception) {
		exceptions.put(getNameKey(Exception.name), Exception);
		exceptionsInOrder.add(Exception);
	}
	
	public boolean hasException(String ExceptionName) {
		return exceptions.containsKey(getNameKey(ExceptionName));
	}
	
	public ExceptionInfo getException(String ExceptionName) {
		return exceptions.get(getNameKey(ExceptionName));
	}
	
	public Iterable<ExceptionInfo> getExceptionsInOrder() {
		return exceptionsInOrder;
	}
}
