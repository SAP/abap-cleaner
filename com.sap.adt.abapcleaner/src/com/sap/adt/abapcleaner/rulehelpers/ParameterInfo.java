package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.parser.Token;

public class ParameterInfo {
   public final Token declarationToken;
	public final String name;
	public final VariableAccessType accessType;
	public final boolean isNeeded;
	public final boolean isByValue; 

	public ParameterInfo(Token declarationToken, String name, VariableAccessType accessType, boolean isNeeded, boolean isByValue, boolean isReference) {
		this.declarationToken = declarationToken;
		this.name = name;
		this.accessType = accessType;
		this.isNeeded = isNeeded;
		this.isByValue = isByValue;
	}
}
