package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.Rule;

public class LocalVariables {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

	public static String getObjectName(String identifier) {
		// remove the @ used in SQL statements
		if (AbapCult.stringStartsWith(identifier, "@"))
			identifier = identifier.substring(1);

		// reduce the identifier to its first part, i.e.
		// - get the structure variable in cases of "structure-component",
		// - get the identifier of the instance in cases of "instance_var->member_var",
		// - remove substring information like lv_date+4(2)
		identifier = ABAP.readTillEndOfVariableName(identifier, 0, true);
		return identifier;
	}

	// -------------------------------------------------------------------------
	
   private Rule rule;
   private MethodInfo methodInfo;
   private boolean methodUsesMacros;
   
	public HashMap<String, VariableInfo> locals = new HashMap<String, VariableInfo>();

	public ArrayList<VariableInfo> localsInDeclarationOrder = new ArrayList<VariableInfo>();
	public ArrayList<VariableInfo> localsInUsageOrder = new ArrayList<VariableInfo>();
	public ArrayList<VariableInfo> localsInNonCommentUsageOrder = new ArrayList<VariableInfo>();

	public HashSet<VariableInfo> localsWithUsage = new HashSet<VariableInfo>();
	public HashSet<VariableInfo> localsWithNonCommentUsage = new HashSet<VariableInfo>();

	public LocalVariables(Rule rule, MethodInfo methodInfo) {
		this.rule = rule;
		this.methodInfo = methodInfo;
	}

	public boolean isMethodSignatureKnown() {
		return (methodInfo != null && !methodInfo.isRedefinition);
	}
	
	public MethodInfo getMethodInfo() {
		return methodInfo;
	}
	
	public boolean isEmpty() { 
		return locals.isEmpty(); 
	}
	
	public Iterable<VariableInfo> getLocalsInDeclarationOrder() {
		return localsInDeclarationOrder;
	}

	public Iterable<VariableInfo> getLocalsInUsageOrder() {
		return localsInUsageOrder;
	}
	
	public Iterable<VariableInfo> getLocalsInNonCommentUsageOrder() {
		return localsInNonCommentUsageOrder;
	}
	
	public VariableInfo addDeclaration(Token identifier, boolean isDeclaredInline, boolean isType, boolean isConstant, boolean isBoundStructuredData) throws UnexpectedSyntaxBeforeChanges {
		if (!identifier.isIdentifier())
			throw new UnexpectedSyntaxBeforeChanges(rule, identifier, "Expected an identifier, but found " + identifier.getTypeAndTextForErrorMessage() + "!");

		String text = identifier.getText();

		// for declarations like "DATA lv_textdat1(20) TYPE c.", reduce the identifier to its first part
		text = ABAP.readTillEndOfVariableName(text, 0, true);

		String key = getNameKey(text);
		if (locals.containsKey(key))
			throw new UnexpectedSyntaxBeforeChanges(rule, identifier, (isConstant ? "Constant" : "Variable") + " '" + identifier.getText() + "' seems to be declared twice!");

		VariableInfo varInfo = new VariableInfo(identifier, isDeclaredInline, isType, isConstant, isBoundStructuredData);
		if (isDeclaredInline) {
			varInfo.addAssignment(identifier);
		}
		
		locals.put(key, varInfo);
		localsInDeclarationOrder.add(varInfo);
		return varInfo;
	}

	public VariableInfo getVariableInfo(Token identifier) {
		return getVariableInfo(getObjectName(identifier.getText()));
	}

	public boolean containsVariableInfo(String objectName) { 
		return locals.containsKey(getNameKey(objectName)); 
	}

	public VariableInfo getVariableInfo(String objectName) { 
		return locals.get(getNameKey(objectName)); 
	}

	public void setNeeded(String name) {
		addUsage(null, name, false, false, false, false);
	}
	public void addInlineDeclaration(Token identifier, String name) {
		addUsage(identifier, name, true, false, false, false);
	}
	public void addUsageInLikeClause(Token identifier, String name, Command methodStart, VariableInfo referringDeclaration) {
		VariableInfo varInfo = addUsage(identifier, name, false, false, false, false);
		if (varInfo == null) 
			return;

		// variables that are used in LIKE clauses of other declarations must be declared at method start to prevent 
		// that the LocalDeclarationOrderRule moves their declaration behind the other declaration (note that addUsage() 
		// above does NOT prevent that, because the other declaration may itself be moved from an inner block to method start)
		if (!varInfo.isDeclaredInline)
			varInfo.setEnclosingCommand(methodStart);

		if (referringDeclaration != null) {
			referringDeclaration.setTypeSource(varInfo);
			// if varInfo is declared inline, the referring declaration (that uses varInfo in its LIKE clause) must NOT be moved
			// to a different position by the LocalDeclarationOrderRule
			if (varInfo.isDeclaredInline) 
				referringDeclaration.declarationCannotBeMoved = true;
		}
	}
	public void addUsage(Token identifier, String name) {
		addUsage(identifier, name, false, false, false, false);
	}
	public VariableInfo addUsage(Token identifier, String name, boolean isAssignment, boolean isUsageInSelfAssignment, boolean isCommentedOut, boolean writesToReferencedMemory) {
		String key = getNameKey(getObjectName(name));
		VariableInfo varInfo = locals.get(key);
		if (varInfo == null) 
			return null;
		
		// for 'real' usages (as opposed to pseudo-usage if ##NEEDED or "#EC NEEDED is found):
		if (identifier != null) {
			// enhance the ordered usage lists
			if (!localsWithUsage.contains(varInfo)) {
				localsInUsageOrder.add(varInfo);
				localsWithUsage.add(varInfo);
			}
			if (!isCommentedOut && !localsWithNonCommentUsage.contains(varInfo)) {
				localsInNonCommentUsageOrder.add(varInfo);
				localsWithNonCommentUsage.add(varInfo);
			}
		}

		varInfo.addUsage(identifier, isAssignment, isUsageInSelfAssignment, isCommentedOut, writesToReferencedMemory);
		return varInfo;
	}
	
	public void setMethodUsesMacros() {
		methodUsesMacros = true;
	}
	
	public boolean getMethodUsesMacros() {
		return methodUsesMacros;
	}
}
