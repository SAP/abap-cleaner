package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.Rule;

public class Variables {
   private static String getNameKey(String name, boolean isType) {
   	// types can use the same identifiers as data objects (e.g. "TYPES BEGIN OF group ..." and "DATA group TYPE TABLE OF group"), 
   	// therefore we use a prefix to store types independently
      return (isType ? "~" : "") + AbapCult.toUpper(name);
   }

	public static String getObjectName(String identifier, boolean isInOOContext) {
		// remove the @ used in SQL statements, or the ! used for escaping identifiers
		int start = AbapCult.stringStartsWithAny(identifier, "@", ABAP.OPERAND_ESCAPE_CHAR_STRING) ? 1 : 0;

		// reduce the identifier to its first part, i.e.
		// - get the structure variable in cases of "structure-component",
		// - get the identifier of the instance in cases of "instance_var->member_var",
		// - remove substring information like lv_date+4(2)
		identifier = ABAP.readTillEndOfVariableName(identifier, start, true, isInOOContext);
		return identifier;
	}

	// -------------------------------------------------------------------------
	
   private final Rule rule;
   private final ClassInfo classInfo;
   private final MethodInfo methodInfo; // null for attributes
   private final boolean isInOOContext;
   public final VariableInfo returningParameter;
   private boolean methodUsesMacrosOrTestInjection;
   private boolean methodUsesDynamicAssign;
   
	public HashMap<String, VariableInfo> locals = new HashMap<String, VariableInfo>();

	public ArrayList<VariableInfo> localsInDeclarationOrder = new ArrayList<VariableInfo>();
	public ArrayList<VariableInfo> localsInUsageOrder = new ArrayList<VariableInfo>();
	public ArrayList<VariableInfo> localsInNonCommentUsageOrder = new ArrayList<VariableInfo>();

	public HashSet<VariableInfo> localsWithUsage = new HashSet<VariableInfo>();
	public HashSet<VariableInfo> localsWithNonCommentUsage = new HashSet<VariableInfo>();

	public Variables(Rule rule, ClassInfo classInfo, MethodInfo methodInfo) {
		this.rule = rule;
		this.classInfo = classInfo;
		this.methodInfo = methodInfo;
		this.isInOOContext = (methodInfo == null) ? false : methodInfo.isInOOContext;
		
		VariableInfo returningParameter = null;
		if (methodInfo != null) {
			// if the method signature is known, add the parameters as local variables (note that a method signature, 
			// e.g. from an interface, may be used for several implementations in the same code document!)  
			for (ParameterInfo parameterInfo : methodInfo.getParametersInOrder()) {
				String key = getNameKey(parameterInfo.name, false);
				VariableInfo varInfo = new VariableInfo(parameterInfo);
				locals.put(key, varInfo);
				localsInDeclarationOrder.add(varInfo);
				if (parameterInfo.accessType == VariableAccessType.RETURNING) {
					returningParameter = varInfo;
				}
			}
		}
		this.returningParameter = returningParameter;
	}

	public boolean isMethodSignatureKnown() {
		return (methodInfo != null && !methodInfo.isRedefinition);
	}
	
	public ClassInfo getClassInfo() {
		return classInfo;
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
	
	public VariableInfo addDeclaration(Token identifier, boolean isDeclaredInline, boolean isType, boolean isConstant, boolean isBoundStructuredData, boolean isInOOContext, boolean isAssignedInMessageInto, VariableAccessType variableAccessType) throws UnexpectedSyntaxBeforeChanges {
		if (!identifier.isIdentifier())
			throw new UnexpectedSyntaxBeforeChanges(rule, identifier, "Expected an identifier, but found " + identifier.getTypeAndTextForErrorMessage() + "!");

		String text = identifier.getText();

		// skip the ! escape character
		int start = StringUtil.startsWith(text, ABAP.OPERAND_ESCAPE_CHAR_STRING, false) ? 1 : 0;

		// for declarations like "DATA lv_textdat1(20) TYPE c.", reduce the identifier to its first part;
		text = ABAP.readTillEndOfVariableName(text, start, true, isInOOContext);

		String key = getNameKey(text, isType);
		if (locals.containsKey(key))
			throw new UnexpectedSyntaxBeforeChanges(rule, identifier, (isConstant ? "Constant" : "Variable") + " '" + identifier.getText() + "' seems to be declared twice!");

		VariableInfo varInfo = new VariableInfo(identifier, isDeclaredInline, isType, isConstant, isBoundStructuredData, variableAccessType);
		if (isDeclaredInline) {
			varInfo.addAssignment(identifier, isAssignedInMessageInto);
		}
		
		locals.put(key, varInfo);
		localsInDeclarationOrder.add(varInfo);
		return varInfo;
	}

	public VariableInfo getVariableInfo(Token identifier, boolean isType) {
		return getVariableInfo(getObjectName(identifier.getText(), isInOOContext), isType);
	}

	public boolean containsVariableInfo(String objectName, boolean isType) { 
		return locals.containsKey(getNameKey(objectName, isType)); 
	}

	public VariableInfo getVariableInfo(String objectName, boolean isType) { 
		return locals.get(getNameKey(objectName, isType)); 
	}

	public void setNeeded(String name) {
		String key = getNameKey(getObjectName(name, isInOOContext), false);
		VariableInfo varInfo = locals.get(key);
		if (varInfo != null) {
			varInfo.setNeeded();
		}
	}

	public void addInlineDeclaration(Token identifier, String name, boolean isUsageInMessageInto) {
		addUsage(identifier, name, true, false, false, false, isUsageInMessageInto);
	}
	public void addUsageInLikeOrValueClause(Token identifier, String name, Command methodStart, VariableInfo referringDeclaration) {
		VariableInfo varInfo = addUsage(identifier, name, false, false, false, false, false);
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
			if (varInfo.isDeclaredInline) {
				referringDeclaration.declarationCannotBeMoved = true;
			}
		}
	}
	public void addUsage(Token identifier, String name) {
		addUsage(identifier, name, false, false, false, false, false);
	}
	public VariableInfo addUsage(Token identifier, String name, boolean isAssignment, boolean isUsageInSelfAssignment, boolean isCommentedOut, boolean writesToReferencedMemory, boolean isAssignedInMessageInto) {
		// determine whether the identifier represent a type (rather than a data object);  
		boolean isType = identifier.isTypeIdentifier(true);
		
		String key = getNameKey(getObjectName(name, isInOOContext), isType);
		VariableInfo varInfo = locals.get(key);
		if (varInfo == null) 
			return null;
		
		// enhance the ordered usage lists
		if (!localsWithUsage.contains(varInfo)) {
			localsInUsageOrder.add(varInfo);
			localsWithUsage.add(varInfo);
		}
		if (!isCommentedOut && !localsWithNonCommentUsage.contains(varInfo)) {
			localsInNonCommentUsageOrder.add(varInfo);
			localsWithNonCommentUsage.add(varInfo);
		}

		varInfo.addUsage(identifier, isAssignment, isUsageInSelfAssignment, isCommentedOut, writesToReferencedMemory, isAssignedInMessageInto);
		return varInfo;
	}
	
	public void setMethodUsesMacrosOrTestInjection() {
		methodUsesMacrosOrTestInjection = true;
	}
	
	public boolean getMethodUsesMacrosOrTestInjection() {
		return methodUsesMacrosOrTestInjection;
	}
	
	public void setMethodUsesDynamicAssign() {
		methodUsesDynamicAssign = true;
	}

	public boolean getMethodUsesDynamicAssign() {
		return methodUsesDynamicAssign;
	}

	public TriState varHasUnstructuredCharlikeType(Token identifier) {
		if (identifier == null)
			return TriState.FALSE;
		// get the variable info for the entire identifier text, meaning that for composed identifiers like 
		// 'ls_struc-comp' and 'lo_obj->mv_attr', this method will deliberately return .UNKNOWN
		VariableInfo varInfo = getVariableInfo(identifier.getText(), false);
		return (varInfo == null) ? TriState.UNKNOWN : varInfo.hasUnstructuredCharlikeType(this);
	}
}
