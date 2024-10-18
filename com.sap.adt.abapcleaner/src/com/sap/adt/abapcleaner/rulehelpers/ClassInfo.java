package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Command;

/** encapsulates class or interface definition information: implemented interfaces, defined aliases and method signatures */
public class ClassInfo {
   private static String getNameKey(String name) {
      return AbapCult.toUpper(name);
   }

   public final Command declarationCommand;
	public final String name;
	public final ClassInfo parentClass;

	private HashMap<String, ClassInfo> interfaces;
	private HashMap<String, String> aliases;
	
	private HashMap<String, MethodInfo> methods;
	private ArrayList<MethodInfo> methodsInOrder;
	
	private Variables variables;
   private Command implementationStart; // may be null while the DEFINITION part is being processed

	public ClassInfo(Command declarationCommand, String name, ClassInfo parentClass) {
		this.declarationCommand = declarationCommand;
		this.name = name;
		this.parentClass = parentClass;
		
		interfaces = new HashMap<>();
		aliases = new HashMap<>();
		
		methods = new HashMap<>();
		methodsInOrder = new ArrayList<>();
	}
	
	public void addMethod(MethodInfo method) {
		methods.put(getNameKey(method.name), method);
		methodsInOrder.add(method);
	}
	
	public void setVariables(Variables variables) {
		this.variables = variables;
	}
	
	public Variables getVariables() {
		return variables;
	}

	public void setImplementationStart(Command implementationStart) {
	   this.implementationStart = implementationStart;
	}

	public Command getImplementationStart() {
	   return implementationStart;
	}

	/** returns the local variable or, otherwise, the attribute of the supplied name */
	public VariableInfo getLocalVariableOrAttribute(Variables localVariables, String objectName, boolean isType, boolean searchInParentClasses) {
		VariableInfo variableInfo = null;
		if (localVariables != null)
			variableInfo = localVariables.getVariableInfo(objectName, isType);
		// do NOT attach with "else if":
		if (variableInfo == null)
			variableInfo = getAttribute(objectName, isType, searchInParentClasses);
		return variableInfo;
	}
	
	/** returns the (public, protected or private) attribute of the supplied name */
	public VariableInfo getAttribute(String objectName, boolean isType, boolean searchInParentClasses) {
		ClassInfo classInfo = this;
		while (classInfo != null) {
			VariableInfo variableInfo = variables.getVariableInfo(objectName, isType);
			if (variableInfo != null) {
				if (classInfo == this || variableInfo.accessType != VariableAccessType.PRIVATE) {
					return variableInfo;
				}
			}
			if (!searchInParentClasses)
				break;
			classInfo = classInfo.parentClass;
		}
		return null;
	}
	
	/** @param methodName - can be a plain method name, an alias, or INTERFACE~METHOD */
	public MethodInfo getMethod(String methodName) {
		if (StringUtil.isNullOrEmpty(methodName))
			return null;
		
		// find the method signature in the class hierarchy (if visible in the code document)
		ClassInfo curClass = this;
		do {
			MethodInfo methodInfo = curClass.methods.get(getNameKey(methodName));
			if (methodInfo != null && !methodInfo.isRedefinition)
				return methodInfo;
			curClass = curClass.parentClass;
		} while (curClass != null);
		// if the full signature is out of sight, use METHODS ... REDEFINITION
		MethodInfo redefMethodInfo = methods.get(getNameKey(methodName));
		if (redefMethodInfo != null)
			return redefMethodInfo;
		
		// is methodName an alias or an explicit interface method?
		String interfaceAndMethod = aliases.get(getNameKey(methodName));
		if (interfaceAndMethod == null)
			interfaceAndMethod = methodName;
		int tildePos = interfaceAndMethod.indexOf(ABAP.TILDE);
		if (tildePos <= 0 || tildePos + 2 >= interfaceAndMethod.length())
			return null;
		
		// retrieve the interface (only possible if it was defined locally)
		ClassInfo interfaceInfo = getInterface(interfaceAndMethod.substring(0, tildePos));
		return (interfaceInfo == null) ? null : interfaceInfo.getMethod(interfaceAndMethod.substring(tildePos + 1));
	}
	
	public void addInterface(ClassInfo interfaceInfo) {
		// to avoid endless loops, never add an interfaceInfo to itself
		if (interfaceInfo != this && !AbapCult.stringEquals(interfaceInfo.name, name, true))
			interfaces.put(getNameKey(interfaceInfo.name), interfaceInfo);
	}

	/** recursively searches for the supplied interface (which may itself be implemented only inside an interface) */
	public ClassInfo getInterface(String interfaceName) {
		ClassInfo result = interfaces.get(getNameKey(interfaceName));
		if (result != null)
			return result;

		// recursively determine whether the interface is implemented indirectly (i.e. inside one of the interfaces) 
		for (ClassInfo interfaceInfo : interfaces.values()) {
			result = interfaceInfo.getInterface(interfaceName);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	public void addAlias(String alias, String interfaceAndMethod) {
		aliases.put(getNameKey(alias), interfaceAndMethod);
	}

	public Iterable<MethodInfo> getMethodsInOrder() {
		return methodsInOrder;
	}
}
