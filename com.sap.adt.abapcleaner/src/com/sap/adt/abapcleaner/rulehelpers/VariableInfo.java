package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;

public class VariableInfo {
	/** the Token that contains the variable name in its declaration */
   public final Token declarationToken;
   public final boolean isDeclaredInline;
   public final boolean isType;
   public final boolean isConstant;
   /** true if this variable was declared with DATA/CONSTANTS/STATICS BEGIN OF */
   public final boolean isBoundStructuredData;
   public final VariableAccessType accessType;
   private boolean isNeeded;
   public final ParameterInfo parameterInfo;
   
   /** the level-opening Command that encloses all (non-declaration) usages of this variable */
   private Command enclosingCommand;

   /** the level-opening Command that encloses all (non-declaration) usages of this variable, including usage in comments */
   private Command enclosingCommandWithCommentUsage;

   /** the declaration from which the type is derived, if this declaration uses LIKE [LINE OF] etc. */
   private VariableInfo typeSource;
   
   private int usedCount;
   private int usedCountInComment;
   @SuppressWarnings("unused")
	private int usedCountInSelfAssignment;
   @SuppressWarnings("unused")
	private int usedCountInSelfAssignmentInComment;

   private int assignedCount;
   private int assignedCountInMessageInto;
   private int assignedCountInComment;
   
   /** is increased when a field-symbol is found in a write position (NOT an assignment position), e.g. '&lt;fs&gt;-comp = 1.' */
   private int writeToReferencedMemoryCount;
   
   /** the VariableInfo of all field-symbols that are at some point assigned to this variable */
   private ArrayList<VariableInfo> assignedFieldSymbols = new ArrayList<>();
   
   /** true if at any point, a data reference is pointed to the memory area of this variable */
   private boolean isReferencedByDataRef;

   /** true if the declaration cannot be moved to a different position, because it has a LIKE clause which refers to another 
    * variable which is declared inline */
   public boolean declarationCannotBeMoved;

   public boolean isParameter() { return (parameterInfo != null); } // alternative: return (accessType != VariableAccessType.LOCAL)
   public boolean isParameterByValue() { return (parameterInfo != null) && parameterInfo.isByValue; } 
   public boolean isNeeded() { return isNeeded; }
   
   public boolean isUsed() { return usedCount > 0; }
   public boolean isUsedInComment() { return usedCountInComment > 0; }

   public boolean isAssigned() { return assignedCount > 0; }
   public boolean isAssignedAfterDeclaration() { return isDeclaredInline ? (assignedCount > 1) : (assignedCount > 0); }
   public boolean isAssignedInMessageInto() { return assignedCountInMessageInto > 0; }
   public boolean isAssignedInComment() { return assignedCountInComment > 0; }
   
   public VariableInfo(Token declarationToken, boolean isDeclaredInline, boolean isType, boolean isConstant, boolean isBoundStructuredData, VariableAccessType variableAccessType) {
      this.declarationToken = declarationToken;
      this.isDeclaredInline = isDeclaredInline;
      this.isType = isType;
      this.isConstant = isConstant;
      this.isBoundStructuredData = isBoundStructuredData;
      this.accessType = variableAccessType;
      this.parameterInfo = null;
   }

   public VariableInfo(ParameterInfo parameterInfo) {
      this.declarationToken = parameterInfo.declarationToken;
      this.isDeclaredInline = false;
      this.isType = false;
      this.isConstant = false;
      this.isBoundStructuredData = false;
      this.accessType = parameterInfo.accessType;
      this.isNeeded = parameterInfo.isNeeded;
      this.parameterInfo = parameterInfo;
   }

   @Override
   public String toString() { // for debugging
   	return (declarationToken == null) ? "" : declarationToken.getText();
   }

   public void setNeeded() {
   	isNeeded = true;
   }
   
	public void addUsage(Token token, boolean isAssignment, boolean isUsageInSelfAssignment, boolean isCommentedOut, boolean writesToReferencedMemory, boolean isAssignedInMessageInto) {
		if (isCommentedOut) {
			if (isAssignment) {
				++assignedCountInComment;
			} else if (isUsageInSelfAssignment) {
				++usedCountInSelfAssignmentInComment;
			} else {
				++usedCountInComment;
			}
		} else {
			if (isAssignment) {
				++assignedCount;
				if (isAssignedInMessageInto) {
					++assignedCountInMessageInto;
				}
			} else if (isUsageInSelfAssignment) {
				++usedCountInSelfAssignment;
			} else {
				++usedCount;
			}
		}
		if (writesToReferencedMemory)
			++writeToReferencedMemoryCount;
		
		if (token != null) {
			Command command = token.getParentCommand();
			enclosingCommandWithCommentUsage = updateEnclosingCommand(enclosingCommandWithCommentUsage, command);
			if (!isCommentedOut) {
				enclosingCommand = updateEnclosingCommand(enclosingCommand, command);
			}
		}
	}

	public void addAssignment(Token token, boolean isAssignedInMessageInto) {
		++assignedCount;
		if (isAssignedInMessageInto) 
			++assignedCountInMessageInto;

		if (token != null) {
			Command command = token.getParentCommand();
			enclosingCommand = updateEnclosingCommand(enclosingCommand, command);
			enclosingCommandWithCommentUsage = updateEnclosingCommand(enclosingCommandWithCommentUsage, command);
		}
	}

	public void addAssignedFieldSymol(VariableInfo fieldSymbolInfo) {
		if (fieldSymbolInfo != null && !assignedFieldSymbols.contains(fieldSymbolInfo)) {
			assignedFieldSymbols.add(fieldSymbolInfo);
		}
	}
	
	public void addReferenceByDataRef() {
		// note down that a data reference points to the memory area of the local variable
		isReferencedByDataRef = true;
	}

	public boolean mayHaveIndirectWrites() {
		// return true if at any point, a data reference is created to the variable memory, keep DATA
		if (isReferencedByDataRef)
			return true;
		
		// determine whether there are potential indirect writes on this variable, as in 
		//   LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>). 
		//     <fs>-component = 1.
		// return true if among the field-symbols which are (at any point) assigned to this variable, any write position is
		// found (i.e. a Command in which the memory referenced by this field-symbol is modified), 
		for (VariableInfo assignedFieldSymbol : assignedFieldSymbols) {
			if (assignedFieldSymbol.writeToReferencedMemoryCount > 0) {
				return true;
			}
		}
		return false;
	}

   public void setEnclosingCommand(Command command) {
   	enclosingCommand = command;
   	enclosingCommandWithCommentUsage = command;
   }
   
   public void setTypeSource(VariableInfo typeSource) {
   	// do not enter cases in which a VariableInfo would refer to itself as the typeSource, 
   	// such as 'DATA: BEGIN OF ls_struct, ...' 
   	if (typeSource != this) {
   		this.typeSource = typeSource;
   	}
   }

   public VariableInfo getTypeSource() {
   	return typeSource;
   }
   
   private static Command updateEnclosingCommand(Command enclosingCommand, Command newCommand) {
   	if (newCommand == null)
   		return enclosingCommand;
   	else if (enclosingCommand == null) 
   		return newCommand.getParent();
   	
   	// find the common parent of the enclosing Command and the new Command
   	HashSet<Command> enclosingCommands = new HashSet<>();
   	Command parent = enclosingCommand;
   	do {
   		enclosingCommands.add(parent);
   		parent = parent.getParent();
   	} while(parent != null);
   	
   	Command command = newCommand.getParent();
   	while (command != null) {
   		if (enclosingCommands.contains(command)) {
   			return command;
   		}
   		command = command.getParent();
   	}
   	return enclosingCommand;
   }
   
   public Command getEnclosingCommand(boolean considerCommentUsage) { 
   	return considerCommentUsage ? enclosingCommandWithCommentUsage : enclosingCommand; 
   }
   
   /** returns true if the type of the variable is a known character-like type that is not structured */
   public TriState hasUnstructuredCharlikeType(Variables variables) {
   	VariableInfo varInfo = this;
   	while (varInfo.typeSource != null)
   		varInfo = varInfo.typeSource;
   	
   	Token token = varInfo.declarationToken; 
   	if (token == null) // pro forma
   		return TriState.UNKNOWN;
   	
   	// move to the 'TYPE' or 'LIKE' keyword in 'lv_var TYPE ...' or 'VALUE(rv_var) TYPE ...'
   	token = token.getNext().textEquals(")") ? token.getNext().getNextCodeSibling() : token.getNextCodeSibling();
   	if (token == null) // pro forma
   		return TriState.UNKNOWN;
   	boolean isType = token.isKeyword("TYPE");
   	boolean isLike = token.isKeyword("LIKE");
   	if (!isType && !isLike)
   		return TriState.UNKNOWN;

   	token = token.getNextCodeSibling();
   	if (token == null) // pro forma
   		return TriState.UNKNOWN;
   	else if (token.isKeyword()) // e.g. STANDARD TABLE OF ...
   		return TriState.FALSE;
   	
   	// if the type is itself defined locally, find out whether it is a structured type
   	VariableInfo typeInfo = variables.getVariableInfo(token.getText(), true);
   	if (typeInfo != null && typeInfo.isBoundStructuredData)
   		return TriState.FALSE;

   	// built-in character-like types: string, c [LENGTH ...], n [LENGTH ...]; 
   	// built-in date/time types d, t (but not utclong) and corresponding built-in ABAP Dictionary types
   	if (isType && token.textEqualsAny("string", "c", "n", "d", "t", "datn", "dats", "timn", "tims", "sstring", "lang")) 
   		return TriState.TRUE;
   	// generic types (only possible for field symbols)
   	if (isType && token.textEqualsAny("clike", "csequence")) 
   		return TriState.TRUE;

   	// character-like system fields and their types e.g. sy-uname / syst_uname (esp. with length > 1)
   	String syField = null;
   	if (token.textStartsWith("sy-")) { // this is also tolerated after TYPE
   		syField = token.getText().substring("sy-".length());
   	} else if (isType && token.textStartsWith("syst_")) {
   		syField = token.getText().substring("syst_".length());
   	}
   	if (syField != null && AbapCult.stringEqualsAny(true, syField, "abcde", "cprog", "dbnam", "dbsys", "host", "langu", "ldbpg", "lisel", "msgid", "msgty", "msgv1", "msgv2", "msgv3", "msgv4", "pfkey", "slset", "sysid", "tcode", "title", "ucomm", "uname", "zonlo"))
	   	return TriState.TRUE;

   	// char5, numc5 etc.
   	String text = token.getText();
   	if (isType && StringUtil.hasTrailingDigits(text)) {
	   	String textWithoutDigits = StringUtil.removeTrailingDigits(text);
	   	if (AbapCult.stringEqualsAny(true, textWithoutDigits, "char", "numc")) { 
	   		return TriState.TRUE;
	   	}
   	}
   	
   	return TriState.UNKNOWN;
   }
}
