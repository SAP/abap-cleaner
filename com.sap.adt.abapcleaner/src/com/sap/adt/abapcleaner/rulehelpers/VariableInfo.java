package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

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

   public boolean isUsed() { return usedCount > 0; }
   public boolean isUsedInComment() { return usedCountInComment > 0; }

   public boolean isAssigned() { return assignedCount > 0; }
   public boolean isAssignedAfterDeclaration() { return isDeclaredInline ? (assignedCount > 1) : (assignedCount > 0); }
   public boolean isAssignedInComment() { return assignedCountInComment > 0; }
   
   public VariableInfo(Token declarationToken, boolean isDeclaredInline, boolean isType, boolean isConstant, boolean isBoundStructuredData) {
      this.declarationToken = declarationToken;
      this.isDeclaredInline = isDeclaredInline;
      this.isType = isType;
      this.isConstant = isConstant;
      this.isBoundStructuredData = isBoundStructuredData;
   }

	public void addUsage(Token token, boolean isAssignment, boolean isUsageInSelfAssignment, boolean isCommentedOut, boolean writesToReferencedMemory) {
		if (isCommentedOut) {
			if (isAssignment)
				++assignedCountInComment;
			else if (isUsageInSelfAssignment)
				++usedCountInSelfAssignmentInComment;
			else
				++usedCountInComment;
		} else {
			if (isAssignment)
				++assignedCount;
			else if (isUsageInSelfAssignment)
				++usedCountInSelfAssignment;
			else
				++usedCount;
		}
		if (writesToReferencedMemory)
			++writeToReferencedMemoryCount;

		if (token != null) {
			Command command = token.getParentCommand();
			enclosingCommandWithCommentUsage = updateEnclosingCommand(enclosingCommandWithCommentUsage, command);
			if (!isCommentedOut)
				enclosingCommand = updateEnclosingCommand(enclosingCommand, command);
		}
	}

	public void addAssignment(Token token) {
		++assignedCount;

		if (token != null) {
			Command command = token.getParentCommand();
			enclosingCommand = updateEnclosingCommand(enclosingCommand, command);
			enclosingCommandWithCommentUsage = updateEnclosingCommand(enclosingCommandWithCommentUsage, command);
		}
	}

	public void addAssignedFieldSymol(VariableInfo fieldSymbolInfo) {
		if (fieldSymbolInfo != null && !assignedFieldSymbols.contains(fieldSymbolInfo))
			assignedFieldSymbols.add(fieldSymbolInfo);
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
   	this.typeSource = typeSource;
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
}
