package com.sap.adt.abapcleaner.rules.declarations;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;

import java.time.LocalDate;

public class UnusedVariablesRule extends RuleForDeclarations {
   private final static RuleReference[] references = new RuleReference[] {new RuleReference(RuleSource.ABAP_CLEANER)};

	@Override
	public RuleID getID() { return RuleID.UNUSED_VARIABLES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Delete unused variables"; }

	@Override
	public String getDescription() { return "Deletes unused variables, or comments them out if they are only 'used' in commented-out code. TODO comments can be added for variables that are assigned but never used."; }

	@Override
	public String getHintsAndRestrictions() { return "Note that this rule will skip methods in which macros are used."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 15); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.CHAIN_OF_ONE, RuleID.UPPER_AND_LOWER_CASE, RuleID.INSET }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD delete_unused_variables." 
			+ LINE_SEP + "    CONSTANTS lc_unused        TYPE i VALUE 1200." 
			+ LINE_SEP + "    CONSTANTS lc_commented_out TYPE i VALUE 2." 
			+ LINE_SEP + "    CONSTANTS lc_used_constant TYPE i VALUE 3." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_unused_a      TYPE i." 
			+ LINE_SEP + "    DATA: lv_unused_b      TYPE string," 
			+ LINE_SEP + "          lv_used_var      TYPE i," 
			+ LINE_SEP + "          lv_commented_out LIKE lv_used_var." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_only_assigned TYPE i." 
			+ LINE_SEP + "    DATA lv_assigned_but_used_incomment TYPE i." 
			+ LINE_SEP + "    DATA lv_assigned_in_msg TYPE string." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" with the ##NEEDED pragma, an unused variable will be kept and no TODO added;" 
			+ LINE_SEP + "    \" the pragma also prevents a warning from the Extended Check (SLIN)" 
			+ LINE_SEP + "    DATA lv_unused_but_needed TYPE string ##NEEDED." 
			+ LINE_SEP 
			+ LINE_SEP + "    FIELD-SYMBOLS:" 
			+ LINE_SEP + "      <ls_unused_a>      TYPE ty_s_structure," 
			+ LINE_SEP + "      <ls_commented_out> LIKE LINE OF its_table," 
			+ LINE_SEP + "      <ls_used>          TYPE tt_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ev_count." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the following variable assigned but unused; however, the call may have side effects" 
			+ LINE_SEP + "    DATA(lo_unused_util) = get_utility( ). " 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_used_var = 0." 
			+ LINE_SEP + "    DO lc_used_constant TIMES." 
			+ LINE_SEP + "      LOOP AT its_table ASSIGNING <ls_used>." 
			+ LINE_SEP + "        lv_used_var += <ls_used>-num." 
			+ LINE_SEP + "      ENDLOOP." 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP + "    ev_count = lv_used_var." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" lv_only_assigned is only used to modifying its own value" 
			+ LINE_SEP + "    CLEAR lv_only_assigned." 
			+ LINE_SEP + "    lv_only_assigned = lv_only_assigned + 2." 
			+ LINE_SEP + "    MULTIPLY lv_only_assigned BY 2." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" without INTO, MESSAGE would interrupt the program flow; therefore," 
			+ LINE_SEP + "    \" this variable is needed for the intended behavior even if it is never used" 
			+ LINE_SEP + "    MESSAGE e123(any_message) INTO DATA(lv_message)." 
			+ LINE_SEP + "    MESSAGE e456(other_message) WITH 'any' 'text' INTO lv_assigned_in_msg." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_assigned_but_used_incomment = 1." 
			+ LINE_SEP 
			+ LINE_SEP + "*    this comment mentions the variable lv_unused_a, but that does not count:" 
			+ LINE_SEP + "*    only variables in commented-out ABAP code count, not variables in text comments!" 
			+ LINE_SEP + "*" 
			+ LINE_SEP + "*    lv_commented_out = 0." 
			+ LINE_SEP + "*    DO lc_commented_out * lv_assigned_but_used_incomment TIMES." 
			+ LINE_SEP + "*      LOOP AT its_any_table ASSIGNING <ls_commented_out>." 
			+ LINE_SEP + "*        lv_commented_out += 1." 
			+ LINE_SEP + "*      ENDLOOP." 
			+ LINE_SEP + "*    ENDDO." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private static final String[] actionTexts = new String[] { "delete", "comment out with *", "comment out with \"", "add TODO comment", "ignore" };
	private static final String[] actionTextsIfAssigned = new String[] { "add TODO comment", "ignore" };
	private static final String[] actionTextsForMessageInto = new String[] { "add pragma ##NEEDED", "add TODO comment", "ignore" };

	final ConfigEnumValue<UnusedVariableAction> configActionForVarsNeverUsed = new ConfigEnumValue<UnusedVariableAction>(this, "MeasureForVarsNeverUsed", "Action for local variables that are never used:", actionTexts, UnusedVariableAction.values(), UnusedVariableAction.DELETE);
	final ConfigEnumValue<UnusedVariableAction> configActionForVarsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableAction>(this, "MeasureForVarsOnlyUsedInComment", "Action for variables only used in commented-out code:", actionTexts, UnusedVariableAction.values(), UnusedVariableAction.COMMENT_OUT_WITH_ASTERISK);
	final ConfigEnumValue<UnusedVariableActionIfAssigned> configActionForAssignedVars = new ConfigEnumValue<UnusedVariableActionIfAssigned>(this, "MeasureForAssignedVars", "Action for assigned but unused local variables:", actionTextsIfAssigned, UnusedVariableActionIfAssigned.values(), UnusedVariableActionIfAssigned.ADD_TODO_COMMENT);
	final ConfigEnumValue<UnusedVariableActionIfAssigned> configActionForAssignedVarsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableActionIfAssigned>(this, "MeasureForAssignedVarsOnlyUsedInComment", "Action for assigned variables only used in commented-out code:", actionTextsIfAssigned, UnusedVariableActionIfAssigned.values(), UnusedVariableActionIfAssigned.ADD_TODO_COMMENT);
	final ConfigEnumValue<UnusedVariableActionForMessageInto> configActionForVarsAssignedInMessageInto = new ConfigEnumValue<UnusedVariableActionForMessageInto>(this, "MeasureForVarsOnlyAssignedInMessageInto", "Action for variables only assigned in MESSAGE ... INTO:", actionTextsForMessageInto, UnusedVariableActionForMessageInto.values(), UnusedVariableActionForMessageInto.ADD_PRAGMA_NEEDED, UnusedVariableActionForMessageInto.ADD_TODO_COMMENT, LocalDate.of(2024, 4, 11));
	final ConfigEnumValue<UnusedVariableAction> configActionForConstantsNeverUsed = new ConfigEnumValue<UnusedVariableAction>(this, "MeasureForConstantsNeverUsed", "Action for local constants that are never used:", actionTexts, UnusedVariableAction.values(), UnusedVariableAction.COMMENT_OUT_WITH_ASTERISK);
	final ConfigEnumValue<UnusedVariableAction> configActionForConstantsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableAction>(this, "MeasureForConstantsOnlyUsedInComment", "Action for constants only used in commented-out code:", actionTexts, UnusedVariableAction.values(), UnusedVariableAction.COMMENT_OUT_WITH_ASTERISK);
 
	private final ConfigValue[] configValues = new ConfigValue[] { configActionForVarsNeverUsed, configActionForVarsOnlyUsedInComment, configActionForAssignedVars, configActionForAssignedVarsOnlyUsedInComment, configActionForVarsAssignedInMessageInto,
			configActionForConstantsNeverUsed, configActionForConstantsOnlyUsedInComment };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	// TODO: create configuration options for prefix and suffix?
	private static final String prefix = "TODO: ";
	private static final String suffix = " (" + Program.PRODUCT_NAME + ")";
	private static final String varAssignedButOnlyUsedInCommentMsg = prefix + "variable is assigned but only used in commented-out code" + suffix;
	private static final String varAssignedButNeverUsedAddPragmaMsg = prefix + "variable is assigned but never used; add pragma ##NEEDED" + suffix;
	private static final String varAssignedButNeverUsedMsg = prefix + "variable is assigned but never used" + suffix;
	private static final String varOnlyUsedInCommentMsg = prefix + "variable is only used in commented-out code" + suffix;
	private static final String constOnlyUsedInCommentMsg = prefix + "constant is only used in commented-out code" + suffix;
	private static final String varNeverUsedMsg = prefix + "variable is never used" + suffix;
	private static final String constNeverUsedMsg = prefix + "constant is never used" + suffix;

	public static final String[] varMessages = new String[] { ABAP.COMMENT_SIGN_STRING + " " + varAssignedButOnlyUsedInCommentMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varAssignedButNeverUsedAddPragmaMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varAssignedButNeverUsedMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varOnlyUsedInCommentMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varNeverUsedMsg };
	
	public static final String[] constMessages = new String[] { ABAP.COMMENT_SIGN_STRING + " " + constOnlyUsedInCommentMsg, 
																					 ABAP.COMMENT_SIGN_STRING + " " + constNeverUsedMsg };
	
	public UnusedVariablesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// even if there are no local variables, remove obsolete comments that were generated in previous runs of this cleanup rule 
		// but are no more attached to a declaration
		try {
			removeObsoleteComments(code, methodStart);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
		
		// skip this method if macros or dynamic ASSIGN are used inside the method to avoid deletion of variables 
		// that are used inside the macros or dynamically (note that macro code may be local or 'out of sight')
		if (localVariables.isEmpty() || localVariables.getMethodUsesMacrosOrTestInjection() || localVariables.getMethodUsesDynamicAssign())
			return;

		for (VariableInfo varInfo : localVariables.getLocalsInDeclarationOrder()) {
			// TYPES declarations are currently not processed by this rule
			if (varInfo.isParameter() || varInfo.isType)
				continue;
			
			Command command = varInfo.declarationToken.getParentCommand();
			// .isBlocked must be considered only at this point, NOT earlier when usage information is gathered 
			if (isCommandBlocked(command)) 
				continue;
			commandForErrorMsg = command;

			// determine the action to be taken; this is NOT dependent on "usedCountInSelfAssignment" (neither in active code nor comment code)
			ChainElementAction action = getAction(varInfo);

			if (varInfo.isUsed() || varInfo.isNeeded() || action == ChainElementAction.ADD_PRAGMA_NEEDED) {
				// if an earlier cleanup produced a comment above the declaration of this variable, remove it (now that the variable or constant is used)
				try {
					if (command.getClosesLevel()) {
						// revert Command.appendCommentToLineOf
						if (command.removeMatchingCommentFromLineOf(varInfo.declarationToken, (varInfo.isConstant ? constMessages : varMessages))) {
							code.addRuleUse(this, command);
						}
					} else {
						// revert Command.putCommentAboveLineOf
						if (command.removeMatchingCommentAboveLineOf(varInfo.declarationToken, (varInfo.isConstant ? constMessages : varMessages))) {
							code.addRuleUse(this, command);
						}
					}
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				}
			}
			if (varInfo.isUsed() || varInfo.isNeeded()) {
				continue;
			}
			
			String message = (action == ChainElementAction.ADD_TODO_COMMENT) ? getMessage(varInfo, command) : null;
			try {
				if (command.handleChainElement(varInfo.declarationToken, action, message)) {
					code.addRuleUse(this, command);
				}
			} catch(UnexpectedSyntaxException ex) {
				throw new UnexpectedSyntaxAfterChanges(this, ex);
			}
		}
	}

	private void removeObsoleteComments(Code code, Command methodStart) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges {
		// remove obsolete comments (both comment lines and comment Tokens at line end) that are no more attached to a declaration
		
		Command command = methodStart;
		while (command != null && command != methodStart.getNextSibling()) {
			Command nextCommand = command.getNext();
			if (isCommandBlocked(command))  {
				// skip this Command
				
			} else if (command.isCommentLine() ) {
				// does the comment line match a message generated by this rule?
				Token firstToken = command.getFirstToken();
				if (firstToken.textEqualsAny(constMessages) || firstToken.textEqualsAny(varMessages)) {
					// remove the comment from the Code, unless it is attached to a declaration
					if (!isAttachedToDeclaration(command)) {
						if (nextCommand.getFirstToken().setLineBreaks(firstToken.lineBreaks)) {
							code.addRuleUse(this, nextCommand);
						}
						command.removeFromCode();
					}
				}

			} else if (!command.isDeclaration()) {
				// does a (full line or line-end) comment Token match a message generated by this rule?
				Token token = command.getFirstToken();
				while (token != null) {
					Token nextToken = token.getNext();
					if (token.isComment() && (token.textEqualsAny(constMessages) || token.textEqualsAny(varMessages))) {
						// remove the comment from the Command, unless it is attached to a declaration
						if (!isAttachedToDeclaration(token)) {
							if (token.lineBreaks > 0 && token.getNext() != null) {
								token.getNext().setLineBreaks(Math.max(token.lineBreaks, token.getNext().lineBreaks));
							}
							token.removeFromCommand();
							code.addRuleUse(this, command);
						}
					}
					token = nextToken;
				}
			}
			command = nextCommand;
		}
	}

	private boolean isAttachedToDeclaration(Command command) {
		// determine whether the comment is attached to a declaration; this also includes Commands that start with an 
		// inline declaration "DATA(...) = ..." etc.
		Command test = command.getNext();
		while (test != null && test.getFirstTokenLineBreaks() == 1 && test.isCommentLine()) {
			test = test.getNext();
		}
		return (test != null && test.getFirstTokenLineBreaks() == 1 && (test.isDeclaration() || test.firstCodeTokenOpensInlineDeclaration()));
	}

	private boolean isAttachedToDeclaration(Token token) {
		if (token.lineBreaks == 0) {
			// for line-end comments, search for inline declarations before the token
			Token test = token.getPrev();
			while (test != null) {
				if (test.opensInlineDeclaration()) 
					return true;
				if (test.lineBreaks > 0) 
					break;
				test = test.getPrev();
			}
			return false;
			
		} else {
			// for comment lines within the Command, search for inline declarations in the line that directly follows the token
			Token test = token.getNext();
			if (test != null && test.lineBreaks > 1)
				return false;
			while (test != null) {
				if (test.opensInlineDeclaration())
					return true;
				test = test.getNext();
				if (test != null && test.lineBreaks > 0) {
					return false;
				}
			}
			return false;
		}
	}

	private ChainElementAction getAction(VariableInfo varInfo) {
		ChainElementAction action;

		if (varInfo.isUsed() || varInfo.isNeeded()) {
			action = ChainElementAction.IGNORE; 
				
		} else if (varInfo.isAssigned()) {
			if (varInfo.isUsedInComment()) {
				action = UnusedVariableActionIfAssigned.forValue(configActionForAssignedVarsOnlyUsedInComment.getValue()).getCorrespondingChainElementAction();
			} else if (varInfo.isAssignedInMessageInto()) {
				action = UnusedVariableActionForMessageInto.forValue(configActionForVarsAssignedInMessageInto.getValue()).getCorrespondingChainElementAction();	
			} else {
				action = UnusedVariableActionIfAssigned.forValue(configActionForAssignedVars.getValue()).getCorrespondingChainElementAction();	
			}
		
		} else if (varInfo.isUsedInComment() || varInfo.isAssignedInComment()) {
			if (varInfo.isConstant) {
				action = UnusedVariableAction.forValue(configActionForConstantsOnlyUsedInComment.getValue()).getCorrespondingChainElementAction();
			} else {
				action = UnusedVariableAction.forValue(configActionForVarsOnlyUsedInComment.getValue()).getCorrespondingChainElementAction();
			}

		} else {
			if (varInfo.isConstant) {
				action = UnusedVariableAction.forValue(configActionForConstantsNeverUsed.getValue()).getCorrespondingChainElementAction();
			} else {
				action = UnusedVariableAction.forValue(configActionForVarsNeverUsed.getValue()).getCorrespondingChainElementAction();
			}
		}
		
		// the declaration can NOT be commented out or deleted if the variable is ...
		// - declared inline with DATA(...) or FIELD-SYMBOL(...)
		// - declared with DATA/CONSTANTS/STATICS BEGIN OF ... (bound structure data)
		if (varInfo.isDeclaredInline || varInfo.isBoundStructuredData) {
			if (action != ChainElementAction.IGNORE && action != ChainElementAction.ADD_PRAGMA_NEEDED) { 
				action = ChainElementAction.ADD_TODO_COMMENT;
			}
		}
		return action;
	}
	
	private String getMessage(VariableInfo varInfo, Command command) { 
		if (varInfo.isAssigned()) {
			if (varInfo.isUsedInComment()) {
				return varAssignedButOnlyUsedInCommentMsg;
			} else if (varInfo.isAssignedInMessageInto()) {
				return varAssignedButNeverUsedAddPragmaMsg;
			} else {
				return varAssignedButNeverUsedMsg;
			}
		} else if (varInfo.isUsedInComment() || varInfo.isAssignedInComment()) {
			return (varInfo.isConstant ? constOnlyUsedInCommentMsg : varOnlyUsedInCommentMsg);
		} else {
			return (varInfo.isConstant ? constNeverUsedMsg : varNeverUsedMsg);
		}
	}

}
