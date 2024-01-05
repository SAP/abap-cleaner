package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.LocalVariables;
import com.sap.adt.abapcleaner.rulehelpers.MethodVisibility;
import com.sap.adt.abapcleaner.rulehelpers.VariableAccessType;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;
import com.sap.adt.abapcleaner.rules.emptylines.EmptyLinesWithinMethodsRule;

public class UnusedParametersRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Clear or overwrite EXPORTING reference parameters", "#clear-or-overwrite-exporting-reference-parameters"), 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't clear VALUE parameters", "#dont-clear-value-parameters") };

	@Override
	public RuleID getID() { return RuleID.UNUSED_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Report unused parameters"; }

	@Override
	public String getDescription() { return "Adds TODO comments for parameters that are never used or assigned. Comments are suppressed for parameters declared as ##NEEDED in the method signature."; }

	@Override
	public String getHintsAndRestrictions() { return "Parameters can only be checked if the method signature is found in the same code document. Methods that use any macros are skipped."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 1, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.INSET }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "CLASS cl_report_unused_parameters DEFINITION."
			+ LINE_SEP + "  PUBLIC SECTION."
			+ LINE_SEP + "    METHODS any_method"
			+ LINE_SEP + "      IMPORTING its_any_table        TYPE ty_ts_table"
			+ LINE_SEP + "                iv_unused_but_needed TYPE string ##NEEDED \" suppresses a TODO comment on this parameter"
			+ LINE_SEP + "      EXPORTING ev_count             TYPE i"
			+ LINE_SEP + "      CHANGING  cs_any_struc         TYPE ty_s_struc"
			+ LINE_SEP + "      RETURNING VALUE(rv_result)     TYPE i."
			+ LINE_SEP + ""
			+ LINE_SEP + "  PROTECTED SECTION."
			+ LINE_SEP + "    METHODS other_method"
			+ LINE_SEP + "      IMPORTING iv_any_value     TYPE i"
			+ LINE_SEP + "      EXPORTING et_error         TYPE ty_tt_error"
			+ LINE_SEP + "                VALUE(ev_status) TYPE i  \" this is automatically cleared, because it is passed by value"
			+ LINE_SEP + "      RETURNING VALUE(rv_result) TYPE i. \" the same is always true for RETURNING parameters"
			+ LINE_SEP + ""
			+ LINE_SEP + "  PRIVATE SECTION."
			+ LINE_SEP + "    METHODS third_method"
			+ LINE_SEP + "      IMPORTING iv_any_text      TYPE string"
			+ LINE_SEP + "      EXPORTING ev_any_value     TYPE i"
			+ LINE_SEP + "      CHANGING  cs_any_struc     TYPE ty_s_struc"
			+ LINE_SEP + "      RETURNING VALUE(rv_result) TYPE i."
			+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP + ""
			+ LINE_SEP + ""
			+ LINE_SEP + "CLASS cl_report_unused_parameters IMPLEMENTATION."
			+ LINE_SEP + "  METHOD any_method." 
			+ LINE_SEP + "    CLEAR ev_count." 
			+ LINE_SEP 
			+ LINE_SEP + "*    LOOP AT its_any_table ASSIGNING <ls_any_struc>." 
			+ LINE_SEP + "*      lv_commented_out += 1." 
			+ LINE_SEP + "*    ENDLOOP." 
			+ LINE_SEP + "*" 
			+ LINE_SEP + "*    this comment mentions the parameter cs_any_struc, but that does not count:" 
			+ LINE_SEP + "*    only parameters in commented-out ABAP code count, not parameters in text comments!" 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + ""
			+ LINE_SEP + "  METHOD other_method." 
			+ LINE_SEP + "    \" the next line implicitly assigns the returning parameter" 
			+ LINE_SEP + "    RETURN 42."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + ""
			+ LINE_SEP + "  METHOD third_method." 
			+ LINE_SEP + "    \" This method does not yet contain an executable statement," 
			+ LINE_SEP + "    \" maybe because it was not yet implemented, or because it is obsolete." 
			+ LINE_SEP + "    \" You may therefore not want TODO comments to be added here." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS.";
   }

	private static final String[] scopeTexts = new String[] { "in all methods", "in non-interface methods", "in protected and private methods", "in private methods", "never" };

	final ConfigEnumValue<UnusedParameterScope> configImportingParamScope = new ConfigEnumValue<UnusedParameterScope>(this, "ImportingParamScope", "Report IMPORTING parameters:", scopeTexts, UnusedParameterScope.values(), UnusedParameterScope.NON_INTERFACE_METHODS);
	final ConfigEnumValue<UnusedParameterScope> configExportingParamScope = new ConfigEnumValue<UnusedParameterScope>(this, "ExportingParamScope", "Report EXPORTING parameters:", scopeTexts, UnusedParameterScope.values(), UnusedParameterScope.ALL_METHODS);
	final ConfigEnumValue<UnusedParameterScope> configChangingParamScope = new ConfigEnumValue<UnusedParameterScope>(this, "ChangingParamScope", "Report CHANGING parameters:", scopeTexts, UnusedParameterScope.values(), UnusedParameterScope.NON_INTERFACE_METHODS);
	final ConfigEnumValue<UnusedParameterScope> configReturningParamScope = new ConfigEnumValue<UnusedParameterScope>(this, "ReturningParamScope", "Report RETURNING parameters:", scopeTexts, UnusedParameterScope.values(), UnusedParameterScope.NEVER);
	final ConfigBoolValue configIgnoreEmptyMethods = new ConfigBoolValue(this, "IgnoreEmptyMethods", "Only add TODO comments in methods with executable statements", true);
	final ConfigBoolValue configIgnoreExportingByValue = new ConfigBoolValue(this, "IgnoreExportingByValue", "Do not add TODO comments for EXPORTING VALUE(...) parameters", true);
 
	private final ConfigValue[] configValues = new ConfigValue[] { configImportingParamScope, configExportingParamScope, configChangingParamScope, configReturningParamScope, configIgnoreEmptyMethods, configIgnoreExportingByValue };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private static final String prefix = "TODO: ";
	private static final String suffix = " (" + Program.PRODUCT_NAME + ")";

	// to-do messages for parameters that are not used or assigned
	private static final String parameterPrefix = prefix + "parameter ";
	private static final String paramNeverUsedSuffix = " is never used" + suffix;
	private static final String paramNeverUsedOrAssignedSuffix = " is never used or assigned" + suffix;
	private static final String paramNeverClearedOrAssignedSuffix = " is never cleared or assigned" + suffix;
	private static final String paramNeverAssignedSuffix = " is never assigned" + suffix;
	private static final String paramOnlyUsedInCommentSuffix = " is only used in commented-out code" + suffix;
	private static final String paramOnlyUsedOrAssignedInCommentSuffix = " is only used or assigned in commented-out code" + suffix;
	private static final String paramOnlyAssignedInCommentSuffix = " is only cleared or assigned in commented-out code" + suffix;

	public UnusedParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// nothing to do on class definition level
		return;
	}

	@Override
	protected void executeOn(Code code, Command methodStart, LocalVariables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// skip this method if macros are used inside the method, because parameters may be used inside the macros 
		// (note that macro code may be local or 'out of sight')
		if (localVariables.getMethodUsesMacrosOrTestInjection())
			return;

		// if the method signature is not visible, nothing can be done, and existing to-do comments are kept
		if (localVariables.getMethodInfo() == null)
			return;
		else if (isCommandBlocked(methodStart))
			return;
		
		// depending on configuration, only sallow messages if the method contains at least one executable statement 
		// (otherwise it may be obsolete, not yet implemented, only relevant in child classes etc.)
		boolean allowMessages = true;
		if (configIgnoreEmptyMethods.getValue()) {
			allowMessages = false;
			Command test = methodStart.getNextNonCommentCommand();
			while (test != methodStart.getNextSibling()) {
				if (!test.isDeclaration() && !test.firstCodeTokenIsAnyKeyword("ASSERT", "BREAK-POINT", "LOG-POINT") && !test.isPragmaLine()) {
					allowMessages = true;
					break;
				}
				test = test.getNextNonCommentCommand();
			}
		}
		
		// determine which parameters to create to-do messages for, depending on method visibility
		MethodVisibility methodVisibility = localVariables.getMethodInfo().visibility;
		String methodName = methodStart.getDefinedName();
		boolean isInterfaceMethod = (methodName != null && methodName.contains(ABAP.TILDE_STRING));
		boolean addTodoForImportingParam = methodVisibilityMatches(UnusedParameterScope.forValue(configImportingParamScope.getValue()), methodVisibility, isInterfaceMethod); 
		boolean addTodoForExportingParam = methodVisibilityMatches(UnusedParameterScope.forValue(configExportingParamScope.getValue()), methodVisibility, isInterfaceMethod);
		boolean addTodoForChangingParam = methodVisibilityMatches(UnusedParameterScope.forValue(configChangingParamScope.getValue()), methodVisibility, isInterfaceMethod); 
		boolean addTodoForReturningParam = methodVisibilityMatches(UnusedParameterScope.forValue(configReturningParamScope.getValue()), methodVisibility, isInterfaceMethod);
		// continue below even if both addTodo... are false, because existing to-do comments might have to be removed   
		
		// create a list of messages for method parameters, if these parameters are not used or not assigned
		ArrayList<String> newMessages = new ArrayList<>();
		if (allowMessages && (addTodoForImportingParam || addTodoForExportingParam || addTodoForChangingParam || addTodoForReturningParam)) {
			for (VariableInfo varInfo : localVariables.getLocalsInDeclarationOrder()) {
				if (!varInfo.isParameter() || varInfo.isNeeded())
					continue;
				
				String varName = StringUtil.removePrefix(varInfo.declarationToken.getText(), ABAP.OPERAND_ESCAPE_CHAR_STRING, false);
				String messageSuffix = getParamMessageSuffix(varInfo, addTodoForImportingParam, addTodoForExportingParam, addTodoForChangingParam, addTodoForReturningParam);
	
				if (messageSuffix != null) {
					newMessages.add(parameterPrefix + varName.toUpperCase() + messageSuffix);
				}
			}
		}

		// insert to-do comments into the code, but keep existing comments if they match the new messages (including their order)
		Command insertAt = methodStart.getNext();
		for (String message : newMessages) {
			if (insertAt.isCommentLine() && insertAt.getFirstToken().textEquals(ABAP.COMMENT_SIGN_STRING + " " + message)) {
				// keep the existing message
				insertAt = insertAt.getNext();
			} else {
				Command newComment = insertAt.insertCommentLineAbove(message);
				if (newComment != null) {
					code.addRuleUse(this, newComment);
					code.addRuleUse(this, methodStart);
				}
			}
		}
		
		// remove (remaining unmatched) old to-do comments that match the pattern "TODO: parameter ... (ABAP cleaner)"
		String expPrefix = ABAP.COMMENT_SIGN_STRING + " " + parameterPrefix;
		String expSuffix = suffix;
		boolean wasCommentRemoved = false;
		while (insertAt != null && insertAt.isCommentLine()) {
			String commentText = insertAt.getFirstToken().getText();
			if (!StringUtil.startsWith(commentText, expPrefix, true) || !StringUtil.endsWith(commentText, expSuffix, true))
				break; 
			Command removeCommand = insertAt;
			insertAt = insertAt.getNext();
			try {
				removeCommand.removeFromCode();
				code.addRuleUse(this, methodStart);
				wasCommentRemoved = true;
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
		}
		
		if (newMessages.size() > 0 && insertAt.getFirstToken().lineBreaks < 2 && insertAt != methodStart.getNextSibling()) {
			// add empty line below messages (but not above ENDMETHOD)
			insertAt.getFirstToken().setLineBreaks(2);
			code.addRuleUse(this, insertAt);

		} else if (wasCommentRemoved && newMessages.isEmpty() && insertAt.getPrev() == methodStart && !methodStart.containsInnerLineBreaks(true)) {
			// remove empty line if all (old) messages were removed and EmptyLinesWithinMethodsRule is configured to remove empty lines at method start
			EmptyLinesWithinMethodsRule emptyLinesWithinMethodsRule = (EmptyLinesWithinMethodsRule)parentProfile.getRule(RuleID.EMPTY_LINES_WITHIN_METHODS);
			if (emptyLinesWithinMethodsRule.isActive && emptyLinesWithinMethodsRule.getMaxEmptyLinesAtMethodStart() == 0) {
				if (insertAt.getFirstToken().setLineBreaks(1)) {
					code.addRuleUse(this, insertAt);
				}
			}
		}
	}

	private boolean methodVisibilityMatches(UnusedParameterScope scope, MethodVisibility methodVisibility, boolean isInterfaceMethod) {
		switch(scope) {
			case ALL_METHODS:
				return true;
			case NON_INTERFACE_METHODS:
				return !isInterfaceMethod;
			case PROTECTED_OR_PRIVATE:
				return (methodVisibility == MethodVisibility.PROTECTED || methodVisibility == MethodVisibility.PRIVATE);
			case PRIVATE_ONLY:
				return (methodVisibility == MethodVisibility.PRIVATE);
			default: // NEVER
				return false;
		}
	}

	private String getParamMessageSuffix(VariableInfo varInfo, boolean addTodoForImportingParam, boolean addTodoForExportingParam, boolean addTodoForChangingParam, boolean addTodoForReturningParam) {
		if (varInfo.accessType == VariableAccessType.IMPORTING) {
			if (addTodoForImportingParam && !varInfo.isUsed()) {
				return varInfo.isUsedInComment() ? paramOnlyUsedInCommentSuffix : paramNeverUsedSuffix;
			}

		} else if (varInfo.accessType == VariableAccessType.EXPORTING) {
			if (configIgnoreExportingByValue.getValue() && varInfo.isParameterByValue()) {
				return null;
			} else if (addTodoForExportingParam && !varInfo.isAssigned()) { 
				return varInfo.isAssignedInComment() ? paramOnlyAssignedInCommentSuffix : (varInfo.isParameterByValue() ? paramNeverAssignedSuffix : paramNeverClearedOrAssignedSuffix);
			}
			
		} else if (varInfo.accessType == VariableAccessType.CHANGING) {
			if (addTodoForChangingParam && !varInfo.isUsed() && !varInfo.isAssigned()) { 
				return varInfo.isUsedInComment() || varInfo.isAssignedInComment() ? paramOnlyUsedOrAssignedInCommentSuffix : paramNeverUsedOrAssignedSuffix;
			}
		
		} else if (varInfo.accessType == VariableAccessType.RETURNING) { 
			if (addTodoForReturningParam && !varInfo.isAssigned()) { 
				return varInfo.isAssignedInComment() ? paramOnlyAssignedInCommentSuffix : paramNeverAssignedSuffix;
			}
		}
		return null;
	}
}
