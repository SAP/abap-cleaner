package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AbapDoc;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.ExceptionInfo;
import com.sap.adt.abapcleaner.rulehelpers.MethodInfo;
import com.sap.adt.abapcleaner.rulehelpers.ParameterInfo;

public class AbapDocParametersRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ABAP_DOC_PARAMETERS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Add missing parameters to ABAP Doc"; }

	@Override
	public String getDescription() { return "Adds missing parameters and exceptions to existing ABAP Doc comments and updates their order."; }

	@Override
	public String getHintsAndRestrictions() { return "No ABAP Doc comments are added for exceptions of FOR TESTING methods."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 6, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ABAP_DOC_LANG, RuleID.ALIGN_ABAP_DOC, RuleID.UPPER_AND_LOWER_CASE, RuleID.INSET } ; }

	@Override
   public String getExample() {
      return "" 
         + LINE_SEP + "CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC."
         + LINE_SEP + "  PUBLIC SECTION."
			+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\">any method documentation (synchronized)</p>" 
			+ LINE_SEP + "    \"!" 
			+ LINE_SEP + "    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter documentation</p>" 
			+ LINE_SEP + "    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">exception documentation</p>" 
			+ LINE_SEP + "    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception which was deleted</p>" 
			+ LINE_SEP + "    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>" 
			+ LINE_SEP + "    METHODS any_method" 
			+ LINE_SEP + "      IMPORTING iv_any_parameter   TYPE i" 
			+ LINE_SEP + "                iv_other_parameter TYPE i" 
			+ LINE_SEP + "      RAISING   cx_any_exception" 
			+ LINE_SEP + "                cx_other_exception." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "    METHODS:" 
			+ LINE_SEP + "      \"! other method documentation (not synchronized)" 
			+ LINE_SEP + "      \"!" 
			+ LINE_SEP + "      \"! @parameter iv_second_parameter | second parameter documentation" 
			+ LINE_SEP + "      \"! @parameter iv_first_parameter | first parameter documentation" 
			+ LINE_SEP + "      \"! @raising cx_other_exception | other exception documentation" 
			+ LINE_SEP + "      other_method" 
			+ LINE_SEP + "        IMPORTING iv_first_parameter  TYPE c" 
			+ LINE_SEP + "                  iv_second_parameter TYPE c" 
			+ LINE_SEP + "        EXPORTING ev_third_parameter  TYPE c" 
			+ LINE_SEP + "        RAISING   cx_any_exception" 
			+ LINE_SEP + "                  cx_other_exception," 
			+ LINE_SEP 
			+ LINE_SEP + "      \"! other method documentation (not synchronized)" 
			+ LINE_SEP + "      \"!" 
			+ LINE_SEP + "      \"! @parameter iv_obsolete_parameter | obsolete parameter which was deleted" 
			+ LINE_SEP + "      \"! @parameter iv_also_obsolete  |" 
			+ LINE_SEP + "      third_method," 
			+ LINE_SEP 
			+ LINE_SEP + "      \"! <p class=\"shorttext synchronized\" lang=\"en\">fourth method documentation (synchronized with lang)</p>" 
			+ LINE_SEP + "      fourth_method" 
			+ LINE_SEP + "        IMPORTING  iv_param_a TYPE i" 
			+ LINE_SEP + "                   iv_param_b TYPE i" 
			+ LINE_SEP + "        EXCEPTIONS any_exception." 
			+ LINE_SEP + "ENDCLASS.";
   }

	private final String[] addDocTexts = new String[] { "Always (CAUTION: discouraged if SAP GUI is used, too)", "Add non-synchronized line", "Only to non-synchronized ABAP Doc", "Never" };
	private final String[] deleteObsoleteDocText = new String[] { "Always", "Only if description is empty", "Never" };

	final ConfigEnumValue<AddAbapDocType> configAddParameters = new ConfigEnumValue<AddAbapDocType>(this, "AddParameters", "Add missing parameters to ABAP Doc", addDocTexts, AddAbapDocType.values(), AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED);
	final ConfigEnumValue<AddAbapDocType> configAddExceptions = new ConfigEnumValue<AddAbapDocType>(this, "AddExceptions", "Add missing exceptions to ABAP Doc", addDocTexts, AddAbapDocType.values(), AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED);
	final ConfigInfoValue configAddAlwaysWarning = new ConfigInfoValue(this, "CAUTION: option 'Always' may delete synchronized descriptions that were added with SAP GUI", ConfigInfoStyle.WARNING);
	final ConfigBoolValue configOnlyAddToExistingDetails = new ConfigBoolValue(this, "OnlyAddToExistingDetails", "Only add if at least one parameter or exception is already documented", false);
	final ConfigBoolValue configUpdateOrder = new ConfigBoolValue(this, "Update order", "Update order of parameters and exceptions in ABAP Doc", true);
	final ConfigEnumValue<DeleteObsoleteAbapDocType> configDeleteParameters = new ConfigEnumValue<DeleteObsoleteAbapDocType>(this, "DeleteParameters", "Delete obsolete parameters from ABAP Doc", deleteObsoleteDocText, DeleteObsoleteAbapDocType.values(), DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY);
	final ConfigEnumValue<DeleteObsoleteAbapDocType> configDeleteExceptions = new ConfigEnumValue<DeleteObsoleteAbapDocType>(this, "DeleteExceptions", "Delete obsolete exceptions from ABAP Doc", deleteObsoleteDocText, DeleteObsoleteAbapDocType.values(), DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY);

	private final ConfigValue[] configValues = new ConfigValue[] { configAddParameters, configAddExceptions, configAddAlwaysWarning, configOnlyAddToExistingDetails, configUpdateOrder, configDeleteParameters, configDeleteExceptions };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AbapDocParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configAddAlwaysWarning ) {
			return AddAbapDocType.forValue(configAddParameters.getValue()) == AddAbapDocType.ALWAYS
				 || AddAbapDocType.forValue(configAddExceptions.getValue()) == AddAbapDocType.ALWAYS;
		} else {
			return true;
		}
	}

	@Override
	protected boolean skipLocalVariableContexts() {
		return true;
	}

	@Override
	protected void executeOnClassDefinition(Code code, ClassInfo classOrInterfaceInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		for (MethodInfo methodInfo : classOrInterfaceInfo.getMethodsInOrder()) {
			try {
				// determine whether ABAP Doc exists for this method
				Command command = methodInfo.declarationToken.getParentCommand();
				if (isCommandBlocked(command))
					continue;

				// read ABAP Doc of this method declaration (if any)
				AbapDoc abapDoc = command.isSimpleChain() ? AbapDoc.readFromChain(methodInfo.declarationToken) 
																		: AbapDoc.readFromNonChain(command);
				if (abapDoc.isEmpty()) 
					continue;

				if (executeOn(code, methodInfo, abapDoc)) {
					code.addRuleUse(this, command);
				}
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
		}
	}

	private boolean executeOn(Code code, MethodInfo methodInfo, AbapDoc abapDoc) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges {
		AddAbapDocType addParameters = AddAbapDocType.forValue(configAddParameters.getValue());
		AddAbapDocType addExceptions = AddAbapDocType.forValue(configAddExceptions.getValue());
		boolean onlyAddToExistingDetails = configOnlyAddToExistingDetails.getValue();
		boolean updateOrder = configUpdateOrder.getValue();
		DeleteObsoleteAbapDocType deleteParameters = DeleteObsoleteAbapDocType.forValue(configDeleteParameters.getValue());
		DeleteObsoleteAbapDocType deleteExceptions = DeleteObsoleteAbapDocType.forValue(configDeleteExceptions.getValue());
		
		if (addParameters == AddAbapDocType.NEVER && addExceptions == AddAbapDocType.NEVER && !updateOrder 
		&& deleteParameters == DeleteObsoleteAbapDocType.NEVER && deleteExceptions == DeleteObsoleteAbapDocType.NEVER) {
			return false;
		}
		
		boolean changed = false;
		Token writePos = abapDoc.getTokenAfterHeader();
		
		// enhance and reorder parameters
		for (ParameterInfo parameterInfo : methodInfo.getParametersInOrder()) {
			if (abapDoc.hasParameter(parameterInfo.name)) {
				AbapDoc.AbapDocSection paramDoc = abapDoc.getParameter(parameterInfo.name);
				if (updateOrder && paramDoc.firstToken != writePos) {
					abapDoc.moveSectionTo(writePos, paramDoc);
					changed = true;
				}
				// put the next parameter behind this one (even if this parameter was not moved to the correct position)
				writePos = abapDoc.getNextToken(paramDoc.lastToken);
			} else if (addParameters != AddAbapDocType.NEVER && (!abapDoc.isSynchronized() || addParameters != AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED)
					 && (abapDoc.hasAnyParameterOrException() || !onlyAddToExistingDetails)) {
				abapDoc.insertParameterDocBefore(writePos, parameterInfo.name, (addParameters == AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED));
				changed = true;
			}
		}

		// enhance and reorder exceptions
		for (ExceptionInfo exceptionInfo : methodInfo.getExceptionsInOrder()) {
			if (abapDoc.hasException(exceptionInfo.name)) {
				AbapDoc.AbapDocSection exceptionDoc = abapDoc.getException(exceptionInfo.name);
				if (updateOrder && exceptionDoc.firstToken != writePos) {
					abapDoc.moveSectionTo(writePos, exceptionDoc);
					changed = true;
				}
				// put the next exception behind this one (even if this exception was not moved to the correct position)
				writePos = abapDoc.getNextToken(exceptionDoc.lastToken);
			} else if (addExceptions != AddAbapDocType.NEVER && (!abapDoc.isSynchronized() || addExceptions != AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED) 
					 && (abapDoc.hasAnyParameterOrException() || !onlyAddToExistingDetails) && !methodInfo.isForTesting) {
				if (exceptionInfo.isClassBased) {
					abapDoc.insertRaisingDocBefore(writePos, exceptionInfo.name, (addExceptions == AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED));
				} else {
					abapDoc.insertExceptionDocBefore(writePos, exceptionInfo.name, (addExceptions == AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED));
				}
				changed = true;
			}
		}

		// delete obsolete parameters
		if (deleteParameters != DeleteObsoleteAbapDocType.NEVER) {
			for (String parameterName : abapDoc.getParameterNames()) {
				if (methodInfo.hasParameter(parameterName)) 
					continue;
				AbapDoc.AbapDocSection abapDocSection = abapDoc.getParameter(parameterName);
				if (abapDocSection == null || deleteParameters == DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY && abapDocSection.hasDescription)
					continue;
				abapDoc.removeSection(abapDocSection);
				changed = true;
			}
		}

		// delete obsolete exceptions
		if (deleteExceptions != DeleteObsoleteAbapDocType.NEVER) {
			for (String exceptionName : abapDoc.getExceptionNames()) {
				if (methodInfo.hasException(exceptionName)) 
					continue;
				AbapDoc.AbapDocSection abapDocSection = abapDoc.getException(exceptionName);
				if (abapDocSection == null || deleteExceptions == DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY && abapDocSection.hasDescription)
					continue;
				abapDoc.removeSection(abapDocSection);
				changed = true;
			}
		}

		return changed;
	}
}
