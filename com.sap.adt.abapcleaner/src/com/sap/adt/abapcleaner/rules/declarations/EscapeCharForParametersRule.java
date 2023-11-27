package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class EscapeCharForParametersRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ESCAPE_CHAR_FOR_PARAMS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Standardize escaping of !parameters"; }

	@Override
	public String getDescription() { return "Removes or adds the ! escape character for parameter names in method declarations, making its usage independent from which IDE the method was created with."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 6, 20); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_METHODS_DECLARATION } ; }

   @Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS cl_escape_char_for_params DEFINITION." 
 			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP + "    METHODS:" 
			+ LINE_SEP + "      \" SAP GUI creates method declarations with ! before each parameter name,"
			+ LINE_SEP + "      \" but not before VALUE(...), REFERENCE(...) or in the RAISING / EXCEPTIONS section:" 
			+ LINE_SEP + "      any_chained_method" 
			+ LINE_SEP + "        IMPORTING" 
			+ LINE_SEP + "          !it_source_table  TYPE ty_tt_any OPTIONAL" 
			+ LINE_SEP + "          !iv_name          TYPE string" 
			+ LINE_SEP + "        EXPORTING" 
			+ LINE_SEP + "          !et_error_table   TYPE ty_tt_any" 
			+ LINE_SEP + "          !ev_value         TYPE i" 
			+ LINE_SEP + "        CHANGING" 
			+ LINE_SEP + "          !cts_result_table TYPE ty_ts_any," 
			+ LINE_SEP + ""  
			+ LINE_SEP + "      \" ABAP Development Tools creates method declarations without escape characters:" 
			+ LINE_SEP + "      other_chained_method" 
			+ LINE_SEP + "        IMPORTING" 
			+ LINE_SEP + "          iv_any_param     TYPE i      OPTIONAL" 
			+ LINE_SEP + "          iv_other_param   TYPE string DEFAULT 'abc'" 
			+ LINE_SEP + "        RETURNING" 
			+ LINE_SEP + "          VALUE(rv_result) TYPE char10" 
			+ LINE_SEP + "        RAISING" 
			+ LINE_SEP + "          cx_any_exception,"  
			+ LINE_SEP + ""  
			+ LINE_SEP + "      \" the escape char ! is recommendable if the parameter name is also an ABAP word:" 
			+ LINE_SEP + "      third_chained_method" 
			+ LINE_SEP + "        IMPORTING" 
			+ LINE_SEP + "          min    TYPE i DEFAULT 0" 
			+ LINE_SEP + "          max    TYPE i DEFAULT 100" 
			+ LINE_SEP + "          sum    TYPE i" 
			+ LINE_SEP + "        EXPORTING" 
			+ LINE_SEP + "          output TYPE string."  
			+ LINE_SEP + ""  
			+ LINE_SEP + "    \" in some rather theoretical cases, ! is strictly necessary to prevent syntax errors:" 
			+ LINE_SEP + "    METHODS fourth_method" 
			+ LINE_SEP + "      IMPORTING" 
			+ LINE_SEP + "        !optional   TYPE i" 
			+ LINE_SEP + "        !default    TYPE i" 
			+ LINE_SEP + "        !preferred  TYPE i OPTIONAL" 
			+ LINE_SEP + "        !exporting  TYPE i OPTIONAL" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        !changing   TYPE string" 
			+ LINE_SEP + "      CHANGING" 
			+ LINE_SEP + "        !raising    TYPE i"  
			+ LINE_SEP + "        !exceptions TYPE string."  
			+ LINE_SEP + "ENDCLASS."; 
   }

	private static final String[] changeTypeSelection = new String[] { "always", "keep as is", "only if parameter name is an ABAP word", "only to avoid syntax errors" };
	private static final String[] criticalParamNames = new String[] { "EXPORTING", "CHANGING", "RAISING", "EXCEPTIONS", "DEFAULT", "OPTIONAL", "PREFERRED" };
	
	final ConfigEnumValue<EscapeCharAction> configUseEscapeCharForParams = new ConfigEnumValue<EscapeCharAction>(this, "UseEscapeCharForParams", "Use ! escape character for parameters", changeTypeSelection, EscapeCharAction.values(), EscapeCharAction.ONLY_FOR_ABAP_WORDS, EscapeCharAction.ONLY_FOR_ABAP_WORDS, LocalDate.of(2023, 5, 30));

	private final ConfigValue[] configValues = new ConfigValue[] { configUseEscapeCharForParams };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EscapeCharForParametersRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!command.isInClassDefinition() && !command.isInInterfaceDefinition())
			return false;
		if (!command.firstCodeTokenIsAnyKeyword("METHODS", "CLASS-METHODS"))
			return false;

		EscapeCharAction useEscapeChar = EscapeCharAction.forValue(configUseEscapeCharForParams.getValue());
		if (useEscapeChar == EscapeCharAction.KEEP_AS_IS)
			return false;

		boolean changedCommand = false;
		Token token = command.getFirstCodeToken();
		boolean isMethodForEvent = false;
		while (token != null) {
			if (token.isKeyword()) {
				if (token.matchesOnSiblings(true, "FOR EVENT")) {
					isMethodForEvent = true;
				
				} else if (isMethodForEvent && token.isKeyword("IMPORTING")) {
					// after 'METHODS ... FOR EVENT ... IMPORTING', parameter names are listed without TYPE
					do {
						Token paramName = token.getNextCodeSibling();
						if (paramName == null || !paramName.isIdentifier()) 
							break;
						if (executeOnParameterName(paramName, useEscapeChar)) 
							changedCommand = true;
						token = paramName;
					} while(true);
					
				} else if (token.isAnyKeyword("TYPE", "LIKE")) {
					if (executeOnParameterName(token.getPrevCodeSibling(), useEscapeChar)) {
						changedCommand = true;
					}
				} 
			} else if (token.isCommaOrPeriod()) {
				isMethodForEvent = false;
			}
			token = token.getNextCodeSibling();
		}

		return changedCommand;
	}
	
	private boolean executeOnParameterName(Token paramName, EscapeCharAction useEscapeChar) {
		final String ESCAPE_CHAR = ABAP.OPERAND_ESCAPE_CHAR_STRING;
		
		boolean isValueOrReference = false;
		boolean isAbapWord = false;
		boolean requiresEscapeChar = false;
		if (paramName.closesLevel()) { // VALUE(...), REFERENCE(...)
			paramName = paramName.getPrevCodeSibling();
			isValueOrReference = true;
		} else if (paramName.isIdentifier()) {
			String paramNameText = StringUtil.removePrefix(paramName.getText(), ESCAPE_CHAR, false);
			isAbapWord = ABAP.isAbapUpperCaseKeyword(paramNameText) || ABAP.isAbapLowerCaseKeyword(paramNameText);
			requiresEscapeChar = AbapCult.stringEqualsAny(true, paramNameText, criticalParamNames);
		}

		if (paramName.textStartsWith(ESCAPE_CHAR) && !requiresEscapeChar) { 
			// remove escape char if a) configured or b) in case of VALUE(...), REFERENCE(...), where SAP GUI does not use ! either
			if (	 isValueOrReference 
				 || useEscapeChar == EscapeCharAction.ONLY_FOR_ABAP_WORDS && !isAbapWord 
				 || useEscapeChar == EscapeCharAction.ONLY_AVOID_ERRORS && !requiresEscapeChar) {
				paramName.setText(paramName.getText().substring(ESCAPE_CHAR.length()), false);
				return true;
			}

		} else if (!paramName.textStartsWith(ESCAPE_CHAR) && !isValueOrReference) {
			// add escape char if a) needed to avoid syntax errors or b) configured
			if (  requiresEscapeChar
			   || useEscapeChar == EscapeCharAction.ALWAYS 
			   || useEscapeChar == EscapeCharAction.ONLY_FOR_ABAP_WORDS && isAbapWord
			   || useEscapeChar == EscapeCharAction.ONLY_AVOID_ERRORS && requiresEscapeChar) {
				paramName.setText(ESCAPE_CHAR + paramName.getText(), false);
				return true;
			}
		} 
			
		return false;
	}
}
