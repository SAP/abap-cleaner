package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.TriState;
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rules.alignment.AlignParametersRule;

public class CondenseRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs"), 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "CONDENSE", "abapcondense.htm"), 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "string_func - condense", "abencondense_functions.htm") };

	@Override
	public RuleID getID() { return RuleID.CONDENSE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace CONDENSE with string function"; }

	@Override
	public String getDescription() { return "Replaces the CONDENSE statement with the string processing function condense( )."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 3, 22); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_ASSIGNMENTS, RuleID.ALIGN_PARAMETERS } ; }

	// getRequiredAbapRelease() not required, as these built-in functions were introduced with ABAP release 7.02 (= 7.0, EhP2) 
	
	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_condense." 
			+ LINE_SEP + "    TYPES: BEGIN OF ty_s_any_struc,"
			+ LINE_SEP + "             field TYPE c LENGTH 10,"
			+ LINE_SEP + "           END OF ty_s_any_struc."
			+ LINE_SEP + ""
			+ LINE_SEP + "    CONSTANTS lc_abc_with_gaps TYPE string VALUE `  a   b   c  `." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    DATA lv_text_a      TYPE char30 VALUE lc_abc_with_gaps." 
			+ LINE_SEP + "    DATA lv_text_b      TYPE char30 VALUE lc_abc_with_gaps." 
			+ LINE_SEP + "    DATA lv_text_c      TYPE char30 VALUE lc_abc_with_gaps." 
			+ LINE_SEP + "    DATA lv_string_a    TYPE string VALUE lc_abc_with_gaps." 
			+ LINE_SEP + "    DATA lv_string_b    TYPE string VALUE lc_abc_with_gaps." 
			+ LINE_SEP + "    DATA ls_structure   TYPE ty_s_any_struc."
			+ LINE_SEP + "    DATA l_unknown_type TYPE if_any_interface=>ty_unknown_type."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" condense first text field to 'a b c', second one to 'abc'"
			+ LINE_SEP + "    CONDENSE lv_text_a." 
			+ LINE_SEP + "    CONDENSE lv_text_b NO-GAPS." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" condense first string to 'a b c', second one to 'abc'"
			+ LINE_SEP + "    CONDENSE lv_string_a." 
			+ LINE_SEP + "    CONDENSE lv_string_b NO-GAPS." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" condense text field with offset 5 and length 7 to `  a  b c`"
			+ LINE_SEP + "    \" (specifying offset and length in write positions is possible for text fields, but not for strings)"
			+ LINE_SEP + "    CONDENSE lv_text_c+5(7)."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" unlike CONDENSE, the string function condense( ) does not work on structured data; therefore,"
			+ LINE_SEP + "    \" changing the next statements might cause syntax errors. You can activate the option"
			+ LINE_SEP + "    \" 'Only replace CONDENSE for known unstructured types' to restrict this cleanup rule"
			+ LINE_SEP + "    \" to cases in which " + Program.PRODUCT_NAME + " can clearly determine the type (as above)"
			+ LINE_SEP + "    CONDENSE ls_structure."
			+ LINE_SEP + "    CONDENSE l_unknown_type."
			+ LINE_SEP + "  ENDMETHOD.";
   }

   final ConfigBoolValue configSpecifyValName = new ConfigBoolValue(this, "SpecifyValName", "Explicitly specify parameter val = ... even if no other parameters are used", false);
   final ConfigBoolValue configSpecifyDel = new ConfigBoolValue(this, "SpecifyDel", "Explicitly specify parameter del = ` `, except for NO-GAPS", false);
   final ConfigBoolValue configSpecifyFromForNoGaps = new ConfigBoolValue(this, "SpecifyFromForNoGaps", "Explicitly specify parameter from = ` ` for NO-GAPS", true);
   final ConfigBoolValue configKeepParamsOnOneLine = new ConfigBoolValue(this, "KeepParamsOnOneLine", "Keep parameters on one line (see rule '" + AlignParametersRule.DISPLAY_NAME + "', option '" + AlignParametersRule.OPTION_NAME_KEEP_OTHER_ONE_LINERS + "')", false);
	final ConfigBoolValue configSkipUnknownTypes = new ConfigBoolValue(this, "SkipUnknownTypes", "Only replace CONDENSE for known unstructured types (STRING, C, N, CHAR10 etc.)", true, true, LocalDate.of(2024, 11, 9));
	final ConfigInfoValue configUnknownTypeWarning = new ConfigInfoValue(this, "Warning: deactivating this option might lead to syntax errors if your code contains CONDENSE with structured types (but at least the syntax check will immediately show this)", ConfigInfoStyle.WARNING);

   private final ConfigValue[] configValues = new ConfigValue[] { configSpecifyValName, configSpecifyDel, configSpecifyFromForNoGaps, configKeepParamsOnOneLine, configSkipUnknownTypes, configUnknownTypeWarning };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configUnknownTypeWarning) {
			return !configSkipUnknownTypes.getValue();
		} else {
			return true; 
		}
	}

	public CondenseRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Command command = methodStart;
		Command methodEnd = command.getNextSibling();
		while (command != methodEnd) {
			commandForErrorMsg = command;

			if (!isCommandBlocked(command)) {
				try {
					if (executeOnCommand(code, command, localVariables, releaseRestriction)) {
						code.addRuleUse(this, command);
					}
				} catch (UnexpectedSyntaxBeforeChanges ex) {
					// log the error and continue with next command
					ex.addToLog();
				}
			}
			
			command = command.getNext();
		}
	}

	private boolean executeOnCommand(Code code, Command command, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (firstToken == null) {
			return false;
		}
		TriState argHasUnstructuredCharlikeType = localVariables.varHasUnstructuredCharlikeType(firstToken.getNextCodeSibling());
		if (argHasUnstructuredCharlikeType == TriState.FALSE
				|| argHasUnstructuredCharlikeType == TriState.UNKNOWN && configSkipUnknownTypes.getValue()) {
			// skip Command: this variable might represent a structured type, which can be processed with CONDENSE, 
			// but would lead to a syntax error with condense( )
			return false;
		} 

		// expect 'CONDENSE text [NO-GAPS].' without comments or pragmas
		if (!firstToken.matchesOnSiblings(false, "CONDENSE", TokenSearch.ANY_IDENTIFIER, TokenSearch.makeOptional("NO-GAPS"), ".")) {
			return false;
		} 
		
		Token condenseKeyword = firstToken;
		Token identifier = condenseKeyword.getNextCodeSibling();
		Token next = identifier.getNextCodeSibling();
		Token noGapsKeyword = null;
		Token periodToken = null;
		if (next.isPeriod()) {
			periodToken = next;
		} else {
			noGapsKeyword = next;
			periodToken = noGapsKeyword.getNextCodeSibling();
		}
		boolean noGaps = (noGapsKeyword != null);
		if (!identifier.isIdentifier() || !periodToken.isPeriod()) // pro forma
			return false;
		
		int sourceLineNum = identifier.sourceLineNum;

		// insert 'identifier = ' and remove CONDENSE keyword
		Token assignedToIdentifier = Token.createForAbap(condenseKeyword.lineBreaks, condenseKeyword.spacesLeft, identifier.getText(), identifier.type, sourceLineNum);
		condenseKeyword.insertLeftSibling(assignedToIdentifier);
		condenseKeyword.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		condenseKeyword.removeFromCommand();

		// remove NO-GAPS 
		if (noGapsKeyword != null)
			noGapsKeyword.removeFromCommand();
		
		// insert 'condense( ... )' around the identifier
		identifier.setWhitespace();
		identifier.insertParenthesesUpTo(periodToken, "condense(", ")");
		periodToken.setWhitespace(0, 0);
		
		boolean insertParamDel = configSpecifyDel.getValue() && !noGaps;
		boolean insertParamFrom = noGaps && configSpecifyFromForNoGaps.getValue();
		boolean insertParamTo = noGaps;
		boolean keepOnOneLine = configKeepParamsOnOneLine.getValue();
		
		int maxParamNameLength = (insertParamFrom ? "from".length() : "val".length());
		
		// insert 'val =' before the identifier, if specified or needed
		if (configSpecifyValName.getValue() || insertParamDel || insertParamFrom || insertParamTo) {
			int spacesLeft = (keepOnOneLine ? 1 : maxParamNameLength - "val".length() + 1);
			identifier.insertLeftSibling(Token.createForAbap(0, 1, "val", TokenType.IDENTIFIER, sourceLineNum));
			identifier.insertLeftSibling(Token.createForAbap(0, spacesLeft, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		}
		
		// insert further parameters, if specified or needed
		if (insertParamTo) 
			insertParam("to", "``", identifier, maxParamNameLength, keepOnOneLine);
		
		if (insertParamFrom) 
			insertParam("from", "` `", identifier, maxParamNameLength, keepOnOneLine);
		
		if (insertParamDel) 
			insertParam("del", "` `", identifier, maxParamNameLength, keepOnOneLine);

		command.invalidateMemoryAccessType();
		return true;
	}
	
	private void insertParam(String paramName, String actualValue, Token identifier, int maxParamNameLength, boolean keepOnOneLine) throws IntegrityBrokenException {
		int lineBreaks = (keepOnOneLine ? 0 : 1);
		int indent = (keepOnOneLine ? 1 : identifier.getParent().getEndIndexInLine() + 1);
		int spacesLeftOfAssignment = (keepOnOneLine ? 1 : maxParamNameLength - paramName.length() + 1);
		int sourceLineNum = identifier.sourceLineNum;

		identifier.insertRightSibling(Token.createForAbap(0, 1, actualValue, sourceLineNum));
		identifier.insertRightSibling(Token.createForAbap(0, spacesLeftOfAssignment, "=", TokenType.ASSIGNMENT_OP, sourceLineNum)); 
		identifier.insertRightSibling(Token.createForAbap(lineBreaks, indent, paramName, TokenType.IDENTIFIER, sourceLineNum));
	}
}
