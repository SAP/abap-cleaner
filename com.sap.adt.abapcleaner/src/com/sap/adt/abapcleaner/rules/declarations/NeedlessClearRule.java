package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
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
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;

public class NeedlessClearRule extends RuleForDeclarations {
   private final static RuleReference[] references = new RuleReference[] {new RuleReference(RuleSource.ABAP_CLEANER)};
	private final String commentText = "TODO: remove needless CLEAR (" + Program.PRODUCT_NAME + ")";

	@Override
	public RuleID getID() { return RuleID.NEEDLESS_CLEAR; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Remove needless CLEAR"; }

	@Override
	public String getDescription() { return "Removes needless CLEAR of local variables from method start and method end."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 10, 25); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.LOCAL_DECLARATION_ORDER, RuleID.CHAIN_OF_ONE, RuleID.UNUSED_VARIABLES, RuleID.INSET, RuleID.UPPER_AND_LOWER_CASE }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD remove_needless_clear." 
			+ LINE_SEP + "    DATA lv_any   TYPE i." 
			+ LINE_SEP + "    DATA lv_other TYPE string." 
			+ LINE_SEP + "    DATA lv_third TYPE i VALUE 3." 
			+ LINE_SEP + "    DATA ls_struc TYPE ty_s_struc." 
			+ LINE_SEP + "    DATA lt_table TYPE ty_tt_any." 
			+ LINE_SEP + "    DATA lr_ref   TYPE REF TO data." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" while exporting parameters should be cleared, local variables automatically" 
			+ LINE_SEP + "    \" get their initial value and therefore do not need CLEAR" 
			+ LINE_SEP + "    CLEAR lv_any." 
			+ LINE_SEP + "    CLEAR: ev_result," 
			+ LINE_SEP + "           lv_other," 
			+ LINE_SEP + "           et_table," 
			+ LINE_SEP + "           lv_third," 
			+ LINE_SEP + "           lt_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" a common case where you may want to keep CLEAR even at method start is this:" 
			+ LINE_SEP + "    CLEAR ls_struc." 
			+ LINE_SEP + "    ls_struc-id   = 1." 
			+ LINE_SEP + "    ls_struc-name = 'abc'." 
			+ LINE_SEP + "    APPEND ls_struc TO et_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ls_struc." 
			+ LINE_SEP + "    ls_struc-id   = 2." 
			+ LINE_SEP + "    ls_struc-name = 'def'." 
			+ LINE_SEP + "    APPEND ls_struc TO et_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    lr_ref = REF #( mt_any_table )." 
			+ LINE_SEP + "    CLEAR lr_ref->*." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" no need to clear local variables at method end, either; however, you may only want to" 
			+ LINE_SEP + "    \" add a TODO comment here, because you may just be in the process of writing the method" 
			+ LINE_SEP + "    CLEAR: lv_other, lv_any." 
			+ LINE_SEP + "    CLEAR: lv_third, lr_ref." 
			+ LINE_SEP + "    CLEAR: lt_table," 
			+ LINE_SEP + "           ev_table." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private static final String[] actionTexts = new String[] { "delete", "add TODO comment", "ignore" };

	final ConfigEnumValue<NeedlessClearAction> configActionAtStart = new ConfigEnumValue<NeedlessClearAction>(this, "ActionAtStart", "Action for CLEAR at method start:", actionTexts, NeedlessClearAction.values(), NeedlessClearAction.DELETE);
	final ConfigEnumValue<NeedlessClearAction> configActionAtEnd = new ConfigEnumValue<NeedlessClearAction>(this, "ActionAtEnd", "Action for CLEAR at method end:", actionTexts, NeedlessClearAction.values(), NeedlessClearAction.ADD_TODO_COMMENT);
	final ConfigBoolValue configKeepStrucBeforeAssign = new ConfigBoolValue(this, "KeepStrucBeforeAssign", "Keep CLEAR for structure that is assigned to directly afterwards", true);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configActionAtStart, configActionAtEnd, configKeepStrucBeforeAssign };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public NeedlessClearRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// skip this method if macros are used inside the method (note that macro code may be local or 'out of sight')
		if (localVariables.isEmpty() || localVariables.getMethodUsesMacrosOrTestInjection() || isCommandBlocked(methodStart))
			return;

		NeedlessClearAction actionAtStart = NeedlessClearAction.forValue(configActionAtStart.getValue());
		NeedlessClearAction actionAtEnd = NeedlessClearAction.forValue(configActionAtEnd.getValue());
		
		Command firstExecutable = null;
		Command lastExecutable = null;
		
		// remove CLEAR from method start, skipping comments, pragmas, declarations and ASSERTs
		Command methodEnd = methodStart.getNextSibling(); // may be null
		Command command = methodStart.getNext();
		while (command != null && command != methodEnd) {
			Command next = command.getNext();
			if (command.firstCodeTokenIsKeyword("CLEAR")) {
				executeOnClear(code, command, localVariables, actionAtStart, true);
			} else if (!command.isCommentLine() && !command.isPragmaLine() && !command.isDeclaration() 
					&& !command.firstCodeTokenIsAnyKeyword("ASSERT", "BREAK-POINT", "LOG-POINT")) {
				firstExecutable = command;
				break;
			}
			command = next;
		}
		// if method end was already reached with no other executable Commands before it, we are done
		if (command == methodEnd)
			return;
		
		// remove CLEAR from method end, skipping comments, pragmas and declarations 
		// (but no ASSERTs or LOG-POINTs, which could depend on CLEAR)
		command = (methodEnd != null) ? methodEnd.getPrev() : code.lastCommand;
		while (command != null && command != methodStart) {
			Command prev = command.getPrev();
			if (command.firstCodeTokenIsKeyword("CLEAR")) {
				executeOnClear(code, command, localVariables, actionAtEnd, false);
			} else if (!command.isCommentLine() && !command.isPragmaLine() && !command.isDeclaration()) {
				lastExecutable = command.getNext();
				break;
			}
			command = prev;
		}
		
		// in the remaining 'executable' section between method start and end, remove to-do comments that might have been 
		// added by earlier executions of this rule, but are no longer valid 
		command = firstExecutable;
		while (command != null && command != lastExecutable) {
			if (command.firstCodeTokenIsKeyword("CLEAR")) {
				removeComments(code, command);
			}
			command = command.getNext();
		}
	}
	
	private void executeOnClear(Code code, Command command, Variables localVariables, NeedlessClearAction action, boolean isAtStart) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		if (isCommandBlocked(command) || action == NeedlessClearAction.IGNORE)
			return;

		Token token = command.getFirstCodeToken().getNextCodeSibling();
		if (token.isChainColon())
			token = token.getNextCodeSibling();
		
		// for Commands that are completely removed, rule use will be set on the METHOD start
		Command parentCommand = command.getParent();
		boolean isInOOContext = command.isInOOContext();
		
		while (token != null && !command.wasRemovedFromCode()) {
			Token chainElemEnd = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (chainElemEnd == null)
				break;
			Token nextAfterElem = chainElemEnd.getNextCodeSibling();
			
			// only process cases where the identifier is directly followed by the comma or period, i.e. 
			// no cases with additions WITH NULL or WITH val [IN {CHARACTER|BYTE} MODE]
			if (token.isIdentifier() && chainElemEnd == token.getNextCodeSibling() && canRemoveClear(token, localVariables, isAtStart, isInOOContext)) {
				try {
					if (command.handleChainElement(token, action.getCorrespondingChainElementAction(), commentText)) {
						code.addRuleUse(this, command.wasRemovedFromCode() ? parentCommand : command);
					}
				} catch(UnexpectedSyntaxException ex) {
					throw new UnexpectedSyntaxAfterChanges(this, ex);
				}
			}
			token = nextAfterElem;
		}
	}
	
	private boolean canRemoveClear(Token token, Variables localVariables, boolean isAtStart, boolean isInOOContext) {
		// excluded cases where the variable is not defined locally (e.g. parameters or attributes), 
		// as well as cases defined with DATA ... BEGIN OF
		VariableInfo varInfo = localVariables.getVariableInfo(token, false);
		if (varInfo == null || varInfo.isBoundStructuredData || varInfo.isParameter())
			return false;

		// exclude cases declared with STATICS (CONSTANTS, FIELD-SYMBOLS, TYPES make no sense anyway)
		if (!varInfo.declarationToken.getParentCommand().firstCodeTokenIsKeyword("DATA"))
			return false;
		
		// exclude cases where the Token after CLEAR contains more than the variable name, e.g. 
		// ls_struc-component, lo_obj->attribute, lr_ref_to_data->*, lt_table[ 1 ] etc.
		String text = token.textStartsWith(ABAP.OPERAND_ESCAPE_CHAR_STRING) ? token.getText().substring(1) : token.getText();
		if (!text.equals(ABAP.readTillEndOfVariableName(text, 0, true, isInOOContext)))
			return false;
		
		// exclude cases where the declaration contains a default non-initial VALUE, e.g. DATA lv_any TYPE i VALUE 42.
		if (isAtStart) {
			Token test = varInfo.declarationToken;
			while (test != null && !test.isCommaOrPeriod()) {
				if (test.isKeyword("VALUE")) {
					if (test.matchesOnSiblings(true, "VALUE", "IS", "INITIAL")) {
						break;
					} else {
						return false;
					}
				}
				test = test.getNextCodeSibling();
			}
		}

		// if configured, exclude cases where a structure is cleared which is assigned directly afterwards 
		if (configKeepStrucBeforeAssign.getValue()) {
			Command next = token.getParentCommand().getNextNonCommentCommand();
			if (next != null && next.isAssignment(false, false) && next.getFirstCodeToken().textStartsWith(text + "-")) {
				return false;
			}
		}
		return true;
	}
	
	private void removeComments(Code code, Command command) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		if (isCommandBlocked(command))
			return;

		Token token = command.getFirstCodeToken().getNextCodeSibling();
		if (token.isChainColon())
			token = token.getNextCodeSibling();
		
		while (token != null && !command.wasRemovedFromCode()) {
			try {
				if (command.removeMatchingCommentAboveLineOf(token, ABAP.COMMENT_SIGN_STRING + " " + commentText)) {
					code.addRuleUse(this, command);
				}
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
			Token chainElemEnd = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
			if (chainElemEnd == null)
				break;
			token = chainElemEnd.getNextCodeSibling();
		}
	}
}
