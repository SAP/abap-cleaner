package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.syntax.ExportingKeywordRule;

public class CreateObjectRule extends RuleForCommands {
	// Replaces CREATE OBJECT o [TYPE t] [EXPORTING ...] with o = NEW [#/t]( ...)

	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer NEW to CREATE OBJECT", "#prefer-new-to-create-object"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Prefer New to Create Object", "prefer-new-to-create-object.md")};

	@Override
	public RuleID getID() { return RuleID.CREATE_OBJECT; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace CREATE OBJECT with NEW constructor"; }

	@Override
	public String getDescription() { return "Transforms CREATE OBJECT statements into functional style using NEW."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.40."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public int getRequiredAbapRelease() {
		// the NEW instance operator was not yet available in release 7.31
		return ABAP.REQUIRED_RELEASE_740; 
	}  

	@Override
	public boolean isEssential() { return true; }

	@Override
	public String getExample() {
		return "" 
			+ LINE_SEP + "  METHOD replace_create_object." 
			+ LINE_SEP + "    CREATE OBJECT lx_message." 
			+ LINE_SEP 
			+ LINE_SEP + "    CREATE OBJECT lo_instance TYPE cl_any_class." 
			+ LINE_SEP 
			+ LINE_SEP + "    CREATE OBJECT mo_instance TYPE cl_other_class" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        io_contract    = me" 
			+ LINE_SEP + "        io_msg_handler = mo_msg_handler." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" dynamic typing is not possible with NEW" 
			+ LINE_SEP + "    CREATE OBJECT mo_instance TYPE (lv_class_name)." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" EXCEPTIONS are not possible with NEW" 
			+ LINE_SEP + "    CREATE OBJECT mo_instance TYPE cl_other_class" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        io_msg_handler = mo_msg_handler" 
			+ LINE_SEP + "      EXCEPTIONS" 
			+ LINE_SEP + "        cx_message     = 1" 
			+ LINE_SEP + "        others         = 2." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" evaluating the target variable as an actual parameter" 
			+ LINE_SEP + "    \" only works with CREATE OBJECT, which immediately assigns mo_target:"
			+ LINE_SEP + "    CREATE OBJECT mo_target" 
			+ LINE_SEP + "      EXPORTING iv_value = mo_target->gc_static_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" chains can only be processed if they are first unchained" 
			+ LINE_SEP + "    CREATE OBJECT: lx_message, lx_other_message." 
			+ LINE_SEP 
			+ LINE_SEP + "    CREATE OBJECT: lo_any TYPE cl_any_class," 
			+ LINE_SEP + "                   lo_other TYPE (lv_class_name)," 
			+ LINE_SEP + "                   lo_third TYPE cl_othird_class" 
			+ LINE_SEP + "                     EXPORTING io_contract = me." 
			+ LINE_SEP + "  ENDMETHOD.";
	}

	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain CREATE OBJECT: chains (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));

   private final ConfigValue[] configValues = new ConfigValue[] { configProcessChains };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CreateObjectRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, "CREATE", "OBJECT", TokenSearch.makeOptional(":"), TokenSearch.ANY_IDENTIFIER))
			return false;
		
		ArrayList<Command> unchainedCommands = unchain(code, command, configProcessChains.getValue());
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return false;
		
		for (Command unchainedCommand : unchainedCommands) {
			if (executeOnUnchained(code, unchainedCommand, releaseRestriction)) {
				code.addRuleUse(this, unchainedCommand);
			}
		}
		return false; // addRuleUse() was already called above
	}
	
	private boolean executeOnUnchained(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// CREATE OBJECT oref [area_handle] [parameter_list].
		// (where 'oref' is a simple class reference variable, not an expression):
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, "CREATE", "OBJECT", TokenSearch.ANY_IDENTIFIER))
			return false;
			
		Token identifier = firstToken.getNext().getNext();
		Token next = identifier.getNextCodeSibling();

		// do not process CREATE OBJECT ... AREA HANDLE
		if (next.matchesOnSiblings(true, "AREA", "HANDLE"))
			return false;
		
		// do not process 'CREATE OBJECT ole class ...' 
		if (!next.isPeriod() && !next.isAnyKeyword("TYPE", "EXPORTING", "EXCEPTIONS"))
			return false;

 		// do not process the RAP-specific variant CREATE OBJECT ... FOR TESTING
		if (next.matchesOnSiblings(true, TokenSearch.ASTERISK, "FOR", "TESTING"))
			return false;

		// ensure the class name is NOT specified dynamically with "CREATE OBJECT ... TYPE (class_name)"
		if (next.isKeyword("TYPE") && next.getNextCodeSibling().textStartsWith("("))
			return false;

		// ensure that no EXCEPTIONS are specified, because that is not possible with NEW
		if (next.matchesOnSiblings(true, TokenSearch.ASTERISK, "EXCEPTIONS"))
			return false;

		// ignore cases in which the target variable is evaluated inside the parameter list, which could happen for an
		// 'instance' access to a static field: 'CREATE OBJECT lo_instance EXPORTING iv_value = lo_instance->gc_static_field.'
		// This works with CREATE OBJECT, but NOT with NEW, because NEW only assigns the target variable at the very end.
		boolean isInOOContext = command.isInOOContext();
		Token test = next;
		while (test != null) {
			Token nextCode = test.getNextCodeSibling();
			if (nextCode != null && nextCode.textEquals("=")) {
				// formal parameters may happen to have the same name as the receiving variable
			} else if (isTargetVariableUsedIn(test, identifier, isInOOContext)) {
				return false;
			}
			test = test.getNextCodeToken();
		}
		
		identifier.copyWhitespaceFrom(firstToken);
		command.getFirstToken().removeFromCommand(); // CREATE
		command.getFirstToken().removeFromCommand(); // OBJECT

		Token typeInfo;
		next.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, next.sourceLineNum));
		if (next.isKeyword("TYPE")) {
			next.setText("NEW", false);
			typeInfo = next.getNextCodeSibling();
		} else {
			next.insertLeftSibling(Token.createForAbap(0, 1, "NEW", TokenType.KEYWORD, next.sourceLineNum));
			typeInfo = Token.createForAbap(0, 1, "#", TokenType.OTHER_OP, next.sourceLineNum);
			next.insertLeftSibling(typeInfo);
		}

		typeInfo.appendParenthesesUpTo(command.getLastNonCommentToken(), true);

		// remove optional EXPORTING by executing ExportingKeywordRule, regardless of whether the Rule is blocked for this Command
		((ExportingKeywordRule) parentProfile.getRule(RuleID.EXPORTING_KEYWORD)).executeOn(code, command, typeInfo, false, releaseRestriction);

		command.invalidateMemoryAccessType();
		return true;
	}

	private boolean isTargetVariableUsedIn(Token test, Token identifier, boolean isInOOContext) {
		// match the identifier against the test Token: return true if the test Token has the same text,
		// or if it extends the text with a field access, e.g. 'lo_instance->gc_static_field'
		if (!test.textStartsWith(identifier.getText())) {
			return false;
		} else if (test.getTextLength() == identifier.getTextLength()) {
			return true;
		} else if (ABAP.isCharAllowedForVariableNames(test.getText(), identifier.getTextLength(), false, false, isInOOContext)) {
			return false;
		} else {
			return true;
		}
	}
}
