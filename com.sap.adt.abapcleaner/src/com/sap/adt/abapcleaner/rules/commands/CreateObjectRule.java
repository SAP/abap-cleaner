package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.syntax.ExportingKeywordRule;

public class CreateObjectRule extends RuleForCommands {
	// Replaces CREATE OBJECT o [TYPE t] [EXPORTING ...] with o = NEW [#/t]( ...)

	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer NEW to CREATE OBJECT", "#prefer-new-to-create-object") };

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
			+ LINE_SEP + "  ENDMETHOD.";
	}

	public CreateObjectRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, "CREATE", "OBJECT", TokenSearch.ANY_IDENTIFIER))
			return false;

		Token identifier = firstToken.getNext().getNext();
		Token next = identifier.getNextCodeSibling();

		// do not process CREATE OBJECT ... AREA HANDLE
		if (next.matchesOnSiblings(true, "AREA", "HANDLE"))
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
}
