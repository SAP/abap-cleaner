package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class RaiseTypeRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer RAISE EXCEPTION NEW to RAISE EXCEPTION TYPE", "#prefer-raise-exception-new-to-raise-exception-type") };

	@Override
	public RuleID getID() { return RuleID.RAISE_TYPE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace RAISE ... TYPE with RAISE ... NEW"; }

	@Override
	public String getDescription() { return "Replaces the TYPE section of RAISE EXCEPTION TYPE ... and RAISE SHORTDUMP TYPE ... with a NEW constructor call."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.52."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 10, 27); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public int getRequiredAbapRelease() {
		// RAISE SHORTDUMP is only available since release 7.53, but what matters is the syntax which ABAP cleaner
		// introduces to the code, which is 7.52 for RAISE EXCEPTION NEW ... Any incoming code that uses RAISE SHORTDUMP
		// will by itself require release 7.53
		return ABAP.REQUIRED_RELEASE_752; 
	}  

	@Override
	public boolean isEssential() { return true; }

	@Override
	public String getExample() {
		return "" 
			+ LINE_SEP + "  METHOD replace_raise_type_with_new." 
			+ LINE_SEP + "    RAISE EXCEPTION TYPE cx_any_exception." 
			+ LINE_SEP 
			+ LINE_SEP + "    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception." 
			+ LINE_SEP 
			+ LINE_SEP + "    RAISE EXCEPTION TYPE cx_other_exception" 
			+ LINE_SEP + "      EXPORTING" 
			+ LINE_SEP + "        any_param   = any_value" 
			+ LINE_SEP + "        other_param = other_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    RAISE SHORTDUMP TYPE cx_demo_t100" 
		   + LINE_SEP + "      EXPORTING" 
		   + LINE_SEP + "        textid = cx_demo_t100=>demo" 
		   + LINE_SEP + "        text1  = 'this'" 
		   + LINE_SEP + "        text2  = 'is'" 
		   + LINE_SEP + "        text3  = 'an'" 
		   + LINE_SEP + "        text4  = 'example'."
		   + LINE_SEP 
			+ LINE_SEP + "    \" NEW is not possible with [USING] MESSAGE"
			+ LINE_SEP + "    RAISE EXCEPTION TYPE cx_any_exception USING MESSAGE." 
			+ LINE_SEP + "  ENDMETHOD.";
	}

	public RaiseTypeRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstCodeToken();
		if (firstToken == null)
			return false;
		Token typeToken = firstToken.getLastTokenOnSiblings(true, "RAISE", TokenSearch.makeOptional("RESUMABLE"), "EXCEPTION", "TYPE");
		if (typeToken == null) {
			typeToken = firstToken.getLastTokenOnSiblings(true, "RAISE", "SHORTDUMP", "TYPE");
			if (typeToken == null)
				return false;
		}

		Token identifier = typeToken.getNextCodeSibling();
		
		// exclude cases with MESSAGE ... or USING MESSAGE, which don't work with NEW 
		Token next = identifier.getNextCodeSibling();
		if (next.matchesOnSiblings(true, TokenSearch.makeOptional("USING"), "MESSAGE"))
			return false;

		typeToken.setText("NEW", false);
		
		// EXPORTING (if any) must be removed, since it would be a syntax error inside a NEW constructor
		Token exportingToken = next.isKeyword("EXPORTING") ? next : null;
		if (exportingToken != null) 
			exportingToken.removeFromCommand();

		// insert (possibly empty) parentheses
		identifier.appendParenthesesUpTo(command.getLastCodeToken(), true);
		return true;
	}
}
