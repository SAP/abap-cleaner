package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * Replaces calls to CL_ABAP_UNIT_ASSERT=&gt;ASSERT_EQUALS( act = sy-subrc  exp = ...)
 * with the more concise calls to ...ASSERT_SUBRC( exp = ...)
 */
public class AssertEqualsSubrcRule extends AssertEqualsRuleBase {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use the right assert type", "#use-the-right-assert-type") };

	@Override
	public RuleID getID() { return RuleID.ASSERT_EQUALS_SUBRC; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Use assert_subrc instead of assert_equals"; }

	@Override
	public String getDescription() { return "Replaces calls to cl_abap_unit_assert=>assert_equals with more concise calls to the dedicated method assert_subrc, where applicable."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 7); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD use_assert_subrc." 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( act = sy-subrc" 
			+ LINE_SEP + "                                        exp = 0 )." 
			+ LINE_SEP 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( exp = 4" 
			+ LINE_SEP + "                                        act = sy-subrc )." 
			+ LINE_SEP 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( act  = sy-subrc" 
			+ LINE_SEP + "                                        exp  = 0" 
			+ LINE_SEP + "                                        msg  = 'message'" 
			+ LINE_SEP + "                                        quit = lv_quit )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configRemoveExpEqualsZero = new ConfigBoolValue(this, "RemoveExpEqualsZero", "Remove 'exp = 0'", true);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configRemoveExpEqualsZero  };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AssertEqualsSubrcRule(Profile profile) {
      super(profile);
		initializeConfiguration();
   }

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		return changeAssert(code, command, releaseRestriction, "assert_equals", "act", "sy-subrc", "assert_subrc");
	}

	@Override
	protected boolean performAdditionalCleanUp(Token assertCall) throws UnexpectedSyntaxAfterChanges {
		if (!configRemoveExpEqualsZero.getValue())
			return false;

		// "exp = 0" can be completely removed, because 0 is the default value for exp;
		// note however that 'exp' is NOT the preferred parameter, so assert_subrc( 0 ) would be equivalent to assert_subrc( act = 0 )!
		// therefore we must NOT shorten assert_subrc( exp = 4 ) to assert_subrc( 4 ).
		Token zeroToken = assertCall.getNext().getLastTokenDeep(false, TokenSearch.ASTERISK, "exp", "=", "0");
		if (zeroToken == null)
			return false;

		Token actAssignmentOp = zeroToken.getPrev();
		Token paramName = actAssignmentOp.getPrev();
		Token next = zeroToken.getNext();
		if (next != null)
			next.copyWhitespaceFrom(paramName);

		paramName.removeFromCommand();
		actAssignmentOp.removeFromCommand();
		zeroToken.removeFromCommand();
		return true;
	}

}