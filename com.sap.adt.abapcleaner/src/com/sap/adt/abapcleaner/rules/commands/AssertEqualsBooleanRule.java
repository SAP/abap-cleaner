package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * Replaces calls to CL_ABAP_UNIT_ASSERT=&gt;ASSERT_EQUALS( exp = abap_true|abap_false act = ...)
 * with the more concise calls to ...ASSERT_TRUE( act = ...) and ...ASSERT_FALSE( act = ...)
 */
public class AssertEqualsBooleanRule extends AssertEqualsRuleBase {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use the right assert type", "#use-the-right-assert-type") };

	@Override
	public RuleID getID() { return RuleID.ASSERT_EQUALS_BOOLEAN; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Use assert_true and assert_false"; }

	@Override
	public String getDescription() {
		return "Replaces calls to cl_abap_unit_assert=>assert_equals with more concise calls to the dedicated methods assert_true and assert_false, where applicable.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 6); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD use_assert_true_and_false." 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( act = mo_any_instance->is_valid( )" 
			+ LINE_SEP + "                                        exp = abap_true )." 
			+ LINE_SEP 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( exp = abap_false" 
			+ LINE_SEP + "                                        act = lv_value )." 
			+ LINE_SEP 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_equals( act  = lv_value" 
			+ LINE_SEP + "                                        exp  = abap_true" 
			+ LINE_SEP + "                                        msg  = 'message' " 
			+ LINE_SEP + "                                        quit = lv_quit )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configRemoveActIfOnlyParameter = new ConfigBoolValue(this, "RemoveActIfOnlyParameter", "Remove 'act =' if only this parameter is passed", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configRemoveActIfOnlyParameter };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AssertEqualsBooleanRule(Profile profile) {
      super(profile);
		initializeConfiguration();
   }

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		return changeAssert(code, command, releaseRestriction, "assert_equals", "exp", "abap_true", "assert_true") || changeAssert(code, command, releaseRestriction, "assert_equals", "exp", "abap_false", "assert_false");
	}

	@Override
	protected boolean performAdditionalCleanUp(Token assertCall) throws UnexpectedSyntaxAfterChanges {
		if (!configRemoveActIfOnlyParameter.getValue())
			return false;

		// if the only remaining parameter is "act = ...", then even "act =" will be removed
		if (!assertCall.getNext().matchesDeep(false, "act", "="))
			return false;
		Token actParam = assertCall.getNext();
		Token actAssignmentOp = actParam.getNext();
		Term actValue;
		try {
			actValue = Term.createArithmetic(actAssignmentOp.getNext());
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}
		if (actValue.getNextSibling() == null) {
			actParam.removeFromCommand();
			actAssignmentOp.removeFromCommand();
		}
		return true;
	}
}
