package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class NeedlessClearTest extends RuleTestBase {
	private NeedlessClearRule rule;
	
	NeedlessClearTest() {
		super(RuleID.NEEDLESS_CLEAR);
		rule = (NeedlessClearRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configActionAtStart.setEnumValue(NeedlessClearAction.DELETE);
		rule.configActionAtEnd.setEnumValue(NeedlessClearAction.ADD_TODO_COMMENT);
		rule.configKeepStrucBeforeAssign.setValue(true);
	}
	

}
