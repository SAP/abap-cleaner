package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;

public abstract class RuleForDdlEmptyLines extends RuleForDdlCommands {
	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_EMPTY_LINES; }

	protected RuleForDdlEmptyLines(Profile profile) {
		super(profile);
	}
}
