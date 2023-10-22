package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.rulebase.*;
import java.util.*;

/**
 * <p>Manages the {@link Rule}s that were used on a certain {@link Command} ({@link #usedRules}),
 * as well as the {@link Rule}s that are blocked for this Command 
 * and will therefore not be executed when the cleaning of that Command is reprocessed ({@link #blockedRules}).</p>
 * 
 * <p>Each {@link Command} holds its own ChangeControl instance.</p>
 */
public class ChangeControl {
	// private int sourceTextStart;
	// private int sourceTextEnd;
	private BitSet usedRules = new BitSet(Rule.RULE_COUNT);
	private BitSet blockedRules = new BitSet(Rule.RULE_COUNT);

	final boolean wasRuleUsed(RuleID ruleId) {
		return usedRules.get(ruleId.getValue());
	}

	public final boolean isRuleBlocked(RuleID ruleId) {
		return blockedRules.get(ruleId.getValue());
	}

	ChangeControl(int sourceTextStart, int sourceTextEnd) {
		usedRules.clear();
		blockedRules.clear();
		// this.sourceTextStart = sourceTextStart;
		// this.sourceTextEnd = sourceTextEnd;
	}

	final void setUsedRule(RuleID ruleId) {
		usedRules.set(ruleId.getValue(), true);
	}

	final void clearUsedRules() {
		usedRules.clear();
	}

	/**  returns true if the setting was changed */
	public final boolean setBlockedRule(RuleID ruleId, boolean blocked) {
		if (blockedRules.get(ruleId.getValue()) == blocked) {
			return false;
		} else {
			blockedRules.set(ruleId.getValue(), blocked);
			return true;
		}
	}

	public final void addToRuleStats(int[] ruleUseCount, int[] ruleBlockedCount) {
		for (int ruleID = 0; ruleID < Rule.RULE_COUNT; ++ruleID) {
			if (usedRules.get(ruleID))
				++ruleUseCount[ruleID];
			if (blockedRules.get(ruleID))
				++ruleBlockedCount[ruleID];
		}
	}
}