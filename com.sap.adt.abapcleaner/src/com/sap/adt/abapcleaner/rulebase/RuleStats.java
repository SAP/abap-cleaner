package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.*;

public class RuleStats {
	private Rule rule;
	private int usedCount;
	private int blockedCount;

	public final boolean isUsed() { return (usedCount > 0); }

	public final boolean isBlocked() { return (blockedCount > 0); }

	public final boolean isUsedButNeverBlocked() { return (usedCount > 0 && blockedCount == 0); }

	public final boolean isBlockedButNeverUsed() { return (usedCount == 0 && blockedCount > 0); }

	public final RuleID getRuleID() { return rule.getID(); }

	public static RuleStats create(Rule rule, int usedCount, int blockedCount) {
		return new RuleStats(rule, usedCount, blockedCount);
	}
	
	private RuleStats(Rule rule, int usedCount, int blockedCount) {
		this.rule = rule;
		this.usedCount = usedCount;
		this.blockedCount = blockedCount;
	}

	@Override
	public String toString() {
		String result = rule.toString();
		if (usedCount > 0)
			result += " (" + Cult.format(usedCount) + ")";
		else if (blockedCount > 0)
			result += " (" + Cult.format(blockedCount) + " blocked)";
		return result;
	}

	public String toConsoleOutput() {
		int USED_WIDTH = 6;
		String used = Cult.format(usedCount);
		if (used.length() < USED_WIDTH)
			used = StringUtil.repeatChar(' ', USED_WIDTH - used.length()) + used;
		return used + "  " + rule.toString();
	}

	public boolean equals(RuleStats other) {
		return (rule == other.rule && usedCount == other.usedCount && blockedCount == other.blockedCount);
	}
	
	public static boolean equals(RuleStats[] stats1, RuleStats[] stats2) {
		if (stats1 == null && stats2 == null)
			return true;
		else if (stats1 == null || stats2 == null || stats1.length != stats2.length)
			return false;
		
		for (int i = 0; i < stats1.length; ++i) {			
			if (!stats1[i].equals(stats2[i]))
				return false;
		}
		return true;
	}
}