package com.sap.adt.abapcleaner.rulebase;

import java.util.*;

public class RuleGroup {
	private static String formatName(String name) {
		return "---------- " + name.toUpperCase(Locale.ROOT) + " ----------";
	}

	public final RuleGroupID iD;
	private ArrayList<Rule> rules = new ArrayList<Rule>();

	RuleGroup(RuleGroupID ruleGroupID) {
		iD = ruleGroupID;
	}

	final void add(Rule rule) {
		rules.add(rule);
	}

	@Override
	public String toString() {
		return formatName(getName());
	}

	public String getName() {
		switch (iD) {
			case EMPTY_LINES:
				return "Empty Lines";

			case SPACES:
				return "Spaces";

			case DECLARATIONS:
				return "Declarations";

			case SYNTAX:
				return "Syntax";

			case COMMANDS:
				return "Commands";

			case PRETTY_PRINTER:
				return "Pretty Printer";

			case ALIGNMENT:
				return "Alignment";

			case DDL_ANNOTATIONS:
				return "DDL Annotations";

			case DDL_POSITION:
				return "DDL Line Breaks and Indent";

			default:
				throw new IndexOutOfBoundsException("Unknown RuleGroup!");
		}
	}
	
	public final boolean isAnyRuleInactive() {
		for (Rule rule : rules) {
			if (!rule.isActive)
				return true;
		}
		return false;
	}

	public final boolean isAnyRuleActive() {
		for (Rule rule : rules) {
			if (rule.isActive)
				return true;
		}
		return false;
	}
}