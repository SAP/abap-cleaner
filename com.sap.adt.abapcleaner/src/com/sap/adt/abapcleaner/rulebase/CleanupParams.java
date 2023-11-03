package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.ABAP;

public class CleanupParams {
	public final Rule rule;

	public final Profile profile;
	public final boolean executeAllRules;
	
	public final int releaseRestriction;

	public static CleanupParams createForParseOnly() {
		return new CleanupParams();
	}

	public static CleanupParams createForRule(Rule rule) {
		return new CleanupParams(rule, ABAP.NO_RELEASE_RESTRICTION);
	}
	public static CleanupParams createForRule(Rule rule, int releaseRestriction) {
		return new CleanupParams(rule, releaseRestriction);
	}

	public static CleanupParams createForProfile(Profile profile, boolean executeAllRules) {
		return new CleanupParams(profile, executeAllRules, ABAP.NO_RELEASE_RESTRICTION);
	}
	public static CleanupParams createForProfile(Profile profile, boolean executeAllRules, int releaseRestriction) {
		return new CleanupParams(profile, executeAllRules, releaseRestriction);
	}

	private CleanupParams() {
		this.rule = null;
		this.profile = null;
		this.executeAllRules = false;
		this.releaseRestriction = ABAP.NO_RELEASE_RESTRICTION;
	}

	private CleanupParams(Rule rule, int releaseRestriction) {
		this.rule = rule;
		this.profile = null;
		this.executeAllRules = false;
		this.releaseRestriction = releaseRestriction;
	}

	private CleanupParams(Profile profile, boolean executeAllRules, int releaseRestriction) {
		this.rule = null;
		this.profile = profile;
		this.executeAllRules = executeAllRules;
		this.releaseRestriction = releaseRestriction;
	}

	public boolean executeCleanup() {
		return (rule != null) || (profile != null);
	}
	
	public boolean executeSingleRuleOnly() {
		return (rule != null);
	}
	
	public String getProfileName() {
		return (profile == null) ? "" : profile.name;
	}
	
	/**
	 * Returns the rules to be (potentially) executed according to these CleanupParams.
	 * Note that execution still depends on CleanupParams.executeAllRules || rule.isActive. 
	 * @return
	 */
	public Rule[] getRules() {
		if (rule != null)
			return new Rule[] { rule };
		else 
			return profile.getAllRules();
	}
}
