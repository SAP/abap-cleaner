package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

/**
 * Exposes possible configuration of a Rule.
 */
public abstract class ConfigValue {
	public abstract boolean isDefault();
	public abstract void setDefault();
	public abstract void setNeutral();
	public abstract String getValueAsCode();
	public abstract String getDefaultValueAsCode();
	
	protected final Rule rule;
	public final String settingName;

	public final String description;
	public final String unit; // e.g. "%" for a Label behind a numeric TextBox
	
	public final LocalDate dateCreated;
	
	protected ConfigValue(Rule rule, String settingName, String description) {
		this(rule, settingName, description, "", null);
	}
	protected ConfigValue(Rule rule, String settingName, String description, LocalDate dateCreated) {
		this(rule, settingName, description, "", dateCreated);
	}
	protected ConfigValue(Rule rule, String settingName, String description, String unit) {
		this(rule, settingName, description, unit, null);
	}
	
	protected ConfigValue(Rule rule, String settingName, String description, String unit, LocalDate dateCreated) {
		this.rule = rule;
		this.settingName = settingName;
		this.description = description;
		this.unit = unit;
		this.dateCreated = dateCreated;
	}
}