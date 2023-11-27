package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

/**
 * Exposes a configuration value that can be set with a CheckBox.
 */
public class ConfigBoolValue extends ConfigValue {
	public final boolean defaultValue;
	public final boolean neutralValue;

	public final boolean getValue() { 
		return rule.getBool(settingName); 
	}
	
	public final void setValue(boolean value) {
		rule.setBool(settingName, value);
	}

	@Override
	public boolean isDefault() {
		return (getValue() == defaultValue);
	}

	public final boolean getDefault() { 
		return defaultValue; 
	}
	
	@Override
	public void setDefault() {
		setValue(defaultValue);
	}

	@Override
	public void setNeutral() {
		setValue(neutralValue);
	}

	public ConfigBoolValue(Rule rule, String settingName, String description, boolean defaultValue) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, defaultValue, defaultValue, null);
	}

	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigBoolValue(Rule rule, String settingName, String description, boolean defaultValue, boolean neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, "", dateCreated);
		this.defaultValue = defaultValue;
		this.neutralValue = neutralValue;
	}
	
	@Override
	public String toString() {
		return (defaultValue ? "[X] " : "[ ] ") + description;
	}

	private static String getCodeForValue(boolean value) {
		return value ? "true" : "false";
	}

	@Override
	public String getValueAsCode() {
		return getCodeForValue(getValue()); 
	}

	@Override
	public String getDefaultValueAsCode() { 		
		return getCodeForValue(getDefault()); 
	}
}