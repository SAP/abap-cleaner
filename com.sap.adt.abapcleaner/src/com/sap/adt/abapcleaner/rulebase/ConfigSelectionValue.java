package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

/**
 * Exposes a configuration value that can be set with a ComboBox with a fixed selection.
 */
public class ConfigSelectionValue extends ConfigValue {
	public final int defaultValue;
	public final int neutralValue;
	public final String[] selection;
	// for generating unit tests:
	public String enumClassName;
	public String[] enumNames;
	
	public int getValue() { 
		return rule.getInt(settingName); 
	}
	
	public void setValue(int value) {
		// validate the input value
		value = Math.min(Math.max(value, 0), selection.length - 1);
		rule.setInt(settingName, value);
	}

	@Override
	public boolean isDefault() {
		return (getValue() == defaultValue);
	}

	public final int getDefault() { 
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

	ConfigSelectionValue(Rule rule, String settingName, String description, String[] selection, int defaultValue) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, selection, defaultValue, defaultValue, null);
	}

	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	ConfigSelectionValue(Rule rule, String settingName, String description, String[] selection, int defaultValue, int neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, dateCreated);
		this.defaultValue = defaultValue;
		this.selection = selection;
		this.neutralValue = neutralValue;
	}

	@Override
	public String toString() {
		return description + " [" + selection[defaultValue] + "]";
	}

	private static String getCodeForValue(int value, String enumClassName, String[] enumNames) {
		return enumClassName + "." + enumNames[value];
	}

	@Override
	public String getValueAsCode() {
		return getCodeForValue(getValue(), enumClassName, enumNames); 
	}

	@Override
	public String getDefaultValueAsCode() { 		
		return getCodeForValue(getDefault(), enumClassName, enumNames); 
	}
}
