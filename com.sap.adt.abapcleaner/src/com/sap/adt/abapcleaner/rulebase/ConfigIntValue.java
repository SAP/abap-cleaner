package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.Cult;

/**
 * Exposes a configuration value that can be set with a numeric TextBox.
 */
public class ConfigIntValue extends ConfigValue {
	private static final int SMALL_STEP_DEFAULT = 1;
	private static final int LARGE_STEP_DEFAULT = 10;
	
	public final int minValue;
	public final int defaultValue;
	public final int neutralValue;
	public final int maxValue;
	public final int smallStep;
	public final int largeStep;

	public final int getValue() { 
		return rule.getInt(settingName); 
	}
	
	public final void setValue(int value) {
		// validate the input value
		value = Math.min(Math.max(value, minValue), maxValue);
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

	public ConfigIntValue(Rule rule, String settingName, String description, String unit, int minValue, int defaultValue, int maxValue) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, unit, minValue, defaultValue, maxValue, SMALL_STEP_DEFAULT, LARGE_STEP_DEFAULT, defaultValue, null);
	}
	public ConfigIntValue(Rule rule, String settingName, String description, String unit, int minValue, int defaultValue, int maxValue, int neutralValue) {
		this(rule, settingName, description, unit, minValue, defaultValue, maxValue, SMALL_STEP_DEFAULT, LARGE_STEP_DEFAULT, neutralValue, null);
	}
	public ConfigIntValue(Rule rule, String settingName, String description, String unit, int minValue, int defaultValue, int maxValue, int neutralValue, LocalDate dateCreated) {
		this(rule, settingName, description, unit, minValue, defaultValue, maxValue, SMALL_STEP_DEFAULT, LARGE_STEP_DEFAULT, neutralValue, dateCreated);
	}
	public ConfigIntValue(Rule rule, String settingName, String description, String unit, int minValue, int defaultValue, int maxValue, int smallStep, int largeStep) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, unit, minValue, defaultValue, maxValue, smallStep, largeStep, defaultValue, null);
	}
	
	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigIntValue(Rule rule, String settingName, String description, String unit, int minValue, int defaultValue, int maxValue, int smallStep, int largeStep, int neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, unit, dateCreated);
		this.minValue = minValue;
		this.defaultValue = defaultValue;
		this.maxValue = maxValue;
		this.smallStep = smallStep;
		this.largeStep = largeStep;
		this.neutralValue = neutralValue;
	}
	
	@Override
	public String toString() {
		return description + " [" + Cult.format(defaultValue) + "] " + unit;
	}
}