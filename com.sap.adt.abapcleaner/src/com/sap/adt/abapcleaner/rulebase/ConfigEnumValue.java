package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

public class ConfigEnumValue<T extends Enum<T>> extends ConfigSelectionValue {
	public ConfigEnumValue(Rule rule, String settingName, String description, String[] selection, T defaultValue) {
		super(rule, settingName, description, selection, defaultValue.ordinal());
	}

	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigEnumValue(Rule rule, String settingName, String description, String[] selection, T defaultValue, T neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, selection, defaultValue.ordinal(), neutralValue.ordinal(), dateCreated);
	}

	/* TODO: Java wizards, how is this done generically?
	public T getEnumValue() {
		return T.forValue(getValue()); 
	}
	*/
	
	public void setEnumValue(T value) {
		setValue(value.ordinal());
		
	}
}
