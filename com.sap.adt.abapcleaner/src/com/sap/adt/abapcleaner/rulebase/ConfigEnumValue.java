package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

public class ConfigEnumValue<T extends Enum<T>> extends ConfigSelectionValue {
	public ConfigEnumValue(Rule rule, String settingName, String description, String[] selection, T[] values, T defaultValue) {
		super(rule, settingName, description, selection, defaultValue.ordinal());
		setEnumReflectionInfo(values);
	}

	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigEnumValue(Rule rule, String settingName, String description, String[] selection, T[] values, T defaultValue, T neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, selection, defaultValue.ordinal(), neutralValue.ordinal(), dateCreated);
		setEnumReflectionInfo(values);
	}

	private void setEnumReflectionInfo(T[] values) {
		enumClassName = values[0].getDeclaringClass().getSimpleName();
		enumNames = new String[values.length];
		for (int i = 0; i < values.length; ++i) {
			enumNames[i] = values[i].name();
		}
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
