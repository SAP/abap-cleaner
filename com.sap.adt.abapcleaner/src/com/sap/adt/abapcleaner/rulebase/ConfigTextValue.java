package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;

/**
 * Exposes a configuration value that can be set with a TextBox.
 */
public class ConfigTextValue extends ConfigValue {
	public final String defaultValue;
	public final String neutralValue;
	public final ConfigTextType textType;
	
	public final String getValue() { 
		return rule.getString(settingName); 
	}
	
	public final void setValue(String value) {
		// validate the input value, also preventing 'ABAP Injection'
		if (textType == ConfigTextType.ABAP_CLASS) 
			value = ABAP.toVariableName(value);

		rule.setString(settingName, (value != null) ? value : "");
	}

	@Override
	public boolean isDefault() {
		return getValue().equals(defaultValue);
	}

	public final String getDefault() { 
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

	public ConfigTextValue(Rule rule, String settingName, String description, String defaultValue, ConfigTextType textType) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, defaultValue, textType, defaultValue, null);
	}
	
	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigTextValue(Rule rule, String settingName, String description, String defaultValue, ConfigTextType textType, String neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, dateCreated);
		this.defaultValue = defaultValue;
		this.textType = textType;
		this.neutralValue = neutralValue;
	}

	@Override
	public String toString() {
		return description + " [" + defaultValue + "]";
	}
	
	public int getMaxLength() {
		switch(textType) {
			case ABAP_CLASS:
				return 30;
			default:
				return 30;
		}
	}

	private static String getCodeForValue(String value) {
		return "\"" + StringUtil.getEscapeText(value) + "\"";
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