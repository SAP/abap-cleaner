package com.sap.adt.abapcleaner.rulebase;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Persistency;

/**
 * Exposes a configuration value that can be set with a TextBox.
 */
public class ConfigTextValue extends ConfigValue {
	public final String defaultValue;
	public final String neutralValue;
	public final ConfigTextType textType;
	// the visible length may or may not reflect the maximum possible input length
	public final int visibleLengthHint;
	public final String buttonText;
	
	public final String getValue() { 
		return rule.getString(settingName); 
	}
	
	public final void setValue(String value) {
		// validate the input value, also preventing 'ABAP Injection'
		if (textType == ConfigTextType.ABAP_CLASS) {
			value = ABAP.toVariableName(value);
			
		} else if (textType == ConfigTextType.DDL_ANNOTATION_LIST) {
			value = DDL.toAnnotationList(value);
			
		} else if (textType == ConfigTextType.FOLDER_FILE_NAME) {
			Persistency persistency = Persistency.get();
			if (persistency != null) {
				value = persistency.getValidPath(value, true);
			}
		}

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

	public ConfigTextValue(Rule rule, String settingName, String description, String defaultValue, ConfigTextType textType, int visibleLength, String buttonText) {
		// as no distinct neutralValue is provided, use the defaultValue twice
		this(rule, settingName, description, defaultValue, textType, visibleLength, buttonText, defaultValue, null);
	}
	
	/** Constructor for configuration that is added to an existing rule in a later update and therefore has a distinct neutralValue */
	public ConfigTextValue(Rule rule, String settingName, String description, String defaultValue, ConfigTextType textType, int visibleLength, String buttonText, String neutralValue, LocalDate dateCreated) {
		super(rule, settingName, description, dateCreated);
		this.defaultValue = defaultValue;
		this.textType = textType;
		this.neutralValue = neutralValue;
		this.visibleLengthHint = visibleLength;
		this.buttonText = buttonText;
	}

	@Override
	public String toString() {
		return description + " [" + defaultValue + "]";
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