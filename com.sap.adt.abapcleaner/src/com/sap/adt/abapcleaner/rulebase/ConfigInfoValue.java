package com.sap.adt.abapcleaner.rulebase;

/**
 * Exposes a configuration information that can be displayed with a Label.
 */
public class ConfigInfoValue extends ConfigValue {
	private final ConfigInfoStyle style;
	
	@Override
	public boolean isDefault() {
		return true;
	}

	@Override
	public void setDefault() {
	}

	@Override
	public void setNeutral() {
	}

	public ConfigInfoValue(Rule rule, String description) {
		super(rule, "", description);
		this.style = ConfigInfoStyle.NORMAL;
	}

	public ConfigInfoValue(Rule rule, String description, ConfigInfoStyle style) {
		super(rule, "", description);
		this.style = style;
	}

	public boolean isWarning() {
		return (style == ConfigInfoStyle.WARNING);
	}

	public boolean isHeading() {
		return (style == ConfigInfoStyle.HEADING);
	}

	@Override
	public String toString() {
		return description;
	}

	@Override
	public String getValueAsCode() {
		return null; 
	}

	@Override
	public String getDefaultValueAsCode() { 		
		return null; 
	}
}