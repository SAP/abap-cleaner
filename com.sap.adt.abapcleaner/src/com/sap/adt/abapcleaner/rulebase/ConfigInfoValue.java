package com.sap.adt.abapcleaner.rulebase;

/**
 * Exposes a configuration information that can be displayed with a Label.
 */
public class ConfigInfoValue extends ConfigValue {
	public final boolean isWarning;
	
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

	public ConfigInfoValue(Rule rule, String description, boolean isWarning) {
		super(rule, "", description);
		this.isWarning = isWarning;
	}
	
	@Override
	public String toString() {
		return description;
	}
}