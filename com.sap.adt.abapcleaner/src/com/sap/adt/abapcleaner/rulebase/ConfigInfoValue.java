package com.sap.adt.abapcleaner.rulebase;

/**
 * Exposes a configuration information that can be displayed with a Label.
 */
public class ConfigInfoValue extends ConfigValue {
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

	ConfigInfoValue(Rule rule, String description) {
		super(rule, "", description);
	}
	
	@Override
	public String toString() {
		return description;
	}
}