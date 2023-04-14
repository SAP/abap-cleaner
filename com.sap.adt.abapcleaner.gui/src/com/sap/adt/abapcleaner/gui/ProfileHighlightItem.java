package com.sap.adt.abapcleaner.gui;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Release;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;

/**
 * Represents an item from the 'Highlight:' combo box in {@link FrmProfiles}. 
 */
public class ProfileHighlightItem {
	private static Profile programDefaults;

	private Release release;
	private Profile compareToProfile;
	private boolean isProgramDefault;

	public Release getRelease() { return release; }
	public Profile getCompareToProfile() { return compareToProfile; }
	public boolean isProgramDefault() { return isProgramDefault; }
	public boolean isHighlightOff() { return (release == null) && (compareToProfile == null); }
	
	public static ProfileHighlightItem createForNoHighlight() {
		return new ProfileHighlightItem(null, null, false);
	}
	public static ProfileHighlightItem createForRelease(Release release) {
		return new ProfileHighlightItem(release, null, false);
	}
	public static ProfileHighlightItem createForCompareToProfile(Profile compareToProfile) {
		return new ProfileHighlightItem(null, compareToProfile, false);
	}
	public static ProfileHighlightItem createForCompareToProgramDefaults() {
		if (programDefaults == null)
			programDefaults = Profile.createDefault();
		return new ProfileHighlightItem(null, programDefaults, true);
	}
	
	private ProfileHighlightItem(Release release, Profile compareToProfile, boolean isProgramDefault) {
		this.release = release;
		this.compareToProfile = compareToProfile;
		this.isProgramDefault = isProgramDefault;
	}
	
	@Override
	public String toString() {
		// Highlight: ...
		if (isProgramDefault) {
			return "Differences to " + Program.PRODUCT_NAME + " defaults";
		} else if (release != null) {
			return "Features added after " + release.toString();
		} else if (compareToProfile != null) {
			return "Differences to profile '" + compareToProfile.name + "'";
		} else {
			return "Off";
		}
	}
	
	public String toPersistentString() {
		if (isProgramDefault) {
			return "defaults";
		} else if (release != null) {
			return getPersistentStringOfRelease(release.version.toString());
		} else if (compareToProfile != null) {
			return getPersistentStringOfProfile(compareToProfile.name);
		} else {
			return "off";
		}
	}
	
	static String getPersistentStringOfRelease(String releaseVersion) {
		return StringUtil.isNullOrEmpty(releaseVersion) ? "" : "release " + releaseVersion;
	}
	
	static String getPersistentStringOfProfile(String profileName) {
		return StringUtil.isNullOrEmpty(profileName) ? "" : "profile '" + profileName + "'"; 
	}
	
	public boolean highlightFeatureOf(int year, int month, int day) {
		return (release != null) && release.wasEarlierThan(year, month, day);
	}

	public boolean isNewConfig(ConfigValue configValue) {
	  return (release != null) && (configValue.dateCreated != null) && (configValue.dateCreated.compareTo(release.releaseDate) > 0);
	}
	
	public boolean isChangedConfig(Rule rule, ConfigValue configValue) {
		if (compareToProfile != null) {
			Rule compareToRule = compareToProfile.getRule(rule.getID());
			return (compareToRule != null) && !rule.hasSameConfigurationAs(compareToRule, configValue.settingName);
		
		} else {
			return false;
		}
	}
}
