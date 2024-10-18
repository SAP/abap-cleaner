package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import java.time.LocalDate;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Release;
import com.sap.adt.abapcleaner.rules.alignment.AlignParametersRule;

import java.util.*;

class RuleTest {
	private Profile profile;
	private Rule[] rules;
	
	@BeforeEach
	void setUp() throws Exception {
		profile = Profile.createDefault();
		rules = Rule.getAllRules(profile);
	}

	@Test
	void testSetupOk() {
		assertNotNull(rules);
	}

	@Test
	void testDependencies() {
		// ensure that dependent Rules are executed only after the rules on which they depend; 
		// the execution order is based on the order in the RuleID enumeration
		for (Rule rule : rules) {
			RuleID ruleID = rule.getID();
			RuleID[] dependentRuleIDs = rule.getDependentRules();
			if (dependentRuleIDs == null) 
				continue;
			for (RuleID dependentRuleID : dependentRuleIDs) {
				if (ruleID.getValue() > dependentRuleID.getValue()) {
					fail("Rule '" + ruleID.toString() + "' must be executed before dependent rule '" + dependentRuleID.toString() + "'; please move '" + ruleID.toString() + "' up in the RuleID enumeration");
				}
			}
		}
	}
	
	@Test
	void testRuleStats() {
		Rule anyRule = rules[0];
		Rule otherRule = rules[1];
		
		RuleStats anyStats = RuleStats.create(anyRule, 1, 2);
		RuleStats otherStats = RuleStats.create(otherRule, 0, 2);

		assertEquals(anyStats.getRuleID(), anyRule.getID());
		assertEquals(otherStats.getRuleID(), otherRule.getID());

		assertTrue(RuleStats.create(anyRule, 0, 0).toString().length() > 0);
		assertTrue(anyStats.toString().length() > 0);
		assertTrue(otherStats.toString().length() > 0);

		assertTrue(anyStats.equals(RuleStats.create(anyRule, 1, 2)));
		assertFalse(anyStats.equals(otherStats));
		assertFalse(anyStats.equals(RuleStats.create(anyRule, 0, 2)));
		assertFalse(anyStats.equals(RuleStats.create(anyRule, 1, 3)));
		
		assertTrue(RuleStats.equals(null, null));
		assertFalse(RuleStats.equals(null, new RuleStats[] { anyStats }));
		assertFalse(RuleStats.equals(new RuleStats[] { anyStats }, null));
		assertFalse(RuleStats.equals(new RuleStats[] { anyStats }, new RuleStats[] { anyStats, otherStats }));
		assertFalse(RuleStats.equals(new RuleStats[] { otherStats, anyStats }, new RuleStats[] { anyStats, otherStats }));
		assertTrue(RuleStats.equals(new RuleStats[] { anyStats, otherStats }, new RuleStats[] { anyStats, otherStats }));
	}
	
	@Test
	void testRuleStatsUsedAndBlocked() {
		Rule anyRule = rules[0];

		assertFalse(RuleStats.create(anyRule, 0, 0).isUsed());
		assertFalse(RuleStats.create(anyRule, 0, 1).isUsed());
		assertTrue(RuleStats.create(anyRule, 1, 0).isUsed());
		assertTrue(RuleStats.create(anyRule, 1, 1).isUsed());

		assertFalse(RuleStats.create(anyRule, 0, 0).isBlocked());
		assertFalse(RuleStats.create(anyRule, 1, 0).isBlocked());
		assertTrue(RuleStats.create(anyRule, 0, 1).isBlocked());
		assertTrue(RuleStats.create(anyRule, 1, 1).isBlocked());

		assertFalse(RuleStats.create(anyRule, 0, 0).isUsedButNeverBlocked());
		assertFalse(RuleStats.create(anyRule, 0, 1).isUsedButNeverBlocked());
		assertTrue(RuleStats.create(anyRule, 1, 0).isUsedButNeverBlocked());
		assertFalse(RuleStats.create(anyRule, 1, 1).isUsedButNeverBlocked());

		assertFalse(RuleStats.create(anyRule, 0, 0).isBlockedButNeverUsed());
		assertFalse(RuleStats.create(anyRule, 1, 0).isBlockedButNeverUsed());
		assertTrue(RuleStats.create(anyRule, 0, 1).isBlockedButNeverUsed());
		assertFalse(RuleStats.create(anyRule, 1, 1).isBlockedButNeverUsed());
	}
	
	@Test 
	void testGetRuleIdAndName() {
		for (Rule rule : rules) {
			assertTrue(rule.getIdAndName().length() > 0);
		}
	}
	
	@Test 
	void testGetRuleDateCreated() {
		LocalDate dec2020 = LocalDate.of(2020, 12, 1);
		for (Rule rule : rules) {
			assertTrue(rule.getDateCreated().compareTo(dec2020) > 0);
		}
	}
	
	@Test
	void testSortByGroupAndDisplayName() {
		Rule[] rules = Rule.getAllRules(profile);

		Arrays.sort(rules, new Rule.ComparerByDisplayName());
		String lastName = null;
		for (Rule rule : rules) {
			String name = rule.getDisplayName();
			if (lastName != null)
				assertTrue(lastName.compareToIgnoreCase(name) < 0); // otherwise, there may be a duplicate Rule name
			lastName = name;
		}

		Arrays.sort(rules, new Rule.ComparerByGroupAndExecutionOrder());
		int lastGroupIdValue = 0;
		for (Rule rule : rules) {
			int groupIdValue = rule.getGroupID().getValue();
			assertTrue(lastGroupIdValue <= groupIdValue);
			lastGroupIdValue = groupIdValue;
		}
	}
	
	@Test
	void testSetDefault() {
		Profile profile = Profile.createDefault();
		Rule[] rules = profile.getAllRules();
		
		for (Rule rule : rules) {
			rule.setDefault();
			ConfigValue[] configValues = rule.getConfigValues();
			for (ConfigValue configValue : configValues) {
				assertTrue(configValue.isDefault());

				if (configValue instanceof ConfigBoolValue)
					assertEquals(((ConfigBoolValue) configValue).getDefault(), ((ConfigBoolValue) configValue).getValue());
				else if (configValue instanceof ConfigIntValue)
					assertEquals(((ConfigIntValue) configValue).getDefault(), ((ConfigIntValue) configValue).getValue());
				else if (configValue instanceof ConfigTextValue)
					assertEquals(((ConfigTextValue) configValue).getDefault(), ((ConfigTextValue) configValue).getValue());
				else if (configValue instanceof ConfigSelectionValue)
					assertEquals(((ConfigSelectionValue) configValue).getDefault(), ((ConfigSelectionValue) configValue).getValue());
			}
		}
	}
	
	@Test
	void testSetNeutral() {
		Profile profile = Profile.createDefault();
		Rule[] rules = profile.getAllRules();
		
		for (Rule rule : rules) {
			rule.setNeutral();
			ConfigValue[] configValues = rule.getConfigValues();
			for (ConfigValue configValue : configValues) {
				if (configValue instanceof ConfigBoolValue)
					assertEquals(((ConfigBoolValue) configValue).neutralValue, ((ConfigBoolValue) configValue).getValue());
				else if (configValue instanceof ConfigIntValue)
					assertEquals(((ConfigIntValue) configValue).neutralValue, ((ConfigIntValue) configValue).getValue());
				else if (configValue instanceof ConfigTextValue)
					assertEquals(((ConfigTextValue) configValue).neutralValue, ((ConfigTextValue) configValue).getValue());
				else if (configValue instanceof ConfigSelectionValue)
					assertEquals(((ConfigSelectionValue) configValue).neutralValue, ((ConfigSelectionValue) configValue).getValue());
			}
		}
	}

	@Test
	void testObsoleteIDsNotReused() {
		// ensure that obsolete RuleIDs are not reused in the RuleID enumeration
		HashSet<String> ruleIDs = new HashSet<>();
		for (RuleID ruleID : RuleID.values()) {
			ruleIDs.add(ruleID.name());
		}
		for (ObsoleteRuleID obsoleteRuleID : ObsoleteRuleID.values()) {
			assertFalse(ruleIDs.contains(obsoleteRuleID.name()));
		}
	}
	
	@Test
	void testEssentialRulesAreActiveByDefault() {
		for (Rule rule : rules) {
			if (rule.isEssential()) {
				assertTrue(rule.isActiveByDefault());
			}
			if (!rule.isActiveByDefault()) {
				assertFalse(rule.isEssential());
			}
		}		
	}
	
	@Test
	void testGetDateLastConfigAdded() {
		// (this test may need different examples when configuration is added to the Rules used here)
		
		// Rule has no configuration at all
		assertNull(profile.getRule(RuleID.CLOSING_BRACKETS_POSITION).getDateLastConfigAdded());
		assertNull(profile.getRule(RuleID.CLOSING_BRACKETS_POSITION).getDateLastConfigAdded());
		
		// Rule configuration unchanged since first release of the Rule 
		assertNull(profile.getRule(RuleID.SPACE_AROUND_COMMENT_SIGN).getDateLastConfigAdded());

		// Rule configuration was enhanced in one or multiple releases after first release of the Rule
		assertEquals(LocalDate.of(2022, 4, 9), profile.getRule(RuleID.CHAIN_OF_ONE).getDateLastConfigAdded());
		assertEquals(LocalDate.of(2022, 12, 16), profile.getRule(RuleID.CALCULATION_ASSIGNMENT).getDateLastConfigAdded());
	}

	@Test
	void testDependsOn() {
		Rule translateRule = profile.getRule(RuleID.TRANSLATE);
		Rule alignCondRule = profile.getRule(RuleID.ALIGN_COND_EXPRESSIONS);
		
		assertTrue(translateRule.dependsOn(RuleID.UPPER_AND_LOWER_CASE));
		assertTrue(translateRule.dependsOn(RuleID.ALIGN_ASSIGNMENTS));
		assertTrue(translateRule.dependsOn(RuleID.ALIGN_PARAMETERS));
		
		assertFalse(translateRule.dependsOn(RuleID.EMPTY_LINES_IN_CLASS_DEFINITION));
		assertFalse(translateRule.dependsOn(RuleID.FINAL_VARIABLE));
		
		assertFalse(alignCondRule.dependsOn(RuleID.UPPER_AND_LOWER_CASE));
	}
	
	@Test
	void testWasAddedSince() {
		Rule translateRule = profile.getRule(RuleID.TRANSLATE);
		assertFalse(translateRule.wasAddedSince(null));

		Release[] releases = Program.getReleases();
		if (releases == null)
			return;
		
		assertFalse(profile.getRule(RuleID.CLOSING_BRACKETS_POSITION).wasAddedSince(releases[0]));
		Release firstRelease = releases[releases.length - 1];
		boolean wasAddedSinceFirstRelease = (translateRule.getDateCreated().compareTo(firstRelease.releaseDate) > 0);
		assertEquals(wasAddedSinceFirstRelease, translateRule.wasAddedSince(firstRelease));
	}
	
	@Test
	void testWasEnhancedSince() {
		Rule translateRule = profile.getRule(RuleID.EXPORTING_KEYWORD);
		assertFalse(translateRule.wasEnhancedSince(null));

		Release[] releases = Program.getReleases();
		if (releases == null)
			return;
		
		assertFalse(profile.getRule(RuleID.CLOSING_BRACKETS_POSITION).wasEnhancedSince(releases[0]));
	}
	
	@Test 
	void testHasSameConfigurationAs() {
		Profile otherProfile = Profile.createDefault();
		
		Rule closingBrackRule = profile.getRule(RuleID.CLOSING_BRACKETS_POSITION);
		Rule otherClosingBrackRule = otherProfile.getRule(RuleID.CLOSING_BRACKETS_POSITION);
		assertTrue(closingBrackRule.hasSameConfigurationAs(otherClosingBrackRule));
		
		AlignParametersRule alignParamRule = (AlignParametersRule)profile.getRule(RuleID.ALIGN_PARAMETERS);
		AlignParametersRule otherAlignParamRule = (AlignParametersRule)otherProfile.getRule(RuleID.ALIGN_PARAMETERS);

		alignParamRule.setDefault();
		otherAlignParamRule.setDefault();
		assertTrue(alignParamRule.hasSameConfigurationAs(otherAlignParamRule));
		assertTrue(otherAlignParamRule.hasSameConfigurationAs(alignParamRule));
		assertTrue(alignParamRule.hasSameConfigurationAs(otherAlignParamRule, "KeepParametersOnSingleLine"));
		assertTrue(alignParamRule.hasSameConfigurationAs(otherAlignParamRule, "AllowContentLeftOfAssignOp"));

		// since AlignParametersRule has configuration that was added after first release of the Rule, some options 
		// such as "AllowContentLeftOfAssignOp" have a different 'neutral' setting than their 'default' setting
		otherAlignParamRule.setNeutral();
		assertFalse(alignParamRule.hasSameConfigurationAs(otherAlignParamRule));
		assertFalse(otherAlignParamRule.hasSameConfigurationAs(alignParamRule));
		assertTrue(alignParamRule.hasSameConfigurationAs(otherAlignParamRule, "KeepParametersOnSingleLine"));
		assertFalse(alignParamRule.hasSameConfigurationAs(otherAlignParamRule, "AllowContentLeftOfAssignOp"));
		
		assertFalse(closingBrackRule.hasSameConfigurationAs(alignParamRule));
		assertFalse(alignParamRule.hasSameConfigurationAs(closingBrackRule));
	}
	
	@Test
	void testMeasureReplacedWithAction() {
		Profile profile = Profile.createDefault();
		Rule[] rules = profile.getAllRules();
		
		StringBuilder sbError = new StringBuilder();
		for (Rule rule : rules) {
			rule.setNeutral();
			ConfigValue[] configValues = rule.getConfigValues();
			for (ConfigValue configValue : configValues) {
				if (StringUtil.containsIgnoringCase(configValue.description, "measure")) {
					if (sbError.length() == 0)
						sbError.append("Replace 'measure' with 'action':").append(System.lineSeparator());
					sbError.append("- rule '" + rule.getDisplayName() + "', option '" + configValue.description + "'").append(System.lineSeparator());
				}
			}
		}
		if (sbError.length() > 0) {
			fail(sbError.toString());
		}
	}
	
	@Test
	void testConfigValueNamesDoNotStartWithConfig() {
		Rule[] rules = Profile.createDefault().getAllRules();
		
		for (Rule rule : rules) {
			for (ConfigValue configValue : rule.getConfigValues()) {
				if (configValue instanceof ConfigInfoValue)
					continue;
				String settingName = configValue.settingName;
				
				assertFalse(settingName.startsWith("config"), "setting name '" + rule.getClass().getSimpleName() + "." + settingName + "' starts with 'config...'");
			}
		}
	}
	
	@Test
	void testConfigValueNamesUnique() {
		Rule[] rules = Profile.createDefault().getAllRules();
		
		for (Rule rule : rules) {
			HashSet<String> names = new HashSet<>();
			ConfigValue[] configValues = rule.getConfigValues();

			for (ConfigValue configValue : configValues) {
				if (configValue instanceof ConfigInfoValue)
					continue;
				String settingNameUpper = configValue.settingName.toUpperCase();
				if (names.contains(settingNameUpper)) {
					fail("rule '" + rule.getDisplayName() + "' has two options with the same setting name '" + configValue.settingName + "'!");
				} else {
					names.add(settingNameUpper);
				}
			}
		}
	}
	
	@Test
	void testConfigSelectionMatchesEnum() {
		Rule[] rules = Profile.createDefault().getAllRules();
		
		for (Rule rule : rules) {
			for (ConfigValue configValue : rule.getConfigValues()) {
				if (configValue instanceof ConfigSelectionValue) {
					ConfigSelectionValue configSelValue = (ConfigSelectionValue) configValue;
					if (configSelValue.enumNames == null) {
						fail("rule '" + rule.getDisplayName() + "', setting '" + configValue.settingName + "': enum member names unknown!");
					} else if (configSelValue.selection.length > configSelValue.enumNames.length) {
						fail("rule '" + rule.getDisplayName() + "', setting '" + configValue.settingName + "' has more selection strings than enum members!");
					}
				}
			}
		}
	}
	
	@Test
	void testConfigAllowsUnitTestGeneration() {
		Rule[] rules = Profile.createDefault().getAllRules();
		
		for (Rule rule : rules) {
			for (ConfigValue configValue : rule.getConfigValues()) {
				if (configValue instanceof ConfigInfoValue) {
					assertNull(configValue.getValueAsCode());
					assertNull(configValue.getDefaultValueAsCode());
				} else {
					assertTrue(configValue.getValueAsCode().length() > 0);
					assertTrue(configValue.getDefaultValueAsCode().length() > 0);
				}
			}
		}
	}
	
	@Test
	void testDateCreatedNotOnReleaseDate() {
		// ensure that the creation dates of rules and configuration options are not equal to release dates, 
		// otherwise it would not be clear in which release they were added
		Rule[] rules = Profile.createDefault().getAllRules();
		Release[] releases = Program.getReleases();
		
		for (Release release : releases) {
			for (Rule rule : rules) {
				assertFalse(release.releaseDate.equals(rule.getDateCreated()), "Rule '" + rule.getDisplayName() + "' was created on release date " + release.releaseDate.toString() + " of version " + release.version.toString());

				for (ConfigValue configValue : rule.getConfigValues()) {
					assertFalse(release.releaseDate.equals(configValue.dateCreated), "Rule '" + rule.getDisplayName() + "', option '" + configValue.settingName + "' was created on release date " + release.releaseDate.toString() + " of version " + release.version.toString());
				}
			}
		}
	}
	
	@Test
	void testButtonClicked() {
		Rule[] rules = Profile.createDefault().getAllRules();
		
		for (Rule rule : rules) {
			assertNull(rule.buttonClicked(null));
		}
	}
}
