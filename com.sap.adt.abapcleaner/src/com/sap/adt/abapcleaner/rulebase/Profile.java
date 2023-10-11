package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rules.spaces.SpaceAroundCommentSignRule;

import java.io.IOException;
import java.util.*;

public class Profile {
	public static final int REQUIRED_VERSION = 1;
	public static final String DEFAULT_NAME = "default";
	public static final String ESSENTIAL_NAME = "essential";
	public static final String READ_ONLY_INFIX = ": ";
	
	private static final String KEY_RULE_COUNT = "ruleCount";
	private static final String KEY_AUTO_ACTIVATE_NEW_FEATURES = "autoActivateNewFeatures";
	private static final String KEY_RULES = "rules";
	private static final String KEY_RULE_ID = "ruleID";

	// by default, automatically activate new features (Rules and configuration options) after an update:
	public static final boolean AUTO_ACTIVATE_NEW_FEATURES_DEFAULT = true;

	public static class Comparer implements Comparator<Profile> {
		public final int compare(Profile x, Profile y) {
			return x.name.compareToIgnoreCase(y.name);
		}
	}

	public static ArrayList<Profile> loadProfiles(String ownProfileDir, ArrayList<ProfileDir> readOnlyProfileDirs) {
		ArrayList<Profile> profiles = new ArrayList<Profile>();
		if (ownProfileDir != null) {
			addProfilesFromDir(profiles, ownProfileDir, "");
		}
		if (readOnlyProfileDirs != null) {
			for (ProfileDir profileDir : readOnlyProfileDirs) {
				addProfilesFromDir(profiles, profileDir.readOnlyDir, profileDir.shortName + READ_ONLY_INFIX);
			}
		}

		if (profiles.isEmpty()) {
			// ensure there is at least the 'default' and the 'essential' profile in the list
			profiles.add(Profile.createDefault());
			profiles.add(Profile.createEssential());
		}
		Collections.sort(profiles, new Profile.Comparer());

		return profiles;
	}

	public static void updateReadOnlyProfiles(ArrayList<Profile> profiles, ArrayList<ProfileDir> readOnlyProfileDirs) {
		profiles.removeIf(p -> p.isReadOnly);
		for (ProfileDir profileDir : readOnlyProfileDirs) {
			addProfilesFromDir(profiles, profileDir.readOnlyDir, profileDir.shortName + READ_ONLY_INFIX);
		}
	}

	private static void addProfilesFromDir(ArrayList<Profile> profiles, String dir, String namePrefix) {
		Persistency persistency = Persistency.get(); 
		String[] paths = getLoadPaths(dir);
		for (String path : paths) {
			try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, path, Program.TECHNICAL_VERSION)) {
				profiles.add(Profile.createFromSettings(reader, namePrefix));
			} catch (IOException e) {
			}
		}
	}
	
	public static boolean addAndSaveEssentialProfile(String directory) {
		if (getNumberOfSavedProfiles(directory) == 0) {
			// on first startup, i.e. when the profiles folder is completely empty, leave creation to Profile.loadProfiles()
         return true;
		} else {
			// at least one profile already exists; now add the 'essential' profile 
      	Profile profile = Profile.createEssential();
         String path = getSavePath(directory, profile.name);
         if (!Persistency.get().fileExists(path)) {
         	profile.save(directory);
         	return true;
         }
         return false;
		}
	}

	public static String getSavePath(String directory, String profileName) {
		Persistency persistency = Persistency.get(); 
		return StringUtil.isNullOrEmpty(directory) ? persistency.getSavePath(FileType.PROFILE_TEXT, profileName)
														 		 : persistency.getSavePath(FileType.PROFILE_TEXT, directory, profileName);
	}

	public static String[] getLoadPaths(String directory) {
		Persistency persistency = Persistency.get(); 
		return StringUtil.isNullOrEmpty(directory) ? persistency.getLoadPaths(FileType.PROFILE_TEXT) 
																 : persistency.getLoadPaths(FileType.PROFILE_TEXT, directory);
	}
	
	public static int getNumberOfSavedProfiles(String directory) {
		String[] paths = getLoadPaths(directory);
		return (paths == null) ? 0 : paths.length;
	}
	
	// -------------------------------------------------------------------------
	
	public String name;
	public boolean isReadOnly;
	
	/** <p>determines handling of new features, i.e. a) new Rules, or b) new configuration options added later to existing Rules:<p> 
	 * <ul><li>true = new Rules will automatically be activated, and new configuration options will be set to their 
	 * 'default value', affecting the cleanup result;</li>
	 * <li>false = new Rules will be deactivated, and new configuration options will be set to a 'neutral value',  
	 * until the user decides to activate them</li></ul> */
	public boolean autoActivateNewFeatures;
	
	private Rule[] rules;
	private RuleGroup[] ruleGroups;
	private final HashMap<String, Rule> ruleOfId = new HashMap<String, Rule>();

	@Override
	public String toString() {
		return name;
	}

	public final int getRuleCount() {
		return rules.length;
	}

	public final Rule[] getAllRules() {
		return rules;
	}

	public final Rule getRule(RuleID ruleID) {
		return rules[ruleID.getValue()];
	}

	public final RuleGroup getRuleGroup(RuleGroupID ruleGroupID) {
		return ruleGroups[ruleGroupID.getValue()];
	}

	// -------------------------------------------------------------------------

	public static Profile createFromSettings(ISettingsReader reader, String namePrefix) throws IOException {
		return new Profile(reader, namePrefix);
	}
	private Profile(ISettingsReader reader, String namePrefix) throws IOException {
		name = namePrefix + reader.getFileNameWithoutExtension();
		isReadOnly = !StringUtil.isNullOrEmpty(namePrefix);
		// path = reader.path;
		initializeRules();
		load(reader);
	}

	public static Profile createDefault() {
		return new Profile(DEFAULT_NAME);
	}
	public static Profile createEssential() {
		Profile profile = new Profile(ESSENTIAL_NAME);
		profile.activateEssentialRulesOnly();
		profile.autoActivateNewFeatures = false;
		return profile;
	}
	public static Profile create(String name) {
		return new Profile(name);
	}
	private Profile(String name) {
		this.name = name;
		this.isReadOnly = false;
		this.autoActivateNewFeatures = AUTO_ACTIVATE_NEW_FEATURES_DEFAULT;
		initializeRules();
	}

	public static Profile createFromModel(String name, Profile model) {
		return new Profile(name, model);
	}
	private Profile(String name, Profile model) {
		this.name = name;
		this.isReadOnly = false;
		this.autoActivateNewFeatures = model.autoActivateNewFeatures;
		
		initializeRules();
		for (Rule rule : rules)
			rule.copyFrom(model.getRule(rule.getID()));
	}

	private void initializeRules() {
		ruleGroups = new RuleGroup[Rule.RULE_GROUP_COUNT];
		for (int i = 0; i < Rule.RULE_GROUP_COUNT; ++i)
			ruleGroups[i] = new RuleGroup(RuleGroupID.forValue(i));

		rules = Rule.getAllRules(this);
		for (Rule rule : rules) {
			// ruleOfId is also used in the context of Rule persistency, 
			// therefore using RuleID.name() is more stable than RuleID.toString()
			ruleOfId.put(rule.getID().name(), rule);
			ruleGroups[rule.getGroupID().getValue()].add(rule);
		}
	}

	public final void executeRules(Code code, int releaseRestriction) throws CleanException {
		executeRules(code, releaseRestriction, false, null);
	}
	public final void executeRules(Code code, int releaseRestriction, boolean executeInactiveRules, IProgress progress) throws CleanException {
		int rulesToExecuteCount = 0;
		if (executeInactiveRules)
			rulesToExecuteCount = rules.length;
		else {
			for (Rule rule : rules) {
				if (rule.isActive)
					++rulesToExecuteCount;
			}
		}

		if (progress != null)
			progress.report(TaskType.CLEANER, 0.0);
		int executedCount = 0;
		for (Rule rule : rules) {
			if (!rule.isActive && !executeInactiveRules)
				continue;
			
			try {
				rule.executeIfAllowedOn(code, releaseRestriction);

			} catch (CleanException ex) {
				if (ex.severity.getValue() <= ExceptionSeverity.S1_STOP_RULE.getValue()) {
					// add to log and continue with next rule
					ex.addToLog();
				} else {
					// escalate
					ex.enhanceIfMissing(rule, rule.commandForErrorMsg);
					throw ex;
				}
			}
			if (progress != null && progress.isCancellationPending())
				return;
			++executedCount;
			if (progress != null)
				progress.report(TaskType.CLEANER, executedCount / (double) rulesToExecuteCount);
		}
	}

	public final boolean save(String dir) {
      String path = Profile.getSavePath(dir, name);
      try (ISettingsWriter writer = TextSettingsWriter.createForFile(Persistency.get(), path, Program.TECHNICAL_VERSION, Profile.REQUIRED_VERSION)) {
         save(writer);
         return true;
      } catch (IOException e) {
      	return false;
		}
	}

	public final void save(ISettingsWriter writer) throws IOException {
		writer.write(KEY_RULE_COUNT, rules.length);
		writer.write(KEY_AUTO_ACTIVATE_NEW_FEATURES, autoActivateNewFeatures);

		writer.startArray(KEY_RULES);
		for (Rule rule : rules) {
			writer.startObjectInArray();
			// for persistency, using RuleID.name() is more stable than RuleID.toString()
			writer.write(KEY_RULE_ID, rule.getID().name());
			rule.save(writer);
			writer.closeObjectInArray();
		}
		writer.closeArray();
	}

	private void load(ISettingsReader reader) throws IOException {
		int ruleCount = reader.readInt32(KEY_RULE_COUNT);
		HashMap<String, String> unknownRulesSettings = new HashMap<>();

		// depending on whether new features shall automatically be activated, set the default Rule activation 
		// (for the case that the file did not yet 'know' this Rule, and therefore does not overwrite this activation) 
		autoActivateNewFeatures = (reader.getFileVersion() >= 14) ? reader.readBool(KEY_AUTO_ACTIVATE_NEW_FEATURES) : AUTO_ACTIVATE_NEW_FEATURES_DEFAULT; 
		if (autoActivateNewFeatures) {
			// activate all Rules that are active by default, so a newly added Rule will usually be active 
			activateDefaultRulesOnly();
		} else {
			// deactivate all rules, so a newly added Rule will always be inactive
			deactivateAllRules();
		}

		reader.startArray(KEY_RULES);
		for (int i = 0; i < ruleCount; ++i) {
			reader.startObjectInArray();
			String id = reader.readString(KEY_RULE_ID);

			if (ruleOfId.containsKey(id)) {
				Rule rule = ruleOfId.get(id);

				// depending on whether new features shall automatically be activated in this Profile, set the Rule 
				// configuration (for the case that the file did not yet 'know' part of the configuration options, and therefore 
				// does not overwrite their value) 
				if (autoActivateNewFeatures) {
					// set all options to their default value, so a newly added configuration will (usually) affect the cleanup result 
					rule.setDefault();
				} else {
					// Set all options to their 'neutral value', so newly added configuration will (ideally) not change the 
					// cleanup result. Such a 'neutral value' is only defined for those options that are added to an existing 
					// Rule with a later update, and only after the 'autoActivateNewFeatures' attribute was introduced.
					rule.setNeutral();
				}

				rule.load(reader);

			} else {
				// a Rule with this ID does not exist anymore, so the respective section in the file will be ignored
				Rule.skipLoad(reader, id, unknownRulesSettings);
			}	
			
			reader.closeObjectInArray();
		}
		reader.closeArray();
		
		// upgrade settings from obsolete rules
		if (reader.getFileVersion() < 18) {
			// transfer settings from obsolete rules SPACE_BEFORE_COMMENT_SIGN and SPACE_AFTER_COMMENT_SIGN 
			// to new merged rule SPACE_AROUND_COMMENT_SIGN
			SpaceAroundCommentSignRule spaceAroundCommentSignRule = (SpaceAroundCommentSignRule)getRule(RuleID.SPACE_AROUND_COMMENT_SIGN);
			if (spaceAroundCommentSignRule != null) {
				if (Rule.wasOldRuleLoaded(unknownRulesSettings, ObsoleteRuleID.SPACE_BEFORE_COMMENT_SIGN)) {
					spaceAroundCommentSignRule.configSpaceBeforeCommentSign.setValue(Rule.wasOldRuleActive(unknownRulesSettings, ObsoleteRuleID.SPACE_BEFORE_COMMENT_SIGN));
				}
				if (Rule.wasOldRuleLoaded(unknownRulesSettings, ObsoleteRuleID.SPACE_AFTER_COMMENT_SIGN)) {
					spaceAroundCommentSignRule.configSpaceAfterCommentSign.setValue(Rule.wasOldRuleActive(unknownRulesSettings, ObsoleteRuleID.SPACE_AFTER_COMMENT_SIGN));
				}
			}
			// other cases may need rule.updateSettingFrom(unknownRulesSettings, oldRuleId, oldSettingName, newSettingName);
		}
	}
	
	public final void activateAllRules() {
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			rules[i].isActive = true;
	}

	public final void activateDefaultRulesOnly() {
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			rules[i].isActive = rules[i].isActiveByDefault();
	}

	public final void activateEssentialRulesOnly() {
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			rules[i].isActive = rules[i].isEssential();
	}

	public final void deactivateAllRules() {
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			rules[i].isActive = false;
	}

	public final Rule[] getRulesSortedByGroup() {
		Rule[] result = rules.clone();
		Arrays.sort(result, new Rule.ComparerByGroupAndExecutionOrder());
		return result;
	}

	public final Rule[] getRulesSortedByName() {
		Rule[] result = rules.clone();
		Arrays.sort(result, new Rule.ComparerByDisplayName());
		return result;
	}

	public int getActiveRuleCount() {
		int activeRuleCount = 0;
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			if (rules[i].isActive)
				++activeRuleCount;
		}
		return activeRuleCount;
	}

	public Rule getSingleActiveRule() {
		Rule activeRule = null;
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			if (rules[i].isActive) {
				if (activeRule == null)
					activeRule = rules[i];
				else
					return null;
			}
		}
		return activeRule;
	}
	
	/**
	 * returns the profile name without the prefix that was added if the profile belongs to a read-only (team) profile directory
	 * @return
	 */
	public String getNameWithoutPrefix() {
		int infixPos = name.indexOf(READ_ONLY_INFIX);
		return (infixPos < 0) ? name : name.substring(infixPos + READ_ONLY_INFIX.length());
	}
}
