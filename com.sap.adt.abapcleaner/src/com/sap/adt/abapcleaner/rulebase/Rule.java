package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.Job;
import com.sap.adt.abapcleaner.programbase.Release;
import com.sap.adt.abapcleaner.programbase.Task;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rules.alignment.*;
import com.sap.adt.abapcleaner.rules.commands.*;
import com.sap.adt.abapcleaner.rules.declarations.*;
import com.sap.adt.abapcleaner.rules.emptylines.*;
import com.sap.adt.abapcleaner.rules.prettyprinter.*;
import com.sap.adt.abapcleaner.rules.spaces.*;
import com.sap.adt.abapcleaner.rules.syntax.*;

import java.io.IOException;
import java.time.LocalDate;
import java.util.*;

public abstract class Rule {
	public static final int RULE_COUNT = 59;
	public static final int RULE_GROUP_COUNT = 7;

	protected static final String LINE_SEP = ABAP.LINE_SEPARATOR;
	protected static Language[] abapOnly =  new Language[] { Language.ABAP };

	private static final String KEY_IS_ACTIVE = "isActive";
	private static final String KEY_SETTING_COUNT = "settingCount";
	private static final String KEY_SETTINGS = "settings";

	private static String getRuleIdAndName(RuleID ruleID) {
		return "rule ID " + String.valueOf((ruleID.getValue())) + " (" + ruleID.name() + ")";
	}

	static class ComparerByDisplayName implements Comparator<Rule> {
		public final int compare(Rule x, Rule y) {
			return x.getDisplayName().compareToIgnoreCase(y.getDisplayName());
		}
	}

	static class ComparerByGroupAndExecutionOrder implements Comparator<Rule> {
		public final int compare(Rule x, Rule y) {
			if (x.getGroupID().getValue() < y.getGroupID().getValue())
				return -1;
			else if (x.getGroupID().getValue() > y.getGroupID().getValue())
				return 1;
			else if (x.getID().getValue() < y.getID().getValue())
				return -1;
			else if (x.getID().getValue() > y.getID().getValue())
				return 1;
			else
				return 0;
		}
	}

   static Rule[] getAllRules(Profile profile) {
      Rule[] rulesUnsorted = new Rule[] {
         // empty lines
         new EmptyLinesInClassDefinitionRule(profile),
         new EmptyLinesWithinMethodsRule(profile),
         new EmptyLinesOutsideMethodsRule(profile),
         
         // spaces
         new SpacesInEmptyBracketsRule(profile),
         new ClosingBracketsPositionRule(profile),
         new SpaceBeforePeriodRule(profile),
         new SpaceAroundCommentSignRule(profile),
         new NeedlessSpacesRule(profile),
         
         // declarations
         new ChainRule(profile),
         new LocalDeclarationOrderRule(profile),
         new UnusedVariablesRule(profile),
         new ChainOfOneRule(profile),
         new ImplicitTypeRule(profile),
         new FinalVariableRule(profile),
         new EscapeCharForParametersRule(profile),
         new EmptySectionsInClassDefRule(profile),
         new AbapDocParametersRule(profile),
         new AbapDocLangRule(profile),
         
         // syntax
         new CommentTypeRule(profile),
         new PseudoCommentRule(profile),
         new PragmaPositionRule(profile),
         new TypoRule(profile),
         new EqualsSignChainRule(profile),
         new CalculationAssignmentRule(profile),
         new ComparisonOperatorRule(profile),
         new NotIsRule(profile),
         new LogicalOperatorPositionRule(profile),
         new EmptyCommandRule(profile),
         new ValueStatementRule(profile),
         new SelfReferenceMeRule(profile),
         new ReceivingKeywordRule(profile),
         new ExportingKeywordRule(profile),
         
         // commands
         new CheckOutsideLoopRule(profile),
         new CheckInLoopRule(profile),
         new IfBlockAtLoopEndRule(profile),
         new IfBlockAtMethodEndRule(profile),
         new CallMethodRule(profile),
         new CreateObjectRule(profile),
         new RaiseTypeRule(profile),
         new AddToEtcRule(profile),
         new MoveToRule(profile),
         new TranslateRule(profile),
         new AssertEqualsBooleanRule(profile),
         new AssertEqualsSubrcRule(profile),
         new AssertClassRule(profile),
         
         // pretty printer
         new UpperAndLowerCaseRule(profile),
         new IndentRule(profile),
         
         // alignment
         new AlignAbapDocRule(profile),
         new AlignMethodsDeclarationRule(profile),
         new AlignMethodsForTestingRule(profile),
         new AlignMethodsRedefinitionRule(profile),
         new AlignAliasesForRule(profile),
         new AlignDeclarationsRule(profile),
         new AlignAssignmentsRule(profile),
         new AlignWithSecondWordRule(profile),
         new AlignClearFreeAndSortRule(profile),
         new AlignParametersRule(profile),
         new AlignLogicalExpressionsRule(profile),
         new AlignCondExpressionsRule(profile)
      };

		StringBuilder errors = new StringBuilder();
		Rule[] rules = new Rule[RULE_COUNT];
		for (Rule rule : rulesUnsorted) {
			if (rules[rule.getID().getValue()] != null)
				errors.append(getRuleIdAndName(rule.getID())).append(" is already used by another rule!").append(System.lineSeparator());
			rules[rule.getID().getValue()] = rule;
		}
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			if (rules[i] == null)
				errors.append("no rule instance found for ").append(getRuleIdAndName(RuleID.forValue(i))).append(System.lineSeparator());
		}
		if (errors.length() > 0)
			throw new IllegalStateException(errors.toString());

		return rules;
	}


	public abstract RuleID getID();

	public abstract RuleGroupID getGroupID();

	public abstract String getDisplayName();

	public abstract String getDescription();

	public String getHintsAndRestrictions() { return ""; }

	public abstract LocalDate getDateCreated();
	
	public abstract RuleReference[] getReferences();

	/** returns a list of Rules that must be executed only AFTER the execution of this Rule  */
	public RuleID[] getDependentRules() { return null; }

	public Language[] getSupportedLanguages() { return abapOnly; }
	
	/** returns the minimum ABAP Release that is required for applying this rule */
	public int getRequiredAbapRelease() { return ABAP.NO_REQUIRED_RELEASE; }

	public boolean isActiveByDefault() { return true; }
	
	/** true if adherence to this rule is explicitly demanded by the 
	 * <a href="https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md">Clean ABAP style guide</a> */
	public boolean isEssential() { return false; }
	
	public abstract String getExample();
	
	public ConfigValue[] getConfigValues() { return new ConfigValue[] { new ConfigInfoValue(this, "(no options available for this rule)", false) }; }

	protected void prepare(Code code) { }

	protected abstract void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges;

	public boolean isConfigValueEnabled(ConfigValue configValue) { return true; }
	
	public final Profile parentProfile;
	/** holds the Command that is currently processed in executeOn() which may be used for error messages */
	public Command commandForErrorMsg; 
	public boolean isActive = isActiveByDefault();

	private HashMap<String, String> settings = new HashMap<String, String>();

	
	protected Rule(Profile profile) {
		parentProfile = profile;
	}

	@Override
	public String toString() {
		return getDisplayName();
	}

	protected final String getNameInSettings() { 
		// for persistency, using RuleID.name() is more stable than RuleID.toString()
		return getID().name(); 
	};

	public String getIdAndName() {
		return getRuleIdAndName(getID());
	}

	protected final boolean getBool(String name) {
		return getBool(settings, name);
	}
	private static final boolean getBool(HashMap<String, String> settings, String name) {
		return settings.containsKey(name) ? SettingsCult.parseBoolean(settings.get(name)) : false;
	}

	protected final void setBool(String name, boolean value) {
		setBool(settings, name, value);
	}
	private static final void setBool(HashMap<String, String> settings, String name, boolean value) {
		settings.put(name, SettingsCult.toString(value));
	}

	protected final int getInt(String name) {
		return getInt(settings, name);
	}
	private static final int getInt(HashMap<String, String> settings, String name) {
		return settings.containsKey(name) ? SettingsCult.parseInt(settings.get(name)) : 0;
	}

	protected final void setInt(String name, int value) {
		setInt(settings, name, value);
	}
	private static final void setInt(HashMap<String, String> settings, String name, int value) {
		settings.put(name, SettingsCult.toString(value));
	}

	protected final String getString(String name) {
		return getString(settings, name);
	}
	private static final String getString(HashMap<String, String> settings, String name) {
		return settings.containsKey(name) ? settings.get(name) : "";
	}

	protected final void setString(String name, String value) {
		setString(settings, name, value);
	}
	private static final void setString(HashMap<String, String> settings, String name, String value) {
		settings.put(name, value);
	}

	public void setDefault() {
		ConfigValue[] values = getConfigValues();
		if (values != null) {
			for (ConfigValue val : values) {
				val.setDefault();
			}
		}
	}

	public void setNeutral() {
		ConfigValue[] values = getConfigValues();
		if (values != null) {
			for (ConfigValue val : values) {
				val.setNeutral();
			}
		}
	}

	final void save(ISettingsWriter writer) throws IOException {
		writer.write(KEY_IS_ACTIVE, isActive);

		writer.write(KEY_SETTING_COUNT, settings.size());
		writer.startObject(KEY_SETTINGS);
		for (Map.Entry<String, String> kvp : settings.entrySet()) {
			writer.writeKeyValue(kvp.getKey(), kvp.getValue());
		}
		writer.closeObject();
	}

	final void load(ISettingsReader reader) throws IOException {
		isActive = reader.readBool(KEY_IS_ACTIVE);

		int settingsCount = reader.readInt32(KEY_SETTING_COUNT);
		reader.startObject(KEY_SETTINGS);
		for (int i = 0; i < settingsCount; ++i) {
			KeyValuePair kvp = reader.readKeyValue();
			setString(kvp.key, kvp.value);
		}
		reader.closeObject();
	}

	final static String getOldSettingKey(ObsoleteRuleID obsoleteRuleId, String oldSettingName) {
		// for persistency, using RuleID.name() is more stable than RuleID.toString()
		return getOldSettingKey(obsoleteRuleId.name() ,oldSettingName);
	}
	final static String getOldSettingKey(String obsoleteRuleId, String oldSettingName) {
		return obsoleteRuleId + "." + oldSettingName;
	}

	static void skipLoad(ISettingsReader reader, String unknownRuleID, HashMap<String, String> unknownRulesSettings) throws IOException {
		boolean isActive = reader.readBool(KEY_IS_ACTIVE);
		setBool(unknownRulesSettings, getOldSettingKey(unknownRuleID, KEY_IS_ACTIVE), isActive);

		int settingsCount = reader.readInt32(KEY_SETTING_COUNT);
		reader.startObject(KEY_SETTINGS);
		for (int i = 0; i < settingsCount; ++i) {
			KeyValuePair kvp = reader.readKeyValue(); 
			setString(unknownRulesSettings, getOldSettingKey(unknownRuleID, kvp.key), kvp.value);
		}
		reader.closeObject();
	}

	final static boolean wasOldRuleLoaded(HashMap<String, String> unknownRulesSettings, ObsoleteRuleID obsoleteRuleId) {
		String oldKey = getOldSettingKey(obsoleteRuleId, KEY_IS_ACTIVE);
		return unknownRulesSettings.containsKey(oldKey);
	}

	final static boolean wasOldRuleActive(HashMap<String, String> unknownRulesSettings, ObsoleteRuleID obsoleteRuleId) {
		String oldKey = getOldSettingKey(obsoleteRuleId, KEY_IS_ACTIVE);
		return unknownRulesSettings.containsKey(oldKey) && getBool(unknownRulesSettings, oldKey);
	}

	final void updateSettingFrom(HashMap<String, String> unknownRulesSettings, ObsoleteRuleID obsoleteRuleId, String oldSettingName, String newSettingName) {
		String oldKey = getOldSettingKey(obsoleteRuleId, oldSettingName);
		if (unknownRulesSettings.containsKey(oldKey)) {
			setString(newSettingName, getString(unknownRulesSettings, oldKey));
		}
	}

	final void copyFrom(Rule model) {
		if (model == null)
			return;
		isActive = model.isActive;
		for (Map.Entry<String, String> kvp : model.settings.entrySet()) 
			settings.put(kvp.getKey(), kvp.getValue());
	}

	protected final void initializeConfiguration() {
		ConfigValue[] configValues = this.getConfigValues();
		if (configValues != null) {
			for (ConfigValue configValue : configValues)
				configValue.setDefault();
		}
	}
	
	public RuleGroup getRuleGroup() {
		return parentProfile.getRuleGroup(getGroupID());
	}
	
	public void executeIfAllowedOn(Code code, int releaseRestriction)  throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (isCleanupAllowedFor(getRequiredAbapRelease(), code, releaseRestriction)) {
			prepare(code);
			executeOn(code, releaseRestriction);
		}
	}
	
	protected static boolean isCleanupAllowedFor(int requiredAbapRelease, Code code, int releaseRestriction) {
		// check whether the rule requires a (minimum) ABAP release
		if (requiredAbapRelease == ABAP.NO_REQUIRED_RELEASE)
			return true;
		
		// check against the ABAP Release restriction specified on the UI
		if (releaseRestriction != ABAP.NO_RELEASE_RESTRICTION && releaseRestriction < requiredAbapRelease)
			return false;
		
		// check against the ABAP Release of the code document in ADT
		if (code.abapRelease == ABAP.NEWEST_RELEASE || StringUtil.isNullOrEmpty(code.abapRelease)) {
			return true;
		} else {
			try {
				return (Integer.parseInt(code.abapRelease) >= requiredAbapRelease);
				
			} catch (NumberFormatException ex) {
				// in older releases, code.abapRelease could have the value "fallback"; in such a case, cleanup is only 
				// allowed for rules that specify no required ABAP release (but this case was already excluded above) 
				return false;
			}
		}
			
	}
	
	protected boolean isCommandBlocked(Command command) {
		return command.isBlocked(getID(), getSupportedLanguages());
	}
	
	public LocalDate getDateLastConfigAdded() {
		ConfigValue[] configValues = getConfigValues();
		if (configValues == null)
			return null;
		LocalDate lastConfigAdded = null;
		 
		for (ConfigValue configValue : configValues) {
			LocalDate configAdded = configValue.dateCreated; 
			if (configAdded != null && (lastConfigAdded == null || configAdded.compareTo(lastConfigAdded) > 0)) {
				lastConfigAdded = configAdded;
			}
		}
		return lastConfigAdded;
	}
	
	public boolean wasAddedSince(Release release) {
      return (release != null && getDateCreated().compareTo(release.releaseDate) > 0);
	}
	
	public boolean wasEnhancedSince(Release release) {
      LocalDate lastConfigAdded = getDateLastConfigAdded();
      return (lastConfigAdded != null && lastConfigAdded.compareTo(release.releaseDate) > 0);
	}
	
	public void toDocumentation(MarkdownBuilder mb) {
		final boolean EXCLUDE_ABAP_CLEANER_AS_REFERENCE = true;

		// rule name 
		mb.startNewHeading(getDisplayName(), 1);
		
		// description
		mb.startNewParagraph();
		mb.appendText(getDescription());

		// hints and restrictions
		if (!StringUtil.isNullOrEmpty(getHintsAndRestrictions())) {
			mb.startNewParagraph();
			mb.appendText(getHintsAndRestrictions());
		}
		
		// references with links
		RuleReference[] references = getReferences();
		boolean hasReference = false;
		for (RuleReference reference : references) {
			if (reference.source != RuleSource.ABAP_CLEANER || !EXCLUDE_ABAP_CLEANER_AS_REFERENCE) {
				hasReference = true;
				break;
			}
		}
		if (hasReference) {
			mb.startNewHeading("References", 2);
			for (RuleReference reference : references) {
				if (EXCLUDE_ABAP_CLEANER_AS_REFERENCE && reference.source == RuleSource.ABAP_CLEANER)
					continue;
				mb.startNewBullet(1);
				String text = reference.getSourceText();
				if (!StringUtil.isNullOrEmpty(reference.chapterTitle)) {
					text  += ": " + reference.chapterTitle;
				}
				if (reference.hasLink()) {
					mb.appendLink(text, reference.getLink());
				} else {
					mb.appendText(text);
				}
			}
		}
		
		// options with default values
		ConfigValue[] configValues = getConfigValues();
		if (configValues != null && configValues.length > 0) {
			mb.startNewHeading("Options", 2);
			for (ConfigValue configValue : configValues) {
				mb.startNewBullet(1);
				mb.appendText(configValue.toString());
			}
		}

		// example code
      String exampleCode = getExample();
      if (!StringUtil.isNullOrEmpty(exampleCode)) {
			mb.startNewHeading("Examples", 2);
			mb.startNewCodeBlock(exampleCode, MarkdownBuilder.ABAP_LANGUAGE_NAME);

			Job job = Job.createForRuleExample(getDisplayName(), exampleCode, this);
	      Task result = job.run();
	      if (result.getSuccess()) {
				mb.startNewParagraph();
				mb.appendText("Resulting code:");
				mb.startNewCodeBlock(result.getResultingCode().toString(), MarkdownBuilder.ABAP_LANGUAGE_NAME);
	      }
      }

	}
	
	public boolean dependsOn(RuleID ruleID) {
		RuleID[] dependentRuleIDs = getDependentRules();
		if (dependentRuleIDs == null)
			return false;
		for (RuleID dependentRuleID : dependentRuleIDs) {
			if (dependentRuleID == ruleID) {
				return true;
			}
		}
		return false;
	}
	
	public boolean hasSameConfigurationAs(Rule otherRule) {
		ConfigValue[] configValues = getConfigValues();
		ConfigValue[] otherConfigValues = otherRule.getConfigValues();
		
		if (configValues == null && otherConfigValues == null)
			return true;
		else if (configValues == null || otherConfigValues == null)
			return false;
		else if (configValues.length != otherConfigValues.length)
			return false;
		
		for (ConfigValue configValue : configValues) {
			String key = configValue.settingName;
			if (!StringUtil.equalsCheckingForNull(settings.get(key), otherRule.settings.get(key))) {
				return false;
			}
		}
		return true;
	}

	public boolean hasSameConfigurationAs(Rule otherRule, String configName) {
		return getString(configName).equals(otherRule.getString(configName));
	}
}