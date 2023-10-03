package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.FileSystemDouble;
import com.sap.adt.abapcleaner.base.ISettingsReader;
import com.sap.adt.abapcleaner.base.ISettingsWriter;
import com.sap.adt.abapcleaner.base.TextSettingsReader;
import com.sap.adt.abapcleaner.base.TextSettingsWriter;
import com.sap.adt.abapcleaner.programbase.FileType;
import com.sap.adt.abapcleaner.programbase.PersistencyDouble;
import com.sap.adt.abapcleaner.programbase.Program;

class ProfileTest {
	private PersistencyDouble persistency;
	private Profile profile;

	private final String anyProfileName = "any";
	private String defaultProfilePath;
	private String essentialProfilePath;
	private String anyProfilePath;
	
	private String profilesDir;
	private String anyDir;

	private final String teamNameA = "team A";
	private final String teamNameB = "team B";
	private String readOnlyDirA;
	private String readOnlyDirB;
	
	@BeforeEach
	void setUp() throws Exception {
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");

		defaultProfilePath = persistency.getSavePath(FileType.PROFILE_TEXT, Profile.DEFAULT_NAME);
		essentialProfilePath = persistency.getSavePath(FileType.PROFILE_TEXT, Profile.ESSENTIAL_NAME);
		anyProfilePath = persistency.getSavePath(FileType.PROFILE_TEXT, anyProfileName);
		profilesDir = persistency.getDirectoryName(defaultProfilePath);
		persistency.prepareDirectory(profilesDir);

		anyDir = persistency.combinePaths(persistency.getWorkDir(), "any_dir");
		readOnlyDirA = persistency.combinePaths(persistency.getWorkDir(), "folder A");
		readOnlyDirB = persistency.combinePaths(persistency.getWorkDir(), "folder B");
		
		persistency.prepareDirectory(anyDir);
		profile = Profile.createDefault();
	}

	private void assertStringArrayEquals(String[] exp, String[] act) {
		assertEquals(exp.length, act.length);
		for (int i = 0; i < exp.length; ++i)
			assertEquals(exp[i], act[i]);
	}

	private void assertUnorderedStringArrayEquals(String[] exp, String[] act) {
		Arrays.sort(exp);
		Arrays.sort(act);
		assertStringArrayEquals(exp, act);
	}

	private void setAnyProfileConfigurationTo(Profile profile) {
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			Rule rule = profile.getRule(RuleID.forValue(i)); 
			rule.isActive = (i % 3 == 0);
			rule.setDefault();
			
			ConfigValue[] values = rule.getConfigValues();
			if (values == null)
				continue;

			for (ConfigValue val : values) {
				// set any configuration
				if (val instanceof ConfigBoolValue) {
					((ConfigBoolValue) val).setValue(i % 2 == 0);
				} else if (val instanceof ConfigIntValue) {
					ConfigIntValue configInt = (ConfigIntValue)val;
					configInt.setValue((i % 5 < 2) ? configInt.minValue : configInt.maxValue);
				} else if (val instanceof ConfigTextValue) {
					((ConfigTextValue)val).setValue("testValue");
				} else if (val instanceof ConfigSelectionValue) {
					ConfigSelectionValue configSel = (ConfigSelectionValue )val;
					configSel.setValue(configSel.selection.length - 1);
				}
			}
		}		
	}
	
	private void assertProfileMatches(Profile profile2) {
		for (int i = 0; i < Rule.RULE_COUNT; ++i) {
			// compare active flag
			RuleID ruleID = RuleID.forValue(i);
			Rule rule1 = profile.getRule(ruleID); 
			Rule rule2 = profile2.getRule(ruleID); 
			assertEquals(rule1.isActive, rule2.isActive);
			
			// compare configuration
			ConfigValue[] values1 = rule1.getConfigValues();
			ConfigValue[] values2 = rule2.getConfigValues();
			if (values1 == null) {
				assertNull(values2);
				continue;
			}
			assertEquals(values1.length, values2.length);
				
			for (int j = 0; j < values1.length; ++j) {
				ConfigValue value1 = values1[j];
				ConfigValue value2 = values2[j];
				if (value1 instanceof ConfigBoolValue) {
					assertTrue(value2 instanceof ConfigBoolValue);
					assertEquals(((ConfigBoolValue)value1).getValue(), ((ConfigBoolValue)value2).getValue()); 

				} else if (value1 instanceof ConfigIntValue) {
					assertTrue(value2 instanceof ConfigIntValue);
					assertEquals(((ConfigIntValue)value1).getValue(), ((ConfigIntValue)value2).getValue()); 

				} else if (value1 instanceof ConfigTextValue) {
					assertTrue(value2 instanceof ConfigTextValue);
					assertEquals(((ConfigTextValue)value1).getValue(), ((ConfigTextValue)value2).getValue()); 
				
				} else if (value1 instanceof ConfigSelectionValue) {
					assertTrue(value2 instanceof ConfigSelectionValue);
					assertEquals(((ConfigSelectionValue)value1).getValue(), ((ConfigSelectionValue)value2).getValue()); 
				
				} else if (value1 instanceof ConfigInfoValue) {
					assertTrue(value2 instanceof ConfigInfoValue);

				} else {
					fail("this config type is not yet supported by the test");
				}
			}
			
		}
	}

	@Test
	void testSetupOk() {
		assertNotNull(profile);
	}

	@Test
	void testActivateAll() {
		profile.activateAllRules();
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			assertTrue(profile.getRule(RuleID.forValue(i)).isActive);

		for (int i = 0; i < Rule.RULE_GROUP_COUNT; ++i) {
			RuleGroup ruleGroup = profile.getRuleGroup(RuleGroupID.forValue(i));
			assertTrue(ruleGroup.isAnyRuleActive());
			assertFalse(ruleGroup.isAnyRuleInactive());
		}

		assertEquals(profile.getActiveRuleCount(), Rule.RULE_COUNT);
		assertEquals(profile.getSingleActiveRule(), null);
	}

	@Test
	void testDeactivateAll() {
		profile.deactivateAllRules();
		for (int i = 0; i < Rule.RULE_COUNT; ++i)
			assertFalse(profile.getRule(RuleID.forValue(i)).isActive);

		for (int i = 0; i < Rule.RULE_GROUP_COUNT; ++i) {
			RuleGroup ruleGroup = profile.getRuleGroup(RuleGroupID.forValue(i));
			assertFalse(ruleGroup.isAnyRuleActive());
			assertTrue(ruleGroup.isAnyRuleInactive());
		}
		
		assertEquals(profile.getActiveRuleCount(), 0);
		assertEquals(profile.getSingleActiveRule(), null);
	}

	@Test
	void testActivateOne() {
		Rule anyRule = profile.getRule(RuleID.ALIGN_DECLARATIONS); 
		
		profile.deactivateAllRules();
		anyRule.isActive = true;
		
		assertEquals(profile.getSingleActiveRule(), anyRule);
	}

	@Test
	void testSortByGroup() {
		Rule[] rules = profile.getRulesSortedByGroup();
		int lastGroupIdValue = 0;
		for (Rule rule : rules) {
			int groupIdValue = rule.getGroupID().getValue();
			assertTrue(lastGroupIdValue <= groupIdValue);
			lastGroupIdValue = groupIdValue;
		}
	}

	@Test
	void testSortByName() {
		Rule[] rules = profile.getRulesSortedByName();
		String lastName = null;
		for (Rule rule : rules) {
			String name = rule.getDisplayName();
			if (lastName != null)
				assertTrue(lastName.compareToIgnoreCase(name) < 0);
			lastName = name;
		}
	}

	@Test
	void testToString() {
		String testName = "testName";
		profile = Profile.create(testName);
		assertEquals(testName, profile.toString());
	}

	@Test
	void testConstructWithModel() {
		// 'given'
		setAnyProfileConfigurationTo(profile);

		// 'when'
		String name = "copy";
		Profile profile2 = Profile.createFromModel(name, profile);

		// 'then'
		assertEquals(name, profile2.toString());
		assertProfileMatches(profile2);
	}

	@Test
	void testSaveAndLoad() throws IOException {
		// 'given'
		PersistencyDouble persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");
		String anyPath = persistency.getAnyNewPath();

		setAnyProfileConfigurationTo(profile);

		// 'when'
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, anyPath, Program.TECHNICAL_VERSION, 0)) {
			profile.save(writer);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
		Profile profile2 = null;
		try (ISettingsReader reader = TextSettingsReader.createFromFile(persistency, anyPath, Program.TECHNICAL_VERSION)) {
			profile2 = Profile.createFromSettings(reader, "");
		} catch (IOException e) {
			fail(e.getMessage());
		}

		persistency.deleteFile(anyPath);
		
		// 'then'
		assertProfileMatches(profile2);
	}

	@Test
	void testSaveEqualAndChangedProfile() throws IOException {
		// 'given' 1
		PersistencyDouble persistency = PersistencyDouble.create();
		FileSystemDouble fileSystem = (FileSystemDouble)persistency.getFileSystem(); 
		Program.initialize(persistency, "");
		String anyPath = persistency.getAnyNewPath();

		setAnyProfileConfigurationTo(profile);

		// save the profile the first time
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, anyPath, Program.TECHNICAL_VERSION, 0)) {
			profile.save(writer);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		int writeCallCount = fileSystem.getWriteAllBytesCallCount();
		
		// 'when' 1: save the profile again without changing it
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, anyPath, Program.TECHNICAL_VERSION, 0)) {
			profile.save(writer);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
		// 'then' 1: expect the number of calls to IFileSystem.writeAllBytesToFile to be unchanged, because the profile was not changed
		assertEquals(writeCallCount, fileSystem.getWriteAllBytesCallCount());

		// 'given' 2: change the profile configuration again
		Rule anyRule = profile.getRule(RuleID.ALIGN_ALIASES_FOR);
		anyRule.isActive = !anyRule.isActive;

		// 'when' 2
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistency, anyPath, Program.TECHNICAL_VERSION, 0)) {
			profile.save(writer);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
		// 'then' 2: expect the number of calls to IFileSystem.writeAllBytesToFile to be higher now
		assertTrue(writeCallCount < fileSystem.getWriteAllBytesCallCount());
	}

	@Test
	void testRuleGroup() {
		for (int i = 0; i < Rule.RULE_GROUP_COUNT; ++i) {
			RuleGroup ruleGroup = profile.getRuleGroup(RuleGroupID.forValue(i));
			assertTrue(ruleGroup.toString().length() > 0);
		}
	}
	
	@Test
	void testRulesDocumentation() {
		Program.setLogForTesting();
		
		RuleDocumentation ruleDoc = new RuleDocumentation(profile.getRulesSortedByName());
		
		// ensure that the overview page can be created without error
		String overviewMarkdown = ruleDoc.createRulesOverview();
		assertTrue(overviewMarkdown.length() > 0);
		
		// ensure that the rule details pages can be created without error
		for (int ruleIndex = 0; ruleIndex < ruleDoc.getRuleCount(); ++ruleIndex) {
			String ruleMarkdown = ruleDoc.createRuleDetails(ruleIndex);
			assertTrue(ruleMarkdown .length() > 0);
		}
	}

	@Test
	void testRulesDocumentationError() {
		try {
			new RuleDocumentation(null);
			fail();
		} catch(IllegalArgumentException ex) {
		}

		RuleDocumentation ruleDoc = new RuleDocumentation(profile.getRulesSortedByName());
		try {
			ruleDoc.createRuleDetails(-1);
			fail();
		} catch(IllegalArgumentException ex) {
		}

		try {
			ruleDoc.getRuleDocumentationFileName(-1);
			fail();
		} catch(IllegalArgumentException ex) {
		}
	}
	
	@Test
	void testDefaultProfile() {
		Profile profile = Profile.createDefault();
		assertTrue(profile.autoActivateNewFeatures);
		for (Rule rule : profile.getAllRules()) {
			assertEquals(rule.isActiveByDefault(), rule.isActive);
		}
	}
	
	@Test
	void testEssentialProfile() {
		Profile profile = Profile.createEssential();
		assertFalse(profile.autoActivateNewFeatures);
		for (Rule rule : profile.getAllRules()) {
			assertEquals(rule.isEssential(), rule.isActive);
		}
	}

	@Test 
	void testGetRuleCount() {
		assertEquals(Rule.RULE_COUNT, profile.getRuleCount());
	}
	
	private void prepareProfiles(String dir) {
		prepareProfiles(dir, true);
	}
	
	private void prepareProfiles(String dir, boolean createEssentialProfile) {
		Profile profile = Profile.createDefault();
		profile.save(dir);
		
		if (createEssentialProfile) {
			profile = Profile.createEssential();
			profile.save(dir);
		}

		profile = Profile.create(anyProfileName);
		profile.deactivateAllRules();
		profile.save(dir);
	}

	private void assertNullOrEmpty(String[] actArray) {
		assertTrue(actArray == null || actArray.length == 0);
	}

	private boolean profilesContain(ArrayList<Profile> profiles, String expName) {
		for (Profile profile : profiles) {
			if (profile.name.equals(expName)) {
				assertEquals(expName, profile.getNameWithoutPrefix());
				return true;
			}
		}
		return false;
	}

	private boolean profilesContain(ArrayList<Profile> profiles, String expTeamName, String expFileName) {
		String expName = expTeamName + Profile.READ_ONLY_INFIX + expFileName;
		for (Profile profile : profiles) {
			if (profile.name.equals(expName)) {
				// also check the "name without prefix"
				assertEquals(expFileName, profile.getNameWithoutPrefix());
				return true;
			}
		}
		return false;
	}

	@Test
	void testLoadProfiles() {
		prepareProfiles(profilesDir);
		prepareProfiles(readOnlyDirA, true);
		prepareProfiles(readOnlyDirB, false);
		
		// create read-only (team) profile dirs and fill them with more profiles
		ArrayList<ProfileDir> readOnlyProfileDirs = new ArrayList<>();
		readOnlyProfileDirs.add(new ProfileDir(teamNameA, readOnlyDirA));
		readOnlyProfileDirs.add(new ProfileDir(teamNameB, readOnlyDirB));

		// ensure that all 8 profiles are loaded, including from the read-only folders
		ArrayList<Profile> profiles = Profile.loadProfiles(profilesDir, readOnlyProfileDirs);
		assertEquals(8, profiles.size());
		assertTrue(profilesContain(profiles, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, Profile.ESSENTIAL_NAME));
		assertTrue(profilesContain(profiles, anyProfileName));
		assertTrue(profilesContain(profiles, teamNameA, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, teamNameA, Profile.ESSENTIAL_NAME));
		assertTrue(profilesContain(profiles, teamNameA, anyProfileName));
		assertTrue(profilesContain(profiles, teamNameB, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, teamNameB, anyProfileName));

		// ensure that in an empty directory, the 'default' and 'essential' profiles are created
		profiles = Profile.loadProfiles(anyDir, null);
		assertEquals(2, profiles.size());
		assertTrue(profilesContain(profiles, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, Profile.ESSENTIAL_NAME));
		assertFalse(profilesContain(profiles, anyProfileName));
	}
	
	@Test 
	void testUpdateReadOnlyProfiles() {
		prepareProfiles(profilesDir);
		prepareProfiles(readOnlyDirA, true);
		prepareProfiles(readOnlyDirB, false);
		
		ArrayList<ProfileDir> readOnlyProfileDirs = new ArrayList<>();
		readOnlyProfileDirs.add(new ProfileDir(teamNameA, readOnlyDirA));

		// load "own" profiles and profiles from the read-only folder of team A
		ArrayList<Profile> profiles = Profile.loadProfiles(profilesDir, readOnlyProfileDirs);
		
		// now update the profile list, with different read-only profile dirs (team A removed, team B added)
		ArrayList<ProfileDir> newReadOnlyProfileDirs = new ArrayList<>();
		newReadOnlyProfileDirs.add(new ProfileDir(teamNameB, readOnlyDirB));
		Profile.updateReadOnlyProfiles(profiles, newReadOnlyProfileDirs);

		// ensure that now 5 profiles are loaded, including from the read-only folder of team B (but not team A)
		assertEquals(5, profiles.size());
		assertTrue(profilesContain(profiles, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, Profile.ESSENTIAL_NAME));
		assertTrue(profilesContain(profiles, anyProfileName));
		assertTrue(profilesContain(profiles, teamNameB, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, teamNameB, anyProfileName));
	}
	
	@Test 
	void testUnknownReadOnlyFolder() {
		prepareProfiles(profilesDir);
		prepareProfiles(readOnlyDirA, true);
		
		ArrayList<ProfileDir> readOnlyProfileDirs = new ArrayList<>();
		readOnlyProfileDirs.add(new ProfileDir(teamNameB, readOnlyDirB));

		// load "own" profiles and profiles from a read-only folder which does not exist
		ArrayList<Profile> profiles = Profile.loadProfiles(profilesDir, readOnlyProfileDirs);
		
		// ensure that 3 profiles are loaded
		assertEquals(3, profiles.size());
		assertTrue(profilesContain(profiles, Profile.DEFAULT_NAME));
		assertTrue(profilesContain(profiles, Profile.ESSENTIAL_NAME));
		assertTrue(profilesContain(profiles, anyProfileName));
	}

	@Test
	void testAddAndSaveEssentialProfile() throws IOException {
		// ensure that on an empty directory, nothing is added
		assertTrue(Profile.addAndSaveEssentialProfile(profilesDir));
		assertNullOrEmpty(persistency.getFilesInDirectory(profilesDir, null));

		// ensure that on a non-empty directory, the 'essential' profile is added if missing
		prepareProfiles(profilesDir, false);
		assertFalse(persistency.fileExists(essentialProfilePath));
		assertTrue(Profile.addAndSaveEssentialProfile(profilesDir));
		assertTrue(persistency.fileExists(essentialProfilePath));

		// ensure that the 'essential' profile is not overwritten if it already exists
		String testContent = "test";
		persistency.writeAllTextToFile(essentialProfilePath, testContent);
		assertFalse(Profile.addAndSaveEssentialProfile(profilesDir));
		assertEquals(testContent, persistency.readAllTextFromFile(essentialProfilePath));
	}

	@Test
	void testGetLoadPaths() throws IOException {
		assertNullOrEmpty(Profile.getLoadPaths(null));
		assertNullOrEmpty(Profile.getLoadPaths(profilesDir)); 
		assertNullOrEmpty(Profile.getLoadPaths(anyDir)); 

		prepareProfiles(anyDir);
		assertNullOrEmpty(Profile.getLoadPaths(null));
		assertNullOrEmpty(Profile.getLoadPaths(profilesDir));
		assertEquals(3, Profile.getLoadPaths(anyDir).length);
		
		prepareProfiles(profilesDir);
		assertUnorderedStringArrayEquals(new String[] {defaultProfilePath, essentialProfilePath, anyProfilePath}, Profile.getLoadPaths(null));
		assertUnorderedStringArrayEquals(new String[] {defaultProfilePath, essentialProfilePath, anyProfilePath}, Profile.getLoadPaths(profilesDir));
	}

	@Test
	void testGetNumberOfSavedProfiles() throws IOException {
		assertEquals(0, Profile.getNumberOfSavedProfiles(null));
		assertEquals(0, Profile.getNumberOfSavedProfiles(profilesDir));
		assertEquals(0, Profile.getNumberOfSavedProfiles(anyDir));

		prepareProfiles(anyDir);
		assertEquals(0, Profile.getNumberOfSavedProfiles(null));
		assertEquals(0, Profile.getNumberOfSavedProfiles(profilesDir));
		assertEquals(3, Profile.getNumberOfSavedProfiles(anyDir));
		
		prepareProfiles(profilesDir);
		assertEquals(3, Profile.getNumberOfSavedProfiles(null));
		assertEquals(3, Profile.getNumberOfSavedProfiles(profilesDir));
	}
}
