package com.sap.adt.abapcleaner.rulehelpers;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ISettingsReader;
import com.sap.adt.abapcleaner.base.ISettingsWriter;
import com.sap.adt.abapcleaner.base.TextSettingsReader;
import com.sap.adt.abapcleaner.base.TextSettingsWriter;
import com.sap.adt.abapcleaner.programbase.FileType;
import com.sap.adt.abapcleaner.programbase.PersistencyDouble;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rules.prettyprinter.CamelCaseNameRule;

public class CustomCamelCaseNamesTest {
	// creates a persistency double to test multiple objects:
	// - CustomCamelCaseNames
	// - Profile.updateCustomNames()
	// - CamelCaseNameRule.buttonClicked()
	
	private static final String LINE_SEP = System.lineSeparator();

	private static final String viewName1 = "I_AnyView";
	private static final String viewName2 = "I_OtherView";
	private static final String viewName3 = "I_ThirdView";
	private static final String invalidViewName1 = "I_ANYINVALIDNAME";
	private static final String invalidViewName2 = "c_otherinvalidname";
	
	private static final String fieldName1 = "AnyField";
	private static final String fieldName2 = "OtherField";
	private static final String fieldName3 = "ThirdField";
	private static final String invalidFieldName1 = "ANYINVALIDNAME";
	private static final String invalidFieldName2 = "otherinvalidname";

	private static final String comment1 = "* comment";
	private static final String comment2 = "\" comment";
	private static final String comment3 = "# comment";
	
	private static long timestamp1 = 1;
	private static long timestamp2 = 2;
	
	private PersistencyDouble persistencyDouble;
	private String profileDir;
	private Profile profile;
	private CamelCaseNameRule camelCaseNameRule; 
	
	private CustomCamelCaseNames customViewNames; // same instance as this.profile.customViewNames
	private CustomCamelCaseNames customFieldNames; // same instance as this.profile.customFieldNames
	
	private String viewNamesPath = null;
	private String fieldNamesPath = null;
	
	@BeforeEach
	void setUp() {
		// create a persistency double for the files of a profile and its CustomViewNames.txt and CustomFieldNames.txt
		persistencyDouble = PersistencyDouble.create();
		Program.initialize(persistencyDouble, "");
		String defaultProfilePath = persistencyDouble.getSavePath(FileType.PROFILE_TEXT, Profile.DEFAULT_NAME);
		profileDir = persistencyDouble.getDirectoryName(defaultProfilePath);

		// create and 'save' a default profile
		Profile defaultProfile = Profile.createDefault();
		try (ISettingsWriter writer = TextSettingsWriter.createForFile(persistencyDouble, defaultProfilePath, Program.TECHNICAL_VERSION, 0)) {
			defaultProfile.save(writer);
		} catch (IOException e) {
			fail(e.getMessage());
		}
		
		// read the profile again to ensure that this.profile knows its path
		try (ISettingsReader reader = TextSettingsReader.createFromFile(persistencyDouble, defaultProfilePath, Program.TECHNICAL_VERSION)) {
			profile = Profile.createFromSettings(reader, "");
		} catch (IOException e) {
			fail(e.getMessage());
		}
		camelCaseNameRule = (CamelCaseNameRule)profile.getRule(RuleID.CAMEL_CASE_NAME);
		
		customViewNames = profile.customViewNames; 
		customFieldNames = profile.customFieldNames; 
	}
	
	private void prepareFiles(String viewNames, String fieldNames) {
		prepareFiles(viewNames, fieldNames, timestamp1);	
	}
	
	private String getViewNamesPath(CamelCaseNameRule camelCaseNameRule) {
		String viewNamesFolderFile = camelCaseNameRule.configCustomViewNamesFile.getValue();
		return persistencyDouble.combinePaths(profileDir, viewNamesFolderFile);
	}

	private String getFieldNamesPath(CamelCaseNameRule camelCaseNameRule) {
		String fieldNamesFolderFile = camelCaseNameRule.configCustomFieldNamesFile.getValue();
		return persistencyDouble.combinePaths(profileDir, fieldNamesFolderFile);
	}

	private void prepareFiles(String viewNames, String fieldNames, long lastModified) {
		// 'save' the [New]CustomViewNames.txt file to the persistency double
		if (viewNames != null) {
			viewNamesPath = getViewNamesPath(camelCaseNameRule);
			persistencyDouble.prepareFile(viewNamesPath, viewNames);
			persistencyDouble.setLastModified(viewNamesPath, lastModified);
		} else {
			viewNamesPath = null;
		}

		// 'save' the [New]CustomFieldNames.txt file to the persistency double
		if (fieldNames != null) {
			fieldNamesPath = getFieldNamesPath(camelCaseNameRule);
			persistencyDouble.prepareFile(fieldNamesPath, fieldNames);
			persistencyDouble.setLastModified(fieldNamesPath, lastModified);
		} else {
			fieldNamesPath = null;
		}
		
		// make the profile update its custom names 
		profile.updateCustomNames();
	}

	private void assertFound(CustomCamelCaseNames customNames, String name) {
		assertEquals(name, customNames.applyCamelCaseTo(name));
		assertEquals(name, customNames.applyCamelCaseTo(name.toLowerCase()));
		assertEquals(name, customNames.applyCamelCaseTo(name.toUpperCase()));
	}
	
	private void assertNotFound(CustomCamelCaseNames customNames, String name) {
		assertNull(customNames.applyCamelCaseTo(name));
		assertNull(customNames.applyCamelCaseTo(name.toLowerCase()));
		assertNull(customNames.applyCamelCaseTo(name.toUpperCase()));
	}
	
	@Test
	void testTwoNames() {
		prepareFiles(viewName1 + LINE_SEP + viewName2, 
						 fieldName1 + LINE_SEP + fieldName2 + LINE_SEP);
		
		for (int loop = 0; loop < 2; ++loop) {
			assertFound(customViewNames, viewName1);
			assertFound(customViewNames, viewName2);
			assertNotFound(customViewNames, viewName3);
	
			assertFound(customFieldNames, fieldName1);
			assertFound(customFieldNames, fieldName2);
			assertNotFound(customFieldNames, fieldName3);
	
			// reload custom names before changing the file
			profile.updateCustomNames();
		}

		// update files with a new time stamp and reload custom names
		prepareFiles(viewName1 + LINE_SEP + viewName3, fieldName3 + LINE_SEP + fieldName2 + LINE_SEP, timestamp2);

		assertFound(customViewNames, viewName1);
		assertNotFound(customViewNames, viewName2);
		assertFound(customViewNames, viewName3);
		
		assertNotFound(customFieldNames, fieldName1);
		assertFound(customFieldNames, fieldName2);
		assertFound(customFieldNames, fieldName3);

		// use different files and reload custom names
		camelCaseNameRule.configCustomViewNamesFile.setValue("NewCustomViewNames.txt");
		camelCaseNameRule.configCustomFieldNamesFile.setValue("NewCustomFieldNames.txt");
		prepareFiles(viewName2 + LINE_SEP + viewName3, LINE_SEP + fieldName1 + LINE_SEP + fieldName3, timestamp2);

		assertNotFound(customViewNames, viewName1);
		assertFound(customViewNames, viewName2);
		assertFound(customViewNames, viewName3);
		
		assertFound(customFieldNames, fieldName1);
		assertNotFound(customFieldNames, fieldName2);
		assertFound(customFieldNames, fieldName3);
	}
	
	@Test
	void testTwoNamesWithSpacesAndComments() {
		prepareFiles(comment1 + LINE_SEP + "   " + viewName1 + " " + LINE_SEP + viewName2 + comment2 + LINE_SEP + " " + comment3, 
						 fieldName1 + "   " + comment1 + LINE_SEP + comment2 + LINE_SEP + "  " + fieldName2 + comment3 + LINE_SEP);
		
		assertFound(customViewNames, viewName1);
		assertFound(customViewNames, viewName2);
		assertNotFound(customViewNames, viewName3);

		assertFound(customFieldNames, fieldName1);
		assertFound(customFieldNames, fieldName2);
		assertNotFound(customFieldNames, fieldName3);
	}
	
	@Test
	void testParametersNull() {
		CustomCamelCaseNames customViewNames = new CustomCamelCaseNames(CamelCaseNameType.VIEW); 
		CustomCamelCaseNames customFieldNames = new CustomCamelCaseNames(CamelCaseNameType.FIELD);
		try {
			customViewNames.load(null, "AnyFile.txt");
			customFieldNames.load(profileDir, null);
		} catch (IOException e) {
			fail();
		}
		assertNotFound(customViewNames, viewName1);
		assertNotFound(customFieldNames, fieldName1);
	}
	
	@Test
	void testInvalidNames() {
		prepareFiles(viewName1 + LINE_SEP + LINE_SEP + invalidViewName1 + LINE_SEP + invalidViewName2, 
						 LINE_SEP + comment1 + LINE_SEP + invalidFieldName1 + LINE_SEP + fieldName1 + LINE_SEP + invalidFieldName2);

		assertFound(customViewNames, viewName1);
		assertNotFound(customViewNames, invalidViewName1);
		assertNotFound(customViewNames, invalidViewName2);
		
		assertFound(customFieldNames, fieldName1);
		assertNotFound(customFieldNames, invalidFieldName1);
		assertNotFound(customFieldNames, invalidFieldName2);
	}
	
	@Test
	void testFileDoesNotExist() {
		prepareFiles(null, null, timestamp1);
		assertNotFound(customViewNames, viewName1);
		assertNotFound(customFieldNames, fieldName1);
	}
	
	@Test
	void testButtonClickedOnExistingFiles() {
		prepareFiles(viewName1, fieldName1);
		
		assertNull(camelCaseNameRule.buttonClicked(null));
		assertEquals(viewNamesPath, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomViewNamesFile));
		assertEquals(fieldNamesPath, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomFieldNamesFile));
		
		// for a read-only synchronized profile, expect the profiles folder to be returned instead of a path to a file
		profile.isReadOnly = true;
		assertEquals(profileDir, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomViewNamesFile));
		assertEquals(profileDir, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomFieldNamesFile));
	}
	
	@Test
	void testButtonClickedToCreateFiles() {
		String expViewNamesPath = getViewNamesPath(camelCaseNameRule);
		String expFieldNamesPath = getFieldNamesPath(camelCaseNameRule);

		// as no prepare() was called, expect the Custom...Names.txt to not exist
		assertFalse(persistencyDouble.fileExists(expViewNamesPath));
		assertFalse(persistencyDouble.fileExists(expFieldNamesPath));
		
		// expect buttonClicked() to return the Custom...Names.txt paths 
		assertEquals(expViewNamesPath, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomViewNamesFile));
		assertEquals(expFieldNamesPath, camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomFieldNamesFile));

		// after .buttonClicked, expect the Custom...Names.txt to exist
		assertTrue(persistencyDouble.fileExists(expViewNamesPath));
		assertTrue(persistencyDouble.fileExists(expFieldNamesPath));
	}
	
	@Test
	void testButtonClickedOnProfileWithoutPath() {
		Profile defaultProfile = Profile.createDefault();
		CamelCaseNameRule camelCaseNameRule = (CamelCaseNameRule)defaultProfile.getRule(RuleID.CAMEL_CASE_NAME);

		assertNull(camelCaseNameRule.buttonClicked(null));
		assertNull(camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomViewNamesFile));
		assertNull(camelCaseNameRule.buttonClicked(camelCaseNameRule.configCustomFieldNamesFile));
	}
}
