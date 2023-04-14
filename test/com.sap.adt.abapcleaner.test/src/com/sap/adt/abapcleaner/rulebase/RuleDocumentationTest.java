package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.MarkdownBuilder;
import com.sap.adt.abapcleaner.programbase.PersistencyDouble;
import com.sap.adt.abapcleaner.programbase.Program;

public class RuleDocumentationTest {
	private static final String callMethodRuleDocumentation = "CallMethodRuleDocumentation";
	private static final String translateRuleDocumentation = "TranslateRuleDocumentation";
	private static final String obsoleteRuleDocumentation = "ObsoleteRuleDocumentation";
	
	private Rule[] rules;
	private RuleDocumentation ruleDocumentation;
	
	private PersistencyDouble persistency;
	private String docsDir;
	private String rulesDir;
	private String rulesSubDir;
	
	private String callMethodRulePath;
	private String translateRulePath;
	private String obsoleteRulePath;
	private String anySubDirPath;
	
	@BeforeEach
	void setUp() throws Exception {
		Profile profile = Profile.createDefault();
		rules = new Rule[] { profile.getRule(RuleID.CALL_METHOD), profile.getRule(RuleID.TRANSLATE) };
		ruleDocumentation = new RuleDocumentation(rules);
		
		persistency = PersistencyDouble.create();
		Program.initialize(persistency, "");
		docsDir = persistency.prepareDirectory(persistency.getWorkDir(), RuleDocumentation.DOCS_FOLDER);
		rulesDir = persistency.prepareDirectory(docsDir, RuleDocumentation.RULES_FOLDER);
		
		// prepare a subfolder and a file in it to ensure subfolders are not touched
		rulesSubDir = persistency.prepareDirectory(rulesDir, "any_folder");
		anySubDirPath = persistency.prepareFile(rulesSubDir, "any_file" + MarkdownBuilder.MARKDOWN_EXTENSION, "any content");
		
		callMethodRulePath = persistency.combinePaths(rulesDir, "CallMethodRule" + MarkdownBuilder.MARKDOWN_EXTENSION);
		translateRulePath = persistency.combinePaths(rulesDir, "TranslateRule" + MarkdownBuilder.MARKDOWN_EXTENSION);
		obsoleteRulePath = persistency.combinePaths(rulesDir, "ObsoleteRule" + MarkdownBuilder.MARKDOWN_EXTENSION);
	}
	
	@Test 
	void testCreateError() {
		boolean exceptionThrown = false;
		try {
			new RuleDocumentation(null);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);

		exceptionThrown = false;
		try {
			new RuleDocumentation(new Rule[] {});
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}

	private void prepareMarkdownFiles() {
		persistency.prepareFile(callMethodRulePath, callMethodRuleDocumentation);
		persistency.prepareFile(translateRulePath, translateRuleDocumentation);
		persistency.prepareFile(obsoleteRulePath, obsoleteRuleDocumentation);
	}
	
	private void assertNullOrEmpty(String[] array) {
		assertTrue(array == null || array.length == 0);
	}
	
	@Test
	void testDeleteOldRuleDocs() {
		// first test on an empty directory, then test on a directory with 3 files
		for (int pass = 0; pass < 2; ++pass) {
			// 'when'
			ruleDocumentation.deleteOldRuleDocs(docsDir, persistency);

			// 'then'
			// expect the files in the directory to be deleted
			assertNullOrEmpty(persistency.getFilesInDirectory(rulesDir, null));
			
			// ensure that the subfolder and the file in it still exists
			assertEquals(1, persistency.getFilesInDirectory(rulesDir, null, true).length);
			assertTrue(persistency.directoryExists(rulesSubDir));
			assertTrue(persistency.fileExists(anySubDirPath));
			
			// 'given' for second pass: test on a directory with 3 files
			prepareMarkdownFiles();
		}
		
		persistency.clear(); 
		ruleDocumentation.deleteOldRuleDocs(docsDir, persistency);
		assertNullOrEmpty(persistency.getFilesInDirectory(rulesDir, null));
	}
	
	@Test
	void testCreate() {
		assertNullOrEmpty(persistency.getFilesInDirectory(rulesDir, null));

		ruleDocumentation.create(docsDir, persistency);
		
		assertTrue(persistency.fileExists(docsDir, RuleDocumentation.RULES_FILE));
		assertTrue(persistency.fileExists(callMethodRulePath));
		assertTrue(persistency.fileExists(translateRulePath));

		assertEquals(1, persistency.getFilesInDirectory(docsDir, null).length);
		assertEquals(2, persistency.getFilesInDirectory(rulesDir, null).length);
	}
	
	@Test
	void testCreateRuleDetailsError() {
		boolean exceptionThrown = false;
		try {
			ruleDocumentation.createRuleDetails(-1);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);

		exceptionThrown = false;
		try {
			ruleDocumentation.createRuleDetails(rules.length);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
	
	@Test
	void testGetRuleDocumentationFileNameError() {
		boolean exceptionThrown = false;
		try {
			ruleDocumentation.getRuleDocumentationFileName(-1);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);

		exceptionThrown = false;
		try {
			ruleDocumentation.getRuleDocumentationFileName(rules.length);
		} catch (IllegalArgumentException e) {
			exceptionThrown = true;
		}
		assertTrue(exceptionThrown);
	}
}
