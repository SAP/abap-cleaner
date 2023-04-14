package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.base.MarkdownBuilder;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.programbase.Program;

public class RuleDocumentation {
	public static final String DOCS_FOLDER = "docs";
	public static final String RULES_FOLDER = "rules";
	public static final String RULES_FILE = "rules" + MarkdownBuilder.MARKDOWN_EXTENSION;
	
	private Rule[] rules;
	
	public int getRuleCount() { return rules.length; }

	public RuleDocumentation(Rule[] rules) {
		if (rules == null || rules.length == 0) 
			throw new IllegalArgumentException();
		this.rules = rules;	
	}
	
	public void deleteOldRuleDocs(String docsDir, Persistency persistency) {
		String rulesDocDir = persistency.combinePaths(docsDir, RULES_FOLDER);
		if (!persistency.directoryExists(rulesDocDir))
			return;

		// delete old markdown files (in case rules were deleted or renamed) 
		String[] rulePaths = persistency.getFilesInDirectory(rulesDocDir, "*" + MarkdownBuilder.MARKDOWN_EXTENSION, false);
		if (rulePaths != null) {
			for (String rulePath : rulePaths) {
				persistency.deleteFile(rulePath);
			}
		}
	}

	public void create(String docsDir, Persistency persistency) {
		// create required destination directories
		persistency.ensureDirectoryExists(docsDir);
		String rulesDocDir = persistency.combinePaths(docsDir, RULES_FOLDER);
		persistency.ensureDirectoryExists(rulesDocDir);

		// create a list of available cleanup rules
		String overviewMarkdown = createRulesOverview();
		String overviewPath = persistency.combinePaths(docsDir, RULES_FILE);
		persistency.writeAllTextToFile(overviewPath, overviewMarkdown, true);

		// create a page for each rule, showing the information that would also be seen on FrmProfiles
		for (int ruleIndex = 0; ruleIndex < getRuleCount(); ++ruleIndex) {
			String ruleMarkdown = createRuleDetails(ruleIndex);
			String ruleDocFile = getRuleDocumentationFileName(ruleIndex); 
			String ruleDocPath = persistency.combinePaths(rulesDocDir, ruleDocFile);
			persistency.writeAllTextToFile(ruleDocPath, ruleMarkdown , true);
		}
	}

	public String createRulesOverview() {
		MarkdownBuilder mb = MarkdownBuilder.create();
		mb.startNewHeading("Available Cleanup Rules", 1);

		// count configuration options
		int ruleCount = rules.length;
		int configCount = 0;
		for (Rule rule : rules) {
			ConfigValue[] configValues = rule.getConfigValues();
			for (ConfigValue configValue : configValues) {
				if (!(configValue instanceof ConfigInfoValue)) {
					++configCount;
				}
			}
		}
		mb.startNewParagraph();
		mb.appendText(Program.PRODUCT_NAME + " offers " + String.valueOf(ruleCount) + " cleanup rules with a total of " + String.valueOf(configCount) + " configuration options:");
		
		// create list of available cleanup rules
		RuleGroup lastRuleGroup = null;
		for (Rule rule : rules) {
			RuleGroup ruleGroup = rule.getRuleGroup();
			if (lastRuleGroup == null || ruleGroup.iD != lastRuleGroup.iD) {
				mb.startNewHeading(ruleGroup.getName(), 2);
			}
			mb.startNewBullet(1);
			mb.appendLink(rule.getDisplayName(), RULES_FOLDER + "/" + getRuleDocumentationFileName(rule));
			lastRuleGroup = ruleGroup;
		}
		mb.startNewParagraph();
		mb.appendBoldLink("Back to first page", "../README.md");
		mb.finishBuild();
		return mb.toString();
	}
	
	public String createRuleDetails(int ruleIndex) {
		if (ruleIndex < 0 || ruleIndex >= rules.length) 
			throw new IllegalArgumentException();

		final String NAV_PREVIOUS_RULE = "<-- previous rule";
		final String NAV_OVERVIEW = "overview";
		final String NAV_NEXT_RULE = "next rule -->";
		final String NAV_SEPARATOR = " | ";
		
		MarkdownBuilder mb = MarkdownBuilder.create();

		Rule rule = rules[ruleIndex];

		// navigation to "<-- previous rule | overview | next rule -->"
		mb.startNewParagraph();
		if (ruleIndex > 0) {
			mb.appendLink(NAV_PREVIOUS_RULE, getRuleDocumentationFileName(ruleIndex - 1));
		} else {
			mb.appendText(NAV_PREVIOUS_RULE);
		}
		mb.appendText(NAV_SEPARATOR);
		mb.appendLink(NAV_OVERVIEW, "../" + RULES_FILE);
		mb.appendText(NAV_SEPARATOR);
		if (ruleIndex + 1 < rules.length) { 
			mb.appendLink(NAV_NEXT_RULE, getRuleDocumentationFileName(ruleIndex + 1));
		} else {
			mb.appendText(NAV_NEXT_RULE);
		}

		// rule name, description, references, options, example code 
		rule.toDocumentation(mb);
		
		// links to Rule implementation and tests
		mb.startNewHeading("Related code", 2);
		mb.startNewBullet(1);
		mb.appendLink("Rule implementation", getRuleImplementationLink(rule));
		mb.startNewBullet(1);
		mb.appendLink("Tests", getRuleTestClassLink(rule));
		
      mb.finishBuild();
      return mb.toString();
		
	}
	
	public String getRuleDocumentationFileName(int ruleIndex) {
		if (ruleIndex < 0 || ruleIndex >= rules.length) 
			throw new IllegalArgumentException();

		return getRuleDocumentationFileName(rules[ruleIndex]);
	}
	
	private String getRuleDocumentationFileName(Rule rule) {
		String className = rule.getClass().toString();
		int dotPos = className.lastIndexOf('.');
		if (dotPos >= 0)
			className = className.substring(dotPos + 1);
		return className + MarkdownBuilder.MARKDOWN_EXTENSION;
	}

	private String getRuleImplementationLink(Rule rule) {
		final String CLASS_PREFIX = "class "; 
		String fullClassName = rule.getClass().toString();
		if (StringUtil.startsWith(fullClassName, CLASS_PREFIX, false))
			fullClassName = fullClassName.substring(CLASS_PREFIX.length());
		return "../../com.sap.adt.abapcleaner/src/" + fullClassName.replace('.', '/') + ".java";
	}

	private String getRuleTestClassLink(Rule rule) {
		final String CLASS_PREFIX = "class "; 
		String fullClassName = rule.getClass().toString();
		if (StringUtil.startsWith(fullClassName, CLASS_PREFIX, false))
			fullClassName = fullClassName.substring(CLASS_PREFIX.length());
		if (StringUtil.endsWith(fullClassName, "Rule", false))
			fullClassName = fullClassName.substring(0, fullClassName.length() - "Rule".length());
		return "../../test/com.sap.adt.abapcleaner.test/src/" + fullClassName.replace('.', '/') + "Test.java";
	}
}
