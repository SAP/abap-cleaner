package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

class AlignMethodsRedefinitionTest extends RuleTestBase {
	private AlignMethodsRedefinitionRule rule;
	
	AlignMethodsRedefinitionTest() {
		super(RuleID.ALIGN_METHODS_REDEFINITION);
		rule = (AlignMethodsRedefinitionRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.ALWAYS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.ALWAYS);
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(true);
	}
	
	@Test
	void testSingleDeclarations() {
		buildSrc("    METHODS method_name");
		buildSrc("    REDEFINITION. \" comment");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION ##PRAGMA.");
		buildSrc("    METHODS yet_another_method_name");
		buildSrc("    FINAL REDEFINITION. \" other comment");

		buildExp("    METHODS method_name             REDEFINITION.       \" comment");
		buildExp("    METHODS another_method_name     REDEFINITION ##PRAGMA.");
		buildExp("    METHODS yet_another_method_name FINAL REDEFINITION. \" other comment");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChain() {
		buildSrc("    METHODS:");
		buildSrc("      method_name");
		buildSrc("        REDEFINITION,");
		buildSrc("      another_method_name");
		buildSrc("        REDEFINITION,");
		buildSrc("      yet_another_method_name");
		buildSrc("        FINAL REDEFINITION.");

		buildExp("    METHODS: method_name             REDEFINITION,");
		buildExp("             another_method_name     REDEFINITION,");
		buildExp("             yet_another_method_name FINAL REDEFINITION.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(true);
		
		buildSrc("    METHODS method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS yet_another_method_name");
		buildSrc("    FINAL REDEFINITION.");

		buildExp("    METHODS method_name             REDEFINITION.");
		buildExp("    METHODS another_method_name     REDEFINITION.");
		buildExp("");
		buildExp("    METHODS yet_another_method_name FINAL REDEFINITION.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNotAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(false);
		
		buildSrc("    METHODS method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS yet_another_method_name");
		buildSrc("    FINAL REDEFINITION.");

		buildExp("    METHODS method_name         REDEFINITION.");
		buildExp("    METHODS another_method_name REDEFINITION.");
		buildExp("");
		buildExp("    METHODS yet_another_method_name FINAL REDEFINITION.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(true);
		
		buildSrc("    METHODS method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("* comment line");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("* another comment line");
		buildSrc("    METHODS yet_another_method_name");
		buildSrc("    FINAL REDEFINITION.");

		buildExp("    METHODS method_name             REDEFINITION.");
		buildExp("* comment line");
		buildExp("    METHODS another_method_name     REDEFINITION.");
		buildExp("* another comment line");
		buildExp("    METHODS yet_another_method_name FINAL REDEFINITION.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNotAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(false);
		
		buildSrc("    METHODS method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("* comment line");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("* another comment line");
		buildSrc("    METHODS yet_another_method_name");
		buildSrc("    FINAL REDEFINITION.");

		buildExp("    METHODS method_name REDEFINITION.");
		buildExp("* comment line");
		buildExp("    METHODS another_method_name REDEFINITION.");
		buildExp("* another comment line");
		buildExp("    METHODS yet_another_method_name FINAL REDEFINITION.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysContinue() {
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    if_yet_another_interface~any_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~other_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~create");
		buildSrc("      REDEFINITION.");

		buildExp("  METHODS method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS another_method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS: if_yet_another_interface~any_method_name   REDEFINITION,");
		buildExp("           if_yet_another_interface~other_method_name REDEFINITION,");
		buildExp("           if_yet_another_interface~create            REDEFINITION.");

		testRule();
	}

	@Test
	void testKeepContinueAfterKeyword() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    if_yet_another_interface~any_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~other_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~create");
		buildSrc("      REDEFINITION.");

		buildExp("  METHODS method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS another_method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS:");
		buildExp("    if_yet_another_interface~any_method_name   REDEFINITION,");
		buildExp("    if_yet_another_interface~other_method_name REDEFINITION,");
		buildExp("    if_yet_another_interface~create            REDEFINITION.");

		testRule();
	}


	@Test
	void testNeverContinueAfterKeyword() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    if_yet_another_interface~any_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~other_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~create");
		buildSrc("      REDEFINITION.");

		buildExp("  METHODS");
		buildExp("    method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    another_method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS:");
		buildExp("    if_yet_another_interface~any_method_name   REDEFINITION,");
		buildExp("    if_yet_another_interface~other_method_name REDEFINITION,");
		buildExp("    if_yet_another_interface~create            REDEFINITION.");

		testRule();
	}

	@Test
	void testKeepContinueAfterMethodName() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    if_yet_another_interface~any_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~other_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~create");
		buildSrc("      REDEFINITION.");

		buildExp("  METHODS method_name REDEFINITION.");
		buildExp("");
		buildExp("  METHODS another_method_name");
		buildExp("    REDEFINITION.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS: if_yet_another_interface~any_method_name");
		buildExp("             REDEFINITION,");
		buildExp("           if_yet_another_interface~other_method_name");
		buildExp("             REDEFINITION,");
		buildExp("           if_yet_another_interface~create");
		buildExp("             REDEFINITION.");

		testRule();
	}

	@Test
	void testNeverContinueAfterMethodName() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    if_yet_another_interface~any_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~other_method_name");
		buildSrc("      REDEFINITION,");
		buildSrc("    if_yet_another_interface~create");
		buildSrc("      REDEFINITION.");

		buildExp("  METHODS method_name");
		buildExp("    REDEFINITION.");
		buildExp("");
		buildExp("  METHODS another_method_name");
		buildExp("    REDEFINITION.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name");
		buildExp("    FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS: if_yet_another_interface~any_method_name");
		buildExp("             REDEFINITION,");
		buildExp("           if_yet_another_interface~other_method_name");
		buildExp("             REDEFINITION,");
		buildExp("           if_yet_another_interface~create");
		buildExp("             REDEFINITION.");

		testRule();
	}

	@Test
	void testKeepContinueAsIs() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("    METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS another_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("");
		buildSrc("    METHODS");
		buildSrc("    yet_another_method_name FINAL REDEFINITION.");

		buildExp("    METHODS method_name REDEFINITION.");
		buildExp("");
		buildExp("    METHODS another_method_name");
		buildExp("      REDEFINITION.");
		buildExp("");
		buildExp("    METHODS");
		buildExp("      yet_another_method_name FINAL REDEFINITION.");

		testRule();
	}

	@Test
	void testNeverContinue() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS method_name REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS another_method_name");
		buildSrc("  REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FINAL REDEFINITION.");
		buildSrc("");
		buildSrc("  METHODS: if_yet_another_interface~any_method_name   REDEFINITION,");
		buildSrc("           if_yet_another_interface~other_method_name REDEFINITION,");
		buildSrc("           if_yet_another_interface~create            REDEFINITION.");

		buildExp("  METHODS");
		buildExp("    method_name");
		buildExp("      REDEFINITION.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    another_method_name");
		buildExp("      REDEFINITION.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name");
		buildExp("      FINAL REDEFINITION.");
		buildExp("");
		buildExp("  METHODS:");
		buildExp("    if_yet_another_interface~any_method_name");
		buildExp("      REDEFINITION,");
		buildExp("    if_yet_another_interface~other_method_name");
		buildExp("      REDEFINITION,");
		buildExp("    if_yet_another_interface~create");
		buildExp("      REDEFINITION.");

		testRule();
	}

	@Test
	void testSkipNonRedefinitionMethods() {
		buildSrc("  METHODS setup.");
		buildSrc("");
		buildSrc("  METHODS: any_method");
		buildSrc("    IMPORTING iv_param TYPE i.");
		buildSrc("");
		buildSrc("  METHODS other_method FOR TESTING.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testNoSpaceAfterLineBreak() {
		// ensure that a space is added before "REDEFINITION" when it is moved up
		buildSrc("METHODS any_method");
		buildSrc("REDEFINITION.");
		buildSrc("METHODS other_method.");

		buildExp("METHODS any_method REDEFINITION.");
		buildExp("METHODS other_method.");

		testRule();
	}

	@Test
	void testMultiplePeriods() {
		buildSrc("  METHODS if_any_interface~medium_method_name");
		buildSrc("    REDEFINITION.");
		buildSrc("* comment line");
		buildSrc("  METHODS if_any_interface~short_name");
		buildSrc("    REDEFINITION...");
		buildSrc("  METHODS if_any_interface~extra_long_method_name");
		buildSrc("    REDEFINITION..");

		buildExp("  METHODS if_any_interface~medium_method_name     REDEFINITION.");
		buildExp("* comment line");
		buildExp("  METHODS if_any_interface~short_name             REDEFINITION...");
		buildExp("  METHODS if_any_interface~extra_long_method_name REDEFINITION..");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
