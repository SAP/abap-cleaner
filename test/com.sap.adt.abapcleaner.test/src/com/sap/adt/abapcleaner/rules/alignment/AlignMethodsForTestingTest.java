package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

class AlignMethodsForTestingTest extends RuleTestBase {
	private AlignMethodsForTestingRule rule;
	
	AlignMethodsForTestingTest() {
		super(RuleID.ALIGN_METHODS_FOR_TESTING);
		rule = (AlignMethodsForTestingRule)getRule();
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
		buildSrc("    METHODS very_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING ##PRAGMA.");
		buildSrc("    METHODS very_very_long_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS very_long_method_name      FOR TESTING.");
		buildExp("    METHODS any_method_name            FOR TESTING ##PRAGMA.");
		buildExp("    METHODS very_very_long_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSingleDeclarationsWithRaising() {
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING");
		buildSrc("    RAISING cx_error_occurred.");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING");
		buildSrc("    RAISING cx_message cx_error_occurred.");
		buildSrc("    METHODS yet_another_long_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name      FOR TESTING RAISING cx_error_occurred.");
		buildExp("    METHODS any_method_name              FOR TESTING.");
		buildExp("    METHODS extremely_long_method_name   FOR TESTING RAISING cx_message cx_error_occurred.");
		buildExp("    METHODS yet_another_long_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSingleDeclarationsWithOneRaising() {
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING");
		buildSrc("    RAISING cx_error_occurred.");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS yet_another_long_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name      FOR TESTING RAISING cx_error_occurred.");
		buildExp("    METHODS any_method_name              FOR TESTING.");
		buildExp("    METHODS yet_another_long_method_name FOR TESTING.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testInnerCommentLineAtStart() {
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING");
		buildSrc("* comment line");
		buildSrc("    RAISING cx_error_occurred.");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testInnerCommentLineAfterStart() {
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING");
		buildSrc("    \" comment line");
		buildSrc("    RAISING cx_error_occurred.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSingleDeclarationsWithComment() {
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING.    \" comment 1");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING. \" comment 2");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS yet_another_long_method_name");
		buildSrc("    FOR TESTING.             \" comment 3");

		buildExp("    METHODS rather_long_method_name      FOR TESTING. \" comment 1");
		buildExp("    METHODS any_method_name              FOR TESTING. \" comment 2");
		buildExp("    METHODS extremely_long_method_name   FOR TESTING.");
		buildExp("    METHODS yet_another_long_method_name FOR TESTING. \" comment 3");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDeclarationChain() {
		buildSrc("    METHODS:");
		buildSrc("      rather_long_method_name");
		buildSrc("        FOR TESTING,");
		buildSrc("      any_method_name");
		buildSrc("        FOR TESTING,");
		buildSrc("      extremely_long_method_name");
		buildSrc("        FOR TESTING.");

		buildExp("    METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("             any_method_name            FOR TESTING,");
		buildExp("             extremely_long_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDeclarationChainWithComment() {
		buildSrc("    METHODS:");
		buildSrc("      rather_long_method_name");
		buildSrc("        FOR TESTING,");
		buildSrc("      \" comment");
		buildSrc("      \" more comment");
		buildSrc("      any_method_name");
		buildSrc("        FOR TESTING,");
		buildSrc("      extremely_long_method_name");
		buildSrc("        FOR TESTING.");

		buildExp("    METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("      \" comment");
		buildExp("      \" more comment");
		buildExp("             any_method_name            FOR TESTING,");
		buildExp("             extremely_long_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(true);
		
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("");
		buildSrc("");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS other_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name    FOR TESTING.");
		buildExp("    METHODS extremely_long_method_name FOR TESTING.");
		buildExp("");
		buildExp("");
		buildExp("    METHODS any_method_name            FOR TESTING.");
		buildExp("    METHODS other_method_name          FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNotAcrossEmptyLines() {
		rule.configAlignAcrossEmptyLines.setValue(false);
		
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS other_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name    FOR TESTING.");
		buildExp("    METHODS extremely_long_method_name FOR TESTING.");
		buildExp("");
		buildExp("    METHODS any_method_name   FOR TESTING.");
		buildExp("    METHODS other_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(true);
		
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("* comment");
		buildSrc("* more comment");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS other_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name    FOR TESTING.");
		buildExp("    METHODS extremely_long_method_name FOR TESTING.");
		buildExp("* comment");
		buildExp("* more comment");
		buildExp("    METHODS any_method_name            FOR TESTING.");
		buildExp("    METHODS other_method_name          FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNotAcrossCommentLines() {
		rule.configAlignAcrossCommentLines.setValue(false);
		
		buildSrc("    METHODS rather_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS extremely_long_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("* comment");
		buildSrc("    METHODS any_method_name");
		buildSrc("    FOR TESTING.");
		buildSrc("    METHODS other_method_name");
		buildSrc("    FOR TESTING.");

		buildExp("    METHODS rather_long_method_name    FOR TESTING.");
		buildExp("    METHODS extremely_long_method_name FOR TESTING.");
		buildExp("* comment");
		buildExp("    METHODS any_method_name   FOR TESTING.");
		buildExp("    METHODS other_method_name FOR TESTING.");
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithSomeForTestingMethods() {
		buildSrc("    METHODS:");
		buildSrc("      setup,");
		buildSrc("      test_method_a");
		buildSrc("      FOR TESTING RAISING cx_static_check,");
		buildSrc("      helper_method,");
		buildSrc("      test_b");
		buildSrc("      FOR TESTING RAISING cx_static_check.");

		buildExp("    METHODS: setup,");
		buildExp("             test_method_a FOR TESTING RAISING cx_static_check,");
		buildExp("             helper_method,");
		buildExp("             test_b        FOR TESTING RAISING cx_static_check.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainWithImportingAndForTestingMethods() {
		buildSrc("    METHODS:");
		buildSrc("      any_method IMPORTING !iv_value TYPE i,");
		buildSrc("      test_method_a");
		buildSrc("      FOR TESTING RAISING cx_static_check,");
		buildSrc("      test_b");
		buildSrc("      FOR TESTING RAISING cx_static_check.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlwaysContinue() {
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS any_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS other_method_name FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("           any_method_name            FOR TESTING RAISING cx_message,");
		buildExp("           extremely_long_method_name FOR TESTING.");

		testRule();
	}

	@Test
	void testKeepContinueAfterKeyword() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS any_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS other_method_name FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("           any_method_name            FOR TESTING RAISING cx_message,");
		buildExp("           extremely_long_method_name FOR TESTING.");

		testRule();
	}

	@Test
	void testNeverContinueAfterKeyword() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS");
		buildExp("    any_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    other_method_name FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS:");
		buildExp("    rather_long_method_name    FOR TESTING,");
		buildExp("    any_method_name            FOR TESTING RAISING cx_message,");
		buildExp("    extremely_long_method_name FOR TESTING.");

		testRule();
	}

	@Test
	void testKeepContinueAfterMethodName() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS any_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS other_method_name");
		buildExp("    FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("           any_method_name            FOR TESTING RAISING cx_message,");
		buildExp("           extremely_long_method_name FOR TESTING.");

		testRule();
	}

	@Test
	void testNeverContinueAfterMethodName() {
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS any_method_name");
		buildExp("    FOR TESTING.");
		buildExp("");
		buildExp("  METHODS other_method_name");
		buildExp("    FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS yet_another_method_name");
		buildExp("    FOR TESTING.");
		buildExp("");
		buildExp("  METHODS: rather_long_method_name");
		buildExp("             FOR TESTING,");
		buildExp("           any_method_name");
		buildExp("             FOR TESTING RAISING cx_message,");
		buildExp("           extremely_long_method_name");
		buildExp("             FOR TESTING.");

		testRule();
	}

	@Test
	void testKeepContinueAsIs() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS any_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS other_method_name");
		buildExp("    FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name FOR TESTING.");
		buildExp("");
		buildExp("  METHODS: rather_long_method_name    FOR TESTING,");
		buildExp("           any_method_name            FOR TESTING RAISING cx_message,");
		buildExp("           extremely_long_method_name FOR TESTING.");

		testRule();
	}

	@Test
	void testNeverContinue() {
		rule.configContinueAfterKeyword.setEnumValue(ChangeType.NEVER);
		rule.configContinueAfterMethodName.setEnumValue(ChangeType.NEVER);
		rule.configAlignAcrossEmptyLines.setValue(false);

		buildSrc("  METHODS any_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS other_method_name");
		buildSrc("  FOR TESTING RAISING cx_message.");
		buildSrc("");
		buildSrc("  METHODS");
		buildSrc("  yet_another_method_name FOR TESTING.");
		buildSrc("");
		buildSrc("  METHODS: rather_long_method_name    FOR TESTING,");
		buildSrc("           any_method_name            FOR TESTING RAISING cx_message,");
		buildSrc("           extremely_long_method_name FOR TESTING.");

		buildExp("  METHODS");
		buildExp("    any_method_name");
		buildExp("      FOR TESTING.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    other_method_name");
		buildExp("      FOR TESTING RAISING cx_message.");
		buildExp("");
		buildExp("  METHODS");
		buildExp("    yet_another_method_name");
		buildExp("      FOR TESTING.");
		buildExp("");
		buildExp("  METHODS:");
		buildExp("    rather_long_method_name");
		buildExp("      FOR TESTING,");
		buildExp("    any_method_name");
		buildExp("      FOR TESTING RAISING cx_message,");
		buildExp("    extremely_long_method_name");
		buildExp("      FOR TESTING.");

		testRule();
	}

	@Test
	void testSkipNonTestingMethods() {
		buildSrc("  METHODS setup.");
		buildSrc("");
		buildSrc("  METHODS: any_method");
		buildSrc("    IMPORTING iv_param TYPE i.");
		buildSrc("");
		buildSrc("  METHODS other_method REDEFINITION.");

		copyExpFromSrc();

		testRule();
	}
}
