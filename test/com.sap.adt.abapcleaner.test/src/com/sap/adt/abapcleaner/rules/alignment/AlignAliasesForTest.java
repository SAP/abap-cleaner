package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignAliasesForTest extends RuleTestBase {
	private AlignAliasesForRule rule;
	
	AlignAliasesForTest() {
		super(RuleID.ALIGN_ALIASES_FOR);
		rule = (AlignAliasesForRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(true);
	}
	
	@Test
	void testWithoutComments() {
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name .");
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name .");
		buildSrc("  ALIASES extra_long_method_name");
		buildSrc("    FOR if_any_interface~extra_long_method_name .");

		buildExp("  ALIASES long_method_name       FOR if_any_interface~long_method_name .");
		buildExp("  ALIASES short_name             FOR if_any_interface~short_name .");
		buildExp("  ALIASES extra_long_method_name FOR if_any_interface~extra_long_method_name .");

		testRule();
	}

	@Test
	void testWithCommentAlign() {
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name .");
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name . \" comment");
		buildSrc("  ALIASES extra_long_method_name");
		buildSrc("    FOR if_any_interface~extra_long_method_name . \" comment");

		buildExp("  ALIASES long_method_name       FOR if_any_interface~long_method_name .");
		buildExp("  ALIASES short_name             FOR if_any_interface~short_name .             \" comment");
		buildExp("  ALIASES extra_long_method_name FOR if_any_interface~extra_long_method_name . \" comment");

		testRule();
	}

	@Test
	void testChain() {
		buildSrc("  ALIASES:");
		buildSrc("    short_name");
		buildSrc("      FOR if_any_interface~short_name,");
		buildSrc("    long_method_name");
		buildSrc("      FOR if_any_interface~long_method_name, \" comment");
		buildSrc("    any_method");
		buildSrc("      FOR if_any_interface~any_method. \" comment");

		buildExp("  ALIASES: short_name       FOR if_any_interface~short_name,");
		buildExp("           long_method_name FOR if_any_interface~long_method_name, \" comment");
		buildExp("           any_method       FOR if_any_interface~any_method.       \" comment");

		testRule();
	}

	@Test
	void testCommentPreventingChange() {
		buildSrc("  ALIASES: \" comment");
		buildSrc("    short_name");
		buildSrc("      FOR if_any_interface~short_name,");
		buildSrc("    long_method_name");
		buildSrc("      FOR if_any_interface~long_method_name,");
		buildSrc("    any_method");
		buildSrc("      FOR if_any_interface~any_method.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAcrossEmptyLine() {
		rule.configAlignAcrossEmptyLines.setValue(true);
		
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name.");
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name.");
		buildSrc("");
		buildSrc("");
		buildSrc("  ALIASES any_method");
		buildSrc("    FOR if_any_interface~any_method.");
		buildSrc("  ALIASES other_method");
		buildSrc("    FOR if_any_interface~other_method.");

		buildExp("  ALIASES short_name       FOR if_any_interface~short_name.");
		buildExp("  ALIASES long_method_name FOR if_any_interface~long_method_name.");
		buildExp("");
		buildExp("");
		buildExp("  ALIASES any_method       FOR if_any_interface~any_method.");
		buildExp("  ALIASES other_method     FOR if_any_interface~other_method.");

		testRule();
	}

	@Test
	void testNotAcrossEmptyLine() {
		rule.configAlignAcrossEmptyLines.setValue(false);
		
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name.");
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name.");
		buildSrc("");
		buildSrc("  ALIASES any_method");
		buildSrc("    FOR if_any_interface~any_method.");
		buildSrc("  ALIASES other_method");
		buildSrc("    FOR if_any_interface~other_method.");

		buildExp("  ALIASES short_name       FOR if_any_interface~short_name.");
		buildExp("  ALIASES long_method_name FOR if_any_interface~long_method_name.");
		buildExp("");
		buildExp("  ALIASES any_method   FOR if_any_interface~any_method.");
		buildExp("  ALIASES other_method FOR if_any_interface~other_method.");

		testRule();
	}

	@Test
	void testAcrossCommentLine() {
		rule.configAlignAcrossCommentLines.setValue(true);
		
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name.");
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name.");
		buildSrc("  \" comment");
		buildSrc("  \" more comment");
		buildSrc("  ALIASES any_method");
		buildSrc("    FOR if_any_interface~any_method.");
		buildSrc("  ALIASES other_method");
		buildSrc("    FOR if_any_interface~other_method.");

		buildExp("  ALIASES short_name       FOR if_any_interface~short_name.");
		buildExp("  ALIASES long_method_name FOR if_any_interface~long_method_name.");
		buildExp("  \" comment");
		buildExp("  \" more comment");
		buildExp("  ALIASES any_method       FOR if_any_interface~any_method.");
		buildExp("  ALIASES other_method     FOR if_any_interface~other_method.");

		testRule();
	}

	@Test
	void testNotAcrossCommentLine() {
		rule.configAlignAcrossCommentLines.setValue(false);
		
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name.");
		buildSrc("  ALIASES long_method_name");
		buildSrc("    FOR if_any_interface~long_method_name.");
		buildSrc("  \" comment");
		buildSrc("  ALIASES any_method");
		buildSrc("    FOR if_any_interface~any_method.");
		buildSrc("  ALIASES other_method");
		buildSrc("    FOR if_any_interface~other_method.");

		buildExp("  ALIASES short_name       FOR if_any_interface~short_name.");
		buildExp("  ALIASES long_method_name FOR if_any_interface~long_method_name.");
		buildExp("  \" comment");
		buildExp("  ALIASES any_method   FOR if_any_interface~any_method.");
		buildExp("  ALIASES other_method FOR if_any_interface~other_method.");

		testRule();
	}

	@Test
	void testMultiplePeriods() {
		buildSrc("  ALIASES medium_method_name");
		buildSrc("    FOR if_any_interface~medium_method_name..");
		buildSrc("  ALIASES short_name");
		buildSrc("    FOR if_any_interface~short_name...");
		buildSrc("  ALIASES extra_long_method_name");
		buildSrc("    FOR if_any_interface~extra_long_method_name.");

		buildExp("  ALIASES medium_method_name     FOR if_any_interface~medium_method_name..");
		buildExp("  ALIASES short_name             FOR if_any_interface~short_name...");
		buildExp("  ALIASES extra_long_method_name FOR if_any_interface~extra_long_method_name.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
