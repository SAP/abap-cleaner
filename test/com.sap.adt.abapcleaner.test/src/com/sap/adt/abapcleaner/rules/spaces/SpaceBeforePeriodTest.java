package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class SpaceBeforePeriodTest extends RuleTestBase {
	private SpaceBeforePeriodRule rule;
	
	SpaceBeforePeriodTest() {
		super(RuleID.SPACE_BEFORE_PERIOD);
		rule = (SpaceBeforePeriodRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnComma.setValue(true);
		rule.configExecuteOnPeriod.setValue(true);
		rule.configExecuteOnClassDefinitionSections.setValue(true);
	}
	
	@Test
	void testSimpleDeclarationsAndCommands() {
		buildSrc("    DATA lv_value TYPE i .");
		buildSrc("");
		buildSrc("    TRY .");
		buildSrc("        any_operation( ) .");
		buildSrc("      CATCH .");
		buildSrc("    ENDTRY .");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    TRY.");
		buildExp("        any_operation( ).");
		buildExp("      CATCH.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChain() {
		buildSrc("    CLEAR:");
		buildSrc("      ev_value ,");
		buildSrc("      ev_other_value");
		buildSrc("    .");

		buildExp("    CLEAR:");
		buildExp("      ev_value,");
		buildExp("      ev_other_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainForPeriodOnly() {
		rule.configExecuteOnComma.setValue(false);

		buildSrc("    CLEAR:");
		buildSrc("      ev_value ,");
		buildSrc("      ev_other_value");
		buildSrc("    .");

		buildExp("    CLEAR:");
		buildExp("      ev_value ,");
		buildExp("      ev_other_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainForCommaOnly() {
		rule.configExecuteOnPeriod.setValue(false);

		buildSrc("    CLEAR:");
		buildSrc("      ev_value ,");
		buildSrc("      ev_other_value");
		buildSrc("    .");

		buildExp("    CLEAR:");
		buildExp("      ev_value,");
		buildExp("      ev_other_value");
		buildExp("    .");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPragma() {
		buildSrc("    DATA lo_object TYPE cl_any_type ##NEEDED .");

		buildExp("    DATA lo_object TYPE cl_any_type ##NEEDED.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLineBreaksWithComments() {
		buildSrc("    lv_value = 3");
		buildSrc("    .");
		buildSrc("");
		buildSrc("    lv_value = 42 \" comment");
		buildSrc("    .");

		buildExp("    lv_value = 3.");
		buildExp("");
		buildExp("    lv_value = 42. \" comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChangeClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(true);

		buildSrc("CLASS tests DEFINITION PUBLIC .");
		buildSrc("  METHODS any_method");
		buildSrc("    FOR TESTING .");
		buildSrc("  METHODS other_methoy");
		buildSrc("    REDEFINITION .");
		buildSrc("  ALIASES:");
		buildSrc("    third_method");
		buildSrc("      FOR if_any_interface~third_method \" comment");
		buildSrc("    , fourth_method");
		buildSrc("      FOR if_any_interface~fourth_method \" comment");
		buildSrc("    .");
		buildSrc("ENDCLASS .");

		buildExp("CLASS tests DEFINITION PUBLIC.");
		buildExp("  METHODS any_method");
		buildExp("    FOR TESTING.");
		buildExp("  METHODS other_methoy");
		buildExp("    REDEFINITION.");
		buildExp("  ALIASES:");
		buildExp("    third_method");
		buildExp("      FOR if_any_interface~third_method, \" comment");
		buildExp("    fourth_method");
		buildExp("      FOR if_any_interface~fourth_method. \" comment");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("CLASS tests DEFINITION PUBLIC .");
		buildSrc("  METHODS any_method");
		buildSrc("    FOR TESTING .");
		buildSrc("  METHODS other_method");
		buildSrc("    REDEFINITION .");
		buildSrc("  ALIASES:");
		buildSrc("    third_method");
		buildSrc("      FOR if_any_interface~third_method . \" comment");
		buildSrc("ENDCLASS .");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCommaAndPeriodAfterCommentLinesUnchanged() {
		buildSrc("    DATA: lv_value TYPE i");
		buildSrc("    \" comment line");
		buildSrc("    , lv_other_value TYPE string");
		buildSrc("    \" comment line");
		buildSrc("    .");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
