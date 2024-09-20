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
		rule.configMoveAcrossCommentLines.setValue(true);
		rule.configExecuteOnClassDefinitionSections.setValue(true);
	}
	
	@Test
	void testSimpleDeclarationsAndCommands() {
		buildSrc("    DATA lv_value TYPE i .");
		buildSrc("");
		buildSrc("    TRY .");
		buildSrc("        any_operation( ) .");
		buildSrc("      CATCH cx_any .");
		buildSrc("    ENDTRY .");

		buildExp("    DATA lv_value TYPE i.");
		buildExp("");
		buildExp("    TRY.");
		buildExp("        any_operation( ).");
		buildExp("      CATCH cx_any.");
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
		rule.configMoveAcrossCommentLines.setValue(false);

		buildSrc("    DATA: lv_value TYPE i");
		buildSrc("    \" comment line");
		buildSrc("    , lv_other_value TYPE string");
		buildSrc("    \" comment line");
		buildSrc("    .");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitOutTrailingCommentLine() {
		buildSrc("    CLEAR");
		buildSrc("      ev_any_value");
		buildSrc("*      comment line");
		buildSrc("    .");

		buildExp("    CLEAR");
		buildExp("      ev_any_value.");
		buildExp("*      comment line");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveAcrossCommentLines() {
		buildSrc("    CLEAR:");
		buildSrc("      ev_any_value  ,");
		buildSrc("      ev_other_value \" line-end comment");
		buildSrc("*      comment line A");
		buildSrc("      , ev_third_value");
		buildSrc("*      comment line B1");
		buildSrc("      \" comment line B2");
		buildSrc("      , ev_fourth_value \" line-end comment");
		buildSrc("*      comment line C1");
		buildSrc("       \" comment line C2");
		buildSrc("    .");

		buildExp("    CLEAR:");
		buildExp("      ev_any_value,");
		buildExp("      ev_other_value, \" line-end comment");
		buildExp("*      comment line A");
		buildExp("      ev_third_value,");
		buildExp("*      comment line B1");
		buildExp("      \" comment line B2");
		buildExp("      ev_fourth_value. \" line-end comment");
		buildExp("*      comment line C1");
		buildExp("    \" comment line C2");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotMoveAcrossCommentLines() {
		rule.configMoveAcrossCommentLines.setValue(false);

		buildSrc("    CLEAR:");
		buildSrc("      ev_any_value  ,");
		buildSrc("      ev_other_value \" line-end comment");
		buildSrc("*      comment line A");
		buildSrc("      , ev_third_value");
		buildSrc("*      comment line B1");
		buildSrc("      \" comment line B2");
		buildSrc("      , ev_fourth_value \" line-end comment");
		buildSrc("*      comment line C1");
		buildSrc("       \" comment line C2");
		buildSrc("    .");

		buildExp("    CLEAR:");
		buildExp("      ev_any_value,");
		buildExp("      ev_other_value \" line-end comment");
		buildExp("*      comment line A");
		buildExp("      , ev_third_value");
		buildExp("*      comment line B1");
		buildExp("      \" comment line B2");
		buildExp("      , ev_fourth_value \" line-end comment");
		buildExp("*      comment line C1");
		buildExp("       \" comment line C2");
		buildExp("    .");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepPeriodAfterAutoGeneratedComment() {
		// ensure that the period is not placed before the specific comment shown in 'any_function', 
		// which was auto-generated when saving a function module with an empty signature; 
		// otherwise, a second comment with the same text would be generated when saving again. 

		buildSrc("FUNCTION any_function");
		buildSrc(" \" You can use the template 'functionModuleParameter' to add here the signature!");
		buildSrc(".");
		buildSrc("");
		buildSrc("  WRITE 'hello world'.");
		buildSrc("ENDFUNCTION.");
		buildSrc("");
		buildSrc("FUNCTION other_function");
		buildSrc(" \" You can use the template 'functionModuleParameter' to add the signature here!");
		buildSrc(".");
		buildSrc("");
		buildSrc("  WRITE 'hello world'.");
		buildSrc("ENDFUNCTION.");

		buildExp("FUNCTION any_function");
		buildExp(" \" You can use the template 'functionModuleParameter' to add here the signature!");
		buildExp(".");
		buildExp("");
		buildExp("  WRITE 'hello world'.");
		buildExp("ENDFUNCTION.");
		buildExp("");
		buildExp("FUNCTION other_function.");
		buildExp("  \" You can use the template 'functionModuleParameter' to add the signature here!");
		buildExp("");
		buildExp("  WRITE 'hello world'.");
		buildExp("ENDFUNCTION.");

		testRule();
	}
}
