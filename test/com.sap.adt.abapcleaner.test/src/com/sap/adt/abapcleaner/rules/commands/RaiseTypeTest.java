package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class RaiseTypeTest extends RuleTestBase {
	RaiseTypeTest() {
		super(RuleID.RAISE_TYPE);
	}

	@Test
	void testRaiseExceptionWithoutExporting() {
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception.");
		buildSrc("");
		buildSrc("    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception.");

		buildExp("    RAISE EXCEPTION NEW cx_any_exception( ).");
		buildExp("");
		buildExp("    RAISE RESUMABLE EXCEPTION NEW cx_any_exception( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExceptionWithExporting() {
		buildSrc("    RAISE EXCEPTION TYPE cx_other_exception");
		buildSrc("      EXPORTING");
		buildSrc("        any_param   = any_value");
		buildSrc("        other_param = other_value.");
		buildSrc("");
		buildSrc("    RAISE SHORTDUMP TYPE cx_demo_t100");
		buildSrc("      EXPORTING");
		buildSrc("        textid = cx_demo_t100=>demo");
		buildSrc("        text1  = 'this'");
		buildSrc("        text2  = 'is'");
		buildSrc("        text3  = 'an'");
		buildSrc("        text4  = 'example'.");

		buildExp("    RAISE EXCEPTION NEW cx_other_exception(");
		buildExp("        any_param   = any_value");
		buildExp("        other_param = other_value ).");
		buildExp("");
		buildExp("    RAISE SHORTDUMP NEW cx_demo_t100(");
		buildExp("        textid = cx_demo_t100=>demo");
		buildExp("        text1  = 'this'");
		buildExp("        text2  = 'is'");
		buildExp("        text3  = 'an'");
		buildExp("        text4  = 'example' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseWithMessageUnchanged() {
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception USING MESSAGE.");
		buildSrc("");
		buildSrc("    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception USING MESSAGE.");
		buildSrc("");
		buildSrc("    RAISE SHORTDUMP TYPE cx_any_exception");
		buildSrc("      MESSAGE ID 'ANY_ID' TYPE 'I' NUMBER '001' WITH 'This' 'is' 'an' 'example'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseTypeWithCommentsAndPragmas() {
		// ensure that the rule works even with a pragma and with comments in weird places  
		buildSrc("    RAISE EXCEPTION \" comment");
		buildSrc("      TYPE \" another comment");
		buildSrc("      cx_any_exception ##ANY_PRAGMA.");
		buildSrc("");
		buildSrc("    RAISE \" comment");
		buildSrc("      RESUMABLE");
		buildSrc("      EXCEPTION");
		buildSrc("* comment line 1");
		buildSrc("* comment line 2");
		buildSrc("      TYPE \" comment");
		buildSrc("      cx_other_exception");
		buildSrc("        EXPORTING");
		buildSrc("          any_param   = any_value");
		buildSrc("          other_param = other_value");
		buildSrc("* comment line 3");
		buildSrc("     .");

		buildExp("    RAISE EXCEPTION \" comment");
		buildExp("      NEW \" another comment");
		buildExp("      cx_any_exception( ) ##ANY_PRAGMA.");
		buildExp("");
		buildExp("    RAISE \" comment");
		buildExp("      RESUMABLE");
		buildExp("      EXCEPTION");
		buildExp("* comment line 1");
		buildExp("* comment line 2");
		buildExp("      NEW \" comment");
		buildExp("      cx_other_exception(");
		buildExp("          any_param   = any_value");
		buildExp("          other_param = other_value");
		buildExp("* comment line 3");
		buildExp("     ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testOldRelease() {
		// expect the code to remain unchanged if the code has an ABAP version < 7.52
		setAbapReleaseOfCode("751");

		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception.");
		buildSrc("    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testReleaseRestriction() {
		// expect the code to remain unchanged if changes are restricted to an ABAP version < 7.52
		setReleaseRestrictionFromUI(751);
		
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception.");
		buildSrc("    RAISE RESUMABLE EXCEPTION TYPE cx_any_exception.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
