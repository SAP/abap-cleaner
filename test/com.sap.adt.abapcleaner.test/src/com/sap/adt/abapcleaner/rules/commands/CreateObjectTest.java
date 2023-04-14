package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class CreateObjectTest extends RuleTestBase {
	CreateObjectTest() {
		super(RuleID.CREATE_OBJECT);
	}
	
	@Test
	void testSimpleCreate() {
		buildSrc("    CREATE OBJECT lx_message.");

		buildExp("    lx_message = NEW #( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreateWithTypeButNoParentheses() {
		buildSrc("    CREATE OBJECT lo_instance TYPE cl_any_class.");
		
		buildExp("    lo_instance = NEW cl_any_class( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreateWithTypeAndParameters() {
		// expect parentheses to be added, and the EXPORTING keyword to be removed (leaving it would be a syntax error!)

		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent         = me");
		buildSrc("        io_msg_handler    = mo_msg_handler.");

		buildExp("    mo_instance = NEW cl_any_class(");
		buildExp("        io_parent         = me");
		buildExp("        io_msg_handler    = mo_msg_handler ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDynamicCallUnchanged() {
		// expect the following to remain unchanged, because the type is determined dynamically

		buildSrc("    CREATE OBJECT rx_excp TYPE (iv_exception_class)");
		buildSrc("      EXPORTING");
		buildSrc("        mv_msgid   = iv_msgid");
		buildSrc("        mv_msgno   = iv_msgno.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreateObjectForTestingUnchanged() {
		// expect the following RAP-specific variant to remain unchanged
		
		buildSrc("    create object cl_testing for testing.");
		 
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallWithExceptionsUnchanged() {
		// expect the following to remain unchanged, because EXCEPTIONS are not possible with NEW
		
		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_msg_handler = mo_msg_handler");
		buildSrc("      EXCEPTIONS");
		buildSrc("        cx_any_assert = 1");
		buildSrc("        others        = 2.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		buildSrc("    CREATE OBJECT:lo_any_object,");
		buildSrc("                  lo_other_object.");
   	buildSrc("    CALL OBJECT lo_any_object");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = : 1, 2.");

	   copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreatePragmaAtEnd() {
		// expect the pragmas to remain outside the parentheses

		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent         = me");
		buildSrc("        io_msg_handler    = mo_msg_handler ##ANY_PRAGMA ##OTHER_PRAGMA.");

		buildExp("    mo_instance = NEW cl_any_class(");
		buildExp("        io_parent         = me");
		buildExp("        io_msg_handler    = mo_msg_handler ) ##ANY_PRAGMA ##OTHER_PRAGMA.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

}
