package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class CreateObjectTest extends RuleTestBase {
	private CreateObjectRule rule;
	
	CreateObjectTest() {
		super(RuleID.CREATE_OBJECT);
		rule = (CreateObjectRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configProcessChains.setValue(true);
	}

	@Test
	void testOldAbapRelease() {
		// ensure that NEW is NOT introduced if the code must compile against an ABAP Release prior to 7.40, 
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-740-expressions.htm
		
		setAbapReleaseOfCode("731");
		
		buildSrc("    CREATE OBJECT lx_message.");
		buildSrc("    CREATE OBJECT lo_instance TYPE cl_any_class.");
		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent         = me");
		buildSrc("        io_msg_handler    = mo_msg_handler.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testUnknownAbapRelease() {
		// ensure that NEW is NOT introduced if the code must compile against an unknown ("fallback") ABAP Release, 
		// which triggers a NumberFormatException in Rule.isCleanupAllowedFor
		 
		setAbapReleaseOfCode(ABAP.FALLBACK_RELEASE); 
		
		buildSrc("    CREATE OBJECT lx_message.");
		buildSrc("    CREATE OBJECT lo_instance TYPE cl_any_class.");
		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent         = me");
		buildSrc("        io_msg_handler    = mo_msg_handler.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReleaseRestriction() {
		// ensure that NEW is NOT introduced if the user applies an ABAP release restriction < 7.40
		// (even if the code itself is compiled against the newest release)
		
		setReleaseRestrictionFromUI(731);
		
		buildSrc("    CREATE OBJECT lx_message.");
		buildSrc("    CREATE OBJECT lo_instance TYPE cl_any_class.");
		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent         = me");
		buildSrc("        io_msg_handler    = mo_msg_handler.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
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
		rule.configProcessChains.setValue(false);
		
		buildSrc("    CREATE OBJECT:lo_any_object,");
		buildSrc("                  lo_other_object.");
   	buildSrc("    CREATE OBJECT lo_any_object");
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

	@Test
	void testCommentAfterIdentifierOrType() {
		// ensure integrity is not broken if a comment is in a weird place
		
		buildSrc("    CREATE OBJECT lo_instance \" comment");
		buildSrc("       TYPE cl_any.");
		buildSrc("    CREATE OBJECT lo_instance \" comment");
		buildSrc("       TYPE cl_any");
		buildSrc("         EXPORTING iv_any = 1.");
		buildSrc("");
		buildSrc("    CREATE OBJECT lo_instance TYPE \" comment");
		buildSrc("       cl_any.");
		buildSrc("    CREATE OBJECT lo_instance TYPE \" comment");
		buildSrc("       cl_any");
		buildSrc("         EXPORTING iv_any = 1.");

		buildExp("    lo_instance \" comment");
		buildExp("       = NEW cl_any( ).");
		buildExp("    lo_instance \" comment");
		buildExp("       = NEW cl_any(");
		buildExp("           iv_any = 1 ).");
		buildExp("");
		buildExp("    lo_instance = NEW \" comment");
		buildExp("       cl_any( ).");
		buildExp("    lo_instance = NEW \" comment");
		buildExp("       cl_any(");
		buildExp("           iv_any = 1 ).");

		testRule();
	}

	@Test
	void testCreateObjectOleUnchanged() {
		buildSrc("    DATA app TYPE ole2_object.");
		buildSrc("");
		buildSrc("    CREATE OBJECT app 'any.class' NO FLUSH QUEUE-ONLY.");
		buildSrc("    CREATE OBJECT app \" comment");
		buildSrc("      'other.class'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPragmaBeforeExporting() {
		// ensure that 'EXPORTING' is removed even if there is a pragma before it

		buildSrc("    CREATE OBJECT mo_instance TYPE cl_any_class ##ANY_PRAGMA");
		buildSrc("      EXPORTING");
		buildSrc("        io_parent = me.");

		buildExp("    mo_instance = NEW cl_any_class( ##ANY_PRAGMA");
		buildExp("        io_parent = me ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainProcessed() {
		rule.configProcessChains.setValue(true);

		buildSrc("    CREATE OBJECT: lx_message, lx_other_message.");
		buildSrc("");
		buildSrc("    CREATE OBJECT: lo_any TYPE cl_any_class,");
		buildSrc("* comment1");
		buildSrc("* comment2");
		buildSrc("                   lo_other TYPE (lv_class_name),");
		buildSrc("                   lo_third TYPE cl_othird_class");
		buildSrc("                     EXPORTING io_contract = me.");

		buildExp("    lx_message = NEW #( ).");
		buildExp("    lx_other_message = NEW #( ).");
		buildExp("");
		buildExp("    lo_any = NEW cl_any_class( ).");
		buildExp("* comment1");
		buildExp("* comment2");
		buildExp("    CREATE OBJECT lo_other TYPE (lv_class_name).");
		buildExp("    lo_third = NEW cl_othird_class(");
		buildExp("                      io_contract = me ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNoLineBreakAboveComment() {
		// ensure that the 2 line breaks above CREATE OBJECT do NOT result in two line breaks above '* comment'
		buildSrc("    any_method( ).");
		buildSrc("");
		buildSrc("    CREATE OBJECT: lo_any,");
		buildSrc("* comment");
		buildSrc("                   lo_other.");

		buildExp("    any_method( ).");
		buildExp("");
		buildExp("    lo_any = NEW #( ).");
		buildExp("* comment");
		buildExp("    lo_other = NEW #( ).");

		testRule();
	}

	@Test
	void testChainKept() {
		rule.configProcessChains.setValue(false);

		buildSrc("    CREATE OBJECT: lx_message, lx_other_message.");
		buildSrc("");
		buildSrc("    CREATE OBJECT: lo_any TYPE cl_any_class,");
		buildSrc("                   lo_other TYPE (lv_class_name),");
		buildSrc("                   lo_third TYPE cl_othird_class");
		buildSrc("                     EXPORTING io_contract = me.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
