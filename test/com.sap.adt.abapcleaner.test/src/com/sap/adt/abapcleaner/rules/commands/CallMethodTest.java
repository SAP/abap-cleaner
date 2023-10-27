package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class CallMethodTest extends RuleTestBase {
	CallMethodTest() {
		super(RuleID.CALL_METHOD);
	}
	
	@Test
	void testCallWithoutParentheses() {
		buildSrc("    CALL METHOD any_method.");

		buildExp("    any_method( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallWithParamsExportingOnly() {
		buildSrc("    CALL METHOD any_method");
		buildSrc("      EXPORTING");
		buildSrc("        iv_par1 = iv_par1");
		buildSrc("        iv_par2 = iv_par2.");

		buildExp("    any_method(");
		buildExp("        iv_par1 = iv_par1");
		buildExp("        iv_par2 = iv_par2 )."); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallWithMultipleKeywordsAndComment() {
		buildSrc("    CALL METHOD any_object->any_method");
		buildSrc("      EXPORTING iv_par = iv_par");
		buildSrc("      IMPORTING ev_par = ev_par");
		buildSrc("      CHANGING  cv_par = cv_par. \" comment");

		buildExp("    any_object->any_method(");
		buildExp("      EXPORTING iv_par = iv_par");
		buildExp("      IMPORTING ev_par = ev_par");
		buildExp("      CHANGING  cv_par = cv_par ). \" comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallWithParentheses() {
		buildSrc("    CALL METHOD any_object->any_method(");
		buildSrc("      EXPORTING iv_par = iv_par");
		buildSrc("      IMPORTING ev_par = ev_par");
		buildSrc("      CHANGING  cv_par = cv_par ).");

		buildExp("    any_object->any_method(");
		buildExp("      EXPORTING iv_par = iv_par");
		buildExp("      IMPORTING ev_par = ev_par");
		buildExp("      CHANGING  cv_par = cv_par ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDynamicMethodCallUnchanged() {
		// ensure that dynamic method calls are NOT replaced
		
		buildSrc("    CALL METHOD any_object->(dynamic_method_name)");
		buildSrc("      EXPORTING iv_par = iv_par.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		buildSrc("    CALL METHOD:any_method,");
		buildSrc("                other_method.");
   	buildSrc("    CALL METHOD : any_method( ),");
		buildSrc("                  other_method( EXPORTING iv_par = `text` ).");
   	buildSrc("    CALL METHOD any_method");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = : 1, 2.");

	   copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testExporting() {
		// expect that the optional EXPORTING is removed
		
		buildSrc("   CALL METHOD any_class=>any_method(");
		buildSrc("     EXPORTING");
		buildSrc("       param1 = value1");
		buildSrc("       param2 = value2 ).");

		buildExp("   any_class=>any_method(");
		buildExp("       param1 = value1");
		buildExp("       param2 = value2 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCallChainWithExporting() {
		// expect that the optional EXPORTING is removed

		buildSrc("   CALL METHOD any_class=>any_static_method( )->any_instance_method(");
		buildSrc("     EXPORTING");
		buildSrc("       param1 = value1");
		buildSrc("       param2 = value2 ).");

		buildExp("   any_class=>any_static_method( )->any_instance_method(");
		buildExp("       param1 = value1");
		buildExp("       param2 = value2 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testExportingAndReceiving() {
		// expect that the optional EXPORTING is removed and RECEIVING changed into functional style

		buildSrc("   CALL METHOD any_class=>any_method(");
		buildSrc("     EXPORTING");
		buildSrc("       param1 = value1");
		buildSrc("     RECEIVING");
		buildSrc("       result = result ).");

		buildExp("   result = any_class=>any_method(");
		buildExp("                param1 = value1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCallChainWithExportingAndReceiving() {
		// expect that the optional EXPORTING is removed and RECEIVING changed into functional style

		buildSrc("   CALL METHOD any_class=>any_static_method( )->any_instance_method(");
		buildSrc("     EXPORTING");
		buildSrc("       param1 = value1");
		buildSrc("     RECEIVING");
		buildSrc("       result = result ).");

		buildExp("   result = any_class=>any_static_method( )->any_instance_method(");
		buildExp("                param1 = value1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaAtEnd() {
		// expect the pragma to remain outside the parentheses

		buildSrc("   CALL METHOD any_class=>any_method CHANGING ct_message = lt_message ##NO_TEXT.");

		buildExp("   any_class=>any_method( CHANGING ct_message = lt_message ) ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmasAndCommentAtEnd() {
		// expect the pragmas and the comment to remain outside the parentheses

		buildSrc("   CALL METHOD any_class=>any_method CHANGING ct_message = lt_message ##NO_TEXT");
		buildSrc("     ##SECOND_PRAGMA");
		buildSrc("     \" comment");
		buildSrc("     ##THIRD_PRAGMA.");

		buildExp("   any_class=>any_method( CHANGING ct_message = lt_message ) ##NO_TEXT");
		buildExp("     ##SECOND_PRAGMA");
		buildExp("     \" comment");
		buildExp("     ##THIRD_PRAGMA.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testCallMethodOfOleUnchanged() {
		// expect the pragmas and the comment to remain outside the parentheses

		buildSrc("   CALL METHOD OF ole 'any_method' = rc");
		buildSrc("     EXPORTING #1 = 'C:\\temp'");
		buildSrc("               #2 = lv_any.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmaAfterCallMethod() {
		// ensure that the pragma between CALL METHOD and the identifier does not cause a syntax error 
		
		buildSrc("   CALL METHOD ##NO_TEXT");
		buildSrc("     any_method( ");
		buildSrc("       EXPORTING");
		buildSrc("         param1 = value1");
		buildSrc("       RECEIVING");
		buildSrc("         result = result ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

}
