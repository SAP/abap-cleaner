package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ReceivingKeywordTest extends RuleTestBase {
	ReceivingKeywordTest() {
		super(RuleID.RECEIVING_KEYWORD);
		
	}
	
	@Test
	void testSimpleMethodCall() {
		buildSrc("    any_method(");
		buildSrc("      EXPORTING iv_param  = lv_param");
		buildSrc("      IMPORTING ev_param  = ev_param");
		buildSrc("      CHANGING  cv_param  = cv_param");
		buildSrc("      RECEIVING rv_result = lv_result ).");

		buildExp("    lv_result = any_method(");
		buildExp("                  EXPORTING iv_param  = lv_param");
		buildExp("                  IMPORTING ev_param  = ev_param");
		buildExp("                  CHANGING  cv_param  = cv_param ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReceivingInlineDecl() {
		buildSrc("    cl_any_class=>get( )->any_method(");
		buildSrc("      IMPORTING");
		buildSrc("        ev_param  = ev_param");
		buildSrc("      CHANGING");
		buildSrc("        cv_param  = cv_param");
		buildSrc("      RECEIVING");
		buildSrc("        rv_result = DATA(lv_result2) ).");

		buildExp("    DATA(lv_result2) = cl_any_class=>get( )->any_method(");
		buildExp("                         IMPORTING");
		buildExp("                           ev_param  = ev_param");
		buildExp("                         CHANGING");
		buildExp("                           cv_param  = cv_param ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReceivingWithOffsetAndLength() {
		buildSrc("    cl_any_class=>get( )->calculate_month(");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = lv_param");
		buildSrc("      RECEIVING");
		buildSrc("        rv_month = lv_date+4(2) ).");

		buildExp("    lv_date+4(2) = cl_any_class=>get( )->calculate_month(");
		buildExp("                     EXPORTING");
		buildExp("                       iv_param = lv_param ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCallChain() {
		buildSrc("    cl_any_class=>get( )->any_method( IMPORTING ev_param  = ev_param");
		buildSrc("                                      CHANGING cv_param   = cv_param  \" comment on changing");
		buildSrc("                                      RECEIVING rv_result = DATA(lv_result3) \" comment on receiving");
		buildSrc("                                    ).");

		buildExp("    DATA(lv_result3) = cl_any_class=>get( )->any_method( IMPORTING ev_param  = ev_param");
		buildExp("                                                         CHANGING cv_param   = cv_param  \" comment on changing");
		buildExp("                                                         \" comment on receiving");
		buildExp("                                                       ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallWithExceptionsUnchanged() {
		// expect the following statement to NOT be converted, as functional method calls must not contain EXCEPTIONS 
		
		buildSrc("    cl_any_class=>any_static_method(");
		buildSrc("      exporting");
		buildSrc("        iv_value        = conv #( lv_value )");
		buildSrc("      receiving");
		buildSrc("        rv_value        = rs_struc-component");
		buildSrc("      exceptions");
		buildSrc("        any_exception   = 1");
		buildSrc("        other_exception = 2");
		buildSrc("        others          = 3 ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testInlineDeclarationInImporting() {
		// if an inline declaration is found after IMPORTING, expect the command to be unchanged,
		// because replacing the RECEIVING would be a syntax error
		
		buildSrc("    complex_method(");
		buildSrc("      EXPORTING iv_int = 1");
		buildSrc("      IMPORTING ev_int = DATA(lv_imported_int)");
		buildSrc("      CHANGING  cv_int = lv_changed_int");
		buildSrc("      RECEIVING rv_int = DATA(lv_returned_int) ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCallMethod() {
		// expect CALL METHOD to be unchanged - such cases require handling by the CallMethodRule first
		
		buildSrc("   CALL METHOD any_class=>any_method(");
		buildSrc("     EXPORTING");
		buildSrc("       param1 = value1");
		buildSrc("     RECEIVING");
		buildSrc("       result = result ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
