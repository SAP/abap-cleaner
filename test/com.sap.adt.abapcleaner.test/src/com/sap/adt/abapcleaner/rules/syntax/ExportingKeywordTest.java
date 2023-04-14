package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ExportingKeywordTest extends RuleTestBase {
	ExportingKeywordTest() {
		super(RuleID.EXPORTING_KEYWORD);
	}
	
	@Test
	void testMethodCall() {
		buildSrc("    lo_any_object->any_method(");
		buildSrc("      EXPORTING iv_par1 = iv_par1");
		buildSrc("                iv_par2 = iv_par2 ).");

		buildExp("    lo_any_object->any_method(");
		buildExp("        iv_par1 = iv_par1");
		buildExp("        iv_par2 = iv_par2 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNestedCalls() {
		buildSrc("    lv_value = get_value( EXPORTING iv_value1 = lv_value");
		buildSrc("                                    iv_value2 = 'abc'");
		buildSrc("                                    iv_value3 = get_inner_value( EXPORTING a = 5");
		buildSrc("                                                                           b = 7 ) ).");

		buildExp("    lv_value = get_value( iv_value1 = lv_value");
		buildExp("                          iv_value2 = 'abc'");
		buildExp("                          iv_value3 = get_inner_value( a = 5");
		buildExp("                                                       b = 7 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMultiKeywordCallUnchanged() {
		buildSrc("    \" calls with other keywords such as IMPORTING and RECEIVING must NOT be changed:");
		buildSrc("    calculate( EXPORTING iv_value  = lv_value");
		buildSrc("               IMPORTING ev_result = lv_result ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChain() {
		buildSrc("    lv_sum += : get_square( EXPORTING iv_num = 5 ),");
		buildSrc("                get_square( EXPORTING iv_num = 6 ),");
		buildSrc("                get_square( EXPORTING iv_num = 7 ).");

		buildExp("    lv_sum += : get_square( iv_num = 5 ),");
		buildExp("                get_square( iv_num = 6 ),");
		buildExp("                get_square( iv_num = 7 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testExportingIdentifierNotChanged() {
		// ensure that the (theoretical) case of an identifier named "exporting" is unchanged
		
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING VALUE(exporting) TYPE string.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}
