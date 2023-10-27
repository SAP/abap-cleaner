package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ClosingBracketsPositionTest extends RuleTestBase {
	ClosingBracketsPositionTest() {
		super(RuleID.CLOSING_BRACKETS_POSITION);
	}
	
	@Test
	void testValueStatement() {
		buildSrc("    ev_result = VALUE #( ( a = 2");
		buildSrc("                           b = 4");
		buildSrc("                         )");
		buildSrc("                       ).");

		buildExp("    ev_result = VALUE #( ( a = 2");
		buildExp("                           b = 4 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueStatementWithComment() {
		buildSrc("    ev_result = VALUE #( ( a = 1");
		buildSrc("                           b = 2");
		buildSrc("                         )");
		buildSrc("                         ( a = 2");
		buildSrc("                           b = 4 \" comment");
		buildSrc("                         )");
		buildSrc("                       ).");

		buildExp("    ev_result = VALUE #( ( a = 1");
		buildExp("                           b = 2 )");
		buildExp("                         ( a = 2");
		buildExp("                           b = 4 ) ). \" comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testParameterList() {
		buildSrc("    any_operation( iv_param_a = 1");
		buildSrc("                   iv_param_b = 2");
		buildSrc("                 ).");

		buildExp("    any_operation( iv_param_a = 1");
		buildExp("                   iv_param_b = 2 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testParameterListWithComment() {
		buildSrc("    any_operation( iv_param = 1 \" comment");
		buildSrc("                 ).");

		buildExp("    any_operation( iv_param = 1 ). \" comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testParameterListWithConflictingComments() {
		buildSrc("    any_operation( iv_param = 1 \" comment");
		buildSrc("                 ). \" conflicting comment");

		buildExp("    any_operation( iv_param = 1 ). \" comment; conflicting comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testParameterListWithCommentAndPseudoCommentUnchanged() {
		// ensure that the comments are NOT merged if the second one is a pseudo-comment
		buildSrc("    any_operation( iv_param = 1 \" comment");
		buildSrc("                 ). \"#EC NEEDED");

		buildExp("    any_operation( iv_param = 1 ) \" comment");
		buildExp("                 . \"#EC NEEDED");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testParameterListWithPseudoCommentAndCommentUnchanged() {
		// ensure that currently, the comments are NOT merged if the first one is a pseudo-comment 
		// (although this would be possible if the separator starts with a space: "#EC NEEDED comment
		buildSrc("    any_operation( iv_param = 1 \"#EC NEEDED");
		buildSrc("                 ). \" comment");

		buildExp("    any_operation( iv_param = 1 ) \"#EC NEEDED");
		buildExp("                 . \" comment");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testPragma() {
	   // expect "... ) ##NEEDED.", which is the only(!) correct result if the closing parenthesis is moved up:
		// both "... ). ##NEEDED" and "... ##NEEDED )." would be wrong and get a warning in ADT!

		buildSrc("    any_method( EXPORTING iv_param1 = lv_value1");
		buildSrc("                IMPORTING ev_param2 = lv_param2 ##NEEDED");
		buildSrc("               ).");

		buildExp("    any_method( EXPORTING iv_param1 = lv_value1");
		buildExp("                IMPORTING ev_param2 = lv_param2 ) ##NEEDED.");

		testRule();
	}

	@Test
	void testPragmas() {
		buildSrc("    any_method( EXPORTING iv_param1 = lv_value1");
		buildSrc("                IMPORTING ev_param2 = lv_param2 ##NEEDED ##OTHER_PRAGMA");
		buildSrc("               ).");

		buildExp("    any_method( EXPORTING iv_param1 = lv_value1");
		buildExp("                IMPORTING ev_param2 = lv_param2 ) ##NEEDED ##OTHER_PRAGMA.");

		testRule();
	}

	@Test
	void testPragmaAndComment() {
		buildSrc("    any_method( EXPORTING iv_param1 = lv_value1");
		buildSrc("                IMPORTING ev_param2 = lv_param2 ##NEEDED \" comment");
		buildSrc("               ).");

		buildExp("    any_method( EXPORTING iv_param1 = lv_value1");
		buildExp("                IMPORTING ev_param2 = lv_param2 ) ##NEEDED. \" comment");

		testRule();
	}

	@Test
	void testPragmaAndConflictingComments() {
		buildSrc("    any_method( EXPORTING iv_param1 = lv_value1");
		buildSrc("                IMPORTING ev_param2 = lv_param2 ##NEEDED \" comment");
		buildSrc("               ). \" conflicting comment");

		buildExp("    any_method( EXPORTING iv_param1 = lv_value1");
		buildExp("                IMPORTING ev_param2 = lv_param2 ) ##NEEDED. \" comment; conflicting comment");

		testRule();
	}

	@Test
	void testArithmeticExpr() {
		buildSrc("    a = ( 1 + 2 \" comment");
		buildSrc("        ) * (");
		buildSrc("        3 + 4 ).");

		buildExp("    a = ( 1 + 2 ) \" comment");
		buildExp("        * (");
		buildExp("        3 + 4 ).");

		testRule();
	}
}
