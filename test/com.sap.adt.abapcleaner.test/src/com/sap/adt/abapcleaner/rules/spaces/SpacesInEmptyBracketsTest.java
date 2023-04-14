package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class SpacesInEmptyBracketsTest extends RuleTestBase {
	SpacesInEmptyBracketsTest() {
		super(RuleID.SPACES_IN_EMPTY_BRACKETS);
	}
	
	@Test
	void testMethodChain() {
		buildSrc("    ev_result = class_name(  )=>get_tool(  )->get_value(    ).");

		buildExp("    ev_result = class_name( )=>get_tool( )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentLineInsideCommand() {
		buildSrc("    class_name(  )=>get_tool(");
		buildSrc("        \" comment");
		buildSrc("        )->get_value(    ).");

		buildExp("    class_name( )=>get_tool(");
		buildExp("        \" comment");
		buildExp("        )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLineBreaksNotRemoved() {
		// expect no change in case of line breaks within the brackets
		
		buildSrc("    ev_result = get_value(");
		buildSrc("                ).");

		copyExpFromSrc(); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
