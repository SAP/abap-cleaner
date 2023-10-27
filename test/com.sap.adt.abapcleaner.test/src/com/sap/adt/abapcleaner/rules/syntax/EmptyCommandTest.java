package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EmptyCommandTest extends RuleTestBase {
	EmptyCommandTest() {
		super(RuleID.EMPTY_COMMAND);
		
		// rule statistics currently don't work for deletion of Commands
		deactivateRuleUseCheck();
	}
	
	@Test
	void testEmptyCommandsAtStartOrEnd() {
		buildSrc("    CLEAR ev_value...");
		buildSrc("    ... CLEAR ev_name.");

		buildExp("    CLEAR ev_value.");
		buildExp("    CLEAR ev_name.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testRemoveLineOfEmptyCommands() {
		buildSrc("    iv_primary = 2.");
		buildSrc("    . . .");
		buildSrc("    iv_primary = 3.");

		buildExp("    iv_primary = 2.");
		buildExp("    iv_primary = 3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepEmptyLine() {
		// ensure that the two line breaks are transferred to the second assignment command
		
		buildSrc("    iv_primary = 3.");
		buildSrc("");
		buildSrc("    . iv_primary = 5.");

		buildExp("    iv_primary = 3.");
		buildExp("");
		buildExp("    iv_primary = 5.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testSequenceOfEmptyCommands() {
		buildSrc("    ::::. iv_primary = 13. ::::.");
		buildSrc("    .:.:,.:,.iv_primary = 17.:.:,.:,.");

		buildExp("    iv_primary = 13.");
		buildExp("    iv_primary = 17.");

		putAnyMethodAroundSrcAndExp();

		deactivateSyntaxCheckAfterParse();
		testRule();
	}
	
	@Test
	void testMultipleCommandsRemainingInLine() {
		buildSrc("    ... iv_primary = 19... iv_primary = 23... iv_primary = 29...");

		buildExp("    iv_primary = 19. iv_primary = 23. iv_primary = 29.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testTransferLineEndComment() {
		buildSrc("    CLEAR ev_value. . . \" comment");

		buildExp("    CLEAR ev_value. \" comment");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepComments() {
		buildSrc("    CLEAR ev_value.");
		buildSrc("");
		buildSrc("    : \" comment 1");
		buildSrc("*   comment 2");
		buildSrc("*   comment 3");
		buildSrc("    , \" comment 4");
		buildSrc("    . \" comment 5");

		buildExp("    CLEAR ev_value.");
		buildExp("");
		buildExp("    \" comment 1");
		buildExp("*   comment 2");
		buildExp("*   comment 3");
		buildExp("    \" comment 4");
		buildExp("    \" comment 5");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepCommentsWithLineBreaks() {
		buildSrc("    CLEAR ev_value.");
		buildSrc("");
		buildSrc("    : \" comment 1");
		buildSrc("");
		buildSrc("*   comment 2");
		buildSrc("*   comment 3");
		buildSrc("");
		buildSrc("    , \" comment 4");
		buildSrc("");
		buildSrc("");
		buildSrc("    . \" comment 5");

		buildExp("    CLEAR ev_value.");
		buildExp("");
		buildExp("    \" comment 1");
		buildExp("");
		buildExp("*   comment 2");
		buildExp("*   comment 3");
		buildExp("");
		buildExp("    \" comment 4");
		buildExp("");
		buildExp("");
		buildExp("    \" comment 5");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testKeepCommentsStartingInSameLine() {
		buildSrc("    CLEAR ev_value.: \" comment 1");
		buildSrc("*   comment 2");
		buildSrc("    , \" comment 3");
		buildSrc("    . \" comment 4");

		buildExp("    CLEAR ev_value. \" comment 1");
		buildExp("*   comment 2");
		buildExp("    \" comment 3");
		buildExp("    \" comment 4");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
