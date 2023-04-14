package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class SpaceAroundCommentSignTest extends RuleTestBase {
	private SpaceAroundCommentSignRule rule;
	
	SpaceAroundCommentSignTest() {
		super(RuleID.SPACE_AROUND_COMMENT_SIGN);
		rule = (SpaceAroundCommentSignRule)getRule();
		
		rule.configSpaceBeforeCommentSign.setValue(true);
		rule.configSpaceAfterCommentSign.setValue(true);
	}
	
	@Test
	void testSpaceAfterLineEndComments() {
		buildSrc("    CLEAR ev_result.  \"comment at line end");

		buildExp("    CLEAR ev_result.  \" comment at line end");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpaceAfterFullLineComments() {
		buildSrc("    \"Comment signs");
		buildSrc("    \"are NOT the same as \"quotation marks\",");
		buildSrc("    \"so it looks much better");
		buildSrc("    \"to put a space between the \" and the text.");

		buildExp("    \" Comment signs");
		buildExp("    \" are NOT the same as \"quotation marks\",");
		buildExp("    \" so it looks much better");
		buildExp("    \" to put a space between the \" and the text.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpaceBeforeLineEndComments() {
		buildSrc("    lv_value = 0.\" comment");
		buildSrc("");
		buildSrc("    ls_pair = VALUE #(\" initial comment");
		buildSrc("                       a = '3.1415'\" pi");
		buildSrc("                       b = '1.4142'\" sqrt(2)");
		buildSrc("                      ).\" final comment");

		buildExp("    lv_value = 0. \" comment");
		buildExp("");
		buildExp("    ls_pair = VALUE #( \" initial comment");
		buildExp("                       a = '3.1415' \" pi");
		buildExp("                       b = '1.4142' \" sqrt(2)");
		buildExp("                      ). \" final comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSpaceBeforeAndAfterLineEndComments() {
		buildSrc("    lv_value = 0.\"comment");
		buildSrc("");
		buildSrc("    ls_pair = VALUE #(\"initial comment");
		buildSrc("                       a = '3.1415'\" pi");
		buildSrc("                       b = '1.4142'\" sqrt(2)");
		buildSrc("                      ).\"final comment");

		buildExp("    lv_value = 0. \" comment");
		buildExp("");
		buildExp("    ls_pair = VALUE #( \" initial comment");
		buildExp("                       a = '3.1415' \" pi");
		buildExp("                       b = '1.4142' \" sqrt(2)");
		buildExp("                      ). \" final comment");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSpaceBeforeFullLineComments() {
		buildSrc("    \" comment lines are not changed by this(!) rule,");
		buildSrc("\" even if they are not indented at all");

		copyExpFromSrc(); // no change expected
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
}
