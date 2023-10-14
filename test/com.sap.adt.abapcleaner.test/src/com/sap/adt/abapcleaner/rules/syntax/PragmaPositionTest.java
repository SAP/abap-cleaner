package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class PragmaPositionTest extends RuleTestBase {
	private PragmaPositionRule rule;
	
	PragmaPositionTest() {
		super(RuleID.PRAGMA_POSITION);
		rule = (PragmaPositionRule)getRule();
		rule.configMovePragmaFromLineStartToEnd.setValue(false);
	}
	
	@Test
	void testCommonCases() {
		buildSrc("    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. ##NO_TEXT");
		buildSrc("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. ##ENH_OK");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.");
		buildExp("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommonCasesWithMultiplePragmas() {
		buildSrc("    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. ##NO_TEXT ##OTHER_PRAGMA[param1][][param3]");
		buildSrc("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. ##ENH_OK ##OTHER_PRAGMA");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT ##OTHER_PRAGMA[param1][][param3].");
		buildExp("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK ##OTHER_PRAGMA.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveFromLineStart() {
		rule.configMovePragmaFromLineStartToEnd.setValue(true);

		buildSrc("    ##NO_TEXT CONSTANTS lc_key TYPE ty_key VALUE 'abc'.");
		buildSrc("");
		buildSrc("    ##ENH_OK MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest.");
		buildSrc("");
		buildSrc("    ##NO_TEXT any_method( iv_any   = 'abc'");
		buildSrc("    ##NO_TEXT             iv_other = 'def' ).");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.");
		buildExp("");
		buildExp("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK.");
		buildExp("");
		buildExp("    any_method( iv_any   = 'abc' ##NO_TEXT");
		buildExp("    iv_other = 'def' ) ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineStartUnchanged() {
		rule.configMovePragmaFromLineStartToEnd.setValue(false);

		buildSrc("    ##NO_TEXT CONSTANTS lc_key TYPE ty_key VALUE 'abc'.");
		buildSrc("");
		buildSrc("    ##ENH_OK MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveFromLineMid() {
		rule.configMovePragmaFromLineStartToEnd.setValue(true);

		buildSrc("    CONSTANTS ##NO_TEXT lc_key ##ANY_PRAGMA TYPE ty_key VALUE 'abc' ##OTHER_PRAGMA.");
		buildSrc("");
		buildSrc("    ##ENH_OK MOVE-CORRESPONDING ##ANY_PRAGMA <ls_data>-source TO <ls_data>-dest. ##OTHER_PRAGMA");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##OTHER_PRAGMA ##NO_TEXT ##ANY_PRAGMA.");
		buildExp("");
		buildExp("    MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK ##ANY_PRAGMA ##OTHER_PRAGMA.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineStartKeptLineMidMoved() {
		rule.configMovePragmaFromLineStartToEnd.setValue(false);

		buildSrc("    CONSTANTS ##NO_TEXT lc_key ##ANY_PRAGMA TYPE ty_key VALUE 'abc' ##OTHER_PRAGMA.");
		buildSrc("");
		buildSrc("    ##ENH_OK MOVE-CORRESPONDING ##ANY_PRAGMA <ls_data>-source TO <ls_data>-dest. ##OTHER_PRAGMA");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##OTHER_PRAGMA ##NO_TEXT ##ANY_PRAGMA.");
		buildExp("");
		buildExp("    ##ENH_OK MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ANY_PRAGMA ##OTHER_PRAGMA.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmasInEmptyBlocks() {
		// expect pragmas to be moved from empty blocks (= blocks without executable command), 
		// even if the blocks contain comments
		buildSrc("    DO 5 TIMES.");
		buildSrc("      ##NEEDED");
		buildSrc("      \" comment");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY.");
		buildSrc("      ##NEEDED");
		buildSrc("    CATCH cx_badi_not_implemented. \" comment");
		buildSrc("      ##NO_HANDLER");
		buildSrc("    ENDTRY.");

		buildExp("    DO 5 TIMES ##NEEDED.");
		buildExp("      \" comment");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("    TRY ##NEEDED.");
		buildExp("    CATCH cx_badi_not_implemented ##NO_HANDLER. \" comment");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmasInNonEmptyBlocks() {
		// expect ##NEEDED and ##NO_HANDLER to be moved only if the block is really empty
		
		buildSrc("    DO 5 TIMES.");
		buildSrc("      ##NEEDED");
		buildSrc("      i += 1.");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY.");
		buildSrc("      i += 1.");
		buildSrc("      ##NEEDED");
		buildSrc("    CATCH cx_badi_not_implemented. \" comment");
		buildSrc("      ##NO_HANDLER");
		buildSrc("      do_something( ).");
		buildSrc("    ENDTRY.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmasInCorrectPosUnchanged() {
		buildSrc("    DATA: a TYPE string ##NEEDED, \" comment");
		buildSrc("          b TYPE string ##NEEDED.");
		buildSrc("");
		buildSrc("    DO 5 TIMES ##NEEDED.");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY ##NEEDED.");
		buildSrc("    CATCH cx_badi_not_implemented ##NO_HANDLER.");
		buildSrc("    ENDTRY.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmasAtLineEndUnchanged() {
		// expect line-end pragmas to be unchanged, even if the line breaks at a weird position
		
		buildSrc("    DATA a TYPE ##NEEDED \" comment");
		buildSrc("         string.");
		buildSrc("");
		buildSrc("    DO ##NEEDED");
		buildSrc("      5 TIMES.");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY ##NEEDED");
		buildSrc("      .");
		buildSrc("    CATCH ##NO_HANDLER");
		buildSrc("          cx_badi_not_implemented.");
		buildSrc("    ENDTRY.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultiplePragmasInEmptyBlocks() {
		buildSrc("    DO 5 TIMES.");
		buildSrc("      ##NEEDED ##NEEDED");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY.");
		buildSrc("      ##NEEDED");
		buildSrc("    CATCH cx_badi_not_implemented. \" comment");
		buildSrc("      ##NO_HANDLER ##NO_HANDLER");
		buildSrc("    ENDTRY.");

		buildExp("    DO 5 TIMES ##NEEDED ##NEEDED.");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("    TRY ##NEEDED.");
		buildExp("    CATCH cx_badi_not_implemented ##NO_HANDLER ##NO_HANDLER. \" comment");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmaInEmptyBlocksUnchanged() {
		// expect that in empty blocks, pragmas that are not specifically identified are kept   
		// in the CATCH block, pragma ##NO_HANDLER would be moved, but not ##NEEDED
		
		buildSrc("    DO 5 TIMES.");
		buildSrc("      ##ANY_PRAGMA ##OTHER_PRAGMA");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    TRY.");
		buildSrc("      ##NEEDED ##OTHER_PRAGMA");
		buildSrc("    CATCH cx_badi_not_implemented.");
		buildSrc("      ##NEEDED");
		buildSrc("    ENDTRY.");

		buildExp("    DO 5 TIMES.");
		buildExp("      ##ANY_PRAGMA ##OTHER_PRAGMA");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("    TRY ##NEEDED.");
		buildExp("      ##OTHER_PRAGMA");
		buildExp("    CATCH cx_badi_not_implemented.");
		buildExp("      ##NEEDED");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPragmaAfterCommaAndAfterPeriod() {
		buildSrc("    DATA: a TYPE string, ##NEEDED");
		buildSrc("          b TYPE string, ##NEEDED \" comment");
		buildSrc("          c TYPE string. ##NEEDED");

		buildExp("    DATA: a TYPE string ##NEEDED,");
		buildExp("          b TYPE string ##NEEDED, \" comment");
		buildExp("          c TYPE string ##NEEDED.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaBeforeCommaAndAfterPeriod() {
		buildSrc("    DATA: a ##NEEDED TYPE string, \" comment");
		buildSrc("          b TYPE string. ##NEEDED");

		buildExp("    DATA: a TYPE string ##NEEDED, \" comment");
		buildExp("          b TYPE string ##NEEDED.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaAndCommentAfterPeriod() {
		// expect the pragma to be moved; note that the source contains two Commands, 
		// because '##NEEDED \" comment' is a Command of its own, which is then merged to a single Command
		buildSrc("    DATA a TYPE string. ##NEEDED \" comment");

		buildExp("    DATA a TYPE string ##NEEDED. \" comment");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaAndCommentAfterLineBreakAndPeriod() {
		// ensure that the rule works in the weird, but syntactically correct case that the lines starts with , or .
		buildSrc("    DATA: a TYPE string");
		buildSrc("         , ##NEEDED \" comment");
		buildSrc("         b TYPE string");
		buildSrc("         . ##NEEDED");

		buildExp("    DATA: a TYPE string");
		buildExp("         ##NEEDED, \" comment");
		buildExp("         b TYPE string ##NEEDED");
		buildExp("         .");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaBeforeColonUnchanged() {
		rule.configMovePragmaFromLineStartToEnd.setValue(true);

		// expect the pragma to NOT be moved behind the colon, as this would change its scope
		buildSrc("    ##NEEDED DATA: a TYPE string,");
		buildSrc("                   b TYPE string.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaBeforeCommaUnchanged() {
		rule.configMovePragmaFromLineStartToEnd.setValue(true);

		// expect the pragma to NOT be moved behind the comma, as this would change its scope
		buildSrc("    DATA: a ##NEEDED TYPE string, b TYPE string.");
		buildSrc("    DATA: c TYPE string ##NEEDED, d TYPE string.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testPragmaBeforeColonChanged() {
		rule.configMovePragmaFromLineStartToEnd.setValue(true);

		// expect the pragma to be moved behind the colon, 
		buildSrc("    ##NEEDED DATA: \" comment");
		buildSrc("      a TYPE string,");
		buildSrc("      b TYPE string.");

		buildExp("    DATA ##NEEDED: \" comment");
		buildExp("      a TYPE string,");
		buildExp("      b TYPE string.");
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTwoPragmasAndCommentAtLineEnd() {
		// ensure that the test terminates
		
		buildSrc("    a ##PRAGMA1 ##PRAGMA2 = \" comment");
		buildSrc("        1.");

		buildExp("    a = ##PRAGMA1 ##PRAGMA2 \" comment");
		buildExp("        1.");

		testRule();
	}

	@Test
	void testLevelOpenerAtLineEnd() {
		// ensure that the pragma is added as the (first) child of the level opening Token at line end
		buildSrc("    DATA(lv_any) ##NEEDED = get_value(");
		buildSrc("        iv_param = 1 ).");
		buildSrc("    DATA(lv_any) ##NEEDED = get_value(");
		buildSrc("        ).");
		buildSrc("    DATA(lv_any) ##NEEDED = get_value( \" comment");
		buildSrc("        iv_param = 1 ).");
		buildSrc("    DATA(lv_any) ##NEEDED = get_value( \" comment");
		buildSrc("        ).");

		buildExp("    DATA(lv_any) = get_value( ##NEEDED");
		buildExp("        iv_param = 1 ).");
		buildExp("    DATA(lv_any) = get_value( ##NEEDED");
		buildExp("        ).");
		buildExp("    DATA(lv_any) = get_value( ##NEEDED \" comment");
		buildExp("        iv_param = 1 ).");
		buildExp("    DATA(lv_any) = get_value( ##NEEDED \" comment");
		buildExp("        ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
