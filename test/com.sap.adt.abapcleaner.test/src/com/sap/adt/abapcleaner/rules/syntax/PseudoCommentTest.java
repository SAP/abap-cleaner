package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class PseudoCommentTest extends RuleTestBase {
	// private PseudoCommentRule rule;
	
	PseudoCommentTest() {
		super(RuleID.PSEUDO_COMMENT);
		// rule = (PseudoCommentRule)getRule();
	}
	
	@Test
	void testSimpleCases() {
		buildSrc("    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. \"#EC NOTEXT");
		buildSrc("");
		buildSrc("    DATA: a TYPE string, \"#ec NEEDED");
		buildSrc("          b TYPE string.");
		buildSrc("    TRY.");
		buildSrc("      GET BADI lo_any_badi.");
		buildSrc("    CATCH cx_badi_not_implemented. \"#EC NO_HANDLER");
		buildSrc("    ENDTRY.");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA: a TYPE string ##NEEDED,");
		buildExp("          b TYPE string.");
		buildExp("    TRY.");
		buildExp("      GET BADI lo_any_badi.");
		buildExp("    CATCH cx_badi_not_implemented ##NO_HANDLER.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLowerOrMixeCaseEC() {
		// pseudo comments work with any (upper, lower, mixed) case of the prefix "#EC; 
		// therefore, expect all of these cases to be changed 
		buildSrc("    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. \"#ec NOTEXT");
		buildSrc("");
		buildSrc("    DATA: a TYPE string, \"#eC NEEDED");
		buildSrc("          b TYPE string. \"#EC NEEDED");

		buildExp("    CONSTANTS lc_key TYPE ty_key VALUE 'abc' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA: a TYPE string ##NEEDED,");
		buildExp("          b TYPE string ##NEEDED.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIneffectivePseudoCommentsUnchanged() {
		// expect ineffective pseudo comments to be kept; for pseudo comments to work, 
		// - there must be exactly one space after "#EC
		// - the code (e.g. NOTEXT, NEEDED) must be upper case  
		
		buildSrc("    CONSTANTS lc_key TYPE ty_key VALUE 'abc'. \"#EC notext");
		buildSrc("");
		buildSrc("    DATA: a TYPE string, \"#ec Needed");
		buildSrc("          b TYPE string. \"#EC  NEEDED");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPseudoCommentAfterColonUnchanged() {
		// expect a pseudo comments after a colon to be kept, because the corresponding pragma would have to go 
		// before the colon and would then be valid for all parts of the chain
		
		buildSrc("    DATA: \"#EC NEEDED");
		buildSrc("          a TYPE string,");
		buildSrc("          b TYPE string.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAdditionalTextComments() {
		// expect additional comments to be kept, avoiding duplicate comment signs;
		// however, if a pseudo comment is followed by another pseudo comment, the Extended Program Check will ignore the
		// second one; therefore, expect that the second pseudo comment is kept ineffective by putting it behind another 
		// comment sign:  " "#EC INEFFECTIVE
		
		buildSrc("    DATA: a TYPE string, \"#EC NEEDED  \" comment A");
		buildSrc("          b TYPE string, \"#EC NEEDED   comment B");
		buildSrc("          c TYPE string. \"#EC NEEDED   \"#EC INEFFECTIVE");

		buildExp("    DATA: a TYPE string ##NEEDED, \" comment A");
		buildExp("          b TYPE string ##NEEDED, \" comment B");
		buildExp("          c TYPE string ##NEEDED. \" \"#EC INEFFECTIVE");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCodeInspectorPseudoCommentUnchanged() {
		buildSrc("    LOOP AT lt_data ASSIGNING <ls_data> WHERE id <= iv_id. \"#EC CI_SORTSEQ");
		buildSrc("      MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest. \"#ec ENHOK");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT lt_data ASSIGNING <ls_data> WHERE id <= iv_id. \"#EC CI_SORTSEQ");
		buildExp("      MOVE-CORRESPONDING <ls_data>-source TO <ls_data>-dest ##ENH_OK.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentLineInsideCommandUnchanged() {
		// in this case, the pseudo comment is part of the CATCH command and can therefore be changed (as opposed to the previous test)
		
		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("      CATCH cx_any");
		buildSrc("        \"#EC NO_HANDLER");
		buildSrc("        .");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("      CATCH cx_any");
		buildExp("        ##NO_HANDLER");
		buildExp("        .");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyCatchBlock() {
		buildSrc("    TRY.");
		buildSrc("        any_method( ).");
		buildSrc("      CATCH cx_any.");
		buildSrc("        \"#EC NO_HANDLER");
		buildSrc("    ENDTRY.");

		buildExp("    TRY.");
		buildExp("        any_method( ).");
		buildExp("      CATCH cx_any ##NO_HANDLER.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyTryAndCatchBlocks() {
		buildSrc("    TRY.");
		buildSrc("        \"#EC NEEDED");
		buildSrc("      CATCH cx_static_check.");
		buildSrc("        \"#EC NO_HANDLER");
		buildSrc("    ENDTRY.");

		buildExp("    TRY ##NEEDED.");
		buildExp("      CATCH cx_static_check ##NO_HANDLER.");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyTryAndCatchBlocksWithMoreComments() {
		buildSrc("    TRY.");
		buildSrc("        \" comment 1");
		buildSrc("        \"#EC NEEDED");
		buildSrc("*        comment 2");
		buildSrc("      CATCH cx_static_check.");
		buildSrc("*        comment 3");
		buildSrc("        \"#EC NO_HANDLER");
		buildSrc("        \" comment 4");
		buildSrc("    ENDTRY.");

		buildExp("    TRY ##NEEDED.");
		buildExp("        \" comment 1");
		buildExp("*        comment 2");
		buildExp("      CATCH cx_static_check ##NO_HANDLER.");
		buildExp("*        comment 3");
		buildExp("        \" comment 4");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyLoops() {
		buildSrc("    WHILE a < 0.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ENDWHILE.");
		buildSrc("");
		buildSrc("    DO 5 TIMES.");
		buildSrc("      \"#EC NEEDED extra comment");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    LOOP AT lt_data ASSIGNING <ls_data>.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ENDLOOP.");

		buildExp("    WHILE a < 0 ##NEEDED.");
		buildExp("    ENDWHILE.");
		buildExp("");
		buildExp("    DO 5 TIMES ##NEEDED.");
		buildExp("      \" extra comment");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("    LOOP AT lt_data ASSIGNING <ls_data> ##NEEDED.");
		buildExp("    ENDLOOP.");

		testRule();
	}

	@Test
	void testEmptyIfBlocks() {
		// expect no change between IF ... ELSEIF as well as ELSEIF ... ELSE, because the Extended Program Check 
		// only creates a warning if the last block, just before ENDIF, is empty; the other pseudo comments have 
		// no effect

		buildSrc("    IF a < b.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ENDIF.");
		buildSrc("");
		buildSrc("    IF a < b.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ELSEIF a > b.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ELSE.");
		buildSrc("      \"#EC NEEDED");
		buildSrc("    ENDIF.");

		buildExp("    IF a < b ##NEEDED.");
		buildExp("    ENDIF.");
		buildExp("");
		buildExp("    IF a < b.");
		buildExp("      \"#EC NEEDED");
		buildExp("    ELSEIF a > b.");
		buildExp("      \"#EC NEEDED");
		buildExp("    ELSE ##NEEDED.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyMethod() {
		buildSrc("  METHOD any_method.");
		buildSrc("    \"#EC NEEDED");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method ##NEEDED.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testStandAlonePseudoCommentUnchanged() {
		// expect no change if it is not clear which Command the pseudo comment belongs to
		buildSrc("    DATA a TYPE i.");
		buildSrc("    \"#EC NEEDED");
		buildSrc("    DATA b TYPE string.");
		buildSrc("    \"#EC BOOL_OK");
		buildSrc("    DATA c TYPE i.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTwoPseudoCommentsAfterComma() {
		buildSrc("      CONSTANTS: lc_any   TYPE syst_subrc VALUE '1', \"#EC STR_NUM");
		buildSrc("                                                   \"#EC NEEDED");
		buildSrc("                 lc_other TYPE syst_subrc VALUE '2'. \"#EC STR_NUM");

		buildExp("      CONSTANTS: lc_any   TYPE syst_subrc VALUE '1' ##STR_NUM ##NEEDED,");
		buildExp("                 lc_other TYPE syst_subrc VALUE '2' ##STR_NUM.");

		testRule();
	}
}
