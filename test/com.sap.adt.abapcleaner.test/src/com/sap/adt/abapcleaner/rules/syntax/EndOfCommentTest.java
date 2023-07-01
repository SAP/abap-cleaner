package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class EndOfCommentTest extends RuleTestBase {
	private EndOfCommentRule rule;
	
	EndOfCommentTest() {
		super(RuleID.END_OF_COMMENT);
		rule = (EndOfCommentRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configEndOfCommentActionInsideMethod.setEnumValue(EndOfCommentAction.REMOVE_REDUNDANT);
		rule.configEndOfCommentActionOutsideMethod.setEnumValue(EndOfCommentAction.REMOVE_REDUNDANT);
		rule.configLineLimitInsideMethod.setValue(50);
	}

	@Test
	void testRemoveRedundantInsideMethod() {
		buildSrc("    LOOP AT its_item INTO DATA(ls_item).");
		buildSrc("      AT NEW group.");
		buildSrc("      ENDAT. \" new group");
		buildSrc("");
		buildSrc("      IF ls_item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("        ENDLOOP. \" at ls_item-inner_item");
		buildSrc("      ENDIF. \" ls_item-inner_item not initial.");
		buildSrc("");
		buildSrc("      AT END OF group.");
		buildSrc("      ENDAT. \" some non-redundant comment");
		buildSrc("    ENDLOOP. \" at item");

		buildExp("    LOOP AT its_item INTO DATA(ls_item).");
		buildExp("      AT NEW group.");
		buildExp("      ENDAT.");
		buildExp("");
		buildExp("      IF ls_item-inner_item IS NOT INITIAL.");
		buildExp("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildExp("        ENDLOOP.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      AT END OF group.");
		buildExp("      ENDAT. \" some non-redundant comment");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveRedundantInsideMethodWithoutPrefixes() {
		// same as previous test, but with identifiers that have no prefixes
		// additionally, use component "group_" with trailing underscore
		
		buildSrc("    LOOP AT items INTO DATA(item).");
		buildSrc("      AT NEW group_.");
		buildSrc("        init( item-group_ ).");
		buildSrc("      ENDAT. \" new group");
		buildSrc("");
		buildSrc("      IF item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("          \" some useful comment");
		buildSrc("        ENDLOOP. \" at item-inner_item");
		buildSrc("      ENDIF. \" item-inner_item not initial.");
		buildSrc("");
		buildSrc("      AT END OF group_.");
		buildSrc("        finalize( item-group_ ).");
		buildSrc("      ENDAT. \" some non-redundant comment");
		buildSrc("    ENDLOOP. \" at items");

		buildExp("    LOOP AT items INTO DATA(item).");
		buildExp("      AT NEW group_.");
		buildExp("        init( item-group_ ).");
		buildExp("      ENDAT.");
		buildExp("");
		buildExp("      IF item-inner_item IS NOT INITIAL.");
		buildExp("        LOOP AT item-inner_item TRANSPORTING NO FIELDS.");
		buildExp("          \" some useful comment");
		buildExp("        ENDLOOP.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      AT END OF group_.");
		buildExp("        finalize( item-group_ ).");
		buildExp("      ENDAT. \" some non-redundant comment");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepAllInsideMethod() {
		rule.configEndOfCommentActionInsideMethod.setEnumValue(EndOfCommentAction.KEEP);

		buildSrc("    LOOP AT its_item INTO DATA(ls_item).");
		buildSrc("      AT NEW group.");
		buildSrc("      ENDAT. \" new group");
		buildSrc("");
		buildSrc("      IF ls_item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("        ENDLOOP. \" at ls_item-inner_item");
		buildSrc("      ENDIF. \" ls_item-inner_item not initial.");
		buildSrc("");
		buildSrc("      AT END OF group.");
		buildSrc("      ENDAT. \" some non-redundant comment");
		buildSrc("    ENDLOOP. \" at item");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveAllInsideMethod() {
		// expect all end-of comments to be removed, except the pseudo-comment
		rule.configEndOfCommentActionInsideMethod.setEnumValue(EndOfCommentAction.REMOVE_ALL);

		buildSrc("    LOOP AT its_item INTO DATA(ls_item).");
		buildSrc("      AT NEW group.");
		buildSrc("      ENDAT. \" new group");
		buildSrc("");
		buildSrc("      IF ls_item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("        ENDLOOP. \"#EC any_pseudo_comment");
		buildSrc("      ENDIF. \" ls_item-inner_item not initial.");
		buildSrc("");
		buildSrc("      AT END OF group.");
		buildSrc("      ENDAT. \" some non-redundant comment");
		buildSrc("    ENDLOOP. \" at item");

		buildExp("    LOOP AT its_item INTO DATA(ls_item).");
		buildExp("      AT NEW group.");
		buildExp("      ENDAT.");
		buildExp("");
		buildExp("      IF ls_item-inner_item IS NOT INITIAL.");
		buildExp("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildExp("        ENDLOOP. \"#EC any_pseudo_comment");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      AT END OF group.");
		buildExp("      ENDAT.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRemoveRedundantOutsideMethod() {
		buildSrc("CLASS lcl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.                    \" any class definition");
		buildSrc("");
		buildSrc("CLASS lcl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD remove_end_of_comments.");
		buildSrc("  ENDMETHOD.                    \" REMOVE_END_OF_COMMENTS");
		buildSrc("");
		buildSrc("  METHOD other_method.");
		buildSrc("  ENDMETHOD. \" remove_end_of_comments (copy error)");
		buildSrc("");
		buildSrc("  METHOD third_method.");
		buildSrc("  ENDMETHOD. \"#EC CI_NOES #EC CI_CYCLO");
		buildSrc("ENDCLASS. \" lcl_any_class IMPLEMENTATION");

		buildExp("CLASS lcl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS lcl_any_class IMPLEMENTATION.");
		buildExp("  METHOD remove_end_of_comments.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD other_method.");
		buildExp("  ENDMETHOD. \" remove_end_of_comments (copy error)");
		buildExp("");
		buildExp("  METHOD third_method.");
		buildExp("  ENDMETHOD. \"#EC CI_NOES #EC CI_CYCLO");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepAllOutsideMethod() {
		rule.configEndOfCommentActionOutsideMethod.setEnumValue(EndOfCommentAction.KEEP);

		buildSrc("CLASS lcl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.                    \" any class definition");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS lcl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD remove_end_of_comments.");
		buildSrc("  ENDMETHOD.                    \" REMOVE_END_OF_COMMENTS");
		buildSrc("");
		buildSrc("  METHOD other_method.");
		buildSrc("  ENDMETHOD. \" remove_end_of_comments (copy error)");
		buildSrc("");
		buildSrc("  METHOD third_method.");
		buildSrc("  ENDMETHOD. \"#EC CI_NOES #EC CI_CYCLO");
		buildSrc("ENDCLASS. \" lcl_any_class IMPLEMENTATION");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testRemoveAllOutsideMethod() {
		// expect all end-of comments to be removed, except the pseudo-comment
		rule.configEndOfCommentActionOutsideMethod.setEnumValue(EndOfCommentAction.REMOVE_ALL);

		buildSrc("CLASS lcl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.                    \" any class definition");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS lcl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD remove_end_of_comments.");
		buildSrc("  ENDMETHOD.                    \" REMOVE_END_OF_COMMENTS");
		buildSrc("");
		buildSrc("  METHOD other_method.");
		buildSrc("  ENDMETHOD. \" remove_end_of_comments (copy error)");
		buildSrc("");
		buildSrc("  METHOD third_method.");
		buildSrc("  ENDMETHOD. \"#EC CI_NOES #EC CI_CYCLO");
		buildSrc("ENDCLASS. \" lcl_any_class IMPLEMENTATION");

		buildExp("CLASS lcl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS lcl_any_class IMPLEMENTATION.");
		buildExp("  METHOD remove_end_of_comments.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD other_method.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD third_method.");
		buildExp("  ENDMETHOD. \"#EC CI_NOES #EC CI_CYCLO");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepIfLineLimitExceeded() {
		// expect the comment at the last ENDLOOP to be kept, because the opening command is > 10 lines away
		rule.configLineLimitInsideMethod.setValue(10);

		buildSrc("    LOOP AT its_item INTO DATA(ls_item).");
		buildSrc("      AT NEW group.");
		buildSrc("        init( its_item-group ).");
		buildSrc("      ENDAT. \" new group");
		buildSrc("");
		buildSrc("      IF ls_item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("          \" some useful comment");
		buildSrc("        ENDLOOP. \" at ls_item-inner_item");
		buildSrc("      ENDIF. \" ls_item-inner_item not initial.");
		buildSrc("");
		buildSrc("      AT END OF group.");
		buildSrc("        finalize( its_item-group ).");
		buildSrc("      ENDAT. \" some non-redundant comment");
		buildSrc("    ENDLOOP. \" at item");

		buildExp("    LOOP AT its_item INTO DATA(ls_item).");
		buildExp("      AT NEW group.");
		buildExp("        init( its_item-group ).");
		buildExp("      ENDAT.");
		buildExp("");
		buildExp("      IF ls_item-inner_item IS NOT INITIAL.");
		buildExp("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildExp("          \" some useful comment");
		buildExp("        ENDLOOP.");
		buildExp("      ENDIF.");
		buildExp("");
		buildExp("      AT END OF group.");
		buildExp("        finalize( its_item-group ).");
		buildExp("      ENDAT. \" some non-redundant comment");
		buildExp("    ENDLOOP. \" at item");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testRemoveCompleteMatch() {
		buildSrc("    LOOP AT its_item INTO DATA(ls_item).");
		buildSrc("      IF ls_item-inner_item IS NOT INITIAL.");
		buildSrc("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("        ENDLOOP. \" AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildSrc("      ENDIF. \" ls_item-inner_item IS NOT INITIAL.");
		buildSrc("    ENDLOOP. \" LOOP AT its_item INTO DATA(ls_item).");

		buildExp("    LOOP AT its_item INTO DATA(ls_item).");
		buildExp("      IF ls_item-inner_item IS NOT INITIAL.");
		buildExp("        LOOP AT ls_item-inner_item TRANSPORTING NO FIELDS.");
		buildExp("        ENDLOOP.");
		buildExp("      ENDIF.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testIfWithBrackets() {
		// expect the end-of comment to be removed even though it does not have the parentheses
		buildSrc("    IF ( a = b ) OR ( b = c ).");
		buildSrc("    ENDIF. \" a = b OR b = c");

		buildExp("    IF ( a = b ) OR ( b = c ).");
		buildExp("    ENDIF.");

		testRule();

		putAnyMethodAroundSrcAndExp();
	}

	@Test
	void testPragmaAndCommentSkipped() {
		// expect the end-of comment ot be removed even though it does not contain the pragma and comment
		buildSrc("    LOOP AT its_table INTO DATA(ls_line) ##NEEDED \" comment");
		buildSrc("         WHERE id > 10.");
		buildSrc("    ENDLOOP. \" AT its_table INTO DATA(ls_line) WHERE id > 10.");

		buildExp("    LOOP AT its_table INTO DATA(ls_line) ##NEEDED \" comment");
		buildExp("         WHERE id > 10.");
		buildExp("    ENDLOOP.");

		testRule();
	}

	@Test
	void testIncompleteIdentifierKept() {
		// expect the second end-of comment to be kept, because it does not contain the complete identifier 
		buildSrc("    IF lv_very_long_variable_name = 1.");
		buildSrc("    ENDIF. \" very long variable name = 1");
		buildSrc("    IF lv_very_long_variable_name = 1.");
		buildSrc("    ENDIF. \" very long variable = 1");

		buildExp("    IF lv_very_long_variable_name = 1.");
		buildExp("    ENDIF.");
		buildExp("    IF lv_very_long_variable_name = 1.");
		buildExp("    ENDIF. \" very long variable = 1");

		testRule();
	}
}
