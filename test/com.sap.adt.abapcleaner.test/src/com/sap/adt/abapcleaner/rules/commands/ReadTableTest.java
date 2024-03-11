package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class ReadTableTest extends RuleTestBase {
	private ReadTableRule rule;
	
	ReadTableTest() {
		super(RuleID.READ_TABLE);
		rule = (ReadTableRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configReplaceWithAssign.setValue(true);
		rule.configReplaceWithLineExists.setValue(true);
		rule.configReplaceWithLineIndex.setValue(true);
		rule.configUseComponentsKeyword.setValue(false);
	}

	@Test
	void testDoNotReplaceAnything() {
		rule.configReplaceWithAssign.setValue(false);
		rule.configReplaceWithLineExists.setValue(false);
		rule.configReplaceWithLineIndex.setValue(false);

		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    CHECK sy-subrc <> 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAssignElseUnassign() {
		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    CHECK sy-subrc <> 0.");

		buildExp("    ASSIGN its_any[ comp1 = iv_value1 ] TO FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildExp("    CHECK sy-subrc <> 0.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotReplaceWithAssigning() {
		rule.configReplaceWithAssign.setValue(false);

		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    CHECK sy-subrc <> 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineExistsWithECComment() {
		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ");
		buildSrc("    IF sy-subrc EQ 0.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		buildExp("    IF line_exists( its_any[ comp2 = iv_value2 ] ). \"#EC CI_SORTSEQ");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTwoFinalCommentsKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ");
		buildSrc("    IF sy-subrc EQ 0. \" comment");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotReplaceWithLineExists() {
		rule.configReplaceWithLineExists.setValue(false);

		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc EQ 0.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotReplaceWithLineIndex() {
		rule.configReplaceWithLineIndex.setValue(false);

		buildSrc("    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS.");
		buildSrc("    DATA(lv_line_index) = sy-tabix.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testDoNotReplaceWithLineExistsOrIndex() {
		rule.configReplaceWithLineExists.setValue(false);
		rule.configReplaceWithLineIndex.setValue(false);

		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc EQ 0.");
		buildSrc("    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS.");
		buildSrc("    DATA(lv_line_index) = sy-tabix.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testLineExistsWithKeyName() {
		buildSrc("    READ TABLE its_any WITH TABLE KEY keyname");
		buildSrc("                       COMPONENTS comp1 = iv_value1");
		buildSrc("                                  comp2 = iv_value2");
		buildSrc("                       TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc <> 0.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		buildExp("    IF NOT line_exists( its_any[ KEY keyname");
		buildExp("                                 comp1 = iv_value1");
		buildExp("                                 comp2 = iv_value2 ] ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineExistsWithComponentsKeyword() {
		rule.configUseComponentsKeyword.setValue(true);

		buildSrc("    READ TABLE its_any WITH TABLE KEY keyname");
		buildSrc("                       COMPONENTS comp1 = iv_value1");
		buildSrc("                                  comp2 = iv_value2");
		buildSrc("                       TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc IS INITIAL.");
		buildSrc("      RETURN.");
		buildSrc("    ENDIF.");

		buildExp("    IF line_exists( its_any[ KEY keyname");
		buildExp("                             COMPONENTS comp1 = iv_value1");
		buildExp("                                        comp2 = iv_value2 ] ).");
		buildExp("      RETURN.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineIndexWithKeyName() {
		// also ensure that the comment lines do not prevent the replacement
		buildSrc("    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS.");
		buildSrc("    \" comment line");
		buildSrc("*    another comment line");
		buildSrc("    DATA(lv_line_index) = sy-tabix.");

		buildExp("    \" comment line");
		buildExp("*    another comment line");
		buildExp("    DATA(lv_line_index) = line_index( its_any[ KEY seckey");
		buildExp("                                               comp1 = 1");
		buildExp("                                               comp2 = abap_true ] ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineIndexWithComponentsKeyword() {
		rule.configUseComponentsKeyword.setValue(true);

		buildSrc("    READ TABLE its_any WITH TABLE KEY seckey COMPONENTS comp1 = 1 comp2 = abap_true TRANSPORTING NO FIELDS.");
		buildSrc("    DATA(lv_line_index) = sy-tabix.");

		buildExp("    DATA(lv_line_index) = line_index( its_any[ KEY seckey COMPONENTS comp1 = 1");
		buildExp("                                                                     comp2 = abap_true ] ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTabixAssignmentKept() {
		// ensure that READ TABLE is kept when subsequent SY-TABIX is in a write position
		buildSrc("    READ TABLE lt_any WITH KEY comp1 = 1 TRANSPORTING NO FIELDS.");
		buildSrc("    sy-tabix = 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBinarySearchKept() {
		buildSrc("    READ TABLE it_any WITH KEY comp1 = iv_value1 BINARY SEARCH TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFromWorkAreaKept() {
		buildSrc("    READ TABLE its_any FROM is_any TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIntoAndReferenceIntoKept() {
		// in the second example, TRANSPORTING NO FIELDS is allowed despite INTO; 
		// the third example (without COMPARING) creates a compiler warning, but no error

		buildSrc("    READ TABLE its_any WITH KEY comp1 = 'abc' INTO DATA(ls_any).");
		buildSrc("    READ TABLE its_any INDEX 1 INTO ls_any COMPARING comp1 TRANSPORTING NO FIELDS.");
		buildSrc("    READ TABLE its_any INDEX 1 TRANSPORTING NO FIELDS INTO ls_any.");
		buildSrc("    READ TABLE its_any INDEX 1 INTO ls_any COMPARING ALL FIELDS TRANSPORTING ALL FIELDS.");
		buildSrc("    READ TABLE its_any WITH KEY comp1 = 'def' REFERENCE INTO lr_any.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableFromFunctionCallKept() {
		buildSrc("    READ TABLE get_table( ) WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    lv_line_index = sy-tabix.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableFromConstructorExprKept() {
		buildSrc("    READ TABLE VALUE ty_tt_any( ( a = 1 ) ) INDEX 1 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcEquals4Kept() {
		buildSrc("    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 4.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcUsedTwiceInSameStatementKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0 OR sy-subrc EQ 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultipleSySubrcKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0.");
		buildSrc("    IF sy-subrc <> 0.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSubrcAssignedToVariableKept() {
		buildSrc("    READ TABLE lt_any_table TRANSPORTING NO FIELDS WITH KEY comp1 = iv_any_param");
		buildSrc("                                                            comp2 = iv_other_param.");
		buildSrc("    DATA(lv_subrc) = sy-subrc.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcAndSyTabixKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc = 0.");
		buildSrc("      DELETE its_any INDEX sy-tabix.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTabixUsedTwiceInSameStatementKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    DATA(lv_any) = sy-tabix + 3 * ( sy-tabix - 1 ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testInnerCommentKept() {
		buildSrc("    READ TABLE its_any \" very important comment which must never get lost");
		buildSrc("          WITH TABLE KEY comp1 = iv_value1 TRANSPORTING NO FIELDS.");
		buildSrc("    CHECK sy-subrc = 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTabixAndAssertSubrcKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ");
		buildSrc("    DATA(lv_line_index) = sy-tabix.");
		buildSrc("    cl_abap_unit_assert=>assert_subrc( ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAssertSubrcWithoutParamsKept() {
		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ");
		buildSrc("    cl_abap_unit_assert=>assert_subrc( ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAssignWithAssertSubrcWithoutParamsChanged() {
		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING <ls_any> CASTING ELSE UNASSIGN.");
		buildSrc("    cl_abap_unit_assert=>assert_subrc( ).");

		buildExp("    ASSIGN its_any[ comp1 = iv_value1 ] TO <ls_any> CASTING ELSE UNASSIGN.");
		buildExp("    cl_abap_unit_assert=>assert_subrc( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAssignWithAssertSubrcWithParamsKept() {
		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    cl_abap_unit_assert=>assert_subrc( exp = 8 ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAssigningWithFreeKeyAndKeyNameKept() {
		buildSrc("    READ TABLE its_any WITH KEY keyname COMPONENTS comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    CHECK sy-subrc <> 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTFillOrTLengKept() {
		buildSrc("    READ TABLE its_any WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>) ELSE UNASSIGN.");
		buildSrc("    DATA(lv_tfill) = sy-tfill.");
		buildSrc("");
		buildSrc("    READ TABLE its_any WITH KEY comp2 = iv_value2 TRANSPORTING NO FIELDS. \"#EC CI_SORTSEQ");
		buildSrc("    DATA(lv_tleng) = sy-tleng.");
		buildSrc("    ASSERT sy-subrc EQ 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFieldSymbolForTable() {
		buildSrc("    READ TABLE <lt_any_table> INDEX lv_any_index ASSIGNING <ls_any_struc>.");

		buildExp("    ASSIGN <lt_any_table>[ lv_any_index ] TO <ls_any_struc>.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcInEnclosingWhileKept() {
		// ensure that READ TABLE is NOT changed, although the ProgramFlowAnalyzer returns exactly one line which 
		// evaluates SY-SUBRC afterwards
		buildSrc("    sy-subrc = 0.");
		buildSrc("    WHILE sy-subrc = 0.");
		buildSrc("      lv_any_value += 1.");
		buildSrc("      READ TABLE lt_any_table TRANSPORTING NO FIELDS WITH KEY comp1 = lv_any_value.");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcInWhileAfterReadTable() {
		// ensure that READ TABLE is NOT changed, because SY-SUBRC is evaluated in a loop (and thus potentially multiple times)
		buildSrc("    READ TABLE lt_any_table WITH KEY comp1 = lv_any_value TRANSPORTING NO FIELDS.");
		buildSrc("    WHILE sy-subrc = 0.");
		buildSrc("      \" do something");
		buildSrc("    ENDWHILE.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcNotInNextCommand() {
		buildSrc("    IF iv_check_comp1 = abap_true.");
		buildSrc("      READ TABLE lt_any WITH KEY comp1 = lv_any_value TRANSPORTING NO FIELDS.");
		buildSrc("    ELSE.");
		buildSrc("      READ TABLE lt_any WITH KEY comp2 = lv_any_value TRANSPORTING NO FIELDS.");
		buildSrc("    ENDIF.");
		buildSrc("    CHECK sy-subrc <> 0.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testInsertLineIndexIntoTable() {
		buildSrc("    READ TABLE lt_any WITH KEY comp = ls_any_struc-any_comp TRANSPORTING NO FIELDS.");
		buildSrc("    INSERT sy-tabix INTO TABLE lt_index_table.");

		buildExp("    INSERT line_index( lt_any[ comp = ls_any_struc-any_comp ] ) INTO TABLE lt_index_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWithKeyTableLine() {
		buildSrc("    READ TABLE lt_any_table WITH KEY table_line = iv_any_param TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc IS NOT INITIAL.");
		buildSrc("    ENDIF.");

		buildExp("    IF NOT line_exists( lt_any_table[ table_line = iv_any_param ] ).");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testWithKeyEqualsChangedToTableLine() {
		// expect the special syntax 'WITH KEY = ...', which is allowed for tables with one anonymous column, 
		// to be transformed to 'table_line = ...'
		
		buildSrc("    READ TABLE lt_any_table WITH KEY = 'A' TRANSPORTING NO FIELDS.");
		buildSrc("    DATA(lv_any_value) = sy-tabix.");

		buildExp("    DATA(lv_any_value) = line_index( lt_any_table[ table_line = 'A' ] ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComponentsWithOffsetAndLength() {
		// also expect the empty line to be kept
		buildSrc("    \" comment and empty line");
		buildSrc("");
		buildSrc("    READ TABLE lt_any_table WITH KEY comp_a      = <ls_struc>-comp_a");
		buildSrc("                                     comp_b+0(7) = <ls_struc>-comp_b+0(7) TRANSPORTING NO FIELDS.");
		buildSrc("    IF sy-subrc = 0.");
		buildSrc("    ENDIF.");

		buildExp("    \" comment and empty line");
		buildExp("");
		buildExp("    IF line_exists( lt_any_table[ comp_a      = <ls_struc>-comp_a");
		buildExp("                                  comp_b+0(7) = <ls_struc>-comp_b+0(7) ] ).");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableWithHeaderLineBracketsRemoved() {
		// ensure that the brackets are removed from 'itab[]'
		buildSrc("    READ TABLE its_any[] WITH TABLE KEY comp1 = iv_value1 ASSIGNING FIELD-SYMBOL(<ls_any>).");

		buildExp("    ASSIGN its_any[ comp1 = iv_value1 ] TO FIELD-SYMBOL(<ls_any>).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testAssignIndexWithKeyName() {
		buildSrc("    READ TABLE lt_any_table INDEX 42 USING KEY key_name ASSIGNING <ls_any_struc>.");

		buildExp("    ASSIGN lt_any_table[ KEY key_name INDEX 42 ] TO <ls_any_struc>.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testLineExistsIndexWithKeyName() {
		buildSrc("    READ TABLE lt_any_table INDEX 42 USING KEY key_name TRANSPORTING NO FIELDS.");
		buildSrc("    ASSERT ( iv_check = abap_true ) AND ( sy-subrc <> 0 ).");

		buildExp("    ASSERT ( iv_check = abap_true ) AND ( NOT line_exists( lt_any_table[ KEY key_name INDEX 42 ] ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
