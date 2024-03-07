package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class DescribeTableTest extends RuleTestBase {
	DescribeTableTest() {
		super(RuleID.DESCRIBE_TABLE);
	}

	@Test
	void testDescribeTableLinesChanged() {
		buildSrc("    DESCRIBE TABLE lt_any_table LINES lv_lines.");
		buildSrc("    DESCRIBE TABLE lt_any_table LINES DATA(lv_lines_inline).");
		buildSrc("    DESCRIBE TABLE lt_any_table LINES FINAL(lv_lines_final).");

		buildExp("    lv_lines = lines( lt_any_table ).");
		buildExp("    DATA(lv_lines_inline) = lines( lt_any_table ).");
		buildExp("    FINAL(lv_lines_final) = lines( lt_any_table ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDescribeTableWithFieldSymbol() {
		buildSrc("    ASSIGN lt_any_table TO FIELD-SYMBOL(<lt_any_table>).");
		buildSrc("    DESCRIBE TABLE <lt_any_table> LINES lv_lines2.");
		buildSrc("    DESCRIBE TABLE <lt_any_table> LINES DATA(lv_lines_inline2).");
		buildSrc("    DESCRIBE TABLE <lt_any_table> LINES FINAL(lv_lines_final2).");

		buildExp("    ASSIGN lt_any_table TO FIELD-SYMBOL(<lt_any_table>).");
		buildExp("    lv_lines2 = lines( <lt_any_table> ).");
		buildExp("    DATA(lv_lines_inline2) = lines( <lt_any_table> ).");
		buildExp("    FINAL(lv_lines_final2) = lines( <lt_any_table> ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOtherAdditionsIgnored() {
		buildSrc("    DESCRIBE TABLE lt_any_table.");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND lv_kind.");
		buildSrc("    DESCRIBE TABLE lt_any_table OCCURS lv_occurs.");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND lv_kind LINES lv_lines.");
		buildSrc("    DESCRIBE TABLE lt_any_table LINES lv_lines OCCURS lv_occurs.");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND lv_kind LINES lv_lines OCCURS lv_occurs.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOtherAdditionsInlineIgnored() {
		buildSrc("    DESCRIBE TABLE lt_any_table.");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND DATA(lv_kind).");
		buildSrc("    DESCRIBE TABLE lt_any_table OCCURS FINAL(lv_occurs).");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND DATA(lv_kind2) LINES DATA(lv_lines).");
		buildSrc("    DESCRIBE TABLE lt_any_table LINES FINAL(lv_lines2) OCCURS FINAL(lv_occurs2).");
		buildSrc("    DESCRIBE TABLE lt_any_table KIND DATA(lv_kind3) LINES DATA(lv_lines3) OCCURS DATA(lv_occurs3).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testSyFieldInWritePositionIgnored() {
		// ensure that cases with SY- fields in the write position after LINES are never touched 
		buildSrc("    DESCRIBE TABLE lt_any_table LINES sy-tfill.");
		buildSrc("    DESCRIBE TABLE lt_other_table LINES sy-tleng.");
		buildSrc("    DESCRIBE TABLE lts_third_table LINES sy-tabix.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSyTFillAfterDescribe() {
		buildSrc("    DESCRIBE TABLE it_any_table LINES DATA(lv_lines).");
		buildSrc("    DATA(lv_line_count) = sy-tfill.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTLengAfterDescribe() {
		buildSrc("    DESCRIBE TABLE it_any_table LINES DATA(lv_lines).");
		buildSrc("    DATA(lv_line_length_in_bytes) = sy-tleng.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTFillAndTLengAfterSecondDescribe() {
		// the first line can be changed, because the SY fields are only evaluated for the second line
		buildSrc("    DESCRIBE TABLE it_any_table LINES DATA(lv_lines).");
		buildSrc("    DESCRIBE TABLE it_other_table LINES DATA(lv_lines2).");
		buildSrc("    DATA(lv_line_count) = sy-tfill.");
		buildSrc("    DATA(lv_line_length_in_bytes) = sy-tleng.");

		buildExp("    DATA(lv_lines) = lines( it_any_table ).");
		buildExp("    DESCRIBE TABLE it_other_table LINES DATA(lv_lines2).");
		buildExp("    DATA(lv_line_count) = sy-tfill.");
		buildExp("    DATA(lv_line_length_in_bytes) = sy-tleng.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSySubrcAfterDescribe() {
		// this can be changed, because DESCRIBE TABLE has nothing to do with SY-SUBRC
		buildSrc("    DESCRIBE TABLE lt_any_table LINES lv_lines.");
		buildSrc("    DATA(lv_sub_rc) = sy-subrc.");

		buildExp("    lv_lines = lines( lt_any_table ).");
		buildExp("    DATA(lv_sub_rc) = sy-subrc.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testConditionalSyTLengAfterDescribe() {
		buildSrc("    DESCRIBE TABLE it_any_table LINES lv_line_count.");
		buildSrc("    IF iv_determine_line_length = abap_true.");
		buildSrc("      RETURN sy-tleng.");
		buildSrc("    ELSE.");
		buildSrc("      RETURN 0.");
		buildSrc("    ENDIF.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSyTLengInProgramFlowAfterDescribe() {
		buildSrc("    DO 2 TIMES.");
		buildSrc("      IF sy-index = 2.");
		buildSrc("        RETURN sy-tleng.");
		buildSrc("      ENDIF.");
		buildSrc("      DESCRIBE TABLE it_fourth_table LINES lv_line_count.");
		buildSrc("    ENDDO.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDescribeTableOutsideMethodKept() {
		// simulate a small code snippet (without METHOD ... ENDMETHOD being visible), 
		// after which an evaluation of SY-TFILL or SY-TLENG may or may not follow 
		buildSrc("  DESCRIBE TABLE lt_any_table LINES lv_lines.");
		buildSrc("  DESCRIBE TABLE lt_any_table LINES DATA(lv_lines_inline).");
		buildSrc("  DESCRIBE TABLE lt_any_table LINES FINAL(lv_lines_final).");

		copyExpFromSrc();
		
		testRule();
	}
}
