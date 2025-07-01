package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignWithSecondWordTest extends RuleTestBase {
	AlignWithSecondWordTest() {
		super(RuleID.ALIGN_WITH_SECOND_WORD);
	}
	
	@Test
	void testReadTableWith() {
		buildSrc("    READ TABLE lth_any_table ASSIGNING <ls_row>");
		buildSrc("      WITH TABLE KEY item_id = <ls_item>-item_id.");

		buildExp("    READ TABLE lth_any_table ASSIGNING <ls_row>");
		buildExp("         WITH TABLE KEY item_id = <ls_item>-item_id.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLoopAtAssigning() {
		// ensure that keywords in subsequent lines are not just indented by 2 spaces, because then it is very difficult to see 
		// where a block starts and ends
		
		buildSrc("    LOOP AT mo_instance->get_all_items( )");
		buildSrc("      ASSIGNING FIELD-SYMBOL(<lo_item>).");
		buildSrc("      INSERT VALUE #( item_id       = <lo_item>->ms_data-item_id");
		buildSrc("                      item_category = if_any_interface=>cos_any_item_category-value )");
		buildSrc("        INTO TABLE lth_item.");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT mo_instance->get_all_items( )");
		buildExp("         ASSIGNING FIELD-SYMBOL(<lo_item>).");
		buildExp("      INSERT VALUE #( item_id       = <lo_item>->ms_data-item_id");
		buildExp("                      item_category = if_any_interface=>cos_any_item_category-value )");
		buildExp("             INTO TABLE lth_item.");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLoopAtWhere() {
		buildSrc("    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_row>)");
		buildSrc("    WHERE event_date = '20220330'.");
		buildSrc("      \" do something");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_row>)");
		buildExp("         WHERE event_date = '20220330'.");
		buildExp("      \" do something");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testReadTableWithInto() {
		// ensure that only keywords like 'WITH' and 'INTO' are aligned, not identifiers like 'other_component'
		
		buildSrc("    READ TABLE lts_table_name");
		buildSrc("    WITH TABLE KEY any_component   = if_any_interface=>co_any_constant");
		buildSrc("                   other_component = if_any_interface=>co_other_constant");
		buildSrc("    INTO DATA(ls_item).");

		buildExp("    READ TABLE lts_table_name");
		buildExp("         WITH TABLE KEY any_component   = if_any_interface=>co_any_constant");
		buildExp("                        other_component = if_any_interface=>co_other_constant");
		buildExp("         INTO DATA(ls_item).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDeleteWhere() {
		// ensure that the position of Boolean operators AND, OR, EQUIV does NOT change 
		
		buildSrc("    DELETE lts_any_table");
		buildSrc("    WHERE item_key < '20220030000101'");
		buildSrc("      AND item_type = 'ABCD'.");

		buildExp("    DELETE lts_any_table");
		buildExp("           WHERE item_key < '20220030000101'");
		buildExp("             AND item_type = 'ABCD'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallAndCreateUnchanged() {
		// ensure that "CALL ..." or "CREATE ..." are NOT aligned to the second word
		
		buildSrc("    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'");
		buildSrc("      EXPORTING");
		buildSrc("        i_date  = iv_date");
		buildSrc("      IMPORTING");
		buildSrc("        e_buper = ev_period.");
		buildSrc("    CREATE OBJECT lo_any_object");
		buildSrc("      EXPORTING");
		buildSrc("        i_date  = iv_date.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testSecondWordOnNextLineUnchanged() {
		// ensure that NO alignment happens if the second word is not on the same line as the first
		
		buildSrc("    IMPORT");
		buildSrc("      field TO lv_value");
		buildSrc("      FROM DATABASE dtab(al) ID ls_struc-component ACCEPTING PADDING.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallTransformation() {
		// ensure that CALL TRANSFORMATION is NOT excluded from alignment
		buildSrc("    CALL TRANSFORMATION demo_escaping_xml");
		buildSrc("     SOURCE text = text");
		buildSrc("       RESULT XML xml.");

		buildExp("    CALL TRANSFORMATION demo_escaping_xml");
		buildExp("         SOURCE text = text");
		buildExp("         RESULT XML xml.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExceptionSkipped() {
		// ensure that RAISE [RESUMABLE] EXCEPTION without MESSAGE is NOT processed
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception");
		buildSrc("      EXPORTING previous = exception.");
		buildSrc("");
		buildSrc("    RAISE RESUMABLE EXCEPTION TYPE cx_other_exception");
		buildSrc("      EXPORTING previous = exception.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testRaiseShortdumpSkipped() {
		// ensure that RAISE [RESUMABLE] EXCEPTION without MESSAGE is NOT processed
		buildSrc("    RAISE SHORTDUMP TYPE cx_any_class");
		buildSrc("      EXPORTING textid = lc_any_text_id.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testReceiveResultsSkipped() {
		// ensure that RECEIVE RESULTS FROM FUNCTION is NOT processed
		buildSrc("    RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION' KEEPING TASK");
		buildSrc("      IMPORTING  et_any                = lt_any");
		buildSrc("                 et_other              = lt_other");
		buildSrc("      EXCEPTIONS communication_failure = 1 MESSAGE lv_message");
		buildSrc("                 system_failure        = 2 MESSAGE lv_message");
		buildSrc("                 OTHERS                = 3.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSubmitWithOnSameLine() {
		// ensure that the second WITH is aligned with the first one, even if it continues after the program name
		buildSrc("  SUBMIT any_prog WITH par1 IN it_any");
		buildSrc("             WITH par2 EQ iv_other");
		buildSrc("                 AND RETURN.");

		buildExp("  SUBMIT any_prog WITH par1 IN it_any");
		buildExp("                  WITH par2 EQ iv_other");
		buildExp("         AND RETURN.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSubmitWithSelection() {
		// ensure that subsequent WITHs at line start are aligned with the first(!) WITH  
		buildSrc("SUBMIT other_prog WITH SELECTION-TABLE rspar WITH par1 IN it_any");
		buildSrc("   WITH par2 EQ iv_other");
		buildSrc("WITH selcrit2 BETWEEN 'H' AND 'K'");
		buildSrc("  WITH FREE SELECTIONS texpr");
		buildSrc("                   AND RETURN.");

		buildExp("SUBMIT other_prog WITH SELECTION-TABLE rspar WITH par1 IN it_any");
		buildExp("                  WITH par2 EQ iv_other");
		buildExp("                  WITH selcrit2 BETWEEN 'H' AND 'K'");
		buildExp("                  WITH FREE SELECTIONS texpr");
		buildExp("       AND RETURN.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSubmitWithOnNextLine() {
		// ensure that the second WITH is aligned with the first one if it starts a new line
		buildSrc("  SUBMIT any_prog");
		buildSrc("   WITH par1 IN it_any");
		buildSrc("     WITH par2 EQ iv_other");
		buildSrc("     AND RETURN.");

		buildExp("  SUBMIT any_prog");
		buildExp("         WITH par1 IN it_any");
		buildExp("         WITH par2 EQ iv_other");
		buildExp("         AND RETURN.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
