package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class MoveToTest extends RuleTestBase {
	private MoveToRule rule;
	
	MoveToTest() {
		super(RuleID.MOVE_TO);
		rule = (MoveToRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configProcessChains.setValue(true);
	}
	
	@Test
	void testSourceLiteral() {
		buildSrc("    MOVE 1 TO ev_result.");
		buildSrc("    MOVE 'text' TO es_result-text.");

		buildExp("    ev_result = 1.");
		buildExp("    es_result-text = 'text'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDestWithIndexAndLength() {
		buildSrc("    MOVE '2022' TO ev_date(4).");
		buildSrc("    MOVE '12' TO ev_date+4(2).");
		buildSrc("    MOVE '31' TO ev_date+6.");

		buildExp("    ev_date(4) = '2022'.");
		buildExp("    ev_date+4(2) = '12'.");
		buildExp("    ev_date+6 = '31'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSourceFunctionalCall() {
		buildSrc("    MOVE lines( it_table ) TO ev_length.");
		buildSrc("    MOVE get_next_date( iv_year   = iv_year");
		buildSrc("                        iv_period = iv_period ) TO ev_date.");
		buildSrc("    MOVE get_next_date( EXPORTING");
		buildSrc("                          iv_year   = iv_year");
		buildSrc("                          iv_period = iv_period ) TO ev_date.");

		buildExp("    ev_length = lines( it_table ).");
		buildExp("    ev_date = get_next_date( iv_year   = iv_year");
		buildExp("                             iv_period = iv_period ).");
		buildExp("    ev_date = get_next_date( EXPORTING");
		buildExp("                               iv_year   = iv_year");
		buildExp("                               iv_period = iv_period ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDownCast() {
		buildSrc("    MOVE lo_source ?TO lo_dest.");
		buildSrc("    MOVE create_item( iv_param_a = 1");
		buildSrc("                      iv_param_b = 'A' ) ?TO lo_item.");

		buildExp("    lo_dest ?= lo_source.");
		buildExp("    lo_item ?= create_item( iv_param_a = 1");
		buildExp("                            iv_param_b = 'A' ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testExact() {
		buildSrc("    MOVE EXACT source TO dest.");

		buildExp("    dest = EXACT #( source ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSimpleChainChanged() {
		buildSrc("    MOVE:");
		buildSrc("      1 TO ev_value,");
		buildSrc("      '2023' TO ev_start(4),");
		buildSrc("");
		buildSrc("      \" comment");
		buildSrc("      EXACT iv_data TO ev_data,");
		buildSrc("      io_instance ?TO eo_instance.");

		buildExp("    ev_value = 1.");
		buildExp("    ev_start(4) = '2023'.");
		buildExp("");
		buildExp("    \" comment");
		buildExp("    ev_data = EXACT #( iv_data ).");
		buildExp("    eo_instance ?= io_instance.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexChainChanged() {
		buildSrc("    MOVE 1 TO: lv_a, lv_b, lv_c.");
		buildSrc("    MOVE 'text' TO: ev_text, ev_other_text.");
		buildSrc("    MOVE get_value( ) TO: lv_first, lv_second.");

		buildExp("    lv_a = 1.");
		buildExp("    lv_b = 1.");
		buildExp("    lv_c = 1.");
		buildExp("    ev_text = 'text'.");
		buildExp("    ev_other_text = 'text'.");
		buildExp("    lv_first = get_value( ).");
		buildExp("    lv_second = get_value( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainOfOneChanged() {
		buildSrc("    MOVE: 1 TO ev_value.");
		buildSrc("    MOVE: \" comment");
		buildSrc("      2 TO ev_value_2.");

		buildExp("    ev_value = 1.");
		buildExp("    \" comment");
		buildExp("    ev_value_2 = 2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexChainOfOneChanged() {
		buildSrc("    MOVE 1 TO : lv_a.");
		buildSrc("    MOVE get_value( ) TO : lv_first.");

		buildExp("    lv_a = 1.");
		buildExp("    lv_first = get_value( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOneLinerChainChanged() {
		buildSrc("    MOVE: 1 TO ev_value, '2023' TO ev_start(4), EXACT iv_data TO ev_data, io_instance ?TO eo_instance.");

		buildExp("    ev_value = 1.");
		buildExp("    ev_start(4) = '2023'.");
		buildExp("    ev_data = EXACT #( iv_data ).");
		buildExp("    eo_instance ?= io_instance.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSimpleChainUnchanged() {
		rule.configProcessChains.setValue(false);

		buildSrc("    MOVE:");
		buildSrc("      1 TO ev_value,");
		buildSrc("      '2023' TO ev_start(4),");
		buildSrc("");
		buildSrc("      \" comment");
		buildSrc("      EXACT iv_data TO ev_data,");
		buildSrc("      io_instance ?TO eo_instance.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainOfOneUnchanged() {
		rule.configProcessChains.setValue(false);

		buildSrc("    MOVE: 1 TO ev_value.");
		buildSrc("    MOVE: \" comment");
		buildSrc("      2 TO ev_value_2.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComments() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    MOVE \" comment");
		buildSrc("         1 TO lv_any.");
		buildSrc("    MOVE 1 \" comment");
		buildSrc("         TO lv_any.");
		buildSrc("    MOVE 1 TO \" comment");
		buildSrc("         lv_any.");
		buildSrc("    MOVE 1 TO lv_any \" comment");
		buildSrc("        .");

		buildExp("    lv_any = \" comment");
		buildExp("         1.");
		buildExp("    lv_any = 1 \" comment");
		buildExp("         .");
		buildExp("    lv_any = 1 \" comment");
		buildExp("         .");
		buildExp("    lv_any = 1 \" comment");
		buildExp("        .");

		testRule();
	}

	@Test
	void testMacroDefinition() {
		buildSrc("DEFINE any_macro.");
		buildSrc("  MOVE &1 TO &2.");
		buildSrc("  MOVE &1 ?TO &2.");
		buildSrc("  MOVE EXACT &1 TO &2.");
		buildSrc("  MOVE: &1 TO &2,");
		buildSrc("        &3 TO &4.");
		buildSrc("END-OF-DEFINITION.");

		buildExp("DEFINE any_macro.");
		buildExp("  &2 = &1.");
		buildExp("  &2 ?= &1.");
		buildExp("  &2 = EXACT #( &1 ).");
		buildExp("  &2 = &1.");
		buildExp("  &4 = &3.");
		buildExp("END-OF-DEFINITION.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
