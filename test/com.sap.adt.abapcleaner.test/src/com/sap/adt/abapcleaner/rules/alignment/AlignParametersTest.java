package com.sap.adt.abapcleaner.rules.alignment;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignParametersTest extends RuleTestBase {
	private AlignParametersRule rule;
	
	AlignParametersTest() {
		super(RuleID.ALIGN_PARAMETERS);
		rule = (AlignParametersRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);
		rule.configMaxLineLengthForSingleLine.setValue(160);
		rule.configMaxParamCountBehindProceduralCall.setValue(0);
		rule.configMaxParamCountBehindFunctionalCall.setValue(100);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(false);
		rule.configPutFunctionalCallKeywordsOnOwnLine.setValue(false);
		rule.configAlignAssignments.setValue(true);
		rule.configAlignAcrossTableRows.setValue(true);
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
		rule.configKeepOtherOneLiners.setEnumValue(ComponentsOnSingleLine.NEVER);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);
	}

	@Test
	void testDependsOnExternalFiles() {
		assertFalse(rule.dependsOnExternalFiles());
	}
	
	@Test
	void testComplexParametersWithExporting() {
		buildSrc("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item->ms_data-item_id");
		buildSrc("        iv_text = 'abcde'");
		buildSrc("        iv_condition_type = if_any_interface=>co_condition_type");
		buildSrc("        iv_item_type = lo_item->get_item_data( )-item_type");
		buildSrc("        iv_amount = 8 ).");

		buildExp("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildExp("                      EXPORTING iv_item_id        = lo_item->ms_data-item_id");
		buildExp("                                iv_text           = 'abcde'");
		buildExp("                                iv_condition_type = if_any_interface=>co_condition_type");
		buildExp("                                iv_item_type      = lo_item->get_item_data( )-item_type");
		buildExp("                                iv_amount         = 8 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testParametersWithAsteriskComments() {
		buildSrc("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item->ms_data-item_id");
		buildSrc("*        iv_text_a = 'abcde'");
		buildSrc("*        iv_text_b = 'xyz'");
		buildSrc("        iv_condition_type = if_any_interface=>co_condition_type");
		buildSrc("        iv_item_type = lo_item->get_item_data( )-item_type");
		buildSrc("        iv_amount = 8 ).");

		buildExp("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildExp("                      EXPORTING iv_item_id        = lo_item->ms_data-item_id");
		buildExp("*                                iv_text_a         = 'abcde'");
		buildExp("*                                iv_text_b         = 'xyz'");
		buildExp("                                iv_condition_type = if_any_interface=>co_condition_type");
		buildExp("                                iv_item_type      = lo_item->get_item_data( )-item_type");
		buildExp("                                iv_amount         = 8 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComplexParametersWithComment() {
		buildSrc("    lts_any_table = cl_any_class_with_long_name=>create_table( EXPORTING iv_item_id = lo_item->ms_data-item_id  \" long comment challenging line length");
		buildSrc("                                                                 iv_text = 'abcde'");
		buildSrc("                                                                 iv_condition_type = if_any_interface=>co_condition_type");
		buildSrc("                                                                 iv_item_type = lo_item->get_item_data( )-item_type");
		buildSrc("                                                                 iv_amount = 8 ).");

		buildExp("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildExp("                      EXPORTING iv_item_id        = lo_item->ms_data-item_id  \" long comment challenging line length");
		buildExp("                                iv_text           = 'abcde'");
		buildExp("                                iv_condition_type = if_any_interface=>co_condition_type");
		buildExp("                                iv_item_type      = lo_item->get_item_data( )-item_type");
		buildExp("                                iv_amount         = 8 ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueConstructorInsideParamList() {
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.NEVER);
		rule.configMaxLineLength.setValue(90);

		buildSrc("    DATA(lo_instance) = ltd_any_test_double=>create_double( is_param_with_long_name = VALUE #( price = '1320.00' \" very long comment that is irrelevant for max line length");
		buildSrc("                                                                                      amount = '1200.00' \" more comment");
		buildSrc("                                                                                      range_amount = '360.00'  \" third comment");
		buildSrc("                                                                                      quantity = 100 ) ).");

		buildExp("    DATA(lo_instance) = ltd_any_test_double=>create_double(");
		buildExp("                            is_param_with_long_name = VALUE #( price        = '1320.00' \" very long comment that is irrelevant for max line length");
		buildExp("                                                               amount       = '1200.00' \" more comment");
		buildExp("                                                               range_amount = '360.00'  \" third comment");
		buildExp("                                                               quantity     = 100 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMoveParamLeftOfAssignOp() {
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);
		rule.configMaxLineLength.setValue(90);

		buildSrc("    DATA(lo_instance_with_long_name) = ltd_any_test_double=>create_method_with_long_name( is_param_with_long_name = VALUE #( price = '1320.00' \" very long comment that is irrelevant for max line length");
		buildSrc("                                                                                                                     amount = '1200.00' \" more comment");
		buildSrc("                                                                                                                     range_amount = '360.00'  \" third comment");
		buildSrc("                                                                                                                     quantity = 100 ) ).");

		buildExp("    DATA(lo_instance_with_long_name) = ltd_any_test_double=>create_method_with_long_name(");
		buildExp("        is_param_with_long_name = VALUE #( price        = '1320.00' \" very long comment that is irrelevant for max line length");
		buildExp("                                           amount       = '1200.00' \" more comment");
		buildExp("                                           range_amount = '360.00'  \" third comment");
		buildExp("                                           quantity     = 100 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentWithCast() {
		rule.configMaxLineLength.setValue(80);
	   rule.configMaxLineLengthForSingleLine.setValue(80);

		buildSrc("    lo_instance ?= cl_th_any_test_helper=>create_helper( is_param_with_long_name = is_param ).");
		
		buildExp("    lo_instance ?= cl_th_any_test_helper=>create_helper(");
		buildExp("                       is_param_with_long_name = is_param ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAssignmentWithCastMoveLeftOfAssignOp() {
		rule.configMaxLineLength.setValue(80);
	   rule.configMaxLineLengthForSingleLine.setValue(80);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.ALWAYS);

		buildSrc("    lo_instance ?= cl_th_any_test_helper=>create_helper( is_param_with_long_name = is_param ).");
		
		buildExp("    lo_instance ?= cl_th_any_test_helper=>create_helper(");
		buildExp("        is_param_with_long_name = is_param ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testForceKeywordOnOwnLine() {
		// ensure that "keywords on same line" configuration is overridden if line length would be exceeded 

		rule.configPutFunctionalCallKeywordsOnOwnLine.setValue(false); 
		rule.configMaxLineLength.setValue(80); // too little to continue after "EXPORTING"
		rule.configMaxLineLengthForSingleLine.setValue(80);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.NEVER);

		buildSrc("    lo_instance_with_long_name ?= lo_factory=>get( )->create_any_instance( EXPORTING is_default_type_long_name = is_default_type_long_name ).");
		
		buildExp("    lo_instance_with_long_name ?= lo_factory=>get( )->create_any_instance(");
		buildExp("                                    EXPORTING");
		buildExp("                                      is_default_type_long_name = is_default_type_long_name ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueSimple() {
		buildSrc("    INSERT VALUE #( comp = ls_data-comp");
		buildSrc("                    long_component_name = lv_any_value )");
		buildSrc("           INTO TABLE lth_table.");

		buildExp("    INSERT VALUE #( comp                = ls_data-comp");
		buildExp("                    long_component_name = lv_any_value )");
		buildExp("           INTO TABLE lth_table.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithDefaults() {
		buildSrc("    ets_fulfillment = VALUE #( event_date = gc_any_date");
		buildSrc("                                amount = gc_any_price");
		buildSrc("                               ( fulfillment_number = lc_fulfill_num_1");
		buildSrc("                                  qty = lc_fulfill_qty_1 )");
		buildSrc("                                 ( fulfillment_number = lc_fulfill_num_2");
		buildSrc("                                    qty = lc_fulfill_qty_2 )");
		buildSrc("                                   ( fulfillment_number = lc_fulfill_num_3");
		buildSrc("                                      qty = lc_fulfill_qty_3 ) ).");

		buildExp("    ets_fulfillment = VALUE #( event_date = gc_any_date");
		buildExp("                               amount     = gc_any_price");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_1");
		buildExp("                                 qty                = lc_fulfill_qty_1 )");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_2");
		buildExp("                                 qty                = lc_fulfill_qty_2 )");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_3");
		buildExp("                                 qty                = lc_fulfill_qty_3 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithDefaultsAndOneLinePerRow() {
		buildSrc("    ets_fulfillment = VALUE #( event_date = gc_any_date");
		buildSrc("                                amount = gc_any_price");
		buildSrc("                               ( fulfillment_number = lc_fulfill_num_1  qty = lc_fulfill_qty_1 )");
		buildSrc("                                 ( fulfillment_number = lc_fulfill_num_2  qty = lc_fulfill_qty_2 )");
		buildSrc("                                   ( fulfillment_number = lc_fulfill_num_3  qty = lc_fulfill_qty_3 ) ).");

		buildExp("    ets_fulfillment = VALUE #( event_date = gc_any_date");
		buildExp("                               amount     = gc_any_price");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_1  qty = lc_fulfill_qty_1 )");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_2  qty = lc_fulfill_qty_2 )");
		buildExp("                               ( fulfillment_number = lc_fulfill_num_3  qty = lc_fulfill_qty_3 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithOneLinePerRow() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.NEVER);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190040000101'  event_date = '20190401'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");

		buildExp("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("                                        ( item_key = '202190040000101'  event_date = '20190401'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMoveValueRowsLeftOfAssignOp() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190040000101'  event_date = '20190401'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");

		buildExp("    mts_table_with_long_name = VALUE #(");
		buildExp("        ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("        ( item_key = '202190040000101'  event_date = '20190401'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComponentsOnSingleLineTooLong() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
		rule.configMaxLineLengthForSingleLine.setValue(120);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.NEVER);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ).");

		buildExp("    mts_table_with_long_name = VALUE #( ( item_key           = '202190030000101'");
		buildExp("                                          event_date         = '20190301'");
		buildExp("                                          total_reported_qty = '30'");
		buildExp("                                          quantity_unit      = 'TAG' )");
		buildExp("                                        ( item_key           = '202190050000101'");
		buildExp("                                          event_date         = '20190501'");
		buildExp("                                          total_reported_qty = '30'");
		buildExp("                                          quantity_unit      = 'TAG' ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testComponentsOnSingleLineMoveLeft() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);
		rule.configMaxLineLengthForSingleLine.setValue(120);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ).");

		buildExp("    mts_table_with_long_name = VALUE #(");
		buildExp("        ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testAlwaysKeepComponentsOnSingleLine() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.ALWAYS);
		rule.configMaxLineLengthForSingleLine.setValue(80);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");

		buildExp("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildExp("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ) .");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testNeverKeepComponentsOnSingleLine() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.NEVER);

		buildSrc("    mts_table_with_long_name = VALUE #( ( item_key = '202190030000101'  event_date = '20190301'  total_reported_qty = '30'  quantity_unit = 'TAG' )");
		buildSrc("                                        ( item_key = '202190050000101'  event_date = '20190501'  total_reported_qty = '30'  quantity_unit = 'TAG' ) ).");

		buildExp("    mts_table_with_long_name = VALUE #( ( item_key           = '202190030000101'");
		buildExp("                                          event_date         = '20190301'");
		buildExp("                                          total_reported_qty = '30'");
		buildExp("                                          quantity_unit      = 'TAG' )");
		buildExp("                                        ( item_key           = '202190050000101'");
		buildExp("                                          event_date         = '20190501'");
		buildExp("                                          total_reported_qty = '30'");
		buildExp("                                          quantity_unit      = 'TAG' ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithBase() {
		buildSrc("    <ls_new_item>-component-any_subcomponent = VALUE #( BASE <ls_new_item>-component-any_subcomponent ( fulfillment_number = lc_fulfill_number");
		buildSrc("                                                                                                        event_date         = lc_fulfill_date");
		buildSrc("                                                                                                        qty                = <ls_new_item>-component-qty");
		buildSrc("                                                                                                        amount             = 0 ) ).");

		buildExp("    <ls_new_item>-component-any_subcomponent = VALUE #( BASE <ls_new_item>-component-any_subcomponent");
		buildExp("                                                        ( fulfillment_number = lc_fulfill_number");
		buildExp("                                                          event_date         = lc_fulfill_date");
		buildExp("                                                          qty                = <ls_new_item>-component-qty");
		buildExp("                                                          amount             = 0 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithBaseAndComment() {
		buildSrc("    <ls_new_item>-component-any_subcomponent = VALUE #( BASE <ls_new_item>-component-any_subcomponent ( \" comment");
		buildSrc("                                                                                                        fulfillment_number = lc_fulfill_number");
		buildSrc("                                                                                                        event_date         = lc_fulfill_date");
		buildSrc("                                                                                                        qty                = <ls_new_item>-component-qty");
		buildSrc("                                                                                                        amount             = 0 ) ).");

		buildExp("    <ls_new_item>-component-any_subcomponent = VALUE #( BASE <ls_new_item>-component-any_subcomponent");
		buildExp("                                                        ( \" comment");
		buildExp("                                                          fulfillment_number = lc_fulfill_number");
		buildExp("                                                          event_date         = lc_fulfill_date");
		buildExp("                                                          qty                = <ls_new_item>-component-qty");
		buildExp("                                                          amount             = 0 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithBaseAndArithmeticExprs() {
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.NEVER);

		buildSrc("    <ls_item>-component_table = VALUE #( BASE <ls_item>-component_table ( invoice_number   = lc_invoice_number");
		buildSrc("                                                                          invoice_date     = lc_invoice_date");
		buildSrc("                                                                          qty              = <ls_struc>-component-qty");
		buildSrc("                                                                          amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty");
		buildSrc("                                                                          is_final_invoice = abap_true ) ).");
		buildSrc("");
		buildSrc("    <ls_item>-component_table = VALUE #( BASE <ls_item>-component_table ( invoice_number   = lc_invoice_number_2");
		buildSrc("                                                                          posting_date     = lc_invoice_date_2");
		buildSrc("                                                                          qty              = <ls_struc>-component-qty / 2");
		buildSrc("                                                                          amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty / 2");
		buildSrc("                                                                          is_final_invoice = abap_false )");
		buildSrc("");
		buildSrc("                                                                        ( invoice_number   = lc_invoice_number_3");
		buildSrc("                                                                          posting_date     = lc_invoice_date_3");
		buildSrc("                                                                          qty              = <ls_struc>-component-qty / 2");
		buildSrc("                                                                          amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty / 2");
		buildSrc("                                                                          is_final_invoice = abap_true ) ).");

		buildExp("    <ls_item>-component_table = VALUE #( BASE <ls_item>-component_table");
		buildExp("                                         ( invoice_number   = lc_invoice_number");
		buildExp("                                           invoice_date     = lc_invoice_date");
		buildExp("                                           qty              = <ls_struc>-component-qty");
		buildExp("                                           amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty");
		buildExp("                                           is_final_invoice = abap_true ) ).");
		buildExp("");
		buildExp("    <ls_item>-component_table = VALUE #( BASE <ls_item>-component_table");
		buildExp("                                         ( invoice_number   = lc_invoice_number_2");
		buildExp("                                           posting_date     = lc_invoice_date_2");
		buildExp("                                           qty              = <ls_struc>-component-qty / 2");
		buildExp("                                           amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty / 2");
		buildExp("                                           is_final_invoice = abap_false )");
		buildExp("");
		buildExp("                                         ( invoice_number   = lc_invoice_number_3");
		buildExp("                                           posting_date     = lc_invoice_date_3");
		buildExp("                                           qty              = <ls_struc>-component-qty / 2");
		buildExp("                                           amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty / 2");
		buildExp("                                           is_final_invoice = abap_true ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testValueWithBaseAndArithmeticMoveLeft() {
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);

		buildSrc("    <ls_item>-component_table = VALUE #( BASE <ls_item>-component_table ( invoice_number   = lc_invoice_number");
		buildSrc("                                                                          invoice_date     = lc_invoice_date");
		buildSrc("                                                                          qty              = <ls_struc>-component-qty");
		buildSrc("                                                                          amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty");
		buildSrc("                                                                          is_final_invoice = abap_true ) ).");

		buildExp("    <ls_item>-component_table = VALUE #(");
		buildExp("        BASE <ls_item>-component_table");
		buildExp("        ( invoice_number   = lc_invoice_number");
		buildExp("          invoice_date     = lc_invoice_date");
		buildExp("          qty              = <ls_struc>-component-qty");
		buildExp("          amount           = <ls_struc>-component-amount_per_unit * <ls_struc>-component-qty");
		buildExp("          is_final_invoice = abap_true ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testExceptionsOthers() {
		// OTHERS is a keyword, but must be aligned like a parameter name

		buildSrc("    any_method(");
		buildSrc("      EXPORTING");
		buildSrc("        iv_any_param = lv_any_value");
		buildSrc("        iv_other_param = lv_other_value");
		buildSrc("      EXCEPTIONS");
		buildSrc("        exc_1 = 1");
		buildSrc("        exception_2 = 2");
		buildSrc("        exception_name_3 = 3");
		buildSrc("        OTHERS = 4 ).");

		buildExp("    any_method( EXPORTING  iv_any_param     = lv_any_value");
		buildExp("                           iv_other_param   = lv_other_value");
		buildExp("                EXCEPTIONS exc_1            = 1");
		buildExp("                           exception_2      = 2");
		buildExp("                           exception_name_3 = 3");
		buildExp("                           OTHERS           = 4 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReadTableWithKey() {
		buildSrc("    READ TABLE ith_table ASSIGNING <ls_entry>");
		buildSrc("         WITH KEY main_item_id = <lo_item>->ms_data-item_id");
		buildSrc("                  name = <lo_item>->ms_data-name.");
		buildSrc("");
		buildSrc("    READ TABLE lt_other_table ASSIGNING <ls_other_entry>");
		buildSrc("         WITH KEY comp_1 = ls_struc-comp_1");
		buildSrc("                  component_2 = ls_struc-component_2");
		buildSrc("                  component_name_3 = ls_struc-component_name_3.");
		buildSrc("");
		buildSrc("    READ TABLE gt_third_table ASSIGNING FIELD-SYMBOL(<ls_third_entry>)");
		buildSrc("                                WITH KEY typ = iv_typ");
		buildSrc("                                         ref = iv_ref");
		buildSrc("                                         long_component_name = lv_long_component_value");
		buildSrc("                                         last_component = lv_any_value.");

		buildExp("    READ TABLE ith_table ASSIGNING <ls_entry>");
		buildExp("         WITH KEY main_item_id = <lo_item>->ms_data-item_id");
		buildExp("                  name         = <lo_item>->ms_data-name.");
		buildExp("");
		buildExp("    READ TABLE lt_other_table ASSIGNING <ls_other_entry>");
		buildExp("         WITH KEY comp_1           = ls_struc-comp_1");
		buildExp("                  component_2      = ls_struc-component_2");
		buildExp("                  component_name_3 = ls_struc-component_name_3.");
		buildExp("");
		buildExp("    READ TABLE gt_third_table ASSIGNING FIELD-SYMBOL(<ls_third_entry>)");
		buildExp("                                WITH KEY typ                 = iv_typ");
		buildExp("                                         ref                 = iv_ref");
		buildExp("                                         long_component_name = lv_long_component_value");
		buildExp("                                         last_component      = lv_any_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	
	@Test
	void testReadTableWithKeyComponents() {
		buildSrc("    READ TABLE lt_any_table ASSIGNING <ls_any_entry>");
		buildSrc("                            WITH TABLE KEY table_key_name");
		buildSrc("                            COMPONENTS     comp_1 = <ls_data>-comp_1");
		buildSrc("                                           component_2 = <ls_data>-component_2");
		buildSrc("                                           component_name_3 = abap_true.");

		buildExp("    READ TABLE lt_any_table ASSIGNING <ls_any_entry>");
		buildExp("                            WITH TABLE KEY table_key_name");
		buildExp("                            COMPONENTS comp_1           = <ls_data>-comp_1");
		buildExp("                                       component_2      = <ls_data>-component_2");
		buildExp("                                       component_name_3 = abap_true.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testReadTableWithKeyComponentsAndAsterisk() {
		buildSrc("    READ TABLE lt_any_table ASSIGNING <ls_any_entry>");
		buildSrc("                            WITH TABLE KEY table_key_name");
		buildSrc("                            COMPONENTS     comp_1 = <ls_data>-comp_1");
		buildSrc("* component_2 = <ls_data>-component_2");
		buildSrc("                                           component_name_3 = abap_true.");

		buildExp("    READ TABLE lt_any_table ASSIGNING <ls_any_entry>");
		buildExp("                            WITH TABLE KEY table_key_name");
		buildExp("                            COMPONENTS comp_1           = <ls_data>-comp_1");
		buildExp("*                                       component_2      = <ls_data>-component_2");
		buildExp("                                       component_name_3 = abap_true.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTableExprWithKeyComponents() {
		buildSrc("    ls_result = lts_table[ KEY key_name COMPONENTS comp_a = if_any_interface=>co_any_constant");
		buildSrc("                                                   component_with_long_name = if_any_interfc_with_long_name=>co_other_constant");
		buildSrc("                                                   flag = abap_true ].");

		buildExp("    ls_result = lts_table[ KEY key_name");
		buildExp("                           COMPONENTS comp_a                   = if_any_interface=>co_any_constant");
		buildExp("                                      component_with_long_name = if_any_interfc_with_long_name=>co_other_constant");
		buildExp("                                      flag                     = abap_true ].");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallMethodBelowCallKeywordsOnOwnLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(4);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("        iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("        ev_category = lv_category");
		buildSrc("        ev_item_type = lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount.");

		buildExp("    CALL METHOD procedural_call_example");
		buildExp("      EXPORTING");
		buildExp("        iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("        iv_item_key  = '20190030000102'");
		buildExp("      IMPORTING");
		buildExp("        ev_category  = lv_category");
		buildExp("        ev_item_type = lv_item_type");
		buildExp("      CHANGING");
		buildExp("        cv_amount    = lv_amount.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallMethodBelowCallKeywordsOnOwnLineAsterisk() {
		rule.configMaxParamCountBehindProceduralCall.setValue(2);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("      iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("*  iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("*  ev_category =           lv_category");
		buildSrc("            ev_item_type =             lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount.");

		buildExp("    CALL METHOD procedural_call_example");
		buildExp("      EXPORTING");
		buildExp("        iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("*        iv_item_key  = '20190030000102'");
		buildExp("      IMPORTING");
		buildExp("*        ev_category  = lv_category");
		buildExp("        ev_item_type = lv_item_type");
		buildExp("      CHANGING");
		buildExp("        cv_amount    = lv_amount.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallMethodBelowCallKeywordsOnSameLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(4);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(false);

		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("        iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("        ev_category = lv_category");
		buildSrc("        ev_item_type = lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount.");

		buildExp("    CALL METHOD procedural_call_example");
		buildExp("      EXPORTING iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("                iv_item_key  = '20190030000102'");
		buildExp("      IMPORTING ev_category  = lv_category");
		buildExp("                ev_item_type = lv_item_type");
		buildExp("      CHANGING  cv_amount    = lv_amount.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallBadiBelowCallKeywordsOnSameLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(0);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(false);

		buildSrc("    CALL BADI lo_badi_instance->any_method");
		buildSrc("     EXPORTING");
		buildSrc("      param1 = is_struc");
		buildSrc("      parametername2 = ith_table");
		buildSrc("      par3 = ith_other_table");
		buildSrc("     CHANGING");
		buildSrc("      result = cth_result.");

		buildExp("    CALL BADI lo_badi_instance->any_method");
		buildExp("      EXPORTING param1         = is_struc");
		buildExp("                parametername2 = ith_table");
		buildExp("                par3           = ith_other_table");
		buildExp("      CHANGING  result         = cth_result.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallMethodBehindCallKeywordsOnOwnLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(5);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("        iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("        ev_category = lv_category");
		buildSrc("        ev_item_type = lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount.");

		buildExp("    CALL METHOD procedural_call_example EXPORTING");
		buildExp("                                          iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("                                          iv_item_key  = '20190030000102'");
		buildExp("                                        IMPORTING");
		buildExp("                                          ev_category  = lv_category");
		buildExp("                                          ev_item_type = lv_item_type");
		buildExp("                                        CHANGING");
		buildExp("                                          cv_amount    = lv_amount.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallMethodBehindCallKeywordsOnSameLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(5);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(false);

		buildSrc("    CALL METHOD procedural_call_example");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("        iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("        ev_category = lv_category");
		buildSrc("        ev_item_type = lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount.");

		buildExp("    CALL METHOD procedural_call_example EXPORTING iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("                                                  iv_item_key  = '20190030000102'");
		buildExp("                                        IMPORTING ev_category  = lv_category");
		buildExp("                                                  ev_item_type = lv_item_type");
		buildExp("                                        CHANGING  cv_amount    = lv_amount.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallFunctionBehindCallKeywordsOnSameLine() {
		rule.configMaxParamCountBehindProceduralCall.setValue(10);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(false);

		buildSrc("    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'");
		buildSrc("      EXPORTING");
		buildSrc("        i_date  = iv_start_date");
		buildSrc("        i_periv = lv_periv");
		buildSrc("      IMPORTING");
		buildSrc("        e_buper = lv_posting_period");
		buildSrc("        e_gjahr = lv_fiscal_year");
		buildSrc("      EXCEPTIONS");
		buildSrc("        OTHERS = 8.");

		buildExp("    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT' EXPORTING  i_date  = iv_start_date");
		buildExp("                                                      i_periv = lv_periv");
		buildExp("                                           IMPORTING  e_buper = lv_posting_period");
		buildExp("                                                      e_gjahr = lv_fiscal_year");
		buildExp("                                           EXCEPTIONS OTHERS  = 8.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFunctionalCallParamsBelowKeywordsOnSameLine() {
		rule.configMaxParamCountBehindFunctionalCall.setValue(3);
		rule.configPutFunctionalCallKeywordsOnOwnLine.setValue(false);

		buildSrc("    lv_result_variable = any_method_with_a_long_name( EXPORTING iv_any_parameter_name = lv_value");
		buildSrc("      ith_other_param_name = eth_any_table");
		buildSrc("      its_param = lts_other_table");
		buildSrc("      CHANGING cts_table = lts_table ).");

		buildExp("    lv_result_variable = any_method_with_a_long_name(");
		buildExp("                           EXPORTING iv_any_parameter_name = lv_value");
		buildExp("                                     ith_other_param_name  = eth_any_table");
		buildExp("                                     its_param             = lts_other_table");
		buildExp("                           CHANGING  cts_table             = lts_table ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFunctionalCallParamsBelowKeywordsOnOwnLine() {
		rule.configMaxParamCountBehindFunctionalCall.setValue(0);
		rule.configPutFunctionalCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    lv_result_variable = any_method_with_a_long_name( EXPORTING iv_any_parameter_name = lv_value");
		buildSrc("      ith_other_param_name = eth_any_table");
		buildSrc("      its_param = lts_other_table");
		buildSrc("      CHANGING cts_table = lts_table ).");

		buildExp("    lv_result_variable = any_method_with_a_long_name(");
		buildExp("                           EXPORTING");
		buildExp("                             iv_any_parameter_name = lv_value");
		buildExp("                             ith_other_param_name  = eth_any_table");
		buildExp("                             its_param             = lts_other_table");
		buildExp("                           CHANGING");
		buildExp("                             cts_table             = lts_table ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithoutDefaults() {
		buildSrc("    rt_result = VALUE #( ( a = 1 )");
		buildSrc("                            ( a = 2 ) ).");

		buildExp("    rt_result = VALUE #( ( a = 1 )");
		buildExp("                         ( a = 2 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithoutDefaults() {
		buildSrc("    dref = NEW ty_tt_any( ( a = 1 )");
		buildSrc("                            ( a = 2 ) ).");

		buildExp("    dref = NEW ty_tt_any( ( a = 1 )");
		buildExp("                          ( a = 2 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithoutDefaultsWithComment() {
		buildSrc("    rt_result = VALUE #( ( a = 1 )");
		buildSrc("                               \" comment");
		buildSrc("                            ( a = 2 ) ).");

		buildExp("    rt_result = VALUE #( ( a = 1 )");
		buildExp("                         \" comment");
		buildExp("                         ( a = 2 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithoutDefaultsWithComment() {
		buildSrc("    dref = NEW ty_tt_any( ( a = 1 )");
		buildSrc("                                \" comment");
		buildSrc("                             ( a = 2 ) ).");

		buildExp("    dref = NEW ty_tt_any( ( a = 1 )");
		buildExp("                          \" comment");
		buildExp("                          ( a = 2 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithMultipleRowsInLine() {
		buildSrc("    rt_result = VALUE #( ( a = 1 )");
		buildSrc("                            ( a = 2 )  ( a = 3 )");
		buildSrc("                  ( a = 4 ) ).");

		buildExp("    rt_result = VALUE #( ( a = 1 )");
		buildExp("                         ( a = 2 )");
		buildExp("                         ( a = 3 )");
		buildExp("                         ( a = 4 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithMultipleRowsInLine() {
		buildSrc("    dref = NEW ty_tt_any( ( a = 1 )");
		buildSrc("                             ( a = 2 )  ( a = 3 )");
		buildSrc("                   ( a = 4 ) ).");

		buildExp("    dref = NEW ty_tt_any( ( a = 1 )");
		buildExp("                          ( a = 2 )");
		buildExp("                          ( a = 3 )");
		buildExp("                          ( a = 4 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithMultipleRowsInLineWithComment() {
		buildSrc("    rt_result = VALUE #( ( a = 1 )");
		buildSrc("                            ( a = 2 )  ( a = 3 )");
		buildSrc("                          \" comment");
		buildSrc("                  ( a = 4 ) ).");

		buildExp("    rt_result = VALUE #( ( a = 1 )");
		buildExp("                         ( a = 2 )");
		buildExp("                         ( a = 3 )");
		buildExp("                         \" comment");
		buildExp("                         ( a = 4 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithMultipleRowsInLineWithComment() {
		buildSrc("    dref = NEW ty_tt_any( ( a = 1 )");
		buildSrc("                             ( a = 2 )  ( a = 3 )");
		buildSrc("                           \" comment");
		buildSrc("                   ( a = 4 ) ).");

		buildExp("    dref = NEW ty_tt_any( ( a = 1 )");
		buildExp("                          ( a = 2 )");
		buildExp("                          ( a = 3 )");
		buildExp("                          \" comment");
		buildExp("                          ( a = 4 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithNonStructuredLineType() {
		buildSrc("    rt_result = VALUE #( ( 'A' )");
		buildSrc("                            ( 'B' ) ).");

		buildExp("    rt_result = VALUE #( ( 'A' )");
		buildExp("                         ( 'B' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithNonStructuredLineType() {
		buildSrc("    dref = NEW ty_tt_any( ( 'A' )");
		buildSrc("                             ( 'B' ) ).");

		buildExp("    dref = NEW ty_tt_any( ( 'A' )");
		buildExp("                          ( 'B' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueTableWithNonStructuredLineTypeWithComment() {
		buildSrc("    rt_result = VALUE #( ( 'A' )");
		buildSrc("                                  \" comment");
		buildSrc("                            ( 'B' ) ).");

		buildExp("    rt_result = VALUE #( ( 'A' )");
		buildExp("                         \" comment");
		buildExp("                         ( 'B' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNewTableWithNonStructuredLineTypeWithComment() {
		buildSrc("    dref = NEW ty_tt_any( ( 'A' )");
		buildSrc("                                   \" comment");
		buildSrc("                             ( 'B' ) ).");

		buildExp("    dref = NEW ty_tt_any( ( 'A' )");
		buildExp("                          \" comment");
		buildExp("                          ( 'B' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithAsteriskComments() {
		buildSrc("    rs_result = VALUE #( a = 1");
		buildSrc("*                              bbb = 3");
		buildSrc("*         ccccc = 5");
		buildSrc("                       ddddddd = 7");
		buildSrc("                         eeeeeeeee = 9");
		buildSrc("                             fffffffffff = 11 ).");

		buildExp("    rs_result = VALUE #( a           = 1");
		buildExp("*                         bbb         = 3");
		buildExp("*                         ccccc       = 5");
		buildExp("                         ddddddd     = 7");
		buildExp("                         eeeeeeeee   = 9");
		buildExp("                         fffffffffff = 11 ).");


		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithTripleAsteriskComments() {
		buildSrc("    rs_result = VALUE #( a = 1");
		buildSrc("***      bbb = 3");
		buildSrc("***      ccccc = 5");
		buildSrc("                       ddddddd = 7");
		buildSrc("                         eeeeeeeee = 9");
		buildSrc("                             fffffffffff = 11 ).");

		buildExp("    rs_result = VALUE #( a           = 1");
		buildExp("***                         bbb         = 3");
		buildExp("***                         ccccc       = 5");
		buildExp("                         ddddddd     = 7");
		buildExp("                         eeeeeeeee   = 9");
		buildExp("                         fffffffffff = 11 ).");


		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithOverlengthAsteriskComment() {
		// since 'column widths' are determined with the executable lines only, expect 'ccccc' to match the alignment, 
		// but 'overlong_param_name' to exceed the alignment 

		buildSrc("    rs_result = VALUE #( a = 1");
		buildSrc("                           bbb = 3");
		buildSrc("*                         ccccc = 5");
		buildSrc("*                           overlong_param_name = 11");
		buildSrc("                      eeeeeeeee = 9");
		buildSrc("                        fffffff = 7 ).");

		buildExp("    rs_result = VALUE #( a         = 1");
		buildExp("                         bbb       = 3");
		buildExp("*                         ccccc     = 5");
		buildExp("*                         overlong_param_name = 11");
		buildExp("                         eeeeeeeee = 9");
		buildExp("                         fffffff   = 7 ).");


		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testInnerValueWithAsteriskComment() {
		// expect that the asterisk comment is aligned with the parameters of the (inner) VALUE statement, 
		// not with those of the method call
		
		buildSrc("    rs_result = method_call( param_1 = 'a'");
		buildSrc("    parameter_2 = VALUE #( a = 1");
		buildSrc("*  bb = 2");
		buildSrc("    ccc = 3 ) ).");

		buildExp("    rs_result = method_call( param_1     = 'a'");
		buildExp("                             parameter_2 = VALUE #( a   = 1");
		buildExp("*                                                    bb  = 2");
		buildExp("                                                    ccc = 3 ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithNonAssignmentAsteriskComments() {
		buildSrc("    rs_result = VALUE #( a = 1");
		buildSrc("** asterisk comment which has no assignment in it");
		buildSrc("* asterisk comment which has a = sign, but not after the first word");
		buildSrc("                       bb = 2");
		buildSrc("* <field_symbol> = nothing that should be aligned here!");
		buildSrc("                     ccc = 3 ).");

		buildExp("    rs_result = VALUE #( a   = 1");
		buildExp("** asterisk comment which has no assignment in it");
		buildExp("* asterisk comment which has a = sign, but not after the first word");
		buildExp("                         bb  = 2");
		buildExp("* <field_symbol> = nothing that should be aligned here!");
		buildExp("                         ccc = 3 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithBaseAndRowBehind() {
		buildSrc("    <ls_item>-table_with_long_name = VALUE #( BASE <ls_item>-table_with_long_name ( change_number  = lc_change_number");
		buildSrc("                                                                                    effective_date = lc_change_date");
		buildSrc("*                                                                                    item_name = 'ANY_ITEM_NAME'");
		buildSrc("                                                                                    amount = lc_any_amount_with_a_long_name ) ).");

		buildExp("    <ls_item>-table_with_long_name = VALUE #( BASE <ls_item>-table_with_long_name");
		buildExp("                                              ( change_number  = lc_change_number");
		buildExp("                                                effective_date = lc_change_date");
		buildExp("*                                                item_name      = 'ANY_ITEM_NAME'");
		buildExp("                                                amount         = lc_any_amount_with_a_long_name ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTextLiteralWithTextSymbolID() {
		// ensure that a text symbol ID which is linked to a text field literal - e.g. 'List of Users'(101) - 
		// is not mistaken for an inner parenthesis
		
		buildSrc("    set_list_header( 'List of Users'(101)  ##text_diff");
		buildSrc("                     && | { sy-mandt } |");
		buildSrc("                     && 'Number of Entries:'(102)  ##text_diff");
		buildSrc("                     && | { lines( gt_user_list ) }| ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithCalculations() {
		// ensure that calculation parentheses are not moved to the next line
		
		buildSrc("    rs_result = VALUE #( name   = <ls_result>-name");
		buildSrc("                         extent = ( <ls_result>-width");
		buildSrc("                                + <ls_result>-height ) / 100");
		buildSrc("                         weight = lv_count * ( lv_base_weight + <ls_result>-weight ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodInsideArithmetic() {
		buildSrc("    rs_result = a * ( b + get_value( p = 1");
		buildSrc("          param2 = 2");
		buildSrc("            parameter3 = 3 ) ).");

		buildExp("    rs_result = a * ( b + get_value( p          = 1");
		buildExp("                                     param2     = 2");
		buildExp("                                     parameter3 = 3 ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodAndValueInsideArithmetic() {
		buildSrc("    rs_result = a * ( b + get_value( tab = VALUE #( ( p = 2");
		buildSrc("     q = 4 )");
		buildSrc("         ( p = 3");
		buildSrc("    q = 9 ) )");
		buildSrc("     name = 'abc' ) ).");

		buildExp("    rs_result = a * ( b + get_value( tab  = VALUE #( ( p = 2");
		buildExp("                                                       q = 4 )");
		buildExp("                                                     ( p = 3");
		buildExp("                                                       q = 9 ) )");
		buildExp("                                     name = 'abc' ) ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testParamNameTooShortForMoveLeftOfAssignOp() {
		// expect the long content to be moved as much left as possible, but in this case NOT left of the assignment operator, 
		// because too little is gained (and layout awkward) in case of the short parameter name "exp ="  
		
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);

		buildSrc("    cl_abap_unit_assert=>assert_equals(");
		buildSrc("      act = lts_any_table");
		buildSrc("      exp = VALUE ty_ts_any_type( item_type = 'ABCD'  any_flag = ' '");
		buildSrc("                      ( item_key = '20200030000101'  other_flag = 'X'  any_amount = 0     other_amount = 40  any_quantity = 30  other_quantity =  0  )");
		buildSrc("                      ( item_key = '20200030000102'  other_flag = ' '  any_amount = '-80' other_amount = 80  any_quantity =  0  other_quantity = 90  ) ) ).");

		buildExp("    cl_abap_unit_assert=>assert_equals(");
		buildExp("        act = lts_any_table");
		buildExp("        exp = VALUE ty_ts_any_type(");
		buildExp("                  item_type = 'ABCD'");
		buildExp("                  any_flag  = ' '");
		buildExp("                  ( item_key = '20200030000101'  other_flag = 'X'  any_amount = 0     other_amount = 40  any_quantity = 30  other_quantity =  0  )");
		buildExp("                  ( item_key = '20200030000102'  other_flag = ' '  any_amount = '-80' other_amount = 80  any_quantity =  0  other_quantity = 90  ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testParamNameNotTooShortForMoveLeft() {
		// although the assignment is to the short variable name "exp =", expect the long content to be moved left of the assignment operator, 
		// because "exp     =" has enough enough space  
		
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.TO_KEEP_MAX_LINE_LENGTH);

		buildSrc("    cl_abap_unit_assert=>assert_equals(");
		buildSrc("      act = lts_any_table");
		buildSrc("      exp = VALUE ty_ts_any_type( item_type = 'ABCD'  any_flag = ' '");
		buildSrc("                      ( item_key = '20200030000101'  other_flag = 'X'  any_amount = 0     other_amount = 40  any_quantity = 30  other_quantity =  0  )");
		buildSrc("                      ( item_key = '20200030000102'  other_flag = ' '  any_amount = '-80' other_amount = 80  any_quantity =  0  other_quantity = 90  ) )");
		buildSrc("      message = 'text' ).");

		buildExp("    cl_abap_unit_assert=>assert_equals(");
		buildExp("        act     = lts_any_table");
		buildExp("        exp     = VALUE ty_ts_any_type(");
		buildExp("            item_type = 'ABCD'");
		buildExp("            any_flag  = ' '");
		buildExp("            ( item_key = '20200030000101'  other_flag = 'X'  any_amount = 0     other_amount = 40  any_quantity = 30  other_quantity =  0  )");
		buildExp("            ( item_key = '20200030000102'  other_flag = ' '  any_amount = '-80' other_amount = 80  any_quantity =  0  other_quantity = 90  ) )");
		buildExp("        message = 'text' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testArithmeticExprInsideStringTemplate() {
		// ensure that the inner parentheses are not mistaken to be rows that should be aligned 
		
		buildSrc("    out->write( |Text { ( var1 - var2 ) / 10 }| ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMethodInsideStringTemplate() {
		// ensure that the inner parentheses are not mistaken to be rows that should be aligned
		
		buildSrc("    out->write( |Text { any_method( iv_param_a = 1");
		buildSrc("      iv_param_b = 2 ) }| ).");

		buildExp("    out->write( |Text { any_method( iv_param_a = 1");
		buildExp("                                    iv_param_b = 2 ) }| ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMethodInsideArithInsideStringTemplate() {
		// ensure that the inner parentheses are not mistaken to be rows that should be aligned
		
		buildSrc("    out->write( |Text { 5 * ( 3 + any_method( iv_param_a = 1");
		buildSrc("      iv_param_b = 2 ) ) }| ).");

		buildExp("    out->write( |Text { 5 * ( 3 + any_method( iv_param_a = 1");
		buildExp("                                              iv_param_b = 2 ) ) }| ).");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testKeepRowsInValueConstructorOnSingleLine() {
		// ensure that a VALUE constructor with multiple rows can be kept on a single line - 
		// rather than putting ( 1 ), ( 2 ), and ( 3 ) below each other 
		
		buildSrc("    any_method( VALUE #( ( a = 1  b = 'A'  c = VALUE #( ( 1 ) ( 2 ) ( 3 ) ) )");
		buildSrc("                         ( a = 1  b = 'B'  c = VALUE #( ( 4 ) ( 5 ) ) )");
		buildSrc("                         ( a = 2  b = 'A'  c = VALUE #( ( 6 ) ( 7 ) ( 8 ) ) )");
		buildSrc("                         ( a = 2  b = 'B'  c = VALUE #( ( 9 ) ) ) ) ).");

		copyExpFromSrc();

		testRule();
	}
	
	@Test
	void testClosingBracketLengthConsidered() {
		// expect that closing brackets and any text attached to them (here: "]-third_long_component_name.") is considered 
		// in the line length decision (this example requires exactly 120 chars to continue behind the opening "[") 
		
		rule.configMaxLineLength.setValue(115);

		buildSrc("   mv_extra_long_variable_name = lts_extra_long_table_name[ first_long_component_name = 11");
		buildSrc("                                                            second_long_component_name = 22 ]-third_long_component_name.");

		buildExp("   mv_extra_long_variable_name = lts_extra_long_table_name[");
		buildExp("                                     first_long_component_name  = 11");
		buildExp("                                     second_long_component_name = 22 ]-third_long_component_name.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testClosingCommentLengthNotConsidered() {
		// expect that closing comment to NOT be considered in the line length decision (even though it is directly attached) 
		
		rule.configMaxLineLength.setValue(115);

		buildSrc("   ms_extra_long_variable_name = lts_extra_long_table_name[ first_long_component_name = 11");
		buildSrc("                                                            second_long_component_name = 22 ].\"attached_comment_with_long_text");

		buildExp("   ms_extra_long_variable_name = lts_extra_long_table_name[ first_long_component_name  = 11");
		buildExp("                                                            second_long_component_name = 22 ].\"attached_comment_with_long_text");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testRaiseExceptionParameters() {
		buildSrc("   RAISE EXCEPTION TYPE cx_any");
		buildSrc("     EXPORTING");
		buildSrc("       iv_msgid = '123'");
		buildSrc("       iv_param2 = 2");
		buildSrc("       iv_parameter3 = 3.");

		buildExp("   RAISE EXCEPTION TYPE cx_any");
		buildExp("     EXPORTING iv_msgid      = '123'");
		buildExp("               iv_param2     = 2");
		buildExp("               iv_parameter3 = 3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExceptionParametersBehindCall() {
		rule.configMaxParamCountBehindProceduralCall.setValue(2);

		buildSrc("   RAISE EXCEPTION TYPE cx_any");
		buildSrc("     EXPORTING");
		buildSrc("       iv_msgid = '123'");
		buildSrc("       iv_param2 = 2.");

		buildExp("   RAISE EXCEPTION TYPE cx_any EXPORTING iv_msgid  = '123'");
		buildExp("                                         iv_param2 = 2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExceptionBreakParameters() {
		buildSrc("   RAISE EXCEPTION TYPE cx_any EXPORTING iv_msgid  = '123'");
		buildSrc("                                         iv_param2 = 2.");

		buildExp("   RAISE EXCEPTION TYPE cx_any");
		buildExp("     EXPORTING iv_msgid  = '123'");
		buildExp("               iv_param2 = 2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithInAtLineEnd() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("               ccc = 3 IN");
		buildSrc("               comp1 = a");
		buildSrc("              component2 = bb");
		buildSrc("             c3 = ccc ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3 IN");
		buildExp("                 comp1      = a");
		buildExp("                 component2 = bb");
		buildExp("                 c3         = ccc ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithInOnNewLine() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("             ccc = 3");
		buildSrc("               IN comp1 = a");
		buildSrc("              component2 = bb");
		buildSrc("             c3 = cc ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3");
		buildExp("                 IN  comp1      = a");
		buildExp("                     component2 = bb");
		buildExp("                     c3         = cc ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithCalculationAndInAtLineEnd() {
		buildSrc("    DATA(foo) = VALUE #(");
		buildSrc("    LET x = 2");
		buildSrc("        y = x ** 2");
		buildSrc("        z = x ** 3 IN");
		buildSrc("    col1 = x + z");
		buildSrc("    col2 = y + z ).");

		buildExp("    DATA(foo) = VALUE #( LET x = 2");
		buildExp("                             y = x ** 2");
		buildExp("                             z = x ** 3 IN");
		buildExp("                         col1 = x + z");
		buildExp("                         col2 = y + z ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithCalculationAndInOnNewLine() {
		buildSrc("    DATA(foo) = VALUE #(");
		buildSrc("    LET x = 2");
		buildSrc("        y = x ** 2");
		buildSrc("        z = x ** 3");
		buildSrc("    IN col1 = x + z");
		buildSrc("    col2 = y + z ).");

		buildExp("    DATA(foo) = VALUE #( LET x = 2");
		buildExp("                             y = x ** 2");
		buildExp("                             z = x ** 3");
		buildExp("                         IN  col1 = x + z");
		buildExp("                             col2 = y + z ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithTableRows() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("               ccc = 3 IN");
		buildSrc("           ( x = a   y = bb  z = ccc )");
		buildSrc("             ( x = bb  y = a   z = ccc ) ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3 IN");
		buildExp("                 ( x = a   y = bb  z = ccc )");
		buildExp("                 ( x = bb  y = a   z = ccc ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithTableRowsAndInOnNewLine() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("               ccc = 3");
		buildSrc("           IN ( x = a   y = bb   z = ccc )");
		buildSrc("           ( x = bb  y = a    z = ccc )");
		buildSrc("        ( x = a   y = ccc  z = bb ) ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3");
		buildExp("                 IN  ( x = a   y = bb   z = ccc )");
		buildExp("                     ( x = bb  y = a    z = ccc )");
		buildExp("                     ( x = a   y = ccc  z = bb ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithParamsAndTableRows() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("               ccc = 3 IN");
		buildSrc("");
		buildSrc("         z = ccc");
		buildSrc("           ( x = a   y = bb )");
		buildSrc("             ( x = bb  y = a ) ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3 IN");
		buildExp("");
		buildExp("                 z = ccc");
		buildExp("                 ( x = a   y = bb )");
		buildExp("                 ( x = bb  y = a ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLetWithParamsAndTableRowsAndInOnNewLine() {
		buildSrc("    a = VALUE #( LET a = 1");
		buildSrc("                   bb = 2");
		buildSrc("               ccc = 3");
		buildSrc("");
		buildSrc("           IN z = ccc");
		buildSrc("           ( x = a   y = bb )");
		buildSrc("           ( x = bb  y = a ) ).");

		buildExp("    a = VALUE #( LET a   = 1");
		buildExp("                     bb  = 2");
		buildExp("                     ccc = 3");
		buildExp("");
		buildExp("                 IN  z = ccc");
		buildExp("                     ( x = a   y = bb )");
		buildExp("                     ( x = bb  y = a ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMoveLetExpressionLeftOfAssignOp() {
		rule.configMaxLineLength.setValue(80);
		rule.configAllowContentLeftOfAssignOp.setEnumValue(ContentLeftOfAssignOp.ALWAYS);

		buildSrc("    DATA(lv_variable_with_long_name) = VALUE #( LET a = 1");
		buildSrc("                                                  bb = 2");
		buildSrc("                                                     ccc = 3");
		buildSrc("");
		buildSrc("                                      IN z = ccc");
		buildSrc("                                       ( long_component_name = a   y = bb )");
		buildSrc("                                         ( long_component_name = bb  y = a ) ).");

		buildExp("    DATA(lv_variable_with_long_name) = VALUE #(");
		buildExp("        LET a   = 1");
		buildExp("            bb  = 2");
		buildExp("            ccc = 3");
		buildExp("");
		buildExp("        IN  z = ccc");
		buildExp("            ( long_component_name = a   y = bb )");
		buildExp("            ( long_component_name = bb  y = a ) ).");

		testRule();
	}

	@Test
	void testValueWithForWhereParenthesis() {
		// expect the parenthesis with the logical expression to be align with FOR, but to be otherwise unchanged
		
		buildSrc(" rt_any_table = VALUE #( FOR <item> IN lt_item WHERE");
		buildSrc("                            ( comp_a  IN it_comp_a AND");
		buildSrc("                              comp_b  IN it_comp_b )");
		buildSrc("                            ( <alias> ) ).");

		buildExp(" rt_any_table = VALUE #( FOR <item> IN lt_item WHERE");
		buildExp("                         ( comp_a  IN it_comp_a AND");
		buildExp("                           comp_b  IN it_comp_b )");
		buildExp("                         ( <alias> ) ).");

		testRule();
	}

	@Test
	void testValueWithForWhile() {
		buildSrc(" rt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE i < 10");
		buildSrc("                      ( any_comp = '1'");
		buildSrc("                        any_amount = i ) ).");

		buildExp(" rt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE i < 10");
		buildExp("                   ( any_comp   = '1'");
		buildExp("                     any_amount = i ) ).");

		testRule();
	}


	@Test
	void testValueWithForWhileParenthesis() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE");
		buildSrc("                        (     i < 10");
		buildSrc("                          AND i < lv_limit )");
		buildSrc("                        ( any_comp = '1'");
		buildSrc("                          any_amount = i ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE");
		buildExp("                      (     i < 10");
		buildExp("                        AND i < lv_limit )");
		buildExp("                      ( any_comp   = '1'");
		buildExp("                        any_amount = i ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testValueWithForUntil() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = 10");
		buildSrc("                        ( any_comp = '1'");
		buildSrc("                          any_amount = i ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = 10");
		buildExp("                      ( any_comp   = '1'");
		buildExp("                        any_amount = i ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testValueWithForUntilParenthesis() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL");
		buildSrc("                        (    i = 10");
		buildSrc("                          OR i > lv_limit )");
		buildSrc("                        ( any_comp = '1'");
		buildSrc("                          any_amount = i ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL");
		buildExp("                      (    i = 10");
		buildExp("                        OR i > lv_limit )");
		buildExp("                      ( any_comp   = '1'");
		buildExp("                        any_amount = i ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithForWhileLet() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE i < 10");
		buildSrc("                        LET j = i * 10");
		buildSrc("                         kk = i * 100 IN");
		buildSrc("                        ( any_comp = '1'");
		buildSrc("                          any_amount = j");
		buildSrc("                          other_amount = kk ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 WHILE i < 10");
		buildExp("                      LET j  = i * 10");
		buildExp("                          kk = i * 100 IN");
		buildExp("                      ( any_comp     = '1'");
		buildExp("                        any_amount   = j");
		buildExp("                        other_amount = kk ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithForUntilLet() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = 10");
		buildSrc("                     LET j = i * 10");
		buildSrc("                           kk = i * 100 IN");
		buildSrc("                       ( any_comp = '1'");
		buildSrc("                          any_amount = j");
		buildSrc("                          other_amount = kk ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL i = 10");
		buildExp("                      LET j  = i * 10");
		buildExp("                          kk = i * 100 IN");
		buildExp("                      ( any_comp     = '1'");
		buildExp("                        any_amount   = j");
		buildExp("                        other_amount = kk ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueWithForUntilParenthesisLet() {
		buildSrc("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL");
		buildSrc("                        (    i = 10");
		buildSrc("                          OR i > lv_limit )");
		buildSrc("                          LET j = i * 10");
		buildSrc("                               kk = i * 100 IN");
		buildSrc("                        ( any_comp = '1'");
		buildSrc("                          any_amount = j");
		buildSrc("                          other_amount = kk ) ).");

		buildExp("    lt_any = VALUE #( FOR i = 1 THEN i + 1 UNTIL");
		buildExp("                      (    i = 10");
		buildExp("                        OR i > lv_limit )");
		buildExp("                      LET j  = i * 10");
		buildExp("                          kk = i * 100 IN");
		buildExp("                      ( any_comp     = '1'");
		buildExp("                        any_amount   = j");
		buildExp("                        other_amount = kk ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseShortdumpType() {
		buildSrc("    RAISE SHORTDUMP TYPE cx_any_type");
		buildSrc("      EXPORTING");
		buildSrc("        any_param = 1");
		buildSrc("        other_param = 'text'");
		buildSrc("        long_parameter_name = VALUE #( ).");

		buildExp("    RAISE SHORTDUMP TYPE cx_any_type");
		buildExp("      EXPORTING any_param           = 1");
		buildExp("                other_param         = 'text'");
		buildExp("                long_parameter_name = VALUE #( ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseShortdumpNew() {
		buildSrc("    RAISE SHORTDUMP NEW cx_any_type(");
		buildSrc("        any_param = 1");
		buildSrc("        other_param = 'text'");
		buildSrc("        long_parameter_name = VALUE #( ) ).");

		buildExp("    RAISE SHORTDUMP NEW cx_any_type( any_param           = 1");
		buildExp("                                     other_param         = 'text'");
		buildExp("                                     long_parameter_name = VALUE #( ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testCallMethodWithParens() {
		rule.configMaxParamCountBehindProceduralCall.setValue(4);

		buildSrc("    CALL METHOD any_method(");
		buildSrc("      EXPORTING");
		buildSrc("        iv_item_id = lo_item1->ms_data-item_id");
		buildSrc("        iv_item_key = '20190030000102'");
		buildSrc("      IMPORTING");
		buildSrc("        ev_category = lv_category");
		buildSrc("        ev_item_type = lv_item_type");
		buildSrc("      CHANGING");
		buildSrc("        cv_amount = lv_amount ).");

		buildExp("    CALL METHOD any_method(");
		buildExp("      EXPORTING iv_item_id   = lo_item1->ms_data-item_id");
		buildExp("                iv_item_key  = '20190030000102'");
		buildExp("      IMPORTING ev_category  = lv_category");
		buildExp("                ev_item_type = lv_item_type");
		buildExp("      CHANGING  cv_amount    = lv_amount ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDisalignComplexParameters() {
		rule.configAlignAssignments.setValue(false);
		
		buildSrc("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildSrc("                      EXPORTING iv_item_id        = lo_item->ms_data-item_id");
		buildSrc("                                iv_text           = 'abcde'");
		buildSrc("                                iv_condition_type = if_any_interface=>co_condition_type");
		buildSrc("                                iv_item_type      = lo_item->get_item_data( )-item_type");
		buildSrc("                                iv_amount         = 8 ).");

		buildExp("    lts_any_table = cl_any_class_with_long_name=>create_table(");
		buildExp("                      EXPORTING iv_item_id = lo_item->ms_data-item_id");
		buildExp("                                iv_text = 'abcde'");
		buildExp("                                iv_condition_type = if_any_interface=>co_condition_type");
		buildExp("                                iv_item_type = lo_item->get_item_data( )-item_type");
		buildExp("                                iv_amount = 8 ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDisalignMethodAndValueInsideArithmetic() {
		rule.configAlignAssignments.setValue(false);
		
		buildSrc("    rs_result = a * ( b + get_value( c   = VALUE #( ( e   = 2");
		buildSrc("                                                      fff = 4 )");
		buildSrc("                                                    ( e   = 3");
		buildSrc("                                                      fff = 9 ) )");
		buildSrc("                                     ddd = 'abc' ) ).");
		
		buildExp("    rs_result = a * ( b + get_value( c = VALUE #( ( e = 2");
		buildExp("                                                    fff = 4 )");
		buildExp("                                                  ( e = 3");
		buildExp("                                                    fff = 9 ) )");
		buildExp("                                     ddd = 'abc' ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDisalignRaiseExceptionParameters() {
		rule.configAlignAssignments.setValue(false);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("   RAISE EXCEPTION TYPE cx_any");
		buildSrc("     EXPORTING iv_msgid      = '123'");
		buildSrc("               iv_param2     = 2");
		buildSrc("               iv_parameter3 = 3.");

		buildExp("   RAISE EXCEPTION TYPE cx_any");
		buildExp("     EXPORTING");
		buildExp("       iv_msgid = '123'");
		buildExp("       iv_param2 = 2");
		buildExp("       iv_parameter3 = 3.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDisalignLetWithInAtLineEnd() {
		rule.configAlignAssignments.setValue(false);

		buildSrc("    a = VALUE #( LET a   = 1");
		buildSrc("                     bb  = 2");
		buildSrc("                     ccc = 3 IN");
		buildSrc("                 comp1      = a");
		buildSrc("                 component2 = bb");
		buildSrc("                 c3         = ccc ).");

		buildExp("    a = VALUE #( LET a = 1");
		buildExp("                     bb = 2");
		buildExp("                     ccc = 3 IN");
		buildExp("                 comp1 = a");
		buildExp("                 component2 = bb");
		buildExp("                 c3 = ccc ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDisalignCallMethodWithParens() {
		rule.configAlignAssignments.setValue(false);
		rule.configMaxParamCountBehindProceduralCall.setValue(4);
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    CALL METHOD any_method(");
		buildSrc("      EXPORTING iv_item_id   = lo_item1->ms_data-item_id");
		buildSrc("                iv_item_key  = '20190030000102'");
		buildSrc("      IMPORTING ev_category  = lv_category");
		buildSrc("                ev_item_type = lv_item_type");
		buildSrc("      CHANGING  cv_amount    = lv_amount ).");

		buildExp("    CALL METHOD any_method(");
		buildExp("      EXPORTING");
		buildExp("        iv_item_id = lo_item1->ms_data-item_id");
		buildExp("        iv_item_key = '20190030000102'");
		buildExp("      IMPORTING");
		buildExp("        ev_category = lv_category");
		buildExp("        ev_item_type = lv_item_type");
		buildExp("      CHANGING");
		buildExp("        cv_amount = lv_amount ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithFor() {
		buildSrc("    lt_any = VALUE #(");
		buildSrc("    FOR <s> IN lt_table");
		buildSrc("    ( <s> ) ).");

		buildExp("    lt_any = VALUE #( FOR <s> IN lt_table");
		buildExp("                      ( <s> ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithBaseFor() {
		buildSrc("    lt_any = VALUE #(");
		buildSrc("    BASE lt_any");
		buildSrc("    FOR <s> IN lt_table");
		buildSrc("    ( <s> ) ).");

		buildExp("    lt_any = VALUE #( BASE lt_any");
		buildExp("                      FOR <s> IN lt_table");
		buildExp("                      ( <s> ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testValueWithLetBaseForFor() {
		buildSrc("lt_test = VALUE #(");
		buildSrc("LET a = get_a( any_param = 1");
		buildSrc("other_param = 2 )");
		buildSrc("b = get_b( ) IN");
		buildSrc("BASE lt_test");
		buildSrc("FOR <ls_table> IN lt_table");
		buildSrc("FOR <ls_inner> IN <ls_table>-inner");
		buildSrc("( id = <ls_inner>-id");
		buildSrc("a = a");
		buildSrc("b = b ) ).");

		buildExp("lt_test = VALUE #( LET a = get_a( any_param   = 1");
		buildExp("                                  other_param = 2 )");
		buildExp("                       b = get_b( ) IN");
		buildExp("                   BASE lt_test");
		buildExp("                   FOR <ls_table> IN lt_table");
		buildExp("                   FOR <ls_inner> IN <ls_table>-inner");
		buildExp("                   ( id = <ls_inner>-id");
		buildExp("                     a  = a");
		buildExp("                     b  = b ) ).");

		testRule();
	}

	@Test
	void testInlineDeclarationUnchanged() {
		// ensure that the contents of inline declaration parentheses etc. are not mistaken for method call parameters
		buildSrc("    DATA(lv_any) = get_value( ).");
		buildSrc("    lv_any(4) = 'abcd'.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCallDynamicMethod() {
		buildSrc("    CALL METHOD me->(lc_method_name) EXPORTING iv_value = lv_value");
		buildSrc("                                     RECEIVING rv_result = lv_result.");

		buildExp("    CALL METHOD me->(lc_method_name)");
		buildExp("      EXPORTING iv_value  = lv_value");
		buildExp("      RECEIVING rv_result = lv_result.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCondenseOneLinersWithOneComponent() {
		buildSrc("    any_method(    iv_param   =   lv_value ).");
		buildSrc("");
		buildSrc("    ls_struct = VALUE #(   comp   =   'a' ).");
		buildSrc("");
		buildSrc("    READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<ls_data>) WITH KEY comp_a   =   'a'.");

		buildExp("    any_method( iv_param = lv_value ).");
		buildExp("");
		buildExp("    ls_struct = VALUE #( comp = 'a' ).");
		buildExp("");
		buildExp("    READ TABLE lt_data ASSIGNING FIELD-SYMBOL(<ls_data>) WITH KEY comp_a = 'a'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTableRowOneLinersUnchanged() {
		buildSrc("lt_table = VALUE #( ( price =  1 ) ).");
		buildSrc("");
		buildSrc("lt_table = VALUE #( ( price =  1 )");
		buildSrc("                    ( price = 10 ) ).");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCreateObject() {
		buildSrc("    CREATE OBJECT lo_any_object");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = iv_any_value");
		buildSrc("        iv_second_param = lv_other_value");
		buildSrc("      EXCEPTIONS");
		buildSrc("        any_exception = 1");
		buildSrc("        other_exception = 2");
		buildSrc("        OTHERS = 3.");

		buildExp("    CREATE OBJECT lo_any_object");
		buildExp("      EXPORTING  iv_param        = iv_any_value");
		buildExp("                 iv_second_param = lv_other_value");
		buildExp("      EXCEPTIONS any_exception   = 1");
		buildExp("                 other_exception = 2");
		buildExp("                 OTHERS          = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreateObjectWithAreaHandleAndType() {
		buildSrc("    CREATE OBJECT lo_any_object AREA HANDLE handle TYPE any_type");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = iv_any_value");
		buildSrc("        iv_second_param = lv_other_value.");

		buildExp("    CREATE OBJECT lo_any_object AREA HANDLE handle TYPE any_type");
		buildExp("      EXPORTING iv_param        = iv_any_value");
		buildExp("                iv_second_param = lv_other_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCreateObjectWithDynamicType() {
		buildSrc("    CREATE OBJECT lo_any_object TYPE (lv_type_name)");
		buildSrc("      EXPORTING");
		buildSrc("        iv_param = iv_any_value");
		buildSrc("        iv_second_param = lv_other_value.");

		buildExp("    CREATE OBJECT lo_any_object TYPE (lv_type_name)");
		buildExp("      EXPORTING iv_param        = iv_any_value");
		buildExp("                iv_second_param = lv_other_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testStartRightOfCallChain() {
		// ensure that the parameters are not moved left of LO_FACTORY (i.e. the beginning of the call chain) 
		buildSrc("        INSERT LINES OF lo_factory=>get( )->get_any_utility( )->get_any_table( iv_long_parameter_name = lv_long_variable_name");
		buildSrc("                                                                               iv_extra_long_parameter_name = lv_extra_long_variable_name )");
		buildSrc("               INTO TABLE lts_any_table.");

		buildExp("        INSERT LINES OF lo_factory=>get( )->get_any_utility( )->get_any_table(");
		buildExp("                            iv_long_parameter_name       = lv_long_variable_name");
		buildExp("                            iv_extra_long_parameter_name = lv_extra_long_variable_name )");
		buildExp("               INTO TABLE lts_any_table.");

		testRule();
	}

	@Test
	void testParenthesesAfterWhereUnchanged() {
		// expect "( comp = 1 )" to NOT be moved to the next line
		buildSrc("    rts_result = VALUE ty_ts_table( FOR <ls_struc> IN mts_source");
		buildSrc("                                    WHERE ( comp = 1 )");
		buildSrc("                                    ( <ls_key> ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCompleteWhereClauseMoved() {
		buildSrc("    rts_result = VALUE ty_ts_table( FOR <ls_struc> IN mts_source");
		buildSrc("WHERE (    comp = 1");
		buildSrc("        OR comp = 2 )");
		buildSrc("                                    ( <ls_key> ) ).");

		buildExp("    rts_result = VALUE ty_ts_table( FOR <ls_struc> IN mts_source");
		buildExp("                                    WHERE (    comp = 1");
		buildExp("                                            OR comp = 2 )");
		buildExp("                                    ( <ls_key> ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testWhereParenthesisMoved() {
		buildSrc("    rts_result = VALUE ty_ts_table( FOR <ls_struc> IN mts_source WHERE");
		buildSrc("(    comp = 1");
		buildSrc("  OR comp = 2 )");
		buildSrc("                                    ( <ls_key> ) ).");

		buildExp("    rts_result = VALUE ty_ts_table( FOR <ls_struc> IN mts_source WHERE");
		buildExp("                                    (    comp = 1");
		buildExp("                                      OR comp = 2 )");
		buildExp("                                    ( <ls_key> ) ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOuterParenthesesObserved() {
		// ensure that the components in the inner VALUE constructors are NOT moved further to the left than the components 
		// of the outer VALUE constructor
		rule.configMaxLineLength.setValue(100);

		buildSrc("    rs_result = VALUE #( comp_1 = VALUE #(");
		buildSrc("                            number = '001'");
		buildSrc("                            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1' )");
		buildSrc("                        comp_2 = VALUE #(");
		buildSrc("                            number = '002'");
		buildSrc("                            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2' ) ).");

		buildExp("    rs_result = VALUE #( comp_1 = VALUE #(");
		buildExp("                             number = '001'");
		buildExp("                             text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1' )");
		buildExp("                         comp_2 = VALUE #(");
		buildExp("                             number = '002'");
		buildExp("                             text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCondenseNestedConstructors() {
		rule.configMaxLineLength.setValue(80);

		buildSrc("    rs_result = VALUE #( comp_1 = VALUE #(");
		buildSrc("                            number = '001'");
		buildSrc("                            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1' )");
		buildSrc("                        comp_2 = VALUE #(");
		buildSrc("                            number = '002'");
		buildSrc("                            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2' ) ).");

		buildExp("    rs_result = VALUE #(");
		buildExp("        comp_1 = VALUE #(");
		buildExp("            number = '001'");
		buildExp("            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1' )");
		buildExp("        comp_2 = VALUE #(");
		buildExp("            number = '002'");
		buildExp("            text   = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAcrossTableRows() {
		buildSrc("    ets_table = VALUE #( date = gc_any_date");
		buildSrc("                           id = gc_any_id");
		buildSrc("");
		buildSrc("                           ( item_name = 'ANY'");
		buildSrc("                              quantity = 1 )");
		buildSrc("                             ( item_name = 'OTHER'");
		buildSrc("                                quantity = 2");
		buildSrc("                                reference_id = '12345' )");
		buildSrc("                                   ( item_name = 'THIRD'");
		buildSrc("                                  quantity = 3 ) ).");

		buildExp("    ets_table = VALUE #( date = gc_any_date");
		buildExp("                         id   = gc_any_id");
		buildExp("");
		buildExp("                         ( item_name    = 'ANY'");
		buildExp("                           quantity     = 1 )");
		buildExp("                         ( item_name    = 'OTHER'");
		buildExp("                           quantity     = 2");
		buildExp("                           reference_id = '12345' )");
		buildExp("                         ( item_name    = 'THIRD'");
		buildExp("                           quantity     = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignWithinTableRows() {
		rule.configAlignAcrossTableRows.setValue(false);
		
		buildSrc("    ets_table = VALUE #( date = gc_any_date");
		buildSrc("                           id = gc_any_id");
		buildSrc("");
		buildSrc("                           ( item_name = 'ANY'");
		buildSrc("                              quantity = 1 )");
		buildSrc("                             ( item_name = 'OTHER'");
		buildSrc("                                quantity = 2");
		buildSrc("                                reference_id = '12345' )");
		buildSrc("                                   ( item_name = 'THIRD'");
		buildSrc("                                  quantity = 3 ) ).");

		buildExp("    ets_table = VALUE #( date = gc_any_date");
		buildExp("                         id   = gc_any_id");
		buildExp("");
		buildExp("                         ( item_name = 'ANY'");
		buildExp("                           quantity  = 1 )");
		buildExp("                         ( item_name    = 'OTHER'");
		buildExp("                           quantity     = 2");
		buildExp("                           reference_id = '12345' )");
		buildExp("                         ( item_name = 'THIRD'");
		buildExp("                           quantity  = 3 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAcrossTableRowsKeepingOneLiners() {
		// ensure that alignment across table rows works even 
		// - across comments 
		// - across other assignments (DATE = GC_OTHER_DATE) 
		// - across one-liner rows
		// and that one-liners are kept
		
		buildSrc("    ets_table = VALUE #( date = gc_any_date");
		buildSrc("                           id = gc_any_id");
		buildSrc("");
		buildSrc("                           ( item_name = 'ANY'");
		buildSrc("                              quantity = 1 )");
		buildSrc("                             \" comment");
		buildSrc("                             ( item_name = 'OTHER'");
		buildSrc("                                quantity = 2");
		buildSrc("                                reference_id = '12345' )");
		buildSrc("");
		buildSrc("                           date = gc_other_date");
		buildSrc("                           id = gc_other_id");
		buildSrc("                                   ( item_name = 'THIRD'  quantity = 3 )");
		buildSrc("                                   ( item_name = 'FOURTH' quantity = 4 )");
		buildSrc("");
		buildSrc("                                   ( item_name = 'FIFTH'");
		buildSrc("                                  quantity = 5 ) ).");

		buildExp("    ets_table = VALUE #( date = gc_any_date");
		buildExp("                         id   = gc_any_id");
		buildExp("");
		buildExp("                         ( item_name    = 'ANY'");
		buildExp("                           quantity     = 1 )");
		buildExp("                         \" comment");
		buildExp("                         ( item_name    = 'OTHER'");
		buildExp("                           quantity     = 2");
		buildExp("                           reference_id = '12345' )");
		buildExp("");
		buildExp("                         date = gc_other_date");
		buildExp("                         id   = gc_other_id");
		buildExp("                         ( item_name = 'THIRD'  quantity = 3 )");
		buildExp("                         ( item_name = 'FOURTH' quantity = 4 )");
		buildExp("");
		buildExp("                         ( item_name    = 'FIFTH'");
		buildExp("                           quantity     = 5 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignAsteriskCommentAcrossTableRows() {
		// ensure that alignment across table rows even works for the commented out assignment
		buildSrc("    lts_table_with_long_name = VALUE #( ( component1 = lv_any_id");
		buildSrc("*                                          comp_2 = 'commented out' ");
		buildSrc("                                          comp_3 = 'A' )");
		buildSrc("");
		buildSrc("                                    ( component1 = lv_other_id");
		buildSrc("                                          comp_3 = 'B'");
		buildSrc("                                          long_component = cl_any_factory=>get( )->get_utility_with_long_name( )->get_any_value( ) ) ).");

		buildExp("    lts_table_with_long_name = VALUE #(");
		buildExp("        ( component1     = lv_any_id");
		buildExp("*          comp_2         = 'commented out' ");
		buildExp("          comp_3         = 'A' )");
		buildExp("");
		buildExp("        ( component1     = lv_other_id");
		buildExp("          comp_3         = 'B'");
		buildExp("          long_component = cl_any_factory=>get( )->get_utility_with_long_name( )->get_any_value( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFunctionalCallWithSingleExpression() {
		buildSrc("    any_method(");
		buildSrc("      42 ).");
		buildSrc("    any_method(");
		buildSrc("      lv_value + 3 * ( 5 + 7 ) ).");
		buildSrc("    any_method(");
		buildSrc("      VALUE #( ) ).");

		buildExp("    any_method( 42 ).");
		buildExp("    any_method( lv_value + 3 * ( 5 + 7 ) ).");
		buildExp("    any_method( VALUE #( ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFunctionalCallWithNestedValue() {
		buildSrc("    mo_any_utility_with_long_name->any_method_long_name(");
		buildSrc("      VALUE #( ( any_component = if_any_interface=>co_any_constant_for_comp");
		buildSrc("              other_component = VALUE #( ( comp = ms_any_struc-any_comp");
		buildSrc("                                       comp_2 = ms_any_struc-any_value )");
		buildSrc("                                           ( comp = ms_any_struc-other_comp");
		buildSrc("                                              component_3 = ms_any_struc-any_value ) ) )");
		buildSrc("               ( any_component = if_any_interface=>co_other_constant_for_comp");
		buildSrc("                   other_component = VALUE #( ( comp = ms_any_struc-any_comp ) ) ) ) ).");

		buildExp("    mo_any_utility_with_long_name->any_method_long_name(");
		buildExp("        VALUE #( ( any_component   = if_any_interface=>co_any_constant_for_comp");
		buildExp("                   other_component = VALUE #( ( comp        = ms_any_struc-any_comp");
		buildExp("                                                comp_2      = ms_any_struc-any_value )");
		buildExp("                                              ( comp        = ms_any_struc-other_comp");
		buildExp("                                                component_3 = ms_any_struc-any_value ) ) )");
		buildExp("                 ( any_component   = if_any_interface=>co_other_constant_for_comp");
		buildExp("                   other_component = VALUE #( ( comp = ms_any_struc-any_comp ) ) ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCallFunctionWithValue() {
		buildSrc("    CALL FUNCTION 'ANY_FUNCTION'");
		buildSrc("      EXPORTING");
		buildSrc("        its_table = VALUE ty_ts_table( ( any_comp = 1");
		buildSrc("                                       other_comp = 'ABC' )");
		buildSrc("                                         ( any_comp = 2");
		buildSrc("                                    third_component = abap_true ) )");
		buildSrc("      IMPORTING");
		buildSrc("        ev_result = lv_result");
		buildSrc("      EXCEPTIONS");
		buildSrc("        any_exception = 1.");

		buildExp("    CALL FUNCTION 'ANY_FUNCTION'");
		buildExp("      EXPORTING  its_table     = VALUE ty_ts_table( ( any_comp        = 1");
		buildExp("                                                      other_comp      = 'ABC' )");
		buildExp("                                                    ( any_comp        = 2");
		buildExp("                                                      third_component = abap_true ) )");
		buildExp("      IMPORTING  ev_result     = lv_result");
		buildExp("      EXCEPTIONS any_exception = 1.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	@Test
	void testRaiseExceptionWithMessage() {
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception");
		buildSrc("      MESSAGE ID sy-msgid");
		buildSrc("      NUMBER sy-msgno");
		buildSrc("      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildSrc("      EXPORTING");
		buildSrc("         any_param = 1");
		buildSrc("        other_param = 'ABC'");
		buildSrc("          previous = exception.");

		buildExp("    RAISE EXCEPTION TYPE cx_any_exception");
		buildExp("      MESSAGE ID sy-msgid");
		buildExp("      NUMBER sy-msgno");
		buildExp("      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildExp("      EXPORTING any_param   = 1");
		buildExp("                other_param = 'ABC'");
		buildExp("                previous    = exception.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExcMessageExportingOnOwnLine() {
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception");
		buildSrc("      MESSAGE ID sy-msgid");
		buildSrc("      NUMBER sy-msgno");
		buildSrc("      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildSrc("      EXPORTING");
		buildSrc("         any_param = 1");
		buildSrc("        other_param = 'ABC'");
		buildSrc("          previous = exception.");

		buildExp("    RAISE EXCEPTION TYPE cx_any_exception");
		buildExp("      MESSAGE ID sy-msgid");
		buildExp("      NUMBER sy-msgno");
		buildExp("      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildExp("      EXPORTING");
		buildExp("        any_param   = 1");
		buildExp("        other_param = 'ABC'");
		buildExp("        previous    = exception.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExcMessageAlignedWithSecondWord() {
		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception");
		buildSrc("          MESSAGE ID     sy-msgid");
		buildSrc("                  NUMBER sy-msgno");
		buildSrc("                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 EXPORTING");
		buildSrc("         any_param = 1");
		buildSrc("       other_param = 'ABC'");
		buildSrc("           previous = exception.");

		buildExp("    RAISE EXCEPTION TYPE cx_any_exception");
		buildExp("          MESSAGE ID     sy-msgid");
		buildExp("                  NUMBER sy-msgno");
		buildExp("                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildExp("          EXPORTING any_param   = 1");
		buildExp("                    other_param = 'ABC'");
		buildExp("                    previous    = exception.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseExcMessageAtSecondWordExportingOnOwnLine() {
		rule.configPutProceduralCallKeywordsOnOwnLine.setValue(true);

		buildSrc("    RAISE EXCEPTION TYPE cx_any_exception");
		buildSrc("          MESSAGE ID sy-msgid");
		buildSrc("                  NUMBER sy-msgno");
		buildSrc("                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 EXPORTING");
		buildSrc("         any_param = 1");
		buildSrc("       other_param = 'ABC'");
		buildSrc("           previous = exception.");

		buildExp("    RAISE EXCEPTION TYPE cx_any_exception");
		buildExp("          MESSAGE ID sy-msgid");
		buildExp("                  NUMBER sy-msgno");
		buildExp("                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildExp("          EXPORTING");
		buildExp("            any_param   = 1");
		buildExp("            other_param = 'ABC'");
		buildExp("            previous    = exception.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseShortdumpWithMessage() {
		buildSrc("    RAISE SHORTDUMP TYPE cx_any_exception");
		buildSrc("          MESSAGE e001(FARR_ANY)");
		buildSrc("          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildSrc("  EXPORTING");
		buildSrc("         any_param = 1");
		buildSrc("       other_param = 'ABC'");
		buildSrc("           previous = exception.");

		buildExp("    RAISE SHORTDUMP TYPE cx_any_exception");
		buildExp("          MESSAGE e001(FARR_ANY)");
		buildExp("          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4");
		buildExp("          EXPORTING any_param   = 1");
		buildExp("                    other_param = 'ABC'");
		buildExp("                    previous    = exception.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testComponentMovedBehindOpeningParenthesis() {
		// ensure that in all cases, the first component inside the table row is moved directly behind the opening "(" 
		buildSrc("    lt_result = VALUE #( ( a = 1 )");
		buildSrc("                         (");
		buildSrc("                            b = 2 ) ).");
		buildSrc("");
		buildSrc("    lt_result = VALUE #( (");
		buildSrc("                             a = 1 )");
		buildSrc("                         (");
		buildSrc("                          b = 2 ) ).");
		buildSrc("");
		buildSrc("    lt_result = VALUE #( ( a = 1");
		buildSrc("                           b = 1 )");
		buildSrc("                         (");
		buildSrc("                           a = 2");
		buildSrc("                           b = 2 )");
		buildSrc("                         (");
		buildSrc("");
		buildSrc("                           a = 3");
		buildSrc("                           b = 3 ) ).");
		buildSrc("");
		buildSrc("    lt_result = VALUE #(  (");
		buildSrc("                           a = 1");
		buildSrc("                           b = 1 )");
		buildSrc("                       (");
		buildSrc("");
		buildSrc("                           a = 2");
		buildSrc("                           b = 2 ) ).");

		buildExp("    lt_result = VALUE #( ( a = 1 )");
		buildExp("                         ( b = 2 ) ).");
		buildExp("");
		buildExp("    lt_result = VALUE #( ( a = 1 )");
		buildExp("                         ( b = 2 ) ).");
		buildExp("");
		buildExp("    lt_result = VALUE #( ( a = 1");
		buildExp("                           b = 1 )");
		buildExp("                         ( a = 2");
		buildExp("                           b = 2 )");
		buildExp("                         ( a = 3");
		buildExp("                           b = 3 ) ).");
		buildExp("");
		buildExp("    lt_result = VALUE #( ( a = 1");
		buildExp("                           b = 1 )");
		buildExp("                         ( a = 2");
		buildExp("                           b = 2 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testPseudoCommentKeptAtLineEnd() {
		// ensure that the pseudo comment is kept at line end (and not moved above EXPORTING); 
		// also, ensure that line-end comments after method calls are kept, because they usually don't refer to the parameters 
		
		buildSrc("    CALL FUNCTION 'ANY_FUNCTION' \"#EC CI_USAGE_OK[1234567]");
		buildSrc("      EXPORTING");
		buildSrc("        param1 = val1.");
		buildSrc("    CALL METHOD any_method( \" any comment");
		buildSrc("      EXPORTING");
		buildSrc("        param1 = val1 ).");
		
		buildExp("    CALL FUNCTION 'ANY_FUNCTION' \"#EC CI_USAGE_OK[1234567]");
		buildExp("      EXPORTING param1 = val1.");
		buildExp("    CALL METHOD any_method( \" any comment");
		buildExp("      EXPORTING param1 = val1 ).");
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOneLinerDespiteKeywordOnOwnLine() {
		// expect the following case to be turned into a one-liner, even if keywords such as CHANGING should normally be 
		// put on a line of their own 
		rule.configMaxParamCountBehindFunctionalCall.setValue(3);
		rule.configPutFunctionalCallKeywordsOnOwnLine.setValue(true);

		buildSrc("   lo_any_class->any_method(");
		buildSrc("     CHANGING");
		buildSrc("       iv_param = lv_value ).");
		buildSrc("");
		buildSrc("   lo_any_class->any_method(");
		buildSrc("     CHANGING");
		buildSrc("       iv_param1 = lv_value1");
		buildSrc("       iv_param2 = lv_value2 ).");

		buildExp("   lo_any_class->any_method( CHANGING iv_param = lv_value ).");
		buildExp("");
		buildExp("   lo_any_class->any_method( CHANGING");
		buildExp("                               iv_param1 = lv_value1");
		buildExp("                               iv_param2 = lv_value2 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableRowWithStrucAlignedWithComponents() {
		// ensure that the table row which only consist of the structure LS_LINE is aligned with the component 'COMP' 
		// of the other row, NOT with the value '1', while at the same time, the length of 'LS_LINE' does not cause the 
		// assignment '= 1' to be moved to the right
		
		rule.configAlignAcrossTableRows.setValue(true);
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.NEVER);

		buildSrc("    lt_table = VALUE #( ( ls_line )");
		buildSrc("                        ( comp = 1 ) ).");

		buildExp("    lt_table = VALUE #( ( ls_line )");
		buildExp("                        ( comp = 1 ) ).");

		testRule();
	}

	@Test
	void testMoveChainedTableExpressionBehindCall() {
		// expect the complete(!) chained table expression to be moved behind the call  
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildSrc("                                           lts_any_table ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildSrc("                                           lts_any_table[ 1 ] ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildSrc("                                           lts_any_table[ 1 ][ 2 ] ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildSrc("                                           lts_any_table[ 1 ][ 2 ][ 3 ] ).");

		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ] ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ][ 2 ] ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ][ 2 ][ 3 ] ).");

		testRule();
	}

	@Test
	void testMoveChainedTableExpressionToNextLine() {
		// where line length is exceeded, expect the complete chained table expressions to be moved to the next line   
		rule.configMaxLineLength.setValue(110);

		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ] ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ][ 2 ] ).");
		buildSrc("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ][ 2 ][ 3 ] ).");

		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name( lts_any_table[ 1 ] ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildExp("                                               lts_any_table[ 1 ][ 2 ] ).");
		buildExp("    DATA(lv_some_value_with_a_long_name) = lo_any_instance->any_method_with_a_long_name(");
		buildExp("                                               lts_any_table[ 1 ][ 2 ][ 3 ] ).");

		testRule();
	}

	@Test
	void testCondenseChainedTableExpression() {
		// expect the chained table expressions to be condensed and moved behind the call

		buildSrc("    any_method(");
		buildSrc("        lt_any_table[");
		buildSrc("            1 ] ).");
		buildSrc("    any_method(");
		buildSrc("        lt_any_table[");
		buildSrc("            1 ][");
		buildSrc("            2 ] ).");
		buildSrc("    any_method(");
		buildSrc("        lt_any_table[");
		buildSrc("            1 ][");
		buildSrc("            2 ][");
		buildSrc("            3 ] ).");
		buildSrc("    any_method(");
		buildSrc("        par1 = lt_any_table[");
		buildSrc("                  1 ]");
		buildSrc("        param2 = lt_any_table[");
		buildSrc("                    1 ][");
		buildSrc("                    2 ]");
		buildSrc("        parameter3 = lt_any_table[");
		buildSrc("                        1 ][");
		buildSrc("                        2 ][");
		buildSrc("                        3 ] ).");

		buildExp("    any_method( lt_any_table[ 1 ] ).");
		buildExp("    any_method( lt_any_table[ 1 ][ 2 ] ).");
		buildExp("    any_method( lt_any_table[ 1 ][ 2 ][ 3 ] ).");
		buildExp("    any_method( par1       = lt_any_table[ 1 ]");
		buildExp("                param2     = lt_any_table[ 1 ][ 2 ]");
		buildExp("                parameter3 = lt_any_table[ 1 ][ 2 ][ 3 ] ).");

		testRule();
	}

	@Test
	void testVariousChainedTableExpressions() {
		// expect chaining of table expressions with -comp and ->attribute to be identified as a single Term
		
		buildSrc("    any_method(");
		buildSrc("        par1 = lt_any_table[ 1 ]-comp");
		buildSrc("        param2 = lt_any_table[ 1 ]-comp[ 2 ]");
		buildSrc("        parameter3 = lt_any_table[ 1 ]-comp[ 2 ]->mt_attr[ 3 ] ).");

		buildExp("    any_method( par1       = lt_any_table[ 1 ]-comp");
		buildExp("                param2     = lt_any_table[ 1 ]-comp[ 2 ]");
		buildExp("                parameter3 = lt_any_table[ 1 ]-comp[ 2 ]->mt_attr[ 3 ] ).");

		testRule();
	}

	@Test
	void testNonTableRowsSplitToMultiLine() {
		// ensure that the option 'Table rows: Keep multiple components on single line' does NOT prevent splitting 
		// constructor calls with NEW, and VALUE constructors for structures 

		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);

		buildSrc("    DATA(lo_any) = NEW cl_any( iv_param1 = 1 iv_param2 = 2 ).");
		buildSrc("    DATA(ls_any) = NEW ty_s_any( comp1 = 1 comp2 = 2 comp3 = 3 ).");
		buildSrc("    any_method( iv_any_param = 1 iv_other_param = 2 ).");

		buildExp("    DATA(lo_any) = NEW cl_any( iv_param1 = 1");
		buildExp("                               iv_param2 = 2 ).");
		buildExp("    DATA(ls_any) = NEW ty_s_any( comp1 = 1");
		buildExp("                                 comp2 = 2");
		buildExp("                                 comp3 = 3 ).");
		buildExp("    any_method( iv_any_param   = 1");
		buildExp("                iv_other_param = 2 ).");

		testRule();
	}

	@Test
	void testReceiveResults() {
		buildSrc("    RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION' KEEPING TASK");
		buildSrc("       IMPORTING");
		buildSrc("  et_any = lt_any");
		buildSrc("    et_other = lt_other");
		buildSrc("       EXCEPTIONS");
		buildSrc("         communication_failure = 1 MESSAGE lv_message");
		buildSrc("       system_failure = 2 MESSAGE lv_message");
		buildSrc("         OTHERS = 3.");
		buildSrc("");
		buildSrc("    RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION' IMPORTING  et_any = lt_any et_other = lt_other");
		buildSrc("       EXCEPTIONS any_exception = 1 communication_failure = 2 system_failure = 3 OTHERS = 4.");

		buildExp("    RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION' KEEPING TASK");
		buildExp("      IMPORTING  et_any                = lt_any");
		buildExp("                 et_other              = lt_other");
		buildExp("      EXCEPTIONS communication_failure = 1 MESSAGE lv_message");
		buildExp("                 system_failure        = 2 MESSAGE lv_message");
		buildExp("                 OTHERS                = 3.");
		buildExp("");
		buildExp("    RECEIVE RESULTS FROM FUNCTION 'ANY_FUNCTION'");
		buildExp("      IMPORTING  et_any                = lt_any");
		buildExp("                 et_other              = lt_other");
		buildExp("      EXCEPTIONS any_exception         = 1");
		buildExp("                 communication_failure = 2");
		buildExp("                 system_failure        = 3");
		buildExp("                 OTHERS                = 4.");

		testRule();
	}

	@Test
	void testOffsetAndAsteriskKept() {
		// ensure that '(*)' is NOT processed, so no space is introduced
		
		buildSrc("    lv_any+5(*) = lv_other.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommandContinuesAfterPrevious() {
		buildSrc("    IF lv_any_value = 'T1'. any_method( iv_any_param = 1");
		buildSrc("             iv_other_param = 2 ).");
		buildSrc("    ENDIF.");

		buildExp("    IF lv_any_value = 'T1'. any_method( iv_any_param   = 1");
		buildExp("                                        iv_other_param = 2 ).");
		buildExp("    ENDIF.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeepValueWithBaseOnOneLine() {
		rule.configKeepOtherOneLiners.setEnumValue(ComponentsOnSingleLine.IF_BELOW_MAX_LINE_LENGTH);

		buildSrc("    result = VALUE #( BASE result ( id = 1 name = 'abc' ) ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitValueWithBaseToTwoLines() {
		buildSrc("    result = VALUE #( BASE result ( id = 1 name = 'abc' ) ).");

		buildExp("    result = VALUE #( BASE result");
		buildExp("                      ( id = 1 name = 'abc' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testSplitValueWithBaseToThreeLines() {
		rule.configKeepComponentsOnSingleLine.setEnumValue(ComponentsOnSingleLine.NEVER);

		buildSrc("    result = VALUE #( BASE result ( id = 1 name = 'abc' ) ).");

		buildExp("    result = VALUE #( BASE result");
		buildExp("                      ( id   = 1");
		buildExp("                        name = 'abc' ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStructureWithBase() {
		buildSrc("  ls_any = VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ).");
		buildSrc("  ls_any = VALUE #( BASE lt_any[ 1 ] compA = 1 componentB = 'ABC' ).");

		buildExp("  ls_any = VALUE #( BASE ls_any");
		buildExp("                    compA      = 1");
		buildExp("                    componentB = 'ABC' ).");
		buildExp("  ls_any = VALUE #( BASE lt_any[ 1 ]");
		buildExp("                    compA      = 1");
		buildExp("                    componentB = 'ABC' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableRowsOfStructuresWithBase() {
		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ) ) ).");
		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE lt_any[ 1 ] compA = 1 componentB = 'ABC' ) ) ).");
		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1");
		buildSrc("           componentB = 'ABC' ) )");
		buildSrc("           ( VALUE #( BASE lt_any[ 1 ] compA = 2");
		buildSrc("           componentB = 'DEF' ) ) ).");

		buildExp("  lt_any = VALUE #( ( VALUE #( BASE ls_any");
		buildExp("                               compA      = 1");
		buildExp("                               componentB = 'ABC' ) ) ).");
		buildExp("  lt_any = VALUE #( ( VALUE #( BASE lt_any[ 1 ]");
		buildExp("                               compA      = 1");
		buildExp("                               componentB = 'ABC' ) ) ).");
		buildExp("  lt_any = VALUE #( ( VALUE #( BASE ls_any");
		buildExp("                               compA      = 1");
		buildExp("                               componentB = 'ABC' ) )");
		buildExp("                    ( VALUE #( BASE lt_any[ 1 ]");
		buildExp("                               compA      = 2");
		buildExp("                               componentB = 'DEF' ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableRowsOfOneLinerStrucsWithBase() {
		rule.configKeepOtherOneLiners.setEnumValue(ComponentsOnSingleLine.ALWAYS);

		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ) ) ).");
		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE lt_any[ 1 ] compA = 1 componentB = 'ABC' ) ) ).");
		buildSrc("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ) )");
		buildSrc("           ( VALUE #( BASE lt_any[ 1 ] compA = 2 componentB = 'DEF' ) ) ).");

		buildExp("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ) ) ).");
		buildExp("  lt_any = VALUE #( ( VALUE #( BASE lt_any[ 1 ] compA = 1 componentB = 'ABC' ) ) ).");
		buildExp("  lt_any = VALUE #( ( VALUE #( BASE ls_any compA = 1 componentB = 'ABC' ) )");
		buildExp("                    ( VALUE #( BASE lt_any[ 1 ] compA = 2 componentB = 'DEF' ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableExprKeyAndComponents() {
		buildSrc("    lts_any_table = VALUE #( BASE lts_any_table");
		buildSrc("        \" comment");
		buildSrc("        ( VALUE #( BASE lts_any_table[ KEY any_key any_comp = '1' other_comp = '2' ]");
		buildSrc("                   other_comp = 'A' ) )");
		buildSrc("        \" comment");
		buildSrc("        ( VALUE #( BASE lts_any_table[ KEY any_key any_comp = '3' other_comp = '4' ]");
		buildSrc("                   other_comp = 'B' \" comment");
		buildSrc("                   third_comp = 'C' ) ) ).");

		buildExp("    lts_any_table = VALUE #( BASE lts_any_table");
		buildExp("                             \" comment");
		buildExp("                             ( VALUE #( BASE lts_any_table[ KEY any_key");
		buildExp("                                                            any_comp   = '1'");
		buildExp("                                                            other_comp = '2' ]");
		buildExp("                                        other_comp = 'A' ) )");
		buildExp("                             \" comment");
		buildExp("                             ( VALUE #( BASE lts_any_table[ KEY any_key");
		buildExp("                                                            any_comp   = '3'");
		buildExp("                                                            other_comp = '4' ]");
		buildExp("                                        other_comp = 'B' \" comment");
		buildExp("                                        third_comp = 'C' ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testClosingParenthesesNotMoved() {
		// ensure that neither the closing parentheses nor the rest of the Command are moved to the right
		// (this case can happen if ClosingBracketsPositionRule is deactivated)
		buildSrc("    READ ENTITIES OF C_Any");
		buildSrc("      ENTITY AnyEntity");
		buildSrc("      ALL FIELDS");
		buildSrc("      WITH VALUE #( ( AnyComponent = '1'");
		buildSrc("        %is_draft = if_abap_behv=>mk-on");
		buildSrc("      ) )");
		buildSrc("      RESULT DATA(results_act)");
		buildSrc("      FAILED DATA(failures_act)");
		buildSrc("      REPORTED DATA(reported_act).");

		buildExp("    READ ENTITIES OF C_Any");
		buildExp("      ENTITY AnyEntity");
		buildExp("      ALL FIELDS");
		buildExp("      WITH VALUE #( ( AnyComponent = '1'");
		buildExp("                      %is_draft    = if_abap_behv=>mk-on");
		buildExp("      ) )");
		buildExp("      RESULT DATA(results_act)");
		buildExp("      FAILED DATA(failures_act)");
		buildExp("      REPORTED DATA(reported_act).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignLoopAtGroupBy() {
		buildSrc("    LOOP AT lt_table ASSIGNING <fs>");
		buildSrc("         GROUP BY ( key1 = <fs>-any_component key2 = get_value( <fs>-other_component )");
		buildSrc("                     indx = GROUP INDEX count = GROUP SIZE )");
		buildSrc("         ASSIGNING FIELD-SYMBOL(<group>).");
		buildSrc("      cl_demo_output=>write( |{ <group>-indx } { <group>-key1 } { <group>-key2 } { <group>-count }| ).");
		buildSrc("    ENDLOOP.");

		buildExp("    LOOP AT lt_table ASSIGNING <fs>");
		buildExp("         GROUP BY ( key1  = <fs>-any_component");
		buildExp("                    key2  = get_value( <fs>-other_component )");
		buildExp("                    indx  = GROUP INDEX");
		buildExp("                    count = GROUP SIZE )");
		buildExp("         ASSIGNING FIELD-SYMBOL(<group>).");
		buildExp("      cl_demo_output=>write( |{ <group>-indx } { <group>-key1 } { <group>-key2 } { <group>-count }| ).");
		buildExp("    ENDLOOP.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithDataSourceParameters() {
		buildSrc("    SELECT");
		buildSrc("      FROM P_AnyView( P_AnyYear   = @lv_year,");
		buildSrc("* P_AnyQuarterFrom = '1'");
		buildSrc("                                             P_AnyPeriodFrom = '001',P_AnyYearTo     = '2020', \" comment");
		buildSrc("* P_AnyQuarterTo = '2'");
		buildSrc("                                p_AnyPeriodTo   =   '004' )");
		buildSrc("      FIELDS AnyField");
		buildSrc("      INTO TABLE @lt_any_table.");

		buildExp("    SELECT");
		buildExp("      FROM P_AnyView( P_AnyYear       = @lv_year,");
		buildExp("*                      P_AnyQuarterFrom = '1'");
		buildExp("                      P_AnyPeriodFrom = '001',");
		buildExp("                      P_AnyYearTo     = '2020', \" comment");
		buildExp("*                      P_AnyQuarterTo  = '2'");
		buildExp("                      p_AnyPeriodTo   = '004' )");
		buildExp("      FIELDS AnyField");
		buildExp("      INTO TABLE @lt_any_table.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFunctionalCallInHostExpression() {
		buildSrc("    SELECT any_col, other_col");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE any_col = @lv_any_value");
		buildSrc("        AND other_col = @( NEW cl_any_class( )->any_method( EXPORTING iv_any_param = 'literal'");
		buildSrc("                                                        iv_other_param = lv_other_value iv_third_param = 42 ) )");
		buildSrc("      INTO TABLE @FINAL(lt_any_result).");

		buildExp("    SELECT any_col, other_col");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE any_col = @lv_any_value");
		buildExp("        AND other_col = @( NEW cl_any_class( )->any_method( EXPORTING iv_any_param   = 'literal'");
		buildExp("                                                                      iv_other_param = lv_other_value");
		buildExp("                                                                      iv_third_param = 42 ) )");
		buildExp("      INTO TABLE @FINAL(lt_any_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFieldListWithFunction() {
		buildSrc("    SELECT FROM any_dtab");
		buildSrc("           FIELDS id,");
		buildSrc("                  currency_conversion(");
		buildSrc("  amount = amount,source_currency =    currency,");
		buildSrc("                     target_currency = @lv_currency,");
		buildSrc("*                target_currency   =  @lv_currency2,");
		buildSrc("                    exchange_rate_date    = @lv_date,");
		buildSrc("                    round = 'X', on_error =");
		buildSrc("                      @sql_currency_conversion=>c_on_error-fail ) AS amount,");
		buildSrc("                    @currency  AS currency");
		buildSrc("            INTO TABLE @DATA(lt_result).");

		buildExp("    SELECT FROM any_dtab");
		buildExp("           FIELDS id,");
		buildExp("                  currency_conversion( amount             = amount,");
		buildExp("                                       source_currency    = currency,");
		buildExp("                                       target_currency    = @lv_currency,");
		buildExp("*                                       target_currency    = @lv_currency2,");
		buildExp("                                       exchange_rate_date = @lv_date,");
		buildExp("                                       round              = 'X',");
		buildExp("                                       on_error           = @sql_currency_conversion=>c_on_error-fail ) AS amount,");
		buildExp("                    @currency  AS currency");
		buildExp("            INTO TABLE @DATA(lt_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFieldListWithNestedFunctions() {
		buildSrc("    SELECT SINGLE");
		buildSrc("      FROM any_dtab");
		buildSrc("      FIELDS timestamp1,");
		buildSrc("            tstmp_is_valid( timestamp1 ) AS valid1,");
		buildSrc("            tstmp_seconds_between( tstmp1 = tstmp_current_utctimestamp( ),");
		buildSrc("                          tstmp2   = tstmp_add_seconds( tstmp  =   timestamp1,");
		buildSrc("                                       seconds = CAST( num1 AS DEC( 15,0 ) ), on_error = @sql_tstmp_add_seconds=>fail ),");
		buildSrc("                        on_error   =    @sql_tstmp_seconds_between=>fail ) AS difference");
		buildSrc("      INTO @DATA(lv_result).");

		buildExp("    SELECT SINGLE");
		buildExp("      FROM any_dtab");
		buildExp("      FIELDS timestamp1,");
		buildExp("            tstmp_is_valid( timestamp1 ) AS valid1,");
		buildExp("            tstmp_seconds_between( tstmp1   = tstmp_current_utctimestamp( ),");
		buildExp("                                   tstmp2   = tstmp_add_seconds( tstmp    = timestamp1,");
		buildExp("                                                                 seconds  = CAST( num1 AS DEC( 15,0 ) ),");
		buildExp("                                                                 on_error = @sql_tstmp_add_seconds=>fail ),");
		buildExp("                                   on_error = @sql_tstmp_seconds_between=>fail ) AS difference");
		buildExp("      INTO @DATA(lv_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithParametersAndNestedFunction() {
		rule.configMaxLineLength.setValue(80);

		buildSrc("    SELECT FROM any_dtab(");
		buildSrc("              p_ts_from = @( CONV timestamp( cl_abap_tstmp=>subtractsecs(");
		buildSrc("                                  tstmp = ts");
		buildSrc("                                     secs  =   3600 ) ) ),");
		buildSrc("                  p_ts_to =  @( CONV timestamp(  cl_abap_tstmp=>add(");
		buildSrc("                                     tstmp = ts secs  = 3600 ) ) ) )");
		buildSrc("           FIELDS any_field");
		buildSrc("           INTO TABLE @DATA(lt_result).");

		buildExp("    SELECT FROM any_dtab(");
		buildExp("                    p_ts_from = @( CONV timestamp( cl_abap_tstmp=>subtractsecs(");
		buildExp("                                                       tstmp = ts");
		buildExp("                                                       secs  = 3600 ) ) ),");
		buildExp("                    p_ts_to   = @( CONV timestamp(  cl_abap_tstmp=>add(");
		buildExp("                                                        tstmp = ts");
		buildExp("                                                        secs  = 3600 ) ) ) )");
		buildExp("           FIELDS any_field");
		buildExp("           INTO TABLE @DATA(lt_result).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectWithBuiltInFunctionInWhere() {
		buildSrc("    SELECT any_field");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE any_field = 'E'");
		buildSrc("        AND like_regexpr( pcre = @regex,");
		buildSrc("                            value =   text,");
		buildSrc("                       case_sensitive  = ' ' ) = 1");
		buildSrc("      ORDER BY any_field");
		buildSrc("      INTO TABLE @DATA(lt_result)");
		buildSrc("      UP TO 100 ROWS.");

		buildExp("    SELECT any_field");
		buildExp("      FROM any_dtab");
		buildExp("      WHERE any_field = 'E'");
		buildExp("        AND like_regexpr( pcre           = @regex,");
		buildExp("                          value          = text,");
		buildExp("                          case_sensitive = ' ' ) = 1");
		buildExp("      ORDER BY any_field");
		buildExp("      INTO TABLE @DATA(lt_result)");
		buildExp("      UP TO 100 ROWS.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectFirstValueOverUnchanged() {
		buildSrc("    SELECT id, col1, col2, col3,");
		buildSrc("           FIRST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3 )");
		buildSrc("                       AS first_value,");
		buildSrc("           LAST_VALUE( col2 ) OVER( PARTITION BY col1 ORDER BY col3");
		buildSrc("                       ROWS BETWEEN UNBOUNDED PRECEDING");
		buildSrc("                         AND UNBOUNDED FOLLOWING )");
		buildSrc("                       AS last_value");
		buildSrc("      FROM demo_update");
		buildSrc("      INTO TABLE @DATA(lt_result).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectHierarchyUnchanged() {
		buildSrc("    SELECT agg~*");
		buildSrc("           FROM HIERARCHY_DESCENDANTS_AGGREGATE(");
		buildSrc("                  SOURCE HIERARCHY(");
		buildSrc("                           SOURCE any_source");
		buildSrc("                            CHILD TO PARENT ASSOCIATION _relat");
		buildSrc("                             START WHERE id = 'A'");
		buildSrc("                              SIBLINGS ORDER BY id )");
		buildSrc("                    MEASURES MIN( num ) AS min,");
		buildSrc("                              MAX( num ) AS max,");
		buildSrc("                               SUM( num ) AS sum,");
		buildSrc("                                COUNT( * ) AS cnt");
		buildSrc("                  WHERE hierarchy_level <= @level ) AS agg");
		buildSrc("           INTO TABLE @DATA(lt_any).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSelectSumCaseUnchanged() {
		buildSrc("    SELECT SUM( CASE WHEN     any_col   = @lv_any_value");
		buildSrc("                          AND other_col = @lv_other_value");
		buildSrc("                     THEN 1");
		buildSrc("                     ELSE 0");
		buildSrc("                END ) AS any_col");
		buildSrc("      FROM any_dtab");
		buildSrc("      WHERE third_col = @lv_third_value");
		buildSrc("      INTO @DATA(ls_any_struc).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentAfterOpeningParens() {
		// ensure that the comment remains inside the inner parenthesis
		buildSrc("lt_table = VALUE #(   ( \" comment");
		buildSrc("                   id  =  1 ) ).");

		buildExp("lt_table = VALUE #( ( \" comment");
		buildExp("                      id = 1 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseEventExporting() {
		buildSrc("RAISE EVENT message");
		buildSrc("EXPORTING");
		buildSrc("iv_id = 'MSG_ID'");
		buildSrc("iv_type = 'E'");
		buildSrc("iv_number = '001'.");

		buildExp("RAISE EVENT message");
		buildExp("  EXPORTING iv_id     = 'MSG_ID'");
		buildExp("            iv_type   = 'E'");
		buildExp("            iv_number = '001'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testRaiseEventContinueBehindCall() {
		rule.configMaxParamCountBehindProceduralCall.setValue(3);

		buildSrc("RAISE EVENT message");
		buildSrc("EXPORTING");
		buildSrc("iv_id = 'MSG_ID'");
		buildSrc("iv_type = 'E'");
		buildSrc("iv_number = '001'.");

		buildExp("RAISE EVENT message EXPORTING iv_id     = 'MSG_ID'");
		buildExp("                              iv_type   = 'E'");
		buildExp("                              iv_number = '001'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignImportAndExport() {
		buildSrc("    IMPORT");
		buildSrc("      any_param       =    lv_any_param");
		buildSrc("           FROM MEMORY ID 'ANY_ID'.");
		buildSrc("");
		buildSrc("    IMPORT any_param   =   lv_any_param FROM MEMORY ID 'ANY_ID'.");
		buildSrc("");
		buildSrc("    IMPORT any_param             = lv_any_param  other_param =    lv_other_param");
		buildSrc("           third_parameter   = lv_third_parameter FROM MEMORY ID 'ANY_ID'.");
		buildSrc("");
		buildSrc("    IMPORT any_param  TO lv_any_param");
		buildSrc("       other_param TO  lv_other_param");
		buildSrc("             third_parameter TO lv_third_parameter");
		buildSrc("           FROM MEMORY ID 'ANY_ID'.");
		buildSrc("");
		buildSrc("    EXPORT");
		buildSrc("      any_param = lv_any_param");
		buildSrc("        other_param = lv_other_param");
		buildSrc("     third_parameter = lv_third_parameter TO INTERNAL TABLE itab.");
		buildSrc("");
		buildSrc("    EXPORT any_param FROM  lv_any_param");
		buildSrc("           other_param  FROM lv_other_param");
		buildSrc("           third_parameter FROM lv_third_parameter");
		buildSrc("        TO SHARED MEMORY dtab(ar) ID any_id.");

		buildExp("    IMPORT any_param = lv_any_param");
		buildExp("           FROM MEMORY ID 'ANY_ID'.");
		buildExp("");
		buildExp("    IMPORT any_param = lv_any_param FROM MEMORY ID 'ANY_ID'.");
		buildExp("");
		buildExp("    IMPORT any_param       = lv_any_param");
		buildExp("           other_param     = lv_other_param");
		buildExp("           third_parameter = lv_third_parameter FROM MEMORY ID 'ANY_ID'.");
		buildExp("");
		buildExp("    IMPORT any_param       TO lv_any_param");
		buildExp("           other_param     TO lv_other_param");
		buildExp("           third_parameter TO lv_third_parameter");
		buildExp("           FROM MEMORY ID 'ANY_ID'.");
		buildExp("");
		buildExp("    EXPORT any_param       = lv_any_param");
		buildExp("           other_param     = lv_other_param");
		buildExp("           third_parameter = lv_third_parameter TO INTERNAL TABLE itab.");
		buildExp("");
		buildExp("    EXPORT any_param       FROM lv_any_param");
		buildExp("           other_param     FROM lv_other_param");
		buildExp("           third_parameter FROM lv_third_parameter");
		buildExp("        TO SHARED MEMORY dtab(ar) ID any_id.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAlignImportExport() {
		buildSrc("    IMPORT lv_any_param lv_other_param");
		buildSrc("           lv_third_parameter FROM MEMORY ID 'ANY_ID'.");
		buildSrc("");
		buildSrc("    EXPORT lv_any_param lv_other_param");
		buildSrc("           lv_third_parameter TO MEMORY ID 'ANY_ID' COMPRESSION ON.");
		buildSrc("");
		buildSrc("    IMPORT DIRECTORY INTO itab");
		buildSrc("           FROM DATABASE dbtab(ar) ID id.");
		buildSrc("");
		buildSrc("    IMPORT (ptab) FROM MEMORY ID 'ANY_ID'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
