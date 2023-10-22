package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignCondExpressionsTest extends RuleTestBase {
	private AlignCondExpressionsRule rule;
	
	AlignCondExpressionsTest() {
		super(RuleID.ALIGN_COND_EXPRESSIONS);
		rule = (AlignCondExpressionsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxLineLength.setValue(120);

		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.KEEP);
		rule.configSimpleStyle.setEnumValue(CondSimpleStyle.VERTICAL_LAYOUT);
		rule.configTabularStyle.setEnumValue(CondTabularStyle.CREATE);
		
		rule.configGapAfterElse.setValue(true);
		rule.configThenOnWhenLine.setValue(true);
		rule.configContinueAfterElse.setValue(false);
		rule.configValueIndent.setEnumValue(CondValueIndent.ADD_2);
	}

	@Test
	void testKeepOneLinersAsIs() {
		buildSrc("    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");

		buildExp("    ev_value = COND #( WHEN iv_value IS SUPPLIED THEN iv_value ELSE gc_default_value ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateOneLiners() {
		buildSrc("    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");

		buildExp("    ev_value = COND #( WHEN iv_value IS SUPPLIED THEN iv_value ELSE gc_default_value ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitOneLiners() {
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.SPLIT);

		buildSrc("    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");

		buildExp("    ev_value = COND #( WHEN iv_value IS SUPPLIED");
		buildExp("                       THEN iv_value");
		buildExp("                       ELSE gc_default_value ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignSimpleCases() {
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN");
		buildSrc("                          io_object->get_sub_object( )->get_number( )");
		buildSrc("                        ELSE");
		buildSrc("                           if_object_boundaries=>co_maximum_number ).");

		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");
		buildExp("");
		buildExp("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number");
		buildExp("                        THEN io_object->get_sub_object( )->get_number( )");
		buildExp("                        ELSE if_object_boundaries=>co_maximum_number ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignSimpleCasesLikeComplex() {
		rule.configSimpleStyle.setEnumValue(CondSimpleStyle.LIKE_COMPLEX);

		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN");
		buildSrc("                          io_object->get_sub_object( )->get_number( )");
		buildSrc("                        ELSE");
		buildSrc("                           if_object_boundaries=>co_maximum_number ).");

		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true THEN");
		buildExp("                        -1");
		buildExp("                      ELSE");
		buildExp("                        1 ).");
		buildExp("");
		buildExp("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN");
		buildExp("                          io_object->get_sub_object( )->get_number( )");
		buildExp("                        ELSE");
		buildExp("                          if_object_boundaries=>co_maximum_number ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateTabularCases() {
		buildSrc("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildSrc("                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildSrc("                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildSrc("                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildSrc("");
		buildSrc("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0");
		buildSrc("                                      THEN   ls_amounts-waers");
		buildSrc("                                    WHEN ls_amounts-betrh <> 0");
		buildSrc("                                       THEN  ls_amounts-hwaer");
		buildSrc("                                    WHEN ls_amounts-betr2 <> 0");
		buildSrc("                                      THEN   ls_amounts-hwae2");
		buildSrc("                                     WHEN ls_amounts-betr3 <> 0");
		buildSrc("                                        THEN ls_amounts-hwae3");
		buildSrc("                                    ELSE    gc_default_currency_code ).");

		buildExp("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildExp("                                     WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildExp("                                     WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildExp("                                     WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildExp("");
		buildExp("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 THEN ls_amounts-waers");
		buildExp("                                    WHEN ls_amounts-betrh <> 0 THEN ls_amounts-hwaer");
		buildExp("                                    WHEN ls_amounts-betr2 <> 0 THEN ls_amounts-hwae2");
		buildExp("                                    WHEN ls_amounts-betr3 <> 0 THEN ls_amounts-hwae3");
		buildExp("                                    ELSE                            gc_default_currency_code ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepTabularCasesAsIs() {
		rule.configTabularStyle.setEnumValue(CondTabularStyle.KEEP);

		buildSrc("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildSrc("                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildSrc("                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildSrc("                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildSrc("");
		buildSrc("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0");
		buildSrc("                                      THEN   ls_amounts-waers");
		buildSrc("                                    WHEN ls_amounts-betrh <> 0");
		buildSrc("                                       THEN  ls_amounts-hwaer");
		buildSrc("                                    WHEN ls_amounts-betr2 <> 0");
		buildSrc("                                      THEN   ls_amounts-hwae2");
		buildSrc("                                     WHEN ls_amounts-betr3 <> 0");
		buildSrc("                                        THEN ls_amounts-hwae3");
		buildSrc("                                    ELSE    gc_default_currency_code ).");

		buildExp("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildExp("                                     WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildExp("                                     WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildExp("                                     WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildExp("");
		buildExp("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 THEN");
		buildExp("                                      ls_amounts-waers");
		buildExp("                                    WHEN ls_amounts-betrh <> 0 THEN");
		buildExp("                                      ls_amounts-hwaer");
		buildExp("                                    WHEN ls_amounts-betr2 <> 0 THEN");
		buildExp("                                      ls_amounts-hwae2");
		buildExp("                                    WHEN ls_amounts-betr3 <> 0 THEN");
		buildExp("                                      ls_amounts-hwae3");
		buildExp("                                    ELSE");
		buildExp("                                      gc_default_currency_code ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSplitTabularCases() {
		rule.configTabularStyle.setEnumValue(CondTabularStyle.SPLIT);

		buildSrc("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildSrc("                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildSrc("                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildSrc("                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildSrc("");
		buildSrc("    out->write( SWITCH string( sy-index");
		buildSrc("                               WHEN 1 THEN 'one'");
		buildSrc("                               WHEN 2 THEN 'two'");
		buildSrc("                               WHEN 3 THEN 'three' ELSE THROW cx_overflow( ) ) ).");

		buildExp("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betrw");
		buildExp("                                     WHEN ls_amounts-betrh IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betrh");
		buildExp("                                     WHEN ls_amounts-betr2 IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betr2");
		buildExp("                                     WHEN ls_amounts-betr3 IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betr3 ).");
		buildExp("");
		buildExp("    out->write( SWITCH string( sy-index");
		buildExp("                               WHEN 1 THEN");
		buildExp("                                 'one'");
		buildExp("                               WHEN 2 THEN");
		buildExp("                                 'two'");
		buildExp("                               WHEN 3 THEN");
		buildExp("                                 'three'");
		buildExp("                               ELSE");
		buildExp("                                 THROW cx_overflow( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTabularCasesContinueAfterElse() {
		rule.configGapAfterElse.setValue(false);

		buildSrc("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0");
		buildSrc("                                      THEN   ls_amounts-waers");
		buildSrc("                                    WHEN ls_amounts-betrh <> 0");
		buildSrc("                                       THEN  ls_amounts-hwaer");
		buildSrc("                                    WHEN ls_amounts-betr2 <> 0");
		buildSrc("                                      THEN   ls_amounts-hwae2");
		buildSrc("                                     WHEN ls_amounts-betr3 <> 0");
		buildSrc("                                        THEN ls_amounts-hwae3");
		buildSrc("                                    ELSE    gc_default_currency_code ).");
		buildSrc("");
		buildSrc("    out->write( SWITCH string( sy-index");
		buildSrc("                               WHEN 1 THEN 'one'");
		buildSrc("                               WHEN 2 THEN 'two'");
		buildSrc("                               WHEN 3 THEN 'three' ELSE THROW cx_overflow( ) ) ).");

		buildExp("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 THEN ls_amounts-waers");
		buildExp("                                    WHEN ls_amounts-betrh <> 0 THEN ls_amounts-hwaer");
		buildExp("                                    WHEN ls_amounts-betr2 <> 0 THEN ls_amounts-hwae2");
		buildExp("                                    WHEN ls_amounts-betr3 <> 0 THEN ls_amounts-hwae3");
		buildExp("                                    ELSE gc_default_currency_code ).");
		buildExp("");
		buildExp("    out->write( SWITCH string( sy-index");
		buildExp("                               WHEN 1 THEN 'one'");
		buildExp("                               WHEN 2 THEN 'two'");
		buildExp("                               WHEN 3 THEN 'three'");
		buildExp("                               ELSE THROW cx_overflow( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCaseThenOnWhenLine() {
		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                                 |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                                 |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                                 |High Noon|");
		buildExp("                               ELSE");
		buildExp("                                 THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCaseThenOnNewLine() {
		rule.configThenOnWhenLine.setValue(false);

		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t");
		buildExp("                                 THEN |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildExp("                                 THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t");
		buildExp("                                 THEN |High Noon|");
		buildExp("                               ELSE");
		buildExp("                                 THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCasesContinueAfterElse() {
		rule.configSimpleStyle.setEnumValue(CondSimpleStyle.LIKE_COMPLEX);
		rule.configContinueAfterElse.setValue(true);

		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN");
		buildSrc("                          io_object->get_sub_object( )->get_number( )");
		buildSrc("                        ELSE");
		buildSrc("                           if_object_boundaries=>co_maximum_number ).");
		buildSrc("");
		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true THEN");
		buildExp("                        -1");
		buildExp("                      ELSE 1 ).");
		buildExp("");
		buildExp("    ev_number = COND #( WHEN io_object->get_sub_object( )->get_number( ) <= if_object_boundaries=>co_maximum_number THEN");
		buildExp("                          io_object->get_sub_object( )->get_number( )");
		buildExp("                        ELSE if_object_boundaries=>co_maximum_number ).");
		buildExp("");
		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                                 |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                                 |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                                 |High Noon|");
		buildExp("                               ELSE THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCaseIndentBelowWhen() {
		rule.configValueIndent.setEnumValue(CondValueIndent.ADD_0);

		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                               |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                               |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                               |High Noon|");
		buildExp("                               ELSE");
		buildExp("                               THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCaseIndentBelowWhenPlus5() {
		rule.configValueIndent.setEnumValue(CondValueIndent.ADD_5);

		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                                    |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                                    |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                                    |High Noon|");
		buildExp("                               ELSE");
		buildExp("                                    THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testComplexCaseIndentBelowWhenPlus7() {
		rule.configValueIndent.setEnumValue(CondValueIndent.ADD_7);

		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                                      |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                                      |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                                      |High Noon|");
		buildExp("                               ELSE");
		buildExp("                                      THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	@Test
	void testLineLengthExceededForOneLiner() {
		rule.configMaxLineLength.setValue(80);

		buildSrc("    ev_value = COND #( WHEN iv_value IS SUPPLIED   THEN   iv_value   ELSE   gc_default_value ).");

		buildExp("    ev_value = COND #( WHEN iv_value IS SUPPLIED");
		buildExp("                       THEN iv_value");
		buildExp("                       ELSE gc_default_value ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLineLengthExceededForTabular() {
		rule.configMaxLineLength.setValue(80);

		buildSrc("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN ls_amounts-betrw");
		buildSrc("                                      WHEN ls_amounts-betrh IS NOT INITIAL THEN ls_amounts-betrh");
		buildSrc("                                   WHEN ls_amounts-betr2 IS NOT INITIAL THEN ls_amounts-betr2");
		buildSrc("                                         WHEN ls_amounts-betr3 IS NOT INITIAL THEN ls_amounts-betr3 ).");
		buildSrc("");
		buildSrc("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0");
		buildSrc("                                      THEN   ls_amounts-waers");
		buildSrc("                                    WHEN ls_amounts-betrh <> 0");
		buildSrc("                                       THEN  ls_amounts-hwaer");
		buildSrc("                                    WHEN ls_amounts-betr2 <> 0");
		buildSrc("                                      THEN   ls_amounts-hwae2");
		buildSrc("                                     WHEN ls_amounts-betr3 <> 0");
		buildSrc("                                        THEN ls_amounts-hwae3");
		buildSrc("                                    ELSE    gc_default_currency_code ).");

		buildExp("    ev_any_non_zero_amount = COND #( WHEN ls_amounts-betrw IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betrw");
		buildExp("                                     WHEN ls_amounts-betrh IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betrh");
		buildExp("                                     WHEN ls_amounts-betr2 IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betr2");
		buildExp("                                     WHEN ls_amounts-betr3 IS NOT INITIAL THEN");
		buildExp("                                       ls_amounts-betr3 ).");
		buildExp("");
		buildExp("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 THEN");
		buildExp("                                      ls_amounts-waers");
		buildExp("                                    WHEN ls_amounts-betrh <> 0 THEN");
		buildExp("                                      ls_amounts-hwaer");
		buildExp("                                    WHEN ls_amounts-betr2 <> 0 THEN");
		buildExp("                                      ls_amounts-hwae2");
		buildExp("                                    WHEN ls_amounts-betr3 <> 0 THEN");
		buildExp("                                      ls_amounts-hwae3");
		buildExp("                                    ELSE");
		buildExp("                                      gc_default_currency_code ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedCase() {
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.CREATE);

		buildSrc("    ev_curr = COND #( WHEN ls_amt-betrw <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true");
		buildSrc("       THEN 'WAERS' ELSE ls_amt-waers )");
		buildSrc("       WHEN ls_amt-betrh <> 0");
		buildSrc("       THEN  COND #( WHEN lv_fld = abap_true THEN 'HWAER'");
		buildSrc("       ELSE ls_amt-hwaer )");
		buildSrc("       WHEN ls_amt-betr2 <> 0");
		buildSrc("       THEN   COND #( WHEN lv_fld = abap_true    THEN 'HWAE2'    ELSE ls_amt-hwae2 )");
		buildSrc("       WHEN ls_amt-betr3 <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true THEN 'HWAE3' ELSE ls_amt-hwae3 )");
		buildSrc("       ELSE COND #( WHEN lv_fld = abap_true THEN '-----' ELSE  gc_default ) ).");

		buildExp("    ev_curr = COND #( WHEN ls_amt-betrw <> 0 THEN COND #( WHEN lv_fld = abap_true THEN 'WAERS' ELSE ls_amt-waers )");
		buildExp("                      WHEN ls_amt-betrh <> 0 THEN COND #( WHEN lv_fld = abap_true THEN 'HWAER' ELSE ls_amt-hwaer )");
		buildExp("                      WHEN ls_amt-betr2 <> 0 THEN COND #( WHEN lv_fld = abap_true THEN 'HWAE2' ELSE ls_amt-hwae2 )");
		buildExp("                      WHEN ls_amt-betr3 <> 0 THEN COND #( WHEN lv_fld = abap_true THEN 'HWAE3' ELSE ls_amt-hwae3 )");
		buildExp("                      ELSE                        COND #( WHEN lv_fld = abap_true THEN '-----' ELSE gc_default ) ).");

		testRule();
	}

	@Test
	void testNestedCaseNoOuterTabular() {
		rule.configMaxLineLength.setValue(110);
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.CREATE);

		buildSrc("    ev_curr = COND #( WHEN ls_amt-betrw <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true");
		buildSrc("       THEN 'WAERS' ELSE ls_amt-waers )");
		buildSrc("       WHEN ls_amt-betrh <> 0");
		buildSrc("       THEN  COND #( WHEN lv_fld = abap_true THEN 'HWAER'");
		buildSrc("       ELSE ls_amt-hwaer )");
		buildSrc("       WHEN ls_amt-betr2 <> 0");
		buildSrc("       THEN   COND #( WHEN lv_fld = abap_true    THEN 'HWAE2'    ELSE ls_amt-hwae2 )");
		buildSrc("       WHEN ls_amt-betr3 <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true THEN 'HWAE3' ELSE ls_amt-hwae3 )");
		buildSrc("       ELSE COND #( WHEN lv_fld = abap_true THEN '-----' ELSE  gc_default ) ).");

		buildExp("    ev_curr = COND #( WHEN ls_amt-betrw <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true THEN 'WAERS' ELSE ls_amt-waers )");
		buildExp("                      WHEN ls_amt-betrh <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true THEN 'HWAER' ELSE ls_amt-hwaer )");
		buildExp("                      WHEN ls_amt-betr2 <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true THEN 'HWAE2' ELSE ls_amt-hwae2 )");
		buildExp("                      WHEN ls_amt-betr3 <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true THEN 'HWAE3' ELSE ls_amt-hwae3 )");
		buildExp("                      ELSE");
		buildExp("                        COND #( WHEN lv_fld = abap_true THEN '-----' ELSE gc_default ) ).");

		testRule();
	}

	@Test
	void testNestedCaseNoInnerOrOuterTabular() {
		rule.configMaxLineLength.setValue(80);
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.CREATE);

		buildSrc("    ev_curr = COND #( WHEN ls_amt-betrw <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true");
		buildSrc("       THEN 'WAERS' ELSE ls_amt-waers )");
		buildSrc("       WHEN ls_amt-betrh <> 0");
		buildSrc("       THEN  COND #( WHEN lv_fld = abap_true THEN 'HWAER'");
		buildSrc("       ELSE ls_amt-hwaer )");
		buildSrc("       WHEN ls_amt-betr2 <> 0");
		buildSrc("       THEN   COND #( WHEN lv_fld = abap_true    THEN 'HWAE2'    ELSE ls_amt-hwae2 )");
		buildSrc("       WHEN ls_amt-betr3 <> 0");
		buildSrc("       THEN COND #( WHEN lv_fld = abap_true THEN 'HWAE3' ELSE ls_amt-hwae3 )");
		buildSrc("       ELSE COND #( WHEN lv_fld = abap_true THEN '-----' ELSE  gc_default ) ).");

		buildExp("    ev_curr = COND #( WHEN ls_amt-betrw <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true");
		buildExp("                                THEN 'WAERS'");
		buildExp("                                ELSE ls_amt-waers )");
		buildExp("                      WHEN ls_amt-betrh <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true");
		buildExp("                                THEN 'HWAER'");
		buildExp("                                ELSE ls_amt-hwaer )");
		buildExp("                      WHEN ls_amt-betr2 <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true");
		buildExp("                                THEN 'HWAE2'");
		buildExp("                                ELSE ls_amt-hwae2 )");
		buildExp("                      WHEN ls_amt-betr3 <> 0 THEN");
		buildExp("                        COND #( WHEN lv_fld = abap_true");
		buildExp("                                THEN 'HWAE3'");
		buildExp("                                ELSE ls_amt-hwae3 )");
		buildExp("                      ELSE");
		buildExp("                        COND #( WHEN lv_fld = abap_true");
		buildExp("                                THEN '-----'");
		buildExp("                                ELSE gc_default ) ).");

		testRule();
	}
	@Test
	void testTabularCaseWithComments() {
		buildSrc("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 \" comment A");
		buildSrc("                                      THEN   ls_amounts-waers");
		buildSrc("                                    \" comment B");
		buildSrc("                                    WHEN ls_amounts-betrh <> 0");
		buildSrc("                                       THEN  ls_amounts-hwaer");
		buildSrc("*                                   comment C");
		buildSrc("                                    WHEN ls_amounts-betr2 <> 0");
		buildSrc("                                      THEN   ls_amounts-hwae2");
		buildSrc("                                     WHEN ls_amounts-betr3 <> 0");
		buildSrc("                                        THEN ls_amounts-hwae3 \" comment D");
		buildSrc("                                    ELSE \" comment E");
		buildSrc("                                    gc_default_currency_code ).");

		buildExp("    ev_used_currency_code = COND #( WHEN ls_amounts-betrw <> 0 \" comment A");
		buildExp("                                                               THEN ls_amounts-waers");
		buildExp("                                    \" comment B");
		buildExp("                                    WHEN ls_amounts-betrh <> 0 THEN ls_amounts-hwaer");
		buildExp("*                                   comment C");
		buildExp("                                    WHEN ls_amounts-betr2 <> 0 THEN ls_amounts-hwae2");
		buildExp("                                    WHEN ls_amounts-betr3 <> 0 THEN ls_amounts-hwae3 \" comment D");
		buildExp("                                    ELSE \" comment E");
		buildExp("                                                                    gc_default_currency_code ).");

		testRule();
	}

	@Test
	void testCommentsPreventingOneLiners() {
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.CREATE);

		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true \" comment A");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        THEN -1 \" comment B");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("                        \" comment C");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");
		buildSrc("");
		buildSrc("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildSrc("*                        comment D");
		buildSrc("                        THEN -1");
		buildSrc("                          ELSE 1 ).");

		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true \" comment A");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                      THEN -1 \" comment B");
		buildExp("                      ELSE 1 ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("                        \" comment C");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");
		buildExp("");
		buildExp("    ev_sign = COND #( WHEN iv_negative = abap_true");
		buildExp("*                        comment D");
		buildExp("                      THEN -1");
		buildExp("                      ELSE 1 ).");

		testRule();
	}


	@Test
	void testContinueAfterElseAdd7() {
		rule.configContinueAfterElse.setValue(true);
		rule.configValueIndent.setEnumValue(CondValueIndent.ADD_7);

		buildSrc("    display_time_info( COND #( LET t = '120000' IN");
		buildSrc("                             WHEN sy-timlo < t");
		buildSrc("                               THEN |{ sy-timlo TIME = ISO } AM|");
		buildSrc("                              WHEN sy-timlo > t AND sy-timlo < '240000'");
		buildSrc("                                THEN |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildSrc("                               WHEN sy-timlo = t");
		buildSrc("                                 THEN |High Noon|");
		buildSrc("                                ELSE THROW cx_cant_be( ) ) ).");

		buildExp("    display_time_info( COND #( LET t = '120000' IN");
		buildExp("                               WHEN sy-timlo < t THEN");
		buildExp("                                      |{ sy-timlo TIME = ISO } AM|");
		buildExp("                               WHEN sy-timlo > t AND sy-timlo < '240000' THEN");
		buildExp("                                      |{ CONV t( sy-timlo - 12 * 3600 ) TIME = ISO } PM|");
		buildExp("                               WHEN sy-timlo = t THEN");
		buildExp("                                      |High Noon|");
		buildExp("                               ELSE   THROW cx_cant_be( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepOneLinerSwitchAsIs() {
		buildSrc("    ev_num = SWITCH #( ev_num WHEN 999 THEN 0 ELSE ( ev_num + 1 ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateOneLinerSwitch() {
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.CREATE);

		buildSrc("    ev_num = SWITCH #( ev_num");
		buildSrc("                       WHEN 999 THEN 0");
		buildSrc("                       ELSE ( ev_num + 1 ) ).");

		buildExp("    ev_num = SWITCH #( ev_num WHEN 999 THEN 0 ELSE ( ev_num + 1 ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test 
	void testSwitchWithLet() {
		rule.configOneLinerStyle.setEnumValue(CondOneLinerStyle.SPLIT);

		buildSrc("    ev_result = SWITCH string( LET lv_prefix = 'number ' IN sy-index WHEN 1 THEN lv_prefix + 'one' ELSE lv_prefix + 'two' ).");

		buildExp("    ev_result = SWITCH string( LET lv_prefix = 'number ' IN");
		buildExp("                               sy-index");
		buildExp("                               WHEN 1");
		buildExp("                               THEN lv_prefix + 'one'");
		buildExp("                               ELSE lv_prefix + 'two' ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
