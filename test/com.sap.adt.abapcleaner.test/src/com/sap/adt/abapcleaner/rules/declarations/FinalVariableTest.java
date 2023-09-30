package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class FinalVariableTest extends RuleTestBase {
	// detailed tests for Token.getMemoryAccessType() can be found in TokenTest.testAccessType...()

	// private FinalVariableRule rule;
	
	FinalVariableTest() {
		super(RuleID.FINAL_VARIABLE);
		// rule = (FinalVariableRule)getRule();
	}
	
	@Test
	void testNonInclineDeclarationUnchanged() {
		buildSrc("    DATA lv_value1 TYPE i.");
		buildSrc("    DATA: lv_value2 TYPE i, lv_value3.");
		buildSrc("    rv_result = lv_value1 + lv_value2 + lv_value3.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testFinalDeclarationsUnchanged() {
		buildSrc("    FINAL(lv_value) = get_value( ).");
		buildSrc("    SELECT * FROM sflight INTO TABLE @FINAL(lt_flight).");
		buildSrc("    rv_result = lv_value + lines( lt_flight ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataWithCalculationAssignmentUnchanged() {
		// ensure that calculation assignment is identified correctly as a write position
		buildSrc("    DATA(lv_value) = get_value( ).");
		buildSrc("    lv_value += 1.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataWithEqualsSignChainUnchanged() {
		// ensure that equals sign chaining is identified correctly as a write position
		buildSrc("    DATA(lv_value) = get_value( ).");
		buildSrc("    rv_value = lv_value = 1.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataWithSpecialCommandUnchanged() {
		// ensure that the receiving variable of various ABAP commands is identified correctly as a write position
		// detailed tests for Token.getMemoryAccessType() can be found in TokenTest.testAccessType...()
		
		buildSrc("    DATA(lv_date) = get_date( ).");
		buildSrc("    DATA(lv_time) = get_time( ).");
		buildSrc("    DATA(lv_value1) = get_value( ).");
		buildSrc("    DATA(lv_value2) = get_value( ).");
		buildSrc("    DATA(lv_value3) = get_value( ).");
		buildSrc("    DATA(lv_value4) = get_value( ).");
		buildSrc("    DATA(lo_instance) = get_instance( ).");
		buildSrc("");
		buildSrc("    CLEAR: lv_date.");
		buildSrc("    GET TIME FIELD lv_time.");
		buildSrc("    ADD 1 TO lv_value1.");
		buildSrc("    SUBTRACT 1 FROM lv_value2.");
		buildSrc("    MULTIPLY lv_value3 BY 2.");
		buildSrc("    DIVIDE lv_value4 BY 2.");
		buildSrc("    CREATE OBJECT lo_instance.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLeadingInlineDeclChanged() {
		buildSrc("    DATA(lo_utility) = cl_any_factory=>get( )->get_any_utility( ).");
		buildSrc("    DATA(lv_date) = lo_utility->get_any_date( ).");
		buildSrc("    DATA(lv_time) = lo_utility->get_any_time( ).");
		buildSrc("    rv_result = |{ lv_date } { lv_time }|.");

		buildExp("    FINAL(lo_utility) = cl_any_factory=>get( )->get_any_utility( ).");
		buildExp("    FINAL(lv_date) = lo_utility->get_any_date( ).");
		buildExp("    FINAL(lv_time) = lo_utility->get_any_time( ).");
		buildExp("    rv_result = |{ lv_date } { lv_time }|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNonLeadingInlineDeclChanged() {
		buildSrc("    GET TIME FIELD DATA(lv_time).");
		buildSrc("    GET PARAMETER ID 'ANY_PARAMETER' FIELD DATA(lv_param)");
		buildSrc("    READ TABLE lt_data INDEX 1 INTO DATA(ls_data).");
		buildSrc("");
		buildSrc("    LOOP AT it_flight INTO DATA(ls_flight) ##INTO_OK.");
		buildSrc("      rv_sum += ls_flight-seatsocc.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("    TRY.");
		buildSrc("      CATCH cx_any INTO DATA(lx_any).");
		buildSrc("    ENDTRY.");

		buildExp("    GET TIME FIELD FINAL(lv_time).");
		buildExp("    GET PARAMETER ID 'ANY_PARAMETER' FIELD FINAL(lv_param)");
		buildExp("    READ TABLE lt_data INDEX 1 INTO FINAL(ls_data).");
		buildExp("");
		buildExp("    LOOP AT it_flight INTO FINAL(ls_flight) ##INTO_OK.");
		buildExp("      rv_sum += ls_flight-seatsocc.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("    TRY.");
		buildExp("      CATCH cx_any INTO FINAL(lx_any).");
		buildExp("    ENDTRY.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testSelectIntoTableChanged() {
		buildSrc("    SELECT carrid, connid, seatsocc");
		buildSrc("           FROM sflight");
		buildSrc("           WHERE fldate = @iv_date");
		buildSrc("           INTO TABLE @DATA(lt_flight).");
		buildSrc("    rt_flight = lt_flight.");

		buildExp("    SELECT carrid, connid, seatsocc");
		buildExp("           FROM sflight");
		buildExp("           WHERE fldate = @iv_date");
		buildExp("           INTO TABLE @FINAL(lt_flight).");
		buildExp("    rt_flight = lt_flight.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOnlyExportingParameterChangedWithReceiving() {
		buildSrc("    DATA(e1) = get_value( ).");
		buildSrc("    DATA(e2) = get_value( ).");
		buildSrc("    DATA(i1) = get_value( ).");
		buildSrc("    DATA(i2) = get_value( ).");
		buildSrc("    DATA(c1) = get_value( ).");
		buildSrc("    DATA(c2) = get_value( ).");
		buildSrc("    DATA(r1) = get_value( ).");
		buildSrc("");
		buildSrc("    any_method( EXPORTING e1 = e1");
		buildSrc("                          e2 = e2");
		buildSrc("                IMPORTING i1 = i1");
		buildSrc("                          i2 = i2");
		buildSrc("                CHANGING  c1 = c1");
		buildSrc("                          c2 = c2");
		buildSrc("                RECEIVING r1 = r1 ).");

		buildExp("    FINAL(e1) = get_value( ).");
		buildExp("    FINAL(e2) = get_value( ).");
		buildExp("    DATA(i1) = get_value( ).");
		buildExp("    DATA(i2) = get_value( ).");
		buildExp("    DATA(c1) = get_value( ).");
		buildExp("    DATA(c2) = get_value( ).");
		buildExp("    DATA(r1) = get_value( ).");
		buildExp("");
		buildExp("    any_method( EXPORTING e1 = e1");
		buildExp("                          e2 = e2");
		buildExp("                IMPORTING i1 = i1");
		buildExp("                          i2 = i2");
		buildExp("                CHANGING  c1 = c1");
		buildExp("                          c2 = c2");
		buildExp("                RECEIVING r1 = r1 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testOnlyExportingParameterChanged() {
		buildSrc("    DATA(e1) = get_value( ).");
		buildSrc("    DATA(e2) = get_value( ).");
		buildSrc("    DATA(i1) = get_value( ).");
		buildSrc("    DATA(i2) = get_value( ).");
		buildSrc("    DATA(c1) = get_value( ).");
		buildSrc("    DATA(c2) = get_value( ).");
		buildSrc("    DATA(r1) = get_value( ).");
		buildSrc("");
		buildSrc("    r1 = any_method( EXPORTING e1 = e1");
		buildSrc("                               e2 = e2");
		buildSrc("                     IMPORTING i1 = i1");
		buildSrc("                               i2 = i2");
		buildSrc("                     CHANGING  c1 = c1");
		buildSrc("                               c2 = c2 ).");

		buildExp("    FINAL(e1) = get_value( ).");
		buildExp("    FINAL(e2) = get_value( ).");
		buildExp("    DATA(i1) = get_value( ).");
		buildExp("    DATA(i2) = get_value( ).");
		buildExp("    DATA(c1) = get_value( ).");
		buildExp("    DATA(c2) = get_value( ).");
		buildExp("    DATA(r1) = get_value( ).");
		buildExp("");
		buildExp("    r1 = any_method( EXPORTING e1 = e1");
		buildExp("                               e2 = e2");
		buildExp("                     IMPORTING i1 = i1");
		buildExp("                               i2 = i2");
		buildExp("                     CHANGING  c1 = c1");
		buildExp("                               c2 = c2 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testInlineImportingReceivingParameterChanged() {
		buildSrc("    DATA(e1) = get_value( ).");
		buildSrc("    DATA(e2) = get_value( ).");
		buildSrc("    DATA(c1) = get_value( ).");
		buildSrc("    DATA(c2) = get_value( ).");
		buildSrc("");
		buildSrc("    DATA(r1) = any_method( EXPORTING e1 = e1");
		buildSrc("                                     e2 = e2");
		buildSrc("                           IMPORTING i1 = DATA(i1)");
		buildSrc("                                     i2 = DATA(i2)");
		buildSrc("                           CHANGING  c1 = c1");
		buildSrc("                                     c2 = c2 ).");

		buildExp("    FINAL(e1) = get_value( ).");
		buildExp("    FINAL(e2) = get_value( ).");
		buildExp("    DATA(c1) = get_value( ).");
		buildExp("    DATA(c2) = get_value( ).");
		buildExp("");
		buildExp("    FINAL(r1) = any_method( EXPORTING e1 = e1");
		buildExp("                                      e2 = e2");
		buildExp("                            IMPORTING i1 = FINAL(i1)");
		buildExp("                                      i2 = FINAL(i2)");
		buildExp("                            CHANGING  c1 = c1");
		buildExp("                                      c2 = c2 ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testOpenCursorUnchanged() {
		// ensure that nothing is changed in an OPEN CURSOR command (even if CLOSE CURSOR is out of sight),  
		// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenfinal_inline.htm
		buildSrc("    OPEN CURSOR @DATA(dbcur) FOR SELECT * FROM dtab.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithWriteToFieldSymbolCompInLoop() {
		// expect DATA to be kept, because <ls_data> is assigned to lt_data and used in a write position 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildSrc("      <ls_data>-any_component = get_value( ).");
		buildSrc("    ENDLOOP.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithWriteToFieldSymbolInLoop() {
		// expect DATA to be kept, because <ls_data> is assigned to lt_data and used in a write position 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildSrc("      <ls_data> = get_data( ).");
		buildSrc("    ENDLOOP.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithReadTableAndWriteToFieldSymbol() {
		// expect DATA to be kept, because <ls_data> is assigned to lt_data and used in a write position 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    FIELD-SYMBOLS <ls_data> LIKE LINE OF lt_data.");
		buildSrc("    READ TABLE lt_data WITH TABLE KEY any_comp = 1 ASSIGNING <ls_data>.");
		buildSrc("    CLEAR: <ls_data>-any_component,");
		buildSrc("           <ls_data>-other_component.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithAssignAndChangingFieldSymbol() {
		// expect DATA to be kept, because <ls_data> is assigned to lt_data and used in a write position 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    ASSIGN lt_data[ 1 ] TO FIELD-SYMBOL(<ls_data>).");
		buildSrc("    any_method( CHANGING iv_any_param = <ls_data>-any_component ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataChangedWithAssignAndReadFromFieldSymbol() {
		// expect FINAL to be introduced, because <ls_data> is only used in read positions 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    ASSIGN lt_data[ 1 ] TO FIELD-SYMBOL(<ls_data>).");
		buildSrc("    any_method( iv_any_param = <ls_data>-any_component ).");
		buildSrc("    ev_result = <ls_data>-other_component.");

		buildExp("    FINAL(lt_data) TYPE ty_tt_any_table.");
		buildExp("    ASSIGN lt_data[ 1 ] TO FIELD-SYMBOL(<ls_data>).");
		buildExp("    any_method( iv_any_param = <ls_data>-any_component ).");
		buildExp("    ev_result = <ls_data>-other_component.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithWriteToDataRefInLoop() {
		// expect DATA to be kept, because lr_data points to lt_data (and is used in a write position) 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    LOOP AT lt_data REFERENCE INTO FINAL(lr_data).");
		buildSrc("      any_method( lr_data ).");
		buildSrc("    ENDLOOP.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithDataRefInLoop() {
		// expect DATA to be kept, because lr_data points to lt_data 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    GET REFERENCE OF lt_data INTO FINAL(lr_data).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithReadTableReferenceInto() {
		// expect DATA to be kept, because lr_data points to lt_data (and is used in a write position) 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    DATA ls_data TYPE REF TO ty_s_any_struc.");
		buildSrc("    READ TABLE lt_data WITH TABLE KEY any_comp = 1 REFERENCE INTO lr_data.");
		buildSrc("    CLEAR: lr_data->any_component,");
		buildSrc("           lr_data->other_component.");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedWithAssignDataRef() {
		// expect DATA to be kept, because lr_data points to lt_data  
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    FINAL(lr_data) = REF #( lt_data[ 1 ]-component ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedExportingRef() {
		// expect DATA to be kept, because lr_data points to lt_data  
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    any_method( ir_data = REF #( lt_data[ 1 ] ) ).");
		
		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataChangedWithDataRefToAttribute() {
		// expect FINAL to be introduced, because lr_data only points to an attribute 
		buildSrc("    DATA(lt_data) TYPE ty_tt_any_table.");
		buildSrc("    FINAL(lr_data) = REF #( mt_data ).");
		buildSrc("    any_method( it_data = lt_data ).");
		buildSrc("    ev_result = lt_data[ 1 ]-any_component.");

		buildExp("    FINAL(lt_data) TYPE ty_tt_any_table.");
		buildExp("    FINAL(lr_data) = REF #( mt_data ).");
		buildExp("    any_method( it_data = lt_data ).");
		buildExp("    ev_result = lt_data[ 1 ]-any_component.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedForNonStrictMode() {
		// expect DATA to be kept if the SELECT statement is NOT in strict mode, i.e. if the INTO clause is followed
		// by another SELECT clause such as WHERE, GROUP BY, ORDER BY etc.
		buildSrc("    SELECT SINGLE * FROM dtab");
		buildSrc("           INTO @DATA(ls_data)");
		buildSrc("           WHERE comp =  1.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedForPerformUsing() {
		// the syntax check does not prevent changes on any of the parameters in a PERFORM call, therefore even variables 
		// passed as USING must NOT be declared with FINAL
		
		buildSrc("    DATA(lt_table) = get_table( ).");
		buildSrc("    DATA(lv_any) = 0.");
		buildSrc("    DATA(lv_other) = 0.");
		buildSrc("    PERFORM any_form IN PROGRAM any_program TABLES lt_table USING lv_any CHANGING lv_other.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedDueToMacroUsage() {
		// expect seemingly unchanged variables to be kept as DATA, because they might be used in the macro
		
		buildSrc("    DATA(lv_any) = 1.");
		buildSrc("    DATA(lv_other) = 2.");
		buildSrc("");
		buildSrc("    any_macro.");
		buildSrc("");
		buildSrc("    rv_result = lv_any + lv_other.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataUnchangedDueToChainedMacroUsage() {
		// expect seemingly unchanged variables to be kept as DATA, because they might be used in the macro
		
		buildSrc("    DATA(lv_any) = 1.");
		buildSrc("    DATA(lv_other) = 2.");
		buildSrc("");
		buildSrc("    other_macro: iv_any_param, iv_other_param.");
		buildSrc("");
		buildSrc("    rv_result = lv_any + lv_other.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testMethodCallInsideCondUnchanged() {
		// expect FINAL to be prevented whenever a method call is used inside of a constructor expression, because some(!)
		// of these cases cause syntax errors - e.g. for COND, it is not possible to have a method call in 
		// both the THEN and the ELSE clause 
		
		buildSrc("    DATA(lv_data_1) = get_any_integer( ).");
		buildSrc("    DATA(lv_data_2) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE 2 ).");
		buildSrc("    DATA(lv_data_3) = get_any_integer( ) + get_any_integer( ) * COND i( WHEN lv_condition = abap_true THEN 1 ELSE 2 ).");
		buildSrc("");
		buildSrc("    DATA(lv_data_4) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE 2 ).");
		buildSrc("    DATA(lv_data_5) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE get_any_integer( ) ).");
		buildSrc("    DATA(lv_data_6) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE 2 ) + get_any_integer( ).");
		buildSrc("    DATA(lv_data_7) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE get_any_integer( ) ) + get_any_integer( ).");
		buildSrc("    DATA(lv_data_8) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE get_any_integer( ) ).");

		buildExp("    FINAL(lv_data_1) = get_any_integer( ).");
		buildExp("    FINAL(lv_data_2) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE 2 ).");
		buildExp("    FINAL(lv_data_3) = get_any_integer( ) + get_any_integer( ) * COND i( WHEN lv_condition = abap_true THEN 1 ELSE 2 ).");
		buildExp("");
		buildExp("    DATA(lv_data_4) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE 2 ).");
		buildExp("    DATA(lv_data_5) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE get_any_integer( ) ).");
		buildExp("    DATA(lv_data_6) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE 2 ) + get_any_integer( ).");
		buildExp("    DATA(lv_data_7) = COND i( WHEN lv_condition = abap_true THEN 1 ELSE get_any_integer( ) ) + get_any_integer( ).");
		buildExp("    DATA(lv_data_8) = COND i( WHEN lv_condition = abap_true THEN get_any_integer( ) ELSE get_any_integer( ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testMethodCallInsideValueUnchanged() {
		// expect FINAL to be prevented whenever a method call is used inside of a constructor expression, because some(!)
		// of these cases cause syntax errors - e.g. for VALUE, a method call is only possible on the last component in the code
		// (regardless of the sequence of components in the structure definition)
		
		buildSrc("    DATA(ls_struc_1) = VALUE ty_s_any( comp1 = 1");
		buildSrc("                                       comp2 = 2 ).");
		buildSrc("    DATA(ls_struc_2) = VALUE ty_s_any( comp1 = 1");
		buildSrc("                                       comp2 = VALUE #( comp3 = 'abc' ) ).");
		buildSrc("");
		buildSrc("    DATA(ls_struc_3) = VALUE ty_s_any( comp1 = get_any_integer( )");
		buildSrc("                                       comp2 = 1 ).");
		buildSrc("    DATA(ls_struc_4) = VALUE ty_s_any( comp1 = 1");
		buildSrc("                                       comp2 = VALUE #( comp3 = get_string( ) ) ).");

		buildExp("    FINAL(ls_struc_1) = VALUE ty_s_any( comp1 = 1");
		buildExp("                                        comp2 = 2 ).");
		buildExp("    FINAL(ls_struc_2) = VALUE ty_s_any( comp1 = 1");
		buildExp("                                        comp2 = VALUE #( comp3 = 'abc' ) ).");
		buildExp("");
		buildExp("    DATA(ls_struc_3) = VALUE ty_s_any( comp1 = get_any_integer( )");
		buildExp("                                       comp2 = 1 ).");
		buildExp("    DATA(ls_struc_4) = VALUE ty_s_any( comp1 = 1");
		buildExp("                                       comp2 = VALUE #( comp3 = get_string( ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTypesAndDataObjectsWithSameNames() {
		// ensure that the types 't1' and 't2' are correctly distinguished from the data objects 't1' and 't2' 
		
		buildSrc("  METHOD any_method.");
		buildSrc("    TYPES t1 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildSrc("    TYPES t2 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildSrc("");
		buildSrc("    DATA lt_table1 TYPE t1.");
		buildSrc("    DATA ls_struc TYPE LINE OF t1.");
		buildSrc("");
		buildSrc("    DATA(t1) = VALUE t1( ).");
		buildSrc("    DATA(t2) = VALUE t2( ).");
		buildSrc("");
		buildSrc("    lt_table1 = VALUE t1( ( a = 1 ) ).");
		buildSrc("    any_method( lt_table = lt_table1 ).");
		buildSrc("    any_method( lt_table = VALUE t2( ( a = 1 ) ) ).");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("    TYPES t1 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildExp("    TYPES t2 TYPE STANDARD TABLE OF ty_s1 WITH EMPTY KEY.");
		buildExp("");
		buildExp("    DATA lt_table1 TYPE t1.");
		buildExp("    DATA ls_struc TYPE LINE OF t1.");
		buildExp("");
		buildExp("    FINAL(t1) = VALUE t1( ).");
		buildExp("    FINAL(t2) = VALUE t2( ).");
		buildExp("");
		buildExp("    lt_table1 = VALUE t1( ( a = 1 ) ).");
		buildExp("    any_method( lt_table = lt_table1 ).");
		buildExp("    any_method( lt_table = VALUE t2( ( a = 1 ) ) ).");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testTableExpressionInWritePos() {
		buildSrc("    DATA(lt_a) = get_table( ).");
		buildSrc("    DATA(lt_b) = get_table( ).");
		buildSrc("    DATA(lt_c) = get_table( ).");
		buildSrc("    DATA(lt_d) = get_table( ).");
		buildSrc(".");
		buildSrc("    lt_a[ 1 ] = is_any.");
		buildSrc("    lt_b[ 1 ]-comp = 1.");
		buildSrc("    lt_c[ id = 1 ]-inner_table[ 1 ]-comp = get_value( ).");
		buildSrc("    lt_d[ 1 ]-ref->mv_attribute = 1.");
		
		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testModifyEntityFieldsTabUnchanged() {
		// expect all three DATA(lt_...) to remain unchanged, because they are used as fields tables, 
		// and a syntax check on static read-only fields is not possible with all variants 
		
		buildSrc("    get_rap_data( IMPORTING et_create_root  = DATA(lt_create_root_data)");
		buildSrc("                            et_create_child = DATA(lt_create_child_data)");
		buildSrc("                            et_update       = DATA(lt_update_data) ).");
		buildSrc("");
		buildSrc("    MODIFY ENTITY any_entity");
		buildSrc("        CREATE SET FIELDS ##SETFIELDS_OK");
		buildSrc("               WITH lt_create_root_data");
		buildSrc("        CREATE BY \\_child SET FIELDS WITH lt_create_child_data");
		buildSrc("        UPDATE SET FIELDS WITH lt_update_data");
		buildSrc("          FAILED   FINAL(failed)");
		buildSrc("          REPORTED FINAL(reported).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}