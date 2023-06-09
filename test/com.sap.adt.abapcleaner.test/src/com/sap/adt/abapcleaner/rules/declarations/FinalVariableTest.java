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
}