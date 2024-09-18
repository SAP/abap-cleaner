package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;

public class ObfuscatorTest {
	private final String SEP = ABAP.LINE_SEPARATOR;
	private final Language language = Language.ABAP;
	
	private Obfuscator obfuscator;
	
	private void prepareMethodScopeLongNames() {
		obfuscator = Obfuscator.createFor(language, false, false, false, false, false, false);
	}
	
	private void prepareMethodScopeShortNames() {
		obfuscator = Obfuscator.createFor(language, false, true, false, false, false, false);
	}
	
	private void prepareCommandScopeShortNames() {
		obfuscator = Obfuscator.createFor(language, true, true, false, false, false, false);
	}
	
	private void prepareMethodScopeLongNamesSimplified() {
		obfuscator = Obfuscator.createFor(language, false, false, true, false, false, false);
	}
	
	private void prepareMethodScopeShortNamesLiterals() {
		obfuscator = Obfuscator.createFor(language, false, true, false, false, false, true);
	}
	
	private void prepareCommandScopeShortNamesLiterals() {
		obfuscator = Obfuscator.createFor(language, true, true, false, false, false, true);
	}

	private void prepareMethodScopeLongNamesRemoveComments() {
		obfuscator = Obfuscator.createFor(language, false, false, false, true, true, false);
	}

	private void test(String codeText, String expCodeText) {
   	Code code;
   	try {
   		code = obfuscator.obfuscate(codeText);
		} catch (UnexpectedSyntaxAfterChanges | ParseException e) {
			fail();
			return;
		}
   	String actCodeText = code.toString();

   	// helper for creating tests:
   	/*
   	if (!actCodeText.equals(expCodeText)) {
   		// only report differences
	   	System.out.print("\t\ttest(\"" + codeText.replaceAll("\r\n", "\" + SEP + \"") + "\",");
	   	System.out.println();
	   	System.out.print("\t\t\t\t\"" + actCodeText.replaceAll("\r\n", "\" + SEP + \"") + "\");");
	   	System.out.println();
   	}
   	*/

   	assertEquals(expCodeText, actCodeText);
	}

	@Test
	void testMethodScopeLongNames() {
		prepareMethodScopeLongNames();

		// declarations
		test("CLASS a DEFINITION. ENDCLASS. CLASS b IMPLEMENTATION. ENDCLASS.",
				"CLASS cl_any_class DEFINITION. ENDCLASS. CLASS cl_other_class IMPLEMENTATION. ENDCLASS.");
		test("CLASS lcl_1 DEFINITION. ENDCLASS. CLASS lcl_2 IMPLEMENTATION. ENDCLASS.",
				"CLASS lcl_any_class DEFINITION. ENDCLASS. CLASS lcl_other_class IMPLEMENTATION. ENDCLASS.");
		test("METHODS a IMPORTING iv_1 TYPE i EXPORTING et_1 TYPE table_type.",
				"METHODS any_method IMPORTING iv_any_param TYPE i EXPORTING et_any_table TYPE ty_any_type.");
		test("METHODS a CHANGING ct_1 TYPE table_type RETURNING VALUE(rv_1) TYPE string.",
				"METHODS any_method CHANGING ct_any_table TYPE ty_any_type RETURNING VALUE(rv_any_result) TYPE string.");
		test("METHOD if_a~meth. ENDMETHOD.",
				"METHOD if_any_interface~any_method. ENDMETHOD.");
		test("INTERFACE a PUBLIC. ENDINTERFACE. INTERFACE b PUBLIC. ENDINTERFACE.",
				"INTERFACE if_any_interface PUBLIC. ENDINTERFACE. INTERFACE if_other_interface PUBLIC. ENDINTERFACE.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~b.",
				"DATA ls_any_struc TYPE if_any_interface~ty_any_type. DATA ls_other_struc TYPE if_any_interface~ty_any_type.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~c.",
				"DATA ls_any_struc TYPE if_any_interface~ty_any_type. DATA ls_other_struc TYPE if_any_interface~ty_other_type.");
		test("CLASS-DATA mo_1 TYPE REF TO cl_1. CLASS-DATA mo_2 TYPE REF TO if_1.",
				"CLASS-DATA mo_any_instance TYPE REF TO cl_any_class. CLASS-DATA mo_other_instance TYPE REF TO if_any_interface.");
		test("TRY. CATCH cx_1 INTO DATA(lx_1). ENDTRY.",
				"TRY. CATCH cx_any_exception INTO DATA(lx_any_exception). ENDTRY.");

		// values, structures, tables
		test("a = 1. b = 2. a = sy-subrc.", 
				"lv_any_value = 1. lv_other_value = 2. lv_any_value = sy-subrc.");
		test("a = xsdbool( b = abap_true OR mv_1 = space ).", 
				"lv_any_value = xsdbool( lv_other_value = abap_true OR mv_any_value = space ).");
		test("LOOP AT mt_1 ASSIGNING FIELD-SYMBOL(<fs1>). meth( ct_1 = <fs1>-nn ). ENDLOOP.", 
				"LOOP AT mt_any_table ASSIGNING FIELD-SYMBOL(<ls_any_struc>). any_method( ct_any_table = <ls_any_struc>-any_comp ). ENDLOOP.");
		test("a = b + c - d + e - f.", 
				"lv_any_value = lv_other_value + lv_third_value - lv_fourth_value + lv_fifth_value - lv_value_6.");
		test("struc1-comp1 = 1. struc2-comp1 = 2. struc1-comp2 = syst-datum.", 
				"ls_any_struc-any_comp = 1. ls_other_struc-any_comp = 2. ls_any_struc-other_comp = syst-datum.");
		test("tab1[ comp1 = gc_const1 ]-comp2 = gc_const2. tab2[ comp2 = 3 ]-comp1 = 4.", 
				"lt_any_table[ any_comp = gc_any_constant ]-other_comp = gc_other_constant. lt_other_table[ other_comp = 3 ]-any_comp = 4.");
		test("METHOD a. lr_1 = REF #( lt_1 ). CLEAR lr_1->*. ENDMETHOD.",  // METHOD required for OO context
				"METHOD any_method. lr_any_ref = REF #( lt_any_table ). CLEAR lr_any_ref->*. ENDMETHOD.");
		test("a_1 = 42. any_2 = 42.", 
				"lv_any_value = 42. lv_other_value = 42.");
		test("ls_1 = VALUE t( b = a ).", 
				"ls_any_struc = VALUE ty_any_type( any_comp = lv_any_value ).");

		// classes, interfaces, objects, methods
		test("obj1=>meth1( ). a = obj2->attr1. obj2=>meth2( ).", 
				"cl_any_class=>any_method( ). lv_any_value = lo_any_instance->mv_any_attribute. cl_other_class=>other_method( ).");
		test("obj1->meth1( ). me->meth2( ). a = me->attr1.", 
				"lo_any_instance->any_method( ). me->other_method( ). lv_any_value = me->mv_any_attribute.");
		test("a = if_1=>co_a + lif_1=>co_b.", 
				"lv_any_value = if_any_interface=>co_any_constant + lif_any_interface=>co_other_constant.");
		test("if1~meth1( param1 = val1 ). if1~meth2( param2 = val1 ).", 
				"if_any_interface~any_method( iv_any_param = lv_any_value ). if_any_interface~other_method( iv_other_param = lv_any_value ).");
		test("if1~meth1( param1 = val1 ). if2~meth1( param1 = val2 ).", 
				"if_any_interface~any_method( iv_any_param = lv_any_value ). if_other_interface~any_method( iv_any_param = lv_other_value ).");

		// comment lines
		test("  \"! ABAP Doc" + SEP + "* any" + SEP + "  \" other", 
				"  \"! comment" + SEP + "* comment" + SEP + "  \" comment");
	}

	@Test
	void testMethodScopeShortNames() {
		prepareMethodScopeShortNames();

		// declarations
		test("CLASS a DEFINITION. ENDCLASS. CLASS b IMPLEMENTATION. ENDCLASS.",
				"CLASS cl1 DEFINITION. ENDCLASS. CLASS cl2 IMPLEMENTATION. ENDCLASS.");
		test("CLASS lcl_1 DEFINITION. ENDCLASS. CLASS lcl_2 IMPLEMENTATION. ENDCLASS.",
				"CLASS cl1 DEFINITION. ENDCLASS. CLASS cl2 IMPLEMENTATION. ENDCLASS.");
		test("METHODS a IMPORTING iv_1 TYPE i EXPORTING et_1 TYPE table_type.",
				"METHODS meth1 IMPORTING p1 TYPE i EXPORTING t1 TYPE ty1.");
		test("METHODS a CHANGING ct_1 TYPE table_type RETURNING VALUE(rv_1) TYPE string.",
				"METHODS meth1 CHANGING t1 TYPE ty1 RETURNING VALUE(r1) TYPE string.");
		test("METHOD if_a~meth. ENDMETHOD.",
				"METHOD if1~meth1. ENDMETHOD.");
		test("INTERFACE a PUBLIC. ENDINTERFACE. INTERFACE b PUBLIC. ENDINTERFACE.",
				"INTERFACE if1 PUBLIC. ENDINTERFACE. INTERFACE if2 PUBLIC. ENDINTERFACE.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~b.",
				"DATA s1 TYPE if1~ty1. DATA s2 TYPE if1~ty1.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~c.",
				"DATA s1 TYPE if1~ty1. DATA s2 TYPE if1~ty2.");
		test("CLASS-DATA mo_1 TYPE REF TO cl_1. CLASS-DATA mo_2 TYPE REF TO if_1.",
				"CLASS-DATA o1 TYPE REF TO cl1. CLASS-DATA o2 TYPE REF TO if1.");
		test("TRY. CATCH cx_1 INTO DATA(lx_1). ENDTRY.",
				"TRY. CATCH cx1 INTO DATA(x1). ENDTRY.");

		// values, structures, tables
		test("a = 1. b = 2. a = sy-subrc.", 
				"v1 = 1. v2 = 2. v1 = sy-subrc.");
		test("a = xsdbool( b = abap_true OR mv_1 = space ).", 
				"v1 = xsdbool( v2 = abap_true OR v3 = space ).");
		test("LOOP AT mt_1 ASSIGNING FIELD-SYMBOL(<fs1>). meth( ct_1 = <fs1>-nn ). ENDLOOP.", 
				"LOOP AT t1 ASSIGNING FIELD-SYMBOL(<s1>). meth1( t2 = <s1>-c1 ). ENDLOOP.");
		test("a = b + c - d + e - f.", 
				"v1 = v2 + v3 - v4 + v5 - v6.");
		test("struc1-comp1 = 1. struc2-comp1 = 2. struc1-comp2 = syst-datum.", 
				"s1-c1 = 1. s2-c1 = 2. s1-c2 = syst-datum.");
		test("tab1[ comp1 = gc_const1 ]-comp2 = gc_const2. tab2[ comp2 = 3 ]-comp1 = 4.", 
				"t1[ c1 = co1 ]-c2 = co2. t2[ c2 = 3 ]-c1 = 4.");
		test("METHOD a. lr_1 = REF #( lt_1 ). CLEAR lr_1->*. ENDMETHOD.", // METHOD required for OO context 
				"METHOD meth1. r1 = REF #( t1 ). CLEAR r1->*. ENDMETHOD.");
		test("a_1 = 42. any_2 = 42.", 
				"v1 = 42. v2 = 42.");
		test("ls_1 = VALUE t( b = a ).", 
				"s1 = VALUE ty1( c1 = v1 ).");

		// classes, interfaces, objects, methods
		test("obj1=>meth1( ). a = obj2->attr1. obj2=>meth2( ).", 
				"cl1=>meth1( ). v1 = o1->m1. cl2=>meth2( ).");
		test("obj1->meth1( ). me->meth2( ). a = me->attr1.", 
				"o1->meth1( ). me->meth2( ). v1 = me->m1.");
		test("a = if_1=>co_a + lif_1=>co_b.", 
				"v1 = if1=>co1 + if2=>co2.");
		test("if1~meth1( param1 = val1 ). if1~meth2( param2 = val1 ).", 
				"if1~meth1( p1 = v1 ). if1~meth2( p2 = v1 ).");
		test("if1~meth1( param1 = val1 ). if2~meth1( param1 = val2 ).", 
				"if1~meth1( p1 = v1 ). if2~meth1( p1 = v2 ).");
	}

	@Test
	void testCommandScopeShortNames() {
		prepareCommandScopeShortNames();

		// declarations
		test("CLASS a DEFINITION. ENDCLASS. CLASS b IMPLEMENTATION. ENDCLASS.",
				"CLASS cl1 DEFINITION. ENDCLASS. CLASS cl1 IMPLEMENTATION. ENDCLASS.");
		test("CLASS lcl_1 DEFINITION. ENDCLASS. CLASS lcl_2 IMPLEMENTATION. ENDCLASS.",
				"CLASS cl1 DEFINITION. ENDCLASS. CLASS cl1 IMPLEMENTATION. ENDCLASS.");
		test("INTERFACE a PUBLIC. ENDINTERFACE. INTERFACE b PUBLIC. ENDINTERFACE.",
				"INTERFACE if1 PUBLIC. ENDINTERFACE. INTERFACE if1 PUBLIC. ENDINTERFACE.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~b.",
				"DATA s1 TYPE if1~ty1. DATA s1 TYPE if1~ty1.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~c.",
				"DATA s1 TYPE if1~ty1. DATA s1 TYPE if1~ty1.");
		test("CLASS-DATA mo_1 TYPE REF TO cl_1. CLASS-DATA mo_2 TYPE REF TO if_1.",
				"CLASS-DATA o1 TYPE REF TO cl1. CLASS-DATA o1 TYPE REF TO if1.");

		// values, structures, tables
		test("a = 1. b = 2. a = sy-subrc.",
				"v1 = 1. v1 = 2. v1 = sy-subrc.");
		test("a = xsdbool( b = abap_true OR mv_1 = space ).",
				"v1 = xsdbool( v2 = abap_true OR v3 = space ).");
		test("a = b + c - d + e - f.",
				"v1 = v2 + v3 - v4 + v5 - v6.");
		test("struc1-comp1 = 1. struc2-comp1 = 2. struc1-comp2 = syst-datum.",
				"s1-c1 = 1. s1-c1 = 2. s1-c1 = syst-datum.");
		test("tab1[ comp1 = gc_const1 ]-comp2 = gc_const2. tab2[ comp2 = 3 ]-comp1 = 4.",
				"t1[ c1 = co1 ]-c2 = co2. t1[ c1 = 3 ]-c2 = 4.");
		test("a_1 = 42. any_2 = 42.",
				"v1 = 42. v1 = 42.");

		// classes, interfaces, objects, methods
		test("obj1=>meth1( ). a = obj2->attr1. obj2=>meth2( ).",
				"cl1=>meth1( ). v1 = o1->m1. cl1=>meth1( ).");
		test("obj1->meth1( ). me->meth2( ). a = me->attr1.",
				"o1->meth1( ). me->meth1( ). v1 = me->m1.");
		test("a = if_1=>co_a + lif_1=>co_b.",
				"v1 = if1=>co1 + if2=>co2.");
		test("if1~meth1( param1 = val1 ). if1~meth2( param2 = val1 ).",
				"if1~meth1( p1 = v1 ). if1~meth1( p1 = v1 ).");
		test("if1~meth1( param1 = val1 ). if2~meth1( param1 = val2 ).",
				"if1~meth1( p1 = v1 ). if1~meth1( p1 = v1 ).");
	}

	@Test
	void testMethodScopeLongNamesSimplified() {
		// expectation: instead of iv_any_param, ct_any_table etc., all simplified identifiers look like local variables;
		// 'chains' like struc-comp, if~const, cl=>meth( ), if~meth( ), lo->attr etc. are shortened to a simple identifier
		// (but only if they consist of one Token)

		prepareMethodScopeLongNamesSimplified();

		// declarations: 
		test("METHODS a IMPORTING iv_1 TYPE i EXPORTING et_1 TYPE table_type.",
				"METHODS any_method IMPORTING lv_any_value TYPE i EXPORTING lt_any_table TYPE ty_any_type.");
		test("METHODS a CHANGING ct_1 TYPE table_type RETURNING VALUE(rv_1) TYPE string.",
				"METHODS any_method CHANGING lt_any_table TYPE ty_any_type RETURNING VALUE(lv_any_value) TYPE string.");
		test("METHOD if_a~meth. ENDMETHOD.",
				"METHOD any_method. ENDMETHOD.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~b.",
				"DATA ls_any_struc TYPE ty_any_type. DATA ls_other_struc TYPE ty_any_type.");
		test("DATA ls_1 TYPE a~b. DATA ls_2 TYPE a~c.",
				"DATA ls_any_struc TYPE ty_any_type. DATA ls_other_struc TYPE ty_other_type.");
		test("CLASS-DATA mo_1 TYPE REF TO cl_1. CLASS-DATA mo_2 TYPE REF TO if_1.",
				"CLASS-DATA lo_any_instance TYPE REF TO cl_any_class. CLASS-DATA lo_other_instance TYPE REF TO if_any_interface.");

		// values, structures, tables
		test("a = xsdbool( b = abap_true OR mv_1 = space ).",
				"lv_any_value = xsdbool( lv_other_value = abap_true OR lv_third_value = space ).");
		test("LOOP AT mt_1 ASSIGNING FIELD-SYMBOL(<fs1>). meth( ct_1 = <fs1>-nn ). ENDLOOP.",
				"LOOP AT lt_any_table ASSIGNING FIELD-SYMBOL(<ls_any_struc>). any_method( lt_other_table = lv_any_value ). ENDLOOP.");
		// expect that despite shortened identifiers, struc1-comp1 and struc2-comp1 are replaced with different(!) obfuscated identifiers
		test("struc1-comp1 = 1. struc2-comp1 = 2. struc1-comp2 = syst-datum.",
				"lv_any_value = 1. lv_other_value = 2. lv_third_value = syst-datum.");
		test("tab1[ comp1 = gc_const1 ]-comp2 = gc_const2. tab2[ comp2 = 3 ]-comp1 = 4.",
				"lt_any_table[ any_comp = lc_any_value ]-other_comp = lc_other_value. lt_other_table[ third_comp = 3 ]-fourth_comp = 4.");

		// classes, interfaces, objects, methods
		test("obj1=>meth1( ). a = obj2->attr1. obj2=>meth2( ).",
				"any_method( ). lv_any_value = lv_other_value. other_method( ).");
		test("obj1->meth1( ). me->meth2( ). a = me->attr1.",
				"any_method( ). other_method( ). lv_any_value = lv_other_value.");
		test("a = if_1=>co_a + lif_1=>co_b.",
				"lv_any_value = lo_any_value + lo_other_value.");
		test("if1~meth1( param1 = val1 ). if1~meth2( param2 = val1 ).",
				"any_method( iv_any_value = lv_any_value ). other_method( iv_other_value = lv_any_value ).");
		// expect that despite shortened identifiers, if1~meth1 and if2~meth1 are replaced with different(!) obfuscated method names
		test("if1~meth1( param1 = val1 ). if2~meth1( param1 = val2 ).",
				"any_method( iv_any_value = lv_any_value ). other_method( iv_any_value = lv_other_value ).");
	}

	@Test
	void testMethodScopeShortNamesLiterals() {
		prepareMethodScopeShortNamesLiterals();

		// in both integers and numeric text literals, expect 0 and 1 to be kept;  
		// all other numbers should start from 2 and then be replaced consistently
		test("a = 30 + 10 + 20. b = 1. c = 0. d = 20 + 30 + 10.",
				"v1 = 2 + 3 + 4. v2 = 1. v3 = 0. v4 = 4 + 2 + 3.");
		test("a = '30' && '10' && '3.1415'. b = '1'. c = '0'. d = '3.1415' && '30' && '10'.",
				"v1 = '2' && '3' && '4'. v2 = '1'. v3 = '0'. v4 = '4' && '2' && '3'.");

		// in non-numeric text literals, expect 'X' and ' ' to be kept;
		// all other literals should be replaced consistently with T1, T2, T3, ...
		test("a = 'abc' && 'def' && 'ghi'. b = 'X'. c = ' '. d = 'def' && 'abc' && 'ghi'.",
				"v1 = 'T1' && 'T2' && 'T3'. v2 = 'X'. v3 = ' '. v4 = 'T2' && 'T1' && 'T3'.");
	}

	@Test
	void testCommandScopeShortNamesLiterals() {
		prepareCommandScopeShortNamesLiterals();

		// in both integers and numeric text literals, expect 0 and 1 to be kept;  
		// all other numbers should start from 2, but with Command scope are not kept across Commands
		test("a = 30 + 10 + 20. b = 1. c = 0. d = 20 + 30 + 10.",
				"v1 = 2 + 3 + 4. v1 = 1. v1 = 0. v1 = 2 + 3 + 4.");
		test("a = '30' && '10' && '3.1415'. b = '1'. c = '0'. d = '3.1415' && '30' && '10'.",
				"v1 = '2' && '3' && '4'. v1 = '1'. v1 = '0'. v1 = '2' && '3' && '4'.");

		// in non-numeric text literals, expect 'X' and ' ' to be kept;
		// all other literals should be replaced with T1, T2, T3, ...
		test("a = 'abc' && 'def' && 'ghi'. b = 'X'. c = ' '. d = 'def' && 'abc' && 'ghi'.",
				"v1 = 'T1' && 'T2' && 'T3'. v1 = 'X'. v1 = ' '. v1 = 'T1' && 'T2' && 'T3'.");
	}

	@Test
	void testRemoveComments() {
		prepareMethodScopeLongNamesRemoveComments();

		// ensure that comments at line end and comment lines are removed (except pseudo-comments)
		test("a = 1. \" comment" + SEP + SEP + "* comment" + SEP + "b = 2. \"#EC any",
				"lv_any_value = 1." + SEP + SEP + "lv_other_value = 2.");
		test("a = 1. \" comment" + SEP + "* comment" + SEP + "b = 2. \"#EC any",
				"lv_any_value = 1." + SEP + "lv_other_value = 2.");
		
		// ensure that comment lines within Commands are removed
		test("a = 1 + \" comment" + SEP + SEP + "* comment" + SEP + "b.",
				"lv_any_value = 1 +" + SEP + SEP + "lv_other_value.");
		test("a = 1 + \" comment" + SEP + "* comment" + SEP + "b.",
				"lv_any_value = 1 +" + SEP + "lv_other_value.");
	}
}
