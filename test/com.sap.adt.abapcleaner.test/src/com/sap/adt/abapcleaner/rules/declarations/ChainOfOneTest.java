package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ChainOfOneTest extends RuleTestBase {
	private ChainOfOneRule rule;
	
	ChainOfOneTest(){
		super(RuleID.CHAIN_OF_ONE);
		rule = (ChainOfOneRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnClassDefinitionSections.setValue(true);
		rule.configExecuteOnLocalDeclarations.setValue(true);
		rule.configExecuteOnNonDeclarations.setValue(true);
	}
	
	@Test
	void testOneLinersInMethod() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    CONSTANTS: lc_any_constant TYPE any_type VALUE 1200.");
		buildSrc("");
		buildSrc("    DATA: lth_any_table TYPE ty_th_any_table_type.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS: <ls_item> LIKE LINE OF its_item.");
		buildSrc("");
		buildSrc("    CHECK: its_table IS NOT INITIAL.");

		buildExp("    CONSTANTS lc_any_constant TYPE any_type VALUE 1200.");
		buildExp("");
		buildExp("    DATA lth_any_table TYPE ty_th_any_table_type.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <ls_item> LIKE LINE OF its_item.");
		buildExp("");
		buildExp("    CHECK its_table IS NOT INITIAL.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testChainOfTwoInMethod() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    CONSTANTS: lc_a TYPE i VALUE 1, lc_b TYPE i VALUE 2.");
		buildSrc("    DATA: lv_value_a TYPE string,");
		buildSrc("          lv_value_b TYPE string.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("      <ls_fs_1> LIKE LINE OF its_table_1,");
		buildSrc("      <ls_fs_2> LIKE LINE OF its_table_2.");
		buildSrc("");
		buildSrc("    CHECK: its_table_1 IS NOT INITIAL,");
		buildSrc("      its_table_2 IS NOT INITIAL.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testTwoLinersInMethod() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    DATA:");
		buildSrc("      lo_item TYPE REF TO cl_item ##NEEDED.");
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("         <ls_data> TYPE ty_s_data.");

		buildExp("    DATA lo_item TYPE REF TO cl_item ##NEEDED.");
		buildExp("    FIELD-SYMBOLS <ls_data> TYPE ty_s_data.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testClassDeclarationOneLiners() {
		rule.configExecuteOnLocalDeclarations.setValue(false);
		rule.configExecuteOnNonDeclarations.setValue(false);

		buildSrc("    INTERFACES: if_any_interface PARTIALLY IMPLEMENTED.");
		buildSrc("    CONSTANTS: co_any_amount TYPE ty_amount VALUE 1200.");
		buildSrc("    CLASS-DATA: mv_value TYPE int8.");
		buildSrc("    CLASS-METHODS: any_class_method IMPORTING iv_value TYPE i.");
		buildSrc("    DATA: mth_any_hash_table TYPE ty_th_hash_table.");
		buildSrc("    METHODS: any_method FOR TESTING.");

		buildExp("    INTERFACES if_any_interface PARTIALLY IMPLEMENTED.");
		buildExp("    CONSTANTS co_any_amount TYPE ty_amount VALUE 1200.");
		buildExp("    CLASS-DATA mv_value TYPE int8.");
		buildExp("    CLASS-METHODS any_class_method IMPORTING iv_value TYPE i.");
		buildExp("    DATA mth_any_hash_table TYPE ty_th_hash_table.");
		buildExp("    METHODS any_method FOR TESTING.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testClassDeclarationChainOfTwo() {
		rule.configExecuteOnLocalDeclarations.setValue(false);
		rule.configExecuteOnNonDeclarations.setValue(false);

		buildSrc("    INTERFACES: if_any_interface PARTIALLY IMPLEMENTED, if_other_interface");
		buildSrc("    CONSTANTS: co_any_amount   TYPE ty_amount VALUE 1200,");
		buildSrc("               co_other_amount TYPE ty_amount VALUE 1500.");
		buildSrc("    CLASS-DATA: mv_value TYPE int8,");
		buildSrc("      mv_other_value TYPE int8.");
		buildSrc("    CLASS-METHODS:");
		buildSrc("      any_class_method IMPORTING iv_value TYPE i,");
		buildSrc("      other_class_method RETURNING VALUE(rv_result) TYPE string.");
		buildSrc("    DATA: mv_value_1 TYPE i,");
		buildSrc("      mv_value_2 TYPE i.");
		buildSrc("    METHODS: any_method FOR TESTING,");
		buildSrc("      other_method FOR TESTING.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	
	@Test
	void testClassDeclarationMultiLine() {
		rule.configExecuteOnLocalDeclarations.setValue(false);
		rule.configExecuteOnNonDeclarations.setValue(false);

		buildSrc("    INTERFACES:");
		buildSrc("      if_any_interface PARTIALLY IMPLEMENTED.");
		buildSrc("    CLASS-DATA:");
		buildSrc("      mv_value TYPE int8.");
		buildSrc("    DATA:");
		buildSrc("      mo_item TYPE REF TO cl_item ##NEEDED.");
		buildSrc("    CLASS-METHODS: any_class_method");
		buildSrc("      IMPORTING");
		buildSrc("        iv_value TYPE i.");
		buildSrc("    ALIASES:");
		buildSrc("      any_method FOR if_any_interface~any_method.");

		buildExp("    INTERFACES if_any_interface PARTIALLY IMPLEMENTED.");
		buildExp("    CLASS-DATA mv_value TYPE int8.");
		buildExp("    DATA mo_item TYPE REF TO cl_item ##NEEDED.");
		buildExp("    CLASS-METHODS any_class_method");
		buildExp("      IMPORTING");
		buildExp("        iv_value TYPE i.");
		buildExp("    ALIASES any_method FOR if_any_interface~any_method.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSkipClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    DATA: lth_any_hash_table TYPE ty_th_hash_table_type.");
		buildSrc("    DATA:");
		buildSrc("      mo_item TYPE REF TO cl_item ##NEEDED.");
		buildSrc("    CONSTANTS:");
		buildSrc("         mv_loop_count TYPE i VALUE 10.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSkipLocalDeclaration() {
		rule.configExecuteOnLocalDeclarations.setValue(false);

		buildSrc("    DATA: lth_any_hash_table TYPE ty_th_hash_table_type.");
		buildSrc("    DATA:");
		buildSrc("      mo_item TYPE REF TO cl_item ##NEEDED.");
		buildSrc("    CONSTANTS:");
		buildSrc("         mv_loop_count TYPE i VALUE 10.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testSkipNonDeclarations() {
		rule.configExecuteOnNonDeclarations.setValue(false);

		buildSrc("    CHECK: its_table IS NOT INITIAL.");
		buildSrc("    CHECK: its_table_1 IS NOT INITIAL,");
		buildSrc("      its_table_2 IS NOT INITIAL.");
		buildSrc("    a += : 1.");
		buildSrc("    CALL METHOD : any_method.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCommentAfterColon() {
		buildSrc("    DATA any_data TYPE i.");
		buildSrc("");
		buildSrc("    DATA: \" comment");
		buildSrc("*   more comment");
		buildSrc("      lo_item TYPE REF TO cl_item ##NEEDED.");

		buildExp("    DATA any_data TYPE i.");
		buildExp("");
		buildExp("    \" comment");
		buildExp("*   more comment");
		buildExp("    DATA lo_item TYPE REF TO cl_item ##NEEDED.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testCommentBeforeColon() {
		buildSrc("    DATA \" comment");
		buildSrc("*   more comment");
		buildSrc("    : lo_item TYPE REF TO cl_item ##NEEDED.");

		buildExp("    \" comment");
		buildExp("*   more comment");
		buildExp("    DATA lo_item TYPE REF TO cl_item ##NEEDED.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testMultipleChainColons() {
		// additional chain colons are marked in red by ADT, but syntactically correct - the compiler simply ignores them; 
		// however, when the first colon is removed, the others must be removed, too
		
		buildSrc(" DATA: lv_num : TYPE i.");
		buildSrc(" DATA: lv_value : : TYPE:string.");
		buildSrc(" CHECK: lv_value:>:5.");

		buildExp(" DATA lv_num TYPE i.");
		buildExp(" DATA lv_value TYPE string.");
		buildExp(" CHECK lv_value > 5.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testNonDeclarations() {
		buildSrc("    CALL METHOD : any_method( ).");
		buildSrc("    CALL METHOD : \" comment directly after initial keyword collocation");
		buildSrc("                  any_method( ).");
		buildSrc("");
		buildSrc("    CALL METHOD \" comment directly after initial keyword collocation");
		buildSrc("                :  any_method( ).");
		buildSrc("    other_method( iv_param_a = 1");
		buildSrc("                  iv_param_b = 'b' ) \" comment later in the command");
		buildSrc("                  : .");
		buildSrc("    lv_value = iv_value +:1.");
		buildSrc("    ASSIGN it_data[ 5 ] TO:<ls_data>.");

		buildExp("    CALL METHOD any_method( ).");
		buildExp("    \" comment directly after initial keyword collocation");
		buildExp("    CALL METHOD any_method( ).");
		buildExp("");
		buildExp("    \" comment directly after initial keyword collocation");
		buildExp("    CALL METHOD any_method( ).");
		buildExp("    other_method( iv_param_a = 1");
		buildExp("                  iv_param_b = 'b' ) \" comment later in the command");
		buildExp("                  .");
		buildExp("    lv_value = iv_value + 1.");
		buildExp("    ASSIGN it_data[ 5 ] TO <ls_data>.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	

	@Test
	void testKeepColonInTypesEndOfAfterInclude() {
		// expect that the chain colon is NOT removed in the special case of "TYPES: END OF ..." after an "INCLUDE TYPE ..." line,  
		// because removing the colon here would cause poor layout the next time PrettyPrinter is called - PrettyPrinter would produce:
		//    TYPES: BEGIN OF ty_s_struc,
		//             comp TYPE i.
		//             INCLUDE TYPE any_include.
		//  TYPES  END OF ty_s_struc.

		buildSrc("    TYPES: BEGIN OF ty_s_struc,");
		buildSrc("             comp TYPE i.");
		buildSrc("             INCLUDE TYPE any_include.");
		buildSrc("    TYPES: END OF ty_s_struc.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	

	@Test
	void testKeepColonInTypesBeginOfBeforeInclude() {
		// expect that the chain colon is NOT removed in the special case of "TYPES: BEGIN OF ..." directly followed by an "INCLUDE TYPE ..." line,
		// because removing the colon here would cause poor layout the next time PrettyPrinter is called - PrettyPrinter would produce:
	   //  TYPES BEGIN OF ty_s_struc.
		//  INCLUDE TYPE any_include.
		//  TYPES: comp TYPE i,
		//         END OF ty_s_struc.

		buildSrc("    TYPES: BEGIN OF ty_s_struc.");
		buildSrc("             INCLUDE TYPE any_include.");
		buildSrc("    TYPES:   comp TYPE i,");
		buildSrc("           END OF ty_s_struc.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}
