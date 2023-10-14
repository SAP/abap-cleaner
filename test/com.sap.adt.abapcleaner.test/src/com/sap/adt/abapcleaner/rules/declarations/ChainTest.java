package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class ChainTest extends RuleTestBase {
	private ChainRule rule;
	
	ChainTest() {
		super(RuleID.DECLARATION_CHAIN);
		rule = (ChainRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnInterfaces.setValue(true);
		rule.configExecuteOnClassDefinitionSections.setValue(true);
		rule.configExecuteOnLocalDeclarations.setValue(true);
		rule.configExecuteOnSimpleCommands.setValue(false);
		rule.configExecuteOnComplexCommands.setValue(true);
	}
	
	@Test
	void testSimpleChain() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    CONSTANTS:");
		buildSrc("      lc_any_price_1200 TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price_1000 TYPE ty_price VALUE 1000,");
		buildSrc("      lc_num_change TYPE ty_sequence_number VALUE 1,");
		buildSrc("      lc_final_date TYPE ty_finalization_date VALUE '20220312'.");

		buildExp("    CONSTANTS lc_any_price_1200 TYPE ty_price VALUE 1200.");
		buildExp("    CONSTANTS lc_other_price_1000 TYPE ty_price VALUE 1000.");
		buildExp("    CONSTANTS lc_num_change TYPE ty_sequence_number VALUE 1.");
		buildExp("    CONSTANTS lc_final_date TYPE ty_finalization_date VALUE '20220312'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSkipLocalDeclarations() {
		rule.configExecuteOnLocalDeclarations.setValue(false);

		buildSrc("    CONSTANTS:");
		buildSrc("      lc_any_price_1200 TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price_1000 TYPE ty_price VALUE 1000.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentBeforeChainColon() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    CONSTANTS \" comment");
		buildSrc("    : lc_any_price_1200 TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price_1000 TYPE ty_price VALUE 1000.");

		buildExp("    \" comment");
		buildExp("    CONSTANTS lc_any_price_1200 TYPE ty_price VALUE 1200.");
		buildExp("    CONSTANTS lc_other_price_1000 TYPE ty_price VALUE 1000.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testInnerAlignmentNotChanged() {
		// expect that alignment within the declaration line is not changed and empty lines are kept

		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    DATA: lth_table TYPE ty_any_table_type, \" comment");
		buildSrc("          lo_item TYPE REF TO cl_item  ##NEEDED,");
		buildSrc("");
		buildSrc("          ls_struc TYPE if_any_interface=>ty_s_struc,");
		buildSrc("          mv_any_flag TYPE abap_bool VALUE abap_false ##NO_TEXT,");
		buildSrc("          lts_other_table TYPE SORTED TABLE OF ty_s_table_structure");
		buildSrc("                                       WITH NON-UNIQUE KEY primary_key");
		buildSrc("                                       COMPONENTS item_id");
		buildSrc("                                                  category");
		buildSrc("                                                  any_flag.");

		buildExp("    DATA lth_table TYPE ty_any_table_type. \" comment");
		buildExp("    DATA lo_item TYPE REF TO cl_item  ##NEEDED.");
		buildExp("");
		buildExp("    DATA ls_struc TYPE if_any_interface=>ty_s_struc.");
		buildExp("    DATA mv_any_flag TYPE abap_bool VALUE abap_false ##NO_TEXT.");
		buildExp("    DATA lts_other_table TYPE SORTED TABLE OF ty_s_table_structure");
		buildExp("                                      WITH NON-UNIQUE KEY primary_key");
		buildExp("                                      COMPONENTS item_id");
		buildExp("                                                 category");
		buildExp("                                                 any_flag.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentsAboveAndWithin() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    FIELD-SYMBOLS: \" comment above the first identifier");
		buildSrc("      <ls_data>      TYPE ty_s_data,");
		buildSrc("      <ls_amount>    LIKE LINE OF its_amount, \" comment");
		buildSrc("");
		buildSrc("      \" comment line within the chain");
		buildSrc("      <ls_item_data> TYPE ty_s_item_data,");
		buildSrc("      <ls_param>     LIKE LINE OF mt_parameter.");

		buildExp("    \" comment above the first identifier");
		buildExp("    FIELD-SYMBOLS <ls_data>      TYPE ty_s_data.");
		buildExp("    FIELD-SYMBOLS <ls_amount>    LIKE LINE OF its_amount. \" comment");
		buildExp("");
		buildExp("    \" comment line within the chain");
		buildExp("    FIELD-SYMBOLS <ls_item_data> TYPE ty_s_item_data.");
		buildExp("    FIELD-SYMBOLS <ls_param>     LIKE LINE OF mt_parameter.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testStructureDeclarationUnchanged() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    \" BEGIN OF ... END OF blocks are kept as a chain");
		buildSrc("    TYPES:");
		buildSrc("      ty_ts_xyz TYPE SORTED TABLE OF ty_s_xyz,");
		buildSrc("      BEGIN of xyz,");
		buildSrc("        a TYPE x,");
		buildSrc("        b TYPE y,");
		buildSrc("        c TYPE z,");
		buildSrc("      END OF xyz.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMultiPartStructureDeclarationUnchanged() {
		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ts_data.");
		buildSrc("        INCLUDE STRUCTURE t000.");
		buildSrc("    TYPES:");
		buildSrc("        a TYPE i,");
		buildSrc("        b TYPE i,");
		buildSrc("        c TYPE i,");
		buildSrc("      END OF ts_data.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testClassDefinition() {
		rule.configExecuteOnLocalDeclarations.setValue(false);

		buildSrc("    CONSTANTS:");
		buildSrc("      lc_any_price_1200 TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price_1000 TYPE ty_price VALUE 1000.");
		buildSrc("");
		buildSrc("    CLASS-DATA: lth_table TYPE ty_any_table_type,");
		buildSrc("                lo_item TYPE REF TO cl_item  ##NEEDED.");
		buildSrc("");
		buildSrc("    METHODS: setup,");
		buildSrc("      get_value");
		buildSrc("        EXPORTING ev_value TYPE i,");
		buildSrc("      set_value");
		buildSrc("        IMPORTING iv_value TYPE i.");

		buildExp("    CONSTANTS lc_any_price_1200 TYPE ty_price VALUE 1200.");
		buildExp("    CONSTANTS lc_other_price_1000 TYPE ty_price VALUE 1000.");
		buildExp("");
		buildExp("    CLASS-DATA lth_table TYPE ty_any_table_type.");
		buildExp("    CLASS-DATA lo_item TYPE REF TO cl_item  ##NEEDED.");
		buildExp("");
		buildExp("    METHODS setup.");
		buildExp("    METHODS get_value");
		buildExp("              EXPORTING ev_value TYPE i.");
		buildExp("    METHODS set_value");
		buildExp("              IMPORTING iv_value TYPE i.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSkipClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);

		buildSrc("    CONSTANTS:");
		buildSrc("      co_any_price_1200 TYPE ty_price VALUE 1200,");
		buildSrc("      co_other_price_1000 TYPE ty_price VALUE 1000,");
		buildSrc("      co_num_change TYPE ty_sequence_number VALUE 1,");
		buildSrc("      co_final_date TYPE ty_finalization_date VALUE '20220312'.");
		buildSrc("    DATA: mth_table TYPE ty_any_table_type, \" comment");
		buildSrc("          mo_item TYPE REF TO cl_item  ##NEEDED.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMultipleChainColons() {
		// expect further chain colons to be removed in the result (because in the initial command, the compiler would ignore them)
		buildSrc("    DATA: lv_num : TYPE ::i, lv_value TYPE:string.");

		buildExp("    DATA lv_num TYPE i.");
		buildExp("    DATA lv_value TYPE string.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSimpleCommands() {
		// expect WRITE: to be kept with chain colon

		rule.configExecuteOnClassDefinitionSections.setValue(false);
		rule.configExecuteOnLocalDeclarations.setValue(false);
		rule.configExecuteOnSimpleCommands.setValue(true);
		rule.configExecuteOnComplexCommands.setValue(false);

		buildSrc("    ASSERT: a = 1,");
		buildSrc("            b = `text`.");
		buildSrc("");
		buildSrc("    CLEAR: ev_result,ev_result_index,et_table.");
		buildSrc("");
		buildSrc("    WRITE: `text`, lv_value, `more text`.");

		buildExp("    ASSERT a = 1.");
		buildExp("    ASSERT b = `text`.");
		buildExp("");
		buildExp("    CLEAR ev_result.");
		buildExp("    CLEAR ev_result_index.");
		buildExp("    CLEAR et_table.");
		buildExp("");
		buildExp("    WRITE: `text`, lv_value, `more text`.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSkipSimpleCommands() {
		rule.configExecuteOnSimpleCommands.setValue(false);

		buildSrc("    ASSERT: a = 1,");
		buildSrc("            b = `text`.");
		buildSrc("    CLEAR: ev_result,ev_result_index,et_table.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testComplexCommands() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);
		rule.configExecuteOnLocalDeclarations.setValue(false);
		rule.configExecuteOnSimpleCommands.setValue(false);

		buildSrc("    CALL METHOD: \" comment");
		buildSrc("      any_method( ), \" any comment");
		buildSrc("      other_method( ). \" other comment");
		buildSrc("");
		buildSrc("    x += 2 ** : 1, 2, 3.");
		buildSrc("");
		buildSrc("    y *= 2:,,.");

		buildExp("    \" comment");
		buildExp("    CALL METHOD any_method( ). \" any comment");
		buildExp("    CALL METHOD other_method( ). \" other comment");
		buildExp("");
		buildExp("    x += 2 ** 1.");
		buildExp("    x += 2 ** 2.");
		buildExp("    x += 2 ** 3.");
		buildExp("");
		buildExp("    y *= 2.");
		buildExp("    y *= 2.");
		buildExp("    y *= 2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSkipComplexCommands() {
		rule.configExecuteOnComplexCommands.setValue(false);

		buildSrc("    x += 2 ** : 1, 2, 3.");
		buildSrc("");
		buildSrc("    y *= 2:,,.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testInterface() {
		buildSrc("INTERFACE if_unchaining.");
		buildSrc("  CONSTANTS: any_constant TYPE i VALUE 1,");
		buildSrc("             other_constant TYPE i VALUE 2.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    any_method,");
		buildSrc("    other_method");
		buildSrc("      IMPORTING iv_any_parameter TYPE i.");
		buildSrc("ENDINTERFACE.");

		buildExp("INTERFACE if_unchaining.");
		buildExp("  CONSTANTS any_constant TYPE i VALUE 1.");
		buildExp("  CONSTANTS other_constant TYPE i VALUE 2.");
		buildExp("");
		buildExp("  METHODS any_method.");
		buildExp("  METHODS other_method");
		buildExp("            IMPORTING iv_any_parameter TYPE i.");
		buildExp("ENDINTERFACE.");

		testRule();
	}

	@Test
	void testInterfaceUnchanged() {
		rule.configExecuteOnInterfaces.setValue(false);
		
		buildSrc("INTERFACE if_unchaining.");
		buildSrc("  CONSTANTS: any_constant TYPE i VALUE 1,");
		buildSrc("             other_constant TYPE i VALUE 2.");
		buildSrc("");
		buildSrc("  METHODS:");
		buildSrc("    any_method,");
		buildSrc("    other_method");
		buildSrc("      IMPORTING iv_any_parameter TYPE i.");
		buildSrc("ENDINTERFACE.");

		copyExpFromSrc();

		testRule();
	}
	
	@Test
	void testDeleteIndexColonComment() {
		buildSrc("  DELETE itab INDEX \" comment");
		buildSrc("                   : 1, 2.");

		buildExp("  DELETE itab INDEX \" comment");
		buildExp("                       1.");
		buildExp("  DELETE itab INDEX \" comment");
		buildExp("                       2.");

		testRule();
	}

}
