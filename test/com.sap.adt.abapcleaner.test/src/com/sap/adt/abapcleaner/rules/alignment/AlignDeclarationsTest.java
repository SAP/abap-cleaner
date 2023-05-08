package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AlignDeclarationsTest extends RuleTestBase {
	private AlignDeclarationsRule rule;
	
	AlignDeclarationsTest() {
		super(RuleID.ALIGN_DECLARATIONS);
		rule = (AlignDeclarationsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configExecuteOnClassDefinitionSections.setValue(true);
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(true);
		rule.configFillPercentageToJustifyOwnColumn.setValue(20);
	}
	
	@Test
	void testConstantsChain() {
		buildSrc("    CONSTANTS: lc_item_list_price_1200 TYPE ty_list_price VALUE 1200,");
		buildSrc("      lc_item_amount_1000 TYPE ty_amount VALUE 1000,");
		buildSrc("               lc_num_process TYPE ty_sequence_number VALUE    1,");
		buildSrc("          lc_final_date TYPE ty_date VALUE '20201012'.");

		buildExp("    CONSTANTS: lc_item_list_price_1200 TYPE ty_list_price      VALUE 1200,");
		buildExp("               lc_item_amount_1000     TYPE ty_amount          VALUE 1000,");
		buildExp("               lc_num_process          TYPE ty_sequence_number VALUE 1,");
		buildExp("               lc_final_date           TYPE ty_date            VALUE '20201012'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testConstantsBlock() {
		buildSrc("    \" the rule aligns subsequent declaration lines just like chains");
		buildSrc("    CONSTANTS lc_item_name_smartphone TYPE ty_item_name VALUE 'SMARTPHONE' ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_subscription TYPE ty_item_name      VALUE 'SUBSCRIPTION'  ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_service TYPE ty_item_name VALUE 'SERVICE' ##NO_TEXT.");

		buildExp("    \" the rule aligns subsequent declaration lines just like chains");
		buildExp("    CONSTANTS lc_item_name_smartphone   TYPE ty_item_name VALUE 'SMARTPHONE' ##NO_TEXT.");
		buildExp("    CONSTANTS lc_item_name_subscription TYPE ty_item_name VALUE 'SUBSCRIPTION'  ##NO_TEXT.");
		buildExp("    CONSTANTS lc_item_name_service      TYPE ty_item_name VALUE 'SERVICE' ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDataChain() {
		buildSrc("    DATA: ls_any_item TYPE ty_item,");
		buildSrc("      lo_any_contract TYPE REF TO ty_contract,");
		buildSrc("          ls_change TYPE ty_change,");
		buildSrc("        lt_table TYPE ty_ts_table.");

		buildExp("    DATA: ls_any_item     TYPE ty_item,");
		buildExp("          lo_any_contract TYPE REF TO ty_contract,");
		buildExp("          ls_change       TYPE ty_change,");
		buildExp("          lt_table        TYPE ty_ts_table.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDataBlockWithNoCommentAlign() {
		buildSrc("    \" if only a single declaration (or a very small ratio of them) has a VALUE, a comment, etc.,");
		buildSrc("    \" no extra column is created for it");
		buildSrc("    DATA lth_any_table            TYPE ty_th_table. \" only line with a comment");
		buildSrc("    DATA lo_contract TYPE REF TO cl_contract  ##NEEDED.");
		buildSrc("    DATA      ls_any_struc  TYPE if_contract=>ty_s_struc.");
		buildSrc("    DATA mv_any_bool_value TYPE abap_bool VALUE abap_false ##NO_TEXT.");

		buildExp("    \" if only a single declaration (or a very small ratio of them) has a VALUE, a comment, etc.,");
		buildExp("    \" no extra column is created for it");
		buildExp("    DATA lth_any_table     TYPE ty_th_table. \" only line with a comment");
		buildExp("    DATA lo_contract       TYPE REF TO cl_contract  ##NEEDED.");
		buildExp("    DATA ls_any_struc      TYPE if_contract=>ty_s_struc.");
		buildExp("    DATA mv_any_bool_value TYPE abap_bool VALUE abap_false ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFieldSymbolsChainWithCommentAlign() {
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("         <fs_item_data> TYPE ty_s_item_data, \" first comment line");
		buildSrc("      <lfs_invoice_amt> LIKE LINE OF its_invoice,");
		buildSrc("    <ls_contract_data> TYPE ty_s_contract, \" second comment line");
		buildSrc("   <ls_param> LIKE LINE OF mt_parameter.");

		buildExp("    FIELD-SYMBOLS:");
		buildExp("      <fs_item_data>     TYPE ty_s_item_data,       \" first comment line");
		buildExp("      <lfs_invoice_amt>  LIKE LINE OF its_invoice,");
		buildExp("      <ls_contract_data> TYPE ty_s_contract,        \" second comment line");
		buildExp("      <ls_param>         LIKE LINE OF mt_parameter.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFieldSymbolsChainWithNoCommentAlign() {
		rule.configFillPercentageToJustifyOwnColumn.setValue(75);
		
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("         <fs_item_data> TYPE ty_s_item_data, \" first comment line");
		buildSrc("      <lfs_invoice_amt> LIKE LINE OF its_invoice,");
		buildSrc("    <ls_contract_data> TYPE ty_s_contract, \" second comment line");
		buildSrc("   <ls_param> LIKE LINE OF mt_parameter.");

		buildExp("    FIELD-SYMBOLS:");
		buildExp("      <fs_item_data>     TYPE ty_s_item_data, \" first comment line");
		buildExp("      <lfs_invoice_amt>  LIKE LINE OF its_invoice,");
		buildExp("      <ls_contract_data> TYPE ty_s_contract, \" second comment line");
		buildExp("      <ls_param>         LIKE LINE OF mt_parameter.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testFieldSymbolsBlock() {
		buildSrc("    FIELD-SYMBOLS <fs_item_data> TYPE ty_s_item_data.");
		buildSrc("    FIELD-SYMBOLS         <lfs_invoice_amt> LIKE LINE OF          its_invoice.");
		buildSrc("    FIELD-SYMBOLS     <ls_contract_data> TYPE ty_s_contract.");

		buildExp("    FIELD-SYMBOLS <fs_item_data>     TYPE ty_s_item_data.");
		buildExp("    FIELD-SYMBOLS <lfs_invoice_amt>  LIKE LINE OF its_invoice.");
		buildExp("    FIELD-SYMBOLS <ls_contract_data> TYPE ty_s_contract.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignAcrossCommentsAndEmptyLines() {
		rule.configAlignAcrossCommentLines.setValue(true);
		rule.configAlignAcrossEmptyLines.setValue(true);

		buildSrc("    \" alignment across comments and empty lines (depending on configuration):");
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    DATA lv_long_variable_name TYPE string.");
		buildSrc("");
		buildSrc("    DATA lts_sorted_table LIKE its_table.");

		buildExp("    \" alignment across comments and empty lines (depending on configuration):");
		buildExp("    DATA lv_value              TYPE i.");
		buildExp("    \" comment");
		buildExp("    DATA lv_long_variable_name TYPE string.");
		buildExp("");
		buildExp("    DATA lts_sorted_table      LIKE its_table.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDontAlignAcrossCommentsAndEmptyLines() {
		rule.configAlignAcrossCommentLines.setValue(false);
		rule.configAlignAcrossEmptyLines.setValue(false);
		
		buildSrc("    \" no alignment across comments and empty lines (depending on configuration):");
		buildSrc("    DATA lv_value TYPE i.");
		buildSrc("    DATA lv_value_2 TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    DATA lv_long_variable_name TYPE string.");
		buildSrc("    DATA lv_long_variable_name_2 TYPE string.");
		buildSrc("");
		buildSrc("    DATA lts_sorted_table LIKE its_table.");
		buildSrc("    DATA lts_sorted_table_2 LIKE its_table.");

		buildExp("    \" no alignment across comments and empty lines (depending on configuration):");
		buildExp("    DATA lv_value   TYPE i.");
		buildExp("    DATA lv_value_2 TYPE i.");
		buildExp("    \" comment");
		buildExp("    DATA lv_long_variable_name   TYPE string.");
		buildExp("    DATA lv_long_variable_name_2 TYPE string.");
		buildExp("");
		buildExp("    DATA lts_sorted_table   LIKE its_table.");
		buildExp("    DATA lts_sorted_table_2 LIKE its_table.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIdentifierWithLengthInParens() {
		buildSrc("    \" identifiers with length information in parentheses:");
		buildSrc("    DATA: lv_textdat1(20) TYPE c,");
		buildSrc("      lv_textdat2(20)  TYPE       c.");

		buildExp("    \" identifiers with length information in parentheses:");
		buildExp("    DATA: lv_textdat1(20) TYPE c,");
		buildExp("          lv_textdat2(20) TYPE c.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTypesBeginOf() {
		buildSrc("    TYPES: BEGIN of xyz,");
		buildSrc("           a TYPE x,");
		buildSrc("           b TYPE y,");
		buildSrc("           c TYPE z,");
		buildSrc("      END OF xyz.");

		buildExp("    TYPES: BEGIN of xyz,");
		buildExp("             a TYPE x,");
		buildExp("             b TYPE y,");
		buildExp("             c TYPE z,");
		buildExp("           END OF xyz.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTypesBeginOfWithComments() {
		buildSrc("    TYPES: BEGIN of xyz,");
		buildSrc("           a TYPE x, \" comment");
		buildSrc("           b TYPE y,");
		buildSrc("           c TYPE z, \" comment");
		buildSrc("      END OF xyz.");

		buildExp("    TYPES: BEGIN of xyz,");
		buildExp("             a TYPE x, \" comment");
		buildExp("             b TYPE y,");
		buildExp("             c TYPE z, \" comment");
		buildExp("           END OF xyz.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTypesBeginOfTypesEndOf() {
		// test an extra TYPES without chain colon directly before END OF 

		buildSrc("    TYPES: BEGIN OF ty_s_struc,");
		buildSrc("    comp TYPE i.");
		buildSrc("    TYPES END OF ty_s_struc.");

		buildExp("    TYPES: BEGIN OF ty_s_struc,");
		buildExp("             comp TYPE i.");
		buildExp("    TYPES  END OF ty_s_struc.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTypesBeginOfIncludeTypesEndOf() {
		// test an INCLUDE, directly followed by TYPES END OF (without chain colon)  
		
		buildSrc("    TYPES: BEGIN OF ty_s_struc,");
		buildSrc("    comp TYPE i.");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES END OF ty_s_struc.");

		buildExp("    TYPES: BEGIN OF ty_s_struc,");
		buildExp("             comp TYPE i.");
		buildExp("             INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES  END OF ty_s_struc.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTypesBeginOfBetweenOtherTypes() {
		// ensure that independent TYPES before and after the structure are aligned separately   
		
		buildSrc("    TYPES ty_any TYPE string.");
		buildSrc("    TYPES ty_other TYPE string.");
		buildSrc("    TYPES: BEGIN OF ty_s_struc,");
		buildSrc("    comp TYPE i.");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES: END OF ty_s_struc.");
		buildSrc("    TYPES ty_any_long TYPE string.");
		buildSrc("    TYPES ty_other_long TYPE string.");

		buildExp("    TYPES ty_any   TYPE string.");
		buildExp("    TYPES ty_other TYPE string.");
		buildExp("    TYPES: BEGIN OF ty_s_struc,");
		buildExp("             comp TYPE i.");
		buildExp("             INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES: END OF ty_s_struc.");
		buildExp("    TYPES ty_any_long   TYPE string.");
		buildExp("    TYPES ty_other_long TYPE string.");

		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testBeginOfWithInclude() {
		buildSrc("    TYPES:");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    component_b_long  TYPE i.");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES:");
		buildSrc("    component_c TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_any_struc,");
		buildExp("        component_a      TYPE i,");
		buildExp("        component_b_long TYPE i.");
		buildExp("        INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES:");
		buildExp("        component_c      TYPE i,");
		buildExp("      END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBeginOfWithIncludeAndComment() {
		// ensure that the comment after the INCLUDE TYPE line does not affect the alignment of the components' TYPE column
		
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    comp_b TYPE string.");
		buildSrc("    INCLUDE TYPE ty_s_include. \" comment");
		buildSrc("    TYPES: END OF ty_s_any_struc.");

		buildExp("    TYPES: BEGIN OF ty_s_any_struc,");
		buildExp("             component_a TYPE i,");
		buildExp("             comp_b      TYPE string.");
		buildExp("             INCLUDE TYPE ty_s_include. \" comment");
		buildExp("    TYPES: END OF ty_s_any_struc.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBeginOfWithIncludeAndCommentLines() {
		buildSrc("    TYPES:");
		buildSrc("    \" comment");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    component_b_long  TYPE i.");
		buildSrc("    \" comment");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES:");
		buildSrc("    \" comment");
		buildSrc("    component_c TYPE i,");
		buildSrc("    \" comment");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      \" comment");
		buildExp("      BEGIN OF ty_s_any_struc,");
		buildExp("        component_a      TYPE i,");
		buildExp("        component_b_long TYPE i.");
		buildExp("        \" comment");
		buildExp("        INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES:");
		buildExp("        \" comment");
		buildExp("        component_c      TYPE i,");
		buildExp("        \" comment");
		buildExp("      END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBeginOf() {
		buildSrc("    TYPES:");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    BEGIN OF ty_s_inner,");
		buildSrc("    comp_1_long TYPE i,");
		buildSrc("    comp_2 TYPE i,");
		buildSrc("    END OF ty_s_inner,");
		buildSrc("    component_b_long  TYPE i,");
		buildSrc("    component_c TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_any_struc,");
		buildExp("        component_a      TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          comp_1_long    TYPE i,");
		buildExp("          comp_2         TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        component_b_long TYPE i,");
		buildExp("        component_c      TYPE i,");
		buildExp("      END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBeginOfWithComments() {
		buildSrc("    TYPES:");
		buildSrc("    \" comment");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("    \" comment");
		buildSrc("    component_a TYPE i,");
		buildSrc("    BEGIN OF ty_s_inner,");
		buildSrc("    comp_1_long TYPE i,");
		buildSrc("    \" comment");
		buildSrc("    comp_2 TYPE i,");
		buildSrc("    END OF ty_s_inner,");
		buildSrc("    component_b_long  TYPE i,");
		buildSrc("    component_c TYPE i,");
		buildSrc("    \" comment");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      \" comment");
		buildExp("      BEGIN OF ty_s_any_struc,");
		buildExp("        \" comment");
		buildExp("        component_a      TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          comp_1_long    TYPE i,");
		buildExp("          \" comment");
		buildExp("          comp_2         TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        component_b_long TYPE i,");
		buildExp("        component_c      TYPE i,");
		buildExp("        \" comment");
		buildExp("      END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBeginOfBehindKeyword() {
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    BEGIN OF ty_s_inner,");
		buildSrc("    comp_1_long TYPE i,");
		buildSrc("    comp_2 TYPE i,");
		buildSrc("    END OF ty_s_inner,");
		buildSrc("    component_b_long  TYPE i,");
		buildSrc("    component_c TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES: BEGIN OF ty_s_any_struc,");
		buildExp("             component_a      TYPE i,");
		buildExp("             BEGIN OF ty_s_inner,");
		buildExp("               comp_1_long    TYPE i,");
		buildExp("               comp_2         TYPE i,");
		buildExp("             END OF ty_s_inner,");
		buildExp("             component_b_long TYPE i,");
		buildExp("             component_c      TYPE i,");
		buildExp("           END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBeginOfBehindKeywordWithComments() {
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i, \" comment A");
		buildSrc("    BEGIN OF ty_s_inner,");
		buildSrc("    comp_1_long TYPE i,");
		buildSrc("    comp_2 TYPE i, \" comment 2");
		buildSrc("    END OF ty_s_inner,");
		buildSrc("    component_b_long  TYPE i, \" comment B");
		buildSrc("    component_c TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES: BEGIN OF ty_s_any_struc,");
		buildExp("             component_a      TYPE i, \" comment A");
		buildExp("             BEGIN OF ty_s_inner,");
		buildExp("               comp_1_long    TYPE i,");
		buildExp("               comp_2         TYPE i, \" comment 2");
		buildExp("             END OF ty_s_inner,");
		buildExp("             component_b_long TYPE i, \" comment B");
		buildExp("             component_c      TYPE i,");
		buildExp("           END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNestedBeginOfWithInclude() {
		buildSrc("    TYPES:");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("    component_a TYPE i,");
		buildSrc("    BEGIN OF ty_s_inner,");
		buildSrc("    comp_1_long TYPE i,");
		buildSrc("    comp_2 TYPE i,");
		buildSrc("    END OF ty_s_inner,");
		buildSrc("    component_b_long  TYPE i.");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES:");
		buildSrc("    component_c_long  TYPE i,");
		buildSrc("    component_d TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_any_struc,");
		buildExp("        component_a      TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          comp_1_long    TYPE i,");
		buildExp("          comp_2         TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        component_b_long TYPE i.");
		buildExp("        INCLUDE TYPE ty_s_include.");
		buildExp("    TYPES:");
		buildExp("        component_c_long TYPE i,");
		buildExp("        component_d      TYPE i,");
		buildExp("      END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testDataBeginOf() {
		// example taken from https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennested_structure_abexa.htm
		
		buildSrc("    DATA:");
		buildSrc("    BEGIN OF address,");
		buildSrc("    BEGIN OF name,");
		buildSrc("    title TYPE string VALUE `Mr.`,");
		buildSrc("    prename TYPE string VALUE `Duncan`,");
		buildSrc("    surname TYPE string VALUE `Pea`,");
		buildSrc("    END OF name,");
		buildSrc("    BEGIN OF street,");
		buildSrc("    name TYPE string VALUE `Vegetable Lane`,");
		buildSrc("    number TYPE string VALUE `11`,");
		buildSrc("    END OF street,");
		buildSrc("    BEGIN OF city,");
		buildSrc("    zipcode TYPE string VALUE `349875`,");
		buildSrc("    name TYPE string VALUE `Botanica`,");
		buildSrc("    END OF city,");
		buildSrc("    END OF address.");

		buildExp("    DATA:");
		buildExp("      BEGIN OF address,");
		buildExp("        BEGIN OF name,");
		buildExp("          title   TYPE string VALUE `Mr.`,");
		buildExp("          prename TYPE string VALUE `Duncan`,");
		buildExp("          surname TYPE string VALUE `Pea`,");
		buildExp("        END OF name,");
		buildExp("        BEGIN OF street,");
		buildExp("          name    TYPE string VALUE `Vegetable Lane`,");
		buildExp("          number  TYPE string VALUE `11`,");
		buildExp("        END OF street,");
		buildExp("        BEGIN OF city,");
		buildExp("          zipcode TYPE string VALUE `349875`,");
		buildExp("          name    TYPE string VALUE `Botanica`,");
		buildExp("        END OF city,");
		buildExp("      END OF address.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDataBeginOfNested() {
		// example taken from https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennested_structure_abexa.htm
		
		buildSrc("    DATA:");
		buildSrc("    BEGIN OF address,");
		buildSrc("    BEGIN OF name,");
		buildSrc("    title TYPE string VALUE `Mr.`,");
		buildSrc("    prename TYPE string VALUE `Duncan`,");
		buildSrc("    surname TYPE string VALUE `Pea`,");
		buildSrc("    END OF name,");
		buildSrc("    BEGIN OF street,");
		buildSrc("    name TYPE string VALUE `Vegetable Lane`,");
		buildSrc("    number TYPE string VALUE `11`,");
		buildSrc("    END OF street,");
		buildSrc("    BEGIN OF city,");
		buildSrc("    zipcode TYPE string VALUE `349875`,");
		buildSrc("    name TYPE string VALUE `Botanica`,");
		buildSrc("    END OF city,");
		buildSrc("    END OF address.");

		buildExp("    DATA:");
		buildExp("      BEGIN OF address,");
		buildExp("        BEGIN OF name,");
		buildExp("          title   TYPE string VALUE `Mr.`,");
		buildExp("          prename TYPE string VALUE `Duncan`,");
		buildExp("          surname TYPE string VALUE `Pea`,");
		buildExp("        END OF name,");
		buildExp("        BEGIN OF street,");
		buildExp("          name    TYPE string VALUE `Vegetable Lane`,");
		buildExp("          number  TYPE string VALUE `11`,");
		buildExp("        END OF street,");
		buildExp("        BEGIN OF city,");
		buildExp("          zipcode TYPE string VALUE `349875`,");
		buildExp("          name    TYPE string VALUE `Botanica`,");
		buildExp("        END OF city,");
		buildExp("      END OF address.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTableDeclarationWithKey() {
		// ensure that table declarations with "WITH ... KEY ..." sections are not aligned, because they usually should not be put on a single line
		
		buildSrc("    DATA:");
		buildSrc("      lt_item     TYPE ty_tt_item,");
		buildSrc("      lts_buffer      TYPE SORTED TABLE OF ty_s_buffer");
		buildSrc("                            WITH NON-UNIQUE KEY primary_key,");
		buildSrc("      lv_index         TYPE i.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainSignAttachedToVarName() {
		buildSrc("    \" consider that there may be no space after the : colon:");
		buildSrc("    CONSTANTS:lc_any TYPE if_any_interface=>ty_any_type VALUE '101',");
		buildSrc("              lc_other TYPE ty_any_quantity VALUE '%' ##NO_TEXT.");
		buildSrc("");
		buildSrc("    DATA:lv_msgv_item_id                TYPE sy-msgv1,");
		buildSrc("         lv_long_variable_name      TYPE sy-tabix VALUE 1.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS:<ls_any> TYPE ty_s_any,");
		buildSrc("                  <ls_other> TYPE ty_s_other.");

		buildExp("    \" consider that there may be no space after the : colon:");
		buildExp("    CONSTANTS: lc_any   TYPE if_any_interface=>ty_any_type VALUE '101',");
		buildExp("               lc_other TYPE ty_any_quantity               VALUE '%' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA: lv_msgv_item_id       TYPE sy-msgv1,");
		buildExp("          lv_long_variable_name TYPE sy-tabix VALUE 1.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS: <ls_any>   TYPE ty_s_any,");
		buildExp("                   <ls_other> TYPE ty_s_other.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAlignClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(true);
		
		buildSrc("    CONSTANTS lc_item_name_smartphone TYPE ty_item_name VALUE 'SMARTPHONE' ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_subscription TYPE ty_item_name      VALUE 'SUBSCRIPTION'  ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_service TYPE ty_item_name VALUE 'SERVICE' ##NO_TEXT.");

		buildExp("    CONSTANTS lc_item_name_smartphone   TYPE ty_item_name VALUE 'SMARTPHONE' ##NO_TEXT.");
		buildExp("    CONSTANTS lc_item_name_subscription TYPE ty_item_name VALUE 'SUBSCRIPTION'  ##NO_TEXT.");
		buildExp("    CONSTANTS lc_item_name_service      TYPE ty_item_name VALUE 'SERVICE' ##NO_TEXT.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontAlignClassDefinition() {
		rule.configExecuteOnClassDefinitionSections.setValue(false);
		
		buildSrc("    CONSTANTS lc_item_name_smartphone TYPE ty_item_name VALUE 'SMARTPHONE' ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_subscription TYPE ty_item_name      VALUE 'SUBSCRIPTION'  ##NO_TEXT.");
		buildSrc("    CONSTANTS lc_item_name_service TYPE ty_item_name VALUE 'SERVICE' ##NO_TEXT.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataWithLengthDecimalsReadOnly() {
		buildSrc("    DATA price TYPE p LENGTH 8 DECIMALS 2 VALUE '1.99' READ-ONLY.");
		buildSrc("    DATA duration TYPE p LENGTH 5 DECIMALS 1 VALUE '57.3' READ-ONLY.");
		buildSrc("    DATA info TYPE c LENGTH 250 VALUE 'text' READ-ONLY.");
		buildSrc("    DATA tag TYPE p LENGTH 12 DECIMALS 10 VALUE '8.012345789' READ-ONLY.");

		buildExp("    DATA price    TYPE p LENGTH 8   DECIMALS 2  VALUE '1.99'        READ-ONLY.");
		buildExp("    DATA duration TYPE p LENGTH 5   DECIMALS 1  VALUE '57.3'        READ-ONLY.");
		buildExp("    DATA info     TYPE c LENGTH 250             VALUE 'text'        READ-ONLY.");
		buildExp("    DATA tag      TYPE p LENGTH 12  DECIMALS 10 VALUE '8.012345789' READ-ONLY.");
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDataWithDefaultType() {
		// ensure that alignment works across a declaration that uses the default type c

		buildSrc("  DATA lv_offset TYPE i.");
		buildSrc("  DATA lv_text(100).");
		buildSrc("  DATA lv_substring TYPE string.");

		
		buildExp("  DATA lv_offset    TYPE i.");
		buildExp("  DATA lv_text(100).");
		buildExp("  DATA lv_substring TYPE string.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testDataChainWithDefaultType() {
		// ensure that alignment works across a declaration that uses the default type c

		buildSrc("  DATA: lv_offset TYPE i,");
		buildSrc("  lv_text(100),");
		buildSrc("   lv_substring TYPE string.");

		buildExp("  DATA: lv_offset    TYPE i,");
		buildExp("        lv_text(100),");
		buildExp("        lv_substring TYPE string.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testTypesWithSpecialCompNames() {
		// ensure that component names which could be mistaken for a keyword (such as 'data' and 'types') are aligned correctly
		
		buildSrc("    TYPES: BEGIN OF ty_structure,");
		buildSrc("             component TYPE i,");
		buildSrc("             data TYPE i,");
		buildSrc("             types TYPE i,");
		buildSrc("             methods TYPE i,");
		buildSrc("           END OF ty_structure.");

		buildExp("    TYPES: BEGIN OF ty_structure,");
		buildExp("             component TYPE i,");
		buildExp("             data      TYPE i,");
		buildExp("             types     TYPE i,");
		buildExp("             methods   TYPE i,");
		buildExp("           END OF ty_structure.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableWithShortKeyDefiniton() {
		buildSrc("  METHOD any_method.");
		buildSrc("    DATA lv_any_num TYPE i.");
		buildSrc("    DATA lt_any_table TYPE STANDARD TABLE OF any_data_element WITH NON-UNIQUE DEFAULT KEY.");
		buildSrc("    DATA lt_other_table TYPE STANDARD TABLE OF any_data_element WITH EMPTY KEY.");
		buildSrc("    DATA lv_other_value TYPE string.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("    DATA lv_any_num     TYPE i.");
		buildExp("    DATA lt_any_table   TYPE STANDARD TABLE OF any_data_element WITH NON-UNIQUE DEFAULT KEY.");
		buildExp("    DATA lt_other_table TYPE STANDARD TABLE OF any_data_element WITH EMPTY KEY.");
		buildExp("    DATA lv_other_value TYPE string.");
		buildExp("  ENDMETHOD.");

		testRule();
	}
}