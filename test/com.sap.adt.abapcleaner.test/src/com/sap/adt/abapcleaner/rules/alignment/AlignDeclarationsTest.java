package com.sap.adt.abapcleaner.rules.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		rule.configExecuteOnClassDefAndInterfaces.setValue(true);
		rule.configAlignChainAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);

		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);
		rule.configAlignAcrossEmptyLines.setValue(true);
		rule.configAlignAcrossCommentLines.setValue(true);

		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);
		rule.configStructureAlignStyle.setEnumValue(StructureAlignStyle.PER_LEVEL);

		rule.configFillPercentageToJustifyOwnColumn.setValue(20);
		rule.configCondenseInnerSpaces.setValue(true);
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
		buildExp("    DATA lo_contract       TYPE REF TO cl_contract ##NEEDED.");
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
		buildExp("          comp_1_long TYPE i,");
		buildExp("          comp_2      TYPE i,");
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
		buildExp("          comp_1_long TYPE i,");
		buildExp("          \" comment");
		buildExp("          comp_2      TYPE i,");
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
		buildExp("               comp_1_long TYPE i,");
		buildExp("               comp_2      TYPE i,");
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
		buildExp("               comp_1_long TYPE i,");
		buildExp("               comp_2      TYPE i, \" comment 2");
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
		buildExp("          comp_1_long TYPE i,");
		buildExp("          comp_2      TYPE i,");
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
		buildExp("          name   TYPE string VALUE `Vegetable Lane`,");
		buildExp("          number TYPE string VALUE `11`,");
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
		buildExp("          name   TYPE string VALUE `Vegetable Lane`,");
		buildExp("          number TYPE string VALUE `11`,");
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
	void testTableDeclarationWithKeyOf3Components() {
		// ensure that in table declarations with more than one component in the "WITH" section, 
		// the "WITH ..." part is kept unchanged, but everything else is aligned
		
		buildSrc("    DATA:");
		buildSrc("      lt_item     TYPE ty_tt_item,");
		buildSrc("      lts_buffer      TYPE SORTED TABLE OF ty_s_buffer");
		buildSrc("                            WITH NON-UNIQUE KEY comp1 comp2 comp3,");
		buildSrc("      lv_index         TYPE i.");

		buildExp("    DATA:");
		buildExp("      lt_item    TYPE ty_tt_item,");
		buildExp("      lts_buffer TYPE SORTED TABLE OF ty_s_buffer");
		buildExp("                            WITH NON-UNIQUE KEY comp1 comp2 comp3,");
		buildExp("      lv_index   TYPE i.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTableDeclarationWithMultiKey() {
		// ensure that in table declarations with multiple "WITH ... KEY ..." sections, the "WITH ..." part is kept unchanged, 
		// but everything else is aligned
		
		buildSrc("    DATA:");
		buildSrc("      lt_item     TYPE ty_tt_item,");
		buildSrc("      lts_buffer      TYPE SORTED TABLE OF ty_s_buffer");
		buildSrc("                    WITH NON-UNIQUE KEY comp1 comp2 comp3");
		buildSrc("                    WITH UNIQUE KEY key_name COMPONENTS comp1 comp2");
		buildSrc("                                                        comp3 comp4,");
		buildSrc("      lv_index         TYPE i.");

		buildExp("    DATA:");
		buildExp("      lt_item    TYPE ty_tt_item,");
		buildExp("      lts_buffer TYPE SORTED TABLE OF ty_s_buffer");
		buildExp("                    WITH NON-UNIQUE KEY comp1 comp2 comp3");
		buildExp("                    WITH UNIQUE KEY key_name COMPONENTS comp1 comp2");
		buildExp("                                                        comp3 comp4,");
		buildExp("      lv_index   TYPE i.");
		
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
		rule.configExecuteOnClassDefAndInterfaces.setValue(true);
		
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
		rule.configExecuteOnClassDefAndInterfaces.setValue(false);
		
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

	@Test
	void testTypesColonIncludeTypes() {
		buildSrc("  TYPES:");
		buildSrc("    BEGIN OF ty_s_any_struc.");
		buildSrc("        INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES component TYPE i.");
		buildSrc("    TYPES END OF ty_s_any_struc.");

		buildExp("  TYPES: BEGIN OF ty_s_any_struc.");
		buildExp("           INCLUDE TYPE ty_s_include.");
		buildExp("  TYPES    component TYPE i.");
		buildExp("  TYPES  END OF ty_s_any_struc.");

		testRule();
	}

	@Test
	void testCommentAfterTypesMovedUp() {
		buildSrc("  TYPES:");
		buildSrc("      \"! documentation");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("        component TYPE string.");
		buildSrc("        INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES END OF ty_s_any_struc.");

		buildExp("  TYPES: \"! documentation");
		buildExp("         BEGIN OF ty_s_any_struc,");
		buildExp("           component TYPE string.");
		buildExp("           INCLUDE TYPE ty_s_include.");
		buildExp("  TYPES  END OF ty_s_any_struc.");

		testRule();
	}

	@Test
	void testAsteriskCommentAfterTypesKept() {
		buildSrc("  TYPES:");
		buildSrc("* comment");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("        component TYPE string.");
		buildSrc("        INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES END OF ty_s_any_struc.");

		buildExp("  TYPES:");
		buildExp("* comment");
		buildExp("         BEGIN OF ty_s_any_struc,");
		buildExp("           component TYPE string.");
		buildExp("           INCLUDE TYPE ty_s_include.");
		buildExp("  TYPES  END OF ty_s_any_struc.");

		testRule();
	}

	@Test
	void testTypesColonTypesInclude() {
		buildSrc("  TYPES:");
		buildSrc("    BEGIN OF ty_s_struc.");
		buildSrc("    TYPES component TYPE string.");
		buildSrc("    INCLUDE TYPE ty_s_include.");
		buildSrc("    TYPES END OF ty_s_struc .");

		buildExp("  TYPES: BEGIN OF ty_s_struc.");
		buildExp("  TYPES    component TYPE string.");
		buildExp("           INCLUDE TYPE ty_s_include.");
		buildExp("  TYPES  END OF ty_s_struc .");

		testRule();
	}

	@Test
	void testLongNameAfterEndOfStructure() {
		// ensure that the alignment of TYPE inside the BEGIN OF ... END OF section 
		// is NOT influenced by the long identifier after END OF
		
		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_struc,");
		buildSrc("      comp1 TYPE string,");
		buildSrc("      component2 TYPE string,");
		buildSrc("      END OF ty_s_struc,");
		buildSrc("      ty_tt_table_with_long_name TYPE TABLE OF ty_s_struc.");
		buildSrc("");
		buildSrc("    DATA mv_any TYPE i.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_struc,");
		buildExp("        comp1      TYPE string,");
		buildExp("        component2 TYPE string,");
		buildExp("      END OF ty_s_struc,");
		buildExp("      ty_tt_table_with_long_name TYPE TABLE OF ty_s_struc.");
		buildExp("");
		buildExp("    DATA mv_any TYPE i.");

		testRule();
	}

	@Test
	void testAlignAllInnerStructures() {
		rule.configStructureAlignStyle.setEnumValue(StructureAlignStyle.ACROSS_LEVELS);

		// expect everything within one structure to be aligned, including TYPE sections of enclosed structures 

		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_outer,");
		buildSrc("      one TYPE i,");
		buildSrc("      two TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      a1 TYPE i,");
		buildSrc("      b2 TYPE i,");
		buildSrc("      c3 TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      three TYPE i,");
		buildSrc("      four TYPE i,");
		buildSrc("      BEGIN OF ty_s_another_inner,");
		buildSrc("      long_component_name TYPE i,");
		buildSrc("      very_long_component_name TYPE i,");
		buildSrc("      END OF ty_s_another_inner,");
		buildSrc("      seventeen TYPE i,");
		buildSrc("      eighteen TYPE i,");
		buildSrc("      END OF ty_s_outer,");
		buildSrc("      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,");
		buildSrc("");
		buildSrc("      BEGIN OF ty_s_outer_2,");
		buildSrc("      any_component TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      alpha TYPE i,");
		buildSrc("      beta TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      other_component TYPE i,");
		buildSrc("      END OF ty_s_outer_2.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_outer,");
		buildExp("        one                        TYPE i,");
		buildExp("        two                        TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          a1                       TYPE i,");
		buildExp("          b2                       TYPE i,");
		buildExp("          c3                       TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        three                      TYPE i,");
		buildExp("        four                       TYPE i,");
		buildExp("        BEGIN OF ty_s_another_inner,");
		buildExp("          long_component_name      TYPE i,");
		buildExp("          very_long_component_name TYPE i,");
		buildExp("        END OF ty_s_another_inner,");
		buildExp("        seventeen                  TYPE i,");
		buildExp("        eighteen                   TYPE i,");
		buildExp("      END OF ty_s_outer,");
		buildExp("      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,");
		buildExp("");
		buildExp("      BEGIN OF ty_s_outer_2,");
		buildExp("        any_component   TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          alpha         TYPE i,");
		buildExp("          beta          TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        other_component TYPE i,");
		buildExp("      END OF ty_s_outer_2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignPerSection() {
		rule.configStructureAlignStyle.setEnumValue(StructureAlignStyle.PER_SECTION);

		// expect TYPE sections of components 'three' and 'four' to be aligned with each other, 
		// but NOT with the TYPE sections of 'one' and 'two', or 'seventeen' and 'eighteen'
		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_outer,");
		buildSrc("      one TYPE i,");
		buildSrc("      two TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      a1 TYPE i,");
		buildSrc("      b2 TYPE i,");
		buildSrc("      c3 TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      three TYPE i,");
		buildSrc("      four TYPE i,");
		buildSrc("      BEGIN OF ty_s_another_inner,");
		buildSrc("      long_component_name TYPE i,");
		buildSrc("      very_long_component_name TYPE i,");
		buildSrc("      END OF ty_s_another_inner,");
		buildSrc("      seventeen TYPE i,");
		buildSrc("      eighteen TYPE i,");
		buildSrc("      END OF ty_s_outer,");
		buildSrc("      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,");
		buildSrc("");
		buildSrc("      BEGIN OF ty_s_outer_2,");
		buildSrc("      any_component TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      alpha TYPE i,");
		buildSrc("      beta TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      other_component TYPE i,");
		buildSrc("      END OF ty_s_outer_2.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_outer,");
		buildExp("        one TYPE i,");
		buildExp("        two TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          a1 TYPE i,");
		buildExp("          b2 TYPE i,");
		buildExp("          c3 TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        three TYPE i,");
		buildExp("        four  TYPE i,");
		buildExp("        BEGIN OF ty_s_another_inner,");
		buildExp("          long_component_name      TYPE i,");
		buildExp("          very_long_component_name TYPE i,");
		buildExp("        END OF ty_s_another_inner,");
		buildExp("        seventeen TYPE i,");
		buildExp("        eighteen  TYPE i,");
		buildExp("      END OF ty_s_outer,");
		buildExp("      ty_tt_outer TYPE STANDARD TABLE OF ty_s_outer WITH DEFAULT KEY,");
		buildExp("");
		buildExp("      BEGIN OF ty_s_outer_2,");
		buildExp("        any_component TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          alpha TYPE i,");
		buildExp("          beta  TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        other_component TYPE i,");
		buildExp("      END OF ty_s_outer_2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinLengthIntoType() {
		// expect the LENGTH column to follow directly after "TYPE c" and "TYPE p" (so it is partly below "string", not right of it)
		
		buildSrc("   TYPES: BEGIN OF ty_s_struc,");
		buildSrc("          any_comp TYPE string,");
		buildSrc("          other_comp TYPE string,");
		buildSrc("           c1 TYPE c     LENGTH 10,");
		buildSrc("          p1 TYPE p   LENGTH 8 DECIMALS 0,");
		buildSrc("          third_comp    TYPE i,");
		buildSrc("             p2 TYPE p LENGTH 6 DECIMALS 0,");
		buildSrc("          c2 TYPE c LENGTH 9,");
		buildSrc("        c3 TYPE c LENGTH 1,");
		buildSrc("           c4 TYPE c LENGTH 4,");
		buildSrc("           c5 TYPE c LENGTH 40,");
		buildSrc("            END OF ty_s_struc.");

		buildExp("   TYPES: BEGIN OF ty_s_struc,");
		buildExp("            any_comp   TYPE string,");
		buildExp("            other_comp TYPE string,");
		buildExp("            c1         TYPE c LENGTH 10,");
		buildExp("            p1         TYPE p LENGTH 8  DECIMALS 0,");
		buildExp("            third_comp TYPE i,");
		buildExp("            p2         TYPE p LENGTH 6  DECIMALS 0,");
		buildExp("            c2         TYPE c LENGTH 9,");
		buildExp("            c3         TYPE c LENGTH 1,");
		buildExp("            c4         TYPE c LENGTH 4,");
		buildExp("            c5         TYPE c LENGTH 40,");
		buildExp("          END OF ty_s_struc.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testJoinValueColumn() {
		// expect the VALUE to be kept directly behind "TYPE i" instead of getting an own column
		// (for which AlignColumn.cellCount must be correctly updated when the comment column is joined into the VALUE column)
		
		buildSrc("    DATA:");
		buildSrc("      ls_any_struc TYPE ty_s_struc,");
		buildSrc("      lv_any_value TYPE i,");
		buildSrc("      lv_other_value TYPE i VALUE 1000000. \" comment");

		buildExp("    DATA:");
		buildExp("      ls_any_struc   TYPE ty_s_struc,");
		buildExp("      lv_any_value   TYPE i,");
		buildExp("      lv_other_value TYPE i VALUE 1000000. \" comment");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCreateValueColumnButJoinCommentColumn() {
		// expect VALUE to get a distinct column, but expect the comment to be joined to the VALUE column
		
		buildSrc("    DATA:");
		buildSrc("      ls_any_struc TYPE ty_s_struc,");
		buildSrc("      lv_any_value TYPE i VALUE 10, \" comment");
		buildSrc("      lv_other_value TYPE i VALUE 1000000.");

		buildExp("    DATA:");
		buildExp("      ls_any_struc   TYPE ty_s_struc,");
		buildExp("      lv_any_value   TYPE i          VALUE 10, \" comment");
		buildExp("      lv_other_value TYPE i          VALUE 1000000.");

		testRule();
	}

	private void assertConfigAlignActionsEnabled() {
		assertTrue(rule.isConfigValueEnabled(rule.configAlignChainAction));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignNonChainsAction));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignStructureAction));
	}
	
	@Test
	void testAlignAcrossEnabled() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAcrossEmptyLines));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAcrossCommentLines));
		assertConfigAlignActionsEnabled();
		
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAcrossEmptyLines));
		assertTrue(rule.isConfigValueEnabled(rule.configAlignAcrossCommentLines));
		assertConfigAlignActionsEnabled();
	}

	@Test
	void testAlignAcrossNotEnabled() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		assertFalse(rule.isConfigValueEnabled(rule.configAlignAcrossEmptyLines));
		assertFalse(rule.isConfigValueEnabled(rule.configAlignAcrossCommentLines));
		assertConfigAlignActionsEnabled();
	}

	@Test
	void testAlignStructureActionEnabled() {
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);
		assertTrue(rule.isConfigValueEnabled(rule.configStructureAlignStyle));
		assertConfigAlignActionsEnabled();

		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);
		assertTrue(rule.isConfigValueEnabled(rule.configStructureAlignStyle));
		assertConfigAlignActionsEnabled();
	}

	@Test
	void testAlignStructureActionNotEnabled() {
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		assertFalse(rule.isConfigValueEnabled(rule.configStructureAlignStyle));
		assertConfigAlignActionsEnabled();
	}

	@Test
	void testFillPercentageNotEnabled() {
		rule.configAlignChainAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		assertFalse(rule.isConfigValueEnabled(rule.configFillPercentageToJustifyOwnColumn));

		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_TYPE_LENGTH_ETC);
		assertTrue(rule.isConfigValueEnabled(rule.configFillPercentageToJustifyOwnColumn));
	}

	@Test
	void testChainAlignNameAndTypeOnly() {
		rule.configAlignChainAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);

		buildSrc("    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price TYPE ty_price VALUE 1000,");
		buildSrc("               lc_num_contract_change TYPE ty_sequence_number VALUE    1,");
		buildSrc("          lc_start_date TYPE ty_start_date VALUE '20220312'.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("         <ls_data> TYPE ty_s_data,   \" first comment line");
		buildSrc("      <ls_amount> LIKE LINE OF its_amount,");
		buildSrc("    <ls_contract> TYPE ty_s_contract, \" second comment line");
		buildSrc("   <ls_param>  LIKE LINE OF mt_parameter.");

		buildExp("    CONSTANTS: lc_any_price           TYPE ty_price VALUE 1200,");
		buildExp("               lc_other_price         TYPE ty_price VALUE 1000,");
		buildExp("               lc_num_contract_change TYPE ty_sequence_number VALUE 1,");
		buildExp("               lc_start_date          TYPE ty_start_date VALUE '20220312'.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS:");
		buildExp("      <ls_data>     TYPE ty_s_data, \" first comment line");
		buildExp("      <ls_amount>   LIKE LINE OF its_amount,");
		buildExp("      <ls_contract> TYPE ty_s_contract, \" second comment line");
		buildExp("      <ls_param>    LIKE LINE OF mt_parameter.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainAlignNameOnly() {
		rule.configAlignChainAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);

		buildSrc("    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price TYPE ty_price VALUE 1000,");
		buildSrc("               lc_num_contract_change TYPE ty_sequence_number VALUE    1,");
		buildSrc("          lc_start_date TYPE ty_start_date VALUE '20220312'.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS:");
		buildSrc("         <ls_data> TYPE ty_s_data,   \" first comment line");
		buildSrc("      <ls_amount> LIKE LINE OF its_amount,");
		buildSrc("    <ls_contract> TYPE ty_s_contract, \" second comment line");
		buildSrc("   <ls_param>  LIKE LINE OF mt_parameter.");

		buildExp("    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200,");
		buildExp("               lc_other_price TYPE ty_price VALUE 1000,");
		buildExp("               lc_num_contract_change TYPE ty_sequence_number VALUE 1,");
		buildExp("               lc_start_date TYPE ty_start_date VALUE '20220312'.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS:");
		buildExp("      <ls_data> TYPE ty_s_data, \" first comment line");
		buildExp("      <ls_amount> LIKE LINE OF its_amount,");
		buildExp("      <ls_contract> TYPE ty_s_contract, \" second comment line");
		buildExp("      <ls_param> LIKE LINE OF mt_parameter.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBlockAlignNameAndTypeOnly() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);

		buildSrc("    CONSTANTS   lc_pi TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.");
		buildSrc("    CONSTANTS lc_e TYPE p LENGTH 8 DECIMALS 10 VALUE '2.718281828'.");
		buildSrc("    CONSTANTS lc_sqrt_2 TYPE p LENGTH 8 DECIMALS 4 VALUE '1.4142'.");
		buildSrc("    CONSTANTS    lc_sqrt_32 TYPE p LENGTH 8 DECIMALS 10 VALUE '5.6568542495'.");
		buildSrc("    CONSTANTS lc_ln_10 TYPE p  LENGTH 8 DECIMALS 4 VALUE '2.3026'.");

		buildExp("    CONSTANTS lc_pi      TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.");
		buildExp("    CONSTANTS lc_e       TYPE p LENGTH 8 DECIMALS 10 VALUE '2.718281828'.");
		buildExp("    CONSTANTS lc_sqrt_2  TYPE p LENGTH 8 DECIMALS 4 VALUE '1.4142'.");
		buildExp("    CONSTANTS lc_sqrt_32 TYPE p LENGTH 8 DECIMALS 10 VALUE '5.6568542495'.");
		buildExp("    CONSTANTS lc_ln_10   TYPE p LENGTH 8 DECIMALS 4 VALUE '2.3026'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAlignNameOnly() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);

		buildSrc("    CONSTANTS   lc_pi TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.");
		buildSrc("    CONSTANTS lc_e TYPE p LENGTH 8 DECIMALS 10 VALUE '2.718281828'.");
		buildSrc("    CONSTANTS lc_sqrt_2 TYPE p LENGTH 8 DECIMALS 4 VALUE '1.4142'.");
		buildSrc("    CONSTANTS    lc_sqrt_32 TYPE p LENGTH 8 DECIMALS 10 VALUE '5.6568542495'.");
		buildSrc("    CONSTANTS lc_ln_10 TYPE p  LENGTH 8 DECIMALS 4 VALUE '2.3026'.");

		buildExp("    CONSTANTS lc_pi TYPE p LENGTH 10 DECIMALS 10 VALUE '3.1415926536'.");
		buildExp("    CONSTANTS lc_e TYPE p LENGTH 8 DECIMALS 10 VALUE '2.718281828'.");
		buildExp("    CONSTANTS lc_sqrt_2 TYPE p LENGTH 8 DECIMALS 4 VALUE '1.4142'.");
		buildExp("    CONSTANTS lc_sqrt_32 TYPE p LENGTH 8 DECIMALS 10 VALUE '5.6568542495'.");
		buildExp("    CONSTANTS lc_ln_10 TYPE p LENGTH 8 DECIMALS 4 VALUE '2.3026'.");

		testRule();
	}

	@Test
	void testStructureAlignNameAndTypeOnly() {
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);

		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_outer,");
		buildSrc("      one TYPE i,");
		buildSrc("      two TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      alpha TYPE p LENGTH 5 DECIMALS 2,");
		buildSrc("      beta TYPE p LENGTH 10 DECIMALS 2,");
		buildSrc("      gamma TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      three  TYPE i,");
		buildSrc("      four   TYPE i,");
		buildSrc("      END OF ty_s_outer.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_outer,");
		buildExp("        one   TYPE i,");
		buildExp("        two   TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          alpha TYPE p LENGTH 5 DECIMALS 2,");
		buildExp("          beta  TYPE p LENGTH 10 DECIMALS 2,");
		buildExp("          gamma TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        three TYPE i,");
		buildExp("        four  TYPE i,");
		buildExp("      END OF ty_s_outer.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStructureAlignNameOnly() {
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);

		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_outer,");
		buildSrc("      one TYPE i,");
		buildSrc("      two TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      alpha TYPE p LENGTH 5 DECIMALS 2,");
		buildSrc("      beta TYPE p LENGTH 10 DECIMALS 2,");
		buildSrc("      gamma TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      three  TYPE i,");
		buildSrc("      four   TYPE i,");
		buildSrc("      END OF ty_s_outer.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_outer,");
		buildExp("        one TYPE i,");
		buildExp("        two TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          alpha TYPE p LENGTH 5 DECIMALS 2,");
		buildExp("          beta TYPE p LENGTH 10 DECIMALS 2,");
		buildExp("          gamma TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        three TYPE i,");
		buildExp("        four TYPE i,");
		buildExp("      END OF ty_s_outer.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainNoCondense() {
		rule.configAlignChainAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);
		rule.configCondenseInnerSpaces.setValue(false);

		buildSrc("    CONSTANTS: lc_any_price TYPE ty_price VALUE 1200,");
		buildSrc("      lc_other_price TYPE ty_price   VALUE 1000,");
		buildSrc("               lc_num_contract_change TYPE ty_sequence_number VALUE    1,");
		buildSrc("          lc_start_date TYPE ty_start_date VALUE '20220312'.");

		buildExp("    CONSTANTS: lc_any_price           TYPE ty_price VALUE 1200,");
		buildExp("               lc_other_price         TYPE ty_price   VALUE 1000,");
		buildExp("               lc_num_contract_change TYPE ty_sequence_number VALUE    1,");
		buildExp("               lc_start_date          TYPE ty_start_date VALUE '20220312'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testBlockNoCondense() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_AND_TYPE);
		rule.configCondenseInnerSpaces.setValue(false);

		buildSrc("    DATA lth_any_table TYPE ty_th_hash_table_type.   \" only line with a comment");
		buildSrc("    DATA lo_contract TYPE REF TO  cl_contract  ##NEEDED.");
		buildSrc("    DATA ls_item_data   TYPE   if_any_interface=>ty_s_item.");
		buildSrc("    DATA lv_was_saved   TYPE abap_bool  VALUE abap_false  ##NO_TEXT.");

		buildExp("    DATA lth_any_table TYPE ty_th_hash_table_type.   \" only line with a comment");
		buildExp("    DATA lo_contract   TYPE REF TO  cl_contract  ##NEEDED.");
		buildExp("    DATA ls_item_data  TYPE   if_any_interface=>ty_s_item.");
		buildExp("    DATA lv_was_saved  TYPE abap_bool  VALUE abap_false  ##NO_TEXT.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testStructureNoCondense() {
		rule.configAlignStructureAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		rule.configCondenseInnerSpaces.setValue(false);

		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF ty_s_outer_2,");
		buildSrc("      any_component TYPE i,");
		buildSrc("      BEGIN OF ty_s_inner,");
		buildSrc("      alpha  TYPE i,");
		buildSrc("      beta   TYPE i,");
		buildSrc("      END OF ty_s_inner,");
		buildSrc("      other_component TYPE i,");
		buildSrc("      END OF ty_s_outer_2.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF ty_s_outer_2,");
		buildExp("        any_component TYPE i,");
		buildExp("        BEGIN OF ty_s_inner,");
		buildExp("          alpha  TYPE i,");
		buildExp("          beta   TYPE i,");
		buildExp("        END OF ty_s_inner,");
		buildExp("        other_component TYPE i,");
		buildExp("      END OF ty_s_outer_2.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepOverlengthValuesBehindType() {
		rule.configMaxLineLength.setValue(140);

		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type   VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name  TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name  TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		testRule();
	}

	@Test
	void testMoveOverlengthValuesBelowType() {
		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type");
		buildExp("                                             VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name  TYPE if_any_interface=>ty_other_type");
		buildExp("                                             VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name  TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		testRule();
	}

	@Test
	void testMoveOverlengthValuesBelowNameOrType() {
		rule.configMaxLineLength.setValue(90);

		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type");
		buildExp("              VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name  TYPE if_any_interface=>ty_other_type");
		buildExp("              VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name  TYPE if_any_interface=>ty_third_type");
		buildExp("                                             VALUE if_any_interface=>co_third_value.");

		testRule();
	}

	@Test
	void testMoveOverlengthValuesBehindType() {
		rule.configMaxLineLength.setValue(140);

		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type");
		buildSrc("                                             VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name  TYPE if_any_interface=>ty_other_type");
		buildSrc("                                             VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name  TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type   VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name  TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name  TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		testRule();
	}

	@Test
	void testMoveOverlengthValuesBelowTypeAlignNameOnly() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);

		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type");
		buildExp("                                             VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type");
		buildExp("                                            VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		testRule();
	}

	@Test
	void testMoveOverlengthValuesBelowNameOrTypeAlignNameOnly() {
		rule.configAlignNonChainsAction.setEnumValue(AlignDeclarationsAction.ALIGN_NAME_ONLY);
		rule.configMaxLineLength.setValue(90);

		buildSrc("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type VALUE if_any_interface=>co_any_value_with_long_name.");
		buildSrc("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type VALUE if_any_interface=>co_other_value_with_long_name.");
		buildSrc("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type VALUE if_any_interface=>co_third_value.");

		buildExp("    CONSTANTS lc_any_constant_with_long_name TYPE if_any_interface=>ty_any_type");
		buildExp("              VALUE if_any_interface=>co_any_value_with_long_name.");
		buildExp("    CONSTANTS lc_other_const_with_long_name TYPE if_any_interface=>ty_other_type");
		buildExp("              VALUE if_any_interface=>co_other_value_with_long_name.");
		buildExp("    CONSTANTS lc_third_const_with_long_name TYPE if_any_interface=>ty_third_type");
		buildExp("                                            VALUE if_any_interface=>co_third_value.");

		testRule();
	}

}