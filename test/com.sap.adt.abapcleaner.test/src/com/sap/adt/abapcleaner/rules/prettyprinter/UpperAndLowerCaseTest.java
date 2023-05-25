package com.sap.adt.abapcleaner.rules.prettyprinter;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class UpperAndLowerCaseTest extends RuleTestBase {
	private UpperAndLowerCaseRule rule;
	
	UpperAndLowerCaseTest() {
		super(RuleID.UPPER_AND_LOWER_CASE);
		rule = (UpperAndLowerCaseRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.NONE);
		
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);
		
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);
		
		rule.configPragmaStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configPragmaParameterStyle.setEnumValue(CaseStyle.UPPER_CASE);
		
		rule.configKeepMixedCaseInIdentifiers.setValue(true);
	}
	
	@Test
	void testInterfaceDefinition() {
		buildSrc("*\"* components of interface IF_ANY_INTERFACE");
		buildSrc("\"! <p class=\"shorttext synchronized\" lang=\"en\">Any Description</p>");
		buildSrc("interface if_pretty_print_case_and_indent \"#EC NUMBER_METHODS");
		buildSrc("  public.");
		buildSrc("");
		buildSrc("  interfaces IF_ANY_INTERFACE.");
		buildSrc("  interfaces IF_OTHER_INTERFACE.");
		buildSrc("");
		buildSrc("  \"! <p class=\"shorttext synchronized\" lang=\"en\">Data Description</p>");
		buildSrc("  data MS_DATA type TY_S_DATA read-only.");
		buildSrc("  \"! <p class=\"shorttext synchronized\" lang=\"en\">Constant Description</p>");
		buildSrc("  constants CO_INDICATOR type CHAR1 value 'C' ##NO_TEXT.");
		buildSrc("");
		buildSrc("  methods ANY_METHOD");
		buildSrc("    importing");
		buildSrc("      !ITS_TABLE type IF_ANY_INTERFACE=>TY_TS_TABLE_TYPE.");
		buildSrc("");
		buildSrc("  methods OTHER_METHOD");
		buildSrc("    returning");
		buildSrc("      value(RV_FLAG) type ABAP_BOOL.");
		buildSrc("endinterface.");

		buildExp("*\"* components of interface IF_ANY_INTERFACE");
		buildExp("\"! <p class=\"shorttext synchronized\" lang=\"en\">Any Description</p>");
		buildExp("INTERFACE if_pretty_print_case_and_indent \"#EC NUMBER_METHODS");
		buildExp("  PUBLIC.");
		buildExp("");
		buildExp("  INTERFACES if_any_interface.");
		buildExp("  INTERFACES if_other_interface.");
		buildExp("");
		buildExp("  \"! <p class=\"shorttext synchronized\" lang=\"en\">Data Description</p>");
		buildExp("  DATA ms_data TYPE ty_s_data READ-ONLY.");
		buildExp("  \"! <p class=\"shorttext synchronized\" lang=\"en\">Constant Description</p>");
		buildExp("  CONSTANTS co_indicator TYPE char1 VALUE 'C' ##NO_TEXT.");
		buildExp("");
		buildExp("  METHODS any_method");
		buildExp("    IMPORTING");
		buildExp("      !its_table TYPE if_any_interface=>ty_ts_table_type.");
		buildExp("");
		buildExp("  METHODS other_method");
		buildExp("    RETURNING");
		buildExp("      VALUE(rv_flag) TYPE abap_bool.");
		buildExp("ENDINTERFACE.");

		testRule();
	}

	@Test
	void testMethodWithDeclAndCommands() {
		buildSrc("  method pretty_print_case_1.");
		buildSrc("    constants lc_text value 'abcde' ##no_text.");
		buildSrc("");
		buildSrc("    data lv_counter type i.");
		buildSrc("    data(lo_inline) = get_object( ) ##needed.");
		buildSrc("");
		buildSrc("    if iv_count = abap_true and io_inline is not bound.");
		buildSrc("      loop at mts_data assigning field-symbol(<ls_data>).");
		buildSrc("        lv_counter += 1.");
		buildSrc("      endloop.");
		buildSrc("    endif.");
		buildSrc("  endmethod.");

		buildExp("  METHOD pretty_print_case_1.");
		buildExp("    CONSTANTS lc_text VALUE 'abcde' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA lv_counter TYPE i.");
		buildExp("    DATA(lo_inline) = get_object( ) ##NEEDED.");
		buildExp("");
		buildExp("    IF iv_count = abap_true AND io_inline IS NOT BOUND.");
		buildExp("      LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildExp("        lv_counter += 1.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDIF.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testClearStatement() {
		buildSrc("    clear: ev_value with '0' in character mode, key with null, result.");

		buildExp("    CLEAR: ev_value WITH '0' IN CHARACTER MODE, key WITH NULL, result.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSortStatement() {
		buildSrc("    sort mt_table by date ascending.");
		buildSrc("    sort lt_data by fiscal_year ascending");
		buildSrc("                    period      descending");
		buildSrc("                    item_count  descending.");

		buildExp("    SORT mt_table BY date ASCENDING.");
		buildExp("    SORT lt_data BY fiscal_year ASCENDING");
		buildExp("                    period      DESCENDING");
		buildExp("                    item_count  DESCENDING.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testSortStatementFromUpper() {
		buildSrc("    SORT MT_TABLE BY DATE ASCENDING.");
		buildSrc("    SORT LT_DATA BY FISCAL_YEAR ASCENDING");
		buildSrc("                    PERIOD      DESCENDING");
		buildSrc("                    ITEM_COUNT  DESCENDING.");

		buildExp("    SORT mt_table BY date ASCENDING.");
		buildExp("    SORT lt_data BY fiscal_year ASCENDING");
		buildExp("                    period      DESCENDING");
		buildExp("                    item_count  DESCENDING.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testDataDeclarationFromUpper() {
		// test whether identifiers and types can be told from their position, although they all look like ABAP keywords
		
		buildSrc("    DATA:");
		buildSrc("      LOOP TYPE TYPE LENGTH 10,");
		buildSrc("      FOR TYPE LINE OF TYPE,");
		buildSrc("      DO TYPE REF TO TYPE,");
		buildSrc("      ENDFOR TYPE TABLE OF TYPE WITH DEFAULT KEY,");
		buildSrc("      ENDDO TYPE SORTED TABLE OF REF TO TYPE,");
		buildSrc("      WHILE TYPE RANGE OF TYPE INITIAL SIZE 4,");
		buildSrc("      ENDWHILE LIKE TYPE,");
		buildSrc("      ENDLOOP LIKE LINE OF TYPE,");
		buildSrc("      IF LIKE REF TO TYPE,");
		buildSrc("      ENDIF LIKE HASHED TABLE OF REF TO TYPE WITH EMPTY KEY.");

		buildExp("    DATA:");
		buildExp("      loop TYPE type LENGTH 10,");
		buildExp("      for TYPE LINE OF type,");
		buildExp("      do TYPE REF TO type,");
		buildExp("      endfor TYPE TABLE OF type WITH DEFAULT KEY,");
		buildExp("      enddo TYPE SORTED TABLE OF REF TO type,");
		buildExp("      while TYPE RANGE OF type INITIAL SIZE 4,");
		buildExp("      endwhile LIKE type,");
		buildExp("      endloop LIKE LINE OF type,");
		buildExp("      if LIKE REF TO type,");
		buildExp("      endif LIKE HASHED TABLE OF REF TO type WITH EMPTY KEY.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIdentifierNamedResult() {
		buildSrc("    \" 'result' must be lower case except in very specific statements:");
		buildSrc("    TYPES ty_action_export_parameter TYPE TABLE FOR ACTION RESULT CDS_entity_name~action_name.");
		buildSrc("    CALL TRANSFORMATION trans OPTIONS any_options SOURCE XML src_xml RESULT XML rslt_xml.");
		buildSrc("    FETCH NEXT CURSOR dbcur INTO wa EXTENDED RESULT @oref.");
		buildSrc("    DATA(result) = abap_true.");
		buildSrc("    result = xsdbool( result = abap_false OR result = abap_true ).");

		buildExp("    \" 'result' must be lower case except in very specific statements:");
		buildExp("    TYPES ty_action_export_parameter TYPE TABLE FOR ACTION RESULT CDS_entity_name~action_name.");
		buildExp("    CALL TRANSFORMATION trans OPTIONS any_options SOURCE XML src_xml RESULT XML rslt_xml.");
		buildExp("    FETCH NEXT CURSOR dbcur INTO wa EXTENDED RESULT @oref.");
		buildExp("    DATA(result) = abap_true.");
		buildExp("    result = xsdbool( result = abap_false OR result = abap_true ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIdentifierNamedBetween() {
		buildSrc("    \" 'between' must remain in lower case here:");
		buildSrc("    constants:");
		buildSrc("      begin of cos_sel_opt_option,");
		buildSrc("        equal       type apj_option value 'EQ',");
		buildSrc("        between     type apj_option value 'BT',");
		buildSrc("        not_between type apj_option value 'NB',");
		buildSrc("      end of cos_sel_opt_option.");

		buildExp("    \" 'between' must remain in lower case here:");
		buildExp("    CONSTANTS:");
		buildExp("      BEGIN OF cos_sel_opt_option,");
		buildExp("        equal       TYPE apj_option VALUE 'EQ',");
		buildExp("        between     TYPE apj_option VALUE 'BT',");
		buildExp("        not_between TYPE apj_option VALUE 'NB',");
		buildExp("      END OF cos_sel_opt_option.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testExceptionsOthers() {
		buildSrc("    \" OTHERS must be upper case:");
		buildSrc("    call function 'OBJECT_KEY_GET'");
		buildSrc("      EXPORTING");
		buildSrc("        i_objnr           = <ls_item>-objnr");
		buildSrc("*     IMPORTING");
		buildSrc("*       E_OBJECTKEY       =");
		buildSrc("      tables");
		buildSrc("        onr_tab           = lt_table");
		buildSrc("      exceptions");
		buildSrc("        wrong_call        = 1");
		buildSrc("        objectkey_invalid = 2");
		buildSrc("        others            = 3.");

		buildExp("    \" OTHERS must be upper case:");
		buildExp("    CALL FUNCTION 'OBJECT_KEY_GET'");
		buildExp("      EXPORTING");
		buildExp("        i_objnr           = <ls_item>-objnr");
		buildExp("*     IMPORTING");
		buildExp("*       E_OBJECTKEY       =");
		buildExp("      TABLES");
		buildExp("        onr_tab           = lt_table");
		buildExp("      EXCEPTIONS");
		buildExp("        wrong_call        = 1");
		buildExp("        objectkey_invalid = 2");
		buildExp("        OTHERS            = 3.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testKeywordHighAndLow() {
		buildSrc("    \" HIGH and LOW must be upper case here:");
		buildSrc("    set run time clock resolution high.");
		buildSrc("    set run time clock resolution low.");

		buildExp("    \" HIGH and LOW must be upper case here:");
		buildExp("    SET RUN TIME CLOCK RESOLUTION HIGH.");
		buildExp("    SET RUN TIME CLOCK RESOLUTION LOW.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCamelCase() {
		buildSrc("    data lvMixedCaseIdentifier type i.");
		buildSrc("");
		buildSrc("    lv_counter = lvMixedCaseIdentifier.");
		buildSrc("");
		buildSrc("    call badi lo_badi_instance->any_method exporting AnyParameter              = ls_data");
		buildSrc("                                           changing  OtherParameterInCamelCase = lv_value.");

		buildExp("    DATA lvMixedCaseIdentifier TYPE i.");
		buildExp("");
		buildExp("    lv_counter = lvMixedCaseIdentifier.");
		buildExp("");
		buildExp("    CALL BADI lo_badi_instance->any_method EXPORTING AnyParameter              = ls_data");
		buildExp("                                           CHANGING  OtherParameterInCamelCase = lv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCamelCaseFromUpper() {
		buildSrc("    CALL BADI LO_BADI_INSTANCE->ANY_METHOD");
		buildSrc("      EXPORTING");
		buildSrc("        AnyParameter              = LS_DATA");
		buildSrc("      CHANGING");
		buildSrc("        OtherParameterInCamelCase = LV_VALUE.");

		buildExp("    CALL BADI lo_badi_instance->any_method");
		buildExp("      EXPORTING");
		buildExp("        AnyParameter              = ls_data");
		buildExp("      CHANGING");
		buildExp("        OtherParameterInCamelCase = lv_value.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIdentifiersNamedLikeKeywords() {
		// test tricky cases, i.e. component or field names that match ABAP identifiers
		
		buildSrc("    data lr_range type ref to data.");
		buildSrc("    types ty_ts_table type sorted table of ty_s_struc with unique key item_id period");
		buildSrc("                                                      with non-unique sorted key key components a b c.");
		buildSrc("    loop at mts_table using key key where a = 1.");
		buildSrc("    endloop.");
		buildSrc("");
		buildSrc("    delete adjacent duplicates from mt_item_range comparing low.");

		buildExp("    DATA lr_range TYPE REF TO data.");
		buildExp("    TYPES ty_ts_table TYPE SORTED TABLE OF ty_s_struc WITH UNIQUE KEY item_id period");
		buildExp("                                                      WITH NON-UNIQUE SORTED KEY key COMPONENTS a b c.");
		buildExp("    LOOP AT mts_table USING KEY key WHERE a = 1.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("    DELETE ADJACENT DUPLICATES FROM mt_item_range COMPARING low.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testIdentifiersNamedLikeKeywordsFromUpper() {
		// test tricky cases, i.e. component or field names that match ABAP identifiers
		
		buildSrc("    DATA LR_RANGE TYPE REF TO DATA.");
		buildSrc("    TYPES TY_TS_TABLE TYPE SORTED TABLE OF TY_S_STRUC WITH UNIQUE KEY ITEM_ID PERIOD");
		buildSrc("                                                      WITH NON-UNIQUE SORTED KEY KEY COMPONENTS A B C.");
		buildSrc("    LOOP AT MTS_TABLE USING KEY KEY WHERE A = 1.");
		buildSrc("    ENDLOOP.");
		buildSrc("");
		buildSrc("    DELETE ADJACENT DUPLICATES FROM MT_ITEM_RANGE COMPARING LOW.");

		buildExp("    DATA lr_range TYPE REF TO data.");
		buildExp("    TYPES ty_ts_table TYPE SORTED TABLE OF ty_s_struc WITH UNIQUE KEY item_id period");
		buildExp("                                                      WITH NON-UNIQUE SORTED KEY key COMPONENTS a b c.");
		buildExp("    LOOP AT mts_table USING KEY key WHERE a = 1.");
		buildExp("    ENDLOOP.");
		buildExp("");
		buildExp("    DELETE ADJACENT DUPLICATES FROM mt_item_range COMPARING low.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testIdentifiersNamedXYZ() {
		// expect 'x', 'y', 'z' to be put to lower case, although they are also ABAP keywords
		
		buildSrc("    TYPES:");
		buildSrc("      BEGIN OF XYZ,");
		buildSrc("        A TYPE X,");
		buildSrc("        B TYPE Y,");
		buildSrc("        C TYPE Z,");
		buildSrc("      END OF XYZ.");

		buildExp("    TYPES:");
		buildExp("      BEGIN OF xyz,");
		buildExp("        a TYPE x,");
		buildExp("        b TYPE y,");
		buildExp("        c TYPE z,");
		buildExp("      END OF xyz.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testMethodAllUpper() {
		buildSrc("    CONSTANTS LC_TEXT VALUE 'TEXT' ##NO_TEXT.");
		buildSrc("");
		buildSrc("    DATA LV_COUNTER            TYPE I.");
		buildSrc("    DATA lvMixedCaseIdentifier TYPE i.");
		buildSrc("    DATA(LO_INLINE) = GET_OBJECT( ) ##NEEDED.");
		buildSrc("");
		buildSrc("    CLEAR: EV_VALUE WITH '0' IN CHARACTER MODE, KEY WITH NULL, RESULT.");
		buildSrc("");
		buildSrc("    LV_COUNTER = lvMixedCaseIdentifier.");
		buildSrc("    IF IV_COUNT = ABAP_TRUE AND IO_INLINE IS NOT BOUND.");
		buildSrc("      LOOP AT MTS_DATA ASSIGNING FIELD-SYMBOL(<LS_DATA>).");
		buildSrc("        LV_COUNTER += 1.");
		buildSrc("      ENDLOOP.");
		buildSrc("    ENDIF.");

		buildExp("    CONSTANTS lc_text VALUE 'TEXT' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA lv_counter            TYPE i.");
		buildExp("    DATA lvMixedCaseIdentifier TYPE i.");
		buildExp("    DATA(lo_inline) = get_object( ) ##NEEDED.");
		buildExp("");
		buildExp("    CLEAR: ev_value WITH '0' IN CHARACTER MODE, key WITH NULL, result.");
		buildExp("");
		buildExp("    lv_counter = lvMixedCaseIdentifier.");
		buildExp("    IF iv_count = abap_true AND io_inline IS NOT BOUND.");
		buildExp("      LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildExp("        lv_counter += 1.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDIF.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	// -------------------------------------------------------------------------
	// test configuration settings for class definition sections
	
	@Test
	void testClassDefinitionAdtStyle() {
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES IF_ANY_INTERFACE.");
		buildSrc("  protected section.");
		buildSrc("    data MO_ITEM type ref to IF_ITEM.");
		buildSrc("  private section.");
		buildSrc("    types ty_ts_xyz type sorted table of xyz with unique key primary_key components a b with further secondary keys.");
		buildSrc("    data mo_message_handler type ref to if_message_handler.");
		buildSrc("    class-methods create");
		buildSrc("      returning");
		buildSrc("        value(ro_instance) type ref to if_any_interface.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_item TYPE REF TO if_item.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    TYPES ty_ts_xyz TYPE SORTED TABLE OF xyz WITH UNIQUE KEY primary_key COMPONENTS a b WITH FURTHER SECONDARY KEYS.");
		buildExp("    DATA mo_message_handler TYPE REF TO if_message_handler.");
		buildExp("    CLASS-METHODS create");
		buildExp("      RETURNING");
		buildExp("        VALUE(ro_instance) TYPE REF TO if_any_interface.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testClassDefinitionSapGuiStyle() {
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.UPPER_CASE);
		
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES IF_ANY_INTERFACE.");
		buildSrc("  protected section.");
		buildSrc("    data MO_ITEM type ref to IF_ITEM.");
		buildSrc("  private section.");
		buildSrc("    types ty_ts_xyz type sorted table of xyz with unique key primary_key components a b with further secondary keys.");
		buildSrc("    data mo_message_handler type ref to if_message_handler.");
		buildSrc("    class-methods create");
		buildSrc("      returning");
		buildSrc("        value(ro_instance) type ref to if_any_interface.");
		buildSrc("ENDCLASS.");

		buildExp("class CL_ANY_CLASS definition public create protected.");
		buildExp("  public section.");
		buildExp("    interfaces IF_ANY_INTERFACE.");
		buildExp("  protected section.");
		buildExp("    data MO_ITEM type ref to IF_ITEM.");
		buildExp("  private section.");
		buildExp("    types TY_TS_XYZ type sorted table of XYZ with unique key PRIMARY_KEY components A B with further secondary keys.");
		buildExp("    data MO_MESSAGE_HANDLER type ref to IF_MESSAGE_HANDLER.");
		buildExp("    class-methods CREATE");
		buildExp("      returning");
		buildExp("        value(RO_INSTANCE) type ref to IF_ANY_INTERFACE.");
		buildExp("endclass.");

		testRule();
	}

	@Test
	void testClassDefinitionUnchanged() {
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.UNCHANGED);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.UNCHANGED);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES IF_ANY_INTERFACE.");
		buildSrc("  protected section.");
		buildSrc("    data MO_ITEM type ref to IF_ITEM.");
		buildSrc("  private section.");
		buildSrc("    types ty_ts_xyz type sorted table of xyz with unique key primary_key components a b with further secondary keys.");
		buildSrc("    data mo_message_handler type ref to if_message_handler.");
		buildSrc("    class-methods create");
		buildSrc("      returning");
		buildSrc("        value(ro_instance) type ref to if_any_interface.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testClassDefinitionKeepMixed() {
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configKeepMixedCaseInIdentifiers.setValue(true);

		buildSrc("CLASS cl_Any_Class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  protected section.");
		buildSrc("    data mo_Item type ref to if_Item.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_Any_Class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_Item TYPE REF TO if_Item.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testClassDefinitionChangeMixed() {
		rule.configDefinitionKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configDefinitionIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configKeepMixedCaseInIdentifiers.setValue(false);

		buildSrc("CLASS cl_Any_Class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  protected section.");
		buildSrc("    data mo_Item type ref to if_Item.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_item TYPE REF TO if_item.");
		buildExp("ENDCLASS.");

		testRule();
	}

	// -------------------------------------------------------------------------
	// test configuration settings for class implementation sections
	
	@Test
	void testImplementationKeywordUpperIdentifierLowerPragmaUpper() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configPragmaStyle.setEnumValue(CaseStyle.UPPER_CASE);

		buildSrc("  method pretty_print_case_1.");
		buildSrc("    constants lc_text value 'TEXT' ##no_text.");
		buildSrc("");
		buildSrc("    data lv_counter type i.");
		buildSrc("    data(lo_inline) = get_object( ) ##NEEDED.");
		buildSrc("");
		buildSrc("    if iv_count = abap_true and io_inline is not bound.");
		buildSrc("      loop at mts_data assigning field-symbol(<ls_data>).");
		buildSrc("        lv_counter += 1.");
		buildSrc("      endloop.");
		buildSrc("    endif.");
		buildSrc("  endmethod.");

		buildExp("  METHOD pretty_print_case_1.");
		buildExp("    CONSTANTS lc_text VALUE 'TEXT' ##NO_TEXT.");
		buildExp("");
		buildExp("    DATA lv_counter TYPE i.");
		buildExp("    DATA(lo_inline) = get_object( ) ##NEEDED.");
		buildExp("");
		buildExp("    IF iv_count = abap_true AND io_inline IS NOT BOUND.");
		buildExp("      LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>).");
		buildExp("        lv_counter += 1.");
		buildExp("      ENDLOOP.");
		buildExp("    ENDIF.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testImplementationKeywordLowerIdentifierUpperPragmaLower() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configPragmaStyle.setEnumValue(CaseStyle.LOWER_CASE);

		buildSrc("  method pretty_print_case_1.");
		buildSrc("    constants lc_text value 'text' ##no_text.");
		buildSrc("");
		buildSrc("    data lv_counter type i.");
		buildSrc("    data(lo_inline) = get_object( ) ##needed.");
		buildSrc("");
		buildSrc("    if iv_count = abap_true and io_inline is not bound.");
		buildSrc("      loop at mts_data assigning field-symbol(<ls_data>).");
		buildSrc("        lv_counter += 1.");
		buildSrc("      endloop.");
		buildSrc("    endif.");
		buildSrc("  endmethod.");

		buildExp("  method PRETTY_PRINT_CASE_1.");
		buildExp("    constants LC_TEXT value 'text' ##no_text.");
		buildExp("");
		buildExp("    data LV_COUNTER type I.");
		buildExp("    data(LO_INLINE) = GET_OBJECT( ) ##needed.");
		buildExp("");
		buildExp("    if IV_COUNT = ABAP_TRUE and IO_INLINE is not bound.");
		buildExp("      loop at MTS_DATA assigning field-symbol(<LS_DATA>).");
		buildExp("        LV_COUNTER += 1.");
		buildExp("      endloop.");
		buildExp("    endif.");
		buildExp("  endmethod.");

		testRule();
	}

	@Test
	void testImplementationUnchanged() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.UNCHANGED);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.UNCHANGED);
		rule.configPragmaStyle.setEnumValue(CaseStyle.UNCHANGED);
		rule.configPragmaParameterStyle.setEnumValue(CaseStyle.UNCHANGED);

		buildSrc("  method pretty_print_case_1.");
		buildSrc("    constants lc_text value 'text' ##no_text.");
		buildSrc("");
		buildSrc("    data lv_counter type i.");
		buildSrc("    data(lo_inline) = get_object( ) ##NEEDED.");
		buildSrc("");
		buildSrc("    if iv_count = abap_true and io_inline is not bound.");
		buildSrc("      loop at mts_data assigning field-symbol(<ls_data>).");
		buildSrc("        lv_counter += 1.");
		buildSrc("      endloop.");
		buildSrc("    endif.");
		buildSrc("  endmethod.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testImplementationKeepMixed() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configKeepMixedCaseInIdentifiers.setValue(true);

		buildSrc("  method pretty_print_case_1.");
		buildSrc("    data(lo_InLine) = getObject( )->getSubObject( ) ##needed.");
		buildSrc("  endmethod.");

		buildExp("  method PRETTY_PRINT_CASE_1.");
		buildExp("    data(lo_InLine) = getObject( )->getSubObject( ) ##NEEDED.");
		buildExp("  endmethod.");

		testRule();
	}

	@Test
	void testImplementationChangeMixed() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configKeepMixedCaseInIdentifiers.setValue(false);

		buildSrc("  method pretty_print_case_1.");
		buildSrc("    data(lo_InLine) = getObject( )->getSubObject( ) ##needed.");
		buildSrc("  endmethod.");

		buildExp("  method PRETTY_PRINT_CASE_1.");
		buildExp("    data(LO_INLINE) = GETOBJECT( )->GETSUBOBJECT( ) ##NEEDED.");
		buildExp("  endmethod.");

		testRule();
	}

	@Test
	void testRaiseExceptionNew() {
		// ensure "new" is classified as a keyword, not as an identifier
		
		buildSrc("  raise exception new cx_message( mv_msgid = '001' ).");
		buildSrc("  raise exception new cx_message( textid = VALUE #( ) ).");

		buildExp("  RAISE EXCEPTION NEW cx_message( mv_msgid = '001' ).");
		buildExp("  RAISE EXCEPTION NEW cx_message( textid = VALUE #( ) ).");

		testRule();
	}

	@Test
	void testAbapSqlSelectCount() {
		// expect ABAP SQL keywords to be put to upper case, including COUNT(*)
		
		buildSrc("  select count(*) into lv_count");
		buildSrc("    from dtab");
		buildSrc("    where bukrs = lv_company_code.");

		buildExp("  SELECT COUNT(*) INTO lv_count");
		buildExp("    FROM dtab");
		buildExp("    WHERE bukrs = lv_company_code.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testComposedIdentifiers() {
		buildSrc("  method IF_ANY_INTERFACE~any_method.");
		buildSrc("    MS_STRUCTURE-component1 = mo_any_instance->GET_ANY_VALUE( ).");
		buildSrc("    ms_structure-COMPONENT2 = if_any_interface=>CS_ANY_STRUCTURE-any_component.");
		buildSrc("    loop at itab assigning field-symbol(<ls_any>).");
		buildSrc("      MO_ANY_INSTANCE->any_method( param = <LS_ANY>-component ).");
		buildSrc("    endloop.");
		buildSrc("  endmethod.");

		buildExp("  METHOD if_any_interface~any_method.");
		buildExp("    ms_structure-component1 = mo_any_instance->get_any_value( ).");
		buildExp("    ms_structure-component2 = if_any_interface=>cs_any_structure-any_component.");
		buildExp("    LOOP AT itab ASSIGNING FIELD-SYMBOL(<ls_any>).");
		buildExp("      mo_any_instance->any_method( param = <ls_any>-component ).");
		buildExp("    ENDLOOP.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testComposedIdentifiersWithCamelCase() {
		buildSrc("  method IF_ANY_INTERFACE~AnyMethod.");
		buildSrc("    MS_STRUCTURE-Component1 = moAnyInstance->GET_ANY_VALUE( ).");
		buildSrc("    msStructure-COMPONENT2 = if_any_interface=>CS_ANY_STRUCTURE-AnyComponent.");
		buildSrc("    loop at itab assigning field-symbol(<lsAny>).");
		buildSrc("      MO_ANY_INSTANCE->any_method( param = <lsAny>-COMPONENT ).");
		buildSrc("    endloop.");
		buildSrc("  endmethod.");

		buildExp("  METHOD if_any_interface~AnyMethod.");
		buildExp("    ms_structure-Component1 = moAnyInstance->get_any_value( ).");
		buildExp("    msStructure-component2 = if_any_interface=>cs_any_structure-AnyComponent.");
		buildExp("    LOOP AT itab ASSIGNING FIELD-SYMBOL(<lsAny>).");
		buildExp("      mo_any_instance->any_method( param = <lsAny>-component ).");
		buildExp("    ENDLOOP.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testRaiseShortdumpNew() {
		buildSrc("    raise shortdump new cx_any( ).");
		buildSrc("    raise shortdump type cx_demo_t100.");

		buildExp("    RAISE SHORTDUMP NEW cx_any( ).");
		buildExp("    RAISE SHORTDUMP TYPE cx_demo_t100.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testEntityManipulationLanguage() {
		// ensure that casing is correct for newer syntax of ABAP Entity Manipulation Language EML)
		
		buildSrc("modify entities of bdef");
		buildSrc("      entity bdef1");
		buildSrc("      create from fields_tab");
		buildSrc("");
		buildSrc("      entity bdef2");
		buildSrc("      create by \\_assoc");
		buildSrc("      set fields with fields_tab");
		buildSrc("");
		buildSrc("      failed   data(ls_failed)");
		buildSrc("      reported data(ls_reported)");
		buildSrc("      mapped   data(ls_mapped).");

		buildExp("MODIFY ENTITIES OF bdef");
		buildExp("      ENTITY bdef1");
		buildExp("      CREATE FROM fields_tab");
		buildExp("");
		buildExp("      ENTITY bdef2");
		buildExp("      CREATE BY \\_assoc");
		buildExp("      SET FIELDS WITH fields_tab");
		buildExp("");
		buildExp("      FAILED   DATA(ls_failed)");
		buildExp("      REPORTED DATA(ls_reported)");
		buildExp("      MAPPED   DATA(ls_mapped).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDeriveFromFirstLowerUpperUpperLower() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);

		buildSrc("class CL_UPPER_AND_LOWER_CASE definition public.");
		buildSrc("  public SECTION ##pragma[PARAM].");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !ev_result type int4 ##PRAGMA[param].");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("CLASS cl_upper_and_lower_case IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5 ##PRAGMA[param].");
		buildSrc("");
		buildSrc("    EV_RESULT = lcMixedCaseIdentifier ##OTHER_PRAGMA.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("class CL_UPPER_AND_LOWER_CASE definition public.");
		buildExp("  public section ##pragma[PARAM].");
		buildExp("    methods PRETTY_PRINT_CASE");
		buildExp("      exporting !EV_RESULT type INT4 ##pragma[PARAM].");
		buildExp("endclass.");
		buildExp("");
		buildExp("CLASS cl_upper_and_lower_case IMPLEMENTATION.");
		buildExp("  METHOD pretty_print_case.");
		buildExp("    CONSTANTS lcMixedCaseIdentifier TYPE i VALUE 5 ##pragma[PARAM].");
		buildExp("");
		buildExp("    ev_result = lcMixedCaseIdentifier ##other_pragma.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDeriveFromFirstUpperUpperLowerLower() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);

		buildSrc("CLASS CL_UPPER_AND_LOWER_CASE definition public.");
		buildSrc("  public SECTION ##PRAGMA[param].");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !ev_result type int4 ##pragma[PARAM].");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("class cl_upper_and_lower_case IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5 ##pragma[PARAM].");
		buildSrc("");
		buildSrc("    EV_RESULT = lcMixedCaseIdentifier ##other_pragma.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS CL_UPPER_AND_LOWER_CASE DEFINITION PUBLIC.");
		buildExp("  PUBLIC SECTION ##PRAGMA[param].");
		buildExp("    METHODS PRETTY_PRINT_CASE");
		buildExp("      EXPORTING !EV_RESULT TYPE INT4 ##PRAGMA[param].");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("class cl_upper_and_lower_case implementation.");
		buildExp("  method pretty_print_case.");
		buildExp("    constants lcMixedCaseIdentifier type i value 5 ##PRAGMA[param].");
		buildExp("");
		buildExp("    ev_result = lcMixedCaseIdentifier ##OTHER_PRAGMA.");
		buildExp("  endmethod.");
		buildExp("endclass.");

		testRule();
	}

	@Test
	void testDeriveFromFirstUpperLowerLowerUpper() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);

		buildSrc("CLASS cl_upper_and_lower_case definition public.");
		buildSrc("  public SECTION.");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !ev_result type int4.");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("class CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5.");
		buildSrc("");
		buildSrc("    EV_RESULT = lcMixedCaseIdentifier.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_upper_and_lower_case DEFINITION PUBLIC.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS pretty_print_case");
		buildExp("      EXPORTING !ev_result TYPE int4.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("class CL_UPPER_AND_LOWER_CASE implementation.");
		buildExp("  method PRETTY_PRINT_CASE.");
		buildExp("    constants lcMixedCaseIdentifier type I value 5.");
		buildExp("");
		buildExp("    EV_RESULT = lcMixedCaseIdentifier.");
		buildExp("  endmethod.");
		buildExp("endclass.");

		testRule();
	}

	@Test
	void testDeriveFromFirstLowerLowerUpperUpper() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);

		buildSrc("class cl_upper_and_lower_case definition public.");
		buildSrc("  public SECTION.");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !ev_result type int4.");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5.");
		buildSrc("");
		buildSrc("    EV_RESULT = lcMixedCaseIdentifier.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("class cl_upper_and_lower_case definition public.");
		buildExp("  public section.");
		buildExp("    methods pretty_print_case");
		buildExp("      exporting !ev_result type int4.");
		buildExp("endclass.");
		buildExp("");
		buildExp("CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildExp("  METHOD PRETTY_PRINT_CASE.");
		buildExp("    CONSTANTS lcMixedCaseIdentifier TYPE I VALUE 5.");
		buildExp("");
		buildExp("    EV_RESULT = lcMixedCaseIdentifier.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDeriveFromFirstDontKeepMixed() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);
		rule.configKeepMixedCaseInIdentifiers.setValue(false);

		buildSrc("class cl_upper_and_lower_case definition public.");
		buildSrc("  public SECTION.");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !evResult type int4.");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5.");
		buildSrc("");
		buildSrc("    evResult = lcMixedCaseIdentifier.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("class cl_upper_and_lower_case definition public.");
		buildExp("  public section.");
		buildExp("    methods pretty_print_case");
		buildExp("      exporting !evresult type int4.");
		buildExp("endclass.");
		buildExp("");
		buildExp("CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildExp("  METHOD PRETTY_PRINT_CASE.");
		buildExp("    CONSTANTS LCMIXEDCASEIDENTIFIER TYPE I VALUE 5.");
		buildExp("");
		buildExp("    EVRESULT = LCMIXEDCASEIDENTIFIER.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDeriveFromMajorityLowerLowerUpperLower() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_MAJORITY);

		buildSrc("class cl_upper_and_lower_case definition public.");
		buildSrc("  public SECTION ##pragma[PARAM].");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !evResult type int4 ##PRAGMA[param].");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5 ##PRAGMA[param].");
		buildSrc("");
		buildSrc("    evResult = lcMixedCaseIdentifier ##OTHER_PRAGMA.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("class cl_upper_and_lower_case definition public.");
		buildExp("  public section ##PRAGMA[param].");
		buildExp("    methods pretty_print_case");
		buildExp("      exporting !evResult type int4 ##PRAGMA[param].");
		buildExp("endclass.");
		buildExp("");
		buildExp("CLASS cl_upper_and_lower_case IMPLEMENTATION.");
		buildExp("  METHOD pretty_print_case.");
		buildExp("    CONSTANTS lcMixedCaseIdentifier TYPE i VALUE 5 ##PRAGMA[param].");
		buildExp("");
		buildExp("    evResult = lcMixedCaseIdentifier ##OTHER_PRAGMA.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDeriveFromMajorityLowerUpperLowerLower() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_MAJORITY);

		buildSrc("class cl_upper_and_lower_case definition public.");
		buildSrc("  public SECTION ##PRAGMA[param].");
		buildSrc("    methods PRETTY_PRINT_CASE");
		buildSrc("      exporting !evResult type INT4 ##pragma[PARAM].");
		buildSrc("endclass.");
		buildSrc("");
		buildSrc("CLASS CL_UPPER_AND_LOWER_CASE implementation.");
		buildSrc("  method pretty_print_case.");
		buildSrc("    constants lcMixedCaseIdentifier TYPE i value 5 ##pragma[PARAM].");
		buildSrc("");
		buildSrc("    evResult = lcMixedCaseIdentifier ##other_pragma.");
		buildSrc("  ENDMETHOD.");
		buildSrc("endclass.");

		buildExp("class CL_UPPER_AND_LOWER_CASE definition public.");
		buildExp("  public section ##pragma[PARAM].");
		buildExp("    methods PRETTY_PRINT_CASE");
		buildExp("      exporting !evResult type INT4 ##pragma[PARAM].");
		buildExp("endclass.");
		buildExp("");
		buildExp("class cl_upper_and_lower_case implementation.");
		buildExp("  method pretty_print_case.");
		buildExp("    constants lcMixedCaseIdentifier type i value 5 ##pragma[PARAM].");
		buildExp("");
		buildExp("    evResult = lcMixedCaseIdentifier ##other_pragma.");
		buildExp("  endmethod.");
		buildExp("endclass.");

		testRule();
	}

	@Test
	void testDeriveFromMajorityExecSql() {
		// expect keywords to be put to lower case and the identifier to remain in upper case, 
		// because the Command inside(!) EXEC SQL ... ENDEXEC does NOT count
		
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_MAJORITY);

		buildSrc("  method PRETTY_PRINT_CASE.");
		buildSrc("    field-symbols <> TYPE TY_S_ANY_TYPE.");
		buildSrc("    EXEC SQL.");
		buildSrc("      SELECT from_year");
		buildSrc("        FROM dtab");
		buildSrc("        INTO :lv_from_year");
		buildSrc("        WHERE bukrs = :lv_company_code;");
		buildSrc("    endexec.");
		buildSrc("  endmethod.");

		buildExp("  method PRETTY_PRINT_CASE.");
		buildExp("    field-symbols <> type TY_S_ANY_TYPE.");
		buildExp("    exec sql.");
		buildExp("      SELECT from_year");
		buildExp("        FROM dtab");
		buildExp("        INTO :lv_from_year");
		buildExp("        WHERE bukrs = :lv_company_code;");
		buildExp("    endexec.");
		buildExp("  endmethod.");

		testRule();
	}
	
	@Test
	void testIsConfigValueEnabledDeriveNone() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.NONE);

		assertTrue(rule.isConfigValueEnabled(rule.configDeriveCaseMethod));
		assertTrue(rule.isConfigValueEnabled(rule.configDefinitionKeywordStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configDefinitionIdentifierStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configImplementationKeywordStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configImplementationIdentifierStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configPragmaStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configPragmaParameterStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configKeepMixedCaseInIdentifiers));
	}
	
	@Test
	void testIsConfigValueEnabledDeriveFromFirst() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_FIRST_TOKEN);

		assertTrue(rule.isConfigValueEnabled(rule.configDeriveCaseMethod));
		assertFalse(rule.isConfigValueEnabled(rule.configDefinitionKeywordStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configDefinitionIdentifierStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configImplementationKeywordStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configImplementationIdentifierStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configPragmaStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configPragmaParameterStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configKeepMixedCaseInIdentifiers));
	}
	
	@Test
	void testIsConfigValueEnabledDeriveFromMajority() {
		rule.configDeriveCaseMethod.setEnumValue(DeriveCaseMethod.FROM_MAJORITY);

		assertTrue(rule.isConfigValueEnabled(rule.configDeriveCaseMethod));
		assertFalse(rule.isConfigValueEnabled(rule.configDefinitionKeywordStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configDefinitionIdentifierStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configImplementationKeywordStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configImplementationIdentifierStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configPragmaStyle));
		assertFalse(rule.isConfigValueEnabled(rule.configPragmaParameterStyle));
		assertTrue(rule.isConfigValueEnabled(rule.configKeepMixedCaseInIdentifiers));
	}

	@Test
	void testLiteralWithAlphanumericTextSymbol() {
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("DATA text TYPE string.");
		buildSrc("text = 'Hello SAP'(a01). ");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testTextualComparisonOps() {
		buildSrc("  METHOD any_method.");
		buildSrc("    if a eq b or c NE d and e Gt f.");
		buildSrc("    endif.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("    IF a EQ b OR c NE d AND e GT f.");
		buildExp("    ENDIF.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testTextualComparisonOpsToLower() {
		rule.configImplementationKeywordStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configImplementationIdentifierStyle.setEnumValue(CaseStyle.UPPER_CASE);

		buildSrc("  METHOD any_method.");
		buildSrc("    if a eq b or c NE d and e Gt f.");
		buildSrc("    endif.");
		buildSrc("  ENDMETHOD.");

		buildExp("  method ANY_METHOD.");
		buildExp("    if A eq B or C ne D and E gt F.");
		buildExp("    endif.");
		buildExp("  endmethod.");

		testRule();
	}
	
	@Test
	void testPragmaUpperParamsLowerKeepMixedCase() {
		rule.configPragmaStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configPragmaParameterStyle.setEnumValue(CaseStyle.LOWER_CASE);

		buildSrc("    METHODS insert ##shadow[INSERT].");
		buildSrc("    METHODS any_method ##any_pragma[PARAM1][][Param3].");

		buildExp("    METHODS insert ##SHADOW[insert].");
		buildExp("    METHODS any_method ##ANY_PRAGMA[param1][][Param3].");
		
		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testPragmaLowerParamsUpperKeepMixedCase() {
		rule.configPragmaStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configPragmaParameterStyle.setEnumValue(CaseStyle.UPPER_CASE);

		buildSrc("    METHODS insert ##SHADOW[insert].");
		buildSrc("    METHODS any_method ##ANY_PRAGMA[PARAM1][][Param3].");

		buildExp("    METHODS insert ##shadow[INSERT].");
		buildExp("    METHODS any_method ##any_pragma[PARAM1][][Param3].");
		
		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testPragmaUpperParamsLowervNoMixedCase() {
		rule.configPragmaStyle.setEnumValue(CaseStyle.UPPER_CASE);
		rule.configPragmaParameterStyle.setEnumValue(CaseStyle.LOWER_CASE);
		rule.configKeepMixedCaseInIdentifiers.setValue(false);

		buildSrc("    METHODS insert ##shadow[Insert].");
		buildSrc("    METHODS any_method ##any_pragma[Param1][][PARam3].");

		buildExp("    METHODS insert ##SHADOW[insert].");
		buildExp("    METHODS any_method ##ANY_PRAGMA[param1][][param3].");
		
		putAnyClassDefAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testUnknownAbapRelease() {
		// ensure that this rule (which is not limited to a certain ABAP release) even works if the ABAP Release
		// is unknown ("fallback")
		 
		setAbapReleaseOfCode(ABAP.FALLBACK_RELEASE); 
		
		buildSrc("    \" HIGH and LOW must be upper case here:");
		buildSrc("    set run time clock resolution high.");
		buildSrc("    set run time clock resolution low.");

		buildExp("    \" HIGH and LOW must be upper case here:");
		buildExp("    SET RUN TIME CLOCK RESOLUTION HIGH.");
		buildExp("    SET RUN TIME CLOCK RESOLUTION LOW.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
