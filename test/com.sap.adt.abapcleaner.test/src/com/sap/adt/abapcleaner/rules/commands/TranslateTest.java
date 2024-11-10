package com.sap.adt.abapcleaner.rules.commands;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class TranslateTest extends RuleTestBase {
	private TranslateRule rule;
	
	TranslateTest() {
		super(RuleID.TRANSLATE);
		rule = (TranslateRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configReplaceTranslateToUpperLower.setValue(true);
		rule.configReplaceTranslateUsing.setValue(true);
		rule.configReplaceUnevenMasks.setValue(true);
		rule.configProcessChains.setValue(true);
		rule.configSkipUnknownTypes.setValue(true);
	}

	@Test
	void testIsConfigValueEnabled() {
		assertTrue(rule.isConfigValueEnabled(rule.configReplaceTranslateToUpperLower));

		rule.configSkipUnknownTypes.setValue(true);
		assertFalse(rule.isConfigValueEnabled(rule.configUnknownTypeWarning));

		rule.configSkipUnknownTypes.setValue(false);
		assertTrue(rule.isConfigValueEnabled(rule.configUnknownTypeWarning));
	}

	@Test
	void testTranslateToUpperAndLower() {
		rule.configSkipUnknownTypes.setValue(false);

		buildSrc("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildSrc("    TRANSLATE lv_text TO LOWER CASE.");
		buildSrc("    TRANSLATE is_struc-component TO UPPER CASE.");

		buildExp("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildExp("    lv_text = to_lower( lv_text ).");
		buildExp("    is_struc-component = to_upper( is_struc-component ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateToUpperAndLowerWithCommentAndPragma() {
		rule.configSkipUnknownTypes.setValue(false);

		buildSrc("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildSrc("    TRANSLATE lv_text TO LOWER CASE ##PRAGMA.");
		buildSrc("    TRANSLATE is_struc-component TO UPPER CASE. \" `ANY TEXT`");

		buildExp("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildExp("    lv_text = to_lower( lv_text ) ##PRAGMA.");
		buildExp("    is_struc-component = to_upper( is_struc-component ). \" `ANY TEXT`");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateToUpperAndLowerUnchanged() {
	   rule.configReplaceTranslateToUpperLower.setValue(false);

	   buildSrc("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildSrc("    TRANSLATE lv_text TO LOWER CASE. \" `any text`");
		buildSrc("    TRANSLATE lv_text TO UPPER CASE. \" `ANY TEXT`");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingSimpleCase() {
		buildSrc("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildSrc("    TRANSLATE lv_magic USING 'abbaABBA'.");
		buildSrc("");
		buildSrc("    DATA lv_file_name TYPE string VALUE `not:all?chars\\are/allowed`.");
		buildSrc("    TRANSLATE lv_file_name USING `\\-/-:-*-?-\"-<->-|-`.");

		buildExp("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildExp("    lv_magic = translate( val  = lv_magic");
		buildExp("                          from = `abAB`");
		buildExp("                          to   = `baBA` ).");
		buildExp("");
		buildExp("    DATA lv_file_name TYPE string VALUE `not:all?chars\\are/allowed`.");
		buildExp("    lv_file_name = translate( val  = lv_file_name");
		buildExp("                              from = `\\/:*?\"<>|`");
		buildExp("                              to   = `---------` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingWithPragmaAndComment() {
		buildSrc("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildSrc("    TRANSLATE lv_magic USING 'abbaABBA' ##PRAGMA. \" comment");

		buildExp("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildExp("    lv_magic = translate( val  = lv_magic");
		buildExp("                          from = `abAB`");
		buildExp("                          to   = `baBA` ) ##PRAGMA. \" comment");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingWithNonLiteralMaskUnchanged() {
		buildSrc("    CONSTANTS lc_mask TYPE string VALUE `abbaABBA`.");
		buildSrc("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildSrc("    TRANSLATE lv_magic USING lc_mask.");
		buildSrc("");
		buildSrc("    DATA lv_file_name TYPE string VALUE `not:all?chars\\are/allowed`.");
		buildSrc("    TRANSLATE lv_file_name USING get_mask( ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingUnchanged() {
		rule.configReplaceTranslateUsing.setValue(false);

		buildSrc("    DATA lv_magic TYPE string VALUE `Barbcbdbarb`.");
		buildSrc("    TRANSLATE lv_magic USING 'abbaABBA'.");
		buildSrc("");
		buildSrc("    DATA lv_file_name TYPE string VALUE `not:all?chars\\are/allowed`.");
		buildSrc("    TRANSLATE lv_file_name USING `\\-/-:-*-?-\"-<->-|-`.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingWithTextFieldLiteral() {
		// ensure that the FROM and TO parameters always get text string literals `...`, NOT text field literals '...', 
		// because that would lead to a different result is case of trailing spaces
		
		buildSrc("    DATA lv_abc TYPE string VALUE `a1b2c3`.");
		buildSrc("    TRANSLATE lv_abc USING '1 2 3 '.");

		buildExp("    DATA lv_abc TYPE string VALUE `a1b2c3`.");
		buildExp("    lv_abc = translate( val  = lv_abc");
		buildExp("                        from = `123`");
		buildExp("                        to   = `   ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingWithUnevenMask() {
		// ensure that from a mask with an uneven number of characters, the last character is NOT added to the FROM 
		// parameter, because FROM = `c+` TO = `C` would remove all `+` from the result
		
		buildSrc("    DATA lv_c_plus_plus TYPE string VALUE `c++`.");
		buildSrc("    TRANSLATE lv_c_plus_plus USING 'cC+'.");

		buildExp("    DATA lv_c_plus_plus TYPE string VALUE `c++`.");
		buildExp("    lv_c_plus_plus = translate( val  = lv_c_plus_plus");
		buildExp("                                from = `c`");
		buildExp("                                to   = `C` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateUsingWithUnevenMaskUnchanged() {
		rule.configReplaceUnevenMasks.setValue(false);
		
		buildSrc("    DATA lv_c_plus_plus TYPE string VALUE `c++`.");
		buildSrc("    TRANSLATE lv_c_plus_plus USING 'cC+'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMaskWithLessThan2Chars() {
		// ensure that these TRANSLATE ... USING is not changed if the mask is shorter than 2 characters
		
		buildSrc("    DATA lv_text TYPE string VALUE ` abc `.");
		buildSrc("    TRANSLATE lv_text USING ``.");
		buildSrc("    TRANSLATE lv_text USING ` `.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnescapingAndEscaping() {
		// ensure that literals are correctly unescaped (depending on their type) and escaped: 
		// '''_`_'  => FROM = `'``` TO = `__`
		// `'_``_`  => FROM = `'``` TO = `__`
		// |\\ \| | => FROM = `\|`  TO = `  `
		
		buildSrc("    DATA lv_text TYPE string VALUE ` ' `` `.");
		buildSrc("    TRANSLATE lv_text USING '''_`_'.");
		buildSrc("    TRANSLATE lv_text USING `'_``_`.");
		buildSrc("    TRANSLATE lv_text USING |\\\\ \\| |.");

		buildExp("    DATA lv_text TYPE string VALUE ` ' `` `.");
		buildExp("    lv_text = translate( val  = lv_text");
		buildExp("                         from = `'```");
		buildExp("                         to   = `__` ).");
		buildExp("    lv_text = translate( val  = lv_text");
		buildExp("                         from = `'```");
		buildExp("                         to   = `__` ).");
		buildExp("    lv_text = translate( val  = lv_text");
		buildExp("                         from = `\\|`");
		buildExp("                         to   = `  ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainKept() {
		rule.configProcessChains.setValue(false);

		buildSrc("    TRANSLATE: lv_text TO LOWER CASE, lv_other_text TO UPPER CASE.");
		buildSrc("");
		buildSrc("    TRANSLATE lv_abc USING: '1 2 3 ', 'a-b-c-'.");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChainProcessed() {
		rule.configProcessChains.setValue(true);

		buildSrc("    DATA: lv_text TYPE string, lv_other_text type string, lv_abc TYPE char10.");
		buildSrc("");
		buildSrc("    TRANSLATE: lv_text TO LOWER CASE, lv_other_text TO UPPER CASE.");
		buildSrc("");
		buildSrc("    TRANSLATE lv_abc USING: '1 2 3 ', 'a-b-c-'.");

		buildExp("    DATA: lv_text TYPE string, lv_other_text type string, lv_abc TYPE char10.");
		buildExp("");
		buildExp("    lv_text = to_lower( lv_text ).");
		buildExp("    lv_other_text = to_upper( lv_other_text ).");
		buildExp("");
		buildExp("    lv_abc = translate( val  = lv_abc");
		buildExp("                        from = `123`");
		buildExp("                        to   = `   ` ).");
		buildExp("    lv_abc = translate( val  = lv_abc");
		buildExp("                        from = `abc`");
		buildExp("                        to   = `---` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMacroDefinition() {
		rule.configSkipUnknownTypes.setValue(false);

		buildSrc("DEFINE any_macro.");
		buildSrc("  TRANSLATE &1 TO UPPER CASE.");
		buildSrc("  TRANSLATE &1 TO LOWER CASE.");
		buildSrc("  TRANSLATE &1 USING 'abbaABBA'.");
		buildSrc("END-OF-DEFINITION.");

		buildExp("DEFINE any_macro.");
		buildExp("  &1 = to_upper( &1 ).");
		buildExp("  &1 = to_lower( &1 ).");
		buildExp("  &1 = translate( val  = &1");
		buildExp("                  from = `abAB`");
		buildExp("                  to   = `baBA` ).");
		buildExp("END-OF-DEFINITION.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownTypesUnchanged() {
		rule.configSkipUnknownTypes.setValue(true);

		buildSrc("    DATA(ls_unknown_type) = get_charlike_structure( ).");
		buildSrc("    DATA ls_ddic_type TYPE some_ddic_type.");
		buildSrc("    DATA ls_other_type TYPE if_any_interface=>ty_s_typedef_out_of_sight.");
		buildSrc("    TRANSLATE ls_unknown_type TO UPPER CASE.");
		buildSrc("    TRANSLATE ls_ddic_type TO LOWER CASE.");
		buildSrc("    TRANSLATE ls_other_type USING 'a1b2'.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKnownCharlikeTypes() {
		buildSrc("    DATA lv_string1  TYPE string.");
		buildSrc("    DATA lv_string2  LIKE lv_string1.");
		buildSrc("    DATA lv_string3  TYPE string VALUE `any`.");
		buildSrc("    DATA lv_sstring  TYPE sstring.");
		buildSrc("    DATA lv_char1(3) TYPE c.");
		buildSrc("    DATA lv_char2    TYPE c LENGTH 2.");
		buildSrc("    DATA lv_char3    TYPE char3.");
		buildSrc("    DATA lv_char4    LIKE lv_char1.");
		buildSrc("    DATA lv_dats1    TYPE dats.");
		buildSrc("    DATA lv_dats2    TYPE d.");
		buildSrc("    DATA lv_uname1   TYPE sy-uname.");
		buildSrc("    DATA lv_uname2   LIKE sy-uname.");
		buildSrc("    DATA lv_uname3   TYPE syst_uname.");
		buildSrc("    DATA lv_uname4   LIKE lv_uname2.");
		buildSrc("");
		buildSrc("    FIELD-SYMBOLS <lv_clike>     TYPE clike.");
		buildSrc("    FIELD-SYMBOLS <lv_csequence> TYPE csequence.");
		buildSrc("");
		buildSrc("    TRANSLATE lv_string1 TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_string2 TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_string3 USING `a1b2`.");
		buildSrc("    TRANSLATE lv_sstring TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_char1   TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_char2   TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_char3   USING `a1b2`.");
		buildSrc("    TRANSLATE lv_char4   TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_dats1   TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_dats2   TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_uname1  TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_uname2  TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_uname3  USING `a1b2`.");
		buildSrc("    TRANSLATE lv_uname4  TO UPPER CASE.");
		buildSrc("");
		buildSrc("    TRANSLATE <lv_clike>     TO UPPER CASE.");
		buildSrc("    TRANSLATE <lv_csequence> TO LOWER CASE.");

		buildExp("    DATA lv_string1  TYPE string.");
		buildExp("    DATA lv_string2  LIKE lv_string1.");
		buildExp("    DATA lv_string3  TYPE string VALUE `any`.");
		buildExp("    DATA lv_sstring  TYPE sstring.");
		buildExp("    DATA lv_char1(3) TYPE c.");
		buildExp("    DATA lv_char2    TYPE c LENGTH 2.");
		buildExp("    DATA lv_char3    TYPE char3.");
		buildExp("    DATA lv_char4    LIKE lv_char1.");
		buildExp("    DATA lv_dats1    TYPE dats.");
		buildExp("    DATA lv_dats2    TYPE d.");
		buildExp("    DATA lv_uname1   TYPE sy-uname.");
		buildExp("    DATA lv_uname2   LIKE sy-uname.");
		buildExp("    DATA lv_uname3   TYPE syst_uname.");
		buildExp("    DATA lv_uname4   LIKE lv_uname2.");
		buildExp("");
		buildExp("    FIELD-SYMBOLS <lv_clike>     TYPE clike.");
		buildExp("    FIELD-SYMBOLS <lv_csequence> TYPE csequence.");
		buildExp("");
		buildExp("    lv_string1 = to_upper( lv_string1 ).");
		buildExp("    lv_string2 = to_lower( lv_string2 ).");
		buildExp("    lv_string3 = translate( val  = lv_string3");
		buildExp("                            from = `ab`");
		buildExp("                            to   = `12` ).");
		buildExp("    lv_sstring = to_upper( lv_sstring ).");
		buildExp("    lv_char1 = to_upper( lv_char1 ).");
		buildExp("    lv_char2 = to_lower( lv_char2 ).");
		buildExp("    lv_char3 = translate( val  = lv_char3");
		buildExp("                          from = `ab`");
		buildExp("                          to   = `12` ).");
		buildExp("    lv_char4 = to_upper( lv_char4 ).");
		buildExp("    lv_dats1 = to_upper( lv_dats1 ).");
		buildExp("    lv_dats2 = to_lower( lv_dats2 ).");
		buildExp("    lv_uname1 = to_upper( lv_uname1 ).");
		buildExp("    lv_uname2 = to_lower( lv_uname2 ).");
		buildExp("    lv_uname3 = translate( val  = lv_uname3");
		buildExp("                           from = `ab`");
		buildExp("                           to   = `12` ).");
		buildExp("    lv_uname4 = to_upper( lv_uname4 ).");
		buildExp("");
		buildExp("    <lv_clike> = to_upper( <lv_clike> ).");
		buildExp("    <lv_csequence> = to_lower( <lv_csequence> ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKnownNonCharlikeTypesUnchanged() {
		buildSrc("    DATA lv_int1   TYPE int4.");
		buildSrc("    DATA lv_int2   TYPE sy-colno.");
		buildSrc("    DATA lv_int3   LIKE sy-colno.");
		buildSrc("    DATA lv_int4   TYPE syst_colno.");
		buildSrc("    DATA lv_float  TYPE decfloat16.");
		buildSrc("    DATA lv_packed TYPE p LENGTH 8 DECIMALS 2.");
		buildSrc("");
		buildSrc("    TRANSLATE lv_int1 TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_int2 TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_int3 USING `a1b2`.");
		buildSrc("    TRANSLATE lv_int4   TO UPPER CASE.");
		buildSrc("    TRANSLATE lv_float   TO LOWER CASE.");
		buildSrc("    TRANSLATE lv_packed   USING `a1b2`.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testValueParameterWithKnownType() {
		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    METHODS any_method");
		buildSrc("      RETURNING VALUE(result) TYPE char10.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_any_class IMPLEMENTATION.");
		buildSrc("  METHOD any_method.");
		buildSrc("    result = get_result( ).");
		buildSrc("    TRANSLATE result TO LOWER CASE.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    METHODS any_method");
		buildExp("      RETURNING VALUE(result) TYPE char10.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_any_class IMPLEMENTATION.");
		buildExp("  METHOD any_method.");
		buildExp("    result = get_result( ).");
		buildExp("    result = to_lower( result ).");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKnownStructuredTypesKept() {
		// expect 'TRANSLATE ls_structure ...' to be kept even with this configuration, because its type is visible and structured

		rule.configSkipUnknownTypes.setValue(false);

		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("             field TYPE c LENGTH 10,");
		buildSrc("           END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA ls_structure   TYPE ty_s_any_struc.");
		buildSrc("    DATA l_unknown_type TYPE ty_unknown_type.");
		buildSrc("");
		buildSrc("    TRANSLATE ls_structure TO UPPER CASE.");
		buildSrc("    TRANSLATE ls_structure TO LOWER CASE.");
		buildSrc("    TRANSLATE ls_structure USING 'a1b2'.");
		buildSrc("    TRANSLATE l_unknown_type TO UPPER CASE.");

		buildExp("    TYPES: BEGIN OF ty_s_any_struc,");
		buildExp("             field TYPE c LENGTH 10,");
		buildExp("           END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA ls_structure   TYPE ty_s_any_struc.");
		buildExp("    DATA l_unknown_type TYPE ty_unknown_type.");
		buildExp("");
		buildExp("    TRANSLATE ls_structure TO UPPER CASE.");
		buildExp("    TRANSLATE ls_structure TO LOWER CASE.");
		buildExp("    TRANSLATE ls_structure USING 'a1b2'.");
		buildExp("    l_unknown_type = to_upper( l_unknown_type ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}	