package com.sap.adt.abapcleaner.rules.commands;

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
	}

	@Test
	void testTranslateToUpperAndLower() {
		buildSrc("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildSrc("    TRANSLATE lv_text TO LOWER.");
		buildSrc("    TRANSLATE is_struc-component TO UPPER.");

		buildExp("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildExp("    lv_text = to_lower( lv_text ).");
		buildExp("    is_struc-component = to_upper( is_struc-component ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTranslateToUpperAndLowerWithCommentAndPragma() {
		buildSrc("    DATA lv_text TYPE string VALUE `Any Text`.");
		buildSrc("    TRANSLATE lv_text TO LOWER ##PRAGMA.");
		buildSrc("    TRANSLATE is_struc-component TO UPPER. \" `ANY TEXT`");

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
		buildSrc("    TRANSLATE lv_text TO LOWER. \" `any text`");
		buildSrc("    TRANSLATE lv_text TO UPPER. \" `ANY TEXT`");

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
}	