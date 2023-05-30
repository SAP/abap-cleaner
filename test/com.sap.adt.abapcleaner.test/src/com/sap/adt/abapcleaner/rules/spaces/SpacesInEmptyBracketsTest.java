package com.sap.adt.abapcleaner.rules.spaces;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class SpacesInEmptyBracketsTest extends RuleTestBase {
	private SpacesInEmptyBracketsRule rule;
	
	SpacesInEmptyBracketsTest() {
		super(RuleID.SPACES_IN_EMPTY_BRACKETS);
		rule = (SpacesInEmptyBracketsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configRemoveMultiSpaceIfEmpty.setValue(true);
		rule.configSeparateFromCharLiterals.setValue(true);
		rule.configSeparateCondensedCases.setValue(false);
	}
	
	@Test
	void testMethodChain() {
		buildSrc("    ev_result = class_name(  )=>get_tool(  )->get_value(    ).");

		buildExp("    ev_result = class_name( )=>get_tool( )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testMethodChainUnchanged() {
		rule.configRemoveMultiSpaceIfEmpty.setValue(false);

		buildSrc("    ev_result = class_name(  )=>get_tool(  )->get_value(    ).");

		copyExpFromSrc(); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentLineInsideCommand() {
		buildSrc("    class_name(  )=>get_tool(");
		buildSrc("        \" comment");
		buildSrc("        )->get_value(    ).");

		buildExp("    class_name( )=>get_tool(");
		buildExp("        \" comment");
		buildExp("        )->get_value( ).");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLineBreaksNotRemoved() {
		// expect no change in case of line breaks within the brackets
		
		buildSrc("    ev_result = get_value(");
		buildSrc("                ).");

		copyExpFromSrc(); 

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testTextFieldLiterals() {
		buildSrc("    any_method( 'text field literal' ).");
		buildSrc("    any_method('other literal' ).");
		buildSrc("    any_method( 'third literal').");
		buildSrc("");
		buildSrc("    lt_any_table = VALUE #( ( CONV #( '11.11') )");
		buildSrc("                            ( CONV #('22.22' ) )");
		buildSrc("                            ( CONV #( '33.33') ) ).");

		buildExp("    any_method( 'text field literal' ).");
		buildExp("    any_method( 'other literal' ).");
		buildExp("    any_method( 'third literal' ).");
		buildExp("");
		buildExp("    lt_any_table = VALUE #( ( CONV #( '11.11' ) )");
		buildExp("                            ( CONV #( '22.22' ) )");
		buildExp("                            ( CONV #( '33.33' ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTextFieldLiteralsUnchanged() {
		rule.configSeparateFromCharLiterals.setValue(false);

		buildSrc("    any_method( 'text field literal' ).");
		buildSrc("    any_method('other literal' ).");
		buildSrc("    any_method( 'third literal').");
		buildSrc("");
		buildSrc("    lt_any_table = VALUE #( ( CONV #( '11.11') )");
		buildSrc("                            ( CONV #('22.22' ) )");
		buildSrc("                            ( CONV #('33.33') ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDynamicMethodCallUnchanged() {
		// introducing spaces in the following dynamic examples would be a syntax error
		buildSrc("    CALL METHOD ('METHOD_NAME').");
		buildSrc("    CALL METHOD lo_instance->('METHOD_NAME').");
		buildSrc("");
		buildSrc("    CALL METHOD (`METHOD_NAME`).");
		buildSrc("    CALL METHOD lo_instance->(`METHOD_NAME`).");
		buildSrc("");
		buildSrc("    ASSIGN ('(FUNC_GRP_NAME)TABLE[]') TO <lt_table>.");
		buildSrc("");
		buildSrc("    ls_struc-('COMPONENT_NAME') = 1.");
		buildSrc("    <ls_struc>-('COMPONENT_NAME') = 1.");
		buildSrc("    lt_table[ 1 ]-('COMPONENT_NAME') = 1.");
		buildSrc("    lr_data_ref->('COMPONENT_NAME') = 1.");
		
		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTextStringLiterals() {
		buildSrc("    other_method( `text string literal` ).");
		buildSrc("    other_method(`other literal` ).");
		buildSrc("    other_method( `third literal`).");
		buildSrc("");
		buildSrc("    lt_other_table = VALUE #( ( `abc` )");
		buildSrc("                              ( `def`)");
		buildSrc("                              ( `ghi`) ).");

		buildExp("    other_method( `text string literal` ).");
		buildExp("    other_method( `other literal` ).");
		buildExp("    other_method( `third literal` ).");
		buildExp("");
		buildExp("    lt_other_table = VALUE #( ( `abc` )");
		buildExp("                              ( `def` )");
		buildExp("                              ( `ghi` ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTextStringLiteralsUnchanged() {
		rule.configSeparateFromCharLiterals.setValue(false);

		buildSrc("    other_method( `text string literal` ).");
		buildSrc("    other_method(`other literal` ).");
		buildSrc("    other_method( `third literal`).");
		buildSrc("");
		buildSrc("    lt_other_table = VALUE #( ( `abc` )");
		buildSrc("                              ( `def`)");
		buildSrc("                              ( `ghi`) ).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testIndentAdjustedForEmptyParens() {
		// expect the indent of the second and third line to be adjusted, but not the indent of the last three lines
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method( iv_any_param    = 1");
		buildSrc("                                                                iv_other_param  = 2");
		buildSrc("                                                                iv_third_param  = 3 ).");

		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method( iv_any_param    = 1");
		buildExp("                                                        iv_other_param  = 2");
		buildExp("                                                        iv_third_param  = 3 ).");

		testRule();
	}

	@Test
	void testIndentAdjustedForAttachedLiterals() {
		// expect the indent of the second and third line to be adjusted, but not the indent of the last three lines
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method( iv_any_param    = 1");
		buildSrc("                                                                iv_other_param  = 2");
		buildSrc("                                                                iv_third_param  = 3 ).");
		buildSrc("");
		buildSrc("    cl_any_factory=>get(    )->get_utility(      )->any_method(");
		buildSrc("        iv_any_param    = 1");
		buildSrc("        iv_other_param  = 2");
		buildSrc("        iv_third_param  = 3 ).");

		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method( iv_any_param    = 1");
		buildExp("                                                        iv_other_param  = 2");
		buildExp("                                                        iv_third_param  = 3 ).");
		buildExp("");
		buildExp("    cl_any_factory=>get( )->get_utility( )->any_method(");
		buildExp("        iv_any_param    = 1");
		buildExp("        iv_other_param  = 2");
		buildExp("        iv_third_param  = 3 ).");

		testRule();
	}

	@Test
	void testKeepCondensedCases() {
		buildSrc("    any_method('text field literal').");
		buildSrc("    other_method(`text string literal`).");
		buildSrc("");
		buildSrc("    lt_amount = VALUE #( ( CONV #('11.11') )");
		buildSrc("                         ( CONV #('22.22') )");
		buildSrc("                         ( CONV #('33.33') ) ).");

		copyExpFromSrc();
		
		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testChangeCondensedCases() {
		rule.configSeparateCondensedCases.setValue(true);

		buildSrc("    any_method('text field literal').");
		buildSrc("    other_method(`text string literal`).");
		buildSrc("");
		buildSrc("    lt_amount = VALUE #( ( CONV #('11.11') )");
		buildSrc("                         ( CONV #('22.22') )");
		buildSrc("                         ( CONV #('33.33') ) ).");

		buildExp("    any_method( 'text field literal' ).");
		buildExp("    other_method( `text string literal` ).");
		buildExp("");
		buildExp("    lt_amount = VALUE #( ( CONV #( '11.11' ) )");
		buildExp("                         ( CONV #( '22.22' ) )");
		buildExp("                         ( CONV #( '33.33' ) ) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}
