package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AddToEtcTest extends RuleTestBase {
	private AddToEtcRule rule;
	
	AddToEtcTest() {
		super(RuleID.ADD_TO_ETC);
		rule = (AddToEtcRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.KEEP);
	}
	
	@Test
	void testOldAbapRelease() {
		// ensure that calculation assignment operators are NOT introduced if the code must compile against an ABAP Release prior to 7.54, 
		// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-754-assignments.htm
		
		setAbapReleaseOfCode("753");
		
		buildSrc("    ADD 1 TO lv_value.");
		buildSrc("    SUBTRACT 1 FROM cv_date.");
		buildSrc("    MULTIPLY iv_value BY 2.");
		buildSrc("    DIVIDE iv_value BY 2.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testUnknownAbapRelease() {
		// ensure that calculation assignment operators are NOT introduced if the code must compile against an unknown ("fallback") ABAP Release, 
		// which triggers a NumberFormatException in Rule.isCleanupAllowedFor
		 
		setAbapReleaseOfCode(ABAP.FALLBACK_RELEASE); 
		
		buildSrc("    ADD 1 TO lv_value.");
		buildSrc("    SUBTRACT 1 FROM cv_date.");
		buildSrc("    MULTIPLY iv_value BY 2.");
		buildSrc("    DIVIDE iv_value BY 2.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testReleaseRestriction() {
		// ensure that calculation assignment operators are NOT introduced if the user applies an ABAP release restriction < 7.54  
		// (even if the code itself is compiled against the newest release)
		
		setReleaseRestrictionFromUI(753);
		
		buildSrc("    ADD 1 TO lv_value.");
		buildSrc("    SUBTRACT 1 FROM cv_date.");
		buildSrc("    MULTIPLY iv_value BY 2.");
		buildSrc("    DIVIDE iv_value BY 2.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}
	
	@Test
	void testOperandLiteral() {
		buildSrc("    ADD 1 TO lv_value.");
		buildSrc("    SUBTRACT 1 FROM cv_date.");
		buildSrc("    MULTIPLY iv_value BY 2.");
		buildSrc("    DIVIDE iv_value BY 2.");

		buildExp("    lv_value += 1.");
		buildExp("    cv_date -= 1.");
		buildExp("    iv_value *= 2.");
		buildExp("    iv_value /= 2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOperandWithComponentAccess() {
		buildSrc("    ADD lo_typedesc->length TO lv_length.");
		buildSrc("    SUBTRACT lo_typedesc->length FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY lo_struc-component.");
		buildSrc("    DIVIDE lv_value BY lo_struc-component.");

		buildExp("    lv_length += lo_typedesc->length.");
		buildExp("    lv_length -= lo_typedesc->length.");
		buildExp("    lv_value *= lo_struc-component.");
		buildExp("    lv_value /= lo_struc-component.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOperandWithTableExpression() {
		buildSrc("    ADD lts_table[ num  = 5");
		buildSrc("                   name = 'abc']-length TO lv_length.");
		buildSrc("    SUBTRACT lts_table[ num  = 5");
		buildSrc("                        name = 'abc']-length FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY lts_table[ num  = 5");
		buildSrc("                                    name = 'abc']-component.");
		buildSrc("    DIVIDE lv_value BY lts_table[ num  = 5");
		buildSrc("                                  name = 'abc']-component.");

		buildExp("    lv_length += lts_table[ num  = 5");
		buildExp("                            name = 'abc']-length.");
		buildExp("    lv_length -= lts_table[ num  = 5");
		buildExp("                            name = 'abc']-length.");
		buildExp("    lv_value *= lts_table[ num  = 5");
		buildExp("                           name = 'abc']-component.");
		buildExp("    lv_value /= lts_table[ num  = 5");
		buildExp("                           name = 'abc']-component.");

		putAnyMethodAroundSrcAndExp();

		// actually, calls or other expressions are NOT allowed in the obsolete commands!
		deactivateSyntaxCheckAfterParse();
		testRule();
	}

	@Test
	void testOperandWithCallChain() {
		buildSrc("    ADD class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                               iv_param2 = 'abc' ) TO lv_length.");
		buildSrc("    SUBTRACT class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                    iv_param2 = 'abc' ) FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                                iv_param2 = 'abc' ).");
		buildSrc("    DIVIDE lv_value BY class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                              iv_param2 = 'abc' ).");

		buildExp("    lv_length += class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                        iv_param2 = 'abc' ).");
		buildExp("    lv_length -= class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                        iv_param2 = 'abc' ).");
		buildExp("    lv_value *= class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                       iv_param2 = 'abc' ).");
		buildExp("    lv_value /= class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                       iv_param2 = 'abc' ).");
		
		putAnyMethodAroundSrcAndExp();
		
		// actually, calls or other expressions are NOT allowed in the obsolete commands!
		deactivateSyntaxCheckAfterParse();
		testRule();
	}

	@Test
	void testAssignmentTargetWithComponentAccess() {
		buildSrc("    ADD 1 TO ls_struc-component.");
		buildSrc("    SUBTRACT 1 FROM ls_struc-component.");

		buildExp("    ls_struc-component += 1.");
		buildExp("    ls_struc-component -= 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAssignmentTargetWithOffsetAndLength() {
		buildSrc("    ADD 1 TO lv_abc_date+4(2).");
		buildSrc("    SUBTRACT 1 FROM lv_abc_date+4(2).");
		
		buildExp("    lv_abc_date+4(2) += 1.");
		buildExp("    lv_abc_date+4(2) -= 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testChainUnchanged() {
		buildSrc("    ADD 1 TO : lv_value_a, lv_value_b.");
		buildSrc("    SUBTRACT: 1 FROM lv_value_a, 2 FROM lv_value_b.");
		buildSrc("    MULTIPLY iv_value BY : 2, 3, 4.");
		buildSrc("    DIVIDE iv_value : BY 2, BY 3, BY 4.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOperandLiteralToOldSyntax() {
		setAbapReleaseOfCode("753");
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);
		
		buildSrc("    ADD 1 TO lv_value.");
		buildSrc("    SUBTRACT 1 FROM cv_date.");
		buildSrc("    MULTIPLY iv_value BY 2.");
		buildSrc("    DIVIDE iv_value BY 2.");

		buildExp("    lv_value = lv_value + 1.");
		buildExp("    cv_date = cv_date - 1.");
		buildExp("    iv_value = iv_value * 2.");
		buildExp("    iv_value = iv_value / 2.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOperandWithComponentAccessToOldSyntax() {
		setAbapReleaseOfCode("753");
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);
		
		buildSrc("    ADD lo_typedesc->length TO lv_length.");
		buildSrc("    SUBTRACT lo_typedesc->length FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY lo_struc-component.");
		buildSrc("    DIVIDE lv_value BY lo_struc-component.");

		buildExp("    lv_length = lv_length + lo_typedesc->length.");
		buildExp("    lv_length = lv_length - lo_typedesc->length.");
		buildExp("    lv_value = lv_value * lo_struc-component.");
		buildExp("    lv_value = lv_value / lo_struc-component.");
		
		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testOperandWithTableExpressionToOldSyntax() {
		setAbapReleaseOfCode("753");
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);
		
		buildSrc("    ADD lts_table[ num  = 5");
		buildSrc("                   name = 'abc']-length TO lv_length.");
		buildSrc("    SUBTRACT lts_table[ num  = 5");
		buildSrc("                        name = 'abc']-length FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY lts_table[ num  = 5");
		buildSrc("                                    name = 'abc']-component.");
		buildSrc("    DIVIDE lv_value BY lts_table[ num  = 5");
		buildSrc("                                  name = 'abc']-component.");

		buildExp("    lv_length = lv_length + lts_table[ num  = 5");
		buildExp("                                       name = 'abc']-length.");
		buildExp("    lv_length = lv_length - lts_table[ num  = 5");
		buildExp("                                       name = 'abc']-length.");
		buildExp("    lv_value = lv_value * lts_table[ num  = 5");
		buildExp("                                     name = 'abc']-component.");
		buildExp("    lv_value = lv_value / lts_table[ num  = 5");
		buildExp("                                     name = 'abc']-component.");

		putAnyMethodAroundSrcAndExp();
		
		// actually, calls or other expressions are NOT allowed in the obsolete commands!
		deactivateSyntaxCheckAfterParse();
		testRule();
	}

	@Test
	void testOperandWithCallChainToOldSyntax() {
		setReleaseRestrictionFromUI(753);
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);

		buildSrc("    ADD class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                               iv_param2 = 'abc' ) TO lv_length.");
		buildSrc("    SUBTRACT class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                    iv_param2 = 'abc' ) FROM lv_length.");
		buildSrc("    MULTIPLY lv_value BY class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                                iv_param2 = 'abc' ).");
		buildSrc("    DIVIDE lv_value BY class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildSrc("                                                              iv_param2 = 'abc' ).");

		buildExp("    lv_length = lv_length + class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                                   iv_param2 = 'abc' ).");
		buildExp("    lv_length = lv_length - class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                                   iv_param2 = 'abc' ).");
		buildExp("    lv_value = lv_value * class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                                 iv_param2 = 'abc' ).");
		buildExp("    lv_value = lv_value / class_name( )=>get_tool( )->get_value( iv_param  = 5");
		buildExp("                                                                 iv_param2 = 'abc' ).");
		
		putAnyMethodAroundSrcAndExp();
		
		// actually, calls or other expressions are NOT allowed in the obsolete commands!
		deactivateSyntaxCheckAfterParse();
		testRule();
	}

	@Test
	void testAssignmentTargetWithComponentAccessToOldSyntax() {
		setReleaseRestrictionFromUI(753);
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);

		buildSrc("    ADD 1 TO ls_struc-component.");
		buildSrc("    SUBTRACT 1 FROM ls_struc-component.");

		buildExp("    ls_struc-component = ls_struc-component + 1.");
		buildExp("    ls_struc-component = ls_struc-component - 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testAssignmentTargetWithOffsetAndLengthToOldSyntax() {
		setReleaseRestrictionFromUI(753);
		rule.configReplacementStyleForOldRelease.setEnumValue(AddToReplacementStyleForOldRelease.REPLACE_WITHOUT_ASSIGNMENT_OP);

		buildSrc("    ADD 1 TO lv_abc_date+4(2).");
		buildSrc("    SUBTRACT 1 FROM lv_abc_date+4(2).");
		
		buildExp("    lv_abc_date+4(2) = lv_abc_date+4(2) + 1.");
		buildExp("    lv_abc_date+4(2) = lv_abc_date+4(2) - 1.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testCommentAfterAddOrSubtract() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD \" comment");
		buildSrc("       1 TO lv_value.");
		buildSrc("    SUBTRACT \" comment");
		buildSrc("       1 FROM cv_date.");

		buildExp("    lv_value += \" comment");
		buildExp("       1.");
		buildExp("    cv_date -= \" comment");
		buildExp("       1.");

		testRule();
	}

	@Test
	void testCommentAfterMultiplyOrDivide() {
		// ensure the Commands are unchanged - otherwise, the comment would have to be moved up to an own command

		buildSrc("    MULTIPLY \" comment");
		buildSrc("       iv_value BY 2.");
		buildSrc("    DIVIDE \" comment");
		buildSrc("       iv_value BY 2.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCommentAfterTerm() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD 1 \" comment");
		buildSrc("       TO lv_value.");
		buildSrc("    SUBTRACT 1 \" comment");
		buildSrc("       FROM cv_date.");
		buildSrc("    MULTIPLY iv_value \" comment");
		buildSrc("       BY 2.");
		buildSrc("    DIVIDE iv_value \" comment");
		buildSrc("       BY 2.");

		buildExp("    lv_value += 1 \" comment");
		buildExp("       .");
		buildExp("    cv_date -= 1 \" comment");
		buildExp("       .");
		buildExp("    iv_value \" comment");
		buildExp("       *= 2.");
		buildExp("    iv_value \" comment");
		buildExp("       /= 2.");

		testRule();
	}

	@Test
	void testCommentAfterSecondKeyword() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD 1 TO \" comment");
		buildSrc("       lv_value.");
		buildSrc("    SUBTRACT 1 FROM \" comment");
		buildSrc("       cv_date.");
		buildSrc("    MULTIPLY iv_value BY \" comment");
		buildSrc("       2.");
		buildSrc("    DIVIDE iv_value BY \" comment");
		buildSrc("       2.");

		buildExp("    lv_value += 1 \" comment");
		buildExp("       .");
		buildExp("    cv_date -= 1 \" comment");
		buildExp("       .");
		buildExp("    iv_value *= \" comment");
		buildExp("       2.");
		buildExp("    iv_value /= \" comment");
		buildExp("       2.");

		testRule();
	}

	@Test
	void testCommentAfterDestVariable() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD 1 TO lv_value \" comment");
		buildSrc("      .");
		buildSrc("    SUBTRACT 1 FROM cv_date \" comment");
		buildSrc("      .");
		buildSrc("    MULTIPLY iv_value BY 2 \" comment");
		buildSrc("      .");
		buildSrc("    DIVIDE iv_value BY 2 \" comment");
		buildSrc("      .");

		buildExp("    lv_value += 1 \" comment");
		buildExp("      .");
		buildExp("    cv_date -= 1 \" comment");
		buildExp("      .");
		buildExp("    iv_value *= 2 \" comment");
		buildExp("      .");
		buildExp("    iv_value /= 2 \" comment");
		buildExp("      .");

		testRule();
	}

	@Test
	void testCommentsAfter() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD \" comment");
		buildSrc("       1 \" comment");
		buildSrc("        TO \" comment");
		buildSrc("        lv_value.");
		buildSrc("    SUBTRACT \" comment");
		buildSrc("       1 \" comment");
		buildSrc("        FROM \" comment");
		buildSrc("        cv_date.");
		buildSrc("    MULTIPLY iv_value \" comment");
		buildSrc("        BY \" comment");
		buildSrc("        2.");
		buildSrc("    DIVIDE iv_value \" comment");
		buildSrc("        BY \" comment");
		buildSrc("        2.");

		buildExp("    lv_value += \" comment");
		buildExp("       1 \" comment");
		buildExp("        \" comment");
		buildExp("        .");
		buildExp("    cv_date -= \" comment");
		buildExp("       1 \" comment");
		buildExp("        \" comment");
		buildExp("        .");
		buildExp("    iv_value \" comment");
		buildExp("        *= \" comment");
		buildExp("        2.");
		buildExp("    iv_value \" comment");
		buildExp("        /= \" comment");
		buildExp("        2.");

		testRule();
	}

	@Test
	void testCommentAfterPeriod() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD 1 TO lv_value. \" comment");
		buildSrc("    SUBTRACT 1 FROM cv_date. \" comment");
		buildSrc("    MULTIPLY iv_value BY 2. \" comment");
		buildSrc("    DIVIDE iv_value BY 2. \" comment");

		buildExp("    lv_value += 1. \" comment");
		buildExp("    cv_date -= 1. \" comment");
		buildExp("    iv_value *= 2. \" comment");
		buildExp("    iv_value /= 2. \" comment");

		testRule();
	}

	@Test
	void testCommentsEverywhere() {
		// ensure referential integrity is not broken (regardless of layout)

		buildSrc("    ADD \" comment");
		buildSrc("        1 \" comment");
		buildSrc("        TO \" comment");
		buildSrc("        lv_value. \" comment");
		buildSrc("    SUBTRACT \" comment");
		buildSrc("        1 \" comment");
		buildSrc("        FROM \" comment");
		buildSrc("        cv_date. \" comment");
		buildSrc("    MULTIPLY iv_value \" comment");
		buildSrc("        BY \" comment");
		buildSrc("        2. \" comment");
		buildSrc("    DIVIDE iv_value \" comment");
		buildSrc("        BY \" comment");
		buildSrc("        2. \" comment");

		buildExp("    lv_value += \" comment");
		buildExp("        1 \" comment");
		buildExp("        \" comment");
		buildExp("        . \" comment");
		buildExp("    cv_date -= \" comment");
		buildExp("        1 \" comment");
		buildExp("        \" comment");
		buildExp("        . \" comment");
		buildExp("    iv_value \" comment");
		buildExp("        *= \" comment");
		buildExp("        2. \" comment");
		buildExp("    iv_value \" comment");
		buildExp("        /= \" comment");
		buildExp("        2. \" comment");

		testRule();
	}
}
