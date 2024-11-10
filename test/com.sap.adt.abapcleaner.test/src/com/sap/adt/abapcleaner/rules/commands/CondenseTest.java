package com.sap.adt.abapcleaner.rules.commands;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class CondenseTest extends RuleTestBase {
	private CondenseRule rule;
	
	CondenseTest() {
		super(RuleID.CONDENSE);
		rule = (CondenseRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configSpecifyValName.setValue(false);
	   rule.configSpecifyDel.setValue(false);
	   rule.configSpecifyFromForNoGaps.setValue(true);
		rule.configKeepParamsOnOneLine.setValue(false);
		rule.configSkipUnknownTypes.setValue(false);
	}

	@Test
	void testIsConfigValueEnabled() {
		assertTrue(rule.isConfigValueEnabled(rule.configKeepParamsOnOneLine));

		rule.configSkipUnknownTypes.setValue(true);
		assertFalse(rule.isConfigValueEnabled(rule.configUnknownTypeWarning));

		rule.configSkipUnknownTypes.setValue(false);
		assertTrue(rule.isConfigValueEnabled(rule.configUnknownTypeWarning));
	}

	@Test
	void testSpecifyValName() {
		rule.configSpecifyValName.setValue(true);

		buildSrc("    \" comment");
		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    \" comment");
		buildExp("    lv_text_a = condense( val = lv_text_a ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyFromForNoGaps() {
		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( lv_text_a ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotSpecifyFromForNoGaps() {
		rule.configSpecifyFromForNoGaps.setValue(false);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( lv_text_a ).");
		buildExp("    lv_text_b = condense( val = lv_text_b");
		buildExp("                          to  = `` ).");
		buildExp("    lv_text_c+5(7) = condense( lv_text_c+5(7) ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyDel() {
		rule.configSpecifyDel.setValue(true);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a");
		buildExp("                          del = ` ` ).");
		buildExp("    lv_text_b = condense( val  = lv_text_b");
		buildExp("                          from = ` `");
		buildExp("                          to   = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7)");
		buildExp("                               del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSpecifyDelButNotFrom() {
		rule.configSpecifyDel.setValue(true);
		rule.configSpecifyFromForNoGaps.setValue(false);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a");
		buildExp("                          del = ` ` ).");
		buildExp("    lv_text_b = condense( val = lv_text_b");
		buildExp("                          to  = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7)");
		buildExp("                               del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCommentsAndPragmasUnchanged() {
		buildSrc("    CONDENSE \" comment");
		buildSrc("             lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS ##ANY_PRAGMA.");
		buildSrc("    CONDENSE");
		buildSrc("* comment line");
		buildSrc("             lv_text_c+5(7).");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKeepParamsOnOneLine() {
		rule.configSpecifyDel.setValue(true);
		rule.configKeepParamsOnOneLine.setValue(true);

		buildSrc("    CONDENSE lv_text_a.");
		buildSrc("    CONDENSE lv_text_b NO-GAPS.");
		buildSrc("    CONDENSE lv_text_c+5(7).");

		buildExp("    lv_text_a = condense( val = lv_text_a del = ` ` ).");
		buildExp("    lv_text_b = condense( val = lv_text_b from = ` ` to = `` ).");
		buildExp("    lv_text_c+5(7) = condense( val = lv_text_c+5(7) del = ` ` ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMacroDefinition() {
		buildSrc("DEFINE any_macro.");
		buildSrc("  CONDENSE &1.");
		buildSrc("  CONDENSE &1 NO-GAPS.");
		buildSrc("END-OF-DEFINITION.");

		buildExp("DEFINE any_macro.");
		buildExp("  &1 = condense( &1 ).");
		buildExp("  &1 = condense( val  = &1");
		buildExp("                 from = ` `");
		buildExp("                 to   = `` ).");
		buildExp("END-OF-DEFINITION.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUnknownStructuredTypesNotReplaced() {
		rule.configSkipUnknownTypes.setValue(true);
		
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("             field TYPE c LENGTH 10,");
		buildSrc("           END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA ls_structure   TYPE ty_s_any_struc.");
		buildSrc("    DATA l_unknown_type TYPE if_any_interface=>ty_unknown_type.");
		buildSrc("");
		buildSrc("    CONDENSE ls_structure.");
		buildSrc("    CONDENSE l_unknown_type.");

		copyExpFromSrc();

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testKnownStructuredTypesKept() {
		// expect 'CONDENSE ls_structure' to be kept even with this configuration, because its type is visible and structured
		
		buildSrc("    TYPES: BEGIN OF ty_s_any_struc,");
		buildSrc("             field TYPE c LENGTH 10,");
		buildSrc("           END OF ty_s_any_struc.");
		buildSrc("");
		buildSrc("    DATA ls_structure   TYPE ty_s_any_struc.");
		buildSrc("    DATA l_unknown_type TYPE if_any_interface=>ty_unknown_type.");
		buildSrc("");
		buildSrc("    CONDENSE ls_structure.");
		buildSrc("    CONDENSE l_unknown_type.");

		buildExp("    TYPES: BEGIN OF ty_s_any_struc,");
		buildExp("             field TYPE c LENGTH 10,");
		buildExp("           END OF ty_s_any_struc.");
		buildExp("");
		buildExp("    DATA ls_structure   TYPE ty_s_any_struc.");
		buildExp("    DATA l_unknown_type TYPE if_any_interface=>ty_unknown_type.");
		buildExp("");
		buildExp("    CONDENSE ls_structure.");
		buildExp("    l_unknown_type = condense( l_unknown_type ).");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}
}	