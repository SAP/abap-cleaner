package com.sap.adt.abapcleaner.rules.commands;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class ExitOutsideLoopTest extends RuleTestBase {
	private ExitOutsideLoopRule rule;
	
	ExitOutsideLoopTest() {
		super(RuleID.EXIT_OUTSIDE_LOOP);
		rule = (ExitOutsideLoopRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configApplyToProcedures.setValue(true);
		rule.configApplyToEventBlocks.setValue(true);
	}
	
	@Test
	void testApplyToMethod() {
		buildSrc("  METHOD any_method.");
		buildSrc("    DO 5 TIMES.");
		buildSrc("      IF sy-index = 4.");
		buildSrc("        \" comment");
		buildSrc("        EXIT.");
		buildSrc("      ENDIF.");
		buildSrc("    ENDDO.");
		buildSrc("");
		buildSrc("    CASE iv_action.");
		buildSrc("      WHEN 'exit'.");
		buildSrc("        EXIT.");
		buildSrc("      WHEN OTHERS.");
		buildSrc("        do_something( ).");
		buildSrc("    ENDCASE.");
		buildSrc("  ENDMETHOD.");

		buildExp("  METHOD any_method.");
		buildExp("    DO 5 TIMES.");
		buildExp("      IF sy-index = 4.");
		buildExp("        \" comment");
		buildExp("        EXIT.");
		buildExp("      ENDIF.");
		buildExp("    ENDDO.");
		buildExp("");
		buildExp("    CASE iv_action.");
		buildExp("      WHEN 'exit'.");
		buildExp("        RETURN.");
		buildExp("      WHEN OTHERS.");
		buildExp("        do_something( ).");
		buildExp("    ENDCASE.");
		buildExp("  ENDMETHOD.");

		testRule();
	}

	@Test
	void testDoNotApplyToMethod() {
		rule.configApplyToProcedures.setValue(false);

		buildSrc("  METHOD any_method.");
		buildSrc("    CASE iv_action.");
		buildSrc("      WHEN 'exit'.");
		buildSrc("        EXIT.");
		buildSrc("      WHEN OTHERS.");
		buildSrc("        do_something( ).");
		buildSrc("    ENDCASE.");
		buildSrc("  ENDMETHOD.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testApplyToForm() {
		buildSrc("FORM any_form.");
		buildSrc("  LOOP AT gt_any_table ASSIGNING <ls_any> WHERE comp1 > 10.");
		buildSrc("    EXIT.");
		buildSrc("  ENDLOOP.");
		buildSrc("  IF sy-subrc = 0.");
		buildSrc("    EXIT.");
		buildSrc("  ENDIF.");
		buildSrc("ENDFORM.");

		buildExp("FORM any_form.");
		buildExp("  LOOP AT gt_any_table ASSIGNING <ls_any> WHERE comp1 > 10.");
		buildExp("    EXIT.");
		buildExp("  ENDLOOP.");
		buildExp("  IF sy-subrc = 0.");
		buildExp("    RETURN.");
		buildExp("  ENDIF.");
		buildExp("ENDFORM.");

		testRule();
	}

	@Test
	void testDoNotApplyToForm() {
		rule.configApplyToProcedures.setValue(false);

		buildSrc("FORM any_form.");
		buildSrc("  IF sy-subrc = 0.");
		buildSrc("    EXIT.");
		buildSrc("  ENDIF.");
		buildSrc("ENDFORM.");

		copyExpFromSrc();

		testRule();
	}


	@Test
	void testDoNotApplyToSnippet() {
		buildSrc("    CASE iv_action.");
		buildSrc("      WHEN 'exit'.");
		buildSrc("        EXIT.");
		buildSrc("      WHEN OTHERS.");
		buildSrc("        do_something( ).");
		buildSrc("    ENDCASE.");

		copyExpFromSrc();

		testRule();
	}


	@Test
	void testApplyToEventBlock() {
		buildSrc("INITIALIZATION.");
		buildSrc("  IF a = b.");
		buildSrc("    EXIT.");
		buildSrc("  ENDIF.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  PERFORM any_form.");
		buildSrc("  EXIT.");
		buildSrc("  PERFORM unreachable_code.");

		buildExp("INITIALIZATION.");
		buildExp("  IF a = b.");
		buildExp("    RETURN.");
		buildExp("  ENDIF.");
		buildExp("");
		buildExp("START-OF-SELECTION.");
		buildExp("  PERFORM any_form.");
		buildExp("  RETURN.");
		buildExp("  PERFORM unreachable_code.");

		testRule();
	}

	@Test
	void testDoNotApplyToEventBlock() {
		rule.configApplyToEventBlocks.setValue(false);

		buildSrc("INITIALIZATION.");
		buildSrc("  IF a = b.");
		buildSrc("    EXIT.");
		buildSrc("  ENDIF.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  PERFORM any_form.");
		buildSrc("  EXIT.");
		buildSrc("  PERFORM unreachable_code.");

		copyExpFromSrc();

		testRule();
	}
}
