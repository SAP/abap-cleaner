package com.sap.adt.abapcleaner.rules.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EmptyLinesOutsideMethodsTest extends RuleTestBase {
	private EmptyLinesOutsideMethodsRule rule;
	
	EmptyLinesOutsideMethodsTest() {
		super(RuleID.EMPTY_LINES_OUTSIDE_METHODS);
		rule = (EmptyLinesOutsideMethodsRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configEmptyLinesBetweenMethods.setValue(2);
	   rule.configEmptyLinesBetweenClasses.setValue(3);
	   rule.configEmptyLinesBetweenClassAndMethod.setValue(0);
	}
	
	@Test
	void testAdd3LinesBetweenClasses() {
		buildSrc("CLASS cl_empty DEFINITION.");
		buildSrc("  \" definition code");
		buildSrc("ENDCLASS.");
		buildSrc("CLASS cl_empty IMPLEMENTATION.");
		buildSrc("  \" implementation code");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty DEFINITION.");
		buildExp("  \" definition code");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_empty IMPLEMENTATION.");
		buildExp("  \" implementation code");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd3LinesBetweenInterfaces() {
		buildSrc("INTERFACE lif_any PUBLIC.");
		buildSrc("  \" definition");
		buildSrc("ENDINTERFACE.");
		buildSrc("INTERFACE lif_other PUBLIC.");
		buildSrc("  \" definition");
		buildSrc("ENDINTERFACE.");

		buildExp("INTERFACE lif_any PUBLIC.");
		buildExp("  \" definition");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("INTERFACE lif_other PUBLIC.");
		buildExp("  \" definition");
		buildExp("ENDINTERFACE.");
		
		testRule();
	}

	@Test
	void testAdd3LinesBetweenClassesAndInterfaces() {
		buildSrc("CLASS cl_empty DEFINITION.");
		buildSrc("  \" definition code");
		buildSrc("ENDCLASS.");
		buildSrc("INTERFACE lif_empty PUBLIC.");
		buildSrc("  \" definition");
		buildSrc("ENDINTERFACE.");
		buildSrc("CLASS cl_empty IMPLEMENTATION.");
		buildSrc("  \" implementation");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty DEFINITION.");
		buildExp("  \" definition code");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("INTERFACE lif_empty PUBLIC.");
		buildExp("  \" definition");
		buildExp("ENDINTERFACE.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_empty IMPLEMENTATION.");
		buildExp("  \" implementation");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd1LineBetweenClasses() {
	   rule.configEmptyLinesBetweenClasses.setValue(1);

	   buildSrc("CLASS cl_empty DEFINITION.");
		buildSrc("  \" definition code");
		buildSrc("ENDCLASS.");
		buildSrc("CLASS cl_empty IMPLEMENTATION.");
		buildSrc("  \" implementation code");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty DEFINITION.");
		buildExp("  \" definition code");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_empty IMPLEMENTATION.");
		buildExp("  \" implementation code");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveLinesBetweenClasses() {
		buildSrc("CLASS cl_empty IMPLEMENTATION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_empty_lines DEFINITION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty IMPLEMENTATION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_empty_lines DEFINITION.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveAllLinesBetweenClasses() {
	   rule.configEmptyLinesBetweenClasses.setValue(1);

	   buildSrc("CLASS cl_empty IMPLEMENTATION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_empty_lines DEFINITION.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty IMPLEMENTATION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("CLASS cl_empty_lines DEFINITION.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveLinesBetweenClassesWithComment() {
		buildSrc("CLASS cl_empty_lines DEFINITION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("\" several comment lines");
		buildSrc("\" that belong to the class");
		buildSrc("CLASS tests DEFINITION PUBLIC.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines DEFINITION.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("\" several comment lines");
		buildExp("\" that belong to the class");
		buildExp("CLASS tests DEFINITION PUBLIC.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd2LinesBetweenMethods() {
		buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines_between_methods_1.");
		buildSrc("    \" code");
		buildSrc("  ENDMETHOD.");
		buildSrc("  METHOD empty_lines_between_methods_2.");
		buildSrc("    \" more code");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("  METHOD empty_lines_between_methods_1.");
		buildExp("    \" code");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("");
		buildExp("  METHOD empty_lines_between_methods_2.");
		buildExp("    \" more code");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd1LineBetweenMethods() {
		rule.configEmptyLinesBetweenMethods.setValue(1);

		buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines_between_methods_1.");
		buildSrc("    \" code");
		buildSrc("  ENDMETHOD.");
		buildSrc("  METHOD empty_lines_between_methods_2.");
		buildSrc("    \" more code");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("  METHOD empty_lines_between_methods_1.");
		buildExp("    \" code");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  METHOD empty_lines_between_methods_2.");
		buildExp("    \" more code");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveLinesBetweenMethods() {
		buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines_between_methods_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("  METHOD empty_lines_between_methods_3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("  METHOD empty_lines_between_methods_2.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("");
		buildExp("  METHOD empty_lines_between_methods_3.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveAllLinesBetweenMethods() {
		rule.configEmptyLinesBetweenMethods.setValue(1);

		buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines_between_methods_2.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("");
		buildSrc("  \" comment");
		buildSrc("  METHOD empty_lines_between_methods_3.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("  METHOD empty_lines_between_methods_2.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("  \" comment");
		buildExp("  METHOD empty_lines_between_methods_3.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testRemoveLinesBetweenClassAndMethod() {
	   rule.configEmptyLinesBetweenClassAndMethod.setValue(0);

	   buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("");
		buildSrc("");
		buildSrc("  METHOD empty_lines.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("  METHOD empty_lines.");
		buildExp("  ENDMETHOD.");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd1LineBetweenClassAndMethod() {
	   rule.configEmptyLinesBetweenClassAndMethod.setValue(1);

	   buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("");
		buildExp("  METHOD empty_lines.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAdd2LinesBetweenClassAndMethod() {
	   rule.configEmptyLinesBetweenClassAndMethod.setValue(2);

	   buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("");
		buildExp("");
		buildExp("  METHOD empty_lines.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("");
		buildExp("ENDCLASS.");
		
		testRule();
	}

	@Test
	void testAddAndRemoveLineBetweenClassAndMethod() {
	   rule.configEmptyLinesBetweenClassAndMethod.setValue(1);

	   buildSrc("CLASS cl_empty_lines IMPLEMENTATION.");
		buildSrc("  METHOD empty_lines.");
		buildSrc("  ENDMETHOD.");
		buildSrc("");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_empty_lines IMPLEMENTATION.");
		buildExp("");
		buildExp("  METHOD empty_lines.");
		buildExp("  ENDMETHOD.");
		buildExp("");
		buildExp("ENDCLASS.");
		
		testRule();
	}
}
