package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class EmptySectionsInClassDefTest extends RuleTestBase {
	private EmptySectionsInClassDefRule rule;
	
	EmptySectionsInClassDefTest() {
		super(RuleID.EMPTY_SECTIONS);
		rule = (EmptySectionsInClassDefRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_ANY_FROM_NON_EMPTY_CLASS);
	}

	@Test
	void testRemoveProtectedSectionOnly() {
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_PROTECTED_OF_FINAL_CLASS);

		buildSrc("CLASS cl_any_final_class DEFINITION FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("*\"* public components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    INTERFACES if_any_interface.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*\"* protected components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("*\"* private components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_final_class DEFINITION FINAL.");
		buildExp("  PUBLIC SECTION.");
		buildExp("*\"* public components of class CL_ANY_FINAL_CLASS");
		buildExp("*\"* do not include other source files here!!!");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("*\"* private components of class CL_ANY_FINAL_CLASS");
		buildExp("*\"* do not include other source files here!!!");
		buildExp("");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepProtectedSectionInNonFinalClass() {
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_PROTECTED_OF_FINAL_CLASS);

		buildSrc("CLASS cl_any_non_final_class DEFINITION FOR TESTING.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_any_data TYPE i.");
		buildSrc("");
		buildSrc("    METHODS setup.");
		buildSrc("    METHODS any_test_method FOR TESTING.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testRemovEmptySectionsWithComments() {
		buildSrc("CLASS cl_any_final_class DEFINITION FINAL.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("*\"* public components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    INTERFACES if_any_interface.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*\"* protected components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("*\"* private components of class CL_ANY_FINAL_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_final_class DEFINITION FINAL.");
		buildExp("  PUBLIC SECTION.");
		buildExp("*\"* public components of class CL_ANY_FINAL_CLASS");
		buildExp("*\"* do not include other source files here!!!");
		buildExp("    INTERFACES if_any_interface.");
		buildExp("");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemovEmptySectionsWithoutComments() {
		buildSrc("CLASS cl_any_non_final_class DEFINITION FOR TESTING.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_any_data TYPE i.");
		buildSrc("");
		buildSrc("    METHODS setup.");
		buildSrc("    METHODS any_test_method FOR TESTING.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_non_final_class DEFINITION FOR TESTING.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    DATA mv_any_data TYPE i.");
		buildExp("");
		buildExp("    METHODS setup.");
		buildExp("    METHODS any_test_method FOR TESTING.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepEmptySectionsInEmptyClass() {
		// expect the SECTIONs of the following class to be kept, since the class was probably freshly created with a template 
		buildSrc("CLASS cl_any_empty_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testRemoveEmptySectionsFromEmptyClass() {
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_ANY);

		buildSrc("CLASS cl_any_empty_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_empty_class DEFINITION.");
		buildExp("");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveEmptySectionsFromAlmostEmptyClass() {
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_ANY);

		buildSrc("CLASS cl_any_empty_class DEFINITION.");
		buildSrc("  \" comment outside the first SECTION");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_empty_class DEFINITION.");
		buildExp("  \" comment outside the first SECTION");
		buildExp("");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepNonEmptySections() {
		rule.configEmptySectionsAction.setEnumValue(EmptySectionsAction.REMOVE_ANY);

		buildSrc("CLASS cl_any_empty_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    \" comment");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*   asterisk comment");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    ##PRAGMA");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testPublicChildClassUnchanged() {
		// ensure that from a PUBLIC child class, empty sections are NOT removed, because a syntax error comes up in ADT 
		// when the protected base class constructor is called in the following scenario:
		// - the base class is "PUBLIC" and "CREATE PROTECTED",
		// - the base class constructor has parameters, 
		// - the child class is "PUBLIC", 
		// - the child class is "CREATE PROTECTED" (explicitly or implicitly inherited) or "CREATE PRIVATE", 
		// - the child class misses both the PROTECTED and the PRIVATE section. 

		buildSrc("CLASS cl_any_child DEFINITION PUBLIC FINAL");
		buildSrc("  INHERITING FROM cl_any_base.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    CLASS-METHODS create RETURNING VALUE(ro_instance) TYPE REF TO cl_any_child.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("CLASS cl_any_child IMPLEMENTATION.");
		buildSrc("  METHOD create.");
		buildSrc("    ro_instance = NEW #( iv_any = 1 ).");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testKeepAllSectionsInPublicNonFinalClass() {
		buildSrc("CLASS cl_any_public_non_final_class DEFINITION PUBLIC.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    DATA mv_any_data TYPE i.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testKeepAllSectionsInPublicNonFinalClass2() {
		buildSrc("CLASS cl_other_public_non_final_cls DEFINITION PUBLIC.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    DATA mv_any_data TYPE i.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}
}
