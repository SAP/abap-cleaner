package com.sap.adt.abapcleaner.rules.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class EmptyLinesInClassDefinitionTest extends RuleTestBase {
	private EmptyLinesInClassDefinitionRule rule;
	
	EmptyLinesInClassDefinitionTest() {
		super(RuleID.EMPTY_LINES_IN_CLASS_DEFINITION);
		rule = (EmptyLinesInClassDefinitionRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configMaxEmptyLines.setValue(1);
		rule.configAddLineBetweenDefTypes.setEnumValue(AddLineBetweenDefTypesStyle.ADD_CONSIDER_STATIC); 
		rule.configAddEmptyLineAboveSections.setValue(true);
		rule.configRemoveEmptyLineBelowSections.setValue(true);
		rule.configRemoveEmptyLineAboveEndClass.setValue(true);
		rule.configRemoveIncludeWarnings.setValue(true);
	}
	

	@Test
	void testMax1EmptyLine() {
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    DATA mo_any   TYPE REF TO if_any_class.");
		buildSrc("    DATA ms_other TYPE ty_s_other.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("    ALIASES any_methody");
		buildExp("      FOR if_any_class~any_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_any   TYPE REF TO if_any_class.");
		buildExp("    DATA ms_other TYPE ty_s_other.");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}


	@Test
	void testKeepEmtyLinesBetweenClasses() {
		// ensure that between two CLASS definitions (with or without comments), no empty lines are removed
		buildSrc("CLASS cl_any_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_other_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("\" comment");
		buildSrc("CLASS cl_third_class DEFINITION.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}


	@Test
	void testMax1EmptyLineInChains() {
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES: if_any_class,");
		buildSrc("");
		buildSrc("");
		buildSrc("      if_other.");
		buildSrc("    ALIASES: any_methody");
		buildSrc("      FOR if_any_class~any_method,");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    other_method");
		buildSrc("      FOR if_any_class~other_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("    DATA: mo_any   TYPE REF TO if_any_class,");
		buildSrc("");
		buildSrc("          ms_other TYPE ty_s_other.");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("    METHODS other_protected_method.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("");
		buildSrc("");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("    CONSTANTS: gc_any_private_const   TYPE i VALUE 1,");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("               gc_other_private_const TYPE i VALUE 2.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES: if_any_class,");
		buildExp("");
		buildExp("      if_other.");
		buildExp("");
		buildExp("    ALIASES: any_methody");
		buildExp("      FOR if_any_class~any_method,");
		buildExp("");
		buildExp("    other_method");
		buildExp("      FOR if_any_class~other_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    CLASS-DATA mv_any_static_text TYPE string.");
		buildExp("");
		buildExp("    DATA: mo_any   TYPE REF TO if_any_class,");
		buildExp("");
		buildExp("          ms_other TYPE ty_s_other.");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("    METHODS other_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    TYPES:");
		buildExp("      ty_any   TYPE char10,");
		buildExp("");
		buildExp("      ty_other TYPE char20.");
		buildExp("");
		buildExp("    CONSTANTS: gc_any_private_const   TYPE i VALUE 1,");
		buildExp("");
		buildExp("               gc_other_private_const TYPE i VALUE 2.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testMax2EmptyLines() {
		rule.configMaxEmptyLines.setValue(2);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    DATA mo_any   TYPE REF TO if_any_class.");
		buildSrc("    DATA ms_other TYPE ty_s_other.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("    ALIASES any_methody");
		buildExp("      FOR if_any_class~any_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mo_any   TYPE REF TO if_any_class.");
		buildExp("    DATA ms_other TYPE ty_s_other.");
		buildExp("");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testMax3EmptyLines() {
		rule.configMaxEmptyLines.setValue(3);
		rule.configAddEmptyLineAboveSections.setValue(false);
		rule.configRemoveEmptyLineBelowSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    DATA mo_any   TYPE REF TO if_any_class.");
		buildSrc("    DATA ms_other TYPE ty_s_other.");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAddEmptyLineAboveSectionsOnly() {
		rule.configRemoveEmptyLineBelowSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION");
		buildSrc("  PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION");
		buildExp("  PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("");
		buildExp("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testDoNotAddEmptyLineAfterClassDef1Liner() {
		// ensure that no empty line is added above 'PUBLIC SECTION' if the CLASS .... DEFITINION is a one-liner
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("ENDCLASS.");
		buildSrc("");
		buildSrc("");
		buildSrc("CLASS cl_other_class DEFINITION.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    DATA mv_any TYPE i.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("ENDCLASS.");
		buildExp("");
		buildExp("");
		buildExp("CLASS cl_other_class DEFINITION.");
		buildExp("  PROTECTED SECTION.");
		buildExp("    DATA mv_any TYPE i.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveEmptyLineBelowSectionsOnly() {
		rule.configAddEmptyLineAboveSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    METHODS any_protected_method.");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}

	
	@Test
	void testDoNotRemoveEmptyLineBetweenTwoSections() {
		// ensure that the empty line between PROTECTED SECTION and PRIVATE SECTION is kept
		
		rule.configAddEmptyLineAboveSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("");
		buildExp("ENDCLASS.");

		testRule();
	}

	
	@Test
	void testAddEmptyLinesAboveAndRemoveBelowSections() {
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("");
		buildSrc("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    METHODS any_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    CONSTANTS gc_any_private_const TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}


	@Test
	void testRemoveDoNotIncludeComments() {
		rule.configAddLineBetweenDefTypes.setEnumValue(AddLineBetweenDefTypesStyle.NEVER);

		buildSrc("CLASS cl_any_class DEFINITION");
		buildSrc("  PUBLIC");
		buildSrc("  CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("*\"* public components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*\"* protected components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("*\"* private components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION");
		buildExp("  PUBLIC");
		buildExp("  CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("");
		buildExp("    ALIASES any_methody");
		buildExp("      FOR if_any_class~any_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    CLASS-DATA mv_any_static_text TYPE string.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    TYPES:");
		buildExp("      ty_any   TYPE char10,");
		buildExp("      ty_other TYPE char20.");
		buildExp("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildExp("ENDCLASS.");

		testRule();
	}


	@Test
	void testKeepModifiedDoNotIncludeComments() {
		// ensure that modified comments (with more text at the end, changed *"* prefix, changed text, changed casing etc. are kept
		
		rule.configAddLineBetweenDefTypes.setEnumValue(AddLineBetweenDefTypesStyle.NEVER);
		rule.configAddEmptyLineAboveSections.setValue(false);
		rule.configRemoveEmptyLineBelowSections.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION");
		buildSrc("  PUBLIC");
		buildSrc("  CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("*\"* public component of class CL_ANY_CLASS"); // 'component' instead of 'components'
		buildSrc("*\"* do not include other source files here!!"); // '!!' instead of '!!!'
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*\"* protected components of class CL_ANY_CLASS and more comment"); // 'and more comment' added
		buildSrc("*   do NOT include other source files here!!!"); // 'NOT' instead of 'not'
		buildSrc("");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("*\"* internal components of class CL_ANY_CLASS"); // 'internal' instead of 'private'
		buildSrc("*\"* do not include other source files here!!! with added comment"); // 'with added comment' added
		buildSrc("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}


	@Test
	void testKeepDoNotIncludeComments() {
		rule.configAddLineBetweenDefTypes.setEnumValue(AddLineBetweenDefTypesStyle.NEVER);
		rule.configAddEmptyLineAboveSections.setValue(false);
		rule.configRemoveEmptyLineBelowSections.setValue(false);
		rule.configRemoveIncludeWarnings.setValue(false);

		buildSrc("CLASS cl_any_class DEFINITION");
		buildSrc("  PUBLIC");
		buildSrc("  CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("");
		buildSrc("*\"* public components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("*\"* protected components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("*\"* private components of class CL_ANY_CLASS");
		buildSrc("*\"* do not include other source files here!!!");
		buildSrc("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}


	@Test
	void testAddLineBetweenDefTypesConsiderStatic() {
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("    INTERFACES if_other.");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("    ALIASES other_method");
		buildSrc("      FOR if_any_class~other_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("    DATA mo_any   TYPE REF TO if_any_class.");
		buildSrc("    DATA ms_other TYPE ty_s_other.");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("    METHODS other_protected_method.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildSrc("    CONSTANTS gc_other_private_const TYPE i VALUE 2.");
		buildSrc("    \" comment on static methods");
		buildSrc("    CLASS-METHODS: any_static_private_method,");
		buildSrc("      other_static_private_method.");
		buildSrc("    METHODS any_instance_private_method.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("    INTERFACES if_other.");
		buildExp("");
		buildExp("    ALIASES any_methody");
		buildExp("      FOR if_any_class~any_method .");
		buildExp("    ALIASES other_method");
		buildExp("      FOR if_any_class~other_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    CLASS-DATA mv_any_static_text TYPE string.");
		buildExp("");
		buildExp("    DATA mo_any   TYPE REF TO if_any_class.");
		buildExp("    DATA ms_other TYPE ty_s_other.");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("    METHODS other_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    TYPES:");
		buildExp("      ty_any   TYPE char10,");
		buildExp("      ty_other TYPE char20.");
		buildExp("");
		buildExp("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildExp("    CONSTANTS gc_other_private_const TYPE i VALUE 2.");
		buildExp("");
		buildExp("    \" comment on static methods");
		buildExp("    CLASS-METHODS: any_static_private_method,");
		buildExp("      other_static_private_method.");
		buildExp("");
		buildExp("    METHODS any_instance_private_method.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testAddLineBetweenDefTypesIgnoreStatic() {
		// ensure that CLASS-DATA and DATA, as well as CLASS-METHODS and METHODS, are kept together
		rule.configAddLineBetweenDefTypes.setEnumValue(AddLineBetweenDefTypesStyle.ADD_IGNORE_STATIC); 

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    INTERFACES if_any_class.");
		buildSrc("    INTERFACES if_other.");
		buildSrc("    ALIASES any_methody");
		buildSrc("      FOR if_any_class~any_method .");
		buildSrc("    ALIASES other_method");
		buildSrc("      FOR if_any_class~other_method .");
		buildSrc("");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("    CLASS-DATA mv_any_static_text TYPE string.");
		buildSrc("    DATA mo_any   TYPE REF TO if_any_class.");
		buildSrc("    DATA ms_other TYPE ty_s_other.");
		buildSrc("    METHODS any_protected_method.");
		buildSrc("    METHODS other_protected_method.");
		buildSrc("");
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    TYPES:");
		buildSrc("      ty_any   TYPE char10,");
		buildSrc("      ty_other TYPE char20.");
		buildSrc("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildSrc("    CONSTANTS gc_other_private_const TYPE i VALUE 2.");
		buildSrc("    \" comment on static methods");
		buildSrc("    CLASS-METHODS: any_static_private_method,");
		buildSrc("      other_static_private_method.");
		buildSrc("    METHODS any_instance_private_method.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("    INTERFACES if_any_class.");
		buildExp("    INTERFACES if_other.");
		buildExp("");
		buildExp("    ALIASES any_methody");
		buildExp("      FOR if_any_class~any_method .");
		buildExp("    ALIASES other_method");
		buildExp("      FOR if_any_class~other_method .");
		buildExp("");
		buildExp("  PROTECTED SECTION.");
		buildExp("    CLASS-DATA mv_any_static_text TYPE string.");
		buildExp("    DATA mo_any   TYPE REF TO if_any_class.");
		buildExp("    DATA ms_other TYPE ty_s_other.");
		buildExp("");
		buildExp("    METHODS any_protected_method.");
		buildExp("    METHODS other_protected_method.");
		buildExp("");
		buildExp("  PRIVATE SECTION.");
		buildExp("    TYPES:");
		buildExp("      ty_any   TYPE char10,");
		buildExp("      ty_other TYPE char20.");
		buildExp("");
		buildExp("    CONSTANTS gc_any_private_const   TYPE i VALUE 1.");
		buildExp("    CONSTANTS gc_other_private_const TYPE i VALUE 2.");
		buildExp("");
		buildExp("    \" comment on static methods");
		buildExp("    CLASS-METHODS: any_static_private_method,");
		buildExp("      other_static_private_method.");
		buildExp("    METHODS any_instance_private_method.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testAddLineBetweenDefTypesWithInclude() {
		// ensure that no empty line is added between the Commands with 'TYPES' and 'INCLUDE' keywords 
		
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("  TYPES:");
		buildSrc("    BEGIN OF ty_s_any_struc,");
		buildSrc("      component_a      TYPE i,");
		buildSrc("      component_b_long TYPE i.");
		buildSrc("      INCLUDE TYPE ty_s_include.");
		buildSrc("  TYPES:");
		buildSrc("      component_c_long TYPE i,");
		buildSrc("      component_d      TYPE i,");
		buildSrc("    END OF ty_s_any_struc.");
		buildSrc("  DATA mv_any TYPE i.");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("");
		buildExp("  PUBLIC SECTION.");
		buildExp("  TYPES:");
		buildExp("    BEGIN OF ty_s_any_struc,");
		buildExp("      component_a      TYPE i,");
		buildExp("      component_b_long TYPE i.");
		buildExp("      INCLUDE TYPE ty_s_include.");
		buildExp("  TYPES:");
		buildExp("      component_c_long TYPE i,");
		buildExp("      component_d      TYPE i,");
		buildExp("    END OF ty_s_any_struc.");
		buildExp("");
		buildExp("  DATA mv_any TYPE i.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testRemoveEmptyLineAboveEndClass() {
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    DATA mv_any TYPE i.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		buildExp("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildExp("  PUBLIC SECTION.");
		buildExp("    DATA mv_any TYPE i.");
		buildExp("ENDCLASS.");

		testRule();
	}

	@Test
	void testKeepEmptyLineBetweenSectionAndEndClass() {
		// ensure that an empty line above ENDCLASS is kept if the section above ENDCLASS is empty

		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PROTECTED SECTION.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testKeepEmptyLineAboveEndClass() {
		rule.configRemoveEmptyLineAboveEndClass.setValue(false);
		
		buildSrc("CLASS cl_any_class DEFINITION PUBLIC CREATE PROTECTED.");
		buildSrc("  PUBLIC SECTION.");
		buildSrc("    DATA mv_any TYPE i.");
		buildSrc("");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}
}
