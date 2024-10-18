package com.sap.adt.abapcleaner.rules.declarations;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class AbapDocParametersTest extends RuleTestBase {
	private AbapDocParametersRule rule;
	
	AbapDocParametersTest() {
		super(RuleID.ABAP_DOC_PARAMETERS);
		rule = (AbapDocParametersRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAddParameters.setEnumValue(AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED);
		rule.configOnlyAddToExistingDetails.setValue(false);
		rule.configUpdateOrder.setValue(true);
		rule.configDeleteParameters.setEnumValue(DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY);
		rule.configDeleteExceptions.setEnumValue(DeleteObsoleteAbapDocType.IF_DESCRIPTION_EMPTY);
	}

	@Test
	void testAddAndDeleteNonSynchronized() {
		buildSrc("    \"! any method");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | any parameter");
		buildSrc("    \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception | any exception");
		buildSrc("    \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("    \"! @raising cx_also_obsolete |");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception,");
		buildSrc("");
		buildSrc("      undocumented_method,");
		buildSrc("");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		buildExp("    \"! any method");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | any parameter");
		buildExp("    \"! @parameter iv_other_parameter |");
		buildExp("    \"! @raising cx_any_exception | any exception");
		buildExp("    \"! @raising cx_other_exception |");
		buildExp("    \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildExp("    \"! @raising cx_obsolete_exception | obsolete exception");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("                iv_other_parameter TYPE i");
		buildExp("      RAISING   cx_any_exception");
		buildExp("                cx_other_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter | any parameter");
		buildExp("      \"! @parameter iv_other_parameter |");
		buildExp("      \"! @raising cx_any_exception | any exception");
		buildExp("      \"! @raising cx_other_exception |");
		buildExp("      chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("                  iv_other_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception");
		buildExp("                  cx_other_exception,");
		buildExp("");
		buildExp("      undocumented_method,");
		buildExp("");
		buildExp("      \"! other chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter | any parameter");
		buildExp("      \"! @parameter iv_other_parameter |");
		buildExp("      \"! @raising cx_any_exception | any exception");
		buildExp("      \"! @raising cx_other_exception |");
		buildExp("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildExp("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("                  iv_other_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception");
		buildExp("                  cx_other_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAddAndDeleteSynchronized() {
		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS undocumented_method");
		buildSrc("      IMPORTING iv_any_parameter TYPE i");
		buildSrc("      EXPORTING ev_any_parameter TYPE i.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! <p class=\"shorttext synchronized\">chained method</p>");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter ev_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("      \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("      chained_method");
		buildSrc("        EXPORTING ev_any_parameter   TYPE i");
		buildSrc("                  ev_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception,");
		buildSrc("");
		buildSrc("      \"! <p class=\"shorttext synchronized\">other chained method</p>");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter ev_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("      \"! @parameter ev_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("      \"! @parameter ev_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("      \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("      \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildSrc("      other_chained_method");
		buildSrc("        EXPORTING ev_any_parameter   TYPE i");
		buildSrc("                  ev_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\"></p>");
		buildExp("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("    \"! @raising cx_other_exception | <p class=\"shorttext synchronized\"></p>");
		buildExp("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildExp("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("                iv_other_parameter TYPE i");
		buildExp("      RAISING   cx_any_exception");
		buildExp("                cx_other_exception.");
		buildExp("");
		buildExp("    METHODS undocumented_method");
		buildExp("      IMPORTING iv_any_parameter TYPE i");
		buildExp("      EXPORTING ev_any_parameter TYPE i.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! <p class=\"shorttext synchronized\">chained method</p>");
		buildExp("      \"!");
		buildExp("      \"! @parameter ev_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("      \"! @parameter ev_other_parameter | <p class=\"shorttext synchronized\"></p>");
		buildExp("      \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("      \"! @raising cx_other_exception | <p class=\"shorttext synchronized\"></p>");
		buildExp("      chained_method");
		buildExp("        EXPORTING ev_any_parameter   TYPE i");
		buildExp("                  ev_other_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception");
		buildExp("                  cx_other_exception,");
		buildExp("");
		buildExp("      \"! <p class=\"shorttext synchronized\">other chained method</p>");
		buildExp("      \"!");
		buildExp("      \"! @parameter ev_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("      \"! @parameter ev_other_parameter | <p class=\"shorttext synchronized\"></p>");
		buildExp("      \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("      \"! @raising cx_other_exception | <p class=\"shorttext synchronized\"></p>");
		buildExp("      \"! @parameter ev_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildExp("      \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildExp("      other_chained_method");
		buildExp("        EXPORTING ev_any_parameter   TYPE i");
		buildExp("                  ev_other_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception");
		buildExp("                  cx_other_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAddSynchronized() {
		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! <p class=\"shorttext synchronized\">chained method</p>");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter ev_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("      \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("      chained_method");
		buildSrc("        EXPORTING ev_any_parameter   TYPE i");
		buildSrc("                  ev_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAddNonSynchronizedLine() {
		// expect a non-synchronized line to be added even to otherwise synchronized ABAP Doc;
		// also, expect the position of the ABAP Doc right after METHODS: (with missing header) to be kept
		// (such a line-end ABAP Doc creates a warning, but it works and is not a syntax error) 

		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS: \"! @parameter iv_other_parameter | other parameter");
		buildSrc("             \"! @raising cx_any_exception | any exception");
		buildSrc("             chained_method");
		buildSrc("               IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                         iv_other_parameter TYPE i");
		buildSrc("               RAISING   cx_any_exception");
		buildSrc("                         cx_other_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("    \"! @parameter iv_other_parameter |");
		buildExp("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("    \"! @raising cx_other_exception |");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("                iv_other_parameter TYPE i");
		buildExp("      RAISING   cx_any_exception");
		buildExp("                cx_other_exception.");
		buildExp("");
		buildExp("    METHODS: \"! @parameter iv_any_parameter |");
		buildExp("             \"! @parameter iv_other_parameter | other parameter");
		buildExp("             \"! @raising cx_any_exception | any exception");
		buildExp("             \"! @raising cx_other_exception |");
		buildExp("             chained_method");
		buildExp("               IMPORTING iv_any_parameter   TYPE i");
		buildExp("                         iv_other_parameter TYPE i");
		buildExp("               RAISING   cx_any_exception");
		buildExp("                         cx_other_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDeleteAllObsolete() {
		rule.configDeleteParameters.setEnumValue(DeleteObsoleteAbapDocType.ALWAYS);
		rule.configDeleteExceptions.setEnumValue(DeleteObsoleteAbapDocType.ALWAYS);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("      RAISING   cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("        RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("      RAISING   cx_any_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! other chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter | any parameter");
		buildExp("      \"! @raising cx_any_exception | any exception");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDeleteAllObsoleteKeepingHeaderOnly() {
		// expect the empty line below the header documentation to be removed if all parameter and exception lines were removed
		rule.configAddParameters.setEnumValue(AddAbapDocType.NEVER);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.NEVER);
		rule.configDeleteParameters.setEnumValue(DeleteObsoleteAbapDocType.ALWAYS);
		rule.configDeleteExceptions.setEnumValue(DeleteObsoleteAbapDocType.ALWAYS);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildSrc("    METHODS any_method.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    METHODS any_method.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! other chained method");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNeverDeleteObsolete() {
		rule.configDeleteParameters.setEnumValue(DeleteObsoleteAbapDocType.NEVER);
		rule.configDeleteExceptions.setEnumValue(DeleteObsoleteAbapDocType.NEVER);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("      RAISING   cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("        RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("    \"! @parameter iv_obsolete_parameter | <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildExp("    \"! @parameter iv_also_obsolete |");
		buildExp("    \"! @raising cx_obsolete_exception | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildExp("    \"! @raising cx_also_obsolete | <p class=\"shorttext synchronized\"></p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("      RAISING   cx_any_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! other chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter | any parameter");
		buildExp("      \"! @raising cx_any_exception | any exception");
		buildExp("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildExp("      \"! @parameter iv_also_obsolete |");
		buildExp("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildExp("      \"! @raising cx_also_obsolete |");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotAddToHeaderOnly() {
		// expect no added parameter and exception lines, because no such lines exist 
		rule.configOnlyAddToExistingDetails.setValue(true);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAddToHeader() {
		// expect parameter and exception lines to be added even if no other parameters and exceptions are documented yet
		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS);
		rule.configOnlyAddToExistingDetails.setValue(false); // pro forma

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | <p class=\"shorttext synchronized\"></p>");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\"></p>");
		buildExp("    \"! @raising cx_any_exception | <p class=\"shorttext synchronized\"></p>");
		buildExp("    \"! @raising cx_other_exception | <p class=\"shorttext synchronized\"></p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("                iv_other_parameter TYPE i");
		buildExp("      RAISING   cx_any_exception");
		buildExp("                cx_other_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! other chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter |");
		buildExp("      \"! @parameter iv_other_parameter |");
		buildExp("      \"! @raising cx_any_exception |");
		buildExp("      \"! @raising cx_other_exception |");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("                  iv_other_parameter TYPE i");
		buildExp("        RAISING   cx_any_exception");
		buildExp("                  cx_other_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testAllSettingsInactive() {
		// expect no change to happen if all parameters are in 'no change' state
		rule.configAddParameters.setEnumValue(AddAbapDocType.NEVER);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.NEVER);
		rule.configUpdateOrder.setValue(false);
		rule.configDeleteParameters.setEnumValue(DeleteObsoleteAbapDocType.NEVER);
		rule.configDeleteExceptions.setEnumValue(DeleteObsoleteAbapDocType.NEVER);
		
		buildSrc("    \"! any method");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter | any parameter");
		buildSrc("    \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception | any exception");
		buildSrc("    \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("    \"! @raising cx_also_obsolete |");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception,");
		buildSrc("");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @parameter iv_obsolete_parameter | obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      \"! @raising cx_obsolete_exception | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception.");

		putAnyClassDefAroundSrcAndExp();

		copyExpFromSrc();
		
		testRule();
	}

	@Test
	void testNonClassBasedExceptions() {
		// expect added parameters and exceptions to be non-synchronized even in otherwise synchronized ABAP Doc
		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);

		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\">other parameter</p>");
		buildSrc("    \"! @exception other_exception | <p class=\"shorttext synchronized\">other exception</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING  iv_any_parameter   TYPE i");
		buildSrc("                 iv_other_parameter TYPE i");
		buildSrc("      EXCEPTIONS any_exception");
		buildSrc("                 other_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_other_parameter | other parameter");
		buildSrc("      \"! @exception other_exception | other exception");
		buildSrc("      chained_method");
		buildSrc("        IMPORTING  iv_any_parameter   TYPE i");
		buildSrc("                   iv_other_parameter TYPE i");
		buildSrc("        EXCEPTIONS any_exception");
		buildSrc("                   other_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter |");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\">other parameter</p>");
		buildExp("    \"! @exception any_exception |");
		buildExp("    \"! @exception other_exception | <p class=\"shorttext synchronized\">other exception</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING  iv_any_parameter   TYPE i");
		buildExp("                 iv_other_parameter TYPE i");
		buildExp("      EXCEPTIONS any_exception");
		buildExp("                 other_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter |");
		buildExp("      \"! @parameter iv_other_parameter | other parameter");
		buildExp("      \"! @exception any_exception |");
		buildExp("      \"! @exception other_exception | other exception");
		buildExp("      chained_method");
		buildExp("        IMPORTING  iv_any_parameter   TYPE i");
		buildExp("                   iv_other_parameter TYPE i");
		buildExp("        EXCEPTIONS any_exception");
		buildExp("                   other_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testUpdateOrder() {
		// expect the order of parameter descriptions to be updated; 
		// also, expect the position of the ABAP Doc right after METHODS: to be kept 
		// (such a line-end ABAP Doc creates a warning, but it works and is not a syntax error) 

		buildSrc("    \"! any method");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_other_parameter | other parameter");
		buildSrc("    \"! @parameter iv_obsolete_parameter |");
		buildSrc("    \"! @parameter iv_any_parameter | any parameter");
		buildSrc("    \"! @raising cx_any_exception | any exception");
		buildSrc("    \"! @raising cx_third_exception | third exception");
		buildSrc("    \"! @raising cx_obsolete_exception |");
		buildSrc("    \"! @raising cx_other_exception | other exception");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception");
		buildSrc("                cx_third_exception.");
		buildSrc("");
		buildSrc("    METHODS: \"! chained method");
		buildSrc("             \"!");
		buildSrc("             \"! @parameter iv_other_parameter | other parameter");
		buildSrc("             \"! @parameter iv_any_parameter | any parameter");
		buildSrc("             \"! @parameter iv_obsolete_parameter |");
		buildSrc("             \"! @raising cx_obsolete_exception |");
		buildSrc("             \"! @raising cx_other_exception | other exception");
		buildSrc("             \"! @raising cx_third_exception | third exception");
		buildSrc("             \"! @raising cx_any_exception | any exception");
		buildSrc("             chained_method");
		buildSrc("               IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                         iv_other_parameter TYPE i");
		buildSrc("               RAISING   cx_any_exception");
		buildSrc("                         cx_other_exception");
		buildSrc("                         cx_third_exception.");

		buildExp("    \"! any method");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter | any parameter");
		buildExp("    \"! @parameter iv_other_parameter | other parameter");
		buildExp("    \"! @raising cx_any_exception | any exception");
		buildExp("    \"! @raising cx_other_exception | other exception");
		buildExp("    \"! @raising cx_third_exception | third exception");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("                iv_other_parameter TYPE i");
		buildExp("      RAISING   cx_any_exception");
		buildExp("                cx_other_exception");
		buildExp("                cx_third_exception.");
		buildExp("");
		buildExp("    METHODS: \"! chained method");
		buildExp("             \"!");
		buildExp("             \"! @parameter iv_any_parameter | any parameter");
		buildExp("             \"! @parameter iv_other_parameter | other parameter");
		buildExp("             \"! @raising cx_any_exception | any exception");
		buildExp("             \"! @raising cx_other_exception | other exception");
		buildExp("             \"! @raising cx_third_exception | third exception");
		buildExp("             chained_method");
		buildExp("               IMPORTING iv_any_parameter   TYPE i");
		buildExp("                         iv_other_parameter TYPE i");
		buildExp("               RAISING   cx_any_exception");
		buildExp("                         cx_other_exception");
		buildExp("                         cx_third_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDoNotUpdateOrder() {
		rule.configUpdateOrder.setValue(false);

		buildSrc("    \"! any method");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_other_parameter | other parameter");
		buildSrc("    \"! @parameter iv_any_parameter | any parameter");
		buildSrc("    \"! @raising cx_any_exception | any exception");
		buildSrc("    \"! @raising cx_third_exception | third exception");
		buildSrc("    \"! @raising cx_other_exception | other exception");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                iv_other_parameter TYPE i");
		buildSrc("      RAISING   cx_any_exception");
		buildSrc("                cx_other_exception");
		buildSrc("                cx_third_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_other_parameter | other parameter");
		buildSrc("      \"! @parameter iv_any_parameter | any parameter");
		buildSrc("      \"! @raising cx_other_exception | other exception");
		buildSrc("      \"! @raising cx_third_exception | third exception");
		buildSrc("      \"! @raising cx_any_exception | any exception");
		buildSrc("      chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("                  iv_other_parameter TYPE i");
		buildSrc("        RAISING   cx_any_exception");
		buildSrc("                  cx_other_exception");
		buildSrc("                  cx_third_exception.");

		copyExpFromSrc();
		
		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testMultiLineParametersAndExceptions() {
		// ensure that multi-line parameters and exceptions are correctly processed, esp. regarding deletion
		// of empty descriptions
		
		buildSrc("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter");
		buildSrc("    \"!            |");
		buildSrc("    \"!            <p class=\"shorttext synchronized\">any parameter</p>");
		buildSrc("    \"! @parameter iv_obsolete_parameter |");
		buildSrc("    \"!            <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildSrc("    \"! @parameter iv_also_obsolete |");
		buildSrc("    \"! @raising cx_any_exception");
		buildSrc("    \"!           | <p class=\"shorttext synchronized\">any exception</p>");
		buildSrc("    \"! @raising cx_obsolete_exception");
		buildSrc("    \"!          | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildSrc("    \"! @raising cx_also_obsolete");
		buildSrc("    \"!          |");
		buildSrc("    \"!          <p class=\"shorttext synchronized\"></p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING iv_any_parameter   TYPE i");
		buildSrc("      RAISING   cx_any_exception.");
		buildSrc("");
		buildSrc("    METHODS:");
		buildSrc("      \"! other chained method");
		buildSrc("      \"!");
		buildSrc("      \"! @parameter iv_any_parameter");
		buildSrc("      \"!            |");
		buildSrc("      \"!            any parameter");
		buildSrc("      \"! @parameter iv_obsolete_parameter |");
		buildSrc("      \"!            obsolete parameter");
		buildSrc("      \"! @parameter iv_also_obsolete |");
		buildSrc("      \"! @raising cx_any_exception");
		buildSrc("      \"!          | any exception");
		buildSrc("      \"! @raising cx_obsolete_exception");
		buildSrc("      \"!          | obsolete exception");
		buildSrc("      \"! @raising cx_also_obsolete");
		buildSrc("      \"!          |");
		buildSrc("      other_chained_method");
		buildSrc("        IMPORTING iv_any_parameter   TYPE i");
		buildSrc("        RAISING   cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter");
		buildExp("    \"!            |");
		buildExp("    \"!            <p class=\"shorttext synchronized\">any parameter</p>");
		buildExp("    \"! @raising cx_any_exception");
		buildExp("    \"!           | <p class=\"shorttext synchronized\">any exception</p>");
		buildExp("    \"! @parameter iv_obsolete_parameter |");
		buildExp("    \"!            <p class=\"shorttext synchronized\">obsolete parameter</p>");
		buildExp("    \"! @raising cx_obsolete_exception");
		buildExp("    \"!          | <p class=\"shorttext synchronized\">obsolete exception</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING iv_any_parameter   TYPE i");
		buildExp("      RAISING   cx_any_exception.");
		buildExp("");
		buildExp("    METHODS:");
		buildExp("      \"! other chained method");
		buildExp("      \"!");
		buildExp("      \"! @parameter iv_any_parameter");
		buildExp("      \"!            |");
		buildExp("      \"!            any parameter");
		buildExp("      \"! @raising cx_any_exception");
		buildExp("      \"!          | any exception");
		buildExp("      \"! @parameter iv_obsolete_parameter |");
		buildExp("      \"!            obsolete parameter");
		buildExp("      \"! @raising cx_obsolete_exception");
		buildExp("      \"!          | obsolete exception");
		buildExp("      other_chained_method");
		buildExp("        IMPORTING iv_any_parameter   TYPE i");
		buildExp("        RAISING   cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}


	@Test
	void testTestMethodExceptionsNotAdded() {
		// ensure that no ABAP Doc is added for exceptions of test methods (because it is unlikely that 
		// documentation should be added for those); parameters can anyway never occur with FOR TESTING methods

		buildSrc("  PRIVATE SECTION.");
		buildSrc("    \"! any method");
		buildSrc("    METHODS any_method FOR TESTING RAISING cx_any_exception\");");
		buildSrc("                                           cx_other_exception.\");");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testTestMethodExceptionDeletedButNotAdded() {
		// ensure that in FOR TESTING methods, documentation for obsolete exceptions is removed, 
		// even if no documentation is added for a missing exception (see previous test)
		
		buildSrc("  PRIVATE SECTION.");
		buildSrc("    \"! any method");
		buildSrc("    \"!");
		buildSrc("    \"! @raising cx_any_exception   | any exception");
		buildSrc("    \"! @raising cx_third_exception |");
		buildSrc("    METHODS any_method FOR TESTING RAISING cx_any_exception\");");
		buildSrc("                                           cx_other_exception.\");");

		buildExp("  PRIVATE SECTION.");
		buildExp("    \"! any method");
		buildExp("    \"!");
		buildExp("    \"! @raising cx_any_exception   | any exception");
		buildExp("    METHODS any_method FOR TESTING RAISING cx_any_exception\");");
		buildExp("                                           cx_other_exception.\");");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
	
	@Test 
	void testIsConfigValueEnabled() {
		assertTrue(rule.isConfigValueEnabled(rule.configAddExceptions));
		
		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS);
		assertTrue(rule.isConfigValueEnabled(rule.configAddAlwaysWarning));

		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS);
		assertTrue(rule.isConfigValueEnabled(rule.configAddAlwaysWarning));

		rule.configAddParameters.setEnumValue(AddAbapDocType.ALWAYS);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.NEVER);
		assertTrue(rule.isConfigValueEnabled(rule.configAddAlwaysWarning));

		rule.configAddParameters.setEnumValue(AddAbapDocType.ONLY_FOR_NON_SYNCHRONIZED);
		rule.configAddExceptions.setEnumValue(AddAbapDocType.ALWAYS_AS_NON_SYNCHRONIZED);
		assertFalse(rule.isConfigValueEnabled(rule.configAddAlwaysWarning));
	}
}