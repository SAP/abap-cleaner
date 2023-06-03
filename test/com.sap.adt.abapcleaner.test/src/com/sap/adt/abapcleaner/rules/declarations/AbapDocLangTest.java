package com.sap.adt.abapcleaner.rules.declarations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

class AbapDocLangTest extends RuleTestBase {
	private AbapDocLangRule rule;
	
	AbapDocLangTest() {
		super(RuleID.ABAP_DOC_LANG);
		rule = (AbapDocLangRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configRemoveNonEnglishLang.setValue(false);
	}

	@Test
	void testEnglishLangAttributeRemoved() {
		// expect lang="en" (including with other casing like EN, En, eN) to be removed 
		
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"EN\">any method documentation</em>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any_parameter   | <p class=\"shorttext synchronized\" lang=\"en\">any parameter documentation</p>");
		buildSrc("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"En\">other parameter documentation</p>");
		buildSrc("    \"! @raising   cx_any_exception   | <p class=\"shorttext synchronized\" lang=\"eN\">exception documentation</p>");
		buildSrc("    METHODS any_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_any_parameter   TYPE i");
		buildSrc("        !iv_other_parameter TYPE i");
		buildSrc("      RAISING");
		buildSrc("        cx_any_exception.");

		buildExp("    \"! <p class=\"shorttext synchronized\">any method documentation</em>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any_parameter   | <p class=\"shorttext synchronized\">any parameter documentation</p>");
		buildExp("    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\">other parameter documentation</p>");
		buildExp("    \"! @raising   cx_any_exception   | <p class=\"shorttext synchronized\">exception documentation</p>");
		buildExp("    METHODS any_method");
		buildExp("      IMPORTING");
		buildExp("        !iv_any_parameter   TYPE i");
		buildExp("        !iv_other_parameter TYPE i");
		buildExp("      RAISING");
		buildExp("        cx_any_exception.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNonEnglishLangAttributeKept() {
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"DE\">Dokumentation der anderen Methode</em>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_first_parameter  | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 1. Parameters</p>");
		buildSrc("    \"! @parameter iv_second_parameter | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 2. Parameters</p>");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_first_parameter  TYPE c");
		buildSrc("        !iv_second_parameter TYPE c.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNonEnglishLangAttributeRemoved() {
		rule.configRemoveNonEnglishLang.setValue(true);

		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"DE\">Dokumentation der anderen Methode</em>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_first_parameter  | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 1. Parameters</p>");
		buildSrc("    \"! @parameter iv_second_parameter | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 2. Parameters</p>");
		buildSrc("    METHODS other_method");
		buildSrc("      IMPORTING");
		buildSrc("        !iv_first_parameter  TYPE c");
		buildSrc("        !iv_second_parameter TYPE c.");

		buildExp("    \"! <p class=\"shorttext synchronized\">Dokumentation der anderen Methode</em>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_first_parameter  | <p class=\"shorttext synchronized\">Dokumentation des 1. Parameters</p>");
		buildExp("    \"! @parameter iv_second_parameter | <p class=\"shorttext synchronized\">Dokumentation des 2. Parameters</p>");
		buildExp("    METHODS other_method");
		buildExp("      IMPORTING");
		buildExp("        !iv_first_parameter  TYPE c");
		buildExp("        !iv_second_parameter TYPE c.");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testNonSynchronizedAttributeKept() {
		// expect the lang attribute of the NON-synchronized shorttext to remain unchanged, 
		// because only synchronized shorttext must use the original language determined in TADIR
		
		buildSrc("    \"! <p class=\"shorttext\" lang=\"en\">For non-synchronized shorttexts, lang=\"...\" will NOT be removed,</p>");
		buildSrc("    \"! <p class=\"shorttext\" lang=\"en\">because texts in non-original languages are allowed here.</p>");
		buildSrc("    \"! <p class=\"shorttext\" lang=\"DE\">F�r nicht-synchronisierte Kurztexte wird lang=\"...\" NICHT entfernt,</p>");
		buildSrc("    \"! <p class=\"shorttext\" lang=\"DE\">weil hier Texte in Nicht-Originalsprachen erlaubt sind.</p>");
		buildSrc("    CLASS-DATA mv_any_variable TYPE string.");

		copyExpFromSrc();

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}
}
