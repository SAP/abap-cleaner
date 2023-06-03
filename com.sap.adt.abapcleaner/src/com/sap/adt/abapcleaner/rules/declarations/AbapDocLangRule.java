package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.*;

public class AbapDocLangRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ABAP_DOC_LANG; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Remove lang=\"en\" from ABAP Doc"; }

	@Override
	public String getDescription() { return "Removes lang=\"en\" from the HMTL tag <p class=\"shorttext synchronized\" lang=\"en\"> in ABAP Doc, since anyway, only the original language of the ABAP object is allowed, as defined in TADIR."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 8, 26); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_ABAP_DOC } ; }

	@Override
   public String getExample() {
      return "" 
         + LINE_SEP + "CLASS cl_any_class DEFINITION PUBLIC FINAL CREATE PUBLIC."
         + LINE_SEP + "  PUBLIC SECTION."
			+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"EN\">any method documentation</p>" 
			+ LINE_SEP + "    \"!" 
			+ LINE_SEP + "    \"! @parameter iv_any_parameter   | <p class=\"shorttext synchronized\" lang=\"en\">any parameter documentation</p>" 
			+ LINE_SEP + "    \"! @parameter iv_other_parameter | <p class=\"shorttext synchronized\" lang=\"en\">other parameter documentation</p>" 
			+ LINE_SEP + "    \"! @raising   cx_any_exception   | <p class=\"shorttext synchronized\" lang=\"en\">exception documentation</p>" 
			+ LINE_SEP + "    METHODS any_method" 
			+ LINE_SEP + "      IMPORTING" 
			+ LINE_SEP + "        !iv_any_parameter   TYPE i" 
			+ LINE_SEP + "        !iv_other_parameter TYPE i" 
			+ LINE_SEP + "      RAISING" 
			+ LINE_SEP + "        cx_any_exception." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \" if the original language of this class is English, lang=\"de\" in the following lines triggers the warning" 
			+ LINE_SEP + "    \" 'Language DE is not allowed. You have to use the original language EN'." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"DE\">Dokumentation der anderen Methode</p>" 
			+ LINE_SEP + "    \"!" 
			+ LINE_SEP + "    \"! @parameter iv_first_parameter  | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 1. Parameters</p>" 
			+ LINE_SEP + "    \"! @parameter iv_second_parameter | <p class=\"shorttext synchronized\" lang=\"de\">Dokumentation des 2. Parameters</p>" 
			+ LINE_SEP + "    METHODS other_method" 
			+ LINE_SEP + "      IMPORTING" 
			+ LINE_SEP + "        !iv_first_parameter  TYPE c" 
			+ LINE_SEP + "        !iv_second_parameter TYPE c." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \"! <p class=\"shorttext\" lang=\"en\">For non-synchronized shorttexts, lang=\"...\" will NOT be removed,</p>" 
			+ LINE_SEP + "    \"! <p class=\"shorttext\" lang=\"en\">because texts in non-original languages are allowed here.</p>" 
			+ LINE_SEP + "    \"! <p class=\"shorttext\" lang=\"DE\">Fuer nicht-synchronisierte Kurztexte wird lang=\"...\" NICHT entfernt,</p>" 
			+ LINE_SEP + "    \"! <p class=\"shorttext\" lang=\"DE\">weil hier Texte in Nicht-Originalsprachen erlaubt sind.</p>" 
			+ LINE_SEP + "    CLASS-DATA mv_any_variable TYPE string." 
			+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigBoolValue configRemoveNonEnglishLang = new ConfigBoolValue(this, "RemoveNonEnglishLang", "Also remove 'lang' attribute for other languages like lang=\"de\"", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configRemoveNonEnglishLang };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AbapDocLangRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.isAbapDoc())
			return false;
		
		// "The attribute lang uses the HTML standard. It must be used to specify the original language of the repository object as a two-character ISO code"
		// (https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=ABENDOCCOMMENT.htm)
		String regex = configRemoveNonEnglishLang.getValue() ? "<p class=\"shorttext synchronized\" lang=\"[a-zA-Z][a-zA-Z]\">" :
													  							 "<p class=\"shorttext synchronized\" lang=\"[eE][nN]\">"; 

		Token firstToken = command.getFirstToken();
		String abapDoc = firstToken.getText();
		String changedAbapDoc = abapDoc.replaceAll(regex, "<p class=\"shorttext synchronized\">");

		if (abapDoc.equals(changedAbapDoc)) {
			return false;
		} else {
			firstToken.setText(changedAbapDoc, false);
			return true;
		}
	}
}