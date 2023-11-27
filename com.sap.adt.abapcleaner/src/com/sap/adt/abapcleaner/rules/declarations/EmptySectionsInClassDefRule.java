package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rules.emptylines.EmptyLinesInClassDefinitionRule;

public class EmptySectionsInClassDefRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.EMPTY_SECTIONS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Remove empty class definition SECTIONs"; }

	@Override
	public String getDescription() { return "Removes empty PUBLIC / PROTECTED / PRIVATE SECTIONs from class definitions (except from public classes that are non-final or inheriting)."; }

	@Override
	public String getHintsAndRestrictions() { return "SECTIONs that contain comments will be kept, except for the auto-generated 'do not include other source files here!!!' comments."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 9, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
  			+ LINE_SEP + "\" it makes no sense for a FINAL class to have a PROTECTED SECTION" 
			+ LINE_SEP + "CLASS cl_any_final_class DEFINITION FINAL." 
			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP + "*\"* public components of class CL_ANY_FINAL_CLASS"
			+ LINE_SEP + "*\"* do not include other source files here!!!"
			+ LINE_SEP + "    INTERFACES if_any_interface." 
			+ LINE_SEP 
			+ LINE_SEP + "  PROTECTED SECTION." 
			+ LINE_SEP + "*\"* protected components of class CL_ANY_FINAL_CLASS"
			+ LINE_SEP + "*\"* do not include other source files here!!!"
			+ LINE_SEP 
			+ LINE_SEP + "  PRIVATE SECTION." 
			+ LINE_SEP + "*\"* private components of class CL_ANY_FINAL_CLASS"
			+ LINE_SEP + "*\"* do not include other source files here!!!"
			+ LINE_SEP 
			+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "\" the following class only has definitions in the PRIVATE SECTION," 
			+ LINE_SEP + "\" so the other two sections may not be needed" 
			+ LINE_SEP + "CLASS cl_any_non_final_class DEFINITION FOR TESTING." 
			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP 
			+ LINE_SEP + "  PROTECTED SECTION." 
			+ LINE_SEP 
			+ LINE_SEP + "  PRIVATE SECTION." 
			+ LINE_SEP + "    DATA mv_any_data TYPE i." 
			+ LINE_SEP 
			+ LINE_SEP + "    METHODS setup." 
			+ LINE_SEP + "    METHODS any_test_method FOR TESTING." 
			+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "\" the following class was probably freshly created with a template," 
			+ LINE_SEP + "\" so it can be assumed that its sections should be kept for now" 
			+ LINE_SEP + "CLASS cl_any_empty_class DEFINITION." 
			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP 
			+ LINE_SEP + "  PROTECTED SECTION." 
			+ LINE_SEP 
			+ LINE_SEP + "  PRIVATE SECTION." 
			+ LINE_SEP 
			+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigEnumValue<EmptySectionsAction> configEmptySectionsAction = new ConfigEnumValue<EmptySectionsAction>(this, "EmptySectionsMeasure", "Scope:",
			new String[] { "Remove empty PROTECTED SECTIONs from FINAL classes", "Remove any empty SECTION from non-empty class definitions", "Remove any empty SECTION" }, EmptySectionsAction.values(), EmptySectionsAction.REMOVE_ANY_FROM_NON_EMPTY_CLASS);

	private final ConfigValue[] configValues = new ConfigValue[] { configEmptySectionsAction };

	private EmptySectionsAction getConfigEmptySectionsAction() {
		return EmptySectionsAction.forValue(configEmptySectionsAction.getValue());
	}

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EmptySectionsInClassDefRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// CLASS ... IMPLEMENTATION can be skipped completely  
			if (command.isClassImplementationStart()) {
				command = command.getNextSibling();
				continue;
			}

			// remember the previous(!) Command, in case the current Command (and potentially, the following Commands with  
			// comments) are removed from the code; for this reason, this class cannot use RuleForCommands as a base class
			Command prevCommand = command.getPrev();
			if (!isCommandBlocked(command)) {
				if (executeOn(code, command)) {
					code.addRuleUse(this, command);
					command = (prevCommand == null) ? code.firstCommand : prevCommand;
				}
			}

			command = command.getNext();
		}
	}

	private boolean executeOn(Code code, Command command) throws UnexpectedSyntaxAfterChanges {
		if (!command.isDeclarationSectionStart())
			return false;
		
		// check whether the declaration section is empty 
		if (!isSectionEmpty(command))
			return false;

		// note for the following that CLASS, PUBLIC SECTION, PROTECTED SECTION, PRIVATE SECTION, and ENDCLASS are siblings(!), 
		// while all other Commands between CLASS and ENDCLASS are children to CLASS or the respective SECTION  
		Command classStart = command.getPrevSibling();
		while (classStart.isDeclarationSectionStart()) {
			classStart = classStart.getPrevSibling();
		}
		if (!classStart.isClassDefinitionStart()) // unexpected case
			return false;

		// ABAP Syntax Check demands: "For technical reasons, the statement "PROTECTED SECTION" or "PRIVATE SECTION" must exist in non-final global classes."
		// Therefore, we prevent the removal of sections in this case 
		if (classStart.isPublicClassDefinitionStart() && !classStart.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, "FINAL")) {
			return false;
		}

		// a syntax error comes up in ADT when a protected base class constructor is called in the following scenario:
		// - the base class is "PUBLIC" and "CREATE PROTECTED",
		// - the base class constructor has parameters, 
		// - the child class is "PUBLIC", 
		// - the child class is "CREATE PROTECTED" (explicitly or implicitly inherited) or "CREATE PRIVATE", 
		// - the child class misses both the PROTECTED and the PRIVATE section. 
		// therefore, we prevent the removal of sections if "PUBLIC" and "INHERITING" are found 
		// ("CREATE PROTECTED" can NOT be used as criterion, because it may be implicitly inherited from the base class)
		if (classStart.isPublicClassDefinitionStart() && classStart.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, "INHERITING")) {
			return false;
		}
		
		EmptySectionsAction configAction = getConfigEmptySectionsAction();
		if (configAction == EmptySectionsAction.REMOVE_PROTECTED_OF_FINAL_CLASS) {
			// only work on empty PROTECTED SECTION of a FINAL class 
			if (!command.firstCodeTokenIsKeyword("PROTECTED"))
				return false;
			if (!classStart.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, "FINAL"))
				return false;
			// otherwise, remove section below

		} else if (configAction == EmptySectionsAction.REMOVE_ANY_FROM_NON_EMPTY_CLASS) {
			// only remove section if at least one of the SECTIONs is non-empty
			boolean areAllSectionsEmpty = true;
			Command testSection = classStart.getNextSibling();
			while (testSection != null && testSection.isDeclarationSectionStart()) {
				if (!isSectionEmpty(testSection)) {
					areAllSectionsEmpty = false;
					break;
				}
				testSection = testSection.getNextSibling();
			}
			if (areAllSectionsEmpty)
				return false;
			// otherwise, remove section below
		}
		
		// remove section 
		try {
			// if needed, delete comments with notorious include warning
			while (EmptyLinesInClassDefinitionRule.isNotoriousIncludeWarning(command.getNext())) {
				code.addRuleUse(this, command.getNext());
				command.getNext().removeFromCode();
			}
			if (command.getNext().isDeclarationSectionStart()) {
				command.getNext().getFirstToken().lineBreaks = Math.max(command.getFirstTokenLineBreaks(), 1);
			}
			// now delete the SECTION; removeFromCode() will throw an exception if it still has child Commands
			// (but the above checks should exclude such a case) 
			command.removeFromCode();
			// also add a rule use to the CLASS ... DEFINITION Command - otherwise, only deleted Commands will be added!
			code.addRuleUse(this, classStart);
			return true;

		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}

	private boolean isSectionEmpty(Command sectionStart) {
		Command command = sectionStart.getNext();
		while (command != null && EmptyLinesInClassDefinitionRule.isNotoriousIncludeWarning(command)) {
			command = command.getNext();
		}
		return (command != null && (command.isDeclarationSectionStart() || command.isClassEnd()));
	}
}
