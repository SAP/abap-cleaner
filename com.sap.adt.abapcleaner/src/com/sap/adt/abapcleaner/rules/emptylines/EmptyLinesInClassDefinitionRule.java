package com.sap.adt.abapcleaner.rules.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.*;


public class EmptyLinesInClassDefinitionRule extends RuleForCommands {
	// possible enhancement: Remove empty PUBLIC / PROTECTED / PRIVATE SECTION
	
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Add a single blank line to separate things, but not more", "#add-a-single-blank-line-to-separate-things-but-not-more"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't obsess with separating blank lines", "#dont-obsess-with-separating-blank-lines") };

	@Override
	public RuleID getID() { return RuleID.EMPTY_LINES_IN_CLASS_DEFINITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Standardize empty lines in class definitions"; }

	@Override
	public String getDescription() { return "Restricts the number of consecutive empty lines in class definition sections, adds an empty line above each SECTION and between different types of definitions."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 9, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
   			+ LINE_SEP + "CLASS cl_any_class DEFINITION"
   			+ LINE_SEP + "  PUBLIC"
   			+ LINE_SEP + "  CREATE PROTECTED."
   			+ LINE_SEP 
   			+ LINE_SEP 
   			+ LINE_SEP + "  PUBLIC SECTION."
   			+ LINE_SEP 
   			+ LINE_SEP + "    INTERFACES if_any_class."
   			+ LINE_SEP + "    INTERFACES if_other."
   			+ LINE_SEP + "*\"* public components of class CL_ANY_CLASS"
   			+ LINE_SEP + "*\"* do not include other source files here!!!"
   			+ LINE_SEP 
   			+ LINE_SEP + "    ALIASES any_methody"
   			+ LINE_SEP + "      FOR if_any_class~any_method ."
   			+ LINE_SEP + "    ALIASES other_method"
   			+ LINE_SEP + "      FOR if_any_class~other_method ."
   			+ LINE_SEP + "  PROTECTED SECTION."
   			+ LINE_SEP 
   			+ LINE_SEP + "*\"* protected components of class CL_ANY_CLASS"
   			+ LINE_SEP + "*\"* do not include other source files here!!!"
   			+ LINE_SEP + "    CLASS-DATA mv_any_static_text TYPE string."
   			+ LINE_SEP + "    DATA mo_any   TYPE REF TO if_any_class."
   			+ LINE_SEP + "    DATA ms_other TYPE ty_s_other."
   			+ LINE_SEP + "    DATA mv_third TYPE i."
   			+ LINE_SEP 
   			+ LINE_SEP 
   			+ LINE_SEP 
   			+ LINE_SEP + "    METHODS any_protected_method."
   			+ LINE_SEP + "    METHODS other_protected_method."
   			+ LINE_SEP + "  PRIVATE SECTION."
   			+ LINE_SEP 
   			+ LINE_SEP 
   			+ LINE_SEP + "    TYPES:"
   			+ LINE_SEP + "      ty_any   TYPE char10,"
   			+ LINE_SEP + "      ty_other TYPE char20."
   			+ LINE_SEP + "    CONSTANTS gc_any_private_const   TYPE i VALUE 1."
   			+ LINE_SEP + "    CONSTANTS gc_other_private_const TYPE i VALUE 2."
   			+ LINE_SEP + "    \" comment on static methods"
   			+ LINE_SEP + "    CLASS-METHODS: any_static_private_method,"
   			+ LINE_SEP + "      other_static_private_method."
   			+ LINE_SEP + "    METHODS any_instance_private_method."
   			+ LINE_SEP 
   			+ LINE_SEP + "ENDCLASS.";
   }

	private static final String[] addLineBetweenDefTypesTexts = new String[] { "never", "add line, except between CLASS-DATA and DATA etc.", "add line, even between CLASS-DATA and DATA etc." };
	
	final ConfigBoolValue configAddEmptyLineAboveSections = new ConfigBoolValue(this, "AddEmptyLineAboveSections", "Add empty line above PUBLIC / PROTECTED / PRIVATE SECTION", true);
	final ConfigBoolValue configRemoveEmptyLineBelowSections = new ConfigBoolValue(this, "RemoveEmptyLineBelowSections", "Remove empty line below PUBLIC / PROTECTED / PRIVATE SECTION", true);
	final ConfigBoolValue configRemoveEmptyLineAboveEndClass = new ConfigBoolValue(this, "RemoveEmptyLineAboveEndClass", "Remove empty line above ENDCLASS", false);
	final ConfigBoolValue configRemoveIncludeWarnings = new ConfigBoolValue(this, "RemoveIncludeWarnings", "Remove notorious 'do not include other source files here!!!' comments", true);
	final ConfigIntValue configMaxEmptyLines = new ConfigIntValue(this, "MaxEmptyLines", "Max. empty lines within definition sections:", "", 1, 1, 20);
	final ConfigEnumValue<AddLineBetweenDefTypesStyle> configAddLineBetweenDefTypes = new ConfigEnumValue<AddLineBetweenDefTypesStyle>(this, "AddLineBetweenDefTypes", "Add empty line between CONSTANTS, DATA, METHODS etc.:", addLineBetweenDefTypesTexts, AddLineBetweenDefTypesStyle.values(), AddLineBetweenDefTypesStyle.ADD_CONSIDER_STATIC); 

	private final ConfigValue[] configValues = new ConfigValue[] { configAddEmptyLineAboveSections, configRemoveEmptyLineBelowSections, configRemoveEmptyLineAboveEndClass, configRemoveIncludeWarnings, configMaxEmptyLines, configAddLineBetweenDefTypes };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private AddLineBetweenDefTypesStyle getAddLineBetweenDefTypes() {
		return AddLineBetweenDefTypesStyle.forValue(configAddLineBetweenDefTypes.getValue());
	}

	public EmptyLinesInClassDefinitionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}


	@Override
	protected boolean skipDeclarationsInsideBeginOf() { return true; }

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.isInClassDefinition())
			return false;
		
		Token firstToken = command.getFirstToken();

		// remove notorious 'do not include other source files here!!!' comments 
		boolean removeCommand = false;
		if (configRemoveIncludeWarnings.getValue() && command.isAsteriskCommentLine()) {
			removeCommand = isNotoriousIncludeWarning(command);
			if (removeCommand) {
				// transfer line breaks to the next Command
				Command nextCommand = command.getNext();
				if (nextCommand != null) {
					Token nextCommandToken = nextCommand.getFirstToken(); 
					nextCommandToken.lineBreaks = Math.max(nextCommandToken.lineBreaks, firstToken.lineBreaks);
				}
				// remove comment
				try {
					command.removeFromCode();
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				}
				return true;
				
			}
		}

		// ensure the maximum number of line breaks inside the Command; note that lineBreaks = empty lines + 1
		boolean changed = false;
		int maxLineBreakWithin = configMaxEmptyLines.getValue() + 1;
		Token token = firstToken;
		if (command.isClassDefinitionStart())
			token = token.getNext();
		while (token != null) {
			if (token.lineBreaks > maxLineBreakWithin) {
				token.lineBreaks = maxLineBreakWithin;
				changed = true;
			}
			token = token.getNext();
		}
		
		// add empty line above PUBLIC / PROTECTED / PRIVATE SECTION
		// note that these empty lines are saved as part of the previous(!) section (e.g. empty lines about 'PROTECTED SECTION' 
		// are stored on the 'PUBLIC SECTION' object), therefore, since command is in the cleanup range, we change nextCommand, 
		// even if it is NOT in the cleanup range
		Command nextCommand = command.getNext();
		if (configAddEmptyLineAboveSections.getValue() && nextCommand != null && nextCommand.isDeclarationSectionStart()) {
			if (command != null && command.isClassDefinitionStart() && !command.containsInnerLineBreaks(false)) {
				// do NOT add an empty line above PUBLIC / PROTECTED / PRIVATE SECTION if it is preceded by a one-liner 'CLASS ... DEFINITION' 
			} else if (nextCommand.getFirstTokenLineBreaks() <= 1) {
				nextCommand.getFirstToken().lineBreaks = 2;
				changed = true;
			}
		}
		// remove empty line below PUBLIC / PROTECTED / PRIVATE SECTION
		Command prevCommand = command.getPrev();
		if (configRemoveEmptyLineBelowSections.getValue() && !command.isDeclarationSectionStart() && !command.isClassEnd()
				&& prevCommand != null && prevCommand.isDeclarationSectionStart()) {
			if (firstToken.lineBreaks > 1) {
				firstToken.lineBreaks = 1;
				changed = true;
			}
		}
		
		// remove empty line above ENDCLASS (except when it is preceded by an empty SECTION start)
		if (configRemoveEmptyLineAboveEndClass.getValue() && command.isClassEnd()
				&& prevCommand != null && !prevCommand.isDeclarationSectionStart()) {
			if (firstToken.lineBreaks > 1) {
				firstToken.lineBreaks = 1;
				changed = true;
			}
		}

		// add empty line between CONSTANTS, DATA, METHODS etc.
		AddLineBetweenDefTypesStyle addLineBetweenDefTypesStyle = getAddLineBetweenDefTypes();
		if (addLineBetweenDefTypesStyle != AddLineBetweenDefTypesStyle.NEVER && prevCommand != null 
				&& (prevCommand.isDeclaration() || prevCommand.isDeclarationInClassDef()) && firstToken.lineBreaks <= 1) {
			// while command is a comment line, move down, as long as there is no empty line between them and the next declaration 
			Command nextDeclaration = command;
			boolean emptyLineFound = false;
			while (nextDeclaration.isCommentLine()) {
				nextDeclaration = nextDeclaration.getNext();
				if (nextDeclaration == null || nextDeclaration.getFirstTokenLineBreaks() > 1) {
					emptyLineFound = true;
					break;
				}
			}
			if (!emptyLineFound && (nextDeclaration.isDeclaration() || nextDeclaration.isDeclarationInClassDef())) {
				// check whether the previous and the next declaration keyword are different
				String prevKeyword = prevCommand.getFirstToken().getText();
				String nextKeyword = nextDeclaration.getFirstToken().getText();
				if (addLineBetweenDefTypesStyle == AddLineBetweenDefTypesStyle.ADD_IGNORE_STATIC) {
					// remove the 'CLASS-' prefix to consider 'CLASS-DATA' and 'DATA' as equal (same for '[CLASS-]METHODS' and '[CLASS-]EVENTS') 
					prevKeyword = StringUtil.removePrefix(prevKeyword, "CLASS-", true);
					nextKeyword = StringUtil.removePrefix(nextKeyword, "CLASS-", true);
				}
				if (!AbapCult.stringEquals(prevKeyword, nextKeyword, true)) {
					// insert an empty line above the current command (which is either the 'next declaration' or a comment attached to it)
					firstToken.lineBreaks = 2;
					changed = true;
				}
			}
		}

		return changed;
	}

	/**
	 * returns true if the supplied Command is an auto-generated comment which either reads 
	 * '*"* public/protected/private components of class ...' or '*"* do not include other source files here!!!'
	 */
	public static boolean isNotoriousIncludeWarning(Command command) {
		if (command == null || !command.isCommentLine())
			return false;
		
		Token firstToken = command.getFirstToken();
		if (firstToken.textStartsWith("*\"* public components of class ")
		|| firstToken.textStartsWith("*\"* protected components of class ")
		|| firstToken.textStartsWith("*\"* private components of class ")) {
			// check whether '*"* private components of class ' is followed by only one more word (the class name) 
			int textPos = firstToken.getText().indexOf(" components of class ");
			if (textPos >= 0 && firstToken.getText().indexOf(' ', textPos + " components of class ".length()) < 0) { 
				return true;
			}
		} else if (firstToken.textEquals("*\"* do not include other source files here!!!")) {
			return true;
		}
		return false;
	}
}