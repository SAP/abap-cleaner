package com.sap.adt.abapcleaner.rules.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class EmptyLinesWithinMethodsRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Add a single blank line to separate things, but not more", "#add-a-single-blank-line-to-separate-things-but-not-more"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't obsess with separating blank lines", "#dont-obsess-with-separating-blank-lines") };

	@Override
	public RuleID getID() { return RuleID.EMPTY_LINES_WITHIN_METHODS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Standardize empty lines within methods"; }

	@Override
	public String getDescription() { return "Restricts the number of consecutive empty lines within methods, and adds an empty line between declarations and the first executable statement (or the comments preceding it)."; }

	@Override
	public String getHintsAndRestrictions() { return "For function modules, additional empty lines are kept at the beginning to align with ADT and SE37 behavior."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD empty_lines_within_methods." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_any_integer TYPE i." 
			+ LINE_SEP + "    DATA lv_any_string  TYPE string." 
			+ LINE_SEP + "    do_something( )." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "    do_something_else( )." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "    finish_doing_something( )." 
			+ LINE_SEP 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD empty_lines." 
			+ LINE_SEP + "    FIELD-SYMBOLS <ls_data> TYPE any_type." 
			+ LINE_SEP + "    \" comment above executable statement" 
			+ LINE_SEP + "    DATA(lo_utility) = cl_factory=>get( )->get_utility( )." 
			+ LINE_SEP 
			+ LINE_SEP + "  ENDMETHOD."; 
   }

	final ConfigIntValue configMaxEmptyLinesAtMethodStart = new ConfigIntValue(this, "MaxEmptyLinesAtMethodStart", "Max. empty lines at method start:", "", 0, 0, 20);
	final ConfigIntValue configMaxEmptyLinesWithinMethods = new ConfigIntValue(this, "MaxEmptyLinesWithinMethods", "Max. empty lines within methods:", "", 0, 1, 20);
	final ConfigIntValue configMaxEmptyLinesAtMethodEnd = new ConfigIntValue(this, "MaxEmptyLinesAtMethodEnd", "Max. empty lines at method end:", "", 0, 0, 20);
	final ConfigBoolValue configEmptyLineAboveFirstExecutable = new ConfigBoolValue(this, "EmptyLineAboveFirstExecutable", "Insert empty line between declarations and first executable statement", true, false, LocalDate.of(2022, 5, 21));

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxEmptyLinesAtMethodStart, configMaxEmptyLinesWithinMethods, configMaxEmptyLinesAtMethodEnd, configEmptyLineAboveFirstExecutable };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EmptyLinesWithinMethodsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		boolean emptyLineAboveFirstExecutable = configEmptyLineAboveFirstExecutable.getValue() && (configMaxEmptyLinesWithinMethods.getValue() > 0);

		// a 'method' in the sense of this Rule includes event blocks in reports (e.g. AT SELECTION-SCREEN events)
		boolean isInMethod = !code.hasMethodFunctionFormOrEventBlockStart();
		boolean declarationFound = false;
		boolean executableFound = false;
		
		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;
			
			// since event blocks do not have dedicated closers (such as "ENDMETHOD."), the following has to processed at the beginning of the loop:
			if (command.endsEventBlock())
				isInMethod = false;

			if (isInMethod && !declarationFound && (command.isDeclaration() || command.isDeclarationInclude())) {
				declarationFound = true;
			}
			
			if (isInMethod) {
				// determine whether an empty line shall be inserted above the command (or the comments preceding it): 
				boolean insertLineBreakAbove;
				if (!emptyLineAboveFirstExecutable) {
					// the option is deactivated
					insertLineBreakAbove = false;
				} else if (executableFound || command.isDeclaration() || command.isDeclarationInclude() || command.isCommentLine() || command.isMethodFunctionOrFormEnd()) {
					// this is not an executable Command - or not the first one in the method
					insertLineBreakAbove = false;
				} else {
					// insert an empty line if(!) declarations were found above this executable Command
					insertLineBreakAbove = declarationFound;
					executableFound = true;
				}

				if (!isCommandBlocked(command)) {
					executeOn(code, command, insertLineBreakAbove);
				}
			}

			// only now (because lineBreaks always deals with the line breaks *above* a statement):
			if (command.isMethodFunctionOrFormEnd())
				isInMethod = false;
			if (command.isMethodFunctionFormOrEventBlockStart()) {
				isInMethod = true;
				declarationFound = false;
				executableFound = false;
			}

			command = command.getNext();
		}
	}

	private void executeOn(Code code, Command command, boolean insertLineBreakAbove) {
		// ensure the maximum number of line breaks at method start and end;
		// in the following section, we deliberately do NOT use command.isMethodFunctionFormOrEventBlockEnd(), 
		// because event blocks do not have proper closing commands (such as "ENDMETHOD.")
		Token token = command.getFirstToken();
		if (command.getPrev() != null && (command.getPrev().isMethodFunctionFormOrEventBlockStart() || command.isMethodFunctionOrFormEnd())) {
			int maxLineBreak = ((command.isMethodFunctionOrFormEnd()) ? configMaxEmptyLinesAtMethodEnd.getValue() : configMaxEmptyLinesAtMethodStart.getValue()) + 1;
			
			// for function modules with parameters, allow 3 extra lines that are needed for additional auto-generated comment lines in SE37, 
			// and therefore ensured when saving a function module from ADT; for function modules without parameters, 1 extra line is needed
			if (command.getPrev().isFunctionStart()) {
				Token functionName = command.getPrev().getFirstCodeToken().getNextCodeSibling();
				boolean hasParameters = (functionName != null && !functionName.getNextCodeSibling().isPeriod());
				maxLineBreak += hasParameters ? 3 : 1;
			}

			if (token != null && token.lineBreaks > maxLineBreak) {
				token.lineBreaks = maxLineBreak;
				code.addRuleUse(this, command, token);
			}
			token = token.getNext(); // do not process the first Token with the maxEmptyLinesWithinMethods criterion
		}

		// ensure the maximum number of line breaks inside the Command; note that lineBreaks = empty lines + 1
		int maxLineBreakWithin = configMaxEmptyLinesWithinMethods.getValue() + 1;
		while (token != null) {
			if (token.lineBreaks > maxLineBreakWithin) {
				token.lineBreaks = maxLineBreakWithin;
				code.addRuleUse(this, command, token);
			}
			token = token.getNext();
		}
		
		// insert an empty line between declarations and the first executable statement (or the comments preceding it)  
		if (insertLineBreakAbove) {
			Command changeCommand = command;
			while (changeCommand.getFirstTokenLineBreaks() == 1 && changeCommand.getPrev() != null && changeCommand.getPrev().isCommentLine()) {
				changeCommand = changeCommand.getPrev();
			}
			if (changeCommand.getFirstTokenLineBreaks() == 1) {
				changeCommand.getFirstToken().lineBreaks = 2;
				code.addRuleUse(this, command);
			}
		}
	}
}