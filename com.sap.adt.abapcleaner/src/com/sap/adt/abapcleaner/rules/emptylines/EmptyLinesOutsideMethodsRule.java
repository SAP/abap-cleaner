package com.sap.adt.abapcleaner.rules.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class EmptyLinesOutsideMethodsRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Add a single blank line to separate things, but not more", "#add-a-single-blank-line-to-separate-things-but-not-more"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Don't obsess with separating blank lines", "#dont-obsess-with-separating-blank-lines") };

	@Override
	public RuleID getID() { return RuleID.EMPTY_LINES_OUTSIDE_METHODS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Separate methods and classes with empty lines"; }

	@Override
	public String getDescription() { return "Separates consecutive methods, classes and interfaces with empty lines."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 12); }

	@Override
	public RuleReference[] getReferences() { return references; }
	
	@Override
	public boolean isEssential() { return true; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "INTERFACE lif_empty_lines PUBLIC." 
			+ LINE_SEP + "  \" definition code" 
			+ LINE_SEP + "ENDINTERFACE." 
			+ LINE_SEP + "CLASS lcl_empty_lines DEFINITION." 
			+ LINE_SEP + "  \" definition code" 
			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "CLASS lcl_empty_lines IMPLEMENTATION." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD empty_lines_between_methods_1." 
			+ LINE_SEP + "    \" code" 
			+ LINE_SEP + "  ENDMETHOD." 
			+ LINE_SEP + "  METHOD empty_lines_between_methods_2." 
			+ LINE_SEP + "    \" more code" 
			+ LINE_SEP + "  ENDMETHOD." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD empty_lines_between_methods_3." 
			+ LINE_SEP + "    \" even more code" 
			+ LINE_SEP + "  ENDMETHOD." 
			+ LINE_SEP + "ENDCLASS.";
   }

   final ConfigIntValue configEmptyLinesBetweenClasses = new ConfigIntValue(this, "EmptyLinesBetweenClasses", "Empty lines between classes or interfaces:", "", 1, 2, 5);
   final ConfigIntValue configEmptyLinesBetweenMethods = new ConfigIntValue(this, "EmptyLinesBetweenMethods", "Empty lines between methods:", "", 1, 1, 5);
   final ConfigIntValue configEmptyLinesBetweenClassAndMethod = new ConfigIntValue(this, "EmptyLinesBetweenClassAndMethod", "Empty lines before first / after last method:", "", 0, 0, 5, 5, LocalDate.of(2022, 1, 25));
   
	private final ConfigValue[] configValues = new ConfigValue[] { configEmptyLinesBetweenClasses, configEmptyLinesBetweenMethods, configEmptyLinesBetweenClassAndMethod };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public EmptyLinesOutsideMethodsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		int emptyLinesBetweenMethods = configEmptyLinesBetweenMethods.getValue();
		int emptyLinesBetweenClasses = configEmptyLinesBetweenClasses.getValue();
		int emptyLinesBetweenClassAndMethod = configEmptyLinesBetweenClassAndMethod.getValue();

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			boolean isClassOrInterfaceStart = command.isClassOrInterfaceStart();
			boolean isClassEnd = command.isClassEnd();
			boolean isInterfaceEnd = command.isInterfaceEnd();
			if (!isClassOrInterfaceStart && !isClassEnd && !isInterfaceEnd && !command.isMethodFunctionOrFormStart()) {
				command = command.getNext();
				continue;
			}

			// move to the first comment line that possibly precedes the class or method
			Command changeCommand = command;
			while (changeCommand.getFirstTokenLineBreaks() == 1 && changeCommand.getPrev() != null && changeCommand.getPrev().isCommentLine())
				changeCommand = changeCommand.getPrev();
			commandForErrorMsg = changeCommand;

			Command prev = changeCommand.getPrev();
			boolean prevMatches;
			if (prev == null) {
				prevMatches = false;
			} else if (isClassOrInterfaceStart) {
				prevMatches = prev.isClassOrInterfaceEnd();
			} else if (isClassEnd) { 
				prevMatches = prev.isMethodFunctionOrFormEnd();
			} else {
				prevMatches = prev.isMethodFunctionOrFormEnd() || prev.isClassStart();
			}
			
			if (prevMatches && !isCommandBlocked(changeCommand)) {
				Token token = changeCommand.getFirstToken();
				int lineBreaks;
				if (isClassOrInterfaceStart) {
					lineBreaks = emptyLinesBetweenClasses + 1;
				} else if (isClassEnd || prev.isClassStart()) {
					lineBreaks = emptyLinesBetweenClassAndMethod + 1;
				} else {
					lineBreaks = emptyLinesBetweenMethods + 1;
				}
					
				if (token.lineBreaks != lineBreaks) {
					token.lineBreaks = lineBreaks;
					code.addRuleUse(this, changeCommand, token);
				}
			}
			
			if (isClassOrInterfaceStart || isClassEnd || isInterfaceEnd)
				command = command.getNext();
			else
				command = command.getNextSibling(); // no need to move inside method code
		}

	}

}