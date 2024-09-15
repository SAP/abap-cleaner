package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellToken;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;

public class AlignFormDeclarationRule extends RuleForCommands {
   private enum Columns  {
      FORM,
      FORM_NAME,
      PARAMETER_GROUP,
      PARAMETER_NAME,
      TYPE_OR_LIKE;

		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 5;

	private static final String[] parameterGroups = new String[] { "TABLES", "USING", "CHANGING", "RAISING" };

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_FORM_DECLARATION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align FORM declarations"; }

	@Override
	public String getDescription() { return "Aligns obsolete subroutine declarations with FORM."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 10, 31); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
      return  LINE_SEP + "FORM any_subroutine USING iv_any_value TYPE string." 
				+ LINE_SEP 
				+ LINE_SEP + "  \" any subroutine implementation" 
				+ LINE_SEP + "ENDFORM." 
				+ LINE_SEP 
				+ LINE_SEP 
				+ LINE_SEP + "FORM other_subroutine USING iv_any_value TYPE i iv_other_value TYPE string CHANGING cv_third_value TYPE i." 
				+ LINE_SEP + "  \" other subroutine implementation" 
				+ LINE_SEP + "ENDFORM." 
				+ LINE_SEP 
				+ LINE_SEP 
				+ LINE_SEP + "FORM third_subr_with_a_long_name TABLES it_any_table STRUCTURE ty_s_any_struc" 
				+ LINE_SEP + "  it_other_table TYPE STANDARD TABLE it_third_table it_fourth_table TYPE ty_tt_any" 
				+ LINE_SEP + "  CHANGING ct_table TYPE ty_tt_table cs_struc TYPE LINE OF ty_tt_any cs_other_struc LIKE cs_any" 
				+ LINE_SEP + "    cs_third_struc LIKE LINE OF ct_table." 
				+ LINE_SEP + "  \" third subroutine implementation" 
				+ LINE_SEP + "ENDFORM." 
				+ LINE_SEP 
				+ LINE_SEP 
				+ LINE_SEP + "FORM fourth_subroutine" 
				+ LINE_SEP + "  USING" 
				+ LINE_SEP + "    VALUE(iv_any) TYPE string" 
				+ LINE_SEP + "    iv_other TYPE REF TO object" 
				+ LINE_SEP + "  RAISING" 
				+ LINE_SEP + "    cx_any_exception RESUMABLE(cx_other_exception) cx_third_exception." 
				+ LINE_SEP + "  \" fourth subroutine implementation" 
				+ LINE_SEP + "ENDFORM.";
   }

	final ConfigIntValue configParamCountBehindFormName = new ConfigIntValue(this, "ParamCountBehindFormName", "Continue after FORM name for up to", "parameters", 0, 3, 100);
	final ConfigBoolValue configContinueAfterParamGroupKeyword = new ConfigBoolValue(this, "ContinueAfterParamGroupKeyword", "Continue after TABLES / USING / CHANGING / RAISING", true);
	final ConfigBoolValue configAlignTypes = new ConfigBoolValue(this, "AlignTypes", "Align TYPEs", true);
	final ConfigBoolValue configAddEmptyLine = new ConfigBoolValue(this, "AddEmptyLine", "Add empty line after multi-line FORM declaration", true);
	final ConfigBoolValue configRemoveEmptyLine = new ConfigBoolValue(this, "RemoveEmptyLine", "Remove empty line after one-line FORM declaration", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configParamCountBehindFormName, configContinueAfterParamGroupKeyword, configAlignTypes, configAddEmptyLine, configRemoveEmptyLine };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignFormDeclarationRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.firstCodeTokenIsKeyword("FORM"))
			return false;
		
		// build the align table
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		AlignLine line = table.addLine();
		
		// add FORM keyword
		Token formKeyword = command.getFirstCodeToken();
		line.setCell(Columns.FORM.getValue(), new AlignCellToken(formKeyword));

		// add FORM name
		Token formName = formKeyword.getNextCodeSibling();
		if (!formName.isIdentifier()) // pro forma
			return false;
		line.setCell(Columns.FORM_NAME.getValue(), new AlignCellToken(formName));

		boolean isInRaisingGroup = false;
		Token token = formName.getNextCodeSibling();
		while(token != null && !token.isPeriod()) {
			// add parameter group keyword
			if (token.isAnyKeyword(parameterGroups)) {
				isInRaisingGroup = token.isKeyword("RAISING");
				line.setCell(Columns.PARAMETER_GROUP.getValue(), new AlignCellToken(token));
				token = token.getNextCodeSibling();
			}
			
			// add identifier
			if (token.isIdentifier()) {
				line.setCell(Columns.PARAMETER_NAME.getValue(), new AlignCellToken(token));
				token = token.getNextCodeSibling();
			} else if (token.textEqualsAny("VALUE(", "RESUMABLE(")) {
				Term parameterTerm;
				try {
					parameterTerm = Term.createForTokenRange(token, token.getNextSibling());
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxBeforeChanges(this, e);
				}
				line.setCell(Columns.PARAMETER_NAME.getValue(), new AlignCellTerm(parameterTerm));
				token = parameterTerm.lastToken.getNextCodeSibling();
			} else {
				throw new UnexpectedSyntaxBeforeChanges(this, token, "Unexpected parameter for a FORM definition");
			}
			if (isInRaisingGroup) {
				// if TYPEs are aligned, do not let exception names influence the width of the PARAMETER_NAME column
				line.getCell(Columns.PARAMETER_NAME.getValue()).setOverrideTextWidth(1);
			}
			if (token == null || token.isPeriod())
				break;

			// add TYPE section (if any)
			if (!isInRaisingGroup && token.isAnyKeyword("TYPE", "LIKE", "STRUCTURE")) {
				Token typeStart = token;
				if (token.isKeyword("STRUCTURE")) {
					token = token.getNextCodeSibling();
				} else { // "TYPE", "LIKE"
					// read until the last token, which is  
	  				// - the keyword 'TABLE' for the (always generic) 'TYPE {ANY|HASHED|INDEX|SORTED|STANDARD} TABLE'
					// - the identifier of a complete type (possibly after 'LINE OF', 'REF TO'), e.g. i, abap_bool, char30,  
					//   ty_s_any, struc-component, class=>attribute, LINE OF ty_tt_table, REF TO cl_any_class
					// - the identifier of a generic type: any, c, clike, csequence, data, decfloat, n, numeric, REF TO object, 
					//   p, simple, table, x, xsequence
					do {
						token = token.getNextCodeSibling();
					} while (token != null && !token.isIdentifier() && !token.isKeyword("TABLE"));
				}
				Term typeTerm;
				try {
					typeTerm = Term.createForTokenRange(typeStart, token);
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxBeforeChanges(this, e);
				}
				line.setCell(Columns.TYPE_OR_LIKE.getValue(), new AlignCellTerm(typeTerm));
				token = token.getNextCodeSibling();
			}
			
			line = table.addLine(); 
		}
		
		if (table.getLineCount() > 1 && line.getCell(Columns.PARAMETER_NAME.getValue()) == null)
			table.removeLastLine();
		
		boolean breakAfterFormName = (table.getLineCount() > configParamCountBehindFormName.getValue());
		if (breakAfterFormName) {
			table.getColumn(Columns.FORM_NAME.getValue()).setForceLineBreakAfter(false, ABAP.INDENT_STEP);
			table.getColumn(Columns.PARAMETER_GROUP.getValue()).setForceIndent(Columns.FORM.getValue(), ABAP.INDENT_STEP);
		}
		if (!configContinueAfterParamGroupKeyword.getValue()) {
			table.getColumn(Columns.PARAMETER_GROUP.getValue()).setForceLineBreakAfter(false, ABAP.INDENT_STEP);
		}
		if (!configAlignTypes.getValue()) {
			try {
				Command[] changedCommands = table.getColumn(Columns.TYPE_OR_LIKE.getValue()).joinIntoPreviousColumns(true);
				code.addRuleUses(this, changedCommands);
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);
			}
		}
		
		int basicIndent = command.getFirstToken().spacesLeft;
		int firstLineBreaks = command.getFirstToken().lineBreaks;
		Command[] changedCommands = table.align(basicIndent, firstLineBreaks, false, true, false);
		for (Command changedCommand : changedCommands) {
			code.addRuleUse(this, changedCommand);
		}
		
		// if configured and required, add or remove an empty line above the first Command in the FORM implementation
		if (command.hasChildren()) {
			boolean isMultiLine = command.containsInnerLineBreaks(false);
			Command nextCommand = command.getNext();
			Token nextToken = nextCommand.getFirstToken();
			
			if (isCommandBlocked(nextCommand)) {
				// do nothing
			} else if (isMultiLine && nextToken.lineBreaks < 2 && configAddEmptyLine.getValue()) {
				nextToken.lineBreaks = 2;
				code.addRuleUse(this, nextCommand);
			} else if (!isMultiLine && nextToken.lineBreaks >= 2 && configRemoveEmptyLine.getValue()) {
				nextToken.lineBreaks = 1;
				code.addRuleUse(this, nextCommand);
			}
		}
		return false; // code.addRuleUse was already called above
	}
}
