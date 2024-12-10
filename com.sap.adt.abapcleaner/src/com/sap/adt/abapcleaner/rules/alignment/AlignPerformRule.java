package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
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
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellToken;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;

public class AlignPerformRule extends RuleForCommands {
   private enum Columns  {
      PERFORM_SUBR,
      PARAMETER_GROUP,
      PARAMETER_NAME;

		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 3;

	private static final String[] parameterGroups = new String[] { "TABLES", "USING", "CHANGING", "OF" };

	private final static RuleReference[] references = new RuleReference[] { 
	   new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Line-break multiple parameters", "#line-break-multiple-parameters") };

	@Override
	public RuleID getID() { return RuleID.ALIGN_PERFORM; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align PERFORM parameters"; }

	@Override
	public String getDescription() { return "Aligns the parameters of obsolete subroutine calls with PERFORM."; }

	@Override
	public String getHintsAndRestrictions() { return "Alignment of PERFORM: chains is not supported."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 10, 31); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
      return  LINE_SEP + "FORM any_form." 
				+ LINE_SEP + "  PERFORM any_subroutine_with_a_long_name USING lv_any_value 42 'ABCDEFGHIJKLMN' gc_any_constant '3.14159265'." 
				+ LINE_SEP 
				+ LINE_SEP + "  PERFORM other_subr" 
				+ LINE_SEP + "    TABLES lt_any" 
				+ LINE_SEP + "    USING lt_any_table[] lv_other_table[]." 
				+ LINE_SEP 
				+ LINE_SEP + "  PERFORM third_subr IN PROGRAM any_program TABLES lt_any_table lt_other_table" 
				+ LINE_SEP + "          USING lv_other_value." 
				+ LINE_SEP 
				+ LINE_SEP + "  PERFORM (lv_subr_name) IN PROGRAM other_program" 
				+ LINE_SEP + "  TABLES lr_table_ref->*" 
				+ LINE_SEP + "    <lt_other_table>" 
				+ LINE_SEP + "    USING if_any_interface=>co_any_constant" 
				+ LINE_SEP + "      lo_any_instance->mv_any_attribute" 
				+ LINE_SEP + "      CHANGING <ls_any_structure>-any_component IF FOUND." 
				+ LINE_SEP 
				+ LINE_SEP + "  PERFORM (lv_subr_name)  IN PROGRAM (lv_program_name) IF  FOUND" 
				+ LINE_SEP + "    USING 100  200   300 400   500 600 700 800 \" comment"  
				+ LINE_SEP + "       'abcde' 'fghij' 'klmno' 'pqrst'  'uwvxy'"
				+ LINE_SEP + "      gc_any_constant gc_other_constant" 
				+ LINE_SEP + "   gc_third_constant gc_fourth_constant" 
				+ LINE_SEP + "    CHANGING lv_any_value lv_other_value." 
				+ LINE_SEP 
				+ LINE_SEP + "  PERFORM lv_subr_index OF form_one form_two form_three form_four" 
				+ LINE_SEP + "   form_five form_six form_seven." 
				+ LINE_SEP 
				+ LINE_SEP + "  \" obsolete syntax:" 
				+ LINE_SEP + "  PERFORM" 
				+ LINE_SEP + "    subr_name(prog_name)" 
				+ LINE_SEP + "    IF FOUND" 
				+ LINE_SEP + "    TABLES lt_any USING" 
				+ LINE_SEP + "    lv_any_value CHANGING lv_other_value." 
				+ LINE_SEP + "ENDFORM.";
   }

	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length", "", MIN_LINE_LENGTH_ABAP, DEFAULT_LINE_LENGTH_ABAP, ABAP.MAX_LINE_LENGTH);
	final ConfigIntValue configParamCountAfterPerform = new ConfigIntValue(this, "ParamCountAfterPerform", "Continue behind PERFORM for up to", "parameters", 0, 3, 100);
	final ConfigBoolValue configBreakAfterAdditions = new ConfigBoolValue(this, "BreakAfterAdditions", "Always break after call if additions IN PROGRAM or IF FOUND are used", false);
	final ConfigBoolValue configAlignWithFormName = new ConfigBoolValue(this, "AlignWithFormName", "Align TABLES / USING / CHANGING with subroutine name", false);
	final ConfigBoolValue configContinueAfterParamGroupKeyword = new ConfigBoolValue(this, "ContinueAfterParamGroupKeyword", "Continue after TABLES / USING / CHANGING", true);
	final ConfigIntValue configParamCountToCondense = new ConfigIntValue(this, "ParamCountToCondense", "Condense parameter lists starting from", "parameters", 0, 16, 100);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configParamCountAfterPerform, configBreakAfterAdditions, configAlignWithFormName, configContinueAfterParamGroupKeyword, configParamCountToCondense };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignPerformRule (Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!command.firstCodeTokenIsKeyword("PERFORM")) {
			return false;
		} else if (command.containsChainColon()) {
			// TODO: add option to pre-process chains?
			return false;
		}

		// read the beginning of the Command
		// 'PERFORM subr|(sname)' or 'PERFORM n' or obsolete 'PERFORM subr(prog)'
		Token performKeyword = command.getFirstCodeToken();
		Token token = performKeyword.getNextCodeSibling();
		if (token.getOpensLevel()) 
			token = token.getNextSibling();
		token = token.getNextCodeSibling();
		if (token == null) // pro forma
			return false;
		
		// exclude 'PERFORM subr ON { {COMMIT [LEVEL idx]} | ROLLBACK }.'
		if (token.isKeyword("ON"))
			return false;
		
		// 'IN PROGRAM [prog|(pname)]'
		boolean hasAdditions = false;
		if (token.matchesOnSiblings(true, "IN", "PROGRAM")) {
			hasAdditions = true;
			token = token.getNextCodeSibling().getNextCodeSibling();
			if (token.getOpensLevel()) {
				token = token.getNextSibling();
				token = token.getNextSibling();
			} else if (token.isCommaOrPeriod()) {
				// do nothing: the program name may be omitted if neither "IF FOUND" nor any parameters are specified, cp.
				// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapperform_form.htm
			} else {
				token = token.getNextSibling();
			}
		}
		
		// '[IF FOUND]'
		if (token.matchesOnSiblings(true, "IF", "FOUND")) {
			hasAdditions = true;
			token = token.getNextCodeSibling().getNextCodeSibling();
		}
		
		// possibly, move 'IF FOUND' from behind the parameters to the correct place
		boolean changed = false;
		if (moveLateIfFound(token)) {
			changed = true;
			hasAdditions = true;
		}
		
		Term performSubrTerm = null;
		AlignTable table = null;
		try {
			// put everything up to now into one Term which will later be condensed
			performSubrTerm = Term.createForTokenRange(performKeyword, token.getPrevCodeSibling());

			// build the align table
			table = buildAlignTable(performSubrTerm);

		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, e);
		}

		// condense the Term
		if (performSubrTerm.condense())
			changed = true;

		// if no parameters were found, there is nothing else to do
		if (table.getLineCount() == 1 && table.getLine(0).getCell(Columns.PARAMETER_NAME.getValue()) == null) 
			return changed;

		// determine indents and forced line breaks after columns
		setForceLineBreakAndIndent(table, performSubrTerm, hasAdditions);

		// if parameter lists shall be condensed, reduce the table to the first parameter of each parameter group
		boolean condense = table.getLineCount() >= configParamCountToCondense.getValue();
		if (condense) 
			table.removeAllLinesWithOutCellIn(Columns.PARAMETER_GROUP.getValue());
		
		// align the table
		int basicIndent = command.getFirstToken().spacesLeft;
		int firstLineBreaks = command.getFirstToken().lineBreaks;
		Command[] changedCommands = table.align(basicIndent, firstLineBreaks, false, true, false);
		for (Command changedCommand : changedCommands) {
			code.addRuleUse(this, changedCommand);
		}
		
		// condense parameter lists, starting from the first parameter of each parameter group, which was aligned above
		if (condense && condenseParameterLists(command, table)) {
			changed = true;
		}

		return changed; 
	}

	private boolean moveLateIfFound(Token token) throws IntegrityBrokenException, UnexpectedSyntaxBeforeChanges {
		Token lateFound = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "IF", "FOUND", ".");
		if (lateFound == null) 
			return false;
		
		lateFound = lateFound.getPrevCodeSibling();
		Token lateIf = lateFound.getPrevCodeSibling();
		if (!lateIf.isKeyword() || !lateFound.isKeyword()) // pro forma
			return false;
		
		try {
			Term lateIfFound = Term.createForTokenRange(lateIf, lateFound);
			lateIfFound.removeFromCommand(false);
			token.insertLeftSibling(lateIfFound);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, e);
		}
		return true;
	}

	private AlignTable buildAlignTable(Term performSubrTerm) throws UnexpectedSyntaxException {
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		AlignLine line = table.addLine();
		line.setCell(Columns.PERFORM_SUBR.getValue(), new AlignCellTerm(performSubrTerm));

		// read parameter groups "TABLES", "USING", "CHANGING" and parameters
		Token token = performSubrTerm.lastToken.getNextCodeSibling();
		while (token != null && !token.isCommaOrPeriod()) {
			// add parameter group keyword "TABLES", "USING", "CHANGING"; 
			// for 'PERFORM n OF subr1 subr2 ...', "OF" can be processed the same way here
			if (token.isAnyKeyword(parameterGroups)) {
				line.setCell(Columns.PARAMETER_GROUP.getValue(), new AlignCellToken(token));
				token = token.getNextCodeSibling();
			} 
				
			// add literal or identifier; note that 'so_table[]' is also one single Token
			// - 1, abap_true, '3.14', 'abc', ...
			// - if_any=>co_constant, so_table[], ls_struc-comp, lr_tab->*, <ls_any>, <ls_any>-comp, lo_instance->attribute
			line.setCell(Columns.PARAMETER_NAME.getValue(), new AlignCellToken(token));
			line = table.addLine();
			token = token.getNextCodeSibling();
		}

		if (table.getLineCount() > 1 && line.getCell(Columns.PARAMETER_NAME.getValue()) == null) // && ... pro forma
			table.removeLastLine();
		
		return table;
	}

	private void setForceLineBreakAndIndent(AlignTable table, Term performSubrTerm, boolean hasAdditions) {
		// nothing to do for for 'PERFORM n OF subr1 subr2 ...' 
		AlignCell firstParamGroup = table.getLine(0).getCell(Columns.PARAMETER_GROUP.getValue());
		if (firstParamGroup != null && firstParamGroup.getFirstToken().isKeyword("OF"))
			return;
		
		boolean breakAfterPerformSubr = (table.getLineCount() > configParamCountAfterPerform.getValue());
		if (breakAfterPerformSubr || hasAdditions && configBreakAfterAdditions.getValue()) {
			int offsetForParamGroup = (configAlignWithFormName.getValue() ? "PERFORM".length() + 1 : ABAP.INDENT_STEP);
			table.getColumn(Columns.PERFORM_SUBR.getValue()).setForceLineBreakAfter(false, offsetForParamGroup);
		}
		
		if (!configContinueAfterParamGroupKeyword.getValue()) {
			table.getColumn(Columns.PARAMETER_GROUP.getValue()).setForceLineBreakAfter(false, ABAP.INDENT_STEP);
		}
	}

	private boolean condenseParameterLists(Command command, AlignTable table) {
		boolean changed = false;

		for (int lineIndex = 0; lineIndex < table.getLineCount(); ++lineIndex) {
			AlignCell firstParamCell = table.getLine(lineIndex).getCell(Columns.PARAMETER_NAME.getValue());
			if (firstParamCell == null) // pro forma
				continue;
			Token firstParam = firstParamCell.getFirstToken();

			// determine the last parameter of this group, which is either directly before the next parameter group keyword,
			// or before the period
			Token lastParam = null;
			if (lineIndex + 1 < table.getLineCount()) {
				AlignCell nextParamGroupCell = table.getLine(lineIndex + 1).getCell(Columns.PARAMETER_GROUP.getValue());
				if (nextParamGroupCell == null) // pro forma, since table.removeAllLinesWithOutCellIn() was called
					continue;
				lastParam = nextParamGroupCell.getFirstToken().getPrevCodeSibling();
			} else {
				lastParam = command.getLastCodeToken().getPrevCodeSibling();
			}

			// condense parameter list
			if (firstParam.condenseUpTo(lastParam, configMaxLineLength.getValue(), firstParam.getStartIndexInLine(), true)) {
				changed = true;
			}
		}
		return changed;
	}
}
