package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP.SyField;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.SyFieldAnalyzer;

public class DescribeTableRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs") };

	@Override
	public RuleID getID() { return RuleID.DESCRIBE_TABLE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace DESCRIBE TABLE ... LINES with lines( )"; }

	@Override
	public String getDescription() { return "Replaces DESCRIBE TABLE ... LINES with the built-in function lines( )."; }

	@Override
	public String getHintsAndRestrictions() { return "Statements cannot be replaced if they use other additions (KIND ... or OCCURS ...) or SY-TFILL / SY-TLENG are evaluated afterwards."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 3, 7); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD describe_table_lines." 
			+ LINE_SEP + "    \" the following cases can be replaced with the built-in function lines( ):" 
			+ LINE_SEP + "    DESCRIBE TABLE its_any_table LINES ev_any_line_count." 
			+ LINE_SEP + "    DESCRIBE TABLE its_any_table LINES DATA(lv_any_line_count)." 
			+ LINE_SEP + "    DESCRIBE TABLE its_other_table LINES FINAL(lv_other_line_count)." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" DESCRIBE TABLE with other additions cannot be replaced" 
			+ LINE_SEP + "    DESCRIBE TABLE it_third_table KIND DATA(lv_kind) LINES lv_any_line_count." 
			+ LINE_SEP + "    DESCRIBE TABLE it_third_table OCCURS FINAL(lv_init_mem_requirement)." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" DESCRIBE TABLE with any subsequent evaluation of SY-TLENG or SY-TFILL cannot be replaced" 
			+ LINE_SEP + "    DESCRIBE TABLE it_third_table LINES lv_any_line_count." 
			+ LINE_SEP + "    DATA(lv_line_count) = sy-tfill." 
			+ LINE_SEP + "    DATA(lv_line_length_in_bytes) = sy-tleng." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" here, SY-TLENG is also evaluated in the program flow after DESCRIBE TABLE"
			+ LINE_SEP + "    \" (but only for the second one, so the first one can be changed)"
			+ LINE_SEP + "    DO 3 TIMES." 
			+ LINE_SEP + "      IF sy-index = 3." 
			+ LINE_SEP + "        RETURN sy-tleng." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "      DESCRIBE TABLE it_third_table LINES lv_any_line_count." 
			+ LINE_SEP + "      DESCRIBE TABLE it_fourth_table LINES lv_line_count." 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public DescribeTableRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean skipOutsideMethodFunctionOrForm() {
		// ensure that we are inside a METHOD, FUNCTION or FORM, otherwise (esp. if the standalone version gets a small 
		// code snippet), we cannot be sure that the statement is not indeed followed by an evaluation of SY-TFILL or SY-TLENG
		return true; 
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// From the possible syntax 'DESCRIBE TABLE itab [KIND knd] [LINES lin] [OCCURS n].', 
		// only process cases that match 'DESCRIBE TABLE itab LINES lin.' (excluding cases with chains, comments, or pragmas) 
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, "DESCRIBE", "TABLE", TokenSearch.ANY_IDENTIFIER, "LINES"))
			return false;
		if (firstToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "KIND") || firstToken.matchesOnSiblings(true, TokenSearch.ASTERISK, "OCCURS"))
			return false;

		// determine the keywords and the table identifier
		Token describeKeyword = firstToken;
		Token tableKeyword = firstToken.getNextSibling();
		Token tableName = tableKeyword.getNextSibling();
		Token linesKeyword = tableName.getNextSibling();
		
		// determine the receiving variable, which may also be an inline declaration DATA(...) or FINAL(...)
		Token lineCountVarStart = linesKeyword.getNextSibling();
		if (lineCountVarStart.isPragmaOrComment())
			return false;
		Token lineCountVarEnd = lineCountVarStart.getNextSiblingWhileLevelOpener();

		// exclude cases where a SY- field is in the write position after LINES, e.g. 'DESCRIBE TABLE lt_any LINES sy-tfill.'
		if (lineCountVarStart.isIdentifier() && lineCountVarStart.textStartsWith("sy-"))
			return false;
		
		// determine the period (chained statements will not be processed)
		Token period = lineCountVarEnd.getNextSibling();
		if (!period.isPeriod())
			return false;
		
		// ensure that SY-TFILL and SY-TLENG are never used in the program flow following this statement
		ArrayList<Command> commandsReadingSyTFill = SyFieldAnalyzer.getSyFieldReadersFor(SyField.TFILL, command);
		if (commandsReadingSyTFill.size() > 0)
			return false;
		ArrayList<Command> commandsReadingSyTLeng = SyFieldAnalyzer.getSyFieldReadersFor(SyField.TLENG, command);
		if (commandsReadingSyTLeng.size() > 0)
			return false;

		// transform "DESCRIBE TABLE itab LINES lin." into "lin = lines( itab )."
		lineCountVarStart.copyWhitespaceFrom(describeKeyword);
		describeKeyword.removeFromCommand();
		tableKeyword.removeFromCommand();
		tableName.removeFromCommand();
		linesKeyword.removeFromCommand();
		period.insertLeftSibling(Token.createForAbap(0, 1, "=", period.sourceLineNum), false);
		Token linesIdentifier = Token.createForAbap(0, 1, "lines(", period.sourceLineNum);
		period.insertLeftSibling(linesIdentifier, false, true);
		period.insertLeftSibling(Token.createForAbap(0, 1, ")", period.sourceLineNum), false, true);
		tableName.setWhitespace();
		linesIdentifier.insertNext(tableName);

		code.addRuleUse(this, command);
		return true;
	}
}