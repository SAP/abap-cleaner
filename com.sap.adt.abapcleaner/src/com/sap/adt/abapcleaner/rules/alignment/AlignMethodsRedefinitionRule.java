package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignMethodsRedefinitionRule extends AlignMethodsWithoutParamsRuleBase {
   private enum Columns  {
      KEYWORD,
      METHOD_NAME,
      REDEFINITION,
      LINE_END_COMMENT;

		public int getValue() { return this.ordinal(); }
   }
	private static final int MAX_COLUMN_COUNT = 4;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_METHODS_REDEFINITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align METHODS ... REDEFINITION"; }

	@Override
	public String getDescription() { return "Aligns consecutive METHODS ... [FINAL] REDEFINITION declarations." + System.lineSeparator() 
													  + "Activate this rule if you want to override the result of 'Align METHODS declarations' with special settings."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
		return  ""
			+ LINE_SEP + "  \" comment on first group of redefinitions" 
			+ LINE_SEP + "  METHODS if_any_interface~medium_method_name" 
			+ LINE_SEP + "    REDEFINITION." 
			+ LINE_SEP + "  METHODS if_any_interface~short_name" 
			+ LINE_SEP + "    REDEFINITION." 
			+ LINE_SEP + "  METHODS if_any_interface~extra_long_method_name" 
			+ LINE_SEP + "    REDEFINITION." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  \" comment on second group of redefinitions" 
			+ LINE_SEP + "  METHODS if_other_interface~any_method_name" 
			+ LINE_SEP + "    FINAL REDEFINITION." 
			+ LINE_SEP + "  METHODS if_other_interface~other_method_name" 
			+ LINE_SEP + "    FINAL REDEFINITION." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  METHODS" 
			+ LINE_SEP + "    if_other_interface~create REDEFINITION." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  \" chains are aligned independently:" 
			+ LINE_SEP + "  METHODS:" 
			+ LINE_SEP + "    if_yet_another_interface~any_method_name" 
			+ LINE_SEP + "      REDEFINITION," 
			+ LINE_SEP + "    if_yet_another_interface~other_method_name" 
			+ LINE_SEP + "      REDEFINITION," 
			+ LINE_SEP + "    if_yet_another_interface~create" 
			+ LINE_SEP + "      REDEFINITION."
			+ LINE_SEP + "";
   }

	private final ConfigValue[] configValues = new ConfigValue[] { configContinueAfterKeyword, configContinueAfterMethodName, configAlignAcrossEmptyLines, configAlignAcrossCommentLines };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignMethodsRedefinitionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean isMatchForFirstCommand(Command command, int pass) {
		return command.getFirstToken().matchesOnSiblings(false, "METHODS", TokenSearch.makeOptional(ABAP.COLON_SIGN_STRING), TokenSearch.ANY_IDENTIFIER, "REDEFINITION|FINAL REDEFINITION");
	}

	@Override
	protected boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass) {
		return command.getFirstToken().matchesOnSiblings(false, keywordOfFirstCommand, TokenSearch.ANY_IDENTIFIER, "REDEFINITION|FINAL REDEFINITION");
	}

	@Override
	protected int getMaxColumnCount() { 
		return MAX_COLUMN_COUNT; 
	}

	@Override
	protected Token buildTableLine(AlignTable table, Token token, boolean isChain) throws UnexpectedSyntaxException {
		AlignLine line = table.addLine();
		
		// keyword METHODS, possibly with ":"
		if (token.isKeyword("METHODS")) {
			Token nextToken = token.getNextCodeSibling();
			AlignCell keywordCell;
			if (nextToken != null && nextToken.isChainColon()) {
				Token chainColon = nextToken;
				keywordCell = new AlignCellTerm(Term.createForTokenRange(token, chainColon));
				token = chainColon;
			} else {
				keywordCell = new AlignCellToken(token);
			}
			line.setCell(Columns.KEYWORD.getValue(), keywordCell);
			token = token.getNext();
		}

		// method name
		Token methodName = token;
		line.setCell(Columns.METHOD_NAME.getValue(), new AlignCellToken(methodName));

		// keyword [FINAL] REDEFINITION
		Token redefinitionEnd = methodName.getNext().getLastTokenOnSiblings(true, "REDEFINITION|FINAL REDEFINITION");
		if (redefinitionEnd == null) {
			table.removeLastLine();
			return null;
		}
		
		Term redefinition = Term.createForTokenRange(methodName.getNext(), redefinitionEnd);
		line.setCell(Columns.REDEFINITION.getValue(), new AlignCellTerm(redefinition));
		token = redefinition.getNext();

		while (!token.isCommaOrPeriod() && token.getNext() != null) {
			token = token.getNext();
		}

		// comment at line end (except in BEGIN OF lines, where "line == null")
		if (token.getNext() != null && token.getNext().isCommentAfterCode()) {
			token = token.getNext();
			line.setCell(Columns.LINE_END_COMMENT.getValue(), new AlignCellToken(token));
		}

		// return the last Token that belonged to this line (a comma, period, or comment at line end)
		return token;
	}
}
