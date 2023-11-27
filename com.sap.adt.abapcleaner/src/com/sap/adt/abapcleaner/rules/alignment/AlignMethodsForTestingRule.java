package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignMethodsForTestingRule extends AlignMethodsWithoutParamsRuleBase {
   private enum Columns  {
      KEYWORD,
      METHOD_NAME,
      FOR_TESTING,
      RAISING,
      LINE_END_COMMENT;

		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 5;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_METHODS_FOR_TESTING; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align METHODS ... FOR TESTING"; }

	@Override
	public String getDescription() { return "Aligns consecutive METHODS ... FOR TESTING declarations."; }
	
	@Override
	public String getHintsAndRestrictions() { return "Activate this rule if you want to override the result of 'Align METHODS declarations' with special settings."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }
   @Override
   public String getExample() {
		return  ""
				+ LINE_SEP + "  \" comment on first group of test methods" 
				+ LINE_SEP + "  METHODS test_setup" 
				+ LINE_SEP + "    FOR TESTING." 
				+ LINE_SEP + "  METHODS test_functionality" 
				+ LINE_SEP + "    FOR TESTING." 
				+ LINE_SEP + "  METHODS test_display" 
				+ LINE_SEP + "    FOR TESTING." 
				+ LINE_SEP + "" 
				+ LINE_SEP + "  \" comment on second group of test methods" 
				+ LINE_SEP + "  METHODS any_test_method_name" 
				+ LINE_SEP + "    FOR TESTING RAISING cx_static_check." 
				+ LINE_SEP + "  METHODS other_test_method_name" 
				+ LINE_SEP + "    FOR TESTING RAISING cx_static_check." 
				+ LINE_SEP + "" 
				+ LINE_SEP + "  METHODS test_invalid_input" 
				+ LINE_SEP + "    FOR TESTING." 
				+ LINE_SEP + "" 
				+ LINE_SEP + "  \" chains are aligned independently:" 
				+ LINE_SEP + "  METHODS:" 
				+ LINE_SEP + "    first_test_method" 
				+ LINE_SEP + "      FOR TESTING," 
				+ LINE_SEP + "    second_test_method" 
				+ LINE_SEP + "      FOR TESTING RAISING cx_message," 
				+ LINE_SEP + "    last_test_method" 
				+ LINE_SEP + "      FOR TESTING."
				+ LINE_SEP + "";
   }

	private static final double FILL_RATIO_TO_JUSTIFY_OWN_COLUMN = 0.2; // exposing this to configuration would be too much

	private final ConfigValue[] configValues = new ConfigValue[] { configContinueAfterKeyword, configContinueAfterMethodName, configAlignAcrossEmptyLines, configAlignAcrossCommentLines };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignMethodsForTestingRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean isMatchForFirstCommand(Command command, int pass) {
		Token firstToken = command.getFirstToken();
		if (!firstToken .isAnyKeyword("METHODS"))
			return false;
		if (command.isSimpleChain()) {
			// if a chain contains methods FOR TESTING, also allow a mixture of FOR TESTING methods with other methods like 'setup', but only WITHOUT parameters
			if (!firstToken .matchesOnSiblings(false, TokenSearch.ASTERISK, "FOR", "TESTING"))
				return false;
			return firstToken .matchesOnSiblings(false, "METHODS", ABAP.COLON_SIGN_STRING, TokenSearch.ANY_IDENTIFIER, "FOR", "TESTING")
			    || firstToken .matchesOnSiblings(false, "METHODS", ABAP.COLON_SIGN_STRING, TokenSearch.ANY_IDENTIFIER, ABAP.COMMA_SIGN_STRING);
		} else {
			return firstToken .matchesOnSiblings(false, "METHODS", TokenSearch.ANY_IDENTIFIER, "FOR", "TESTING");
		}
	}

	@Override
	protected boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass) {
		return command.getFirstToken().matchesOnSiblings(false, keywordOfFirstCommand, TokenSearch.ANY_IDENTIFIER, "FOR", "TESTING");
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

		// keywords FOR TESTING 
		if (methodName.getNext().matchesOnSiblings(false, "FOR", "TESTING")) {
			Term forTesting = Term.createForTokenRange(methodName.getNext(), methodName.getNext().getNextSibling());
			line.setCell(Columns.FOR_TESTING.getValue(), new AlignCellTerm(forTesting));
			token = forTesting.getNext();

			// keyword RAISING
			if (token.isKeyword("RAISING")) {
				Token period = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
				Term raisingTerm = Term.createForTokenRange(token, period.getPrevCodeSibling());
				line.setCell(Columns.RAISING.getValue(), new AlignCellTerm(raisingTerm));
				token = period;
			}
		} else {
			// only inside chains, allow a mixture of FOR TESTING methods and 'simple' methods without parameters like "setup"
			if (!isChain){
				table.removeLastLine();
				return null;
			}
			token = methodName.getNext();
		}
		
		while (!token.isCommaOrPeriod() && token.getNext() != null) {
			token = token.getNext();
		}

		// comment at line end (except in BEGIN OF lines, where "line == null")
		if (token.getNext() != null && token.getNext().isCommentAfterCode()) {
			token = token.getNext();
			if (line != null)
				line.setCell(Columns.LINE_END_COMMENT.getValue(), new AlignCellToken(token));
		}
		
		return token;
	}
	
	@Override
	protected void joinColumns(Code code, AlignTable table, boolean isChain) throws UnexpectedSyntaxException {
		// decide whether RAISING statements and comments should really be aligned (i.e. whether horizontal space should be reserved for them):
		// if only one single line (or less than 20% of all lines) has content in this column, then join the content into a previous column
		for (int i = Columns.LINE_END_COMMENT.getValue(); i >= Columns.RAISING.getValue(); --i) {
			AlignColumn testColumn = table.getColumn(i);
			if (testColumn.isEmpty())
				continue;
			if (testColumn.getCellCount() <= 1 || testColumn.getCellCount() < (int) (table.getLineCount() * FILL_RATIO_TO_JUSTIFY_OWN_COLUMN)) {
				testColumn.joinIntoPreviousColumns(true);
			}
		}

		// only now call the super class method, because this may change the code
		super.joinColumns(code, table, isChain);
	}
}
