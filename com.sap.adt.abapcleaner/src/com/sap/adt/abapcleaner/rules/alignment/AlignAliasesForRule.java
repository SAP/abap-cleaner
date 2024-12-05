package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignAliasesForRule extends AlignDeclarationSectionRuleBase {
   private enum Columns  {
      KEYWORD_ALIASES,
      IDENTIFIER1,
      KEYWORD_FOR,
      IDENTIFIER2,
      LINE_END_COMMENT;

      public int getValue() {
         return this.ordinal();
      }
   }
   private static final int MAX_COLUMN_COUNT = 5;

	private static String[] declarationKeywords = new String[] { "ALIASES" };

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_ALIASES_FOR; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align ALIASES ... FOR ..."; }

	@Override
	public String getDescription() { return "Aligns consecutive ALIASES ... FOR ... declarations."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 19); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return  ""
			+ LINE_SEP + "  \" comment on first group of aliases" 
			+ LINE_SEP + "  ALIASES medium_method_name" 
			+ LINE_SEP + "    FOR if_any_interface~medium_method_name." 
			+ LINE_SEP + "  ALIASES short_name" 
			+ LINE_SEP + "    FOR if_any_interface~short_name." 
			+ LINE_SEP + "  ALIASES extra_long_method_name" 
			+ LINE_SEP + "    FOR if_any_interface~extra_long_method_name." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  \" comment on first group of aliases" 
			+ LINE_SEP + "  ALIASES any_method_name" 
			+ LINE_SEP + "    FOR if_other_interface~any_method_name." 
			+ LINE_SEP + "  ALIASES other_method_name" 
			+ LINE_SEP + "    FOR if_other_interface~other_method_name." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  ALIASES create" 
			+ LINE_SEP + "    FOR if_other_interface~create." 
			+ LINE_SEP + "" 
			+ LINE_SEP + "  \" chains are aligned independently:" 
			+ LINE_SEP + "  ALIASES:" 
			+ LINE_SEP + "    first_method_name" 
			+ LINE_SEP + "      FOR if_yet_another_interface~first_method_name," 
			+ LINE_SEP + "    second_method_name" 
			+ LINE_SEP + "      FOR if_yet_another_interface~second_method_name," 
			+ LINE_SEP + "    last_method_name" 
			+ LINE_SEP + "      FOR if_yet_another_interface~last_method_name." 
			+ LINE_SEP + "";
	}

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignAcrossEmptyLines, configAlignAcrossCommentLines };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignAliasesForRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean isMatchForFirstCommand(Command command, int pass) {
		return command.getFirstToken().matchesOnSiblings(false, "ALIASES", TokenSearch.makeOptional(ABAP.COLON_SIGN_STRING), TokenSearch.ANY_IDENTIFIER, "FOR");
	}

	@Override
	protected boolean isMatchForFurtherCommand(Command command, String keywordOfFirstCommand, int pass) {
		return command.getFirstToken().matchesOnSiblings(false, keywordOfFirstCommand, TokenSearch.ANY_IDENTIFIER, "FOR");
	}

	@Override
	protected void executeOn(Code code, Command startCommand, Command endCommand, boolean isChain, int pass) {
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);

		Command command = startCommand;
		if (command.containsInnerCommentLine())
			return;
		Token start = command.getFirstToken();

		// determine whether to start with line breaks
		Token offsetToken = (isChain ? start.getNext() : start);
		int firstLineBreaks = offsetToken.lineBreaks;

		// determine the basic indent of the sequence
		int basicIndent;
		if (isChain)
			basicIndent = (firstLineBreaks > 0) ? start.getStartIndexInLine() + 2 : start.getNext().getEndIndexInLine() + 1;
		else
			basicIndent = start.getStartIndexInLine();

		do {
			// skip comment lines
			if (isChain) {
				while (start != null && start.isCommentLine())
					start = start.getNext();
				if (start == null)
					break;
			} else {
				while (command != endCommand && skipCommand(command))
					command = command.getNext();
				if (command == endCommand)
					break;
				commandForErrorMsg = command;
				start = command.getFirstToken();
			}

			AlignLine line = table.addLine();

			// keyword ALIASES, possibly with ":"
			if (start.isAnyKeyword(declarationKeywords)) {
				if (isChain)
					start = start.getNext(); // skip the chain colon : and start alignment with the identifier
				else
					line.setCell(Columns.KEYWORD_ALIASES.getValue(), new AlignCellToken(start));
				start = start.getNext();
			}

			// identifier 1
			Token identifier1 = start;
			line.setCell(Columns.IDENTIFIER1.getValue(), new AlignCellToken(identifier1));

			Token forKeyword = identifier1.getNext();
			line.setCell(Columns.KEYWORD_FOR.getValue(), new AlignCellToken(forKeyword));

			Token identifier2 = forKeyword.getNext();
			line.setCell(Columns.IDENTIFIER2.getValue(), new AlignCellToken(identifier2));

			start = identifier2.getNext();
			if (start == null || !start.isCommaOrPeriod())
				return;

			// comment at line end
			if (start.getNext() != null && start.getNext().isCommentAfterCode()) {
				start = start.getNext();
				if (line != null)
					line.setCell(Columns.LINE_END_COMMENT.getValue(), new AlignCellToken(start));
			}

			start = start.getNext();
			if (isChain) {
				if (start == null)
					break;
			} else {
				command = command.getNext();
				if (command == endCommand)
					break;
				if (command.containsInnerCommentLine())
					return;
				commandForErrorMsg = command;
				start = command.getFirstToken();
			}
		} while (true);
		if (table.getLineCount() > 0) {
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, false);
			code.addRuleUses(this, changedCommands);
		}
	}

}
