package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

public class IfBlockAtLoopEndRule extends IfBlockRuleBase {
	protected final static String[] loopEndKeywords = new String[] { "ENDLOOP", "ENDDO", "ENDWHILE" };

	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Keep the nesting depth low", "#keep-the-nesting-depth-low"),
			new RuleReference(RuleSource.ABAP_CLEANER, "Replace long IF blocks at loop end", "https://en.wikipedia.org/wiki/Structured_programming#Early_exit") };

	@Override
	public RuleID getID() { return RuleID.IF_BLOCK_AT_LOOP_END; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace long IF blocks at loop end"; }

	@Override
	public String getDescription() {
		return "Replaces long IF blocks at loop end with IF NOT ... CONTINUE to decrease nesting depth, prefering early exit to overdoing structured programming.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 18); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_if_block_at_loop_end." 
			+ LINE_SEP + "    DATA lv_count TYPE i." 
			+ LINE_SEP + "    DATA lv_value TYPE i." 
			+ LINE_SEP + "    " 
			+ LINE_SEP + "    WHILE lv_count > 0." 
			+ LINE_SEP + "      lv_count -= 1." 
			+ LINE_SEP + "      IF lv_count MOD 2 = 0." 
			+ LINE_SEP + "        LOOP AT its_table INTO DATA(ls_row)." 
			+ LINE_SEP + "          \" if the row was already processed, there is nothing to do. " 
			+ LINE_SEP + "          IF ls_row-processed = abap_false." 
			+ LINE_SEP + "            IF ls_row-another_component = abap_true." 
			+ LINE_SEP + "              \" if the row was not yet processed, there's so much to do!" 
			+ LINE_SEP + "              lv_value = 1." 
			+ LINE_SEP + "              lv_value = 2." 
			+ LINE_SEP + "              lv_value = 3." 
			+ LINE_SEP + "              lv_value = 4." 
			+ LINE_SEP + "              lv_value = 5." 
			+ LINE_SEP + "              lv_value = 6." 
			+ LINE_SEP + "              lv_value = 7." 
			+ LINE_SEP + "              lv_value = 8." 
			+ LINE_SEP + "              lv_value = 9." 
			+ LINE_SEP + "              ls_row-processed = abap_true." 
			+ LINE_SEP + "            ENDIF." 
			+ LINE_SEP + "          ENDIF." 
			+ LINE_SEP + "        ENDLOOP." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDWHILE." 
			+ LINE_SEP + "  ENDMETHOD." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD replace_if_block_at_loop_end_2." 
			+ LINE_SEP + "    DATA lv_count TYPE i." 
			+ LINE_SEP + "    DATA lv_value TYPE i." 
			+ LINE_SEP + "    " 
			+ LINE_SEP + "    WHILE lv_count > 0." 
			+ LINE_SEP + "      lv_count -= 1." 
			+ LINE_SEP + "      IF lv_count MOD 2 = 0." 
			+ LINE_SEP + "        LOOP AT its_table INTO DATA(ls_row)." 
			+ LINE_SEP + "          \" if the row was already processed, there is nothing to do. " 
			+ LINE_SEP + "          IF ls_row-processed = abap_false." 
			+ LINE_SEP + "            \" if the row was not yet processed, there's so much to do!" 
			+ LINE_SEP + "            lv_value = 1." 
			+ LINE_SEP + "            lv_value = 2." 
			+ LINE_SEP + "            lv_value = 3." 
			+ LINE_SEP + "            ls_row-processed = abap_true." 
			+ LINE_SEP + "          ENDIF." 
			+ LINE_SEP + "        ENDLOOP." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDWHILE." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private final ConfigValue[] configValues = new ConfigValue[] { configMinLineCount, configMinLinePercentage, configKeepExceptionLogicInIf, configNegationStyle, configConvertAbapFalseAndAbapTrue, configEnsureEmptyLineAfterEndIf };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public IfBlockAtLoopEndRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		int minLineCount = configMinLineCount.getValue();
		double minLineRatio = configMinLinePercentage.getValue() / 100.0;
		boolean keepExceptionalLogicInIf = configKeepExceptionLogicInIf.getValue();
		NegationStyle negationStyle = NegationStyle.forValue(configNegationStyle.getValue());
		boolean convertAbapFalseAndAbapTrue = configConvertAbapFalseAndAbapTrue.getValue();
		boolean ensureEmptyLineAfterEndIf = configEnsureEmptyLineAfterEndIf.getValue();

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// starting from an ENDLOOP, ENDDO, or ENDWHILE command, move to the possible IF ... ENDIF at the end of the loop body
			if (command.firstCodeTokenIsAnyKeyword(loopEndKeywords)) {
				do {
					Command endifCommand = command.getPrevNonCommentCommand();
					if (!endifCommand.isEndIf())
						break;

					Command ifCommand = endifCommand.getPrevSibling();
					if (!ifCommand.firstCodeTokenIsKeyword("IF") || !ifCommand.hasChildren() || isCommandBlocked(ifCommand)) 
						break; // the command could, of course, also be an "ELSE" or "ELSEIF" in which case it cannot be processed

					// only transform the Command in case ...
					// 1. the IF block is long enough 
					int lineCountInIf = ifCommand.getNext().getLineBreakSumUpTo(endifCommand);
					if (lineCountInIf < minLineCount)
						break;

					// 2. the IF block covers a certain minimum percentage of all lines in this LOOP
					int lineCountInLoop = command.getPrevSibling().getNext().getLineBreakSumUpTo(command); 
					if (lineCountInIf < minLineRatio * lineCountInLoop)
						break;
					
					// 3. the IF block does NOT contain exceptional logic (RAISE, MESSAGE, RETURN, EXIT) 
					if (keepExceptionalLogicInIf && blockContainsExceptionalLogic(ifCommand))
						break;
					
					// transform to IF NOT ... EXIT  
					if (!executeOn(code, ifCommand, true, negationStyle, convertAbapFalseAndAbapTrue, ensureEmptyLineAfterEndIf, releaseRestriction))
						break;

					// if the IF block was replaced, continue recursively, as there may be several nested IF ... ENDIF blocks at loop end
				} while (true);
			}

			command = command.getNext();
		}
	}
}