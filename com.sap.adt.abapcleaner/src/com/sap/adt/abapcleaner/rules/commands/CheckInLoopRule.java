package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

public class CheckInLoopRule extends CheckStatementRuleBase {
   private final static RuleReference[] references = new RuleReference[] {
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid CHECK in other positions", "#avoid-check-in-other-positions"),
      new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "CHECK in LOOP", "check-in-loop.md")
   };

	@Override
	public RuleID getID() { return RuleID.CHECK_IN_LOOP; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Convert CHECK in loop to IF NOT ... CONTINUE"; }

	@Override
	public String getDescription() { return "Converts CHECK inside a loop to IF NOT ... CONTINUE (this applies to LOOP, DO and WHILE)"; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 17); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD convert_check_in_loop." 
			+ LINE_SEP + "    \" outside the scope of this rule:" 
			+ LINE_SEP + "    CHECK its_input_table IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "    DO 5 TIMES." 
			+ LINE_SEP + "      CHECK its_table IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "      LOOP AT its_table INTO DATA(ls_row)." 
			+ LINE_SEP + "        \" the following CHECKs are considered to be at loop start (despite this comment)" 
			+ LINE_SEP + "        CHECK ls_row-min_id <> 0." 
			+ LINE_SEP + "        CHECK ls_row-processed = abap_false." 
			+ LINE_SEP 
			+ LINE_SEP + "        \" chains can only be processed if they are first unchained" 
			+ LINE_SEP + "        CHECK: ls_row-max_id > 0," 
			+ LINE_SEP + "               ls_row-flag IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "        WHILE lv_id < ls_row-max_id." 
			+ LINE_SEP + "          lv_id += 1." 
			+ LINE_SEP 
			+ LINE_SEP + "          \" these CHECKs are obviously NOT at loop start: " 
			+ LINE_SEP + "          CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL )." 
			+ LINE_SEP 
			+ LINE_SEP + "          CHECK line_exists( its_table[ 0 ] ) AND its_table[ 0 ]-processed = abap_true" 
			+ LINE_SEP + "             OR lines( its_table ) > 2  AND line_exists( its_table[ 1 ] )." 
			+ LINE_SEP 
			+ LINE_SEP + "          \" do something very important" 
			+ LINE_SEP + "        ENDWHILE." 
			+ LINE_SEP 
			+ LINE_SEP + "      ENDLOOP." 
			+ LINE_SEP 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

   final ConfigEnumValue<KeepCheckInLoopCondition> configKeepCondition = new ConfigEnumValue<KeepCheckInLoopCondition>(this, "KeepCondition", "Keep CHECK statement in LOOP:", new String[] { "never", "at loop start" }, KeepCheckInLoopCondition.values(), KeepCheckInLoopCondition.NEVER);
	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain CHECK: chains in loops (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));
   
	private final ConfigValue[] configValues = new ConfigValue[] { configKeepCondition, configNegationStyle, configConvertAbapFalseAndAbapTrue, configProcessChains };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CheckInLoopRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		KeepCheckInLoopCondition keepCondition = KeepCheckInLoopCondition.forValue(configKeepCondition.getValue()); // the condition configured by the user
		KeepCheckInLoopCondition convertUpTo = KeepCheckInLoopCondition.NEVER; // tells which condition(s) would still be satisfied at the position of the current Command
		NegationStyle negationStyle = NegationStyle.forValue(configNegationStyle.getValue());
		boolean convertAbapFalseAndAbapTrue = configConvertAbapFalseAndAbapTrue.getValue();
		boolean processChains = configProcessChains.getValue();

		Command command = code.firstCommand;
		int loopLevel = 0;

		while (command != null) {
			commandForErrorMsg = command;

			// update "convert up to"
			Token firstToken = command.getFirstToken(); 
			if (command.getOpensLevel() && command.firstCodeTokenIsAnyKeyword(ABAP.loopKeywords)) {
				++loopLevel;
				convertUpTo = KeepCheckInLoopCondition.NEVER;
			} else if (command.getClosesLevel() && command.firstCodeTokenIsAnyKeyword(ABAP.loopEndKeywords)) {
				--loopLevel;
				convertUpTo = KeepCheckInLoopCondition.KEEP_AT_LOOP_START; // with respect to the outer loop (if any), we are no more at loop start (as we just ended an inner loop)
			} else if (!firstToken.isKeyword("CHECK") && !firstToken.isComment()) {
				convertUpTo = KeepCheckInLoopCondition.KEEP_AT_LOOP_START;
			}

			if (loopLevel > 0 && !isCommandBlocked(command) && firstToken.isKeyword("CHECK")
					&& keepCondition.getValue() <= convertUpTo.getValue()) {
				executeOn(code, command, true, processChains, negationStyle, convertAbapFalseAndAbapTrue, releaseRestriction);
			}
			
			command = command.getNext();
		}
	}
}
