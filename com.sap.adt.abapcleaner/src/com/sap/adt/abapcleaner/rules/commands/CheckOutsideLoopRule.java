package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.NegationStyle;

public class CheckOutsideLoopRule extends CheckStatementRuleBase {
   private final static RuleReference[] references = new RuleReference[] {
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "CHECK vs. RETURN", "#check-vs-return"),
      new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid CHECK in other positions", "#avoid-check-in-other-positions"),
      new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "CHECK Statement Position", "check-statement-position.md")
   };

	@Override
	public RuleID getID() { return RuleID.CHECK_OUTSIDE_LOOP; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Convert CHECK outside loop to IF NOT ... RETURN"; }

	@Override
	public String getDescription() { return "Converts CHECK that is found outside of loops (LOOP, DO, WHILE) to IF NOT ... RETURN."; }

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
			+ LINE_SEP + "  METHOD convert_check_outside_loop." 
			+ LINE_SEP + "    \" CHECKs at earliest possible position" 
			+ LINE_SEP + "    CHECK its_table IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "    CHECK is_item_buffer-item_id     IS NOT INITIAL" 
			+ LINE_SEP + "      AND is_item_buffer-first_flag   = abap_false" 
			+ LINE_SEP + "      AND is_item_buffer-second_flag  = abap_false" 
			+ LINE_SEP + "      AND is_item_buffer-last_flag    = abap_true." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA: lv_any_value     TYPE i," 
			+ LINE_SEP + "          lv_another_value TYPE string." 
			+ LINE_SEP 
			+ LINE_SEP + "    FIELD-SYMBOLS <ls_struc> LIKE LINE OF its_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" CHECKs only preceded by declarations" 
			+ LINE_SEP + "    CHECK ( c IS NOT SUPPLIED OR b IS INITIAL ) AND ( d IS SUPPLIED OR b IS NOT INITIAL )." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ev_success." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" CHECKs only preceded by declarations and CLEAR" 
			+ LINE_SEP + "    CHECK a = abap_false AND b > 3 OR a = abap_true AND b <= 10." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = 1." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" CHECKs inside the method" 
			+ LINE_SEP + "    CHECK line_exists( its_table[ 0 ] ) " 
			+ LINE_SEP + "       OR lines( its_table ) > 2  AND line_exists( its_table[ 1 ] )." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD check_after_checkpoints." 
			+ LINE_SEP + "    \" various checkpoints" 
			+ LINE_SEP + "    BREAK-POINT." 
			+ LINE_SEP + "    LOG-POINT ID any_id." 
			+ LINE_SEP + "    ASSERT iv_value > 0." 
			+ LINE_SEP 
			+ LINE_SEP + "    CHECK its_table IS NOT INITIAL." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_any_value TYPE i." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ev_success." 
			+ LINE_SEP + "    CHECK its_table IS NOT INITIAL." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

   final ConfigEnumValue<KeepCheckOutsideLoopCondition> configKeepCondition = new ConfigEnumValue<KeepCheckOutsideLoopCondition>(this, "KeepCondition", "Keep CHECK statement:", new String[] { "never", "at method start", "after declarations", "after declarations and CLEAR statements" }, KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS);
	final ConfigBoolValue configAllowCheckAfterCheckpoints = new ConfigBoolValue(this, "AllowCheckAfterCheckpoints", "Allow CHECK after ASSERT, BREAK-POINT and LOG-POINT", true, false, LocalDate.of(2023, 10, 10));
   
	private final ConfigValue[] configValues = new ConfigValue[] { configKeepCondition, configNegationStyle, configConvertAbapFalseAndAbapTrue, configAllowCheckAfterCheckpoints };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CheckOutsideLoopRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		KeepCheckOutsideLoopCondition keepCondition = KeepCheckOutsideLoopCondition.forValue(configKeepCondition.getValue()); // the condition configured by the user
		KeepCheckOutsideLoopCondition convertUpTo = KeepCheckOutsideLoopCondition.NEVER; // tells which condition(s) would still be satisfied at the position of the current Command
		NegationStyle negationStyle = NegationStyle.forValue(configNegationStyle.getValue());
		boolean convertAbapFalseAndAbapTrue = configConvertAbapFalseAndAbapTrue.getValue();
		boolean allowCheckAfterCheckpoints = configAllowCheckAfterCheckpoints.getValue();
		boolean isInsideMethod = false; 
		
		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// update "convert up to"
			if (command.isMethodFunctionOrFormStart()) {
				isInsideMethod = true;
				convertUpTo = KeepCheckOutsideLoopCondition.NEVER;
			} else if (command.isMethodFunctionOrFormEnd()) {
				isInsideMethod = false;
				convertUpTo = KeepCheckOutsideLoopCondition.NEVER;
			} else if (command.isDeclaration() || command.isDeclarationInclude()) {
				convertUpTo = KeepCheckOutsideLoopCondition.KEEP_AT_METHOD_START;
			} else if (command.firstCodeTokenIsKeyword("CLEAR")) {
				convertUpTo = KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS;
			} else if (allowCheckAfterCheckpoints && command.firstCodeTokenIsAnyKeyword("ASSERT", "BREAK-POINT", "LOG-POINT")) {
				// keep convertUpTo unchanged
			} else if (!command.firstCodeTokenIsKeyword("CHECK") && !command.isCommentLine()) {
				convertUpTo = KeepCheckOutsideLoopCondition.KEEP_AFTER_DECLARATIONS_AND_CLEAR;
			}

			// only change the statement if we are SURE to be inside a method, because if only a code snippet from a LOOP body is processed, CHECK may be erroneously converted to IF ... RETURN! 
			if (isInsideMethod && !isCommandBlocked(command) && command.firstCodeTokenIsKeyword("CHECK") && !command.containsChainColon() && keepCondition.getValue() <= convertUpTo.getValue())
				executeOn(code, command, false, negationStyle, convertAbapFalseAndAbapTrue, releaseRestriction);

			if (command.getOpensLevel() && command.firstCodeTokenIsAnyKeyword(ABAP.loopKeywords))
				command = command.getNextSibling();
			else
				command = command.getNext();
		}
	}
}