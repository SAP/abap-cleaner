package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForLogicalExpressions;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.LogicalExpression;
import com.sap.adt.abapcleaner.rules.alignment.AlignLogicalExpressionsRule;

public class NeedlessParenthesesRule extends RuleForLogicalExpressions {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "log_exp - Boolean Operators and Parentheses", "abenlogexp_boole.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "log_exp - Logical Expressions", "abenlogexp.htm"),
			new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.NEEDLESS_PARENTHESES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Remove needless parentheses"; }

	@Override
	public String getDescription() { return "Removes needless parentheses from logical expressions."; }

	@Override
	public String getHintsAndRestrictions() { return "If parentheses are removed, '" + AlignLogicalExpressionsRule.DEFAULT_NAME + "' is executed, even if it is otherwise deactivated."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 11, 2); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_LOGICAL_EXPRESSIONS, RuleID.INSET }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD remove_needless_parentheses."
			+ LINE_SEP + "    \" in ABAP, there is no need to put parentheses around entire logical expressions,"
			+ LINE_SEP + "    \" with the exception of table iterations with 'FOR ... IN ... WHERE ( log_expr )' ...:"
			+ LINE_SEP + "    CHECK ( sy-subrc <> 0 )."
			+ LINE_SEP + "    CHECK ( lt_any_table IS NOT INITIAL )."
			+ LINE_SEP + "    CHECK ( lt_any_table IS REQUESTED OR lt_other_table IS REQUESTED )."
			+ LINE_SEP
			+ LINE_SEP + "    WHILE ( lv_continue = abap_true )."
			+ LINE_SEP + "      IF ( prepare_next_item( ) < 0 )."
			+ LINE_SEP + "        lv_continue = abap_false."
			+ LINE_SEP + "      ELSEIF ( iv_any_value >= 0 AND iv_other_value <= 0 )."
			+ LINE_SEP + "        process_item( )."
			+ LINE_SEP + "      ENDIF."
			+ LINE_SEP + "    ENDWHILE."
			+ LINE_SEP
			+ LINE_SEP + "    \" relational expressions (i.e. comparisons and predicates) do not need parentheses around them, either,"
			+ LINE_SEP + "    \" because they are evaluated before the Boolean operators EQUIV / OR / AND join them, or NOT negates them:"
			+ LINE_SEP + "    CHECK     ( lv_any_value <> lv_other_value )"
			+ LINE_SEP + "          AND ( lv_any_value <> lv_third_value )."
			+ LINE_SEP
			+ LINE_SEP + "    lv_result = xsdbool( ( lv_any_value IS INITIAL ) AND ( lt_any_table IS NOT INITIAL ) )."
			+ LINE_SEP
			+ LINE_SEP + "    IF    ( iv_state = lc_invalid_state )"
			+ LINE_SEP + "       OR ( iv_state = lc_obsolete_state )."
			+ LINE_SEP + "      RETURN."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP
			+ LINE_SEP + "    \" from lowest to highest, operator precedence is: EQUIV < OR < AND < NOT < comparison / predicate;"
			+ LINE_SEP + "    \" therefore, no parentheses are needed in the frequent case of OR ( ... AND ... )."
			+ LINE_SEP + "    \" It usually helps readability to start a new line for every OR in such cases:"
			+ LINE_SEP + "    IF    lv_event_year < lv_final_year"
			+ LINE_SEP + "       OR (     lv_event_year  = lv_final_year"
			+ LINE_SEP + "            AND lv_event_month < lv_final_month )."
			+ LINE_SEP + "      process_past_event( ). \" the event happened before the final year / month"
			+ LINE_SEP
			+ LINE_SEP + "    ELSEIF    ( lv_any_amount < 0 AND lv_other_amount > 0 )"
			+ LINE_SEP + "           OR ( lv_any_amount > 0 AND lv_other_amount < 0 )."
			+ LINE_SEP + "      process_different_signs( )."
			+ LINE_SEP
			+ LINE_SEP + "    ELSEIF ( sy-subrc <> 0 OR ( sy-subrc = 0 AND lv_any_value IS INITIAL ) OR lv_stop = abap_true )."
			+ LINE_SEP + "      RETURN."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP
			+ LINE_SEP + "    LOOP AT lt_any_table INTO DATA(ls_struc)"
			+ LINE_SEP + "         WHERE    ( comp1 IS INITIAL     AND comp2 = 'A' )"
			+ LINE_SEP + "               OR ( comp1 IS NOT INITIAL AND comp2 = 'B' )."
			+ LINE_SEP + "    ENDLOOP."
			+ LINE_SEP
			+ LINE_SEP + "    \" parentheses must NEVER be removed from AND ( ... OR ... ), as that would completely change the logic:"
			+ LINE_SEP + "    IF iv_check_year_range = abap_true AND ( lv_year < iv_min_year OR lv_year > iv_max_year )."
			+ LINE_SEP + "      RETURN."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP
			+ LINE_SEP + "    \" if all is AND (or all is OR), parentheses can be removed as well, evaluation order stays the same;"
			+ LINE_SEP + "    \" however, the parentheses might serve readability by grouping similar things together:"
			+ LINE_SEP + "    IF    (    lt_table1  IS INITIAL"
			+ LINE_SEP + "            OR lt_table2  IS INITIAL )"
			+ LINE_SEP + "       OR (    lv_amount1 <= 0"
			+ LINE_SEP + "            OR lv_amount2 <= 0 )."
			+ LINE_SEP + "      RETURN."
			+ LINE_SEP
			+ LINE_SEP + "    ELSEIF     ( lv_year  >= iv_min_year  AND lv_year  <= iv_max_year )"
			+ LINE_SEP + "           AND ( iv_count >= iv_min_count AND iv_count <= iv_max_count )."
			+ LINE_SEP + "      process_match( )."
			+ LINE_SEP + "    ENDIF."
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configRemoveAroundAll = new ConfigBoolValue(this, "RemoveAroundAll", "Remove needless parentheses around the entire logical expression", true);
	final ConfigBoolValue configRemoveAroundRelExpr = new ConfigBoolValue(this, "RemoveAroundRelExpr", "Remove needless parentheses around relational expressions (i.e. comparisons or predicates)", false);
	final ConfigBoolValue configRemoveOrParenthesisAnd = new ConfigBoolValue(this, "RemoveOrParenthesisAnd", "Remove needless parentheses from OR  ( ... AND ... )", false);
	final ConfigBoolValue configRemoveAroundSameOp = new ConfigBoolValue(this, "RemoveAroundSameOp", "Remove needless parentheses from AND ( ... AND ... ) /  OR  ( ... OR ... )", false);
	// final ConfigBoolValue configRemoveAroundNot = new ConfigBoolValue(this, "RemoveAroundNot", "Remove needless parentheses for AND ( NOT ... ) /  OR  ( NOT ... )", false);
	final ConfigInfoValue configWarning = new ConfigInfoValue(this, "CAUTION: While the logic remains unchanged, readability may suffer in some cases.", ConfigInfoStyle.WARNING);

	private final ConfigValue[] configValues = new ConfigValue[] { configRemoveAroundAll, configRemoveAroundRelExpr, configRemoveOrParenthesisAnd, configRemoveAroundSameOp, configWarning };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) { 
		if (configValue == configWarning) {
			return configRemoveOrParenthesisAnd.getValue() || configRemoveAroundSameOp.getValue(); // || configRemoveAroundNot.getValue()
		} else {
			return true;
		}
	}

	public NeedlessParenthesesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// in table iterations FOR ... IN ... WHERE ( ... ), the surrounding parentheses are mandatory (while they aren't in case of FOR ... UNTIL|WHILE ...)
		boolean canRemoveAroundAll = !(keyword.isKeyword("WHERE") && keyword.getParent() != null);
		
		boolean changed = false;
		try {
			Token logExpStart = keyword.getNextCodeToken();
			LogicalExpression logicalExpression = LogicalExpression.create(logExpStart, end.getPrev());
			if (!logicalExpression.isSupported()) 
				return false;

			boolean removeAroundAll = canRemoveAroundAll && configRemoveAroundAll.getValue();
			boolean removeAroundRelExpr = configRemoveAroundRelExpr.getValue();
			boolean removeOrParenthesisAnd = configRemoveOrParenthesisAnd.getValue();
			boolean removeAroundSameOp = configRemoveAroundSameOp.getValue();
			boolean removeAroundNot = false; // configRemoveAroundNot.getValue();
			changed = logicalExpression.removeAllNeedlessParentheses(removeAroundAll, removeAroundRelExpr, removeOrParenthesisAnd, removeAroundSameOp, removeAroundNot);

		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxAfterChanges(this, ex);
		}
		if (changed) {
			Token lastInLogExpr = keyword.getLastTokenOfLogicalExpression();
			if (lastInLogExpr != null) {
				Token newEnd = lastInLogExpr.getNextCodeToken();
				((AlignLogicalExpressionsRule)parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, command, keyword, newEnd, false, releaseRestriction);
			}
		}
		return changed;
	}
}
