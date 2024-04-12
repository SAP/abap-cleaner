package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class ComparisonOperatorRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Use consistent spelling", "abenlogexp_any_operand.htm"),
			new RuleReference(RuleSource.ABAP_CLEANER) };

	private final static String[] textualComparisonOps = new String[] { "LT", "LE", "EQ", "NE", "GE", "GT" };
	private final static String[] symbolicComparisonOps = new String[] { "<", "<=", "=", "<>", ">=", ">" };
	private final static String[] obsoleteComparisonOps = new String[] { "><", "=<", "=>" };

	@Override
	public RuleID getID() { return RuleID.COMPARISON_OPERATOR; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Use consistent set of comparison operators"; }

	@Override
	public String getDescription() { return "Replaces textual comparison operators (LT, LE, EQ, NE, GE, GT) with symbolic comparison operators (<, <=, =, <>, >=, >) or vice versa."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 17); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_LOGICAL_EXPRESSIONS } ; }

	@Override
   public String getExample() {
      return "" 
      	+ LINE_SEP + "CLASS any_class IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD use_consistent_comparison_ops." 
			+ LINE_SEP + "    IF a EQ b OR c NE d." 
			+ LINE_SEP + "      IF a < c AND b > d" 
			+ LINE_SEP + "               AND b < e." 
			+ LINE_SEP + "        IF a LE d AND c GE b." 
			+ LINE_SEP + "          result = xsdbool( a <= d OR a GE b )." 
			+ LINE_SEP + "        ENDIF." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP + "  ENDMETHOD."
   		+ LINE_SEP + "ENDCLASS."
			+ LINE_SEP 
			+ LINE_SEP + "FORM any_form."
			+ LINE_SEP + "  \" these obsolete variants are only possible outside of the object-oriented context:"
			+ LINE_SEP + "  IF a >< b AND b => c OR c =< d."
			+ LINE_SEP + "    RETURN."
			+ LINE_SEP + "  ENDIF."
			+ LINE_SEP + "ENDFORM."; 
   }

	final ConfigEnumValue<ComparisonOperatorType> configPreferredOperatorSet = new ConfigEnumValue<ComparisonOperatorType>(this, "PreferredOperatorSet", "Preferred set of comparison operators:", new String[] { "symbolic (<, <=, =, <>, >=, >)", "textual (LT, LE, EQ, NE, GE, GT)" }, ComparisonOperatorType.values(), ComparisonOperatorType.SYMBOLIC, ComparisonOperatorType.SYMBOLIC, LocalDate.of(2024, 4, 12));
	final ConfigBoolValue configReplaceRegularOperators = new ConfigBoolValue(this, "ReplaceRegularOperators", "Replace regular comparison operators with preferred variant", true);
	final ConfigBoolValue configReplaceObsoleteOperators = new ConfigBoolValue(this, "ReplaceObsoleteOperators", "Replace obsolete comparison operators (><  =>  =<) with preferred variant", true, false, LocalDate.of(2024, 4, 12));

	private final ConfigValue[] configValues = new ConfigValue[] { configPreferredOperatorSet, configReplaceRegularOperators, configReplaceObsoleteOperators };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ComparisonOperatorRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean skipCommand(Command command) { 
		// ABAP Reference "SUBMIT - Short reference", https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapsubmit_shortref.htm does not seem to include symbolic operators!
		return command.firstCodeTokenIsKeyword("SUBMIT");
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) {
		if (!token.isComparisonOperator())
			return false;
		
		// do NOT change 'opt' in: 'SELECT-OPTIONS selcrit FOR dobj DEFAULT val1 [TO val2] [OPTION opt] ....'
		if (command.firstCodeTokenIsKeyword("SELECT-OPTIONS")) {
			Token prev = token.getPrevCodeSibling();
			if (prev != null && prev.isKeyword("OPTION")) {
				return false;
			}
		}

		ComparisonOperatorType targetType = ComparisonOperatorType.forValue(configPreferredOperatorSet.getValue());
		String newOperator;
		if (configReplaceObsoleteOperators.getValue() && token.isAnyComparisonOperator(obsoleteComparisonOps) && !command.isInOOContext()) {
			newOperator = ABAP.getNonObsoleteComparisonOperator(token.getText());
			if (targetType == ComparisonOperatorType.TEXTUAL) {
				newOperator = ABAP.getTextualComparisonOperator(newOperator);
			}
			
		} else if (targetType == ComparisonOperatorType.SYMBOLIC && configReplaceRegularOperators.getValue() && token.isAnyComparisonOperator(textualComparisonOps)) {
			newOperator = ABAP.getSymbolicComparisonOperator(token.getText());
		
		} else if (targetType == ComparisonOperatorType.TEXTUAL && configReplaceRegularOperators.getValue() && token.isAnyComparisonOperator(symbolicComparisonOps)) {
			newOperator = ABAP.getTextualComparisonOperator(token.getText());
		
		} else {
			return false;
		}
		
		token.setText(newOperator, true);
		return true;
	}
}