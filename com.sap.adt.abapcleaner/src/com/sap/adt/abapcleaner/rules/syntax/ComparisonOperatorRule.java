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

	@Override
	public RuleID getID() { return RuleID.COMPARISON_OPERATOR; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Prefer =, <>, <= etc. to EQ, NE, LE etc."; }

	@Override
	public String getDescription() { return "Replaces keywords (LT, LE, EQ, NE, GT, GE) with symbolic comparison operators (<, <=, =, >=, >, <>)."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 17); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_LOGICAL_EXPRESSIONS } ; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD prefer_symbolic_comparison_ops." 
			+ LINE_SEP + "    IF a EQ b OR c NE d." 
			+ LINE_SEP + "      IF a LT c AND b GT d" 
			+ LINE_SEP + "                AND b LT e." 
			+ LINE_SEP + "        IF a LE d AND c GE b." 
			+ LINE_SEP + "          \" do something" 
			+ LINE_SEP + "        ENDIF." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

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
		if (!token.isComparisonOperator() || !token.isAnyComparisonOperator(textualComparisonOps))
			return false;
		String newOperator = ABAP.getSymbolicComparisonOperator(token.getText());
		token.setText(newOperator, true);
		return true;
	}
}