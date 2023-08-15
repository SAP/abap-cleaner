package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;
import com.sap.adt.abapcleaner.rules.alignment.AlignLogicalExpressionsRule;

public class NotIsRule extends RuleForLogicalExpressions {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer IS NOT to NOT IS", "#prefer-is-not-to-not-is"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Prefer IS NOT to NOT IS", "prefer-is-not-to-not-is.md") };

	@Override
	public RuleID getID() { return RuleID.NOT_IS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Prefer IS NOT to NOT IS"; }

	@Override
	public String getDescription() { return "Transforms logical expressions to use IS NOT instead of NOT IS, if possible."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 6); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD prefer_is_not_to_not_is." 
			+ LINE_SEP + "    \" simple cases" 
			+ LINE_SEP + "    IF NOT iv_param IS SUPPLIED." 
			+ LINE_SEP + "      IF NOT <ls_data> IS ASSIGNED." 
			+ LINE_SEP + "        IF NOT lo_object IS BOUND." 
			+ LINE_SEP + "          IF NOT lo_object IS INSTANCE OF cl_any_class." 
			+ LINE_SEP + "            IF NOT lts_table[ a = 1" 
			+ LINE_SEP + "                              b = 2" 
			+ LINE_SEP + "                              c = 3 ]-field IS INITIAL." 
			+ LINE_SEP + "              \" do nothing" 
			+ LINE_SEP + "            ENDIF." 
			+ LINE_SEP + "          ENDIF." 
			+ LINE_SEP + "        ENDIF." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" complex cases" 
			+ LINE_SEP + "    IF NOT iv_param IS SUPPLIED" 
			+ LINE_SEP + "    OR NOT iv_other_param IS SUPPLIED." 
			+ LINE_SEP 
			+ LINE_SEP + "      IF NOT <ls_data> IS ASSIGNED" 
			+ LINE_SEP + "      OR ( NOT lo_object IS BOUND" 
			+ LINE_SEP + "           AND NOT lo_object IS INSTANCE OF cl_any_class )." 
			+ LINE_SEP + "        \" do nothing" 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public NotIsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		LogicalExpression logicalExpression;
		try {
			logicalExpression = LogicalExpression.create(keyword.getNextNonCommentToken(), end.getPrev());
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
			return false;
		}
		if (logicalExpression.isSupported() && logicalExpression.transformIsNotToNotIs()) {
			// execute AlignLogicalExpressionsRule on the logical expression (unless the Rule is blocked for this Command); keyword and end should still be valid
			((AlignLogicalExpressionsRule) parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, command, keyword, end, true, releaseRestriction);
			return true;
		} else {
			return false;
		}
	}
}
