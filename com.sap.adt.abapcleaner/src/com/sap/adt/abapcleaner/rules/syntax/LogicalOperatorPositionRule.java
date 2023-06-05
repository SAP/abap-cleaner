package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.alignment.AlignLogicalExpressionsRule;

public class LogicalOperatorPositionRule extends RuleForLogicalExpressions {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.LOGICAL_OPERATOR_POSITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Move AND/OR from line end to next line start"; }

	@Override
	public String getDescription() {
		return "Moves boolean operators (AND, OR, EQUIV) from the end of a line to the start of the next line." + System.lineSeparator() 
				+ "The expression is then aligned according to the settings of the rule '" + AlignLogicalExpressionsRule.DEFAULT_NAME + "' (even if the rule is otherwise deactivated).";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 21); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_DECLARATIONS } ; }
	
   @Override
   public String getExample() {
      return "" 
   			+ LINE_SEP + "  METHOD move_bool_ops_to_next_line." 
   			+ LINE_SEP + "    IF ls_item_data-category = if_any_interface=>co_any_category OR" 
   			+ LINE_SEP + "       ls_item_data-start_date IS NOT INITIAL OR" 
   			+ LINE_SEP + "       ls_item_data-end_date IS INITIAL OR" 
   			+ LINE_SEP + "       ls_item_data-processing_method = if_other_interface=>co_any_processing_method." 
   			+ LINE_SEP + "      \" do something" 
   			+ LINE_SEP + "    ENDIF." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    IF  ( c IS NOT SUPPLIED OR" 
   			+ LINE_SEP + "          b IS INITIAL ) AND" 
   			+ LINE_SEP + "        ( d IS SUPPLIED OR" 
   			+ LINE_SEP + "          b IS NOT INITIAL )." 
   			+ LINE_SEP + "      \" do something" 
   			+ LINE_SEP + "    ENDIF." 
   			+ LINE_SEP + "  ENDMETHOD.";
  }

	public LogicalOperatorPositionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token token = keyword.getNext();
		boolean changed = false;
		while (token != end) {
			if (token.isAnyKeyword("AND", "OR", "EQUIV") && !token.isFirstTokenInLine() && token.isLastTokenInLineExceptComment()) {
				Token nextNonComment = token.getNextNonCommentToken();
				if (nextNonComment != token.getNext()) {
					token.removeFromCommand();
					nextNonComment.insertLeftSibling(token, false, true);
				}
				token.setWhitespace(1, nextNonComment.getStartIndexInLine());
				nextNonComment.setWhitespace();
				changed = true;
			}
			token = token.getNext();
		}
		if (changed)
			((AlignLogicalExpressionsRule) parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, command, keyword, end, true, releaseRestriction);
		return changed;
	}
}