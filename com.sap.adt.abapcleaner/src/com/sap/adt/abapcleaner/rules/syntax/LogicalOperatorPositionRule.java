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
	public String getDisplayName() { return "Move AND/OR etc. from line end to next line start"; }

	@Override
	public String getDescription() { return "Moves Boolean operators (AND, OR, EQUIV) and keywords starting the logical expression (WHERE, UNTIL, WHILE) from the end of a line to the start of the next line."; }

	@Override
	public String getHintsAndRestrictions() { return "After moving Boolean operators, the expression is aligned according to the settings of the rule '" + AlignLogicalExpressionsRule.DEFAULT_NAME + "' (even if the rule is otherwise deactivated)."; }

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
   			+ LINE_SEP 
   			+ LINE_SEP + "    ELSEIF  ( c IS NOT SUPPLIED OR" 
   			+ LINE_SEP + "              b IS INITIAL ) AND" 
   			+ LINE_SEP + "            ( d IS SUPPLIED OR" 
   			+ LINE_SEP + "              b IS NOT INITIAL )." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    ENDIF." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    \" with WHERE at line end, 'component = 1.' looks like an assignment"
   			+ LINE_SEP + "    LOOP AT lts_any_table ASSIGNING FIELD-SYMBOL(<ls_any>) WHERE" 
   			+ LINE_SEP + "      component = 1." 
   			+ LINE_SEP + "      <ls_any>-component = 2." 
   			+ LINE_SEP + "    ENDLOOP." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    \" proper alignment helps to understand the logic and to check whether it is correct" 
   			+ LINE_SEP + "    DELETE lts_other_table WHERE" 
   			+ LINE_SEP + "           status = 'a' OR" 
   			+ LINE_SEP + "           status = 'b' AND" 
   			+ LINE_SEP + "           obsolete = abap_true." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    \" if WHERE is at line end, the logical expression looks like a table line" 
   			+ LINE_SEP + "    rt_result = VALUE #( FOR ls_struc IN lt_table WHERE" 
   			+ LINE_SEP + "                         ( status = 'a' )" 
   			+ LINE_SEP + "                         ( ls_struc ) )." 
   			+ LINE_SEP 
   			+ LINE_SEP + "    lt_other_result = FILTER #( lt_any_table USING KEY any_key WHERE" 
   			+ LINE_SEP + "                                active = abap_true AND used = abap_true )."
   			+ LINE_SEP 
   			+ LINE_SEP + "    lv_sum = REDUCE i( INIT s = 0" 
   			+ LINE_SEP + "                       FOR i = 1 UNTIL"
   			+ LINE_SEP + "                       i = 10 OR"
   			+ LINE_SEP + "                       i >= iv_max"
   			+ LINE_SEP + "                       NEXT s += i )."
   			+ LINE_SEP + "  ENDMETHOD.";
  }

	ConfigBoolValue configMoveKeyword = new ConfigBoolValue(this, "MoveKeyword", "Move keywords (WHERE, UNTIL, WHILE)", true, false, LocalDate.of(2023, 6, 9));
	ConfigBoolValue configMoveBooleanOperators = new ConfigBoolValue(this, "MoveBooleanOperators", "Move Boolean operators (AND, OR, EQUIV)", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configMoveKeyword, configMoveBooleanOperators };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public LogicalOperatorPositionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		boolean moveKeyword = configMoveKeyword.getValue();
		boolean moveBoolOps = configMoveBooleanOperators.getValue();
				
		if (!moveKeyword && !moveBoolOps)
			return false;

		boolean changed = false;

		// if the keyword is at line end, move it in front of the logical expression
		Token logExpStart = keyword.getNextCodeToken();
		if (moveKeyword && keyword != command.getFirstToken() && keyword.lineBreaks == 0 && logExpStart.lineBreaks > 0) {
			// determine the minimum start index of the logical expression; note that this is not necessarily the start index
			// of the first line in a case like "WHERE
			//      status = 1
			//   OR status = 2"
			int minLineStartInLogExp = logExpStart.getMinIndexInLine(end);
			int relativeIndentAtLogExpStart = logExpStart.getStartIndexInLine() - minLineStartInLogExp;
			
			// determine how much to move the logical expression to the right, also considering a minimum expected line start
			int moveForKeyword = keyword.getTextLength() + 1;
			Token firstToken = command.getFirstToken();
			int minExpLineStart = firstToken.spacesLeft + 2;
			if (firstToken.isKeyword()) {
				// alternative: '..., firstToken.spacesLeft + firstToken.getTextLength() + 1', but that would be doing
				// the job of the AlignWithSecondWordRule, which will be executed after this rule (if activated)
				minExpLineStart = Math.max(minExpLineStart, firstToken.spacesLeft); 
			}
			int addIndent = Math.max(minExpLineStart - minLineStartInLogExp, 0) + moveForKeyword;
			
			// move the logical expression to the right and put the keyword in front of it
			command.addIndent(addIndent, minLineStartInLogExp, logExpStart, end, true);
			keyword.setWhitespace(logExpStart.lineBreaks, Math.max(minLineStartInLogExp, minExpLineStart));
			if (keyword.getNext() != logExpStart) { // keyword is followed by a line-end comment
				keyword.removeFromCommand(false);
				logExpStart.insertLeftSibling(keyword, false);
			}
			logExpStart.setWhitespace(0, 1 + relativeIndentAtLogExpStart);
			changed = true;
		}

		// move AND, OR, EQUIV from line end to next line start
		if (moveBoolOps) {
			Token token = keyword.getNext();
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
		}
		
		// align the logical expression, even if the rule is otherwise deactivated
		if (changed && moveBoolOps)
			((AlignLogicalExpressionsRule) parentProfile.getRule(RuleID.ALIGN_LOGICAL_EXPRESSIONS)).alignLogicalExpression(code, command, keyword, end, true, releaseRestriction);

		return changed;
	}
}