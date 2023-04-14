package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;

public class ClosingBracketsPositionRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Close brackets at line end", "#close-brackets-at-line-end") };

	@Override
	public RuleID getID() { return RuleID.CLOSING_BRACKETS_POSITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Close brackets at line end"; }

	@Override
	public String getDescription() { return "Closes brackets at line end by moving one or many ) ] . to the previous line."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS }; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD close_brackets_at_line_end." 
			+ LINE_SEP + "    ev_result = VALUE #( ( a = 1" 
			+ LINE_SEP + "                           b = 2" 
			+ LINE_SEP + "                         )" 
			+ LINE_SEP + "                         ( a = 2" 
			+ LINE_SEP + "                           b = 4 \" comment" 
			+ LINE_SEP + "                         )" 
			+ LINE_SEP + "                       )." 
			+ LINE_SEP  
			+ LINE_SEP + "    any_method( iv_name   = 'ABC'" 
			+ LINE_SEP + "                iv_amount = get_random( iv_min = lts_min[ id  = lv_item_id" 
			+ LINE_SEP + "                                                          var = lv_var" 
			+ LINE_SEP + "                                                        ]" 
			+ LINE_SEP + "                                        iv_max = 100" 
			+ LINE_SEP + "                                      )" 
			+ LINE_SEP + "              )." 
			+ LINE_SEP + "  ENDMETHOD.";
	}

	public ClosingBracketsPositionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	public final boolean executeOn(Code code, Command command, boolean addRuleUse, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (executeOn(code, command, releaseRestriction)) {
			if (addRuleUse)
				code.addRuleUse(this, command);
			return true;
		} else {
			return false;
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		boolean movePeriod = false;
		boolean changed = false;

		Token token = command.getFirstToken();
		while (token != null) {
			boolean textMatch = (token.closesLevel() && token.textEqualsAny(")", "]")) || (movePeriod && token.textEquals("."));
			if (token.lineBreaks == 0 || !textMatch || token.getPrev() == null || token.getPrev().isCommentLine()) {
				token = token.getNext();
				continue;
			}

			Token lineEndToken = token;
			while (lineEndToken.getNext() != null && lineEndToken.getNext().lineBreaks == 0) {
				lineEndToken = lineEndToken.getNext();
			}

			if (token.getPrev().isCommentAfterCode() && lineEndToken.isCommentAfterCode()
					&& (token.getPrev().isPseudoComment() || lineEndToken.isPseudoComment())) {
				// merging comments is NOT possible if the second one is a pseudo-comment "#EC ...;
				// if the first one is, we could merge with a space as a separator (see below), but we rather don't do this
				token = token.getNext();
				continue;
			}
			
			// move the next token to the bracket's current position; however, move the final line-end comment up with the period // TODO: possibly depending on line length?
			Token next = token.getNext();
			if (next != null && next.lineBreaks == 0) {
				if (token.isPeriod() && next.isCommentAfterCode()) // merging of comments could be prevented here with "... && !prev.isComment()"  
					next.setWhitespace();
				else
					next.copyWhitespaceFrom(token);
			}
			// if this bracket is followed by a period (even on the next line), move this period behind the bracket, too (in the next step)
			if (next != null && next.isPeriod())
				movePeriod = true;

			// move the bracket or period to the previous line
			token.lineBreaks = 0;
			token.spacesLeft = token.isPeriod() ? 0 : 1;
			
			// move previous line-end comment behind the bracket or period
			if (token.getPrev().isCommentAfterCode()) {
				// the line-end comment changes its level (and therefore its siblings) when a bracket is moved up,
				// therefore remove and insert the comment, NOT the bracket (otherwise, referential integrity is broken)
				Token prev = token.getPrev();
				boolean prevIsPseudoComment = prev.isPseudoComment();
				prev.spacesLeft = Math.max(prev.spacesLeft - token.spacesLeft - token.getTextLength(), 1);
				prev.removeFromCommand();
				if (lineEndToken.isComment()) {
					// merge comments
					String commentWithoutSign = StringUtil.trimStart(lineEndToken.getText().substring(ABAP.COMMENT_SIGN_STRING.length()));
					String separator = prevIsPseudoComment ? " " : "; ";
					lineEndToken.setText(StringUtil.trimEnd(prev.getText()) + separator + commentWithoutSign, false);
				} else {
					lineEndToken.insertRightSibling(prev);
				}
			}
			// pragmas must be moved behind closing brackets (but stay in front of the period), e.g. "IMPORTING ev_param2 = lv_param2 ) ##NEEDED."
			if (!token.isPeriod()) {
				while (!token.isFirstTokenInLine() && token.getPrev().isPragma()) {
					// the pragma changes its level (and therefore its siblings) when a bracket is moved up,
					// therefore remove and insert the pragma, NOT the bracket 
					Token pragma = token.getPrev();
					if (pragma.isFirstTokenInLine()) {
						token.copyWhitespaceFrom(pragma);
						pragma.setWhitespace();
					} else {
						pragma.spacesLeft = Math.max(pragma.spacesLeft - token.spacesLeft - token.getTextLength(), 1);
					}
					pragma.removeFromCommand();
					token.insertRightSibling(pragma);
				}
			}
			changed = true;
			token = token.getNext();
		}
		return changed;
	}
}
