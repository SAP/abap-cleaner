package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * Ensures that empty parentheses ( ) contain a single space only
 *
 */
public class SpacesInEmptyBracketsRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Condense your code", "#condense-your-code") };

	@Override
	public RuleID getID() { return RuleID.SPACES_IN_EMPTY_BRACKETS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Remove multiple spaces in empty parentheses"; }

	@Override
	public String getDescription() { return "Removes multiple spaces from empty parentheses (e.g. in method call chains)."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD single_space_in_empty_parens." 
			+ LINE_SEP + "    ev_result = class_name(  )=>get_tool(  )->get_value(      )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public SpacesInEmptyBracketsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) {
		if (token.getOpensLevel() && !token.isLiteral() && !token.hasChildren() && token.getNext().closesLevel() && token.getNext().lineBreaks == 0 && token.getNext().spacesLeft > 1) {
			token.getNext().spacesLeft = 1;
			return true;
		} else {
			return false;
		}
	}
}
