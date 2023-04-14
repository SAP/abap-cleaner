package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.spaces.ClosingBracketsPositionRule;

public class ReceivingKeywordRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Omit RECEIVING", "#omit-receiving"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "RECEIVING Statement Usage", "receiving-usage.md") };

	@Override
	public RuleID getID() { return RuleID.RECEIVING_KEYWORD; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Omit RECEIVING"; }

	@Override
	public String getDescription() { return "Transforms method calls that use the RECEIVING keyword into functional style."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 7); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.EXPORTING_KEYWORD, RuleID.ALIGN_PARAMETERS } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD omit_receiving." 
			+ LINE_SEP + "    any_method(" 
			+ LINE_SEP + "      EXPORTING iv_param  = lv_param" 
			+ LINE_SEP + "      IMPORTING ev_param  = ev_param" 
			+ LINE_SEP + "      CHANGING  cv_param  = cv_param" 
			+ LINE_SEP + "      RECEIVING rv_result = lv_result )." 
			+ LINE_SEP 
			+ LINE_SEP + "    cl_any_class=>get( )->any_method(" 
			+ LINE_SEP + "      IMPORTING" 
			+ LINE_SEP + "        ev_param  = ev_param" 
			+ LINE_SEP + "      CHANGING" 
			+ LINE_SEP + "        cv_param  = cv_param" 
			+ LINE_SEP + "      RECEIVING" 
			+ LINE_SEP + "        rv_result = DATA(lv_result2) )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private ClosingBracketsPositionRule closeBracketsAtLineEndRule = null;

	public ReceivingKeywordRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (!token.isKeyword() || token.getParent() == null || !token.matchesOnSiblings(false, "RECEIVING", TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_TERM))
			return false;

		// ensure this is NOT a CALL METHOD command
		Token firstCode = command.getFirstCodeToken();
		if (firstCode != null && firstCode.isKeyword())
			return false;
		// ensure this is a functional method call, and the call (chain) is the start of the Command 
		Token parent = token.getParent(); 
		while (parent.closesLevel() && parent.getPrevSibling() != null)
			parent = parent.getPrevSibling();
		if (parent != command.getFirstToken())
			return false;
		
		// ensure the functional method call does NOT contain EXCEPTIONS 
		if (token.getParent().getFirstChild().matchesOnSiblings(true, TokenSearch.ASTERISK, "EXCEPTIONS"))
			return false;

		Token keyword = token;
		Token paramName = keyword.getNext();
		Token assignmentOp = paramName.getNext();

		Term receivingTerm;
		try {
			receivingTerm = Term.createSimple(assignmentOp.getNext()); // e.g. "DATA(lv_...)", "FINAL(...)" or "lv_date+4(2)"
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		// RECEIVING can NOT be omitted if there is an(other) inline declaration in the IMPORTING section
		Token firstInlineDeclaration = token.getParent().getFirstChild().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "DATA(|FINAL(");
		if (firstInlineDeclaration != null && firstInlineDeclaration != receivingTerm.firstToken) {
			return false;
		}
		
		Token next = receivingTerm.getNext();
		if (next.lineBreaks == 0)
			next.copyWhitespaceFrom(keyword);

		receivingTerm.removeFromCommand(true);
		assignmentOp.removeFromCommand();
		paramName.removeFromCommand();
		keyword.removeFromCommand();

		Token oldFirstToken = command.getFirstToken();
		int oldIndent = oldFirstToken.spacesLeft;
		receivingTerm.firstToken.copyWhitespaceFrom(oldFirstToken);
		assignmentOp.setWhitespace();
		oldFirstToken.setWhitespace();

		oldFirstToken.insertLeftSibling(receivingTerm);
		oldFirstToken.insertLeftSibling(assignmentOp);
		command.addIndent(receivingTerm.getSumTextAndSpaceWidth() + 1 + assignmentOp.getTextLength() + 1, oldIndent, oldFirstToken);

		if (closeBracketsAtLineEndRule == null)
			closeBracketsAtLineEndRule = (ClosingBracketsPositionRule) parentProfile.getRule(RuleID.CLOSING_BRACKETS_POSITION);
		closeBracketsAtLineEndRule.executeOn(code, command, false, releaseRestriction);

		command.invalidateMemoryAccessType();
		
		return true;
	}
}
