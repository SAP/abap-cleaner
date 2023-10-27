package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;

public class ExportingKeywordRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Omit the optional keyword EXPORTING", "#omit-the-optional-keyword-exporting"), 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Omit Optional EXPORTING", "omit-optional-exporting.md") };

	@Override
	public RuleID getID() { return RuleID.EXPORTING_KEYWORD; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Omit optional EXPORTING"; }

	@Override
	public String getDescription() {
		return "Removes the optional EXPORTING keyword from method calls that do not use any other keyword (IMPORTING, CHANGING, RECEIVING, or EXCEPTIONS).";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 14); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD omit_optional_exporting." 
			+ LINE_SEP + "    lo_any_object->any_method(" 
			+ LINE_SEP + "      EXPORTING iv_par1 = iv_par1" 
			+ LINE_SEP + "                iv_par2 = iv_par2 )." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = get_value( EXPORTING iv_value1 = lv_value" 
			+ LINE_SEP + "                                    iv_value2 = 'abc'" 
			+ LINE_SEP + "                                    iv_value3 = get_inner_value( EXPORTING a = 5" 
			+ LINE_SEP + "                                                                           b = 7 ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" calls with other keywords such as IMPORTING and RECEIVING must NOT be changed:" 
			+ LINE_SEP + "    calculate( EXPORTING iv_value  = lv_value" 
			+ LINE_SEP + "               IMPORTING ev_result = lv_result )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public ExportingKeywordRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	public final boolean executeOn(Code code, Command command, Token token, boolean testRuleBlocked, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (testRuleBlocked && isCommandBlocked(command))
			return false;
		else
			return executeOn(code, command, token, releaseRestriction);
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (!token.getOpensLevel() || !token.hasChildren() || token.isLiteral() || token.getFirstChild().isAttached())
			return false;

		// ensure that EXPORTING is the only keyword
		Token firstChildCode = token.getFirstChild().getThisOrNextCodeToken();
		if (firstChildCode == null || !firstChildCode.isKeyword("EXPORTING")) {
			return false;
		} else if (firstChildCode.matchesOnSiblings(true, TokenSearch.ASTERISK, "IMPORTING|CHANGING|RECEIVING|EXCEPTIONS")) { 
			return false;
		}
		
		Token keyword = firstChildCode;
		if (!keyword.isKeyword())
			return false;
		Token next = keyword.getNext();
		int endIndex = keyword.getEndIndexInLine();
		boolean keywordIsFirstInLine = keyword.isFirstTokenInLine();
		int moveLeft = 0;
		if (next.lineBreaks == 0 && !next.isComment()) {
			if (keywordIsFirstInLine) {
				moveLeft = keyword.getTextLength() + next.spacesLeft - 2;
				next.spacesLeft += keyword.spacesLeft + keyword.getTextLength(); // will be moved left below
				next.lineBreaks = keyword.lineBreaks;
			} else {
				moveLeft = keyword.getTextLength() + next.spacesLeft;
				next.setWhitespace();
			}
		}
		keyword.removeFromCommand();
		command.addIndent(-moveLeft, endIndex, next);

		return true;
	}

}