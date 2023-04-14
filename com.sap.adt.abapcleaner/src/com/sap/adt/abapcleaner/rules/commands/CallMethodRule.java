package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rules.syntax.ExportingKeywordRule;
import com.sap.adt.abapcleaner.rules.syntax.ReceivingKeywordRule;

/**
 * Replaces CALL METHOD with a functional call, adding parentheses if needed. 
 * (parentheses may already be in the code: "CALL METHOD cx_message=&gt;raise_symsg( )" ); 
 * excludes cases of dynamic typing which cannot be replaced: CALL METHOD modify-&gt;(method_name) EXPORTING ...
 *
 */
public class CallMethodRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural calls", "#prefer-functional-to-procedural-calls"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid obsolete language elements", "#avoid-obsolete-language-elements"),	
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "CALL METHOD Usage", "call-method-usage.md"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Formulate static method calls without CALL METHOD", "abenmethod_call_guidl.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Obsolete Calls: CALL METHOD, Static", "abapcall_method_static.htm") };

	@Override
	public RuleID getID() { return RuleID.CALL_METHOD; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace CALL METHOD with functional call"; }

	@Override
	public String getDescription() {
		return "Replaces obsolete CALL METHOD statements with functional calls, adding parentheses, if missing. Leaves CALL METHOD if dynamic typing is used for the method name.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_call_method." 
			+ LINE_SEP + "    CALL METHOD lo_any_instance->any_method" 
			+ LINE_SEP + "      EXPORTING iv_param = iv_param" 
			+ LINE_SEP + "      IMPORTING ev_param = ev_param" 
			+ LINE_SEP + "      CHANGING  cv_param = cv_param." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" obsolete EXPORTING will be removed:" 
			+ LINE_SEP + "    CALL METHOD any_method_name" 
			+ LINE_SEP + "      EXPORTING iv_name   = iv_name" 
			+ LINE_SEP + "                iv_value  = iv_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" RECEIVING may be omitted, depending on Omit RECEIVING rule:" 
			+ LINE_SEP + "    CALL METHOD other_method_name" 
			+ LINE_SEP + "      EXPORTING iv_name   = iv_name" 
			+ LINE_SEP + "                iv_value  = iv_value" 
			+ LINE_SEP + "      RECEIVING rv_result = DATA(lv_result)." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" dynamic method calls cannot be replaced" 
			+ LINE_SEP + "    CALL METHOD mo_any_instance->(iv_dynamic_method_name)" 
			+ LINE_SEP + "      EXPORTING iv_par = iv_par. " 
			+ LINE_SEP 
			+ LINE_SEP + "    CALL METHOD (iv_dynamic_class_name)=>(iv_dynamic_method_name)" 
			+ LINE_SEP + "      EXPORTING iv_par = iv_par. " 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public CallMethodRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (command.containsChainColon())
			return false;
		
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, "CALL", "METHOD", TokenSearch.ANY_NON_COMMENT))
			return false;

		// the replacement is NOT possible for dynamic typing: CALL METHOD modify->(method_name) EXPORTING ...
		Token methodName = firstToken.getNext().getNext();
		if (methodName.textEndsWith("(") && methodName.getNext() != null && !methodName.getNext().isPrecededByWhitespace())
			return false;

		methodName.copyWhitespaceFrom(firstToken);

		command.getFirstToken().removeFromCommand(); // CALL
		command.getFirstToken().removeFromCommand(); // METHOD
		if (methodName.textEndsWith("(")) {
			// move to the end of the call chain 
			while (methodName.getNextSibling().getOpensLevel()) {
				methodName = methodName.getNextSibling();
			}
		} else {
			// in this case, there can be no call chain, so put everything that follows methodName in parentheses
			methodName.appendParenthesesUpTo(command.getLastNonCommentToken(), true);
		}

		// if activated, remove the RECEIVING keyword by executing the ReceivingKeywordRule
		ReceivingKeywordRule receivingKeywordRule = (ReceivingKeywordRule)parentProfile.getRule(RuleID.RECEIVING_KEYWORD);
		if (receivingKeywordRule.isActive) {
			Token receivingKeyword = command.getFirstToken().getLastTokenDeep(true, TokenSearch.ASTERISK, "RECEIVING");
			if (receivingKeyword != null && receivingKeyword.isKeyword()) {
				try {
					receivingKeywordRule.executeOn(code, command, receivingKeyword, releaseRestriction);
				} catch (UnexpectedSyntaxBeforeChanges e) {
					throw new UnexpectedSyntaxAfterChanges(this, command, "error removing RECEIVING keyword");
				}
			}
		}
		
		// remove optional EXPORTING by executing ExportingKeywordRule, regardless of whether the Rule is blocked for this Command
		((ExportingKeywordRule)parentProfile.getRule(RuleID.EXPORTING_KEYWORD)).executeOn(code, command, methodName, false, releaseRestriction);

		return true;
	}
}