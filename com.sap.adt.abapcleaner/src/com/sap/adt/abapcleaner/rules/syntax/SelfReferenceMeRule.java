package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.LocalVariables;
import com.sap.adt.abapcleaner.rulehelpers.MethodInfo;

public class SelfReferenceMeRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Omit the self-reference me when calling an instance attribute or method", "#omit-the-self-reference-me-when-calling-an-instance-attribute-or-method"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Self-Reference", "self-reference.md") };
	
	private final String selfReferenceMe = "me->";

	@Override
	public RuleID getID() { return RuleID.SELF_REFERENCE_ME; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Remove the self-reference me->"; }

	@Override
	public String getDescription() { return "Removes the self-reference me->."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 7); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS any_class DEFINITION." 
 			+ LINE_SEP + "  PUBLIC SECTION." 
 			+ LINE_SEP + "    METHODS remove_self_reference_me" 
 			+ LINE_SEP + "      EXPORTING ev_result  TYPE x" 
 			+ LINE_SEP + "                ev_count   TYPE y" 
 			+ LINE_SEP + "                ets_result TYPE z." 
 			+ LINE_SEP + "    METHODS signature_out_of_sight REDEFINITION." 
 			+ LINE_SEP + "  PRIVATE SECTION." 
 			+ LINE_SEP + "    \" DATA ..." 
 			+ LINE_SEP + "ENDCLASS." 
 			+ LINE_SEP + "" 
 			+ LINE_SEP + "CLASS any_class IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD remove_self_reference_me." 
			+ LINE_SEP + "    me->any_method( a = 5" 
			+ LINE_SEP + "                    b = 'abc' )." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    ev_result = me->mv_value." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    ev_count += me->get_count( )." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    ets_result = VALUE #( ( line_id = 1  price = me->mv_price_1 )" 
			+ LINE_SEP + "                          ( line_id = 2  price = me->create_price( iv_currency = me->get_currency( )" 
			+ LINE_SEP + "                                                                   iv_amount   = me->mv_amount ) ) )." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "" 
			+ LINE_SEP + "  METHOD signature_out_of_sight."
			+ LINE_SEP + "    \" 'amount' and 'price' could be IMPORTING parameters, so me-> must NOT be removed"
			+ LINE_SEP + "    me->amount = me->price + 1." 
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS."; 
   }

	public SelfReferenceMeRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// nothing to do on class definition level
		return;
	}

	@Override
	protected void executeOn(Code code, Command methodStart, LocalVariables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Command command = methodStart.getNext();
		while (command != null && command != methodStart.getNextSibling()) {
			if (!isCommandBlocked(command)) { 
				
				Token token = command.getFirstToken();
				while (token != null) {
					if (token.isIdentifier() && token.textStartsWith(selfReferenceMe)
							&& (token.isFirstTokenInCommand() || token.getPrev().isAssignmentOperator() || token.getPrev().isComparisonOperator())) {
						
						if (executeOn(code, command, token, localVariables)) {
							code.addRuleUse(this, command);
						}
					}
					token = token.getNext();
				}
				
			}
			command = command.getNext();
		}
	}

	private boolean executeOn(Code code, Command command, Token token, LocalVariables localVariables) throws UnexpectedSyntaxAfterChanges {
		// determine the identifier behind 'me->'; note that token may contain expressions like 'me->mo_instance->any_method('
		String expression = token.getText().substring(selfReferenceMe.length());
		String identifier = LocalVariables.getObjectName(expression);
		boolean identifierIsMethodName = (expression.endsWith("(") && identifier.length() == expression.length() - ")".length());

		boolean isMethodSignatureKnown = localVariables.isMethodSignatureKnown();
		MethodInfo methodInfo = localVariables.getMethodInfo();
		
		if (!identifierIsMethodName) {
			// me-> is followed by an attribute (including cases like 'me->mo_instance->any_method(');
			
			// if there is a local variable with the same name as that attribute, then do NOT remove 'me->'
			if (localVariables.containsVariableInfo(identifier)) 
				return false;

			// if there is a method parameter with the same name as the attribute, do NOT remove 'me->'
			// (typically, IMPORTING parameters in constructors are stored to attributes: 'me->name = name.')
			if (isMethodSignatureKnown && methodInfo.hasParameter(identifier)) 
				return false;

			if (!isMethodSignatureKnown) {
				// if the method signature is unknown (e.g. because the method is redefined from a base class whose definition 
				// is not visible to ABAP cleaner), check the typical case 'me->name = name'; in such a case, 'me->' must (very likely)
				// NOT be removed
				if (token.isFirstTokenInCommand() && token.getNext() != null && token.getNext().isAssignmentOperator()) {
					Token valueToken = token.getNext().getNext();
					while (valueToken != null) {
						if (valueToken.isIdentifier() && AbapCult.stringEquals(identifier, LocalVariables.getObjectName(valueToken.getText()), true))
							return false;
						valueToken = valueToken.getNext();
					}
				}

				// TODO: at this point, we cannot be completely sure whether there may be a parameter of the same name as the identifier; 
				// currently, we therefore refrain from removing 'me->'; however, this decision could be exposed to the user 
				// by a configuration setting (with a cautious default), since the whole problem can NOT come up if prefixes like 
				// mv_, iv_, ev_ etc. are used
				return false;
			}
		}
		
		token.setText(token.getText().substring(selfReferenceMe.length()), true);
		return true;
	}
}
