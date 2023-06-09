package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * Replaces "a = a + 1." with "a += 1." etc.
 *
 */
public class CalculationAssignmentRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.CALCULATION_ASSIGNMENT; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Prefer calculation assignment operators (+=, -= etc.)"; }

	@Override
	public String getDescription() {
		return "Transforms assignments like i = i + 1 to use calculation assignment operators (+=, -= etc.).";
	}

	@Override
	public String getHintsAndRestrictions() { return "This rule requires a NetWeaver version >= 7.54."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 1); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS, RuleID.ALIGN_ASSIGNMENTS } ; }

	@Override
	public int getRequiredAbapRelease() { return ABAP.REQUIRED_RELEASE_754; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD prefer_calculation_assign_ops." 
			+ LINE_SEP + "    \" simple cases" 
			+ LINE_SEP + "    lv_value = lv_value + 1." 
			+ LINE_SEP + "    lv_value = lv_value - 1." 
			+ LINE_SEP + "    lv_value = lv_value * lv_factor." 
			+ LINE_SEP + "    lv_value = lv_value / lv_divisor." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = - lv_value." 
			+ LINE_SEP + "    lv_value = - lv_value * 10." 
			+ LINE_SEP + "    lv_value = - lv_value / lv_divisor." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = 2 + lv_value." 
			+ LINE_SEP + "    lv_value = - 10 * lv_value." 
			+ LINE_SEP + "    lv_value = lv_factor * lv_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" more complex cases" 
			+ LINE_SEP + "    ls_struc-component = ls_struc-component + 1." 
			+ LINE_SEP + "    <ls_field>-component = <ls_field>-component - 1." 
			+ LINE_SEP + "    <ls_field>-component = - <ls_field>-component." 
			+ LINE_SEP + "    lv_value = lv_value * ( -1 ).  \" parentheses must be removed, because *= ( -1 ) would be a syntax error" 
			+ LINE_SEP + "    lv_value = lv_value + get_value( EXPORTING iv_value1 = lv_value " 
			+ LINE_SEP + "                                               iv_value2 = 'abc'    " 
			+ LINE_SEP + "                                               iv_value3 = VALUE #( a = 5" 
			+ LINE_SEP + "                                                                    b = 7 ) ). " 
			+ LINE_SEP + "    lv_date+4(2) = lv_date+4(2) - 1." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" cases that must NOT be changed" 
			+ LINE_SEP + "    lv_value = iv_value + 1.     \" identifiers do not match!" 
			+ LINE_SEP + "    lv_value = lv_value * 5 + 3. \" due to operator priority this is NOT the same as lv_value *= 5 + 3" 
			+ LINE_SEP + "    lv_value = lv_value / 2 + 1. \" due to operator priority this is NOT the same as lv_value /= 2 + 1" 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configConvertMinusToMultiplication = new ConfigBoolValue(this, "ConvertMinusToMultiplication", "Convert 'a = - a' to 'a *= -1'", false, false, LocalDate.of(2022, 7, 4));
	final ConfigBoolValue configAllowMinusBeforeMultOrDiv = new ConfigBoolValue(this, "AllowMinusBeforeMultOrDiv", "Convert 'a = - a * b' and 'a = - a / b'", true, false, LocalDate.of(2022, 12, 16));
	final ConfigBoolValue configAllowVariableAtEnd = new ConfigBoolValue(this, "AllowVariableAtEnd", "Convert 'a = b + a' and 'a = b * a'", true, false, LocalDate.of(2022, 12, 16));

	private final ConfigValue[] configValues = new ConfigValue[] { configConvertMinusToMultiplication, configAllowMinusBeforeMultOrDiv, configAllowVariableAtEnd };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CalculationAssignmentRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (command.containsChainColon())
			return false;

		Token firstToken = command.getFirstToken();
		if (firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", "-", TokenSearch.ANY_IDENTIFIER, ".")) {
			return convertMinusToMultiplication(code, command);
		}

		boolean changed = convertVariableAtStart(command);
		if (!changed && configAllowVariableAtEnd.getValue()) {
			changed = convertVariableAtEnd(command);
		}
		return changed;
	}
	
	private boolean convertMinusToMultiplication(Code code, Command command) throws UnexpectedSyntaxAfterChanges {
		if (!configConvertMinusToMultiplication.getValue())
			return false;
		
		Token firstIdentifier = command.getFirstToken();
		Token assignment = firstIdentifier.getNext();
		Token minusToken = assignment.getNext();
		Token secondIdentifier = minusToken.getNext();
		if (!firstIdentifier.textEquals(secondIdentifier.getText()))
			return false;

		assignment.setText("*=", false);
		minusToken.insertLeftSibling(Token.createForAbap(0, 1, "-1", TokenType.LITERAL, minusToken.sourceLineNum));
		minusToken.removeFromCommand();
		secondIdentifier.removeFromCommand();

		return true;	
	}
	
	private boolean convertVariableAtStart(Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// For operator priority and execution order, see
		// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenarith_operators.htm
		// - prio 3) **          [only this is right-to-left]
		// - prio 2) * / DIV MOD [all left-to-right]
		// - prio 1) + -         [all left-to-right]
		// Therefore, we only allow the expansion to longer arithmetic expressions for "+=", because "a = a + ..." is always the same as "a += (...)".
		// - for "-", "a = a - b - c" is NOT the same as "a -= b - c" (execution order "(a - b) - c" versus "a - (b - c)")
		// - for "*", "a = a * b + c" is NOT the same as "a *= b + c" (operator priority "(a * b) + c" versus "a * (b + c)")
		// - for "/", "a = a / b + c" is NOT the same as "a /= b + c" (operator priority "(a / b) + c" versus "a / (b + c)")

		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_IDENTIFIER, "+", TokenSearch.ANY_ARITHMETIC_EXPRESSION, ".")
		 && !firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_IDENTIFIER, "-", TokenSearch.ANY_TERM, ".")
		 && !firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_IDENTIFIER, "*|/", TokenSearch.ANY_TERM, ".")
		 && !(firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", "-", TokenSearch.ANY_IDENTIFIER, "*|/", TokenSearch.ANY_TERM, ".") && configAllowMinusBeforeMultOrDiv.getValue())) {
			return false;
		}

		Token firstIdentifier = firstToken;
		Token assignment = firstIdentifier.getNext();

		Token token = assignment.getNext();
		Token minusSign = null;
		if (token.textEquals("-")) { // only possible for "*" and "/", otherwise we would have returned from the method already
			minusSign = token;
			token = minusSign.getNext();
		}

		Token secondIdentifier = token;
		if (!firstIdentifier.textEquals(secondIdentifier.getText()))
			return false;

		Token arithmeticOp = secondIdentifier.getNext();
		if (!arithmeticOp.textEqualsAny("+", "-", "*", "/"))
			throw new UnexpectedSyntaxBeforeChanges(this, arithmeticOp, "Operator +, -, * or / expected, but found " + arithmeticOp.getTypeAndTextForErrorMessage());

		Term term;
		try {
			if (arithmeticOp.textEquals("+"))
				term = Term.createArithmetic(arithmeticOp.getNext());
			else
				term = Term.createSimple(arithmeticOp.getNext());
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}

		int termPos = term.firstToken.getStartIndexInLine();

		assignment.setText(arithmeticOp.getText() + "=", false);
		secondIdentifier.removeFromCommand();
		arithmeticOp.removeFromCommand();
		term.addIndent(term.firstToken.getStartIndexInLine() - termPos);
		
		simplifyTerm(command, minusSign, term);
		
		return true;
	}
	
	private boolean convertVariableAtEnd(Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// check for cases like "a = b + a" or "a = b * a" (which of course does NOT work for - or / )  
		// only allowing "b" to be a single term (not an arithmetic expression) 
		Token firstToken = command.getFirstToken();
		if (!firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_TERM, "+", TokenSearch.ANY_IDENTIFIER, ".")
		 && !firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", TokenSearch.ANY_TERM, "*", TokenSearch.ANY_IDENTIFIER, ".")
		 && !(firstToken.matchesOnSiblings(false, TokenSearch.ANY_IDENTIFIER, "=", "-", TokenSearch.ANY_TERM, "*", TokenSearch.ANY_IDENTIFIER, ".") && configAllowMinusBeforeMultOrDiv.getValue())) {
			return false;
		}

		Token firstIdentifier = firstToken;
		Token assignment = firstIdentifier.getNext();

		Token token = assignment.getNext();
		Token minusSign = null;
		if (token.textEquals("-")) { // only possible for "*", otherwise we would have returned from the method already
			minusSign = token;
			token = minusSign.getNext();
		}

		Term term;
		try {
			term = Term.createSimple(token);
		} catch (UnexpectedSyntaxException ex) {
			throw new UnexpectedSyntaxBeforeChanges(this, ex);
		}
		int termPos = term.firstToken.getStartIndexInLine();

		Token arithmeticOp = term.getNext();
		if (!arithmeticOp.textEqualsAny("+", "*"))
			throw new UnexpectedSyntaxBeforeChanges(this, arithmeticOp, "Operator + or * expected, but found " + arithmeticOp.getTypeAndTextForErrorMessage());

		Token secondIdentifier = arithmeticOp.getNext();
		if (!firstIdentifier.textEquals(secondIdentifier.getText()))
			return false;

		assignment.setText(arithmeticOp.getText() + "=", false);
		arithmeticOp.removeFromCommand();
		secondIdentifier.removeFromCommand();
		term.addIndent(term.firstToken.getStartIndexInLine() - termPos);

		simplifyTerm(command, minusSign, term);

		return true;
	}

	private void simplifyTerm(Command command, Token minusSign, Term term) throws UnexpectedSyntaxAfterChanges {
		// remove parentheses that enclose the whole expression on the right-hand side of the calculation assignment, 
		// because lhs *= rhs is replaced by lhs = lhs * ( rhs ), see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencalculation_assignments.htm 
	   // Therefore it is a syntax error to have a single expression inside the parentheses (unless there is a minus sign before it): 
		// - syntax error: "iv_value *= ( -1 )." (because it would be replaced with "iv_value = iv_value * ( ( - 1 ) ).")
		// - syntax error: "iv_value *= ( lts_table[ 1 ] )." 
		// - syntax error: "iv_value *= ( CONV i( -1 ) )"
		// By contrast, arithmetic expressions are fine:
		// - correct syntax: "iv_value *= ( 1 - 2 )." (here, however, it would be okay to remove the parentheses!)
		// - correct syntax: "iv_value += ( 1 - 2 ) + ( 3 - 4 ).", but these are of course no parentheses enclosing the whole right-hand side
		boolean isEnclosedInParens = term.firstToken.getOpensLevel() && term.firstToken.textEquals("(") && term.firstToken.hasChildren() 
													&& term.firstToken.getNextSibling() == term.lastToken && term.lastToken.textEquals(")"); 
		if (isEnclosedInParens && minusSign == null) {
			try {
				// determine the first inner term, e.g. "-1", "lts_table[ 1 ]", "CONV i( -1 )"
				Term firstInnerTerm = Term.createSimple(term.firstToken.getFirstChild());
				// remove parentheses if no other term follows
				if (firstInnerTerm.lastToken.getNextCodeSibling() == null) 
					term.firstToken.removeParentheses();
			} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, command, e.getMessage());
			}
		} else if (minusSign != null && term.isSingleIntegerLiteral()) {
			// convert "- n" to "-n" and "- -n" to "n"
			String negatedInt = ABAP.negateInteger(term.firstToken.getText());
			if (negatedInt != null) {
				minusSign.removeFromCommand();
				term.firstToken.setText(negatedInt, false);
			}
		}
		
		command.invalidateMemoryAccessType();
	}
}
