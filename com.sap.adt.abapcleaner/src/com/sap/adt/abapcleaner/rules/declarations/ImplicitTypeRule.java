package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class ImplicitTypeRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Avoid obsolete language elements", "#avoid-obsolete-language-elements"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Obsolete Declarations: TYPES, implicit", "abaptypes_implicit.htm"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "For legibility, use LENGTH instead of parentheses", "abapdata_simple.htm")};

	@Override
	public RuleID getID() { return RuleID.IMPLICIT_TYPE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Make implicit type explicit"; }

	@Override
	public String getDescription() { return "Replaces obsolete short forms of declarations with explicit forms that specify the TYPE, LENGTH, and DECIMALS (if applicable)."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 10, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_DECLARATIONS, RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
 			+ LINE_SEP + "CLASS cl_implicit_type DEFINITION." 
 			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP + "    \" in the object-oriented context, TYPES only allows for two short forms with implicit TYPE,"
			+ LINE_SEP + "    \" while more short forms (with implicit LENGTH and DECIMALS) are tolerated with (CLASS-)DATA, CONSTANTS, STATICS:"
			+ LINE_SEP + "    TYPES: ty_b(5),"
			+ LINE_SEP + "           ty_c LENGTH 5."
			+ LINE_SEP
			+ LINE_SEP + "    TYPES: BEGIN OF ty_s_implicit,"
			+ LINE_SEP + "             b(5),"
			+ LINE_SEP + "             c LENGTH 5,"
			+ LINE_SEP + "           END OF ty_s_implicit."
			+ LINE_SEP
			+ LINE_SEP + "    \" for built-in types c, n, and x, the default length is 1"
			+ LINE_SEP + "    CONSTANTS: gc_a VALUE 'a',"
			+ LINE_SEP + "               gc_b(5) VALUE 'abcde',"
			+ LINE_SEP + "               gc_d TYPE c VALUE 'a',"
			+ LINE_SEP + "               gc_e TYPE n VALUE '2',"
			+ LINE_SEP + "               gc_f TYPE x VALUE ''."
			+ LINE_SEP
			+ LINE_SEP + "    \" the cleanup rule also ensures that each of the expanded declarations gets a line of its own"
			+ LINE_SEP + "    CLASS-DATA:"
			+ LINE_SEP + "      gv_a, gv_b(5),"
			+ LINE_SEP + "      gv_d TYPE c, gv_e TYPE n, gv_f TYPE x,"
			+ LINE_SEP + "      \" for built-in type p (packed numbers), the default is LENGTH 8 DECIMALS 0"
			+ LINE_SEP + "      gv_g TYPE p, gv_h(5) TYPE p, gv_i TYPE p LENGTH 3, gv_j TYPE p DECIMALS 4."
			+ LINE_SEP 
 			+ LINE_SEP + "    CLASS-METHODS make_implicit_type_explicit." 
 			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP 
 			+ LINE_SEP + "CLASS cl_implicit_type IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD make_implicit_type_explicit."
			+ LINE_SEP + "    CONSTANTS lc_text_length TYPE i VALUE 10."
			+ LINE_SEP
			+ LINE_SEP + "    STATICS:"
			+ LINE_SEP + "      st_g TYPE p VALUE '123456789012345-',"
			+ LINE_SEP + "      st_h(5) TYPE p VALUE '123456789-',"
			+ LINE_SEP + "      st_i TYPE p LENGTH 3 VALUE '12345-',"
			+ LINE_SEP + "      st_j TYPE p DECIMALS 4 VALUE '12345678901.2345-'."
			+ LINE_SEP
			+ LINE_SEP + "    \" DATA allows for all short forms except 'x LENGTH n':"
			+ LINE_SEP + "    DATA lv_a."
			+ LINE_SEP + "    DATA lv_b(5)."
			+ LINE_SEP + "    DATA lv_d TYPE c."
			+ LINE_SEP + "    DATA lv_e TYPE n."
			+ LINE_SEP + "    DATA lv_f TYPE x."
			+ LINE_SEP
			+ LINE_SEP + "    DATA lv_g TYPE p."
			+ LINE_SEP + "    DATA lv_h(5) TYPE p."
			+ LINE_SEP + "    DATA lv_i TYPE p LENGTH 3."
			+ LINE_SEP + "    DATA lv_j TYPE p DECIMALS 4."
			+ LINE_SEP
			+ LINE_SEP + "    \" the length may also be specified with a constant:"
			+ LINE_SEP + "    DATA lv_k(lc_text_length)."
			+ LINE_SEP + "    DATA lv_l(lc_text_length) TYPE c."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP + "ENDCLASS." ;
   }

   final ConfigBoolValue configExecuteOnTypes = new ConfigBoolValue(this, "ExecuteOnTypes", "Execute on TYPES statements", true);
   final ConfigBoolValue configExecuteOnConstantsAndStatics = new ConfigBoolValue(this, "ExecuteOnConstantsAndStatics", "Execute on CONSTANTS and STATICS statements", true);
   final ConfigBoolValue configExecuteOnDataAndClassData = new ConfigBoolValue(this, "ExecuteOnDataAndClassData", "Execute on DATA and CLASS-DATA statements", true);
   final ConfigBoolValue configReplaceParenthesisWithLength = new ConfigBoolValue(this, "ReplaceParenthesisWithLength", "Replace parentheses with LENGTH", true);

   private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnTypes, configExecuteOnConstantsAndStatics, configExecuteOnDataAndClassData, configReplaceParenthesisWithLength };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ImplicitTypeRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override 
	protected boolean skipDeclarationsInsideBeginOf() {
		return false; 
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstCodeToken();
		if (firstToken == null)
			return false;

		boolean process = false;
		if (firstToken.isKeyword("TYPES")) 
			process = configExecuteOnTypes.getValue();
		else if (firstToken.isAnyKeyword("DATA", "CLASS-DATA")) // this does NOT include "DATA("
			process = configExecuteOnDataAndClassData.getValue();
		else if (firstToken.isAnyKeyword("CONSTANTS", "STATICS")) 
			process = configExecuteOnConstantsAndStatics.getValue();

		if (!process)
			return false;
		
		// if the whole Command is inside a BEGIN OF ... block, then make sure this is NOT a BEGIN OF ENUM block
		if (command.isInsideBeginOfEnumBlock())
			return false;
		
		Token token = firstToken.getNextCodeSibling();
		if (token.isChainColon()) 
			token = token.getNextCodeSibling();

		int firstIdentifierIndent = -1;
		while (token != null) {
			if (token.matchesOnSiblings(true, "BEGIN|END", "OF")) {
				if (firstIdentifierIndent >= 0)
					firstIdentifierIndent += token.isKeyword("BEGIN") ? ABAP.INDENT_STEP : -ABAP.INDENT_STEP; 
				
				// BEGIN OF ENUM ... END OF ENUM must be completely skipped
				if (token.matchesOnSiblings(true, "BEGIN|END", "OF", "ENUM")) {
					token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "END", "OF", "ENUM");
					if (token == null)
						break;
				} 
				// skip up to the comma or period
				token = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ".|,");

			} else if (token.isIdentifier()) {
				if (firstIdentifierIndent < 0)
					firstIdentifierIndent = token.getStartIndexInLine();
				token = executeOn(code, command, token, firstIdentifierIndent);
			}
			if (token != null) {
				token = token.getNextCodeSibling();
			}
		}
		return false;
	}
	
	private Token executeOn(Code code, Command command, Token identifier, int baseIndent) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		boolean hasType = false;
		boolean hasNumericLengthInParens = identifier.textEndsWith(")");
		boolean hasConstantLengthInParens = identifier.textEndsWith("(") && identifier.hasChildren() && identifier.getNext().isAttached();
		boolean hasLength = hasNumericLengthInParens || hasConstantLengthInParens;
		boolean hasDecimals = false; // for packed numbers
		boolean skip = false;
		
		Token typeKeyword = null;
		Token typeIdentifier = null;
		Token lengthKeyword = null;
		Token lengthLiteral = null; 
		
		boolean changed = false;
		
		// find out whether this declaration already specifies TYPE, LENGTH, and DECIMALS,
		// while moving token to the next period or comma (which is later returned to the caller) 
		Token token = identifier.getNextCodeSibling();
		while (token != null && !token.isCommaOrPeriod()) {
			if (token.isKeyword()) {
				if (token.isKeyword("TYPE")) {
					hasType = true;
					typeKeyword = token;
					typeIdentifier = token.getNextCodeSibling();
				
				} else if (token.isKeyword("LIKE")) {
					// skip declarations with LIKE
					skip = true;

				} else if (token.isKeyword("LENGTH")) {
					hasLength = true;
					lengthKeyword = token;
					lengthLiteral = token.getNextCodeSibling();
				
				} else if (token.isKeyword("DECIMALS")) {
					hasDecimals = true;

				} else if (token.isKeyword("OCCURS")) {
					// skip obsolete DATA ... OCCURS / TYPES ... OCCURS declarations (forbidden in classes),   
					// because changing 'DATA lt_val(20) OCCURS 0 WITH HEADER LINE.' into
					// 'DATA lt_any TYPE c LENGTH 20 OCCURS 0 WITH HEADER LINE.' would be a syntax error, see 
					// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abapdata_occurs.htm
					// https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abaptypes_occurs.htm
					skip = true;
				}
			}
			token = token.getNextCodeSibling();
		}
		
		// skip declarations with LIKE or obsolete OCCURS    
		if (skip)
			return token;

		// only continue with built-in types c, n, x, and p without TYPE, the type is "c" (ABAP.DEFAULT_TYPE)
		boolean isBuiltInTypeCNXP = !hasType || typeIdentifier.textEqualsAny("c", "n", "x", "p");
		if (!isBuiltInTypeCNXP)
			return token;
		boolean isPackedNumber = (typeIdentifier != null && typeIdentifier.textEquals("p"));

		// 'c(5)': remove length from the parentheses attached to the identifier (if configured) 
		// see Hints on https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abapdata_simple.htm
		String lengthFromParens = null;
		boolean moveLengthFromParens = false;
		if (hasNumericLengthInParens && configReplaceParenthesisWithLength.getValue()) {
			// e.g. 'DATA lv_text(5).', where 'lv_text(5)' is parsed into one single Token
			String identifierText = identifier.getText();
			int parensPos = identifierText.indexOf('(');
			if (parensPos > 0) {
				lengthFromParens = identifierText.substring(parensPos + 1, identifierText.length() - 1);
				if (ABAP.isInteger(lengthFromParens)) {
					moveLengthFromParens = true;
					identifier.setText(identifierText.substring(0, parensPos), false);
					changed = true;
				}
			}
			
		} else if (hasConstantLengthInParens && configReplaceParenthesisWithLength.getValue()) {
			// e.g. 'DATA lv_text(lc_length) TYPE c.', where the identifier for the length (which must be a constant) is parsed into a distinct Token
			lengthFromParens = identifier.getNext().getText();
			moveLengthFromParens = true;
			
			// create a new Token for the identifier without length (the old Token opens a level, as it ends with an opening parenthesis)
			String newText = identifier.getText().substring(0, identifier.getTextLength() - 1);
			Token newIdentifier = Token.createForAbap(identifier.lineBreaks, identifier.spacesLeft, newText, identifier.type, identifier.sourceLineNum);
			identifier.insertLeftSibling(newIdentifier);
			
			// remove the old identifier and length, e.g. 'lv_text(lc_length)'
			Term oldIdentifierWithLength;
			try {
				oldIdentifierWithLength = Term.createForTokenRange(identifier, identifier.getNextCodeSibling());
			} catch (UnexpectedSyntaxException e) {
				throw new UnexpectedSyntaxAfterChanges(this, e);		
			}
			oldIdentifierWithLength.firstToken.setWhitespace(); // continue after the new identifier, otherwise .removeFromCommand will put a line break after oldIdentifierWithLength 
			oldIdentifierWithLength.removeFromCommand(false);
			identifier = newIdentifier; 

			changed = true;
		}

		// add TYPE 
		if (!hasType) {
			typeKeyword = Token.createForAbap(0, 1, "TYPE", TokenType.KEYWORD, identifier.sourceLineNum);
			typeIdentifier = Token.createForAbap(0, 1, ABAP.DEFAULT_TYPE, TokenType.IDENTIFIER, identifier.sourceLineNum);
			// in case of 'DATA lv_k(lc_text_length).' (with deactivated configReplaceParenthesisWithLength), identifier may still open a level
			Token identifierEnd = identifier.getNextSiblingWhileLevelOpener();
			identifierEnd.insertRightSibling(typeKeyword);
			typeKeyword.insertRightSibling(typeIdentifier);
			changed = true;
		}

		// add LENGTH 
		if (moveLengthFromParens || !hasLength) {
			String defaultLength = isPackedNumber ? ABAP.DEFAULT_LENGTH_OF_TYPE_P : ABAP.DEFAULT_LENGTH_OF_TYPE_C_N_X;
			String useLength = moveLengthFromParens ? lengthFromParens : defaultLength;
			lengthKeyword = Token.createForAbap(0, 1, "LENGTH", TokenType.KEYWORD, typeIdentifier.sourceLineNum);
			lengthLiteral = Token.createForAbap(0, 1, useLength, typeIdentifier.sourceLineNum);
			typeIdentifier.insertRightSibling(lengthKeyword);
			lengthKeyword.insertRightSibling(lengthLiteral);
			changed = true;
		}

		// add DECIMALS
		if (isPackedNumber && !hasDecimals) {
			// if the length is provided with "identifier(length)", append the DECIMALS after the TYPE information 
			Token prevToken = (lengthLiteral == null) ? typeIdentifier : lengthLiteral;
			Token decimalsKeyword = Token.createForAbap(0, 1, "DECIMALS", TokenType.KEYWORD, prevToken.sourceLineNum);
			Token decimalsLiteral = Token.createForAbap(0, 1, ABAP.DEFAULT_DECIMALS_OF_TYPE_P, TokenType.LITERAL, prevToken.sourceLineNum);
			prevToken.insertRightSibling(decimalsKeyword);
			decimalsKeyword.insertRightSibling(decimalsLiteral);
			changed = true;
		}

		// if the declaration was changed, make sure it is on an own line
		if (changed) {
			// break before the identifier (unless it is the first identifier in this Command)
			if (identifier.lineBreaks == 0) {
				Token prev = identifier.getPrevCodeSibling();
				if (prev != null && prev.isComma()) {
					identifier.setWhitespace(1, baseIndent);
				}
			}

			// break before the next identifier
			Token nextIdentifier = token.getNextCodeSibling();
			if (nextIdentifier != null && nextIdentifier.lineBreaks == 0) {
				nextIdentifier.setWhitespace(1, baseIndent);
			}
		}
		
		if (changed)
			code.addRuleUse(this, command);

		return token;
	}
}