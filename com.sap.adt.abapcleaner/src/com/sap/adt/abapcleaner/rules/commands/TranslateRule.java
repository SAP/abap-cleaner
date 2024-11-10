package com.sap.adt.abapcleaner.rules.commands;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rulehelpers.TriState;

public class TranslateRule extends RuleForDeclarations {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Prefer functional to procedural language constructs", "#prefer-functional-to-procedural-language-constructs"), 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Deprecated Key Word Check", "deprecated-key-word.md"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "TRANSLATE", "abaptranslate.htm"), 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "string_func - Processing Functions", "abenprocess_functions.htm") };

	@Override
	public RuleID getID() { return RuleID.TRANSLATE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace TRANSLATE with string functions"; }

	@Override
	public String getDescription() { return "Replaces the deprecated TRANSLATE statement with corresponding string processing functions."; }

	@Override
	public String getHintsAndRestrictions() { return "'TRANSLATE text USING mask' is only replaced if the mask is a literal."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 29); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE, RuleID.ALIGN_ASSIGNMENTS, RuleID.ALIGN_PARAMETERS } ; }

	// getRequiredAbapRelease() not required, as these built-in functions were introduced with ABAP release 7.02 (= 7.0, EhP2) 
	
	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD replace_deprecated_translate." 
			+ LINE_SEP + "    \" change text to lower and then to upper case"
			+ LINE_SEP + "    DATA lv_text TYPE string VALUE `Any Text`." 
			+ LINE_SEP + "    TRANSLATE lv_text TO LOWER CASE. \" `any text`" 
			+ LINE_SEP + "    TRANSLATE lv_text TO UPPER CASE. \" `ANY TEXT`" 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" replace a with b, A with B and back to get `Abracadabra`"
			+ LINE_SEP + "    DATA lv_magic TYPE string VALUE `Barbcbdbarb`." 
			+ LINE_SEP + "    TRANSLATE lv_magic USING 'abbaABBA'." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" replace invalid chars in file name to get `not-all-chars-are-allowed`"
			+ LINE_SEP + "    DATA lv_file_name TYPE string VALUE `not:all?chars\\are/allowed`." 
			+ LINE_SEP + "    TRANSLATE lv_file_name USING `\\-/-:-*-?-\"-<->-|-`." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" TRANSLATE results in `a b c ` here; for translate( ), it is important to use"
			+ LINE_SEP + "    \" text string literals `...` for the FROM and TO parameters, because the trailing"
			+ LINE_SEP + "    \" spaces of a text field literal TO = '   ' would instead result in `abc`:"
			+ LINE_SEP + "    DATA lv_abc TYPE string VALUE `a1b2c3`." 
			+ LINE_SEP + "    TRANSLATE lv_abc USING '1 2 3 '." 
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" if the mask has an uneven number of characters, TRANSLATE simply ignores"
			+ LINE_SEP + "    \" the last replacement, so in this case we get `C++` (NOT `C  ` or `C`); therefore,"
			+ LINE_SEP + "    \" translate( ) must have FROM = `c`, since FROM = `c+`  TO = `C` would remove the `+`"
			+ LINE_SEP + "    DATA lv_c_plus_plus TYPE string VALUE `c++`." 
			+ LINE_SEP + "    TRANSLATE lv_c_plus_plus USING 'cC+'." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" chains can only be processed if they are first unchained" 
			+ LINE_SEP + "    DATA lv_other_text TYPE char5 VALUE 'abcde'." 
			+ LINE_SEP + "    TRANSLATE: lv_text TO LOWER CASE, lv_other_text TO UPPER CASE." 
			+ LINE_SEP 
			+ LINE_SEP + "    TRANSLATE lv_abc USING: '1 2 3 ', 'a-b-c-'." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" unlike TRANSLATE, the string functions do not work on structured data; therefore,"
			+ LINE_SEP + "    \" changing the next statements might cause syntax errors. You can activate the option"
			+ LINE_SEP + "    \" 'Only replace TRANSLATE for known unstructured types' to restrict this cleanup rule"
			+ LINE_SEP + "    \" to cases in which " + Program.PRODUCT_NAME + " can clearly determine the type (as above)"
			+ LINE_SEP + "    DATA(ls_unknown_type) = get_charlike_structure( )." 
			+ LINE_SEP + "    DATA ls_ddic_type TYPE some_ddic_type." 
			+ LINE_SEP + "    DATA ls_other_type TYPE if_any_interface=>ty_s_typedef_out_of_sight." 
			+ LINE_SEP + "    TRANSLATE ls_unknown_type TO UPPER CASE." 
			+ LINE_SEP + "    TRANSLATE ls_ddic_type TO LOWER CASE." 
			+ LINE_SEP + "    TRANSLATE ls_other_type USING 'a1b2'." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

   final ConfigBoolValue configReplaceTranslateToUpperLower = new ConfigBoolValue(this, "ReplaceTranslateToUpperLower", "Replace TRANSLATE ... TO UPPER|LOWER", true);
   final ConfigBoolValue configReplaceTranslateUsing = new ConfigBoolValue(this, "ReplaceTranslateUsing", "Replace TRANSLATE ... USING", true);
   final ConfigBoolValue configReplaceUnevenMasks = new ConfigBoolValue(this, "ReplaceUnevenMasks", "Replace TRANSLATE ... USING if mask has an uneven number of chars", true);
	final ConfigBoolValue configProcessChains = new ConfigBoolValue(this, "ProcessChains", "Unchain TRANSLATE: chains (required for processing them with this rule)", true, false, LocalDate.of(2023, 10, 27));
	final ConfigBoolValue configSkipUnknownTypes = new ConfigBoolValue(this, "SkipUnknownTypes", "Only replace TRANSLATE for known unstructured types (STRING, C, N, CHAR1 etc.)", true, true, LocalDate.of(2024, 7, 6));
	final ConfigInfoValue configUnknownTypeWarning = new ConfigInfoValue(this, "Warning: deactivating this option might lead to syntax errors if your code contains TRANSLATE with structured types (but at least the syntax check will immediately show this)", ConfigInfoStyle.WARNING);

   private final ConfigValue[] configValues = new ConfigValue[] { configReplaceTranslateToUpperLower, configReplaceTranslateUsing, configReplaceUnevenMasks, configProcessChains, configSkipUnknownTypes, configUnknownTypeWarning };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configUnknownTypeWarning) {
			return !configSkipUnknownTypes.getValue();
		} else {
			return true; 
		}
	}

	public TranslateRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		Command command = methodStart;
		Command methodEnd = command.getNextSibling();
		while (command != methodEnd) {
			commandForErrorMsg = command;

			// get the next Command now, in case the current command is unchained into multiple Commands
			Command nextCommand = command.getNext();

			if (!isCommandBlocked(command)) {
				try {
					executeOnCommand(code, command, localVariables, releaseRestriction);
				} catch (UnexpectedSyntaxBeforeChanges ex) {
					// log the error and continue with next command
					ex.addToLog();
				}
			}
			
			command = nextCommand;
		}
	}

	private void executeOnCommand(Code code, Command command, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		// for the syntax of the deprecated TRANSLATE statement, see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abaptranslate.htm
		Token firstToken = command.getFirstToken();
		if (firstToken == null) // pro forma
			return;
		
		if (	!firstToken.matchesOnSiblings(true, "TRANSLATE", TokenSearch.ASTERISK, "TO", "UPPER|LOWER", "CASE")
			&& !firstToken.matchesOnSiblings(true, "TRANSLATE", TokenSearch.ASTERISK, "USING")) {
			return;
		}
		
		ArrayList<Command> unchainedCommands = unchain(code, command, configProcessChains.getValue());
		if (unchainedCommands == null || unchainedCommands.isEmpty())
			return;
		
		for (Command unchainedCommand : unchainedCommands) {
			firstToken = unchainedCommand.getFirstCodeToken();
			if (firstToken == null) 
				continue;

			TriState argHasUnstructuredCharlikeType = localVariables.varHasUnstructuredCharlikeType(firstToken.getNextCodeSibling());
			if (argHasUnstructuredCharlikeType == TriState.FALSE
					|| argHasUnstructuredCharlikeType == TriState.UNKNOWN && configSkipUnknownTypes.getValue()) {
				// skip Command: this variable might represent a structured type, which can be processed with TRANSLATE, 
				// but would lead to a syntax error with to_upper( ), to_lower( ) or translate( )
				continue;
			}
			
			if (firstToken.matchesOnSiblings(false, "TRANSLATE", TokenSearch.ANY_IDENTIFIER, "TO", "UPPER|LOWER", "CASE")) {
				if (configReplaceTranslateToUpperLower.getValue() && replaceTranslateToUpperOrLower(unchainedCommand, firstToken)) {
					code.addRuleUse(this, unchainedCommand);
				}
			} else if (firstToken.matchesOnSiblings(false, "TRANSLATE", TokenSearch.ANY_IDENTIFIER, "USING", TokenSearch.ANY_LITERAL)) {
				if (configReplaceTranslateUsing.getValue() && replaceTranslateUsing(unchainedCommand, firstToken)){
					code.addRuleUse(this, unchainedCommand);
				}
			}
		}
	}

	private boolean replaceTranslateToUpperOrLower(Command command, Token firstToken) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		Token identifier1 = firstToken.getNextCodeSibling();
		Token toToken = identifier1.getNextCodeSibling();
		Token upperLowerToken = toToken.getNextCodeSibling();
		Token caseToken = upperLowerToken.getNextCodeSibling();
		if (!caseToken.getNextCodeSibling().isPeriod()) // pro forma
			return false;
		
		int sourceLineNum = identifier1.sourceLineNum;
		
		// remove TRANSLATE
		firstToken.removeFromCommand();
		
		// create 'identifier = identifier TO UPPER|LOWER'
		toToken.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		Token identifier2 = Token.createForAbap(0, 1, identifier1.getText(), TokenType.IDENTIFIER, sourceLineNum);
		// ensure the type is correct, even if the identifier looks like an ABAP keyword
		identifier2.type = identifier1.type;
		toToken.insertLeftSibling(identifier2);

		// insert parentheses 'to_upper( ... )' or 'to_lower( ... )' around the second identifier Token
		String stringFunc = upperLowerToken.textEquals("UPPER") ? "to_upper(" : "to_lower(";
		identifier2.insertParenthesesUpTo(toToken, stringFunc, ")");
		
		toToken.removeFromCommand();
		upperLowerToken.removeFromCommand();
		caseToken.removeFromCommand();

		command.invalidateMemoryAccessType();
		return true;
	}
	
	private boolean replaceTranslateUsing(Command command, Token firstToken) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		Token identifier1 = firstToken.getNextCodeSibling();
		Token usingToken = identifier1.getNextCodeSibling();
		Token maskToken = usingToken.getNextCodeSibling();
		
		// only process if there is a single mask literal (not an identifier, a concatenation of several literals, etc.)
		if (!maskToken.isStringLiteral() || !maskToken.getNextCodeSibling().isPeriod()) // || ... pro forma
			return false;

		int sourceLineNum = identifier1.sourceLineNum;
		
		String maskText = ABAP.unescapeCharLiteral(maskToken.getText());
		if (maskText == null || maskText.length() < 2)
			return false;
		if (maskText.length() % 2 != 0 && !configReplaceUnevenMasks.getValue())
			return false;

		// determine the 'from' and 'to' masks
		// if the mask length is uneven, simply ignore the last character, as does TRANSLATE ... USING;
		// it would NOT be equivalent to add the last character only to the fromChars, because translate( ) would then 
		// delete this character (thus shortening the string)  
		StringBuilder fromChars = new StringBuilder();
		StringBuilder toChars = new StringBuilder();
		for (int i = 0; i + 1 < maskText.length(); i += 2) {
			fromChars.append(maskText.charAt(i));
			toChars.append(maskText.charAt(i + 1));
		}
		// the new literals MUST be text string literals `...`, otherwise processing of trailing spaces may get wrong
		String fromCharsText = ABAP.toTextStringLiteral(fromChars.toString());
		String toCharsText = ABAP.toTextStringLiteral(toChars.toString());
		
		// remove TRANSLATE
		firstToken.removeFromCommand();
		
		// create 'identifier = val  = identifier  
		//                      from = <fromLiteral>  
		//                      to   = <toLiteral> USING ...'
		usingToken.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));

		Token paramStart = Token.createForAbap(0, 1, "val", TokenType.IDENTIFIER, sourceLineNum);
		usingToken.insertLeftSibling(paramStart);
		usingToken.insertLeftSibling(Token.createForAbap(0, 2, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		Token identifier2 = Token.createForAbap(0, 1, identifier1.getText(), TokenType.IDENTIFIER, sourceLineNum);
		usingToken.insertLeftSibling(identifier2);

		int indent = paramStart.getStartIndexInLine();
		usingToken.insertLeftSibling(Token.createForAbap(1, indent, "from", TokenType.IDENTIFIER, sourceLineNum));
		usingToken.insertLeftSibling(Token.createForAbap(0, 1, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		usingToken.insertLeftSibling(Token.createForAbap(0, 1, fromCharsText, TokenType.LITERAL, sourceLineNum));
		
		usingToken.insertLeftSibling(Token.createForAbap(1, indent, "to", TokenType.IDENTIFIER, sourceLineNum));
		usingToken.insertLeftSibling(Token.createForAbap(0, 3, "=", TokenType.ASSIGNMENT_OP, sourceLineNum));
		usingToken.insertLeftSibling(Token.createForAbap(0, 1, toCharsText, TokenType.LITERAL, sourceLineNum));
		
		// insert parentheses 'translate( ... )' around the parameters; this also moves the 'from' and 'to' line to the right 
		paramStart.insertParenthesesUpTo(usingToken, "translate(", ")");
		
		usingToken.removeFromCommand();
		maskToken.removeFromCommand();

		command.invalidateMemoryAccessType();
		return true;
	}
}
