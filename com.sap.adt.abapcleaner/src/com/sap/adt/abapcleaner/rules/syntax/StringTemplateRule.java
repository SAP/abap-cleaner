package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class StringTemplateRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use | to assemble text", "#use--to-assemble-text"), 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Text Assembly", "text-assembly.md") };

	@Override
	public RuleID getID() { return RuleID.STRING_TEMPLATE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Use string templates to assemble text"; }

	@Override
	public String getDescription() { return "Replaces the concatenation operator && with string templates |text { variable } text|."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 3, 12); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS } ; }

	// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-71-string_processing.htm
	public int getRequiredAbapRelease() { return ABAP.REQUIRED_RELEASE_702; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD use_string_templates." 
			+ LINE_SEP + "    \" cases with multiple literals" 
			+ LINE_SEP + "    lv_from = left && ` join ` && right && ` on `." 
			+ LINE_SEP + "    ls_sel_params-low = '*' && ls_sel_params-low && '*'." 
			+ LINE_SEP + "    out->write( `Name:` && ` ` && iv_first_name && ` ` && iv_last_name )." 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_fail( msg = 'Expected:' && ` ` && iv_number && ',' && ` ` && 'Actual:' && ` ` && lv_act )." 
			+ LINE_SEP + "    \" existing line breaks are kept:" 
			+ LINE_SEP + "    cl_abap_unit_assert=>assert_fail( msg = 'Expected:' && ` ` && iv_number && ',' && ` `" 
			+ LINE_SEP + "                                         && 'Actual:' && ` ` && lv_act )." 
			+ LINE_SEP + "    \" you might want to keep string templates with control characters separate:" 
			+ LINE_SEP + "    lv_text = |\\r\\n| && |first line|  && |\\t| && |cell B1|" 
			+ LINE_SEP + "           && |\\r\\n| && |second line| && |\\t| && |cell B2|." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" cases with one literal" 
			+ LINE_SEP + "    lv_first_day_of_march = lv_year && '0301'." 
			+ LINE_SEP + "    lv_formula = lv_var_1 && ` + ` && lv_var_2." 
			+ LINE_SEP + "    lv_salutation = `Hello ` && lv_name."
			+ LINE_SEP + "    \" the multi-line operand at the end can be embedded or ignored"
			+ LINE_SEP + "    lv_text = |{ lv_count } | && COND #( WHEN lv_count = 1" 
			+ LINE_SEP + "                                         THEN 'element'" 
			+ LINE_SEP + "                                         ELSE 'elements')." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" cases without literal" 
			+ LINE_SEP + "    lv_date = lv_year && lv_month && lv_day." 
			+ LINE_SEP + "    lv_fiscal_year_period = lv_fiscal_year && lv_period." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the && operator ignores trailing spaces in text field literals '...',"
			+ LINE_SEP + "    \" but keeps them in text string literals `...`, so the conversion reflects this:"
			+ LINE_SEP + "    lv_result1 = ' a   ' && ' +   ' && ' b  ' && ' =     ' && ' 10' && '  '." 
			+ LINE_SEP + "    lv_result2 = ` a   ` && ` +   ` && ` b  ` && ` =     ` && ` 10` && `  `." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" escape chars are changed accordingly:" 
			+ LINE_SEP + "    lv_escape1 = 'To ''be''' && ` or ``not`` to be`." 
			+ LINE_SEP + "    lv_escape2 = 'String templates must escape |' && ` as well as { and }.`." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" you may want to use string templates only on lines that contain operands to embed:" 
			+ LINE_SEP + "    rv_example  = `3 + 5 = ` && `8` && `. `" 
			+ LINE_SEP + "               && `a + b = ` && c && `.`." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigEnumValue<StringTemplateCondition> configStringTemplateCondition = new ConfigEnumValue<StringTemplateCondition>(this, "StringTemplateCondition", "Embed operands with |{ ... }|:",
			new String[] { "if result is shorter", "if result is shorter or equal", "always" }, StringTemplateCondition.values(), StringTemplateCondition.SHORTER_OR_EQUAL);
	final ConfigBoolValue configAlwaysConvertLiterals = new ConfigBoolValue(this, "AlwaysConvertLiterals", "Convert text literals in concatenations regardless of result length", true);
	final ConfigBoolValue configRequireOperandsOnSameLine = new ConfigBoolValue(this, "RequireOperandsOnSameLine", "Only convert text literals if line contains operands to embed", false, false, LocalDate.of(2024, 7, 6));
	final ConfigBoolValue configIgnoreMultiLineOperands = new ConfigBoolValue(this, "IgnoreMultiLineOperands", "Ignore multi-line operands at line start or end", true);
	final ConfigBoolValue configKeepControlCharsSeparate = new ConfigBoolValue(this, "KeepControlCharsSeparate", "Keep string templates with control characters \\t \\n \\r separate", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configStringTemplateCondition, configAlwaysConvertLiterals, configRequireOperandsOnSameLine, configIgnoreMultiLineOperands, configKeepControlCharsSeparate };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public StringTemplateRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// in ABAP SQL commands, String templates can only be used within @( .... ) 
		if (command.isAbapSqlOperation())
			return false;
		
		Token token = command.getFirstCodeToken();
		while (token != null) {
			if (token.textEquals("&&")) {
				// do NOT create string templates inside string templates, e.g. 
				//   any_method( |{ condense( lv_any ) && lv_other ALIGN = RIGHT }| ).
				// is better kept than changed to:  
				//   any_method( |{ |{ condense( lv_any) }{ lv_other }| ALIGN = RIGHT }| ).
				if (token.getParent() != null && token.getParent().startsEmbeddedExpression()) {
					token = token.getParent().getNextSibling();
					continue;
				}
				
				token = executeOn(code, command, token);
				if (token == null) {
					break;
				}
			}
			token = token.getNextCodeToken();
		}
		return false;
	}
	
	private Token executeOn(Code code, Command command, Token firstConcatOp) throws UnexpectedSyntaxAfterChanges {
		// find the previous operand: a literal or an identifier, which is possibly chained and with parentheses or brackets, 
		// and may belong to a constructor expression, e.g. 'CONV string( )'; TEXT-xyz is also possible 
		Token token = firstConcatOp;
		token = token.getPrevCodeSibling().getPrevSiblingWhileLevelCloser();
		if (token.startsConstructorExpression())
			token = token.getPrevCodeSibling();
		
		// create a list of consecutive operands and the concatenation operators between them
		ArrayList<Term> terms = new ArrayList<>();
		ArrayList<Token> concatOps = new ArrayList<>();
		do {
			Term newTerm;
			try {
				newTerm = Term.createSimple(token);
			} catch (UnexpectedSyntaxException e) {
				return firstConcatOp;
			}
			terms.add(newTerm);
			token = newTerm.getNextCodeSibling();
			concatOps.add(token);
			if (token == null || !token.textEquals("&&"))
				break;
			token = token.getNextCodeSibling();
		} while (token != null);

		// process the Terms in each line
		int startIndex = 0;
		do {
			ArrayList<Term> termsInLine = new ArrayList<>();
			ArrayList<Token> concatOpsInLine = new ArrayList<>();
			boolean isFirstTermInLine = true;
			
			int index = startIndex;
			do {
				Term term = terms.get(index);
				Token concatOp = concatOps.get(index); // may be null
				boolean isLastTerm = (index == terms.size() - 1);
				boolean isLastTermInLine = isLastTerm || term.lastToken.isLastTokenInLineExceptComment() || concatOp.isLastTokenInLineExceptComment();

				// enter the Term and the following && operator to the list, except for multi-line terms at line start or end (if they shall be ignored)
				if ((!isFirstTermInLine && !isLastTermInLine) || term.isOnSingleLine() || !configIgnoreMultiLineOperands.getValue()) {
					termsInLine.add(term);
					concatOpsInLine.add(concatOp);
					isFirstTermInLine = false;
				}

				if (isLastTermInLine) {
					if (executeOn(termsInLine, concatOpsInLine)) {
						code.addRuleUse(this, command);
					}
					startIndex = index + 1;
					break;
				}

				++index;
			} while (index < terms.size());
		} while (startIndex < terms.size());

		// continue after this concatenation
		return terms.get(terms.size() - 1).getNextCodeToken();
	}

	private boolean executeOn(ArrayList<Term> termsInLine, ArrayList<Token> concatOpsInLine) throws UnexpectedSyntaxAfterChanges {
		StringTemplateCondition condition = StringTemplateCondition.forValue(configStringTemplateCondition.getValue());
		
		if (termsInLine.isEmpty())
			return false;

		// determine how many literals are found in this line, and how many would be required to justify embedding operands
		// count all literals, including those that are already string templates |...| or |...{ ... }...|
		int literalCount = 0;
		for (Term term : termsInLine) {
			if (term.firstToken.isStringLiteral()) {
				++literalCount;
			}
		}

		// if configured, do not convert (and potentially merge) literals if there are no embeddings on the same line
		if (configRequireOperandsOnSameLine.getValue() && literalCount == termsInLine.size())
			return false;
		
		int literalCountRequiredToEmbed = 0;
		if (condition == StringTemplateCondition.SHORTER) 
			literalCountRequiredToEmbed = 2;
		else if (condition == StringTemplateCondition.SHORTER_OR_EQUAL) 
			literalCountRequiredToEmbed = 1;

		// return early if not even literals shall be converted to string templates
		if (literalCount < literalCountRequiredToEmbed && !configAlwaysConvertLiterals.getValue()) {
			return false;
		}

		// first, convert all literals to string templates
		boolean changedLiteral = false;
		for (Term term : termsInLine) {
			Token token = term.firstToken;
			// convert text string literals `...` and text field literals '...' to string templates |...|
			// exception: text elements 'text'(idf) must be embedded like other operands; they do NOT match Token.isTextFieldLiteral()
			if (term.isSingleStringLiteral() && (token.isTextFieldLiteral() || token.isTextStringLiteral())) { 
				String text = ABAP.unescapeCharLiteral(token.getText());
				// trailing blanks are removed from text field literals, i.e. ' abc ' will become | abc|
				if (token.isTextFieldLiteral())
					text = StringUtil.trimEnd(text);
				token.setText(ABAP.toStringTemplate(text), true);
				changedLiteral = true;
			}
		}
		if (literalCount < literalCountRequiredToEmbed) 
			return changedLiteral;

		// before embedding or joining, ensure that there is no (comment or) pragma between Terms and concatenation operators &&
		for (int index = 0; index < concatOpsInLine.size() - 1; ++index) { // the last operator is not relevant (and may be null)
			Token concatOp = concatOpsInLine.get(index);
			if (concatOp.getPrev().isPragmaOrComment() || concatOp.getNext().isPragmaOrComment()) {
				return changedLiteral;
			}
		}
		
		// embed operands and join string templates
		// this loop temporarily violates referential integrity by treating |{ operand }| as three siblings,  
		// although the operand must be a child of the opening |{ etc.; therefore, all integrity checks must be temporarily skipped 
		ArrayList<Term> embeddedTerms = new ArrayList<>();
		Token endOfLastTermInLine = termsInLine.get(termsInLine.size() - 1).lastToken;
		for (int index = 0; index < termsInLine.size(); ++index) {
			Term term = termsInLine.get(index);
			boolean isStringTemplate = term.firstToken.isStringTemplate(); 

			// adjust Tokens to the left of the Term to be embedded
			Token firstToken = term.firstToken;
			Term prevTerm = (index == 0) ? null : termsInLine.get(index - 1);
			boolean prevTermIsStringTemplate = (prevTerm != null && prevTerm.lastToken.isStringTemplate()); 
			if (isStringTemplate) {
				if (prevTermIsStringTemplate && canJoinStringTemplate(prevTerm.lastToken) && canJoinStringTemplate(firstToken)) {
					// join (the last Token of) the previous string template into the current one; note that |{ ... }{ ... }| is one single Term
					firstToken.copyWhitespaceFrom(prevTerm.lastToken);
					String prevText = prevTerm.lastToken.getText();
					firstToken.setText(prevText.substring(0, prevText.length() - 1) + firstToken.getText().substring(1), false);
					if (prevTerm.lastToken.closesLevel())
						firstToken.convertToEndEmbeddedExpression();
					concatOpsInLine.get(index - 1).removeFromCommand(true, true);
					prevTerm.lastToken.removeFromCommand(false, true);
				} 
				continue;
			}
			if (prevTerm == null) {
				// insert |{ before the Term
				firstToken.insertLeftSibling(Token.createForAbap(firstToken.lineBreaks, firstToken.spacesLeft, "|{", firstToken.sourceLineNum), true, endOfLastTermInLine, true);
				firstToken.setWhitespace();
			} else if (prevTermIsStringTemplate) {
				// change (the last Token of) the previous string template to embed this Term, and remove the &&
				prevTerm.lastToken.convertToStartEmbeddedExpression();
				concatOpsInLine.get(index - 1).removeFromCommand(true, true);
			} else {
				// insert }{ before the Term
				firstToken.insertLeftSibling(Token.createForAbap(firstToken.lineBreaks, firstToken.spacesLeft, "}{", firstToken.sourceLineNum), true, endOfLastTermInLine, true);
				firstToken.setWhitespace();
				concatOpsInLine.get(index - 1).removeFromCommand(true, true);
			}
			
			// adjust Tokens to the right of the Term to be embedded
			Token lastToken = term.lastToken;
			Term nextTerm = (index == termsInLine.size() - 1) ? null : termsInLine.get(index + 1);
			if (nextTerm == null) {
				// insert }| after the Term
				lastToken.insertRightSibling(Token.createForAbap(0, 1, "}|", lastToken.sourceLineNum), true, true);
			} else if (nextTerm.firstToken.isStringTemplate()) {
				// change the next string template to embed this Term, and remove the &&
				nextTerm.firstToken.convertToEndEmbeddedExpression();
				concatOpsInLine.get(index).removeFromCommand(true, true);
			} else {
				// this will be handled by the next Term, which must also be embedded and will insert a "}{" Token
			}
			embeddedTerms.add(term);
		}

		// the newly embedded Terms must now be moved from sibling level (which violates referential integrity) to child level
		// only after this, referential integrity is restored
		for (Term term : embeddedTerms) {
			Token newParent = term.firstToken.getPrevCodeToken();
			term.removeFromCommand(true);
			newParent.insertNext(term, true);
		}

		return true;
	}

	private boolean canJoinStringTemplate(Token template) {
		if (!configKeepControlCharsSeparate.getValue() || template.getTextLength() < 2)
			return true;
		String innerText = template.getText().substring(1, template.getTextLength() - 1);
		String unescapedText = StringUtil.getUnescapedText(innerText);
		boolean foundControlChar = false;
		for (char c : unescapedText.toCharArray()) {
			if (c == '\r' || c == '\n' || c == '\t') {
				foundControlChar = true;
			} else if (c == ' ') {
				// continue to see whether control chars are found: a string template that only consists of spaces will 
				// be joined, but spaces in combination with control chars will not
			} else {
				return true;
			}
		}
		return !foundControlChar;
	}
}
