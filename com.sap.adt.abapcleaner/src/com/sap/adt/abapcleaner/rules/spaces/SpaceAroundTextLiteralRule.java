package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class SpaceAroundTextLiteralRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.SPACES_IN_EMPTY_BRACKETS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Put spaces around text literals"; }

	@Override
	public String getDescription() { return "Adds missing spaces before and after text field literals '...' and text string literals `...`."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 5, 30); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD space_around_text_literal." 
			+ LINE_SEP + "    DATA lv_any TYPE string VALUE`abc`." // compilable error at `abc`
			+ LINE_SEP + "    DATA lv_other TYPE string VALUE'abc'." 
			+ LINE_SEP  
			+ LINE_SEP + "    ASSERT lv_any EQ`abc`."  // compilable error at `abc`
			+ LINE_SEP + "    ASSERT lv_any EQ'abc'." 
			+ LINE_SEP  
			+ LINE_SEP + "    lv_any =`abc` &&`def`."  // compilable error at `abc` (and `def`)
			+ LINE_SEP + "    lv_any ='abc' &&'def'." 
			+ LINE_SEP + "    lv_any =:`abc`,`def`." 
			+ LINE_SEP + "    lv_any =:'abc','def'." 
			+ LINE_SEP  
			+ LINE_SEP + "    lv_any =`abc`\"comment" // compilable error at `abc` (and `def`)
			+ LINE_SEP + "          &&`def`." 
			+ LINE_SEP + "    lv_any ='abc'\"comment" 
			+ LINE_SEP + "          &&'def'." 
			+ LINE_SEP  
			+ LINE_SEP + "    \" these cases are syntactically correct even without spaces:" 
			+ LINE_SEP + "    any_method('text field literal')." 
			+ LINE_SEP + "    any_method('other literal' )." 
			+ LINE_SEP + "    any_method( 'third literal')." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_any_table = VALUE #( ( '111')" 
			+ LINE_SEP + "                            ( '222' )" 
			+ LINE_SEP + "                            ( '333') )." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_value = lt_other_table[ name = 'abc']-value." 
			+ LINE_SEP + "    lt_any[`1` ]-num =`a`."  // compilable error at `a`
			+ LINE_SEP + "    lt_any['1' ]-num ='a'." 
			+ LINE_SEP  
			+ LINE_SEP + "    lt_amount = VALUE #( ( CONV #( '11.11') )" 
			+ LINE_SEP + "                         ( CONV #('22.22' ) )" 
			+ LINE_SEP + "                         ( CONV #( '33.33') ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_amount = VALUE #( ( CONV #('11.11') )" 
			+ LINE_SEP + "                         ( CONV #('22.22') )" 
			+ LINE_SEP + "                         ( CONV #('33.33') ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the first two cases are executable but create a syntax warning without a space after the opening (" 
			+ LINE_SEP + "    other_method(`text string literal`)." 
			+ LINE_SEP + "    other_method(`other literal` )." 
			+ LINE_SEP + "    other_method( `third literal`)." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_other_table = VALUE #( ( text = `abc` )" 
			+ LINE_SEP + "                              ( text = `def`)" 
			+ LINE_SEP + "                              ( text = `ghi`) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" introducing spaces here would be a syntax error:"
			+ LINE_SEP + "    CALL METHOD lo_instance->('METHOD_NAME')." 
			+ LINE_SEP + "    CALL METHOD ('CLASS_NAME')=>('METHOD_NAME')." 
			+ LINE_SEP + "    ls_struc-('COMPONENT_NAME') = 1." 
			+ LINE_SEP + "    lr_data_ref->('COMPONENT_NAME') = 1." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	// old setting (from when this rule was called SpacesInEmptyBracketsRule), now moved to RuleID.NEEDLESS_SPACES, option 'ProcessEmptyBrackets' (see Profile.load())
	public final ConfigBoolValue configRemoveMultiSpaceIfEmpty_MOVED = new ConfigBoolValue(this, "RemoveMultiSpaceIfEmpty", "Remove multiple spaces from empty parentheses", true);

	final ConfigBoolValue configSeparateFromKeywords = new ConfigBoolValue(this, "SeparateFromKeywords", "Add space between keywords and text literals", true, false, LocalDate.of(2023, 10, 31));
	final ConfigBoolValue configSeparateFromOperators = new ConfigBoolValue(this, "SeparateFromOperators", "Add space between operators and text literals", true, false, LocalDate.of(2023, 10, 31));
	final ConfigBoolValue configSeparateFromComments = new ConfigBoolValue(this, "SeparateFromComments", "Add space between text literals and comments", true, false, LocalDate.of(2023, 10, 31));
	final ConfigBoolValue configSeparateFromBrackets = new ConfigBoolValue(this, "SeparateFromCharLiterals", "Add space between parentheses and character literals", true, false, LocalDate.of(2023, 5, 30));
	final ConfigBoolValue configSeparateFromBracketPairs = new ConfigBoolValue(this, "SeparateCondensedCases", "Add space in condensed cases with single character literal: ...('...')", true, false, LocalDate.of(2023, 5, 30));

	private final ConfigValue[] configValues = new ConfigValue[] { configSeparateFromKeywords, configSeparateFromOperators, configSeparateFromComments, configSeparateFromBrackets, configSeparateFromBracketPairs };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public SpaceAroundTextLiteralRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) {
		if (!token.isCharacterLiteral())
			return false;
		
		boolean changed = false;
		Token prev = token.getPrev();
		if (prev != null && token.isAttached()) {
			boolean detachFromPrev = false;
			if (prev.isSqlLiteralType()) {
				detachFromPrev = false;

			} else if (prev.getOpensLevel() && prev.hasChildren()) {
				if (textLiteralMustRemainAttached(token, prev)) {
					// do NOT execute the section on the next Token below, either
					return false;
				}
				detachFromPrev = configSeparateFromBrackets.getValue();

			} else if (prev.isKeyword() && prev.textEquals("&&")) {
				// RND Parser categorizes && as a keyword, but it is more intuitive to use the ...Operators option
				detachFromPrev = configSeparateFromOperators.getValue();
			
			} else if (prev.isKeyword() || prev.isTextualComparisonOp()) {
				detachFromPrev = configSeparateFromKeywords.getValue();
			
			} else if (prev.type == TokenType.ASSIGNMENT_OP || prev.type == TokenType.OTHER_OP
					|| prev.type == TokenType.COMMA || prev.type == TokenType.COLON
					|| prev.type == TokenType.COMPARISON_OP && !prev.isTextualComparisonOp()) {
				detachFromPrev = configSeparateFromOperators.getValue();
			}
			if (detachFromPrev) {
				int startIndex = token.getStartIndexInLine();
				token.spacesLeft = 1;
				command.addIndent(1, startIndex, token, null, true);
				changed = true;
			}
		}

		Token next = token.getNext();
		if (next != null && next.isAttached()) {
			// missing spaces between text literals and the next Token is a syntax error in most of the above cases;
			// only comments and closing parentheses can be attached (note that we already returned from the method in 
			// cases checked by textLiteralMustRemainAttached()
			boolean detachFromNext = false;
			if (next.isComment()) {
				detachFromNext = configSeparateFromComments.getValue();
			} else if (next.closesLevel() && next.textStartsWith(")")) {
				detachFromNext = configSeparateFromBrackets.getValue();
			}
			if (detachFromNext) {
				int startIndex = next.getStartIndexInLine();
				next.spacesLeft = 1;
				command.addIndent(1, startIndex, next, null, true);
				changed = true;
			}
		}
		
		return changed;
	}
	
	private boolean textLiteralMustRemainAttached(Token token, Token openingBracket) {
		// exclude cases of ('...') where introducing spaces could be a syntax error, as in 
		// - CALL METHOD ('METHOD_NAME'), CALL METHOD lo_instance->('METHOD_NAME'),
		// - ASSIGN ('(FUNC_GRP_NAME)TABLE[]') TO <lt_table>.
		// - ls_struc-('COMPONENT_NAME') = ..., lt_table[ 1 ]-('COMPONENT_NAME') = ...
		// - lr_data_ref->('COMPONENT_NAME') = ..., <ls_struc>-('COMPONENT_NAME') = ...
		// harmless cases such as CONV #('12.34') can be kept optionally (configKeepCondensedCases)
		Token closingBracket = openingBracket.getNextSibling();
		if (token.getNext() == closingBracket && closingBracket.isAttached()) {
			if (openingBracket.textEqualsAny("(", "[") || openingBracket.textEndsWithAny("->(", "=>(", "-(")) {
				return true;
			} else if (!configSeparateFromBracketPairs.getValue()) {
				return true;
			}
		}
		return false;
	}
}
