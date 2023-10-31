package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

/**
 * Removes multiple spaces from empty parentheses and adds missing spaces between parentheses and character literals.
 */
public class SpaceAroundTextLiteralRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Condense your code", "#condense-your-code") };

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
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD space_around_text_literal." 
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
			+ LINE_SEP 
			+ LINE_SEP + "    lt_amount = VALUE #( ( CONV #( '11.11') )" 
			+ LINE_SEP + "                         ( CONV #('22.22' ) )" 
			+ LINE_SEP + "                         ( CONV #( '33.33') ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_amount = VALUE #( ( CONV #('11.11') )" 
			+ LINE_SEP + "                         ( CONV #('22.22') )" 
			+ LINE_SEP + "                         ( CONV #('33.33') ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" introducing spaces here would be a syntax error:"
			+ LINE_SEP + "    CALL METHOD lo_instance->('METHOD_NAME')." 
			+ LINE_SEP + "    CALL METHOD ('CLASS_NAME')=>('METHOD_NAME')." 
			+ LINE_SEP + "    ls_struc-('COMPONENT_NAME') = 1." 
			+ LINE_SEP + "    lr_data_ref->('COMPONENT_NAME') = 1." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the first two cases are executable but create a syntax warning without a space after the opening (" 
			+ LINE_SEP + "    other_method(`text string literal`)." 
			+ LINE_SEP + "    other_method(`other literal` )." 
			+ LINE_SEP + "    other_method( `third literal`)." 
			+ LINE_SEP 
			+ LINE_SEP + "    lt_other_table = VALUE #( ( text = `abc` )" 
			+ LINE_SEP + "                              ( text = `def`)" 
			+ LINE_SEP + "                              ( text = `ghi`) )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	// old setting (from when this rule was called SpacesInEmptyBracketsRule), now moved to RuleID.NEEDLESS_SPACES, option 'ProcessEmptyBrackets' (see Profile.load())
	public final ConfigBoolValue configRemoveMultiSpaceIfEmpty_MOVED = new ConfigBoolValue(this, "RemoveMultiSpaceIfEmpty", "Remove multiple spaces from empty parentheses", true);

	final ConfigBoolValue configSeparateFromCharLiterals = new ConfigBoolValue(this, "SeparateFromCharLiterals", "Add space between parentheses and character literals", true, false, LocalDate.of(2023, 5, 30));
	final ConfigBoolValue configSeparateCondensedCases = new ConfigBoolValue(this, "SeparateCondensedCases", "Add space in condensed cases with single character literal: ...('...')", true, false, LocalDate.of(2023, 5, 30));

	private final ConfigValue[] configValues = new ConfigValue[] { configSeparateFromCharLiterals, configSeparateCondensedCases };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public SpaceAroundTextLiteralRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) {
		if (!token.getOpensLevel() || token.isLiteral()) 
			return false;
		
		boolean changed = false;
		Token next = token.getNext();
		Token closingBracket = token.getNextSibling();
		if (token.hasChildren() && configSeparateFromCharLiterals.getValue()) {
			// exclude cases of ('...') where introducing spaces could be a syntax error, as in 
			// - CALL METHOD ('METHOD_NAME'), CALL METHOD lo_instance->('METHOD_NAME'),
			// - ASSIGN ('(FUNC_GRP_NAME)TABLE[]') TO <lt_table>.
			// - ls_struc-('COMPONENT_NAME') = ..., lt_table[ 1 ]-('COMPONENT_NAME') = ...
			// - lr_data_ref->('COMPONENT_NAME') = ..., <ls_struc>-('COMPONENT_NAME') = ...
			// harmless cases such as CONV #('12.34') can be kept optionally (configKeepCondensedCases)
			if (next.isAttached() && next.getNext() == closingBracket && closingBracket.isAttached()) {
				if (!configSeparateCondensedCases.getValue() || token.textEqualsAny("(", "[") || token.textEndsWith("->(") || token.textEndsWith("=>(") || token.textEndsWith("-(")) {
					return false;
				}
			}
			
			if (next.isCharacterLiteral() && next.isAttached()) {
				// add space between opening parenthesis and character literal: ('...' ) or (`...` )
				int startIndex = next.getStartIndexInLine();
				next.spacesLeft = 1;
				command.addIndent(1, startIndex, next, null, true);
				changed = true;
			} // do NOT attach the next section with 'else if'!
			
			if (token.getLastChild().isCharacterLiteral() && closingBracket.isAttached()) {
				// add space between character literal and closing parenthesis: ( '...') or ( `...`)
				int startIndex = closingBracket.getStartIndexInLine();
				closingBracket.spacesLeft = 1;
				command.addIndent(1, startIndex, closingBracket, null, true);
				changed = true;
			}
		}
		return changed;
	}
}
