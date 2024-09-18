package com.sap.adt.abapcleaner.rules.ddl.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rules.ddl.annotations.DdlAnnotationLayoutRule;

public class DdlSpacesAroundBracketsRule extends RuleForDdlCommands {
	public static final String displayName = "Standardize spaces around brackets";
	@Override
	public RuleID getID() { return RuleID.DDL_SPACES_AROUND_BRACKETS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_SPACES; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Standardizes spaces around brackets [...] and parentheses (...)."; }

	public String getHintsAndRestrictions() { return "Spaces in annotations are handled in '" + DdlAnnotationLayoutRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 11); }

	public RuleID[] getDependentRules() { 
		// the following rules reuse some of this rule's settings; it is therefore not strictly necessary, but more intuitive to put them further down the rule list
		return new RuleID[] { RuleID.DDL_ALIGN_LOGICAL_EXPRESSIONS, RuleID.DDL_ALIGN_SOURCE_PARAMETERS, RuleID.DDL_ALIGN_FUNCTION_PARAMETERS, RuleID.DDL_ALIGN_FUNCTION_PARAMETERS }; 
	}

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "// spaces in annotations are handled in 'Standardize annotation layout':"
				+ LINE_SEP + "@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }"
				+ LINE_SEP + ""
				+ LINE_SEP + "define view C_AnyView"
				+ LINE_SEP + "  as select from I_AnyView"
				+ LINE_SEP + ""
				+ LINE_SEP + "  // associations with cardinality"
				+ LINE_SEP + "  association[1..* ] to I_OtherView as _OtherAlias"
				+ LINE_SEP + "    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [ *]to I_ThirdView as _ThirdAlias"
				+ LINE_SEP + "    on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // path expressions"
				+ LINE_SEP + "      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,"
				+ LINE_SEP + "      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // built-in functions"
				+ LINE_SEP + "      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,"
				+ LINE_SEP + "      division(AnyArg *10, OtherArg, 2) as ThirdValue,"
				+ LINE_SEP + "      round( AnyValue / 100, 5 ) as RoundedValue,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // casts and ABAP types"
				+ LINE_SEP + "      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,"
				+ LINE_SEP + "      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // arithmetic expressions"
				+ LINE_SEP + "      ( AnyValue + OtherValue) as AnyValueField,"
				+ LINE_SEP + "      3 * (-2 * AnyValue - 4 * (OtherValue - 1) ) as OtherValueField"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<ChangeType> configSpacesAroundCardBrackets = new ConfigEnumValue<ChangeType>(this, "SpacesAroundCardBrackets", "Spaces around brackets for cardinality:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);
	final ConfigEnumValue<ChangeType> configSpacesInsideCardBrackets = new ConfigEnumValue<ChangeType>(this, "SpacesInsideCardBrackets", "Spaces inside brackets for cardinality:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	final ConfigEnumValue<ChangeType> configSpacesBeforePathBrackets = new ConfigEnumValue<ChangeType>(this, "SpacesBeforePathBrackets", "Spaces before brackets for path expressions:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);
	final ConfigEnumValue<ChangeType> configSpacesInsidePathBrackets = new ConfigEnumValue<ChangeType>(this, "SpacesInsidePathBrackets", "Spaces inside brackets for path expressions:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	final ConfigEnumValue<ChangeType> configSpacesBeforeFuncParens = new ConfigEnumValue<ChangeType>(this, "SpacesBeforeFuncParens", "Spaces before parentheses for functions:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);
	public final ConfigEnumValue<ChangeType> configSpacesInsideFuncParens = new ConfigEnumValue<ChangeType>(this, "SpacesInsideFuncParens", "Spaces inside parentheses for functions:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	final ConfigEnumValue<ChangeType> configSpacesBeforeTypeParens = new ConfigEnumValue<ChangeType>(this, "SpacesBeforeTypeParens", "Spaces before parentheses for ABAP types:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);
	final ConfigEnumValue<ChangeType> configSpacesInsideTypeParens = new ConfigEnumValue<ChangeType>(this, "SpacesInsideTypeParens", "Spaces inside parentheses for ABAP types:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	public final ConfigEnumValue<ChangeType> configSpacesInsideArithParens = new ConfigEnumValue<ChangeType>(this, "SpacesInsideArithParens", "Spaces inside parentheses in arithmetics:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	private final ConfigValue[] configValues = new ConfigValue[] { configSpacesAroundCardBrackets, configSpacesInsideCardBrackets, configSpacesBeforePathBrackets, configSpacesInsidePathBrackets, 
			configSpacesBeforeFuncParens, configSpacesInsideFuncParens, configSpacesBeforeTypeParens, configSpacesInsideTypeParens, configSpacesInsideArithParens };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ChangeType getSpacesInsideFuncParens() { return ChangeType.forValue(configSpacesInsideFuncParens.getValue()); }
	public ChangeType getSpacesInsideArithParens() { return ChangeType.forValue(configSpacesInsideArithParens.getValue()); }
	
	public DdlSpacesAroundBracketsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (command.isDdlAnnotation() || command.isCommentLine())
			return false;

		boolean changed = false;

		Token token = command.getFirstToken();
		
		while (token != null) {
			changed |= executeOn(command, token);
			token = token.getNext();
		}
		return changed;
	}

	private boolean executeOn(Command command, Token token) {
		if (token.isComment())
			return false;
		
		boolean changed = false;
		
		if (token.textEqualsAny(DDL.BRACKET_OPEN_STRING, DDL.BRACKET_CLOSE_STRING)) {
			// brackets [ ] for association/composition cardinality or path expressions

			ChangeType spacesAroundCardBrackets = ChangeType.forValue(configSpacesAroundCardBrackets.getValue());
			ChangeType spacesInsideCardBrackets = ChangeType.forValue(configSpacesInsideCardBrackets.getValue());
			ChangeType spacesBeforePathBrackets = ChangeType.forValue(configSpacesBeforePathBrackets.getValue());
			ChangeType spacesInsidePathBrackets = ChangeType.forValue(configSpacesInsidePathBrackets.getValue());

			// determine the Token before the bracket
			Token prevCode = token.getPrevCodeSibling();
			if (token.textEquals(DDL.BRACKET_CLOSE_STRING))
				prevCode = prevCode.getPrevCodeSibling();

			boolean isAssociation = (prevCode != null && prevCode.isAnyKeyword("ASSOCIATION", "COMPOSITION"));

			if (token.textEquals(DDL.BRACKET_OPEN_STRING)) {
				ChangeType changeBefore = isAssociation ? spacesAroundCardBrackets : spacesBeforePathBrackets;
				ChangeType changeAfter = isAssociation ? spacesInsideCardBrackets : spacesInsidePathBrackets;
				changed |= executeOnOpening(token, changeBefore, changeAfter);
				
			} else {
				ChangeType changeBefore = isAssociation ? spacesInsideCardBrackets : spacesInsidePathBrackets;
				ChangeType changeAfter = isAssociation ? spacesAroundCardBrackets : ChangeType.KEEP_AS_IS;
				changed |= executeOnClosing(token, changeBefore, changeAfter);
			}
			
		} else if (token.textEqualsAny(DDL.PARENS_OPEN_STRING, DDL.PARENS_CLOSE_STRING)) {
			// parentheses ( ) for functions or arithmetic expressions

			ChangeType spacesBeforeFuncParens = ChangeType.forValue(configSpacesBeforeFuncParens.getValue());
			ChangeType spacesInsideFuncParens = ChangeType.forValue(configSpacesInsideFuncParens.getValue());
			ChangeType spacesBeforeTypeParens = ChangeType.forValue(configSpacesBeforeTypeParens.getValue());
			ChangeType spacesInsideTypeParens = ChangeType.forValue(configSpacesInsideTypeParens.getValue());
			ChangeType spacesInsideArithParens = ChangeType.forValue(configSpacesInsideArithParens.getValue());

			// determine the Token before the parenthesis
			Token prevCode = token.getPrevCodeSibling();
			if (token.textEquals(DDL.PARENS_CLOSE_STRING))
				prevCode = prevCode.getPrevCodeSibling();

			// alternative to DDL.isAllowedFunctionName: prevToken.textEquals("cast") || prevToken.textEqualsAny(DDL.aggregationFunctions) || DDL.isBuiltInDdlFunction(prevToken.getText())
			boolean isAbapType = (prevCode != null && prevCode.textStartsWith(DDL.TYPED_LITERAL_PREFIX));
			boolean isFunction = (prevCode != null && !isAbapType && DDL.isAllowedFunctionName(prevCode.getText()));

			if (token.textEquals(DDL.PARENS_OPEN_STRING)) {
				ChangeType changeBefore = isAbapType ? spacesBeforeTypeParens : (isFunction ? spacesBeforeFuncParens : ChangeType.KEEP_AS_IS);
				ChangeType changeAfter = isAbapType ? spacesInsideTypeParens : (isFunction ? spacesInsideFuncParens : spacesInsideArithParens);
				changed |= executeOnOpening(token, changeBefore, changeAfter);
	
			} else {
				// spaces after ")" are deliberately not considered: for arithmetic expressions, this is handled by spaces 
				// around arithmetic operators (or by spaces inside the next closing parenthesis)
				ChangeType changeBefore = isAbapType ? spacesInsideTypeParens : (isFunction ? spacesInsideFuncParens : spacesInsideArithParens);
				ChangeType changeAfter = ChangeType.KEEP_AS_IS;
				changed |= executeOnClosing(token, changeBefore, changeAfter);
			}
		}
		
		return changed;
	}
	
	private boolean executeOnOpening(Token token, ChangeType changeBefore, ChangeType changeAfter) {
		boolean changed = false;
		
		// spaces before "[" or "("
		if (changeBefore != ChangeType.KEEP_AS_IS && token.lineBreaks == 0) {
			int spaces = (changeBefore == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0; 
			changed |= token.setSpacesLeftAdjustingIndent(spaces, true);
		}

		// spaces after "[" or "("
		Token next = token.getNext();
		if (changeAfter != ChangeType.KEEP_AS_IS && next == token.getFirstChild() && next.lineBreaks == 0 && !next.isComment()) {
			int spaces = (changeAfter == ChangeType.ALWAYS) ? Math.max(next.spacesLeft, 1) : 0;
			changed |= next.setSpacesLeftAdjustingIndent(spaces, true);
		}
		
		return changed;
	}

	private boolean executeOnClosing(Token token, ChangeType changeBefore, ChangeType changeAfter) {
		boolean changed = false;
		
		// spaces before "]" or ")"
		if (changeBefore != ChangeType.KEEP_AS_IS && token.lineBreaks == 0) {
			int spaces = (changeBefore == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0;
			changed |= token.setSpacesLeftAdjustingIndent(spaces, true);
		}
		
		// spaces after "]" (not for path expressions) or ")" (not used)
		Token next = token.getNext();
		if (changeAfter != ChangeType.KEEP_AS_IS && next != null && next.lineBreaks == 0 && !next.isComment()) {
			int spaces = (changeAfter == ChangeType.ALWAYS) ? Math.max(next.spacesLeft, 1) : 0;
			changed |= next.setSpacesLeftAdjustingIndent(spaces, true);
		}
		
		return changed;
	}
}
