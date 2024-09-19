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

public class DdlSpacesAroundSignsRule extends RuleForDdlCommands {
	public static final String displayName = "Standardize spaces around colon, comma etc.";
	
	@Override
	public RuleID getID() { return RuleID.DDL_SPACES_AROUND_SIGNS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_SPACES; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Standardizes spaces around comment signs, colons, commas and arithmetic operators."; }

	public String getHintsAndRestrictions() { return "Spaces in annotations are handled in '" + DdlAnnotationLayoutRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 10); }

	public RuleID[] getDependentRules() { 
		// the following rules reuse some of this rule's settings; it is therefore not strictly necessary, but more intuitive to put them further down the rule list
		return new RuleID[] { RuleID.DDL_ALIGN_ENTITY_PARAMETERS, RuleID.DDL_ALIGN_SOURCE_PARAMETERS }; 
	}

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "// colons and commas in annotations are not changed by this rule"
				+ LINE_SEP + "// but handled in 'Standardize annotation layout':"
				+ LINE_SEP + "@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }"
				+ LINE_SEP + ""
				+ LINE_SEP + "define view C_AnyView"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    P_AnyParam  : any_type    ,"
				+ LINE_SEP + "    P_OtherParam:other_type"
				+ LINE_SEP + ""
				+ LINE_SEP + "  --colons that start parameter names :P_Any must not be detached"
				+ LINE_SEP + "  as select from I_AnyView("
				+ LINE_SEP + "    P_AnyParam  ::P_AnyParam ,"
				+ LINE_SEP + "    P_OtherParam: :P_OtherParam ) as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "  --no spaces can be put around the * that specifies cardinality"
				+ LINE_SEP + "  association [1..*] to I_OtherView as _OtherAlias"
				+ LINE_SEP + "    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyKeyField   ,    //any comment"
				+ LINE_SEP + "  key OtherKeyField ,-- other comment"
				+ LINE_SEP + ""
				+ LINE_SEP + "      //arithmetic expressions"
				+ LINE_SEP + "      AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,"
				+ LINE_SEP + "      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      -- built-in functions"
				+ LINE_SEP + "      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,"
				+ LINE_SEP + "      division(AnyArg *10,OtherArg,2) as ThirdValue,"
				+ LINE_SEP + "      round(AnyValue* 100, 5) as RoundedValue,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      //you may format commas in ABAP types in a different way,"
				+ LINE_SEP + "      // because these are not built-in functions"
				+ LINE_SEP + "      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,"
				+ LINE_SEP + "      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,"
				+ LINE_SEP + "      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<ChangeType> configSpaceBeforeCommentSign = new ConfigEnumValue<ChangeType>(this, "SpaceBeforeCommentSign", "Space before comment sign:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);
	final ConfigEnumValue<ChangeType> configSpaceAfterCommentSign = new ConfigEnumValue<ChangeType>(this, "SpaceAfterCommentSign", "Space after comment sign:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);

	public final ConfigEnumValue<ChangeType> configSpaceBeforeColon = new ConfigEnumValue<ChangeType>(this, "SpaceBeforeColon", "Space before colon:", changeTypeSelection, ChangeType.values(), ChangeType.KEEP_AS_IS);
	public final ConfigEnumValue<ChangeType> configSpaceAfterColon = new ConfigEnumValue<ChangeType>(this, "SpaceAfterColon", "Space after colon:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);

	final ConfigEnumValue<ChangeType> configSpaceBeforeComma = new ConfigEnumValue<ChangeType>(this, "SpaceBeforeComma", "Space before comma:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);
	final ConfigEnumValue<ChangeType> configSpaceAfterComma = new ConfigEnumValue<ChangeType>(this, "SpaceAfterComma", "Space after comma:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);
	final ConfigEnumValue<ChangeType> configSpaceAfterCommaInAbapType = new ConfigEnumValue<ChangeType>(this, "SpaceAfterCommaInAbapType", "Space after comma in ABAP type:", changeTypeSelection, ChangeType.values(), ChangeType.NEVER);

	final ConfigEnumValue<ChangeType> configSpaceAroundArithmeticOps = new ConfigEnumValue<ChangeType>(this, "SpaceAroundArithmeticOps", "Spaces around arithmetic operators:", changeTypeSelection, ChangeType.values(), ChangeType.ALWAYS);

	private final ConfigValue[] configValues = new ConfigValue[] { configSpaceBeforeCommentSign, configSpaceAfterCommentSign, configSpaceBeforeColon, configSpaceAfterColon, configSpaceBeforeComma, configSpaceAfterComma, configSpaceAfterCommaInAbapType, configSpaceAroundArithmeticOps };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ChangeType getSpaceBeforeColon() { return ChangeType.forValue(configSpaceBeforeColon.getValue()); }
	public ChangeType getSpaceAfterColon() { return ChangeType.forValue(configSpaceAfterColon.getValue()); }
	
	public DdlSpacesAroundSignsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (command.isDdlAnnotation())
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
		final int commentSignLength = DDL.LINE_END_COMMENT.length(); // same length as DDL.LINE_END_MINUS_COMMENT

		ChangeType spaceBeforeCommentSign = ChangeType.forValue(configSpaceBeforeCommentSign.getValue());
		ChangeType spaceAfterCommentSign = ChangeType.forValue(configSpaceAfterCommentSign.getValue());
		
		ChangeType spaceBeforeColon = ChangeType.forValue(configSpaceBeforeColon.getValue());
		ChangeType spaceAfterColon = ChangeType.forValue(configSpaceAfterColon.getValue());
		
		ChangeType spaceBeforeComma = ChangeType.forValue(configSpaceBeforeComma.getValue());
		ChangeType spaceAfterComma = ChangeType.forValue(configSpaceAfterComma.getValue());
		ChangeType spaceAfterCommaInAbapType = ChangeType.forValue(configSpaceAfterCommaInAbapType.getValue());
		
		ChangeType spaceAroundArithmeticOps = ChangeType.forValue(configSpaceAroundArithmeticOps.getValue());

		boolean changed = false;
		
		// space before or after line-end comments
		if (token.isDdlLineEndComment()) {
			// space before line-end comment (// or --)
			if (spaceBeforeCommentSign != ChangeType.KEEP_AS_IS && token.lineBreaks == 0 && command.getPrev() != null) {
				int newSpacesLeft = (spaceBeforeCommentSign == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0;
				changed |= token.setWhitespace(token.lineBreaks, newSpacesLeft);
			}
			
			// space after line-end comment sign (// or --, except for ///... or ---...)
			if ((token.lineBreaks > 0 || command.getPrev() == null) && token.spacesLeft == 0 && token.textStartsWith(DDL.LINE_END_COMMENT)) {
				// skip cases of "//..." at position 0, because those are often created by commenting out a section of code
			} else if (spaceAfterCommentSign != ChangeType.KEEP_AS_IS && token.getTextLength() > commentSignLength) {
				String text = token.getText();
				char nextChar = text.charAt(commentSignLength); 
				if (spaceAfterCommentSign == ChangeType.ALWAYS && nextChar != ' ' && nextChar != token.getText().charAt(0)) {
					token.setText(text.substring(0, commentSignLength) + " " + text.substring(commentSignLength), false);
					changed = true;
				} else if (spaceAfterCommentSign == ChangeType.NEVER && nextChar == ' ') {
					token.setText(text.substring(0, commentSignLength) + text.substring(commentSignLength).stripLeading(), false);
					changed = true;
				}
			}
		}
		
		// the remaining changes do not apply to tokens at line start or to comments (note that in a multi-line comment, even "," and ":" could be comment texts)
		if (token.isComment() || token.lineBreaks > 0) 
			return changed;
		
		// space before colon
		if (spaceBeforeColon != ChangeType.KEEP_AS_IS && token.textEquals(":")) {
			if (token.getParent() != null && token.getParent().textEndsWith(DDL.BRACKET_OPEN_STRING)) {
				// ignore colons in path expressions such as "_Any[*:...]", because a space between "*:", "1:" etc. would be a syntax error
			} else {
				int spaces = (spaceBeforeColon == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0;
				changed |= token.setSpacesLeftAdjustingIndent(spaces, false);
			}
		}
		
		// space before comma 
		if (spaceBeforeComma != ChangeType.KEEP_AS_IS && token.textEqualsAny(",")) {
			int spaces = (spaceBeforeComma == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0;
			changed |= token.setSpacesLeftAdjustingIndent(spaces, false);
		}
		
		// space before arithmetic operators (+ - * /)
		Token prevToken = token.getPrev();
		if (prevToken == null && command.getPrev() != null) {
			// for list elements, commas belong to the previous Command 
			prevToken = command.getPrev().getLastToken();
		}
		Token nextToken = token.getNext();
		Token prevCode = token.getPrevCodeToken();
		if (spaceAroundArithmeticOps != ChangeType.KEEP_AS_IS && token.textEqualsAny(DDL.arithmeticOperators) && prevCode != null && prevCode.mayBeFollowedByArithmeticOp()) {
			if (token.textEquals("*") && token.isChildOfAny("ASSOCIATION", "COMPOSITION")) {
				// ignore "association [1..*]", "composition [0..*]" etc., because a space between .. and * is a syntax error
			} else if (token.textEquals("*") && prevToken != null && prevToken.textEndsWith(".")) { // '!= null' pro forma
				// ignore "$extension.*"
			} else if (token.textEquals("*") && token.getParent() != null && token.getParent().textEndsWith(DDL.BRACKET_OPEN_STRING)) { 
				// ignore path expression "_Any[*:...]"
			} else if (token.textEquals("*") && prevToken != null && prevToken.textEndsWith("(") && nextToken.textStartsWith(")")) { // '!= null' pro forma
				// ignore "count(*)"
			} else if (token.textEquals(DDL.divisionOperator) && spaceAroundArithmeticOps == ChangeType.NEVER) {
				// Spaces around "/" are NOT removed, because attaching "/" to an identifier would be a syntax error, since "/" can be used  
				// for namespaces: /ANY/Identifier. While "2/" is not a syntax error, it still causes red highlighting in ADT and should therefore be avoided, too. 
			} else {
				int spaces = (spaceAroundArithmeticOps == ChangeType.ALWAYS) ? Math.max(token.spacesLeft, 1) : 0;
				changed |= token.setSpacesLeftAdjustingIndent(spaces, true);
			}
		}
		
		// space after colon, comma, or arithmetic operator (which is now the prevToken!)
		ChangeType changeType = ChangeType.KEEP_AS_IS;
		if (prevToken == null) { 
			changeType = ChangeType.KEEP_AS_IS;

		} else if (prevToken.textEquals(DDL.COLON_SIGN_STRING)) {
			changeType = spaceAfterColon;
		
		} else if (prevToken.textEquals(DDL.COMMA_SIGN_STRING)) {
			Token parent = token.getParent();
			Token parentPrev = (parent == null) ? null : parent.getPrevCodeToken();
			changeType = (parentPrev != null && parentPrev.textStartsWith(DDL.TYPED_LITERAL_PREFIX)) ? spaceAfterCommaInAbapType : spaceAfterComma;
		
		} else if (prevToken.textEqualsAny(DDL.arithmeticOperators)) {
			Token prevPrevCode = prevToken.getPrevCodeToken();
			if (prevPrevCode != null && prevPrevCode.mayBeFollowedByArithmeticOp()) {
				changeType = spaceAroundArithmeticOps;
			}
		}
		
		if (changeType != ChangeType.KEEP_AS_IS) {
			if (prevToken.textEquals("*") && token.textEquals(DDL.BRACKET_CLOSE_STRING)) {
				// ignore association cardinality as in "[1..*]", even though "[1..* ]" would be syntactically okay
			} else if (prevToken.textEquals("*") && token.textEquals(DDL.COLON_SIGN_STRING)) {
				// ignore cardinality in path expression "_Any[*: ...]", because a space in "*:" would be a syntax error
			} else if (prevToken.textEquals("*") && token.textEquals(DDL.PARENS_CLOSE_STRING)) {
				// ignore star in parenthesis as in "(*)"
			} else if (prevToken.isDdlParameterColon()) {
				// ignore colon that starts a parameter name such as ":P_Any"
			} else if (prevToken.textEquals(DDL.divisionOperator) && changeType == ChangeType.NEVER) {
				// spaces around "/" are not removed, because attaching "/" to an identifier is a syntax error, since "/" can be used for namespaces: /ANY/Identifier
				// while "2/" is not a syntax error, it still causes red highlighting in ADT and should therefore be avoided, too 
			} else {
				int spaces = (changeType == ChangeType.ALWAYS) ? 1 : 0;
				changed |= token.setSpacesLeftAdjustingIndent(spaces, true);
			}
		}
		return changed;
	}
}
