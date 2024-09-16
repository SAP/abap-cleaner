package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.AlignStyle;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rulehelpers.LogicalExpression;
import com.sap.adt.abapcleaner.rulehelpers.TreeAlign;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;

public class DdlAlignLogicalExpressionsRule extends RuleForDdlCommands {
	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_LOGICAL_EXPRESSIONS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align logical expressions in views"; }

	@Override
	public String getDescription() { return "Aligns logical expressions in ON / WHERE / HAVING conditions and path expressions."; }

	@Override
	public String getHintsAndRestrictions() { return "Space inside parentheses is configured in '" + DdlSpacesAroundBracketsRule.displayName + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 14); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity I_AnyEntity"
				+ LINE_SEP + "  as select from I_AnySource as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_OtherSource as OtherAlias"
				+ LINE_SEP + "      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField"
				+ LINE_SEP + "         and ( OtherAlias.AnyType = 'X' or"
				+ LINE_SEP + "      OtherAlias.AnyType = 'Y' )"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [1..*] to I_ThirdSource  as _ThirdSource"
				+ LINE_SEP + "    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and"
				+ LINE_SEP + "    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField"
				+ LINE_SEP + "    and OtherAlias.NumericField >= _ThirdSource.NumericField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "  key OtherAlias.OtherKeyField,"
				+ LINE_SEP + "      OtherAlias.AnyType,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      case when AnyAlias.Category = 'A' then 'category A'"
				+ LINE_SEP + "           when AnyAlias.Category = 'B' or"
				+ LINE_SEP + "      AnyAlias.Category = 'C' then 'category B or C'"
				+ LINE_SEP + "      end as CategoryText,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      sum(OtherAlias.NumericField) as SumField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      max(_ThirdSource[1:SubCategory = 'X' or"
				+ LINE_SEP + "      SubCategory = 'Y'"
				+ LINE_SEP + "      or SubCategory = 'Z'].NumericField) as MaxNumericField"
				+ LINE_SEP + ""
				+ LINE_SEP + "}"
				+ LINE_SEP + "where"
				+ LINE_SEP + "OtherAlias.OtherKeyField > 'NN' and"
				+ LINE_SEP + "OtherAlias.NumericField > 100"
				+ LINE_SEP + "and ( AnyAlias.Category = 'A' or"
				+ LINE_SEP + "AnyAlias.Category = 'B'"
				+ LINE_SEP + "or AnyAlias.Category = 'C')"
				+ LINE_SEP + ""
				+ LINE_SEP + "group by AnyAlias.AnyKeyField,"
				+ LINE_SEP + "         OtherAlias.OtherKeyField,"
				+ LINE_SEP + "         OtherAlias.AnyType,"
				+ LINE_SEP + "         AnyAlias.Category"
				+ LINE_SEP + ""
				+ LINE_SEP + "having AnyAlias.Category = 'A' and"
				+ LINE_SEP + "  avg(OtherAlias.NumericField) >= 200 or"
				+ LINE_SEP + "    AnyAlias.Category = 'B'"
				+ LINE_SEP + "  and sum(OtherAlias.NumericField) >= 1000 and"
				+ LINE_SEP + "sum(OtherAlias.NumericField) < 5000";
	}
	
	private static final String[] alignStyleSelectionLeftOnly = new String[] { "do not align", "left-align" }; // for IF (because this keyword is too short to be right-aligned with "AND" or "EQUIV")
	private static final String[] alignStyleSelection = new String[] { "do not align", "left-align", "right-align" };

	final ConfigBoolValue configAlignJoinOn = new ConfigBoolValue(this, "AlignJoinOn", "Align ON condition in JOINs", true);
	final ConfigBoolValue configAlignAssociationOn = new ConfigBoolValue(this, "AlignAssociationOn", "Align ON conditions in ASSOCIATIONs", true);
	final ConfigBoolValue configAlignWhen = new ConfigBoolValue(this, "AlignWhen", "Align WHEN condition in complex CASE distinctions", true);
	final ConfigBoolValue configAlignPathExpressions = new ConfigBoolValue(this, "AlignPathExpressions", "Align path expressions", true);
	final ConfigBoolValue configAlignWhere = new ConfigBoolValue(this, "AlignWhere", "Align WHERE clause", true);
	final ConfigBoolValue configAlignHaving = new ConfigBoolValue(this, "AlignHaving", "Align HAVING clause", true);
	
	final ConfigEnumValue<AlignStyle> configAlignOnWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignOnWithBoolOps", "Align AND / OR with ON", alignStyleSelectionLeftOnly, AlignStyle.values(), AlignStyle.LEFT_ALIGN);
	final ConfigEnumValue<AlignStyle> configAlignFilterWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignFilterWithBoolOps", "Align AND / OR with FILTER", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN);
	final ConfigEnumValue<AlignStyle> configAlignWhenWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignWhenWithBoolOps", "Align AND / OR with WHEN", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN);
	final ConfigEnumValue<AlignStyle> configAlignWhereWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignWhereWithBoolOps", "Align AND / OR with WHERE", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN);
	final ConfigEnumValue<AlignStyle> configAlignHavingWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignHavingWithBoolOps", "Align AND / OR with HAVING", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN);

	final ConfigBoolValue configRightAlignComparisonOps = new ConfigBoolValue(this, "RightAlignComparisonOps", "Right-align comparison operators / IS", true);
	final ConfigIntValue configMaxInnerSpaces = new ConfigIntValue(this, "MaxInnerSpaces", "Do not align if more than", "inner spaces would be required", 1, 20, 999);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignJoinOn, configAlignAssociationOn, configAlignWhen, configAlignPathExpressions, configAlignWhere, configAlignHaving,
			configAlignOnWithBoolOps, configAlignFilterWithBoolOps, configAlignWhenWithBoolOps, configAlignWhereWithBoolOps, configAlignHavingWithBoolOps, 
			configRightAlignComparisonOps, configMaxInnerSpaces };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignLogicalExpressionsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	private ChangeType getSpaceInsideParens() { 
		return ((DdlSpacesAroundBracketsRule)parentProfile.getRule(RuleID.DDL_SPACES_AROUND_BRACKETS)).getSpacesInsideArithParens();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
      boolean changed = false;
      
      Token firstToken = command.getFirstToken();
      Token token = firstToken;
      do {
      	// skip anything inside of "[DEFINE] HIERARCHY ... AS PARENT CHILD HIERARCHY( ... )", this is not yet supported:
      	// (conditions inside are: [DIRECTORY _directory_assoc FILTER BY cds_cond] and [START WHERE cds_cond])
      	if (token.isKeyword("HIERARCHY") && token.getNextCodeToken() != null && token.getNextCodeToken().getOpensLevel()) {
      		token = token.getNextCodeToken().getNextSibling();
      		continue;
      	}
      	
      	// does this Token start a logical expression? If so, get the Token following the logical expression  
      	Token lastInLogExpr = token.getLastTokenOfLogicalExpression();
      	if (lastInLogExpr != null) {
      		if (canAlignExpression(firstToken, token)) {
	      		preprocessLogicalExpression(code, command, token, lastInLogExpr);
	            if (alignLogicalExpressions(code, command, token, lastInLogExpr)) {
	               changed = true;
	            }
      		}
            token = lastInLogExpr.getNextCodeToken();
      	} else {
            token = token.getNextCodeToken();
      	}
      } while (token != null);
      return changed;
	}

	private void preprocessLogicalExpression(Code code, Command command, Token keyword, Token lastInLogExpr) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		Token token = keyword;
		
		Token tokenAfterKeyword = token.getNext();
		if (keyword.isAnyKeyword("WHERE", "HAVING") && !tokenAfterKeyword.isComment() && tokenAfterKeyword.lineBreaks > 0) {
			tokenAfterKeyword.setWhitespace();
			changed = true;
		}
		while (token != null) {
			Token prev = token.getPrev();
			Token next = token.getNext();
			if (token.isAnyKeyword("AND", "OR") && next.lineBreaks > 0) {
				if (!token.isFirstTokenInLine())
					token.copyWhitespaceFrom(next);
				next.setWhitespace();
				changed = true;
			} else if (token.textEquals("(") && next.lineBreaks > 0 && !next.isComment()) {
				next.setWhitespace();
				changed = true;
			} else if (token.textEquals(")") && token.isFirstTokenInLine() && !prev.isComment()) {
				if (next != null && next.lineBreaks == 0)
					next.copyWhitespaceFrom(token);
				token.setWhitespace();
				changed = true;
			}
			if (token == lastInLogExpr)
				break;
			token = token.getNext();
		}
		if (changed) {
			code.addRuleUse(this, command);
		}
	}
	
	private boolean alignLogicalExpressions(Code code, Command command, Token keyword, Token lastInLogExpr) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		try {
			Token logExpStart = keyword.getNextCodeToken();
			LogicalExpression logicalExpression = LogicalExpression.create(logExpStart, lastInLogExpr);
			if (!logicalExpression.isSupported()) 
				return false;

			AlignStyle alignStyle = getAlignStyle(keyword);
			boolean rightAlignComparisonOps = configRightAlignComparisonOps.getValue();
			int maxInnerSpaces = configMaxInnerSpaces.getValue();

			boolean attachParentheses = (getSpaceInsideParens() == ChangeType.NEVER);
			TreeAlign treeAlign = TreeAlign.createFrom(logicalExpression);
			changed = treeAlign.align(keyword, alignStyle, rightAlignComparisonOps, true, false, maxInnerSpaces, attachParentheses); 
			
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
		}
		return changed;
	}

	private boolean canAlignExpression(Token firstToken, Token token) {
		// cp. getAlignStyle() and possible keywords in Token.getLastTokenOfDdlLogicalExpression()
		if (token.isAnyKeyword("ON", "FILTER")) {
			if (firstToken.startsDdlJoin()) {
				return configAlignJoinOn.getValue();
			} else { // firstToken.startsDdlAssociation()
				return configAlignAssociationOn.getValue();
			}

		} else if (token.isKeyword("WHEN")) {
			return configAlignWhen.getValue();

		} else if (token.isKeyword("WHERE")) {
			return configAlignWhere.getValue();
		
		} else if (token.isKeyword("HAVING")) {
			return configAlignHaving.getValue();

		} else if (token.getParent() != null && token.getParent().textEquals(DDL.BRACKET_OPEN_STRING)) {
			return configAlignPathExpressions.getValue();
		}

		return true; // pro forma
	}
	
	private AlignStyle getAlignStyle(Token keyword) {
		// cp. canAlignExpression() and possible keywords in Token.getLastTokenOfDdlLogicalExpression() 
		if (keyword.isKeyword("ON")) { // both for associations and joins
			return AlignStyle.forValue(configAlignOnWithBoolOps.getValue());
		
		} else if (keyword.isKeyword("FILTER")) { // "ASSOCIATION ... WITH DEFAULT FILTER"
			return AlignStyle.forValue(configAlignFilterWithBoolOps.getValue());
		
		} else if (keyword.isKeyword("WHEN")) {
			return AlignStyle.forValue(configAlignWhenWithBoolOps.getValue());
		
		}  else if (keyword.isKeyword("WHERE")) {
			return AlignStyle.forValue(configAlignWhereWithBoolOps.getValue());
		
		} else if (keyword.isKeyword("HAVING")) {
			return AlignStyle.forValue(configAlignHavingWithBoolOps.getValue());
		
		} else if (keyword.getParent() != null && keyword.getParent().textEquals(DDL.BRACKET_OPEN_STRING)) {
			// path expressions
			return AlignStyle.DO_NOT_ALIGN;
		
		} else { // pro forma
			return AlignStyle.DO_NOT_ALIGN;
		}
	}
}
