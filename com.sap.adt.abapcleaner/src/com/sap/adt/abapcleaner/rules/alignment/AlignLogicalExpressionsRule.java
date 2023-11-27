package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

public class AlignLogicalExpressionsRule extends RuleForLogicalExpressions {
	public static final String DEFAULT_NAME = "Align logical expressions";

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_LOGICAL_EXPRESSIONS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return DEFAULT_NAME; }

	@Override
	public String getDescription() { return "Aligns logical expressions, especially if they span multiple lines, to express operator priority in the layout."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 19); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_logical_expressions." 
			+ LINE_SEP + "    CHECK is_buffer-item_id IS NOT INITIAL" 
			+ LINE_SEP + "       AND is_buffer-any_flag = abap_false" 
			+ LINE_SEP + "           AND is_buffer-other_flag = abap_false" 
			+ LINE_SEP + "                 AND is_buffer-was_changed = abap_true." 
			+ LINE_SEP 
			+ LINE_SEP + "    CHECK line_exists( its_table[ 0 ] )    AND  its_table[ 0 ]-processed  =     abap_true" 
			+ LINE_SEP + "                OR line_exists( its_other_table[ 1 ] ) AND lines( its_other_table )   >     2 ." 
			+ LINE_SEP 
			+ LINE_SEP + "    IF a = abap_false AND b > 3 " 
			+ LINE_SEP + "         OR a = abap_true AND b <= 10." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    IF  ( c IS NOT SUPPLIED " 
			+ LINE_SEP + "       OR b IS INITIAL ) " 
			+ LINE_SEP + "       AND ( d IS SUPPLIED " 
			+ LINE_SEP + "       OR b IS NOT INITIAL )." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP 
			+ LINE_SEP + "    ELSEIF line_exists( its_table[ 0 ] ) " 
			+ LINE_SEP + "       OR ( lines( its_table ) > 2" 
			+ LINE_SEP + "    AND line_exists( its_table[ 1 ] ) )." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    WHILE ( a = abap_true      OR b > 3 AND ( c IS BOUND OR d IS INSTANCE OF cl_x ) )" 
			+ LINE_SEP + "       AND ( a = abap_false OR b <= 10 AND ( c_alt IS NOT BOUND OR e_alt IS INSTANCE OF cl_xyz ) )" 
			+ LINE_SEP + "          OR ( c IS NOT SUPPLIED OR b IS INITIAL )" 
			+ LINE_SEP + "       AND ( d IS SUPPLIED OR b IS NOT INITIAL )" 
			+ LINE_SEP + "       AND line_exists( its_table[ 0 ] )" 
			+ LINE_SEP + "            EQUIV     lines( its_table ) > 2" 
			+ LINE_SEP + "       AND line_exists( its_table[ 1 ] )." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDWHILE." 
			+ LINE_SEP 
			+ LINE_SEP + "    IF mo_item->ms_data-item_id = if_any_interface=>co_any_item_id" 
			+ LINE_SEP + "    AND mo_item->ms_data-name = if_any_interface=>co_any_name" 
			+ LINE_SEP + "    AND ( ( lv_quantity >= 0 AND iv_total_quantity >= lv_quantity ) " 
			+ LINE_SEP + "    OR ( lv_quantity < 0 AND iv_total_quantity <= lv_quantity ) ). " 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    LOOP AT mts_data ASSIGNING FIELD-SYMBOL(<ls_data>) USING KEY any_key_name" 
			+ LINE_SEP + "         WHERE is_valid = abap_true" 
			+ LINE_SEP + "             AND category = if_any_interface=>co_any_category" 
			+ LINE_SEP + "              AND name <> if_any_interface=>co_any_name" 
			+ LINE_SEP + "               AND item_id = if_any_interface=>co_any_item_id" 
			+ LINE_SEP + "                AND statistic = abap_false." 
			+ LINE_SEP + "      \" do something" 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private static final String[] alignStyleSelectionLeftOnly = new String[] { "do not align", "left-align" }; // for IF (because this keyword is too short to be right-aligned with "AND" or "EQUIV")
	private static final String[] alignStyleSelection = new String[] { "do not align", "left-align", "right-align" };

	ConfigEnumValue<AlignStyle> configAlignIfWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignIfWithBoolOps", "Align AND / OR / EQUIV with IF", alignStyleSelectionLeftOnly, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN);
	ConfigEnumValue<AlignStyle> configAlignElseIfWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignElseIfWithBoolOps", "Align AND / OR / EQUIV with ELSEIF", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN);
	ConfigEnumValue<AlignStyle> configAlignCheckWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignCheckWithBoolOps", "Align AND / OR / EQUIV with CHECK", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN);
	ConfigEnumValue<AlignStyle> configAlignWhileWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignWhileWithBoolOps", "Align AND / OR / EQUIV with WHILE", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN);
	ConfigEnumValue<AlignStyle> configAlignWhereWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignWhereWithBoolOps", "Align AND / OR / EQUIV with WHERE", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN);
	ConfigEnumValue<AlignStyle> configAlignUntilWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignUntilWithBoolOps", "Align AND / OR / EQUIV with UNTIL", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN, AlignStyle.DO_NOT_ALIGN, LocalDate.of(2023, 6, 9));

	ConfigEnumValue<AlignStyle> configAlignSqlOnWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignSqlOnWithBoolOps", "SQL: Align AND / OR with ON", alignStyleSelectionLeftOnly, AlignStyle.values(), AlignStyle.LEFT_ALIGN, AlignStyle.DO_NOT_ALIGN, LocalDate.of(2023, 11, 18));
	ConfigEnumValue<AlignStyle> configAlignSqlWhereWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignSqlWhereWithBoolOps", "SQL: Align AND / OR with WHERE", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN, AlignStyle.DO_NOT_ALIGN, LocalDate.of(2023, 11, 18));
	ConfigEnumValue<AlignStyle> configAlignSqlHavingWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignSqlHavingWithBoolOps", "SQL: Align AND / OR with HAVING", alignStyleSelection, AlignStyle.values(), AlignStyle.RIGHT_ALIGN, AlignStyle.DO_NOT_ALIGN, LocalDate.of(2023, 11, 18));
	ConfigEnumValue<AlignStyle> configAlignSqlWhenWithBoolOps = new ConfigEnumValue<AlignStyle>(this, "AlignSqlWhenWithBoolOps", "SQL: Align AND / OR with WHEN", alignStyleSelection, AlignStyle.values(), AlignStyle.DO_NOT_ALIGN, AlignStyle.DO_NOT_ALIGN, LocalDate.of(2023, 11, 18));
	
	ConfigBoolValue configRightAlignComparisonOps = new ConfigBoolValue(this, "RightAlignComparisonOps", "Right-align comparison operators / IS", true);
	ConfigBoolValue configOnlyAlignSameObjects = new ConfigBoolValue(this, "OnlyAlignSameObjects", "Only align comparisons on same object", false);
	ConfigIntValue configMaxInnerSpaces = new ConfigIntValue(this, "MaxInnerSpaces", "Do not align if more than", "inner spaces would be required", 1, 20, 999);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignIfWithBoolOps, configAlignElseIfWithBoolOps, configAlignCheckWithBoolOps, configAlignWhileWithBoolOps, configAlignWhereWithBoolOps, configAlignUntilWithBoolOps, 
			configAlignSqlOnWithBoolOps, configAlignSqlWhereWithBoolOps, configAlignSqlHavingWithBoolOps,configAlignSqlWhenWithBoolOps,  
			configRightAlignComparisonOps, configOnlyAlignSameObjects, configMaxInnerSpaces };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignLogicalExpressionsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	public final boolean alignLogicalExpression(Code code, Command command, Token keyword, Token end, boolean testRuleBlocked, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (testRuleBlocked && isCommandBlocked(command)) {
			return false;
		} else {
			return executeOn(code, command, keyword, end, releaseRestriction);
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		AlignStyle keywordAlignStyle = getAlignStyle(keyword);
		try {
			Token logExpStart = keyword.getNextCodeToken();
			LogicalExpression logicalExpression = LogicalExpression.create(logExpStart, end.getPrev());
			if (!logicalExpression.isSupported()) 
				return false;

			TreeAlign treeAlign = TreeAlign.createFrom(logicalExpression);
			return treeAlign.align(keyword, keywordAlignStyle, configRightAlignComparisonOps.getValue(), true, configOnlyAlignSameObjects.getValue(), configMaxInnerSpaces.getValue());
		} catch (UnexpectedSyntaxException ex) {
			(new UnexpectedSyntaxBeforeChanges(this, ex)).addToLog();
		}
		return false;
	}

	private AlignStyle getAlignStyle(Token keyword) {
		if (keyword.isKeyword("IF")) {
			return AlignStyle.forValue(configAlignIfWithBoolOps.getValue());
		} else if (keyword.isKeyword("ELSEIF")) {
			return AlignStyle.forValue(configAlignElseIfWithBoolOps.getValue());
		} else if (keyword.isKeyword("CHECK")) {
			return AlignStyle.forValue(configAlignCheckWithBoolOps.getValue());
		} else if (keyword.isKeyword("WHILE")) {
			return AlignStyle.forValue(configAlignWhileWithBoolOps.getValue());
		} else if (keyword.isKeyword("WHERE")) { 
			if (keyword.getParentCommand().isAbapSqlOperation()) {
				// in SELECT etc.
				return AlignStyle.forValue(configAlignSqlWhereWithBoolOps.getValue());
			} else {
				// in LOOPs or constructor expressions
				return AlignStyle.forValue(configAlignWhereWithBoolOps.getValue());
			}
		} else if (keyword.isKeyword("UNTIL")) { // in constructor expressions
			return AlignStyle.forValue(configAlignUntilWithBoolOps.getValue());

		} else if (keyword.isKeyword("ON")) { 
			return AlignStyle.forValue(configAlignSqlOnWithBoolOps.getValue());
		} else if (keyword.isKeyword("HAVING")) { 
			return AlignStyle.forValue(configAlignSqlHavingWithBoolOps.getValue());
		} else if (keyword.isKeyword("WHEN")) { 
			return AlignStyle.forValue(configAlignSqlWhenWithBoolOps.getValue());
		
		} else if (keyword.textEquals("xsdbool(")) {
			return AlignStyle.DO_NOT_ALIGN;
		} else {
			return AlignStyle.DO_NOT_ALIGN;
		}
	}
}
