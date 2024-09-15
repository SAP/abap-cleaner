package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.AlignStyle;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;

public class DdlAlignLogicalExpressionsTest extends RuleTestBase {
	private DdlAlignLogicalExpressionsRule rule;
	private DdlSpacesAroundBracketsRule spacesAroundBracketsRule;
	
	DdlAlignLogicalExpressionsTest() {
		super(RuleID.DDL_ALIGN_LOGICAL_EXPRESSIONS);

		rule = (DdlAlignLogicalExpressionsRule)getRule();
		spacesAroundBracketsRule = (DdlSpacesAroundBracketsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_BRACKETS); 
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignJoinOn.setValue(true);
		rule.configAlignAssociationOn.setValue(true);
		rule.configAlignWhen.setValue(true);
		rule.configAlignPathExpressions.setValue(true);
		rule.configAlignWhere.setValue(true);
		rule.configAlignHaving.setValue(true);
		
		rule.configAlignOnWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);
		rule.configAlignFilterWithBoolOps.setEnumValue( AlignStyle.RIGHT_ALIGN);
		rule.configAlignWhenWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);
		rule.configAlignHavingWithBoolOps.setEnumValue(AlignStyle.RIGHT_ALIGN);

		rule.configRightAlignComparisonOps.setValue(true);
		rule.configMaxInnerSpaces.setValue(20);

		// setup configuration of other rules that is reused by the rule under test
		spacesAroundBracketsRule.configSpacesInsideArithParens.setEnumValue(ChangeType.NEVER);
	}

	@Test
	void testAlignJoinOnAndAssociationOnLeft() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("         and ( OtherAlias.AnyType = 'X' or");
		buildSrc("      OtherAlias.AnyType = 'Y' )");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildSrc("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildSrc("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on  OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("      and (   OtherAlias.AnyType = 'X'");
		buildExp("           or OtherAlias.AnyType = 'Y')");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on  AnyAlias.AnyKeyField      = _ThirdSource.AnyKeyField");
		buildExp("    and OtherAlias.OtherKeyField  = _ThirdSource.OtherKeyField");
		buildExp("    and OtherAlias.NumericField  >= _ThirdSource.NumericField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignJoinOnAndAssociationOnDetached() {
		rule.configAlignOnWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("         and ( OtherAlias.AnyType = 'X' or");
		buildSrc("      OtherAlias.AnyType = 'Y' )");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildSrc("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildSrc("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on     OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("         and (   OtherAlias.AnyType = 'X'");
		buildExp("              or OtherAlias.AnyType = 'Y')");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on     AnyAlias.AnyKeyField      = _ThirdSource.AnyKeyField");
		buildExp("       and OtherAlias.OtherKeyField  = _ThirdSource.OtherKeyField");
		buildExp("       and OtherAlias.NumericField  >= _ThirdSource.NumericField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSkipJoinOn() {
		rule.configAlignJoinOn.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("         and ( OtherAlias.AnyType = 'X' or");
		buildSrc("      OtherAlias.AnyType = 'Y' )");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildSrc("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildSrc("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("         and ( OtherAlias.AnyType = 'X' or");
		buildExp("      OtherAlias.AnyType = 'Y' )");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on  AnyAlias.AnyKeyField      = _ThirdSource.AnyKeyField");
		buildExp("    and OtherAlias.OtherKeyField  = _ThirdSource.OtherKeyField");
		buildExp("    and OtherAlias.NumericField  >= _ThirdSource.NumericField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSkipAssociationOn() {
		rule.configAlignAssociationOn.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("         and ( OtherAlias.AnyType = 'X' or");
		buildSrc("      OtherAlias.AnyType = 'Y' )");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildSrc("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildSrc("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on  OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("      and (   OtherAlias.AnyType = 'X'");
		buildExp("           or OtherAlias.AnyType = 'Y')");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildExp("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildExp("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignWhenRight() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      case when AnyAlias.Category = 'A' then 'category A'");
		buildSrc("           when AnyAlias.Category = 'B' or");
		buildSrc("      AnyAlias.Category = 'C' then 'category B or C'");
		buildSrc("      end as CategoryText,");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      case when AnyAlias.Category = 'A' then 'category A'");
		buildExp("           when AnyAlias.Category = 'B'");
		buildExp("             or AnyAlias.Category = 'C' then 'category B or C'");
		buildExp("      end as CategoryText,");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignWhenLeft() {
		rule.configAlignWhenWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      case when AnyAlias.Category = 'A' then 'category A'");
		buildSrc("           when AnyAlias.Category = 'B' or");
		buildSrc("      AnyAlias.Category = 'C' then 'category B or C'");
		buildSrc("      end as CategoryText,");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      case when AnyAlias.Category = 'A' then 'category A'");
		buildExp("           when AnyAlias.Category = 'B'");
		buildExp("           or   AnyAlias.Category = 'C' then 'category B or C'");
		buildExp("      end as CategoryText,");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignWhenDetached() {
		rule.configAlignWhenWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      case when AnyAlias.Category = 'A' then 'category A'");
		buildSrc("           when AnyAlias.Category = 'B' or");
		buildSrc("      AnyAlias.Category = 'C' then 'category B or C'");
		buildSrc("      end as CategoryText,");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      case when AnyAlias.Category = 'A' then 'category A'");
		buildExp("           when    AnyAlias.Category = 'B'");
		buildExp("                or AnyAlias.Category = 'C' then 'category B or C'");
		buildExp("      end as CategoryText,");
		buildExp("}");

		testRule();
	}

	@Test
	void testSkipWhen() {
		rule.configAlignWhen.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      case when AnyAlias.Category = 'A' then 'category A'");
		buildSrc("           when AnyAlias.Category = 'B' or");
		buildSrc("      AnyAlias.Category = 'C' then 'category B or C'");
		buildSrc("      end as CategoryText,");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlignPathExpression() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      max(_ThirdSource[1:SubCategory = 'X' or");
		buildSrc("      SubCategory = 'Y'");
		buildSrc("      or SubCategory = 'Z'].NumericField) as MaxNumericField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("      max(_ThirdSource[1:    SubCategory = 'X'");
		buildExp("                          or SubCategory = 'Y'");
		buildExp("                          or SubCategory = 'Z'].NumericField) as MaxNumericField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSkipPathExpressions() {
		rule.configAlignPathExpressions.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("      max(_ThirdSource[1:SubCategory = 'X' or");
		buildSrc("      SubCategory = 'Y'");
		buildSrc("      or SubCategory = 'Z'].NumericField) as MaxNumericField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlignWhereRight() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("where OtherAlias.OtherKeyField > 'NN'");
		buildExp("  and OtherAlias.NumericField  > 100");
		buildExp("  and (   AnyAlias.Category = 'A'");
		buildExp("       or AnyAlias.Category = 'B'");
		buildExp("       or AnyAlias.Category = 'C')");

		testRule();
	}

	@Test
	void testAlignWhereLeft() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where OtherAlias.OtherKeyField > 'NN'");
		buildExp("  and   OtherAlias.NumericField  > 100");
		buildExp("  and   (   AnyAlias.Category = 'A'");
		buildExp("         or AnyAlias.Category = 'B'");
		buildExp("         or AnyAlias.Category = 'C')");

		testRule();
	}

	@Test
	void testAlignWhereDetached() {
		rule.configAlignWhereWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where     OtherAlias.OtherKeyField > 'NN'");
		buildExp("        and OtherAlias.NumericField  > 100");
		buildExp("        and (   AnyAlias.Category = 'A'");
		buildExp("             or AnyAlias.Category = 'B'");
		buildExp("             or AnyAlias.Category = 'C')");

		testRule();
	}

	@Test
	void testSkipWhere() {
		rule.configAlignWhere.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlignHavingRight() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("  group by AnyAlias.AnyKeyField,");
		buildSrc("           OtherAlias.OtherKeyField,");
		buildSrc("           OtherAlias.AnyType,");
		buildSrc("           AnyAlias.Category");
		buildSrc("");
		buildSrc("  having AnyAlias.Category = 'A' and");
		buildSrc("  avg(OtherAlias.NumericField) >= 200 or");
		buildSrc("    AnyAlias.Category = 'B'");
		buildSrc("  and sum(OtherAlias.NumericField) >= 1000 and");
		buildSrc("sum(OtherAlias.NumericField) < 5000");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.AnyType,");
		buildExp("           AnyAlias.Category");
		buildExp("");
		buildExp("  having     AnyAlias.Category             = 'A'");
		buildExp("         and avg(OtherAlias.NumericField) >= 200");
		buildExp("      or     AnyAlias.Category             = 'B'");
		buildExp("         and sum(OtherAlias.NumericField) >= 1000");
		buildExp("         and sum(OtherAlias.NumericField)  < 5000");

		testRule();
	}

	@Test
	void testAlignHavingLeft() {
		rule.configAlignHavingWithBoolOps.setEnumValue(AlignStyle.LEFT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("  group by AnyAlias.AnyKeyField,");
		buildSrc("           OtherAlias.OtherKeyField,");
		buildSrc("           OtherAlias.AnyType,");
		buildSrc("           AnyAlias.Category");
		buildSrc("");
		buildSrc("  having AnyAlias.Category = 'A' and");
		buildSrc("  avg(OtherAlias.NumericField) >= 200 or");
		buildSrc("    AnyAlias.Category = 'B'");
		buildSrc("  and sum(OtherAlias.NumericField) >= 1000 and");
		buildSrc("sum(OtherAlias.NumericField) < 5000");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.AnyType,");
		buildExp("           AnyAlias.Category");
		buildExp("");
		buildExp("  having     AnyAlias.Category             = 'A'");
		buildExp("         and avg(OtherAlias.NumericField) >= 200");
		buildExp("  or         AnyAlias.Category             = 'B'");
		buildExp("         and sum(OtherAlias.NumericField) >= 1000");
		buildExp("         and sum(OtherAlias.NumericField)  < 5000");

		testRule();
	}

	@Test
	void testAlignHavingDetached() {
		rule.configAlignHavingWithBoolOps.setEnumValue(AlignStyle.DO_NOT_ALIGN);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("  group by AnyAlias.AnyKeyField,");
		buildSrc("           OtherAlias.OtherKeyField,");
		buildSrc("           OtherAlias.AnyType,");
		buildSrc("           AnyAlias.Category");
		buildSrc("");
		buildSrc("  having AnyAlias.Category = 'A' and");
		buildSrc("  avg(OtherAlias.NumericField) >= 200 or");
		buildSrc("    AnyAlias.Category = 'B'");
		buildSrc("  and sum(OtherAlias.NumericField) >= 1000 and");
		buildSrc("sum(OtherAlias.NumericField) < 5000");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.AnyType,");
		buildExp("           AnyAlias.Category");
		buildExp("");
		buildExp("  having        AnyAlias.Category             = 'A'");
		buildExp("            and avg(OtherAlias.NumericField) >= 200");
		buildExp("         or     AnyAlias.Category             = 'B'");
		buildExp("            and sum(OtherAlias.NumericField) >= 1000");
		buildExp("            and sum(OtherAlias.NumericField)  < 5000");

		testRule();
	}

	@Test
	void testSkipHaving() {
		rule.configAlignHaving.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("  group by AnyAlias.AnyKeyField,");
		buildSrc("           OtherAlias.OtherKeyField,");
		buildSrc("           OtherAlias.AnyType,");
		buildSrc("           AnyAlias.Category");
		buildSrc("");
		buildSrc("  having AnyAlias.Category = 'A' and");
		buildSrc("  avg(OtherAlias.NumericField) >= 200 or");
		buildSrc("    AnyAlias.Category = 'B'");
		buildSrc("  and sum(OtherAlias.NumericField) >= 1000 and");
		buildSrc("sum(OtherAlias.NumericField) < 5000");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testLeftAlignComparisonOps() {
		rule.configRightAlignComparisonOps.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField and");
		buildSrc("    OtherAlias.OtherKeyField = _ThirdSource.OtherKeyField");
		buildSrc("    and OtherAlias.NumericField >= _ThirdSource.NumericField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on  AnyAlias.AnyKeyField     =  _ThirdSource.AnyKeyField");
		buildExp("    and OtherAlias.OtherKeyField =  _ThirdSource.OtherKeyField");
		buildExp("    and OtherAlias.NumericField  >= _ThirdSource.NumericField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testMaxInnerSpacesLow() {
		rule.configMaxInnerSpaces.setValue(12);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("  group by AnyAlias.AnyKeyField,");
		buildSrc("           OtherAlias.OtherKeyField,");
		buildSrc("           OtherAlias.AnyType,");
		buildSrc("           AnyAlias.Category");
		buildSrc("");
		buildSrc("  having AnyAlias.Category = 'A' and");
		buildSrc("  avg(OtherAlias.NumericField) >= 200 or");
		buildSrc("    AnyAlias.Category = 'B'");
		buildSrc("  and sum(OtherAlias.NumericField) >= 1000 and");
		buildSrc("sum(OtherAlias.NumericField) < 5000");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [1..*] to I_ThirdSource  as _ThirdSource");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdSource.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.AnyType,");
		buildExp("           AnyAlias.Category");
		buildExp("");
		buildExp("  having     AnyAlias.Category = 'A'");
		buildExp("         and avg(OtherAlias.NumericField) >= 200");
		buildExp("      or     AnyAlias.Category = 'B'");
		buildExp("         and sum(OtherAlias.NumericField) >= 1000");
		buildExp("         and sum(OtherAlias.NumericField)  < 5000");

		testRule();
	}

	@Test
	void testHierarchySkipped() {
		buildSrc("define hierarchy AnyHierarchy");
		buildSrc("  with parameters");
		buildSrc("    p_date_from : dats,");
		buildSrc("    p_date_to   : dats");
		buildSrc("");
		buildSrc("  as parent child hierarchy(");
		buildSrc("    source AnySource");
		buildSrc("    child to parent association _relat");
		buildSrc("    start where id = 'A'");
		buildSrc("    siblings order by id");
		buildSrc("  )");
		buildSrc("");
		buildSrc("{");
		buildSrc("  id,");
		buildSrc("  parent");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}
	
	@Test
	void testCommentAfterWhereKeyword() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where // comment");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where // comment");
		buildExp("        OtherAlias.OtherKeyField > 'NN'");
		buildExp("    and OtherAlias.NumericField  > 100");
		buildExp("    and (   AnyAlias.Category = 'A'");
		buildExp("         or AnyAlias.Category = 'B'");
		buildExp("         or AnyAlias.Category = 'C')");

		testRule();
	}

	@Test
	void testSpacesInsideParentheses() {
		spacesAroundBracketsRule.configSpacesInsideArithParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and ( AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C')");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where OtherAlias.OtherKeyField > 'NN'");
		buildExp("    and OtherAlias.NumericField  > 100");
		buildExp("    and (    AnyAlias.Category = 'A'");
		buildExp("          or AnyAlias.Category = 'B'");
		buildExp("          or AnyAlias.Category = 'C' )");

		testRule();
	}

	@Test
	void testParenthesesOnOwnLineAtEnd() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and");
		buildSrc("(");
		buildSrc("AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C'");
		buildSrc(")");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where OtherAlias.OtherKeyField > 'NN'");
		buildExp("    and OtherAlias.NumericField  > 100");
		buildExp("    and (   AnyAlias.Category = 'A'");
		buildExp("         or AnyAlias.Category = 'B'");
		buildExp("         or AnyAlias.Category = 'C')");

		testRule();
	}

	@Test
	void testParenthesesOnOwnLine() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("(");
		buildSrc("AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C'");
		buildSrc("   ) and");
		buildSrc("OtherAlias.NumericField > 100");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where OtherAlias.OtherKeyField > 'NN'");
		buildExp("    and (   AnyAlias.Category = 'A'");
		buildExp("         or AnyAlias.Category = 'B'");
		buildExp("         or AnyAlias.Category = 'C')");
		buildExp("    and OtherAlias.NumericField > 100");

		testRule();
	}

	@Test
	void testCommentsBlockingParentheses() {
		// if comments are blocking parentheses from being on the same line as their content,
		// alignment does not work, but nevertheless, referential integrity must be kept
		
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherSource as OtherAlias");
		buildSrc("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where");
		buildSrc("OtherAlias.OtherKeyField > 'NN' and");
		buildSrc("OtherAlias.NumericField > 100");
		buildSrc("and");
		buildSrc("( // comment1");
		buildSrc("AnyAlias.Category = 'A' or");
		buildSrc("AnyAlias.Category = 'B'");
		buildSrc("or AnyAlias.Category = 'C' // comment2");
		buildSrc(")");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherSource as OtherAlias");
		buildExp("      on OtherAlias.AnyKeyField = AnyAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where OtherAlias.OtherKeyField > 'NN'");
		buildExp("    and OtherAlias.NumericField  > 100");
		buildExp("    and ( // comment1");
		buildExp("         AnyAlias.Category = 'A'");
		buildExp("  or AnyAlias.Category = 'B'");
		buildExp("  or AnyAlias.Category = 'C' // comment2");
		buildExp("  )");

		testRule();
	}


	@Test
	void testAlignAlternativeNotEqualsOperator() {
		// ensure that comparison operator != is not changed to "! =", which would be a syntax error;
		// using != works and is not a syntax error, although documentation only mentions <>
		buildSrc("  define view entity C_AnyEntity");
		buildSrc("    as select from I_AnyEntity as AnyAlias");
		buildSrc("  {");
		buildSrc("    key AnyKeyField");
		buildSrc("  }");
		buildSrc("  where AnyKeyField  !=   5");

		buildExp("  define view entity C_AnyEntity");
		buildExp("    as select from I_AnyEntity as AnyAlias");
		buildExp("  {");
		buildExp("    key AnyKeyField");
		buildExp("  }");
		buildExp("  where AnyKeyField != 5");

		testRule();
	}

	@Test
	void testParenthesisWithTwoExprsOnOneLine() {
		spacesAroundBracketsRule.configSpacesInsideArithParens.setEnumValue(ChangeType.NEVER);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where (AnyAlias.AnyField = 'X' and AnyAlias.AnyNum < 0 )");
		buildSrc("    or (AnyAlias.OtherField != 'Y' and AnyAlias.OtherNum != 10 )");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where (AnyAlias.AnyField    = 'X' and AnyAlias.AnyNum    < 0)");
		buildExp("     or (AnyAlias.OtherField != 'Y' and AnyAlias.OtherNum != 10)");

		testRule();
	}

	@Test
	void testAlignWhereWithSpacesInsideParens() {
		spacesAroundBracketsRule.configSpacesInsideArithParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("  where (AnyAlias.AnyField = 'X' and AnyAlias.AnyNum < 0 )");
		buildSrc("    or (AnyAlias.OtherField != 'Y' and AnyAlias.OtherNum <> 10 )");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("  where ( AnyAlias.AnyField    = 'X' and AnyAlias.AnyNum    < 0 )");
		buildExp("     or ( AnyAlias.OtherField != 'Y' and AnyAlias.OtherNum <> 10 )");

		testRule();
	}
}
