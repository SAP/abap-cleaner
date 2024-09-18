package com.sap.adt.abapcleaner.rules.ddl.position;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlPositionAssociationTest extends RuleTestBase {
	private DdlPositionAssociationRule rule;
	
	DdlPositionAssociationTest() {
		super(RuleID.DDL_POSITION_ASSOCIATION);
		rule = (DdlPositionAssociationRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configBreakBeforeKeywords.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configKeywordsIndent.setValue(2);
		
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.NEVER);
		rule.configDataSourceIndent.setValue(2);
		
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
		rule.configConditionIndent.setValue(4);
		
		rule.configBreakBeforeFilter.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configFilterIndent.setValue(4);
	}

	@Test
	void testAlwaysBreakBeforeAssociationKeywords() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    left outer to one join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField");
		buildSrc("                                                  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("// comment");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  // comment");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepAssociationKeywordsAsIs() {
		rule.configBreakBeforeKeywords.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBeforeAssociationDataSource() {
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.ALWAYS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to");
		buildExp("  I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many");
		buildExp("  I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepAssociationDataSourceAsIs() {
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to");
		buildSrc("  I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to");
		buildExp("  I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeAssociationCondition() {
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testContinueWithOneLinerAssociationConditions() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testBreakBeforeAssociationConditionsDueToMultiLiners() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepAssociationConditionsAsIs() {
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildExp("                                                      and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverBreakBeforeAssociationCondition() {
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField");
		buildSrc("  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField");
		buildExp("                                                  and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildExp("                                                      and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testIndents() {
		rule.configKeywordsIndent.setValue(3);
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configDataSourceIndent.setValue(6);
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);
		rule.configConditionIndent.setValue(9);
		rule.configFilterIndent.setValue(12);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..1] to I_FourthEntity as _Fourth on  $projection.IdField = _Fourth.IdField");
		buildSrc("                                                  and _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField,");
		buildSrc("");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("   association [0..1] to");
		buildExp("      I_FourthEntity as _Fourth");
		buildExp("         on  $projection.IdField = _Fourth.IdField");
		buildExp("         and _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("   association of many to many");
		buildExp("      I_FifthEntity as _Fifth");
		buildExp("         on  $projection.IdField    = _Fifth.IdField");
		buildExp("         and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField,");
		buildExp("");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeAssociationFilter() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField with default");
		buildSrc("  filter _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("   with default filter _Fifth.CondField > 10");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("    with default filter _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("    with default filter _Fifth.CondField > 10");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepAssociationFilterAsIs() {
		rule.configBreakBeforeFilter.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias association [0..1] to I_FourthEntity as _Fourth");
		buildSrc("  on  $projection.IdField = _Fourth.IdField with default");
		buildSrc("  filter _Fourth.CondField   = 'X'");
		buildSrc("");
		buildSrc("  association of many");
		buildSrc("  to many I_FifthEntity as _Fifth on  $projection.IdField    = _Fifth.IdField");
		buildSrc("                                  and $projection.SubIdField = _Fifth.SubIdField");
		buildSrc("   with default filter _Fifth.CondField > 10");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("      _Fourth,");
		buildSrc("      _Fifth");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField with default filter _Fourth.CondField   = 'X'");
		buildExp("");
		buildExp("  association of many to many I_FifthEntity as _Fifth");
		buildExp("    on  $projection.IdField    = _Fifth.IdField");
		buildExp("    and $projection.SubIdField = _Fifth.SubIdField");
		buildExp("    with default filter _Fifth.CondField > 10");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("      _Fourth,");
		buildExp("      _Fifth");
		buildExp("}");

		testRule();
	}

	@Test
	void testRedefineAssociation() {
		buildSrc("define view entity projection_view");
		buildSrc("  as projection on I_Any as Alias redefine association Alias._AnyAssociation as _RedefinedName");
		buildSrc("    redirected to composition child I_Other");

		buildExp("define view entity projection_view");
		buildExp("  as projection on I_Any as Alias");
		buildExp("");
		buildExp("  redefine association Alias._AnyAssociation as _RedefinedName");
		buildExp("    redirected to composition child I_Other");

		testRule();
	}

	@Test
	void testComposition() {
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configDataSourceIndent.setValue(4);

		buildSrc("define root view entity I_AnyView");
		buildSrc("  as select from any_dtab composition [0..*] of I_TextView as _Text");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyField,");
		buildSrc("      OtherField");
		buildSrc("}");

		buildExp("define root view entity I_AnyView");
		buildExp("  as select from any_dtab");
		buildExp("");
		buildExp("  composition [0..*] of");
		buildExp("    I_TextView as _Text");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyField,");
		buildExp("      OtherField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAssociationWithParameters() {
		// ensure that the ON condition is moved to the next line, since a join source gets parameters

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..1] to I_FourthEntity(");
		buildSrc("                          P_AnyParameter   = 'any value'");
		buildSrc("                          P_OtherParameter = 'other value' ) as _Fourth on  $projection.IdField = _Fourth.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_FourthEntity(");
		buildExp("                          P_AnyParameter   = 'any value'");
		buildExp("                          P_OtherParameter = 'other value' ) as _Fourth");
		buildExp("    on  $projection.IdField = _Fourth.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField");
		buildExp("}");

		testRule();
	}
}
