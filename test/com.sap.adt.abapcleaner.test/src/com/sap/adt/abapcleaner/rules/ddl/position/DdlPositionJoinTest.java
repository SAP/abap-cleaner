package com.sap.adt.abapcleaner.rules.ddl.position;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlPositionJoinTest extends RuleTestBase {
	private DdlPositionJoinRule rule;
	
	DdlPositionJoinTest() {
		super(RuleID.DDL_POSITION_JOIN);
		rule = (DdlPositionJoinRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configBreakBeforeJoinKeywords.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configJoinKeywordsIndent.setValue(4);
		
		rule.configBreakBeforeJoinDataSource.setEnumValue(DdlLineBreak.NEVER);
		rule.configJoinDataSourceIndent.setValue(4);
		
		rule.configBreakBeforeJoinCondition.setEnumValue(DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
		rule.configJoinConditionIndent.setValue(6);
		
		// configuration for ASSOCIATION
		rule.configBreakBeforeAssociationKeywords.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configAssociationKeywordsIndent.setValue(2);
		
		rule.configBreakBeforeAssociationDataSource.setEnumValue(DdlLineBreak.NEVER);
		rule.configAssociationDataSourceIndent.setValue(2);
		
		rule.configBreakBeforeAssociationCondition.setEnumValue(DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
		rule.configAssociationConditionIndent.setValue(4);
		
		rule.configBreakBeforeAssociationFilter.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configAssociationFilterIndent.setValue(4);
	}

	@Test
	void testBreakBeforeJoin() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  // comment");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    // comment");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("      and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepJoinAsIs() {
		rule.configBreakBeforeJoinKeywords.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testBreakBeforeJoinDataSource() {
		rule.configBreakBeforeJoinDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configJoinDataSourceIndent.setValue(6);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join");
		buildExp("      I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join");
		buildExp("      I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("      and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepJoinDataSourceAsIs() {
		rule.configBreakBeforeJoinDataSource.setEnumValue(DdlLineBreak.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join");
		buildSrc("         I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join");
		buildExp("    I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("      and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeJoinCondition() {
		rule.configBreakBeforeJoinCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  // comment 1");
		buildSrc("  /* comment 2 */");
		buildSrc("  /* multi-line");
		buildSrc("     comment 3 */");
		buildSrc("  on AnyAlias.IdField = ThirdAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      // comment 1");
		buildExp("      /* comment 2 */");
		buildExp("      /* multi-line");
		buildExp("         comment 3 */");
		buildExp("      on AnyAlias.IdField = ThirdAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testContinueWithOneLinerJoinConditions() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on AnyAlias.IdField = ThirdAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias on AnyAlias.IdField = ThirdAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testBreakJoinConditionsDueToMultiLiner() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepJoinConditionAsIs() {
		rule.configBreakBeforeJoinCondition.setEnumValue(DdlConditionLineBreak.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("      and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverBreakBeforeJoinCondition() {
		rule.configBreakBeforeJoinCondition.setEnumValue(DdlConditionLineBreak.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField,");
		buildSrc("  key ThirdAlias.SubIdField,");
		buildSrc("      OtherAlias.AnyNonKeyField,");
		buildSrc("      ThirdAlias.OtherNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("                                                       and OtherAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField,");
		buildExp("  key ThirdAlias.SubIdField,");
		buildExp("      OtherAlias.AnyNonKeyField,");
		buildExp("      ThirdAlias.OtherNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeAssociationKeywords() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
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
		rule.configBreakBeforeAssociationKeywords.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

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
		rule.configBreakBeforeAssociationDataSource.setEnumValue(DdlLineBreak.ALWAYS);

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
		rule.configBreakBeforeAssociationDataSource.setEnumValue(DdlLineBreak.KEEP_AS_IS);

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
		rule.configBreakBeforeAssociationCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);

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
		rule.configBreakBeforeAssociationCondition.setEnumValue(DdlConditionLineBreak.KEEP_AS_IS);

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
		rule.configBreakBeforeAssociationCondition.setEnumValue(DdlConditionLineBreak.NEVER);

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
		rule.configJoinKeywordsIndent.setValue(3);
		rule.configBreakBeforeJoinDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configJoinDataSourceIndent.setValue(6);
		rule.configBreakBeforeJoinCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);
		rule.configJoinConditionIndent.setValue(9);
		rule.configAssociationKeywordsIndent.setValue(3);
		rule.configBreakBeforeAssociationDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configAssociationDataSourceIndent.setValue(6);
		rule.configBreakBeforeAssociationCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);
		rule.configAssociationConditionIndent.setValue(9);
		rule.configAssociationFilterIndent.setValue(12);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias left outer to one join I_OtherEntity as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  left outer to one");
		buildSrc("  join I_ThirdEntity as ThirdAlias");
		buildSrc("  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("  and OtherAlias.SubIdField = ThirdAlias.SubIdField");
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
		buildExp("   left outer to one join");
		buildExp("      I_OtherEntity as OtherAlias");
		buildExp("         on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("   left outer to one join");
		buildExp("      I_ThirdEntity as ThirdAlias");
		buildExp("         on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("         and OtherAlias.SubIdField = ThirdAlias.SubIdField");
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
		rule.configBreakBeforeAssociationFilter.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

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
		rule.configBreakBeforeAssociationDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configAssociationDataSourceIndent.setValue(4);

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
}
