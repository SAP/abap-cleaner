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
		rule.configBreakBeforeKeywords.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configKeywordsIndent.setValue(4);
		
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.NEVER);
		rule.configDataSourceIndent.setValue(4);
		
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.IF_MULTI_LINE_FOUND);
		rule.configConditionIndent.setValue(6);
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
		rule.configBreakBeforeKeywords.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

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
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configDataSourceIndent.setValue(6);

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
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.KEEP_AS_IS);

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
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);

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
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.KEEP_AS_IS);

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
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.NEVER);

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
	void testIndents() {
		rule.configKeywordsIndent.setValue(3);
		rule.configBreakBeforeDataSource.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configDataSourceIndent.setValue(6);
		rule.configBreakBeforeCondition.setEnumValue(DdlConditionLineBreak.ALWAYS);
		rule.configConditionIndent.setValue(9);

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
	void testJoinWithParameters() {
		// ensure that the ON condition is moved to the next line, since a join source gets parameters
		
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    left outer to one join I_OtherEntity( P_AnyParameter = 'any value' ) as OtherAlias");
		buildSrc("        on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.IdField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity( P_AnyParameter = 'any value' ) as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.IdField");
		buildExp("}");

		testRule();
	}
}
