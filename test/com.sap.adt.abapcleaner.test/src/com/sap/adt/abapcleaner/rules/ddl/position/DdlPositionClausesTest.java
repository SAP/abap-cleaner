package com.sap.adt.abapcleaner.rules.ddl.position;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlPositionClausesTest extends RuleTestBase {
	private DdlPositionClausesRule rule;
	
	DdlPositionClausesTest() {
		super(RuleID.DDL_POSITION_CLAUSES);
		rule = (DdlPositionClausesRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configBreakBeforeWhereEtc.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configWhereEtcIndent.setValue(2);
		
		rule.configBreakBeforeUnionEtc.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configUnionEtcIndent.setValue(0);
	}

	@Test
	void testBreakBeforeClauses() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildSrc("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildSrc("      AnyAlias.UnitField            as Unit");
		buildSrc("}");
		buildSrc("// comment");
		buildSrc("where AnyAlias.AnyConditionField = 'X'");
		buildSrc("  and AnyAlias.AnyCategory       = 'A' group");
		buildSrc("by AnyAlias.AnyKeyField,");
		buildSrc("   AnyAlias.AnyNumericField,");
		buildSrc("   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity as OtherAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildSrc("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildSrc("      OtherAlias.OtherUnitField         as Unit");
		buildSrc("} where OtherAlias.OtherCategory = 'A'");
		buildSrc("     or OtherAlias.OtherCategory = 'B'");
		buildSrc("");
		buildSrc("group by OtherAlias.OtherKeyField,");
		buildSrc("         OtherAlias.OtherNumericField,");
		buildSrc("         OtherAlias.OtherUnitField");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildExp("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildExp("      AnyAlias.UnitField            as Unit");
		buildExp("}");
		buildExp("  // comment");
		buildExp("  where AnyAlias.AnyConditionField = 'X'");
		buildExp("    and AnyAlias.AnyCategory       = 'A'");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           AnyAlias.AnyNumericField,");
		buildExp("           AnyAlias.UnitField");
		buildExp("  having AnyAlias.AnyNumericField > 100");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity as OtherAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildExp("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildExp("      OtherAlias.OtherUnitField         as Unit");
		buildExp("}");
		buildExp("  where OtherAlias.OtherCategory = 'A'");
		buildExp("     or OtherAlias.OtherCategory = 'B'");
		buildExp("");
		buildExp("  group by OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.OtherNumericField,");
		buildExp("           OtherAlias.OtherUnitField");

		testRule();
	}

	@Test
	void testKeepWhereEtcAsIs() {
		rule.configBreakBeforeWhereEtc.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildSrc("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildSrc("      AnyAlias.UnitField            as Unit");
		buildSrc("} where AnyAlias.AnyConditionField = 'X'");
		buildSrc("    and AnyAlias.AnyCategory       = 'A' group");
		buildSrc("by AnyAlias.AnyKeyField,");
		buildSrc("   AnyAlias.AnyNumericField,");
		buildSrc("   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildExp("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildExp("      AnyAlias.UnitField            as Unit");
		buildExp("} where AnyAlias.AnyConditionField = 'X'");
		buildExp("    and AnyAlias.AnyCategory       = 'A' group by AnyAlias.AnyKeyField,");
		buildExp("                                                  AnyAlias.AnyNumericField,");
		buildExp("                                                  AnyAlias.UnitField having AnyAlias.AnyNumericField > 100");

		testRule();
	}

	@Test
	void testKeepUnionEtcAsIs() {
		rule.configBreakBeforeUnionEtc.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildSrc("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildSrc("      AnyAlias.UnitField            as Unit");
		buildSrc("}");
		buildSrc("");
		buildSrc("where AnyAlias.AnyConditionField = 'X'");
		buildSrc("  and AnyAlias.AnyCategory       = 'A' group");
		buildSrc("by AnyAlias.AnyKeyField,");
		buildSrc("   AnyAlias.AnyNumericField,");
		buildSrc("   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100 union");
		buildSrc("   all select from I_OtherEntity as OtherAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildSrc("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildSrc("      OtherAlias.OtherUnitField         as Unit");
		buildSrc("} where OtherAlias.OtherCategory = 'A'");
		buildSrc("     or OtherAlias.OtherCategory = 'B'");
		buildSrc("");
		buildSrc("group by OtherAlias.OtherKeyField,");
		buildSrc("         OtherAlias.OtherNumericField,");
		buildSrc("         OtherAlias.OtherUnitField");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildExp("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildExp("      AnyAlias.UnitField            as Unit");
		buildExp("}");
		buildExp("");
		buildExp("  where AnyAlias.AnyConditionField = 'X'");
		buildExp("    and AnyAlias.AnyCategory       = 'A'");
		buildExp("  group by AnyAlias.AnyKeyField,");
		buildExp("           AnyAlias.AnyNumericField,");
		buildExp("           AnyAlias.UnitField");
		buildExp("  having AnyAlias.AnyNumericField > 100 union all select from I_OtherEntity as OtherAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildExp("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildExp("      OtherAlias.OtherUnitField         as Unit");
		buildExp("}");
		buildExp("  where OtherAlias.OtherCategory = 'A'");
		buildExp("     or OtherAlias.OtherCategory = 'B'");
		buildExp("");
		buildExp("  group by OtherAlias.OtherKeyField,");
		buildExp("           OtherAlias.OtherNumericField,");
		buildExp("           OtherAlias.OtherUnitField");

		testRule();
	}

	@Test
	void testIndent() {
		rule.configWhereEtcIndent.setValue(4);
		rule.configUnionEtcIndent.setValue(2);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildSrc("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildSrc("      AnyAlias.UnitField            as Unit");
		buildSrc("}");
		buildSrc("");
		buildSrc("where AnyAlias.AnyConditionField = 'X'");
		buildSrc("  and AnyAlias.AnyCategory       = 'A' group");
		buildSrc("by AnyAlias.AnyKeyField,");
		buildSrc("   AnyAlias.AnyNumericField,");
		buildSrc("   AnyAlias.UnitField having AnyAlias.AnyNumericField > 100");
		buildSrc("");
		buildSrc("/* multi-line");
		buildSrc("   comment */");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity as OtherAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildSrc("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildSrc("      OtherAlias.OtherUnitField         as Unit");
		buildSrc("} where OtherAlias.OtherCategory = 'A'");
		buildSrc("     or OtherAlias.OtherCategory = 'B'");
		buildSrc("");
		buildSrc("group by OtherAlias.OtherKeyField,");
		buildSrc("         OtherAlias.OtherNumericField,");
		buildSrc("         OtherAlias.OtherUnitField");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField          as AnyKey,");
		buildExp("      sum(AnyAlias.AnyNumericField) as AnySum,");
		buildExp("      AnyAlias.UnitField            as Unit");
		buildExp("}");
		buildExp("");
		buildExp("    where AnyAlias.AnyConditionField = 'X'");
		buildExp("      and AnyAlias.AnyCategory       = 'A'");
		buildExp("    group by AnyAlias.AnyKeyField,");
		buildExp("             AnyAlias.AnyNumericField,");
		buildExp("             AnyAlias.UnitField");
		buildExp("    having AnyAlias.AnyNumericField > 100");
		buildExp("");
		buildExp("  /* multi-line");
		buildExp("     comment */");
		buildExp("  union all");
		buildExp("  select from I_OtherEntity as OtherAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key OtherAlias.OtherKeyField          as AnyKey,");
		buildExp("      sum(OtherAlias.OtherNumericField) as AnySum,");
		buildExp("      OtherAlias.OtherUnitField         as Unit");
		buildExp("}");
		buildExp("    where OtherAlias.OtherCategory = 'A'");
		buildExp("       or OtherAlias.OtherCategory = 'B'");
		buildExp("");
		buildExp("    group by OtherAlias.OtherKeyField,");
		buildExp("             OtherAlias.OtherNumericField,");
		buildExp("             OtherAlias.OtherUnitField");

		testRule();
	}
}
