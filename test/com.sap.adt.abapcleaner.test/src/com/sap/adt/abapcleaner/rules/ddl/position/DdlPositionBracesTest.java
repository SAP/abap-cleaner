package com.sap.adt.abapcleaner.rules.ddl.position;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlPositionBracesTest extends RuleTestBase {
	private DdlPositionBracesRule rule;
	
	DdlPositionBracesTest() {
		super(RuleID.DDL_POSITION_BRACES);
		rule = (DdlPositionBracesRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configBreakBeforeOpeningBrace.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configOpeningBraceIndent.setValue(0);
		
		rule.configBreakBeforeClosingBrace.setEnumValue(DdlLineBreak.ALWAYS);
		rule.configClosingBraceIndent.setValue(0);
		
		rule.configBreakBeforeFrom.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
		rule.configFromIndent.setValue(2);
	}

	@Test
	void testAlwaysBreakBeforeBraces() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  {");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("      }");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity As OtherAlias {");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("  }");
		buildSrc("");
		buildSrc("except");
		buildSrc("  select from I_ThirdEntity As ThirdAlias");
		buildSrc("  {");
		buildSrc("  key ThirdAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField }");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity As OtherAlias");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("except");
		buildExp("  select from I_ThirdEntity As ThirdAlias");
		buildExp("{");
		buildExp("  key ThirdAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepOpeningBraceAsIs() {
		rule.configBreakBeforeOpeningBrace.setEnumValue(DdlLineBreak.KEEP_AS_IS);
		rule.configOpeningBraceIndent.setValue(2);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  {");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("      }");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity As OtherAlias {");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("  }");
		buildSrc("");
		buildSrc("except");
		buildSrc("  select from I_ThirdEntity As ThirdAlias");
		buildSrc("  {");
		buildSrc("  key ThirdAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField }");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  {");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity As OtherAlias {");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("except");
		buildExp("  select from I_ThirdEntity As ThirdAlias");
		buildExp("  {");
		buildExp("  key ThirdAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverBreakBeforeOpeningBrace() {
		rule.configBreakBeforeOpeningBrace.setEnumValue(DdlLineBreak.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  {");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("      }");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity As OtherAlias {");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("  }");
		buildSrc("");
		buildSrc("except");
		buildSrc("  select from I_ThirdEntity As ThirdAlias");
		buildSrc("  {");
		buildSrc("  key ThirdAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField }");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias {");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity As OtherAlias {");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("except");
		buildExp("  select from I_ThirdEntity As ThirdAlias {");
		buildExp("  key ThirdAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepClosingBraceAsIs() {
		rule.configBreakBeforeClosingBrace.setEnumValue(DdlLineBreak.KEEP_AS_IS);
		rule.configClosingBraceIndent.setValue(2);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  {");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("      }");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity As OtherAlias {");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("  }");
		buildSrc("");
		buildSrc("except");
		buildSrc("  select from I_ThirdEntity As ThirdAlias");
		buildSrc("  {");
		buildSrc("  key ThirdAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField }");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("  }");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity As OtherAlias");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("  }");
		buildExp("");
		buildExp("except");
		buildExp("  select from I_ThirdEntity As ThirdAlias");
		buildExp("{");
		buildExp("  key ThirdAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField }");

		testRule();
	}

	@Test
	void testNeverBreakBeforeClosingBrace() {
		// also ensure that the first }, which cannot move due to the line-end comment, is instead indented correctly
		rule.configBreakBeforeClosingBrace.setEnumValue(DdlLineBreak.NEVER);
		rule.configClosingBraceIndent.setValue(4);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  {");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField // comment");
		buildSrc("      }");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherEntity As OtherAlias {");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("  }");
		buildSrc("");
		buildSrc("except");
		buildSrc("  select from I_ThirdEntity As ThirdAlias");
		buildSrc("  {");
		buildSrc("  key ThirdAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField }");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField // comment");
		buildExp("    }");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_OtherEntity As OtherAlias");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField }");
		buildExp("");
		buildExp("except");
		buildExp("  select from I_ThirdEntity As ThirdAlias");
		buildExp("{");
		buildExp("  key ThirdAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField }");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeFrom() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select distinct");
		buildSrc("    key AnyField,");
		buildSrc("        OtherField,");
		buildSrc("        ThirdField from I_OtherView");

		buildExp("define view I_AnyView");
		buildExp("  as select distinct");
		buildExp("    key AnyField,");
		buildExp("        OtherField,");
		buildExp("        ThirdField");
		buildExp("  from I_OtherView");

		testRule();
	}

	@Test
	void testKeepFromAsIs() {
		rule.configBreakBeforeFrom.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);

		buildSrc("define view I_AnyView");
		buildSrc("  as select distinct");
		buildSrc("    key AnyField,");
		buildSrc("        OtherField,");
		buildSrc("        ThirdField from I_OtherView");

		copyExpFromSrc();

		testRule();
	}
}
