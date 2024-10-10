package com.sap.adt.abapcleaner.rules.ddl.position;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlPositionDefineTest extends RuleTestBase {
	private DdlPositionDefineRule rule;
	
	DdlPositionDefineTest() {
		super(RuleID.DDL_POSITION_DEFINE);
		rule = (DdlPositionDefineRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configBreakBeforeDefine.setEnumValue(DdlLineBreakWithoutNever.ALWAYS);
	   rule.configDefineIndent.setValue(0);

	   rule.configBreakBeforeEntityName.setEnumValue(DdlLineBreak.NEVER);
	   rule.configEntityNameIndent.setValue(2);
		
	   rule.configBreakBeforeWithParams.setEnumValue(DdlLineBreak.ALWAYS);
	   rule.configWithParamsIndent.setValue(2);
	   rule.configParamsIndent.setValue(4);
	}

	@Test
	void testAlwaysBreakBeforeDefine() {
		rule.configDefineIndent.setValue(1);

		buildSrc("@EndUserText.label: 'Any Description' define");
		buildSrc("   view");
		buildSrc(" entity");
		buildSrc("C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp(" define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepDefineAsIs() {
		rule.configBreakBeforeDefine.setEnumValue(DdlLineBreakWithoutNever.KEEP_AS_IS);
		rule.configDefineIndent.setValue(4);

		buildSrc("@EndUserText.label: 'Any Description' define");
		buildSrc("   view");
		buildSrc(" entity");
		buildSrc("C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description' define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverBreakBeforeEntityName() {
		buildSrc("define");
		buildSrc("view");
		buildSrc("entity");
		buildSrc("C_AnyEntity");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}
	
	@Test
	void testAlwaysBreakBeforeEntityName() {
		rule.configBreakBeforeEntityName.setEnumValue(DdlLineBreak.ALWAYS);

		buildSrc("define");
		buildSrc("view");
		buildSrc("entity C_AnyEntity");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity");
		buildExp("  C_AnyEntity");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepEntityName() {
		rule.configBreakBeforeEntityName.setEnumValue(DdlLineBreak.KEEP_AS_IS);

		buildSrc("define");
		buildSrc("view");
		buildSrc("entity C_AnyEntity");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysBreakBeforeWithParameters() {
		buildSrc("define view entity C_AnyEntity with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("     @Anno.subAnno: 'value'");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("          /* multi-");
		buildSrc("           * line");
		buildSrc("           * comment */");
		buildSrc("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    @Anno.subAnno: 'value'");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    /* multi-");
		buildExp("     * line");
		buildExp("     * comment */");
		buildExp("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepWithParameters() {
		rule.configBreakBeforeWithParams.setEnumValue(DdlLineBreak.KEEP_AS_IS);

		buildSrc("define view entity C_AnyEntity with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("     @Anno.subAnno: 'value'");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    /* comment in correct position */");
		buildSrc("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    @Anno.subAnno: 'value'");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    /* comment in correct position */");
		buildExp("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverBreakBeforeWithParameters() {
		rule.configBreakBeforeWithParams.setEnumValue(DdlLineBreak.NEVER);
		rule.configWithParamsIndent.setValue(10);

		buildSrc("define view entity C_AnyEntity with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("     @Anno.subAnno: 'value'");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    @Anno.subAnno: 'value'");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    P_ThirdParam : ThirdType as select from I_AnyEntity as AnyAlias");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testMoveAttachedComments() {
		// ensure that the attached comments are moved along with "WITH PARAMETERS"
		buildSrc("define view entity C_AnyEntity");
		buildSrc("// single-line comment");
		buildSrc("/* multi-line");
		buildSrc("   comment */");
		buildSrc("with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    P_ThirdParam : ThirdType");
		buildSrc("  // comment on select");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  // single-line comment");
		buildExp("  /* multi-line");
		buildExp("     comment */");
		buildExp("  with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    P_ThirdParam : ThirdType");
		buildExp("  // comment on select");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotMoveDetachedComments() {
		// ensure that the detached comments are not moved along with "WITH PARAMETERS"
		buildSrc("define view entity C_AnyEntity");
		buildSrc("// detached comment");
		buildSrc("");
		buildSrc("with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    P_ThirdParam : ThirdType");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("// detached comment");
		buildExp("");
		buildExp("  with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    P_ThirdParam : ThirdType");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotMoveCommentedOutAnnotations() {
		// ensure that the commented-out annotations are not moved along with "WITH PARAMETERS"
		buildSrc("define view entity C_AnyEntity");
		buildSrc("// @Anno.subAnno");
		buildSrc("      // single-line comment");
		buildSrc("with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    P_ThirdParam : ThirdType");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("// @Anno.subAnno");
		buildExp("  // single-line comment");
		buildExp("  with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    P_ThirdParam : ThirdType");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotMoveComments() {
		// ensure that neither the commented-out annotation nor the comment above it is being moved
		buildSrc("define view entity C_AnyEntity");
		buildSrc("      // single-line comment");
		buildSrc("// @Anno.subAnno");
		buildSrc("with");
		buildSrc("parameters");
		buildSrc("// comment");
		buildSrc("P_AnyParam   : AnyType,");
		buildSrc("  P_OtherParam : OtherType,");
		buildSrc("    P_ThirdParam : ThirdType");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      AnyAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("      // single-line comment");
		buildExp("// @Anno.subAnno");
		buildExp("  with parameters");
		buildExp("    // comment");
		buildExp("    P_AnyParam   : AnyType,");
		buildExp("    P_OtherParam : OtherType,");
		buildExp("    P_ThirdParam : ThirdType");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      AnyAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testMultiCommentLineBeforeDefine() {
	   // ensure that the empty line is added above the comment, not above "define" 
		buildSrc("@Analytics.query: true");
		buildSrc("/*+[hideWarning] { \"IDS\" : [ \"KEY_CHECK\" ]  } */  // any comment");
		buildSrc("define view entity C_AnyQuery");
		buildSrc("  as select from I_AnySource");
		buildSrc("{");
		buildSrc("  AnyField,");
		buildSrc("  OtherField");
		buildSrc("}");

		buildExp("@Analytics.query: true");
		buildExp("");
		buildExp("/*+[hideWarning] { \"IDS\" : [ \"KEY_CHECK\" ]  } */  // any comment");
		buildExp("define view entity C_AnyQuery");
		buildExp("  as select from I_AnySource");
		buildExp("{");
		buildExp("  AnyField,");
		buildExp("  OtherField");
		buildExp("}");

		testRule();
	}
}