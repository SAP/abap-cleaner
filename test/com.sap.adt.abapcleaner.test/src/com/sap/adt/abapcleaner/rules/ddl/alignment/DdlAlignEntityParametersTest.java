package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rules.ddl.position.DdlPositionDefineRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public class DdlAlignEntityParametersTest extends RuleTestBase {
	private DdlAlignEntityParametersRule rule;
	private DdlPositionDefineRule positionDefineRule;
	private DdlSpacesAroundSignsRule spacesAroundSignsRule;
	
	DdlAlignEntityParametersTest() {
		super(RuleID.DDL_ALIGN_ENTITY_PARAMETERS);
		
		rule = (DdlAlignEntityParametersRule)getRule();
		positionDefineRule = (DdlPositionDefineRule)profile.getRule(RuleID.DDL_POSITION_DEFINE);
		spacesAroundSignsRule = (DdlSpacesAroundSignsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_SIGNS);
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignColons.setValue(true);
		rule.configAlignTypes.setValue(true);

		// setup configuration of other rules that is reused by the rule under test
		positionDefineRule.configParamsIndent.setValue(4);
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.KEEP_AS_IS);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.ALWAYS);
	}

	@Test
	void testNoParameters() {
		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    left outer to one join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlignAll() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 30");
		buildSrc("    P_AnyParam: any_parameter_type,");
		buildSrc("");
		buildSrc("    // comment on OtherParam");
		buildSrc("    P_OtherParam :           other_type,");
		buildSrc("");
		buildSrc("    P_ThirdParameter   : third_type,");
		buildSrc("    P_FourthParam:fourth_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    @AnalyticsDetails.query.variableSequence: 30");
		buildExp("    P_AnyParam       : any_parameter_type,");
		buildExp("");
		buildExp("    // comment on OtherParam");
		buildExp("    P_OtherParam     : other_type,");
		buildExp("");
		buildExp("    P_ThirdParameter : third_type,");
		buildExp("    P_FourthParam    : fourth_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignColons() {
		rule.configAlignColons.setValue(false);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 30");
		buildSrc("    P_AnyParam: any_parameter_type,");
		buildSrc("");
		buildSrc("    // comment on OtherParam");
		buildSrc("    P_OtherParam :           other_type,");
		buildSrc("");
		buildSrc("    P_ThirdParameter   : third_type,");
		buildSrc("    P_FourthParam:fourth_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    @AnalyticsDetails.query.variableSequence: 30");
		buildExp("    P_AnyParam:       any_parameter_type,");
		buildExp("");
		buildExp("    // comment on OtherParam");
		buildExp("    P_OtherParam:     other_type,");
		buildExp("");
		buildExp("    P_ThirdParameter: third_type,");
		buildExp("    P_FourthParam:    fourth_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testJoinAll() {
		rule.configAlignColons.setValue(false);
		rule.configAlignTypes.setValue(false);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 30");
		buildSrc("    P_AnyParam: any_parameter_type,");
		buildSrc("");
		buildSrc("    // comment on OtherParam");
		buildSrc("    P_OtherParam :           other_type,");
		buildSrc("");
		buildSrc("    P_ThirdParameter   : third_type,");
		buildSrc("    P_FourthParam:fourth_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    @AnalyticsDetails.query.variableSequence: 30");
		buildExp("    P_AnyParam: any_parameter_type,");
		buildExp("");
		buildExp("    // comment on OtherParam");
		buildExp("    P_OtherParam: other_type,");
		buildExp("");
		buildExp("    P_ThirdParameter: third_type,");
		buildExp("    P_FourthParam: fourth_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpaceBeforeButNotAfterColon() {
		rule.configAlignColons.setValue(false);
		rule.configAlignTypes.setValue(false);

		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.NEVER);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 30");
		buildSrc("    P_AnyParam: any_parameter_type,");
		buildSrc("");
		buildSrc("    // comment on OtherParam");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 50");
		buildSrc("    P_OtherParam :           other_type,");
		buildSrc("");
		buildSrc("    @AnalyticsDetails.query.variableSequence: 60");
		buildSrc("    @Consumption.defaultValue: 'NN'");
		buildSrc("    P_ThirdParameter   : third_type,");
		buildSrc("");
		buildSrc("    @Consumption.hidden: true");
		buildSrc("    P_FourthParam:fourth_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    left outer to one join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    @AnalyticsDetails.query.variableSequence: 30");
		buildExp("    P_AnyParam :any_parameter_type,");
		buildExp("");
		buildExp("    // comment on OtherParam");
		buildExp("    @AnalyticsDetails.query.variableSequence: 50");
		buildExp("    P_OtherParam :other_type,");
		buildExp("");
		buildExp("    @AnalyticsDetails.query.variableSequence: 60");
		buildExp("    @Consumption.defaultValue: 'NN'");
		buildExp("    P_ThirdParameter :third_type,");
		buildExp("");
		buildExp("    @Consumption.hidden: true");
		buildExp("    P_FourthParam :fourth_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testIndentByEightSpaces() {
		positionDefineRule.configParamsIndent.setValue(8);
		
		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam: any_parameter_type,");
		buildSrc("    P_OtherParam :           other_type,");
		buildSrc("    P_ThirdParameter   : third_type,");
		buildSrc("    P_FourthParam:fourth_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    left outer to one join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("        P_AnyParam       : any_parameter_type,");
		buildExp("        P_OtherParam     : other_type,");
		buildExp("        P_ThirdParameter : third_type,");
		buildExp("        P_FourthParam    : fourth_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    left outer to one join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}
}
