package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundBracketsRule;
import com.sap.adt.abapcleaner.rules.ddl.spaces.DdlSpacesAroundSignsRule;

public class DdlAlignSourceParametersTest extends RuleTestBase {
	private DdlAlignSourceParametersRule rule;
	private DdlSpacesAroundSignsRule spacesAroundSignsRule;
	private DdlSpacesAroundBracketsRule spacesAroundBracketsRule;
	
	DdlAlignSourceParametersTest() {
		super(RuleID.DDL_ALIGN_SOURCE_PARAMETERS);

		rule = (DdlAlignSourceParametersRule)getRule();
		spacesAroundSignsRule = (DdlSpacesAroundSignsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_SIGNS);
		spacesAroundBracketsRule = (DdlSpacesAroundBracketsRule)profile.getRule(RuleID.DDL_SPACES_AROUND_BRACKETS); 
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.CONTINUE);
		rule.configAlignAssignmentOps.setValue(true);
		rule.configAlignActualParams.setValue(true);
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.CONTINUE);

		// setup configuration of other rules that is reused by the rule under test
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.KEEP_AS_IS);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.ALWAYS);
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.NEVER);
	}

	@Test
	void testParamsContinueAfterOpeningParens() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam       : any_parameter_type,");
		buildSrc("    P_OtherParam     : other_type,");
		buildSrc("    P_ThirdParameter : third_type,");
		buildSrc("    P_FourthParam    : fourth_type");
		buildSrc("");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as OtherAlias");
		buildSrc("");
		buildSrc("    inner join I_ThirdEntity");
		buildSrc("          (P_AnyParam  :  $parameters.P_AnyParam,");
		buildSrc("          P_OtherParam : $parameters.P_OtherParam,");
		buildSrc("          P_ThirdParameter:$parameters.P_ThirdParameter)");
		buildSrc("          as ThirdAlias");
		buildSrc("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildSrc("{");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField");
		buildSrc("}");
		buildSrc("union all");
		buildSrc("  select from I_FourthEntity( P_AnyParam   :");
		buildSrc("         $parameters.P_AnyParam, P_OtherParam        :");
		buildSrc("         $parameters.P_OtherParam, P_ThirdParameter :");
		buildSrc("         $parameters.P_ThirdParameter, P_FourthParam:");
		buildSrc("         $parameters.P_FourthParam  ) as FourthAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key FourthAlias.AnyKeyField,");
		buildSrc("      FourthAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  with parameters");
		buildExp("    P_AnyParam       : any_parameter_type,");
		buildExp("    P_OtherParam     : other_type,");
		buildExp("    P_ThirdParameter : third_type,");
		buildExp("    P_FourthParam    : fourth_type");
		buildExp("");
		buildExp("  as select from I_OtherEntity(P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam    : $parameters.P_FourthParam) as OtherAlias");
		buildExp("");
		buildExp("    inner join I_ThirdEntity(P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                             P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                             P_ThirdParameter : $parameters.P_ThirdParameter) as ThirdAlias");
		buildExp("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("union all");
		buildExp("  select from I_FourthEntity(P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                             P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                             P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                             P_FourthParam    : $parameters.P_FourthParam) as FourthAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key FourthAlias.AnyKeyField,");
		buildExp("      FourthAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowLineStartPlus2() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("    P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("    P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("    // comment");
		buildExp("    P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("    P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowLineStartPlus4() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_4);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("      P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("      P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("      // comment");
		buildExp("      P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("      P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowSourceNamePlus2() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.SOURCE_NAME_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("                   P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                   P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                   // comment");
		buildExp("                   P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                   P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowSourceNamePlus4() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.SOURCE_NAME_PLUS_4);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("                     P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                     P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                     // comment");
		buildExp("                     P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                     P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testParamsBelowFirstParamAsIs() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.KEEP_AS_IS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("   P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("   P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("   // comment");
		buildExp("   P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("   P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignColons() {
		rule.configAlignAssignmentOps.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam:       $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam:     $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter: $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam:    $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignActualParameters() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam: $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam: $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter: $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam: $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignPutSpaceBeforeColon() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam : $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam : $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignNoSpaceAroundColon() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.NEVER);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.NEVER);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam:$parameters.P_AnyParam,");
		buildExp("                               P_OtherParam:$parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter:$parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam:$parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesInsideParentheses() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity( P_AnyParam: $parameters.P_AnyParam,");
		buildExp("                                P_OtherParam: $parameters.P_OtherParam,");
		buildExp("                                // comment");
		buildExp("                                P_ThirdParameter: $parameters.P_ThirdParameter,");
		buildExp("                                P_FourthParam: $parameters.P_FourthParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAsAliasBelowLineStart() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_2);
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.LINE_START);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("    P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("    P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("    // comment");
		buildExp("    P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("    P_FourthParam    : $parameters.P_FourthParam)");
		buildExp("  as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAsAliasBelowLineStartPlus2() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_4);
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.LINE_START_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("      P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("      P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("      // comment");
		buildExp("      P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("      P_FourthParam    : $parameters.P_FourthParam)");
		buildExp("    as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAsAliasBelowViewName() {
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.SOURCE_NAME);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam    : $parameters.P_FourthParam)");
		buildExp("                 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAsAliasBelowViewNamePlus2() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_2);
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.SOURCE_NAME_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("    P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("    P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("    // comment");
		buildExp("    P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("    P_FourthParam    : $parameters.P_FourthParam)");
		buildExp("                   as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepAsAliasAsIs() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.LINE_START_PLUS_2);
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.KEEP_AS_IS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("    P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("    P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("    // comment");
		buildExp("    P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("    P_FourthParam    : $parameters.P_FourthParam)");
		buildExp("   as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testCommentsBeforeParentheses() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity // comment");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam -- comment");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity( // comment");
		buildExp("                               P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam    : $parameters.P_FourthParam -- comment");
		buildExp("                              ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testCommentsAboveParentheses() {
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  -- comment 2");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc("  -- comment");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("                                -- comment 2");
		buildExp("                                P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                                P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                                // comment");
		buildExp("                                P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                                P_FourthParam    : $parameters.P_FourthParam");
		buildExp("                                -- comment");
		buildExp("                              ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testCommentsAboveParenthesesParamsBelowSourcePlus2() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.SOURCE_NAME_PLUS_2);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("  -- comment 2");
		buildSrc("  (");
		buildSrc("   P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam");
		buildSrc("  -- comment");
		buildSrc(" )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("                   -- comment 2");
		buildExp("                   P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                   P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                   // comment");
		buildExp("                   P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                   P_FourthParam    : $parameters.P_FourthParam");
		buildExp("                   -- comment");
		buildExp("                   ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepUnevenSpacesInsideFuncParens() {
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity(P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam )");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                               P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                               // comment");
		buildExp("                               P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                               P_FourthParam    : $parameters.P_FourthParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepUnevenSpacesInsideFuncParens2() {
		spacesAroundBracketsRule.configSpacesInsideFuncParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity( P_AnyParam : $parameters.P_AnyParam,");
		buildSrc("     P_OtherParam    :   $parameters.P_OtherParam,");
		buildSrc("    // comment");
		buildSrc(" P_ThirdParameter        :");
		buildSrc("    $parameters.P_ThirdParameter,");
		buildSrc("  P_FourthParam");
		buildSrc("  : $parameters.P_FourthParam)");
		buildSrc("   as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity( P_AnyParam       : $parameters.P_AnyParam,");
		buildExp("                                P_OtherParam     : $parameters.P_OtherParam,");
		buildExp("                                // comment");
		buildExp("                                P_ThirdParameter : $parameters.P_ThirdParameter,");
		buildExp("                                P_FourthParam    : $parameters.P_FourthParam) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testEmptyParentheses() {
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity( ) as OtherAlias");
		buildSrc("");
		buildSrc("    inner join I_ThirdEntity( ) as ThirdAlias");
		buildSrc("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildSrc("{");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField");
		buildSrc("}");
		buildSrc("union all");
		buildSrc("  select from I_FourthEntity( ) as FourthAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key FourthAlias.AnyKeyField,");
		buildSrc("      FourthAlias.AnyNonKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testOnlyCommentInParentheses() {
		rule.configAsAliasPos.setEnumValue(DdlNextAfterParensPos.SOURCE_NAME);

		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("                   ( /* comment */ )");
		buildSrc("   as OtherAlias");
		buildSrc("");
		buildSrc("    inner join I_ThirdEntity(   // comment");
		buildSrc("                              ) as ThirdAlias");
		buildSrc("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildSrc("{");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity(");
		buildExp("                   /* comment */ )");
		buildExp("                 as OtherAlias");
		buildExp("");
		buildExp("    inner join I_ThirdEntity(   // comment");
		buildExp("                              )");
		buildExp("               as ThirdAlias");
		buildExp("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpaceAfterColonDoNotAlign() {
		rule.configAlignAssignmentOps.setValue(false);
		rule.configAlignActualParams.setValue(false);
		spacesAroundSignsRule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);
		spacesAroundSignsRule.configSpaceAfterColon.setEnumValue(ChangeType.NEVER);
		
		buildSrc("define view entity I_AnyEntity");
		buildSrc("  as select from I_OtherEntity as OtherAlias");
		buildSrc("");
		buildSrc("    inner join I_ThirdEntity");
		buildSrc("          (P_AnyParam  :  $parameters.P_AnyParam,");
		buildSrc("//        P_OtherParam : $parameters.P_OtherParam,");
		buildSrc("//        P_ThirdParam:$parameters.P_ThirdParameter,");
		buildSrc("          P_FourthParameter: $parameters.P_FourthParam)");
		buildSrc("          as ThirdAlias");
		buildSrc("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildSrc("{");
		buildSrc("  key OtherAlias.AnyKeyField,");
		buildSrc("      ThirdAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity I_AnyEntity");
		buildExp("  as select from I_OtherEntity as OtherAlias");
		buildExp("");
		buildExp("    inner join I_ThirdEntity(P_AnyParam :$parameters.P_AnyParam,");
		buildExp("//                             P_OtherParam :$parameters.P_OtherParam,");
		buildExp("//                             P_ThirdParam :$parameters.P_ThirdParameter,");
		buildExp("                             P_FourthParameter :$parameters.P_FourthParam) as ThirdAlias");
		buildExp("      on OtherAlias.AnyKeyField = ThirdAlias.AnyKeyField");
		buildExp("{");
		buildExp("  key OtherAlias.AnyKeyField,");
		buildExp("      ThirdAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAsAliasMissingAssociationNextLine() {
		// ensure that ASSOCIATION ... TO ... is NOT moved behind the parameter assignments if "AS <alias>" is missing
		
		buildSrc("define view P_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_Any : any_type,");
		buildSrc("");
		buildSrc("  as select from P_OtherView(");
		buildSrc("                   P_Any : :P_Any )");
		buildSrc("                   as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..1] to I_ThirdView as _Third on AnyAlias.AnyKeyField = _Third.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from P_FourthView(");
		buildSrc("                P_Any : :P_Any )");
		buildSrc("");
		buildSrc("  association [0..1] to I_FifthView as _Fifth on AnyAlias.AnyKeyField = _Fourth.AnyKeyField");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view P_AnyView");
		buildExp("  with parameters");
		buildExp("    P_Any : any_type,");
		buildExp("");
		buildExp("  as select from P_OtherView(P_Any : :P_Any) as AnyAlias");
		buildExp("");
		buildExp("  association [0..1] to I_ThirdView as _Third on AnyAlias.AnyKeyField = _Third.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from P_FourthView(P_Any : :P_Any)");
		buildExp("");
		buildExp("  association [0..1] to I_FifthView as _Fifth on AnyAlias.AnyKeyField = _Fourth.AnyKeyField");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAnalyticalQuery() {
		rule.configParameterPos.setEnumValue(DdlSourceParamPos.SOURCE_NAME_PLUS_2);

		buildSrc("define transient view entity C_AnyQuery");
		buildSrc("  provider contract analytical_query");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParameter   : any_type,");
		buildSrc("    P_OtherParameter : other_type");
		buildSrc("");
		buildSrc("  as projection on I_AnyCube(");
		buildSrc("           P_AnyParameter   : $parameters.P_AnyParameter,");
		buildSrc("         P_OtherParameter:  $parameters.P_OtherParameter) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define transient view entity C_AnyQuery");
		buildExp("  provider contract analytical_query");
		buildExp("  with parameters");
		buildExp("    P_AnyParameter   : any_type,");
		buildExp("    P_OtherParameter : other_type");
		buildExp("");
		buildExp("  as projection on I_AnyCube(");
		buildExp("                     P_AnyParameter   : $parameters.P_AnyParameter,");
		buildExp("                     P_OtherParameter : $parameters.P_OtherParameter) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}
}
