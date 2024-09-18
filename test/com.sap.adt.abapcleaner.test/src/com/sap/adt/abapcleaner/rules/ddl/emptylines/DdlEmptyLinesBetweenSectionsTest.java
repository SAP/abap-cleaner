package com.sap.adt.abapcleaner.rules.ddl.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlEmptyLinesBetweenSectionsTest extends RuleTestBase {
	private DdlEmptyLinesBetweenSectionsRule rule;
	
	DdlEmptyLinesBetweenSectionsTest() {
		super(RuleID.DDL_EMPTY_LINES_BETWEEN);

		rule = (DdlEmptyLinesBetweenSectionsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configBetweenEntityAnnosAndDefine.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
		rule.configBetweenParametersAndAsSelect.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
		rule.configBetweenSelectFromAndJoins.setEnumValue(DdlEmptyLineType.ALWAYS_EXACTLY_ONE);
		rule.configBetweenJoinsAndAssociations.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

		rule.configBeforeSelectListStart.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);
		rule.configAfterSelectListStart.setEnumValue(DdlEmptyLineType.NEVER);
		rule.configBeforeSelectListEnd.setEnumValue(DdlEmptyLineType.NEVER);
		rule.configAfterSelectListEnd.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

		rule.configRemoveAtDocumentEnd.setValue(true);
		rule.configMaxConsecutiveEmptyLines.setValue(2);
	}

	@Test
	void testNoAnnosBeforeEntity() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineBetweenEntityAnnosAndDefine() {
		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testExactlyOneLineBetweenEntityAnnosAndDefine() {
		rule.configBetweenEntityAnnosAndDefine.setEnumValue(DdlEmptyLineType.ALWAYS_EXACTLY_ONE);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepLinesBetweenEntityAnnosAndDefine() {
		rule.configBetweenEntityAnnosAndDefine.setEnumValue(DdlEmptyLineType.KEEP_AS_IS);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoLinesBetweenEntityAnnosAndDefine() {
		rule.configBetweenEntityAnnosAndDefine.setEnumValue(DdlEmptyLineType.NEVER);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineBetweenParametersAndAsSelect() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    P_AnyParam:   any_parameter_type,");
		buildExp("    P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoLineBetweenParametersAndAsSelect() {
		rule.configBetweenParametersAndAsSelect.setEnumValue(DdlEmptyLineType.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    P_AnyParam:   any_parameter_type,");
		buildExp("    P_OtherParam: other_type");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoParameters() {
		rule.configBetweenParametersAndAsSelect.setEnumValue(DdlEmptyLineType.ALWAYS_EXACTLY_ONE);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testExactlyOneLineBetweenSelectFromAndJoins() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoJoin() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildExp("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoEmptyLineBetweenSelectFromAndAssociations() {
		rule.configBetweenSelectFromAndJoins.setEnumValue(DdlEmptyLineType.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildExp("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testExactlyOneLineBetweenJoinsAndAssociations() {
		rule.configBetweenJoinsAndAssociations.setEnumValue(DdlEmptyLineType.ALWAYS_EXACTLY_ONE);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    // comment on association");
		buildSrc("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    // comment on association");
		buildExp("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildExp("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineBeforeSelectListStart() {
		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("/* comment */");
		buildSrc("-- comment");
		buildSrc("// comment");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("/* comment */");
		buildExp("-- comment");
		buildExp("// comment");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineBeforeSelectListStartKeeping2() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity as OtherAlias");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("");
		buildSrc("/* comment */");
		buildSrc("-- comment");
		buildSrc("// comment");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity as OtherAlias");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("");
		buildExp("/* comment */");
		buildExp("-- comment");
		buildExp("// comment");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testExactlyOneLineAfterSelectListStart() {
		rule.configAfterSelectListStart.setEnumValue(DdlEmptyLineType.ALWAYS_EXACTLY_ONE);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  -- comment");
		buildSrc("  @Annotation.anno: 'value'");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("");
		buildExp("  -- comment");
		buildExp("  @Annotation.anno: 'value'");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineBeforeSelectListEnd() {
		rule.configBeforeSelectListEnd.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoLineBeforeSelectListEnd() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAtLeastOneLineAfterSelectListEnd() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("");
		buildSrc("where AnyNonKeyField > 10");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnyEntity2 as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("} where AnyNonKeyField > 10");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("");
		buildExp("where AnyNonKeyField > 10");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnyEntity2 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("} where AnyNonKeyField > 10");

		testRule();
	}

	@Test
	void testMax1ConsecutiveEmptyLines() {
		// expect multiple consecutive empty lines to be removed both between Commands and between Tokens of the same Command
		rule.configMaxConsecutiveEmptyLines.setValue(1);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity as OtherAlias");
		buildSrc("");
		buildSrc("");
		buildSrc("      on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildSrc("      on  AnyAlias.IdField    = ThirdAlias.IdField");
		buildSrc("      and AnyAlias.SubIdField = ThirdAlias.SubIdField");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("    // comment on association");
		buildSrc("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      @Annotation.subAnno:");
		buildSrc("");
		buildSrc("");
		buildSrc("      'value'");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");
		buildSrc("where AnyNonKeyField > 10");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    P_AnyParam:   any_parameter_type,");
		buildExp("    P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity as OtherAlias");
		buildExp("");
		buildExp("      on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("      on  AnyAlias.IdField    = ThirdAlias.IdField");
		buildExp("      and AnyAlias.SubIdField = ThirdAlias.SubIdField");
		buildExp("");
		buildExp("    // comment on association");
		buildExp("    association [0..*] to I_FourthEntity as _FourthAlias");
		buildExp("      on AnyAlias.IdField = _FourthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      @Annotation.subAnno:");
		buildExp("");
		buildExp("      'value'");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("where AnyNonKeyField > 10");

		testRule();
	}

	@Test
	void testDDicBasedSelectListBeforeFrom() {
		// ensure that the options are mapped to DDIC-based SELECT [DISTINCT] select_list FROM ... syntax, where it makes sense
		buildSrc("@EndUserText.label: 'any text'");
		buildSrc("define view I_AnyDdicView(");
		buildSrc("  AnyFieldAlias,");
		buildSrc("  OtherFieldAlias,");
		buildSrc("  ThirdFieldSum");
		buildSrc(")");
		buildSrc("as select distinct");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      @EndUserText.label: 'any label'");
		buildSrc("      OtherAlias.OtherField,");
		buildSrc("      sum(_ThirdAlias.ThirdField)");
		buildSrc("from I_AnySource as AnyAlias");
		buildSrc("  // comment");
		buildSrc("  left outer join I_OtherSource as OtherAlias");
		buildSrc("    on AnyAlias.AnyKeyField = OtherAlias.AnyKeyField");
		buildSrc("  // comment");
		buildSrc("  association [0..*] to I_ThirdSource as _ThirdAlias");
		buildSrc("    on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("group by AnyAlias.AnyKeyField,");
		buildSrc("         OtherAilas.OtherField");

		buildExp("@EndUserText.label: 'any text'");
		buildExp("");
		buildExp("define view I_AnyDdicView(");
		buildExp("  AnyFieldAlias,");
		buildExp("  OtherFieldAlias,");
		buildExp("  ThirdFieldSum");
		buildExp(")");
		buildExp("as select distinct");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("      @EndUserText.label: 'any label'");
		buildExp("      OtherAlias.OtherField,");
		buildExp("      sum(_ThirdAlias.ThirdField)");
		buildExp("");
		buildExp("from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("  // comment");
		buildExp("  left outer join I_OtherSource as OtherAlias");
		buildExp("    on AnyAlias.AnyKeyField = OtherAlias.AnyKeyField");
		buildExp("");
		buildExp("  // comment");
		buildExp("  association [0..*] to I_ThirdSource as _ThirdAlias");
		buildExp("    on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("group by AnyAlias.AnyKeyField,");
		buildExp("         OtherAilas.OtherField");

		testRule();
	}

	@Test
	void testNonAttachedCommentAfterSelectListStartAndEnd() {
		// ensure that the empty line is added directly below "{" and "}", not above the 'attached comments'
		
		rule.configAfterSelectListStart.setEnumValue(DdlEmptyLineType.ALWAYS_AT_LEAST_ONE);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("{");
		buildSrc("  // comment");
		buildSrc("");
		buildSrc("  // attached comment");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");
		buildSrc("-- comment");
		buildSrc("");
		buildSrc("-- attached comment");
		buildSrc("group by AnyAlias.AnyKeyField");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("");
		buildExp("  // comment");
		buildExp("");
		buildExp("  // attached comment");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");
		buildExp("");
		buildExp("-- comment");
		buildExp("");
		buildExp("-- attached comment");
		buildExp("group by AnyAlias.AnyKeyField");

		testRule();
	}
}
