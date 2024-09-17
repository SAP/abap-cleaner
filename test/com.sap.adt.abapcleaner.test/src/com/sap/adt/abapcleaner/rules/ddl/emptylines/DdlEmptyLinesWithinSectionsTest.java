package com.sap.adt.abapcleaner.rules.ddl.emptylines;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlEmptyLinesWithinSectionsTest extends RuleTestBase {
	private DdlEmptyLinesWithinSectionsRule rule;
	
	DdlEmptyLinesWithinSectionsTest() {
		super(RuleID.DDL_EMPTY_LINES_WITHIN);

		rule = (DdlEmptyLinesWithinSectionsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configSurroundParameterLineCountMin.setValue(2);
		rule.configSurroundJoinLineCountMin.setValue(2);
		rule.configSurroundAssociationLineCountMin.setValue(2);
		rule.configSurroundElementLineCountMin.setValue(2);
		rule.configSurroundClauseLineCountMin.setValue(2);

		rule.configCondenseParameters.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);
		rule.configCondenseJoins.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);
		rule.configCondenseAssociations.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);
		rule.configCondenseClauses.setEnumValue(DdlCondenseMode.NEVER);
	}

	@Test
	void testSurroundParametersWith2Lines() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    @Annotation.subAnno: 'value'");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    P_AnyParam:   any_parameter_type,");
		buildExp("");
		buildExp("    @Annotation.subAnno: 'value'");
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
	void testSurroundFirstParameterWith2Lines() {
		// ensure that no empty line is inserted above the first parameter
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    @Annotation.subAnno: 'value'");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("    P_ThirdParam: third_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  with parameters");
		buildExp("    @Annotation.subAnno: 'value'");
		buildExp("    P_AnyParam:   any_parameter_type,");
		buildExp("");
		buildExp("    P_OtherParam: other_type");
		buildExp("    P_ThirdParam: third_type");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundParametersWith3Lines() {
		rule.configSurroundParameterLineCountMin.setValue(3);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("    @Annotation.subAnno: 'value'");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSurroundJoinsWith4Lines() {
		rule.configSurroundJoinLineCountMin.setValue(4);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity      as OtherAlias  on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("    // comment");
		buildSrc("    left outer join I_ThirdEntity as ThirdAlias  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField");
		buildSrc("                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField");
		buildSrc("    inner join FourthEntity       as FourthAlias on AnyAlias.IdField = Fourth.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("    inner join I_OtherEntity      as OtherAlias  on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("    // comment");
		buildExp("    left outer join I_ThirdEntity as ThirdAlias  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildExp("                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField");
		buildExp("                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField");
		buildExp("");
		buildExp("    inner join FourthEntity       as FourthAlias on AnyAlias.IdField = Fourth.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundJoinsWith5Lines() {
		rule.configSurroundJoinLineCountMin.setValue(5);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity      as OtherAlias  on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("    // comment");
		buildSrc("    left outer join I_ThirdEntity as ThirdAlias  on  AnyAlias.IdField      = ThirdAlias.IdField");
		buildSrc("                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField");
		buildSrc("                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField");
		buildSrc("    inner join FourthEntity       as FourthAlias on AnyAlias.IdField = Fourth.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAssociationsWith4Lines() {
		rule.configSurroundAssociationLineCountMin.setValue(4);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  /* comment on association");
		buildSrc("     more comment on association */");
		buildSrc("  association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("    on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("  association [0..*] to I_FifthEntity as _FifthAlias");
		buildSrc("    on AnyAlias.IdField = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("  /* comment on association");
		buildExp("     more comment on association */");
		buildExp("  association [0..*] to I_FourthEntity as _FourthAlias");
		buildExp("    on AnyAlias.IdField = _FourthAlias.IdField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthEntity as _FifthAlias");
		buildExp("    on AnyAlias.IdField = _FifthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAssociationsWith5Lines() {
		rule.configSurroundAssociationLineCountMin.setValue(5);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  /* comment on association");
		buildSrc("     more comment on association */");
		buildSrc("  association [0..*] to I_FourthEntity as _FourthAlias");
		buildSrc("    on AnyAlias.IdField = _FourthAlias.IdField");
		buildSrc("  association [0..*] to I_FifthEntity as _FifthAlias");
		buildSrc("    on AnyAlias.IdField = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSurroundElementsWith1Line() {
		rule.configSurroundElementLineCountMin.setValue(1);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildSrc("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildSrc("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildExp("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildExp("");
		buildExp("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildExp("");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("");
		buildExp("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundElementsWith2Lines() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildSrc("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildSrc("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildExp("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildExp("");
		buildExp("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundElementsWith3Lines() {
		rule.configSurroundElementLineCountMin.setValue(3);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("");
		buildSrc("");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildSrc("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildSrc("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("");
		buildExp("");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildExp("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildExp("");
		buildExp("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundClausesWith2Lines() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("where  AnyNonKeyField  > 10");
		buildSrc("  and OtherNonKeyField = 'A'");
		buildSrc("group by AnyAlias.AnyKeyField");
		buildSrc("         AnyAlias.OtherKeyField");
		buildSrc("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildSrc("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildSrc("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnyEntity2         as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("where  AnyNonKeyField  > 10");
		buildExp("  and OtherNonKeyField = 'A'");
		buildExp("");
		buildExp("group by AnyAlias.AnyKeyField");
		buildExp("         AnyAlias.OtherKeyField");
		buildExp("");
		buildExp("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildExp("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildExp("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnyEntity2         as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundClausesWith3Lines() {
		rule.configSurroundClauseLineCountMin.setValue(3);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("where  AnyNonKeyField  > 10");
		buildSrc("  and OtherNonKeyField = 'A'");
		buildSrc("group by AnyAlias.AnyKeyField");
		buildSrc("         AnyAlias.OtherKeyField");
		buildSrc("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildSrc("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildSrc("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnyEntity2         as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("where  AnyNonKeyField  > 10");
		buildExp("  and OtherNonKeyField = 'A'");
		buildExp("group by AnyAlias.AnyKeyField");
		buildExp("         AnyAlias.OtherKeyField");
		buildExp("");
		buildExp("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildExp("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildExp("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnyEntity2         as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSurroundClausesAtDocumentEndWith3Lines() {
		rule.configSurroundClauseLineCountMin.setValue(3);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("where  AnyNonKeyField  > 10");
		buildSrc("  and OtherNonKeyField = 'A'");
		buildSrc("group by AnyAlias.AnyKeyField");
		buildSrc("         AnyAlias.OtherKeyField");
		buildSrc("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildSrc("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildSrc("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");

		buildExp("define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("where  AnyNonKeyField  > 10");
		buildExp("  and OtherNonKeyField = 'A'");
		buildExp("group by AnyAlias.AnyKeyField");
		buildExp("         AnyAlias.OtherKeyField");
		buildExp("");
		buildExp("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildExp("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildExp("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");

		testRule();
	}

	@Test
	void testCondenseParametersIfAllAreOneLiners() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("");
		buildSrc("");
		buildSrc("    P_OtherParam: other_type");
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
		buildExp("");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverCondenseSingleLineParameters() {
		rule.configCondenseParameters.setEnumValue(DdlCondenseMode.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam:   any_parameter_type,");
		buildSrc("");
		buildSrc("");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlwaysCondenseSingleLineJoins() {
		rule.configCondenseJoins.setEnumValue(DdlCondenseMode.ALWAYS);

		buildSrc("@EndUserText.label: 'Any Description'");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("");
		buildSrc("  as select from    I_AnyEntity     as AnyAlias");
		buildSrc("    inner join      I_OtherEntity2  as OtherAlias  on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("");
		buildSrc("    left outer join I_ThirdEntity2  as ThirdAlias  on AnyAlias.IdField = ThirdAlias.IdField");
		buildSrc("");
		buildSrc("    inner join      I_FourthEntity2 as FourthAlias on  AnyAlias.IdField    = FourthAlias.IdField");
		buildSrc("                                                   and AnyAlias.SubIdField = FourthAlias.SubIdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnyEntity2               as AnyAlias");
		buildSrc("");
		buildSrc("    inner join I_OtherEntity             as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("");
		buildSrc("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any Description'");
		buildExp("define view entity C_AnyEntity");
		buildExp("");
		buildExp("  as select from    I_AnyEntity     as AnyAlias");
		buildExp("    inner join      I_OtherEntity2  as OtherAlias  on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("    left outer join I_ThirdEntity2  as ThirdAlias  on AnyAlias.IdField = ThirdAlias.IdField");
		buildExp("");
		buildExp("    inner join      I_FourthEntity2 as FourthAlias on  AnyAlias.IdField    = FourthAlias.IdField");
		buildExp("                                                   and AnyAlias.SubIdField = FourthAlias.SubIdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnyEntity2               as AnyAlias");
		buildExp("");
		buildExp("    inner join I_OtherEntity             as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("    left outer to one join I_ThirdEntity as ThirdAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlwaysCondenseSingleLineAssociations() {
		rule.configCondenseAssociations.setEnumValue(DdlCondenseMode.ALWAYS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverCondenseSingleLineElements() {
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.NEVER);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlwaysCondenseSingleLineClauses() {
		rule.configCondenseClauses.setEnumValue(DdlCondenseMode.ALWAYS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField");
		buildSrc("}");
		buildSrc("where  AnyNonKeyField  > 10");
		buildSrc("  and OtherNonKeyField = 'A'");
		buildSrc("group by AnyAlias.AnyKeyField");
		buildSrc("         AnyAlias.OtherKeyField");
		buildSrc("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildSrc("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildSrc("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField");
		buildExp("}");
		buildExp("where  AnyNonKeyField  > 10");
		buildExp("  and OtherNonKeyField = 'A'");
		buildExp("");
		buildExp("group by AnyAlias.AnyKeyField");
		buildExp("         AnyAlias.OtherKeyField");
		buildExp("");
		buildExp("having sum(OtherAilas.AnyNonKeyField)     > 100");
		buildExp("   and avg(OtherAlias.ThirdNonKeyField)   < 42");
		buildExp("   and max(ThirdAlias.FourthNonKeyField) >= 'Z'");

		testRule();
	}
}
