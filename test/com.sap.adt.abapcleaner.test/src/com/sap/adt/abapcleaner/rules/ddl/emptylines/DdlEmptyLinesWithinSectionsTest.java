package com.sap.adt.abapcleaner.rules.ddl.emptylines;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
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

		rule.configDetachKeyFields.setValue(true);
		rule.configDetachExposedAssociations.setValue(true);

		rule.configCondenseParameters.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
		rule.configCondenseJoins.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
		rule.configCondenseAssociations.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
		rule.configCondenseClauses.setEnumValue(DdlCondenseMode.NEVER);
	}

	@Test
	void testConfigValueEnabled() {
		ArrayList<ConfigEnumValue<DdlCondenseMode>> configValues = new ArrayList<>();
		configValues.add(rule.configCondenseParameters);
		configValues.add(rule.configCondenseJoins);
		configValues.add(rule.configCondenseAssociations);
		configValues.add(rule.configCondenseElements);
		configValues.add(rule.configCondenseClauses);

		// expect the warning to be hidden if DdlCondenseMode.NEVER is selected everywhere
		for (ConfigEnumValue<DdlCondenseMode> configValue : configValues) {
			configValue.setEnumValue(DdlCondenseMode.NEVER);
		}
		assertTrue(rule.isConfigValueEnabled(rule.configSurroundParameterLineCountMin));
		assertTrue(rule.isConfigValueEnabled(rule.configCondenseParameters));
		assertFalse(rule.isConfigValueEnabled(rule.configCondenseWarning));

		DdlCondenseMode[] riskModes = new DdlCondenseMode[] { DdlCondenseMode.ALWAYS, DdlCondenseMode.IF_ONLY_ONE_LINERS };
		for (DdlCondenseMode riskMode : riskModes) { 
			// set exactly one of the values to a "risk mode"
			for (ConfigEnumValue<DdlCondenseMode> riskValue : configValues) {
				for (ConfigEnumValue<DdlCondenseMode> configValue : configValues) {
					DdlCondenseMode mode = (configValue == riskValue) ? riskMode : DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS;
					configValue.setEnumValue(mode);
				}
				// ensure that the warning is enabled
				assertTrue(rule.isConfigValueEnabled(rule.configCondenseWarning));
			}
		}
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
		buildExp("");
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

	@Test
	void testAttachedCommentKeptWithElement() {
		// ensure that empty lines are inserted above the attached comments, not below them
		
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      // comment A");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("      /* comment B */");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildSrc("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildSrc("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildSrc("}");
		buildSrc("where  AnyNonKeyField  > 10");
		buildSrc("  and OtherNonKeyField = 'A'");
		buildSrc("-- comment");
		buildSrc("group by AnyAlias.AnyKeyField");
		buildSrc("         AnyAlias.OtherKeyField");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      // comment A");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("      /* comment B */");
		buildExp("      @Annotation.subAnno: 'value'");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField");
		buildExp("      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',");
		buildExp("");
		buildExp("      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField");
		buildExp("}");
		buildExp("where  AnyNonKeyField  > 10");
		buildExp("  and OtherNonKeyField = 'A'");
		buildExp("");
		buildExp("-- comment");
		buildExp("group by AnyAlias.AnyKeyField");
		buildExp("         AnyAlias.OtherKeyField");

		testRule();
	}

	@Test
	void testPartlyAttachedOneLinersNotCondensed() {
		rule.configCondenseAssociations.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testPartlyAttachedOneLinersCondensed() {
		rule.configCondenseAssociations.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      OtherAlias.AnyNonKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      OtherAlias.AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDetachKeyFieldsAndAssociations() {
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("");
		buildExp("      _FourthAlias,");
		buildExp("      _FifthAlias,");
		buildExp("      _SixthAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotDetachKeyFields() {
		rule.configDetachKeyFields.setValue(false);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildExp("");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("");
		buildExp("      _FourthAlias,");
		buildExp("      _FifthAlias,");
		buildExp("      _SixthAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotDetachExposedAssociations() {
		rule.configDetachExposedAssociations.setValue(false);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildExp("");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("      _FourthAlias,");
		buildExp("");
		buildExp("      _FifthAlias,");
		buildExp("");
		buildExp("      _SixthAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotDetachKeyFieldsOrAssociations() {
		rule.configDetachKeyFields.setValue(false);
		rule.configDetachExposedAssociations.setValue(false);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCondenseElementsIfOnlyOneLinersInDetachedGroups() {
		rule.configCondenseElements.setEnumValue(DdlCondenseMode.IF_ONLY_ONE_LINERS);

		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("");
		buildExp("      _FourthAlias,");
		buildExp("      _FifthAlias,");
		buildExp("      _SixthAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testCondenseElementsIfAllAreOneLinersInDetachedGroups() {
		// ensure that DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS works for the three groups of select list elements 
		// separately: a) key fields, b) non-key fields, c) exposed associations
		
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_AnyEntity as AnyAlias");
		buildSrc("");
		buildSrc("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildSrc("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildSrc("");
		buildSrc("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("");
		buildSrc("  key AnyAlias.OtherKeyField,");
		buildSrc("      sum(OtherAlias.AnyNonKeyField),");
		buildSrc("");
		buildSrc("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildSrc("");
		buildSrc("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildSrc("      _FourthAlias,");
		buildSrc("");
		buildSrc("      _FifthAlias,");
		buildSrc("");
		buildSrc("      _SixthAlias");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_AnyEntity as AnyAlias");
		buildExp("");
		buildExp("  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField");
		buildExp("  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField");
		buildExp("");
		buildExp("  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyKeyField,");
		buildExp("  key AnyAlias.OtherKeyField,");
		buildExp("");
		buildExp("      sum(OtherAlias.AnyNonKeyField),");
		buildExp("      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField,");
		buildExp("      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,");
		buildExp("");
		buildExp("      _FourthAlias,");
		buildExp("      _FifthAlias,");
		buildExp("      _SixthAlias");
		buildExp("}");

		testRule();
	}
}
