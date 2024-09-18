package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlAlignDataSourcesTest extends RuleTestBase {
	private DdlAlignDataSourcesRule rule;
	
	DdlAlignDataSourcesTest() {
		super(RuleID.DDL_ALIGN_DATA_SOURCES);
		rule = (DdlAlignDataSourcesRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignDataSources.setValue(true);
		rule.configAlignAliases.setValue(true);
		rule.configAlignOnConditions.setValue(true);
		rule.configAlignAssociationsWithJoins.setValue(false);
		rule.configConsiderAllParamAssignLines.setValue(false);
	}

	@Test
	void testAlignAll() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource                         as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource                       as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName           as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 ) as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource             as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from              I_AnySource2                         as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2                       as OtherAlias             on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2           as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias            on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignDataSources() {
		rule.configAlignDataSources.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource                                   as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource                              as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join I_ThirdSourceWithLongName                       as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 ) as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource             as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnySource2                                      as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource2                              as OtherAlias             on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join I_ThirdSourceWithLongName2                       as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias            on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignAliases() {
		rule.configAlignAliases.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 ) as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from              I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2 as OtherAlias                         on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias  on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2 as _FifthAlias             on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignOnConditions() {
		rule.configAlignOnConditions.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource2                         as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2                       as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2           as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignDataSourcesOnly() {
		rule.configAlignAliases.setValue(false);
		rule.configAlignOnConditions.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignAliasesOnly() {
		rule.configAlignDataSources.setValue(false);
		rule.configAlignOnConditions.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource2                                   as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource2                              as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join I_ThirdSourceWithLongName2                       as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignOnConditionsOnly() {
		rule.configAlignDataSources.setValue(false);
		rule.configAlignAliases.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 ) as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource2 as OtherAlias                               on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName            on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                               and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2 as _FifthAlias             on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignAssociationsWithJoins() {
		rule.configAlignAssociationsWithJoins.setValue(true);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource2                         as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2                       as OtherAlias             on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2           as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                          and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 ) as FourthAlias            on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to    I_FifthSource2                       as _FifthAlias            on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to    I_SixthSourceWithLongName2           as _SixthAlias            on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                                          and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testConsiderAllParamAssignLines() {
		rule.configConsiderAllParamAssignLines.setValue(true);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource2 as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName2 as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("                                                                    and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildSrc("                                            P_OtherParam : 42 ) as FourthAlias on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  association [0..*] to I_FifthSource2 as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from           I_AnySource                                  as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource                                as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName                    as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 )          as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource             as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("");
		buildExp("union all");
		buildExp("  select from              I_AnySource2                                  as AnyAlias");
		buildExp("");
		buildExp("    left outer join        I_OtherSource2                                as OtherAlias             on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName2                    as ThirdAliasWithLongName on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("                                                                                                   and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    left outer to one join I_FourthSource2( P_AnyParam   : 'any literal'");
		buildExp("                                            P_OtherParam : 42 )          as FourthAlias            on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  association [0..*] to I_FifthSource2             as _FifthAlias on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association [0..1] to I_SixthSourceWithLongName2 as _SixthAlias on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("                                                                  and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testDDicBasedViewWithDetachedFromClause() {
		buildSrc("define view I_AnyView( AnyField,");
		buildSrc("                       OtherField )");
		buildSrc("as select distinct");
		buildSrc("");
		buildSrc("  key AnyField,");
		buildSrc("      OtherField");
		buildSrc("");
		buildSrc("from I_AnySource");
		buildSrc("");
		buildSrc("  left outer join I_OtherSource on I_AnySource.AnyField = I_OtherSource.AnyField");
		buildSrc("");
		buildSrc("  left outer join I_ThirdSourceWithLongName on  I_ThirdSourceWithLongName.AnyField = I_AnySource.AnyField");
		buildSrc("                                            and I_ThirdSourceWithLongName.OtherField = I_OtherSource.OtherField");

		buildExp("define view I_AnyView( AnyField,");
		buildExp("                       OtherField )");
		buildExp("as select distinct");
		buildExp("");
		buildExp("  key AnyField,");
		buildExp("      OtherField");
		buildExp("");
		buildExp("from              I_AnySource");
		buildExp("");
		buildExp("  left outer join I_OtherSource             on I_AnySource.AnyField = I_OtherSource.AnyField");
		buildExp("");
		buildExp("  left outer join I_ThirdSourceWithLongName on  I_ThirdSourceWithLongName.AnyField = I_AnySource.AnyField");
		buildExp("                                            and I_ThirdSourceWithLongName.OtherField = I_OtherSource.OtherField");

		testRule();
	}

	@Test
	void testCommentsBetweenJoinsAndAssociations() {
		buildSrc("define view I_AnyView");
		buildSrc("  // comment");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    // comment before joins");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("    inner join I_ThirdSourceWithLongName as ThirdAliasWithLongName");
		buildSrc("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildSrc("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildSrc("");
		buildSrc("    // comment within joins");
		buildSrc("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildSrc("                                           P_OtherParam : 42 ) as FourthAlias");
		buildSrc("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildSrc("");
		buildSrc("  // comment before associations");
		buildSrc("  association [0..*] to I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  // comment within associations");
		buildSrc("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  // comment");
		buildExp("  as select from           I_AnySource                         as AnyAlias");
		buildExp("");
		buildExp("    // comment before joins");
		buildExp("    left outer join        I_OtherSource                       as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("    inner join             I_ThirdSourceWithLongName           as ThirdAliasWithLongName");
		buildExp("      on  AnyAlias.AnyField   = ThirdAliasWithLongName.AnyField");
		buildExp("      and AnyAlias.OtherField = ThirdAliasWithLongName.OtherField");
		buildExp("");
		buildExp("    // comment within joins");
		buildExp("    left outer to one join I_FourthSource( P_AnyParam   : 'any literal'");
		buildExp("                                           P_OtherParam : 42 ) as FourthAlias");
		buildExp("      on AnyAlias.AnyField = FourthAlias.OtherField");
		buildExp("");
		buildExp("  // comment before associations");
		buildExp("  association [0..*] to I_FifthSource             as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  // comment within associations");
		buildExp("  association [0..1] to I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignAssociationToParent() {
		// ensure that PARENT is aligned with the keywords, not with the data sources 
		
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("  association to parent I_FifthSource as _FifthAlias");
		buildSrc("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildSrc("");
		buildSrc("  association to I_SixthSourceWithLongName as _SixthAlias");
		buildSrc("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildSrc("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("  association to parent I_FifthSource             as _FifthAlias");
		buildExp("     on _FifthAlias.AnyField = AnyAlias.AnyField");
		buildExp("");
		buildExp("  association to        I_SixthSourceWithLongName as _SixthAlias");
		buildExp("     on  _SixthAlias.AnyField   = AnyAlias.AnyField");
		buildExp("     and _SixthAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}
}
