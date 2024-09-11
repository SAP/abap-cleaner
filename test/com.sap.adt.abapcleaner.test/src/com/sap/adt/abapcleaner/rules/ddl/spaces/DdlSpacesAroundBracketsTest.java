package com.sap.adt.abapcleaner.rules.ddl.spaces;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

public class DdlSpacesAroundBracketsTest extends RuleTestBase {
	private DdlSpacesAroundBracketsRule rule;
	
	DdlSpacesAroundBracketsTest() {
		super(RuleID.DDL_SPACES_AROUND_BRACKETS);
		rule = (DdlSpacesAroundBracketsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configSpacesAroundCardBrackets.setEnumValue(ChangeType.ALWAYS);
		rule.configSpacesInsideCardBrackets.setEnumValue(ChangeType.NEVER);

		rule.configSpacesBeforePathBrackets.setEnumValue(ChangeType.NEVER);
		rule.configSpacesInsidePathBrackets.setEnumValue(ChangeType.NEVER);

		rule.configSpacesBeforeFuncParens.setEnumValue(ChangeType.NEVER);
		rule.configSpacesInsideFuncParens.setEnumValue(ChangeType.NEVER);

		rule.configSpacesBeforeTypeParens.setEnumValue(ChangeType.NEVER);
		rule.configSpacesInsideTypeParens.setEnumValue(ChangeType.NEVER);

		rule.configSpacesInsideArithParens.setEnumValue(ChangeType.NEVER);
	}

	@Test
	void testSpacesAroundCardBrackets() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesAroundCardBrackets() {
		rule.configSpacesAroundCardBrackets.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association[1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesAroundCardBrackets() {
		rule.configSpacesAroundCardBrackets.setEnumValue(ChangeType.NEVER);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association[1..*]to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association[*]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesBeforePathBrackets() {
		rule.configSpacesBeforePathBrackets.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias [1:AnyValue > 0]._AnyAssoc [*: OtherValue = 42].AnyField as AnyField,");
		buildExp("      _ThirdAlias [inner where i = '2']._Text [1: Language = $session.system_language].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesBeforePathBrackets() {
		rule.configSpacesBeforePathBrackets.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[*: OtherValue = 42].AnyField as AnyField,");
		buildExp("      _ThirdAlias[inner where i = '2']._Text [1: Language = $session.system_language].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesBeforePathBrackets() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text[ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias[1:AnyValue > 0]._AnyAssoc[*: OtherValue = 42].AnyField as AnyField,");
		buildExp("      _ThirdAlias[inner where i = '2']._Text[1: Language = $session.system_language].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesInsidePathBrackets() {
		rule.configSpacesInsidePathBrackets.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias[ 1:AnyValue > 0 ]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildExp("      _ThirdAlias[ inner where i = '2' ]._Text[ 1: Language = $session.system_language ].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesInsidePathBrackets() {
		rule.configSpacesInsidePathBrackets.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias[1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildExp("      _ThirdAlias[ inner where i = '2']._Text[ 1: Language = $session.system_language].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesInsidePathBrackets() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("  association [ *]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      _OtherAlias [1:AnyValue > 0]._AnyAssoc[ *: OtherValue = 42 ].AnyField as AnyField,");
		buildSrc("      _ThirdAlias[ inner where i = '2']._Text [ 1: Language = $session.system_language].AnyName as AnyName");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association [1..*] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("  association [*] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      _OtherAlias[1:AnyValue > 0]._AnyAssoc[*: OtherValue = 42].AnyField as AnyField,");
		buildExp("      _ThirdAlias[inner where i = '2']._Text[1: Language = $session.system_language].AnyName as AnyName");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesBeforeFuncParens() {
		rule.configSpacesBeforeFuncParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat (AnyText, concat ('_', OtherText)) as AnyTextField,");
		buildExp("      division (AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round (AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast (AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast (AnyQuantity as abap.quan(18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesBeforeFuncParens() {
		rule.configSpacesBeforeFuncParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat (AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast (AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesBeforeFuncParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat(AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesInsideFuncParens() {
		rule.configSpacesInsideFuncParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat( AnyText, concat( '_', OtherText ) ) as AnyTextField,");
		buildExp("      division( AnyArg *10, OtherArg, 2 ) as ThirdValue,");
		buildExp("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildExp("");
		buildExp("      cast(  AnyAmount as abap.curr(23,2) ) as AnyAmount,");
		buildExp("      cast( AnyQuantity as abap.quan(18, 6) ) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesInsideFuncParens() {
		rule.configSpacesInsideFuncParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildExp("");
		buildExp("      cast(  AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6) ) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesInsideFuncParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat ( AnyText, concat( '_', OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildSrc("      round( AnyValue / 100, 5 ) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat(AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesBeforeTypeParens() {
		rule.configSpacesBeforeTypeParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      cast(AnyAmount as abap.curr (23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan (18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesBeforeTypeParens() {
		rule.configSpacesBeforeTypeParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      cast(AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan (18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpacesBeforeTypeParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      cast (  AnyAmount as abap.curr( 23,2)) as AnyAmount,");
		buildSrc("      cast(AnyQuantity as abap.quan (18, 6 ) ) as AnyQuantity");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      cast(AnyAmount as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpacesInsideArithParens() {
		rule.configSpacesInsideArithParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      ( AnyValue + OtherValue) as AnyValueField,");
		buildSrc("      3 * (-2 * AnyValue - 4 * (OtherValue - 1) ) as OtherValueField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      ( AnyValue + OtherValue ) as AnyValueField,");
		buildExp("      3 * ( -2 * AnyValue - 4 * ( OtherValue - 1 ) ) as OtherValueField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpacesInsideArithParens() {
		rule.configSpacesInsideArithParens.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      ( AnyValue + OtherValue) as AnyValueField,");
		buildSrc("      3 * (-2 * AnyValue - 4 * (OtherValue - 1) ) as OtherValueField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testNoSpacesInsideArithParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      ( AnyValue + OtherValue) as AnyValueField,");
		buildSrc("      3 * (-2 * AnyValue - 4 * (OtherValue - 1) ) as OtherValueField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("      (AnyValue + OtherValue) as AnyValueField,");
		buildExp("      3 * (-2 * AnyValue - 4 * (OtherValue - 1)) as OtherValueField");
		buildExp("}");

		testRule();
	}

	@Test
	void testLineBreaksAroundCardBrackets() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("  association");
		buildSrc("    [ // comment");
		buildSrc("      1..*");
		buildSrc("    ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  association [ *");
		buildSrc("    ]to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("  association");
		buildExp("    [ // comment");
		buildExp("      1..*");
		buildExp("    ] to I_OtherView as _OtherAlias on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildExp("");
		buildExp("  association [*");
		buildExp("    ] to I_ThirdView as _ThirdAlias on AnyAlias.AnyKeyField = _ThirdAlias.AnyKeyField");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField}");

		testRule();
	}

	@Test
	void testLineBreaksAroundPathBrackets() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      _OtherAlias [ 1:AnyValue > 0");
		buildSrc("                  ]._AnyAssoc[ *:");
		buildSrc("                    OtherValue = 42 ] -- comment");
		buildSrc("                   .AnyField as AnyField}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      _OtherAlias[1:AnyValue > 0");
		buildExp("                 ]._AnyAssoc[*:");
		buildExp("                   OtherValue = 42] -- comment");
		buildExp("                  .AnyField as AnyField}");

		testRule();
	}

	@Test
	void testLineBreaksAroundFuncParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat");
		buildSrc("      (");
		buildSrc("        AnyText, concat");
		buildSrc("        (   '_',");
		buildSrc("            OtherText");
		buildSrc("        )");
		buildSrc("      ) as AnyTextField,");
		buildSrc("");
		buildSrc("      cast");
		buildSrc("         ( AnyAmount as abap.curr");
		buildSrc("           (");
		buildSrc("              23,2");
		buildSrc("           )");
		buildSrc("         ) as AnyAmount");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat");
		buildExp("      (");
		buildExp("        AnyText, concat");
		buildExp("        ('_',");
		buildExp("         OtherText");
		buildExp("        )");
		buildExp("      ) as AnyTextField,");
		buildExp("");
		buildExp("      cast");
		buildExp("         (AnyAmount as abap.curr");
		buildExp("          (");
		buildExp("             23,2");
		buildExp("          )");
		buildExp("         ) as AnyAmount");
		buildExp("}");

		testRule();
	}

	@Test
	void testLineBreaksAroundFuncParensAddSpace() {
		rule.configSpacesInsideFuncParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      concat");
		buildSrc("      (");
		buildSrc("        AnyText, concat");
		buildSrc("        ('_',");
		buildSrc("         OtherText");
		buildSrc("        )");
		buildSrc("      ) as AnyTextField,");
		buildSrc("");
		buildSrc("      // casts and ABAP types");
		buildSrc("      cast");
		buildSrc("         (AnyAmount as abap.curr");
		buildSrc("          (");
		buildSrc("             23,2 // comment");
		buildSrc("          ) -- comment");
		buildSrc("         ) as AnyAmount");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      concat");
		buildExp("      (");
		buildExp("        AnyText, concat");
		buildExp("        ( '_',");
		buildExp("          OtherText");
		buildExp("        )");
		buildExp("      ) as AnyTextField,");
		buildExp("");
		buildExp("      // casts and ABAP types");
		buildExp("      cast");
		buildExp("         ( AnyAmount as abap.curr");
		buildExp("           (");
		buildExp("              23,2 // comment");
		buildExp("           ) -- comment");
		buildExp("         ) as AnyAmount");
		buildExp("}");

		testRule();
	}

	@Test
	void testLineBreaksAroundArithParens() {
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      (");
		buildSrc("        AnyValue + OtherValue");
		buildSrc("      ) as AnyValueField,");
		buildSrc("      3 * (    -2 * AnyValue - 4 *");
		buildSrc("               (OtherValue");
		buildSrc("                - 1)");
		buildSrc("          ) as OtherValueField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      (");
		buildExp("        AnyValue + OtherValue");
		buildExp("      ) as AnyValueField,");
		buildExp("      3 * (-2 * AnyValue - 4 *");
		buildExp("           (OtherValue");
		buildExp("            - 1)");
		buildExp("          ) as OtherValueField");
		buildExp("}");

		testRule();
	}

	@Test
	void testLineBreaksAroundArithParensAddSpace() {
		rule.configSpacesInsideArithParens.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      (");
		buildSrc("        AnyValue + OtherValue");
		buildSrc("      ) as AnyValueField,");
		buildSrc("      3 * (    -2 * AnyValue - 4 *");
		buildSrc("               (OtherValue");
		buildSrc("                - 1)");
		buildSrc("          ) as OtherValueField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      (");
		buildExp("        AnyValue + OtherValue");
		buildExp("      ) as AnyValueField,");
		buildExp("      3 * (    -2 * AnyValue - 4 *");
		buildExp("               ( OtherValue");
		buildExp("                 - 1 )");
		buildExp("          ) as OtherValueField");
		buildExp("}");

		testRule();
	}
}
