package com.sap.adt.abapcleaner.rules.ddl.spaces;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.ChangeType;

public class DdlSpacesAroundSignsTest extends RuleTestBase {
	private DdlSpacesAroundSignsRule rule;
	
	DdlSpacesAroundSignsTest() {
		super(RuleID.DDL_SPACES_AROUND_SIGNS);
		rule = (DdlSpacesAroundSignsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configSpaceBeforeCommentSign.setEnumValue(ChangeType.ALWAYS);
		rule.configSpaceAfterCommentSign.setEnumValue(ChangeType.ALWAYS);

		rule.configSpaceBeforeColon.setEnumValue(ChangeType.KEEP_AS_IS);
		rule.configSpaceAfterColon.setEnumValue(ChangeType.ALWAYS);

		rule.configSpaceBeforeComma.setEnumValue(ChangeType.NEVER);
		rule.configSpaceAfterComma.setEnumValue(ChangeType.ALWAYS);
		rule.configSpaceAfterCommaInAbapType.setEnumValue(ChangeType.NEVER);

		rule.configSpaceAroundArithmeticOps.setEnumValue(ChangeType.ALWAYS);
	}

	@Test
	void testAnnotationUnchanged() {
		rule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);
		rule.configSpaceAfterColon.setEnumValue(ChangeType.NEVER);
		rule.configSpaceBeforeComma.setEnumValue(ChangeType.ALWAYS);
		rule.configSpaceAfterComma.setEnumValue(ChangeType.NEVER);

		buildSrc("@Annotation: { anySubAnno: 'value', otherSubAnno: 'value' }");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSpaceAroundComments() {
		buildSrc("//comment at line start");
		buildSrc("define view C_AnyView");
		buildSrc("");
		buildSrc("  --comment0");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,   //comment1");
		buildSrc("  key OtherKeyField,-- comment2");
		buildSrc("");
		buildSrc("      // comment3");
		buildSrc("      --comment4");
		buildSrc("}");

		buildExp("//comment at line start");
		buildExp("define view C_AnyView");
		buildExp("");
		buildExp("  -- comment0");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,   // comment1");
		buildExp("  key OtherKeyField, -- comment2");
		buildExp("");
		buildExp("      // comment3");
		buildExp("      -- comment4");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceBeforeCommentAsIs() {
		rule.configSpaceBeforeCommentSign.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("//comment at line start");
		buildSrc("define view C_AnyView");
		buildSrc("");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,   //comment1");
		buildSrc("  key OtherKeyField,-- comment2");
		buildSrc("");
		buildSrc("      // comment3");
		buildSrc("      --comment4");
		buildSrc("}");

		buildExp("//comment at line start");
		buildExp("define view C_AnyView");
		buildExp("");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,   // comment1");
		buildExp("  key OtherKeyField,-- comment2");
		buildExp("");
		buildExp("      // comment3");
		buildExp("      -- comment4");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceBeforeComment() {
		rule.configSpaceBeforeCommentSign.setEnumValue(ChangeType.NEVER);

		buildSrc("//comment at line start");
		buildSrc("define view C_AnyView");
		buildSrc("");
		buildSrc("  --comment0");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,   //comment1");
		buildSrc("  key OtherKeyField,-- comment2");
		buildSrc("");
		buildSrc("      // comment3");
		buildSrc("      --comment4");
		buildSrc("}");

		buildExp("//comment at line start");
		buildExp("define view C_AnyView");
		buildExp("");
		buildExp("  -- comment0");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,// comment1");
		buildExp("  key OtherKeyField,-- comment2");
		buildExp("");
		buildExp("      // comment3");
		buildExp("      -- comment4");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceAfterCommentSign() {
		rule.configSpaceAfterCommentSign.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("//comment at line start");
		buildSrc("define view C_AnyView");
		buildSrc("");
		buildSrc("  --comment0");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,   //comment1");
		buildSrc("  key OtherKeyField,-- comment2");
		buildSrc("");
		buildSrc("      // comment3");
		buildSrc("      --comment4");
		buildSrc("}");

		buildExp("//comment at line start");
		buildExp("define view C_AnyView");
		buildExp("");
		buildExp("  --comment0");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,   //comment1");
		buildExp("  key OtherKeyField, -- comment2");
		buildExp("");
		buildExp("      // comment3");
		buildExp("      --comment4");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceAfterCommentSign() {
		rule.configSpaceAfterCommentSign.setEnumValue(ChangeType.NEVER);

		buildSrc("//comment at line start");
		buildSrc("define view C_AnyView");
		buildSrc("");
		buildSrc("  --comment0");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField,   //comment1");
		buildSrc("  key OtherKeyField,-- comment2");
		buildSrc("");
		buildSrc("      // comment3");
		buildSrc("      --comment4");
		buildSrc("}");

		buildExp("//comment at line start");
		buildExp("define view C_AnyView");
		buildExp("");
		buildExp("  --comment0");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,   //comment1");
		buildExp("  key OtherKeyField, --comment2");
		buildExp("");
		buildExp("      //comment3");
		buildExp("      --comment4");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpaceBeforeColon() {
		rule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam ,");
		buildSrc("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  : any_type,");
		buildExp("    P_OtherParam : other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam  : :P_AnyParam,");
		buildExp("    P_OtherParam : :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceBeforeColon() {
		rule.configSpaceBeforeColon.setEnumValue(ChangeType.NEVER);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam: other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam ,");
		buildSrc("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam: any_type,");
		buildExp("    P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam: :P_AnyParam,");
		buildExp("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceAfterColonAsIs() {
		rule.configSpaceAfterColon.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam:other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam ,");
		buildSrc("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  : any_type,");
		buildExp("    P_OtherParam:other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam  ::P_AnyParam,");
		buildExp("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceAfterColon() {
		rule.configSpaceAfterColon.setEnumValue(ChangeType.NEVER);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam:other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam ,");
		buildSrc("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  :any_type,");
		buildExp("    P_OtherParam:other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam  ::P_AnyParam,");
		buildExp("    P_OtherParam::P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpaceBeforeComma() {
		rule.configSpaceBeforeComma.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam:other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam ,");
		buildSrc("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,");
		buildSrc("      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  : any_type    ,");
		buildExp("    P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam  : :P_AnyParam ,");
		buildExp("    P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField   ,");
		buildExp("  key OtherKeyField ,");
		buildExp("");
		buildExp("      AnyValue * 2 + OtherValue * 4 - ThirdValue as AnyValueField ,");
		buildExp("      3 * (-2 * AnyValue - 4 * OtherValue) as OtherValueField ,");
		buildExp("");
		buildExp("      concat(AnyText , concat('_' , OtherText)) as AnyTextField ,");
		buildExp("      division(AnyArg * 10 , OtherArg , 2) as ThirdValue ,");
		buildExp("      round(AnyValue / 100 , 5) as RoundedValue ,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildExp("      cast(OtherAmount as abap.curr(23 ,2)) as OtherAmount ,");
		buildExp("      cast(AnyQuantity as abap.quan(18 ,6)) as AnyQuantity ,");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceBeforeCommaAsIs() {
		rule.configSpaceBeforeComma.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type    ,");
		buildSrc("    P_OtherParam:other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  : any_type    ,");
		buildExp("    P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField   ,");
		buildExp("  key OtherKeyField ,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildExp("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildExp("      cast(AnyQuantity as abap.quan(18,6)) as AnyQuantity,");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceAfterCommaAsIs() {
		rule.configSpaceAfterComma.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("  key OtherKeyField,");
		buildExp("");
		buildExp("      concat(AnyText,concat('_',OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg * 10,OtherArg,2) as ThirdValue,");
		buildExp("      round(AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18,6)) as AnyQuantity,");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceAfterComma() {
		rule.configSpaceAfterComma.setEnumValue(ChangeType.NEVER);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("  key OtherKeyField,");
		buildExp("");
		buildExp("      concat(AnyText,concat('_',OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg * 10,OtherArg,2) as ThirdValue,");
		buildExp("      round(AnyValue / 100,5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18,6)) as AnyQuantity,");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpaceAfterCommaInAbapTypeOnly() {
		rule.configSpaceAfterComma.setEnumValue(ChangeType.NEVER);
		rule.configSpaceAfterCommaInAbapType.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("  key OtherKeyField,");
		buildExp("");
		buildExp("      concat(AnyText,concat('_',OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg * 10,OtherArg,2) as ThirdValue,");
		buildExp("      round(AnyValue / 100,5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23, 2)) as AnyAmount,");
		buildExp("      cast(OtherAmount as abap.curr(23, 2)) as OtherAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceAfterCommaInAbapTypeAsIs() {
		rule.configSpaceAfterCommaInAbapType.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField   ,");
		buildSrc("  key OtherKeyField ,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue,");
		buildSrc("");
		buildSrc("      cast(AnyAmount   as abap.curr(23 ,2)) as AnyAmount  ,");
		buildSrc("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount ,");
		buildSrc("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField,");
		buildExp("  key OtherKeyField,");
		buildExp("");
		buildExp("      concat(AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg * 10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue / 100, 5) as RoundedValue,");
		buildExp("");
		buildExp("      cast(AnyAmount   as abap.curr(23,2)) as AnyAmount,");
		buildExp("      cast(OtherAmount as abap.curr(23,2)) as OtherAmount,");
		buildExp("      cast(AnyQuantity as abap.quan(18, 6)) as AnyQuantity,");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepSpaceAroundArithmeticOpsAsIs() {
		rule.configSpaceAroundArithmeticOps.setEnumValue(ChangeType.KEEP_AS_IS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,");
		buildSrc("      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,");
		buildExp("      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,");
		buildExp("");
		buildExp("      concat(AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg *10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue/ 100, 5) as RoundedValue");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceAroundArithmeticOps() {
		rule.configSpaceAroundArithmeticOps.setEnumValue(ChangeType.NEVER);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyValue*2 +OtherValue* 4- ThirdValue as AnyValueField,");
		buildSrc("      3*(-2*AnyValue- 4*OtherValue) as OtherValueField,");
		buildSrc("");
		buildSrc("      concat(AnyText,concat('_' ,OtherText)) as AnyTextField,");
		buildSrc("      division(AnyArg *10,OtherArg,2) as ThirdValue,");
		buildSrc("      round(AnyValue/ 100, 5) as RoundedValue");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyValue*2+OtherValue*4-ThirdValue as AnyValueField,");
		buildExp("      3*(-2*AnyValue-4*OtherValue) as OtherValueField,");
		buildExp("");
		buildExp("      concat(AnyText, concat('_', OtherText)) as AnyTextField,");
		buildExp("      division(AnyArg*10, OtherArg, 2) as ThirdValue,");
		buildExp("      round(AnyValue/100, 5) as RoundedValue");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpecialCasesOfComments() {
		buildSrc("--comment");
		buildSrc("//comment");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("  //// comment with double //");
		buildSrc("  -----------------------------");
		buildSrc("  key AnyKeyField,//");
		buildSrc("  key OtherKeyField--");
		buildSrc("}");

		buildExp("-- comment");
		buildExp("//comment");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  //// comment with double //");
		buildExp("  -----------------------------");
		buildExp("  key AnyKeyField, //");
		buildExp("  key OtherKeyField --");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpecialCasesOfCommas() {
		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam  : any_type");
		buildSrc("    ,P_OtherParam:other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam  ::P_AnyParam");
		buildSrc("    , P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("  ,key OtherKeyField");
		buildSrc("  ,");
		buildSrc("      concat(AnyText");
		buildSrc("             ,concat('_' ,");
		buildSrc("                     OtherText)) as AnyTextField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam  : any_type");
		buildExp("    , P_OtherParam: other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam  : :P_AnyParam");
		buildExp("    , P_OtherParam: :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("  , key OtherKeyField");
		buildExp("  ,");
		buildExp("      concat(AnyText");
		buildExp("             , concat('_',");
		buildExp("                      OtherText)) as AnyTextField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSpecialCasesOfColons() {
		buildSrc("define view C_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_AnyParam");
		buildSrc("    : any_type,");
		buildSrc("    P_OtherParam:");
		buildSrc("    other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyView(");
		buildSrc("    P_AnyParam");
		buildSrc("    ::P_AnyParam,");
		buildSrc("    P_OtherParam:");
		buildSrc("    :P_OtherParam ) as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view C_AnyView");
		buildExp("  with parameters");
		buildExp("    P_AnyParam");
		buildExp("    : any_type,");
		buildExp("    P_OtherParam:");
		buildExp("    other_type");
		buildExp("");
		buildExp("  as select from I_AnyView(");
		buildExp("    P_AnyParam");
		buildExp("    : :P_AnyParam,");
		buildExp("    P_OtherParam:");
		buildExp("    :P_OtherParam ) as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoSpaceBeforeColonInPathExpr() {
		// ensure that no space is added between "*:" or "1:" in path expressions
		rule.configSpaceBeforeColon.setEnumValue(ChangeType.ALWAYS);

		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("  association [1..*] to I_OtherView as _OtherAlias");
		buildSrc("    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      _OtherAlias[1: AnyField = 10].OtherField as OtherField,");
		buildSrc("      _OtherAlias[*: AnyField > 10].ThirdField as ThirdField");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSpecialCasesOfAsterisk() {
		// ensure that no spaces are put around "*" in association cardinality and in "count(*)"
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyView as AnyAlias");
		buildSrc("");
		buildSrc("  association [1..*] to I_OtherView as _OtherAlias");
		buildSrc("    on AnyAlias.AnyKeyField = _OtherAlias.AnyKeyField");
		buildSrc("");
		buildSrc("  composition [0..*] of I_ThirdView as _ThirdAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyKeyField,");
		buildSrc("      count(*) as AnyCount,");
		buildSrc("      count( * ) as OtherCount,");
		buildSrc("      _OtherAlias[*: AnyField = 1].OtherField as OtherField");
		buildSrc("}");
		buildSrc("group by AnyAlias.AnyKeyField");

		copyExpFromSrc();

		testRule();
	}
}
