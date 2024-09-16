package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rules.alignment.SelectListLayout;

public class DdlAlignFieldListsTest extends RuleTestBase {
	private DdlAlignFieldListsRule rule;
	
	DdlAlignFieldListsTest() {
		super(RuleID.DDL_ALIGN_FIELD_LISTS);
		rule = (DdlAlignFieldListsRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configMaxLineLength.setValue(120);
		rule.configNameListPos.setEnumValue(DdlNameListPos.LINE_START_PLUS_2);
	   rule.configNameListLayout.setEnumValue(SelectListLayout.MULTI_LINE);

		rule.configGroupByListPos.setEnumValue(DdlGroupByListPos.CONTINUE);
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.MULTI_LINE);
		rule.configConsiderDotAsComplex.setValue(true);
	}

	@Test
	void testAlignNameListBelowLineStartPlus2() {
		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("(");
		buildExp("  AnyFieldAlias,");
		buildExp("  OtherFieldAlias,");
		buildExp("  ThirdFieldAlias,");
		buildExp("  FourthFieldAliasWithLongName,");
		buildExp("  FifthFieldAlias");
		buildExp(")");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListBelowLineStartPlus4() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.LINE_START_PLUS_4);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  (");
		buildExp("    AnyFieldAlias,");
		buildExp("    OtherFieldAlias,");
		buildExp("    ThirdFieldAlias,");
		buildExp("    FourthFieldAliasWithLongName,");
		buildExp("    FifthFieldAlias");
		buildExp("  )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListBelowViewNamePlus2() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.VIEW_NAME_PLUS_2);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc("//CommentedOutFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("            (");
		buildExp("              AnyFieldAlias,");
		buildExp("              OtherFieldAlias,");
		buildExp("//              CommentedOutFieldAlias,");
		buildExp("              ThirdFieldAlias,");
		buildExp("              FourthFieldAliasWithLongName,");
		buildExp("              FifthFieldAlias");
		buildExp("            )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListBelowViewNamePlus4() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.VIEW_NAME_PLUS_4);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("//CommentedOutFieldAlias,");
		buildSrc("  AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("//CommentedOutFieldAlias2");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("              (");
		buildExp("//                CommentedOutFieldAlias,");
		buildExp("                AnyFieldAlias,");
		buildExp("                OtherFieldAlias,");
		buildExp("                ThirdFieldAlias,");
		buildExp("                FourthFieldAliasWithLongName,");
		buildExp("                FifthFieldAlias");
		buildExp("//                CommentedOutFieldAlias2");
		buildExp("              )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListKeepAsIs() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.KEEP_AS_IS);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("//CommentedOutFieldAlias,");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("//CommentedOutFieldAlias2");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp(" (");
		buildExp("//    CommentedOutFieldAlias,");
		buildExp("    AnyFieldAlias,");
		buildExp("    OtherFieldAlias,");
		buildExp("    ThirdFieldAlias,");
		buildExp("    FourthFieldAliasWithLongName,");
		buildExp("    FifthFieldAlias");
		buildExp("//    CommentedOutFieldAlias2");
		buildExp("    )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListAfterViewName() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView( AnyFieldAlias,");
		buildExp("                                OtherFieldAlias,");
		buildExp("                                ThirdFieldAlias,");
		buildExp("                                FourthFieldAliasWithLongName,");
		buildExp("                                FifthFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListAfterViewNameWithComments() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("//CommentedOutFieldAlias,");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("//CommentedOutFieldAlias2");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView(");
		buildExp("//                                CommentedOutFieldAlias,");
		buildExp("                                AnyFieldAlias,");
		buildExp("                                OtherFieldAlias,");
		buildExp("                                ThirdFieldAlias,");
		buildExp("                                FourthFieldAliasWithLongName,");
		buildExp("                                FifthFieldAlias");
		buildExp("//                                CommentedOutFieldAlias2");
		buildExp("                              )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListSingleLine() {
		rule.configNameListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("( AnyFieldAlias, OtherFieldAlias, ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListContinueWithSingleLine() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);
		rule.configNameListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildSrc("  FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView( AnyFieldAlias, OtherFieldAlias, ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildExp("                                FifthFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListContinueWithSingleLineNarrow() {
		rule.configMaxLineLength.setValue(100);
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);
		rule.configNameListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildSrc("  FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView( AnyFieldAlias, OtherFieldAlias, ThirdFieldAlias,");
		buildExp("                                FourthFieldAliasWithLongName, FifthFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListDeriveSingleLine() {
		rule.configNameListLayout.setEnumValue(SelectListLayout.DERIVE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("( AnyFieldAlias, OtherFieldAlias, ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListDeriveMultiLine() {
		rule.configNameListLayout.setEnumValue(SelectListLayout.DERIVE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildSrc("  FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("(");
		buildExp("  AnyFieldAlias,");
		buildExp("  OtherFieldAlias,");
		buildExp("  ThirdFieldAlias,");
		buildExp("  FourthFieldAliasWithLongName,");
		buildExp("  FifthFieldAlias");
		buildExp(")");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignNameListKeepLayoutAsIs() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.VIEW_NAME_PLUS_2);
		rule.configNameListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("    AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildSrc("  FifthFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("            (");
		buildExp("    AnyFieldAlias,");
		buildExp("OtherFieldAlias,");
		buildExp(" ThirdFieldAlias, FourthFieldAliasWithLongName,");
		buildExp("  FifthFieldAlias");
		buildExp("            )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignGroupByContinue() {
		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField,");
		buildExp("         _ThirdAlias.FourthFieldWithLongName,");
		buildExp("         _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignGroupByBelowListStartPlus2() {
		rule.configGroupByListPos.setEnumValue(DdlGroupByListPos.LINE_START_PLUS_2);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by");
		buildExp("  AnyAlias.AnyField,");
		buildExp("  OtherAlias.OtherField,");
		buildExp("//  OtherAlias.CommentedOutField,");
		buildExp("  OtherAlias.ThirdField,");
		buildExp("  _ThirdAlias.FourthFieldWithLongName,");
		buildExp("  _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignGroupByBelowListStartPlus4() {
		rule.configGroupByListPos.setEnumValue(DdlGroupByListPos.LINE_START_PLUS_4);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by");
		buildExp("    AnyAlias.AnyField,");
		buildExp("    OtherAlias.OtherField,");
		buildExp("//    OtherAlias.CommentedOutField,");
		buildExp("    OtherAlias.ThirdField,");
		buildExp("    _ThirdAlias.FourthFieldWithLongName,");
		buildExp("    _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignGroupByListKeepPosAsIs() {
		rule.configGroupByListPos.setEnumValue(DdlGroupByListPos.KEEP_AS_IS);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by");
		buildExp("       AnyAlias.AnyField,");
		buildExp("       OtherAlias.OtherField,");
		buildExp("//       OtherAlias.CommentedOutField,");
		buildExp("       OtherAlias.ThirdField,");
		buildExp("       _ThirdAlias.FourthFieldWithLongName,");
		buildExp("       _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignComplexGroupByListSingleLine() {
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField, OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName, _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignComplexGroupByListDeriveMultiLine() {
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.DERIVE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField,");
		buildExp("         _ThirdAlias.FourthFieldWithLongName,");
		buildExp("         _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignComplexGroupByListKeepAsIs() {
		rule.configComplexGroupByListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAlignSimpleGroupByListMultiLine() {
		rule.configConsiderDotAsComplex.setValue(false);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField,");
		buildExp("         _ThirdAlias.FourthFieldWithLongName,");
		buildExp("         _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignSimpleGroupByListSingleLine() {
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.ONE_LINE);
		rule.configConsiderDotAsComplex.setValue(false);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField, OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName, _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignSimpleGroupByListDeriveSingleLine() {
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.DERIVE);
		rule.configConsiderDotAsComplex.setValue(false);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField, OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField, OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName, _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testAlignSimpleGroupByListKeepLayoutAsIs() {
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.KEEP_AS_IS);
		rule.configConsiderDotAsComplex.setValue(false);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField, OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc(" OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("      _ThirdAlias._FourthAlias.FifthField");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testVerySimpleGroupByListSingleLine() {
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyField,");
		buildSrc("     OtherField,");
		buildSrc("// CommentedOutField,");
		buildSrc(" ThirdField, FourthFieldWithLongName,");
		buildSrc("      FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyField, OtherField,");
		buildExp("//         CommentedOutField,");
		buildExp("         ThirdField, FourthFieldWithLongName, FifthField");

		testRule();
	}

	@Test
	void testContinueBlockedByComment() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);

		buildSrc("define view I_AnyDDicBasedView // comment");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias,");
		buildSrc("OtherFieldAlias,");
		buildSrc(" ThirdFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by // comment");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc(" OtherAlias.ThirdField");

		buildExp("define view I_AnyDDicBasedView // comment");
		buildExp("                              ( AnyFieldAlias,");
		buildExp("                                OtherFieldAlias,");
		buildExp("                                ThirdFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by // comment");
		buildExp("         AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("         OtherAlias.ThirdField");

		testRule();
	}

	@Test
	void testMoveCommentsBehindFieldNames() {
		buildSrc("define view I_AnyDDicBasedView // comment");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias // comment");
		buildSrc("  ,");
		buildSrc("OtherFieldAlias");
		buildSrc("");
		buildSrc(",");
		buildSrc(" ThirdFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by // comment");
		buildSrc("       AnyAlias.AnyField");
		buildSrc("          ,");
		buildSrc("     OtherAlias.OtherField");
		buildSrc("    , OtherAlias.ThirdField");

		buildExp("define view I_AnyDDicBasedView // comment");
		buildExp("(");
		buildExp("  AnyFieldAlias // comment");
		buildExp("  ,");
		buildExp("  OtherFieldAlias,");
		buildExp("  ThirdFieldAlias");
		buildExp(")");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by // comment");
		buildExp("         AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("         OtherAlias.ThirdField");

		testRule();
	}

	@Test
	void testContinueWithComments() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.CONTINUE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("  // comment 1");
		buildSrc("  AnyFieldAlias,");
		buildSrc("-- comment 2");
		buildSrc("OtherFieldAlias,");
		buildSrc("// comment 3");
		buildSrc(" ThirdFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("  /* comment A */");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("// comment B");
		buildSrc("     OtherAlias.OtherField,");
		buildSrc("     -- comment C");
		buildSrc("   OtherAlias.ThirdField");

		buildExp("define view I_AnyDDicBasedView( // comment 1");
		buildExp("                                AnyFieldAlias,");
		buildExp("                                -- comment 2");
		buildExp("                                OtherFieldAlias,");
		buildExp("//                                comment 3");
		buildExp("                                ThirdFieldAlias )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by /* comment A */");
		buildExp("         AnyAlias.AnyField,");
		buildExp("//         comment B");
		buildExp("         OtherAlias.OtherField,");
		buildExp("         -- comment C");
		buildExp("         OtherAlias.ThirdField");

		testRule();
	}

	@Test
	void testCommentsWithoutText() {
		rule.configNameListPos.setEnumValue(DdlNameListPos.VIEW_NAME_PLUS_4);
		rule.configSimpleGroupByListLayout.setEnumValue(SelectListLayout.ONE_LINE);

		buildSrc("define view I_AnyDDicBasedView");
		buildSrc(" (");
		buildSrc("  AnyFieldAlias,");
		buildSrc("//");
		buildSrc("   OtherFieldAlias");
		buildSrc("    )");
		buildSrc("");
		buildSrc("  as select from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField");
		buildSrc("}");
		buildSrc("group by");
		buildSrc("       AnyAlias.AnyField,");
		buildSrc("////");
		buildSrc("     OtherAlias.OtherField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("              (");
		buildExp("                AnyFieldAlias,");
		buildExp("//");
		buildExp("                OtherFieldAlias");
		buildExp("              )");
		buildExp("");
		buildExp("  as select from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField");
		buildExp("}");
		buildExp("group by AnyAlias.AnyField,");
		buildExp("////");
		buildExp("         OtherAlias.OtherField");

		testRule();
	}

}
