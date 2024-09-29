package com.sap.adt.abapcleaner.rules.ddl.alignment;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlAlignSelectListTest extends RuleTestBase {
	private DdlAlignSelectListRule rule;
	
	DdlAlignSelectListTest() {
		super(RuleID.DDL_ALIGN_SELECT_LIST);
		rule = (DdlAlignSelectListRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configAlignKeyKeyword.setValue(true);
		rule.configAlignAliases.setValue(true);
		rule.configConsiderSimpleElementsWithoutAlias.setValue(true);
		rule.configConsiderComplexElementsWithoutAlias.setValue(false);
		rule.configConsiderAllElementLines.setValue(false);
		rule.configMaxAliasStart.setValue(120);
	   rule.configAlignTextualComments.setValue(true);
		rule.configAlignCommentedOutCode.setValue(true);
	}

	@Test
	void testAlignKeywordColumn() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out select list elements");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                                as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out select list elements");
		buildExp("//  key OtherAlias.CommentedOutField                                                                         as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                                                                    as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignKeywordColumn() {
		rule.configAlignKeyKeyword.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out select list elements");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                                                                as AnyFieldAlias,");
		buildExp("");
		buildExp("  OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("  OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,");
		buildExp("  OtherAlias.FifthField                                                                                as FifthFieldAlias,");
		buildExp("  // complex element");
		buildExp("  _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("  cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//  // Commented-out select list elements");
		buildExp("//  key OtherAlias.CommentedOutField                                                                     as CommentedOutFieldAlias,");
		buildExp("//  OtherAlias.OtherCommentedOutField                                                                    as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("  _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignAliases() {
		rule.configAlignAliases.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out select list elements");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out select list elements");
		buildExp("//  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testConsiderSimpleElementsWithoutAlias() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField     as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField        as FifthFieldAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildSrc("      + OtherAlias.SixthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("//  OtherAlias.CommentedOutFieldWithoutAlias,");
		buildSrc("");
		buildSrc("    // associations");
		buildSrc(" _ThirdAliasWithVeryLongName");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                                     as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                 as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                    as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                 as FifthFieldAlias,");
		buildExp("      _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildExp("      _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildExp("           + OtherAlias.SixthField as any_type)             as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                          as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                     as OtherCommentedOutFieldAlias,");
		buildExp("//      OtherAlias.CommentedOutFieldWithoutAlias,");
		buildExp("");
		buildExp("      // associations");
		buildExp("      _ThirdAliasWithVeryLongName");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotConsiderSimpleElementsWithoutAlias() {
		rule.configConsiderSimpleElementsWithoutAlias.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField     as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField        as FifthFieldAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildSrc("      + OtherAlias.SixthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("//  OtherAlias.CommentedOutFieldWithoutAlias,");
		buildSrc("");
		buildSrc("    // associations");
		buildSrc(" _ThirdAliasWithVeryLongName");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                         as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                     as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName        as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                     as FifthFieldAlias,");
		buildExp("      _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildExp("      _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildExp("           + OtherAlias.SixthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField              as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField         as OtherCommentedOutFieldAlias,");
		buildExp("//      OtherAlias.CommentedOutFieldWithoutAlias,");
		buildExp("");
		buildExp("      // associations");
		buildExp("      _ThirdAliasWithVeryLongName");
		buildExp("}");

		testRule();
	}

	@Test
	void testConsiderComplexElementsWithoutAlias() {
		rule.configConsiderComplexElementsWithoutAlias.setValue(true);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField     as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField        as FifthFieldAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildSrc("      + OtherAlias.SixthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("//  OtherAlias.CommentedOutFieldWithoutAlias,");
		buildSrc("");
		buildSrc("    // associations");
		buildSrc(" _ThirdAliasWithVeryLongName");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                                                                       as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                   as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                      as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                   as FifthFieldAlias,");
		buildExp("      _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildExp("      _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildExp("           + OtherAlias.SixthField as any_type)                                               as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                                                            as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                                                       as OtherCommentedOutFieldAlias,");
		buildExp("//      OtherAlias.CommentedOutFieldWithoutAlias,");
		buildExp("");
		buildExp("      // associations");
		buildExp("      _ThirdAliasWithVeryLongName");
		buildExp("}");

		testRule();
	}

	@Test
	void testConsiderAllElementLines() {
		rule.configConsiderAllElementLines.setValue(true);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource2 as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField     as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField        as FifthFieldAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildSrc("         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildSrc("      + OtherAlias.SixthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("//  OtherAlias.CommentedOutFieldWithoutAlias,");
		buildSrc("");
		buildSrc("    // associations");
		buildSrc(" _ThirdAliasWithVeryLongName");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource2 as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                                                                                   as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                               as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                                  as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                               as FifthFieldAlias,");
		buildExp("      _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,");
		buildExp("      _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName");
		buildExp("           + OtherAlias.SixthField as any_type)                                                           as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                                                                        as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                                                                   as OtherCommentedOutFieldAlias,");
		buildExp("//      OtherAlias.CommentedOutFieldWithoutAlias,");
		buildExp("");
		buildExp("      // associations");
		buildExp("      _ThirdAliasWithVeryLongName");
		buildExp("}");

		testRule();
	}

	@Test
	void testMaxAliasStartLow() {
		rule.configMaxAliasStart.setValue(60);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                     as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                 as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                    as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                 as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type)");
		buildExp("                                                            as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                          as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                     as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignTextualComments() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc("// calculated fields");
		buildSrc("// A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildSrc("// Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                                as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      // calculated fields");
		buildExp("      // A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildExp("      // Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                                                                         as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                                                                    as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignTextualComments() {
		rule.configAlignTextualComments.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc("// calculated fields");
		buildSrc("// A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildSrc("// Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                                as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("// calculated fields");
		buildExp("// A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildExp("// Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it.");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField                                                                         as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField                                                                    as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAlignCommentedOutCode() {
		rule.configAlignCommentedOutCode.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName                                                                   as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField                                                                                as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("");
		buildExp("//   // Commented-out");
		buildExp("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoKeywords() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  AnyAlias.AnyField                  as AnyFieldAlias,");
		buildExp("");
		buildExp("  OtherAlias.ThirdField              as ThirdFieldAlias,");
		buildExp("  OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("  OtherAlias.FifthField              as FifthFieldAlias,");
		buildExp("");
		buildExp("//  // Commented-out");
		buildExp("//  OtherAlias.CommentedOutField       as CommentedOutFieldAlias,");
		buildExp("//  OtherAlias.OtherCommentedOutField  as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("  _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testNoKeywordsDoNotAlignKeywords() {
		rule.configAlignKeyKeyword.setValue(false);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  AnyAlias.AnyField                  as AnyFieldAlias,");
		buildExp("");
		buildExp("  OtherAlias.ThirdField              as ThirdFieldAlias,");
		buildExp("  OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("  OtherAlias.FifthField              as FifthFieldAlias,");
		buildExp("");
		buildExp("//  // Commented-out");
		buildExp("//  OtherAlias.CommentedOutField       as CommentedOutFieldAlias,");
		buildExp("//  OtherAlias.OtherCommentedOutField  as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("  _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testEmptyLineStartComments() {
		rule.configMaxAliasStart.setValue(90);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("//");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("///");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField                  as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField              as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//");
		buildExp("//  key OtherAlias.CommentedOutField       as CommentedOutFieldAlias,");
		buildExp("///");
		buildExp("//      OtherAlias.OtherCommentedOutField  as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testCommentedOutMultiLineNotMoved() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//        cast(OtherAlias.AnyNumericField");
		buildSrc("//             + OtherAlias.OtherNumericField) as AnyNumericAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("      @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildExp("");
		buildExp("//      // Commented-out");
		buildExp("//  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("//        cast(OtherAlias.AnyNumericField");
		buildExp("//             + OtherAlias.OtherNumericField) as AnyNumericAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeywordVirtual() {
		rule.configMaxAliasStart.setValue(60);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("   @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("  virtual AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("//   // Commented-out");
		buildSrc("// virtual OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("          @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("  virtual AnyAlias.AnyField                                 as AnyFieldAlias,");
		buildExp("");
		buildExp("          OtherAlias.ThirdField                             as ThirdFieldAlias,");
		buildExp("          OtherAlias.FourthFieldWithLongName                as FourthFieldAlias,");
		buildExp("          OtherAlias.FifthField                             as FifthFieldAlias,");
		buildExp("");
		buildExp("          cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type)");
		buildExp("                                                            as CalculatedField,");
		buildExp("");
		buildExp("//          // Commented-out");
		buildExp("//  virtual OtherAlias.CommentedOutField                      as CommentedOutFieldAlias,");
		buildExp("//          OtherAlias.OtherCommentedOutField                 as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("          _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testCommentedOutCodeAtStart() {
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("//   // Commented-out");
		buildSrc("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("//      // Commented-out");
		buildExp("// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("//      OtherAlias.OtherCommentedOutField  as OtherCommentedOutFieldAlias,");
		buildExp("");
		buildExp("  key AnyAlias.AnyField                  as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField              as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField              as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDDicBasedFromAfterSelectList() {
		buildSrc("define view I_AnyDDicBasedView");
		buildSrc("(");
		buildSrc("  AnyFieldAlias,");
		buildSrc("  OtherFieldAlias,");
		buildSrc("  ThirdFieldAlias,");
		buildSrc("  FourthFieldAliasWithLongName,");
		buildSrc("  FifthFieldAlias");
		buildSrc(")");
		buildSrc("");
		buildSrc("  as select");
		buildSrc("");
		buildSrc(" key AnyAlias.AnyField,");
		buildSrc("@ObjectModel.text.element: ['ThirdFieldAlias']");
		buildSrc("         OtherAlias.OtherField,");
		buildSrc("// OtherAlias.CommentedOutField,");
		buildSrc("    OtherAlias.ThirdField,");
		buildSrc("        _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("          _ThirdAlias._FourthAlias.FifthField");
		buildSrc("");
		buildSrc("  from  I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("    left outer join I_OtherSource as OtherAlias");
		buildSrc("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildSrc("");
		buildSrc("  association [0..*] to I_ThirdSource as _ThirdAlias");
		buildSrc("     on _ThirdAlias.OtherField = AnyAlias.OtherField");
		buildSrc("");
		buildSrc("group by AnyAlias.AnyField,");
		buildSrc("         OtherAlias.OtherField,");
		buildSrc("//         OtherAlias.CommentedOutField,");
		buildSrc("         OtherAlias.ThirdField,");
		buildSrc("         _ThirdAlias.FourthFieldWithLongName,");
		buildSrc("         _ThirdAlias._FourthAlias.FifthField");

		buildExp("define view I_AnyDDicBasedView");
		buildExp("(");
		buildExp("  AnyFieldAlias,");
		buildExp("  OtherFieldAlias,");
		buildExp("  ThirdFieldAlias,");
		buildExp("  FourthFieldAliasWithLongName,");
		buildExp("  FifthFieldAlias");
		buildExp(")");
		buildExp("");
		buildExp("  as select");
		buildExp("");
		buildExp("    key AnyAlias.AnyField,");
		buildExp("        @ObjectModel.text.element: ['ThirdFieldAlias']");
		buildExp("        OtherAlias.OtherField,");
		buildExp("//        OtherAlias.CommentedOutField,");
		buildExp("        OtherAlias.ThirdField,");
		buildExp("        _ThirdAlias.FourthFieldWithLongName,");
		buildExp("        _ThirdAlias._FourthAlias.FifthField");
		buildExp("");
		buildExp("  from  I_AnySource as AnyAlias");
		buildExp("");
		buildExp("    left outer join I_OtherSource as OtherAlias");
		buildExp("      on AnyAlias.AnyField = OtherAlias.AnyField");
		buildExp("");
		buildExp("  association [0..*] to I_ThirdSource as _ThirdAlias");
		buildExp("     on _ThirdAlias.OtherField = AnyAlias.OtherField");
		buildExp("");
		buildExp("group by AnyAlias.AnyField,");
		buildExp("         OtherAlias.OtherField,");
		buildExp("//         OtherAlias.CommentedOutField,");
		buildExp("         OtherAlias.ThirdField,");
		buildExp("         _ThirdAlias.FourthFieldWithLongName,");
		buildExp("         _ThirdAlias._FourthAlias.FifthField");

		testRule();
	}

	@Test
	void testMultiLineCommentSkipped() {
		rule.configMaxAliasStart.setValue(90);

		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildSrc("       OtherAlias.FifthField");
		buildSrc("         as FifthFieldAlias,");
		buildSrc("  // complex element");
		buildSrc("         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildSrc("");
		buildSrc("/*");
		buildSrc(" // Commented-out");
		buildSrc("  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("   OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("*/");
		buildSrc("");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                  as AnyFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField              as ThirdFieldAlias,");
		buildExp("      OtherAlias.FourthFieldWithLongName as FourthFieldAlias,");
		buildExp("      OtherAlias.FifthField              as FifthFieldAlias,");
		buildExp("      // complex element");
		buildExp("      _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,");
		buildExp("");
		buildExp("/*");
		buildExp(" // Commented-out");
		buildExp("  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("   OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildExp("*/");
		buildExp("");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testMultiLineAnnotation() {
		// ensure that the multi-line annotation does not influence the alias position, but the 'simple element' does
		
		buildSrc("define view C_AnyView");
		buildSrc(" as select from I_AnyView");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @AnalyticsDetails.exceptionAggregationSteps.exceptionAggregationElements:");
		buildSrc("      ['AnyField','OtherField']");
		buildSrc("      1 as AnyField,");
		buildSrc("      AnyAlias.OtherField,");
		buildSrc("      AnyAlias._AnyAssociation.ThirdField,");
		buildSrc(" }");

		buildExp("define view C_AnyView");
		buildExp(" as select from I_AnyView");
		buildExp("");
		buildExp("{");
		buildExp("  @AnalyticsDetails.exceptionAggregationSteps.exceptionAggregationElements:");
		buildExp("  ['AnyField','OtherField']");
		buildExp("  1                    as AnyField,");
		buildExp("  AnyAlias.OtherField,");
		buildExp("  AnyAlias._AnyAssociation.ThirdField,");
		buildExp(" }");

		testRule();
	}

	@Test
	void testOnlyTextualMultiLineCommentMoved() {
		// ensure that only the textual multi-line comment is being moved, while the multi-line comment that contains code is not
		buildSrc("define view I_AnyView");
		buildSrc("  as select from I_AnySource as AnyAlias");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key AnyAlias.AnyField as AnyFieldAlias,");
		buildSrc(" key   AnyAlias.OtherField as OtherFieldAlias,");
		buildSrc("");
		buildSrc("   OtherAlias.ThirdField as ThirdFieldAlias,");
		buildSrc("");
		buildSrc("/* A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildSrc("   Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it. */");
		buildSrc(" cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildSrc("/* ");
		buildSrc("  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildSrc("      OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildSrc("      OtherAlias.CommentedOutFieldWithoutAlias, */");
		buildSrc("");
		buildSrc("    // associations");
		buildSrc(" _ThirdAlias");
		buildSrc("}");

		buildExp("define view I_AnyView");
		buildExp("  as select from I_AnySource as AnyAlias");
		buildExp("");
		buildExp("{");
		buildExp("  key AnyAlias.AnyField                                                                                    as AnyFieldAlias,");
		buildExp("  key AnyAlias.OtherField                                                                                  as OtherFieldAlias,");
		buildExp("");
		buildExp("      OtherAlias.ThirdField                                                                                as ThirdFieldAlias,");
		buildExp("");
		buildExp("      /* A textual comment like this one should not be at line start, but should be aligned with the elements.");
		buildExp("         Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it. */");
		buildExp("      cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,");
		buildExp("/* ");
		buildExp("  key OtherAlias.CommentedOutField as CommentedOutFieldAlias,");
		buildExp("      OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,");
		buildExp("      OtherAlias.CommentedOutFieldWithoutAlias, */");
		buildExp("");
		buildExp("      // associations");
		buildExp("      _ThirdAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testIdentifiersWithNamespaces() {
		buildSrc("define view entity /ANY/I_AnyView");
		buildSrc("  with parameters");
		buildSrc("    P_Any : /ANY/any_type");
		buildSrc("");
		buildSrc("  as select from /ANY/I_OtherView as /ANY/A/l/i/a/s/");
		buildSrc("");
		buildSrc("  association to /ANY/I_ThirdView as  _/Other/Alias");
		buildSrc("  on _/Other/Alias.AnyID = /ANY/A/l/i/a/s/.AnyID");
		buildSrc("");
		buildSrc("{");
		buildSrc("  key  /ANY/A/l/i/a/s/./ANY/Field/ as /ANY/Field/,");
		buildSrc("");
		buildSrc("      2 / $parameters.P_Any as /ANY/OtherField,");
		buildSrc("      1 + 10 * /ANY/A/l/i/a/s/./ANY/ThirdField / 2 as /ANY/ThirdField,");
		buildSrc("      1 + 10 * /ANY/FourthField / 2 as /ANY/FourthField/,");
		buildSrc("      1 + _/Other/Alias./ANY/FifthField/ as /ANY/FifthField/,");
		buildSrc("      _/Other/Alias");
		buildSrc("}");

		buildExp("define view entity /ANY/I_AnyView");
		buildExp("  with parameters");
		buildExp("    P_Any : /ANY/any_type");
		buildExp("");
		buildExp("  as select from /ANY/I_OtherView as /ANY/A/l/i/a/s/");
		buildExp("");
		buildExp("  association to /ANY/I_ThirdView as  _/Other/Alias");
		buildExp("  on _/Other/Alias.AnyID = /ANY/A/l/i/a/s/.AnyID");
		buildExp("");
		buildExp("{");
		buildExp("  key /ANY/A/l/i/a/s/./ANY/Field/                  as /ANY/Field/,");
		buildExp("");
		buildExp("      2 / $parameters.P_Any                        as /ANY/OtherField,");
		buildExp("      1 + 10 * /ANY/A/l/i/a/s/./ANY/ThirdField / 2 as /ANY/ThirdField,");
		buildExp("      1 + 10 * /ANY/FourthField / 2                as /ANY/FourthField/,");
		buildExp("      1 + _/Other/Alias./ANY/FifthField/           as /ANY/FifthField/,");
		buildExp("      _/Other/Alias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDdicBasedSelectListStartingWithComment() {
		buildSrc("define view AnyView as");
		buildSrc("  select");
		buildSrc(" // a.AnyField,");
		buildSrc("a.OtherField");
		buildSrc("");
		buildSrc("  from AnySource as a");

		buildExp("define view AnyView as");
		buildExp("  select");
		buildExp("    // a.AnyField,");
		buildExp("    a.OtherField");
		buildExp("");
		buildExp("  from AnySource as a");

		testRule();
	}
}
