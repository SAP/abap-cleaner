package com.sap.adt.abapcleaner.rules.ddl.spaces;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlTypoTest extends RuleTestBase {
	private DdlTypoRule rule;
	
	DdlTypoTest() {
		super(RuleID.DDL_TYPO);
		rule = (DdlTypoRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configCorrectTypos.setValue(true);
		rule.configConvertBritishToAmerican.setValue(true);

		rule.configProcessComments.setValue(true);
		rule.configProcessAnnotations.setValue(true);
		rule.configProcessAnnotationRefs.setValue(false);
	}

	@Test
	void testCorrectAll() {
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  // assocation to coresponding other view");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("      Doc.Mesage,");
		buildSrc("      Doc.Valididty,");
		buildSrc("");
		buildSrc("      // caculate total ammount");
		buildSrc("      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,");
		buildSrc("");
		buildSrc("      /* to optimise perfomance, only expose small selction of");
		buildSrc("         fields neccessary for the applicaton; futher detials");
		buildSrc("         could be made avaialable separatly via extention */");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorization check required");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("// TODO: check spelling: dcoument (typo) -> document (ABAP cleaner)");
		buildExp("// TODO: check spelling: imformation (typo) -> information (ABAP cleaner)");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("// TODO: check spelling: Mesage (typo) -> Message (ABAP cleaner)");
		buildExp("// TODO: check spelling: Valididty (typo) -> Validity (ABAP cleaner)");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  // association to corresponding other view");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // document ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- document name and attributes");
		buildExp("      Doc.DocumentName,");
		buildExp("      Doc.Mesage,");
		buildExp("      Doc.Valididty,");
		buildExp("");
		buildExp("      // calculate total amount");
		buildExp("      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,");
		buildExp("");
		buildExp("      /* to optimize performance, only expose small selection of");
		buildExp("         fields necessary for the application; further details");
		buildExp("         could be made available separately via extension */");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotCorrectTypos() {
		rule.configCorrectTypos.setValue(false);
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  // assocation to coresponding other view");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("      Doc.Mesage,");
		buildSrc("      Doc.Valididty,");
		buildSrc("");
		buildSrc("      // caculate total ammount");
		buildSrc("      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,");
		buildSrc("");
		buildSrc("      /* to optimise perfomance, only expose small selction of");
		buildSrc("         fields neccessary for the applicaton; futher detials");
		buildSrc("         could be made avaialable separatly via extention */");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorization check requried");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  // assocation to coresponding other view");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // docuent ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- docuemnt name and attibutes");
		buildExp("      Doc.DocumentName,");
		buildExp("      Doc.Mesage,");
		buildExp("      Doc.Valididty,");
		buildExp("");
		buildExp("      // caculate total ammount");
		buildExp("      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,");
		buildExp("");
		buildExp("      /* to optimize perfomance, only expose small selction of");
		buildExp("         fields neccessary for the applicaton; futher detials");
		buildExp("         could be made avaialable separatly via extention */");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotChangeBritishEnglish() {
		// expect the words "authorisation" and "optimise" to remain unchanged
		
		rule.configConvertBritishToAmerican.setValue(false);
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      /* to optimise perfomance, only expose small selction of");
		buildSrc("         fields neccessary for the applicaton; futher detials");
		buildSrc("         could be made avaialable separatly via extention */");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorisation check required");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("{");
		buildExp("      // document ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      /* to optimise performance, only expose small selection of");
		buildExp("         fields necessary for the application; further details");
		buildExp("         could be made available separately via extension */");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotProcessComments() {
		rule.configProcessComments.setValue(false);
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  // assocation to coresponding other view");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("");
		buildSrc("      /* to optimise perfomance, only expose small selction of");
		buildSrc("         fields neccessary for the applicaton; futher detials");
		buildSrc("         could be made avaialable separatly via extention */");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorisation check requried");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("// TODO: check spelling: dcoument (typo) -> document (ABAP cleaner)");
		buildExp("// TODO: check spelling: imformation (typo) -> information (ABAP cleaner)");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("// TODO: check spelling: Mesage (typo) -> Message (ABAP cleaner)");
		buildExp("// TODO: check spelling: Valididty (typo) -> Validity (ABAP cleaner)");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  // assocation to coresponding other view");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // docuent ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- docuemnt name and attibutes");
		buildExp("      Doc.DocumentName,");
		buildExp("");
		buildExp("      /* to optimise perfomance, only expose small selction of");
		buildExp("         fields neccessary for the applicaton; futher detials");
		buildExp("         could be made avaialable separatly via extention */");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotProcessAnnotationValues() {
		rule.configProcessAnnotations.setValue(false);
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  /* assocation to coresponding other view */");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorization check required");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("// TODO: check spelling: Mesage (typo) -> Message (ABAP cleaner)");
		buildExp("// TODO: check spelling: Valididty (typo) -> Validity (ABAP cleaner)");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  /* association to corresponding other view */");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // document ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- document name and attributes");
		buildExp("      Doc.DocumentName,");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotProcessAnnotationRefs() {
		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  /* assocation to coresponding other view */");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorization check required");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("// TODO: check spelling: dcoument (typo) -> document (ABAP cleaner)");
		buildExp("// TODO: check spelling: imformation (typo) -> information (ABAP cleaner)");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  /* association to corresponding other view */");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // document ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- document name and attributes");
		buildExp("      Doc.DocumentName,");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testDoNotAddDuplicateTodoComments() {
		// expect only the non-generated to-do comment to be changed 
		
		rule.configProcessAnnotationRefs.setValue(true);

		buildSrc("// no authorization check required");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("// TODO: check spelling: dcoument (typo) -> document (ABAP cleaner)");
		buildSrc("// TODO: check spelling: imformation (typo) -> information (ABAP cleaner)");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("// TODO: check spelling: Mesage (typo) -> Message (ABAP cleaner)");
		buildSrc("// TODO: check spelling: Valididty (typo) -> Validity (ABAP cleaner)");
		buildSrc("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("  /* association to corresponding other view */");
		buildSrc("  association[1..* ] to I_OtherView as _OtherAlias");
		buildSrc("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // document ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- TODO: docuemnt name and attibutes");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		buildExp("// no authorization check required");
		buildExp("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildExp("");
		buildExp("// TODO: check spelling: dcoument (typo) -> document (ABAP cleaner)");
		buildExp("// TODO: check spelling: imformation (typo) -> information (ABAP cleaner)");
		buildExp("@EndUserText.label: 'main dcoument imformation'");
		buildExp("");
		buildExp("// TODO: check spelling: Mesage (typo) -> Message (ABAP cleaner)");
		buildExp("// TODO: check spelling: Valididty (typo) -> Validity (ABAP cleaner)");
		buildExp("@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]");
		buildExp("");
		buildExp("define view C_AnyView");
		buildExp("  as select from I_AnyDocument as Doc");
		buildExp("");
		buildExp("  /* association to corresponding other view */");
		buildExp("  association[1..* ] to I_OtherView as _OtherAlias");
		buildExp("    on Doc.DocumentId = _OtherAlias.DocumentId");
		buildExp("");
		buildExp("{");
		buildExp("      // document ID");
		buildExp("  key Doc.DocumentId,");
		buildExp("");
		buildExp("      -- TODO: document name and attributes");
		buildExp("");
		buildExp("      _OtherAlias");
		buildExp("}");

		testRule();
	}

	@Test
	void testEarlyExit() {
		rule.configCorrectTypos.setValue(false);
		rule.configConvertBritishToAmerican.setValue(false);

		buildSrc("// no authorisation check requried");
		buildSrc("@AccessControl.authorizationCheck: #NOT_REQUIRED");
		buildSrc("");
		buildSrc("@EndUserText.label: 'main dcoument imformation'");
		buildSrc("");
		buildSrc("define view C_AnyView");
		buildSrc("  as select from I_AnyDocument as Doc");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // docuent ID");
		buildSrc("  key Doc.DocumentId,");
		buildSrc("");
		buildSrc("      -- docuemnt name and attibutes");
		buildSrc("      Doc.DocumentName,");
		buildSrc("");
		buildSrc("      // caculate total ammount");
		buildSrc("      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,");
		buildSrc("");
		buildSrc("      _OtherAlias");
		buildSrc("}");

		copyExpFromSrc();

		testRule();
	}

}
