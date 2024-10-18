package com.sap.adt.abapcleaner.parser;

import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;

public class DdlObfuscatorTest {
	private final String SEP = ABAP.LINE_SEPARATOR; // this must be the same separator that is used in Token.toString()
	private final Language language = Language.DDL;
	
	private Obfuscator obfuscator;
	
	private void prepareViewScopeLongNames() {
		obfuscator = Obfuscator.createFor(language, false, false, false, false, false, false);
	}
	
	private void prepareViewScopeShortNames() {
		obfuscator = Obfuscator.createFor(language, false, true, false, false, false, false);
	}
	
	private void prepareCommandScopeShortNames() {
		obfuscator = Obfuscator.createFor(language, true, true, false, false, false, false);
	}
	
	private void prepareViewScopeLongNamesSimplified() {
		obfuscator = Obfuscator.createFor(language, false, false, true, false, false, false);
	}
	
	private void prepareViewScopeShortNamesLiterals() {
		obfuscator = Obfuscator.createFor(language, false, true, false, false, false, true);
	}
	
	private void prepareCommandScopeShortNamesLiterals() {
		obfuscator = Obfuscator.createFor(language, true, true, false, false, false, true);
	}

	private void prepareViewScopeLongNamesRemoveComments() {
		obfuscator = Obfuscator.createFor(language, false, false, false, true, true, false);
	}

	private void test(String codeText, String expCodeText) {
   	Code code;
   	try {
   		code = obfuscator.obfuscate(codeText);
		} catch (UnexpectedSyntaxAfterChanges | ParseException e) {
			fail();
			return;
		}
   	String actCodeText = code.toString();
   	
   	// helper for creating tests:
   	/*
   	if (!actCodeText.equals(expCodeText)) {
   		// only report differences
	   	System.out.print("\t\ttest(\"" + codeText.replaceAll("\r\n", "\" + SEP + \"") + "\",");
	   	System.out.println();
	   	System.out.print("\t\t\t\t\"" + actCodeText.replaceAll("\r\n", "\" + SEP + \"") + "\");");
	   	System.out.println();
   	}
   	*/
   	assertEquals(expCodeText, actCodeText);
	}

	@Test
	void testViewScopeLongNames() {
		prepareViewScopeLongNames();

		// entity name
		test("define view entity vw as select from src as al { key fld }",
				"define view entity I_AnyView as select from any_datasource as AnyAlias { key any_field }");
		test("define view entity Vw as select from Src as Al { key Fld as Fld }",
				"define view entity I_AnyView as select from I_AnyDataSource as AnyAlias { key AnyField as AnyField }");

		// parameter names and types
		test("define view entity Vw with parameters Par1 : ty1, Par2 : ty2 as select from Src as Al1 { key Fld1 as Fld1, $parameters.Par1 as Fld2, $parameters.Par2 as Fld3 }",
				"define view entity I_AnyView with parameters P_AnyParameter : any_type, P_OtherParameter : other_type as select from I_AnyDataSource as AnyAlias { key AnyField as AnyField, $parameters.P_AnyParameter as OtherField, $parameters.P_OtherParameter as ThirdField }");
		test("define view entity Vw with parameters Par1 : ty1, Par2 : ty1 as select from Src as Al1 { key Fld1 as Fld1, $parameters.Par1 as Fld2, $parameters.Par2 as Fld3 }",
				"define view entity I_AnyView with parameters P_AnyParameter : any_type, P_OtherParameter : any_type as select from I_AnyDataSource as AnyAlias { key AnyField as AnyField, $parameters.P_AnyParameter as OtherField, $parameters.P_OtherParameter as ThirdField }");
		test("define view entity Vw with parameters Par1 : ty1, Par2 : ty2 as select from Src( Par1 : Par1, Par2 : Par2) as Al1 { key Fld1 as Fld1, Par1 as Fld2, Par2 as Fld3 }",
				"define view entity I_AnyView with parameters P_AnyParameter : any_type, P_OtherParameter : other_type as select from I_AnyDataSource( P_AnyParameter : P_AnyParameter, P_OtherParameter : P_OtherParameter) as AnyAlias { key AnyField as AnyField, P_AnyParameter as OtherField, P_OtherParameter as ThirdField }");
		test("define view Vw with parameters Par1 : ty1, Par2 : ty2 as select from Src( Par1 : :Par1 ) as Al1 { key Fld1 as Fld1, :Par1 as Fld2, :Par2 as Fld3 }",
				"define view I_AnyView with parameters P_AnyParameter : any_type, P_OtherParameter : other_type as select from I_AnyDataSource( P_AnyParameter : :P_AnyParameter ) as AnyAlias { key AnyField as AnyField, :P_AnyParameter as OtherField, :P_OtherParameter as ThirdField }");
		test("define view entity Vw with parameters Par1 : Ty1, Par2 : Ty2 as select from Src as Al1 { key Fld1 as Fld1, $parameters.Par1 as Fld2, $parameters.Par2 as Fld3 }",
				"define view entity I_AnyView with parameters P_AnyParameter : AnyType, P_OtherParameter : OtherType as select from I_AnyDataSource as AnyAlias { key AnyField as AnyField, $parameters.P_AnyParameter as OtherField, $parameters.P_OtherParameter as ThirdField }");

		// data sources
		test("define view entity Vw as select from Src1 as Al1 inner join Src2 as Al2 on Al1.Fld1 = Al2.Fld1 and Al1.Fld2 = Al2.Fld2 { key Al1.Fld1, Al2.Fld2, Al2.Fld3 }",
				"define view entity I_AnyView as select from I_AnyDataSource as AnyAlias inner join I_OtherDataSource as OtherAlias on AnyAlias.AnyField = OtherAlias.AnyField and AnyAlias.OtherField = OtherAlias.OtherField { key AnyAlias.AnyField, OtherAlias.OtherField, OtherAlias.ThirdField }");
		test("define view entity Vw as select from Src1 as Al1 association [0..*] to Src2 as _Al2 on Al1.Fld1 = _Al2.Fld1 and Al1.Fld2 = _Al2.Fld2 { key Al1.Fld1, _Al2.Fld2, _Al2 }",
				"define view entity I_AnyView as select from I_AnyDataSource as AnyAlias association [0..*] to I_OtherDataSource as _OtherAlias on AnyAlias.AnyField = _OtherAlias.AnyField and AnyAlias.OtherField = _OtherAlias.OtherField { key AnyAlias.AnyField, _OtherAlias.OtherField, _OtherAlias }");
	}

	@Test
	void testCommandScopeShortNames() {
		prepareCommandScopeShortNames();

		test("define view entity Vw as select from Src1 as Al1 inner join Src2 as Al2 on Al1.Fld1 = Al2.Fld1 and Al1.Fld2 = Al2.Fld2 { key Al1.Fld1, Al2.Fld2, Al2.Fld3 }",
				"define view entity V1 as select from D1 as A1 inner join D1 as A1 on A2.F1 = A1.F1 and A2.F2 = A1.F2 { key A1.F1, A1.F1, A1.F1 }");
		test("define view entity Vw as select from Src1 as Al1 association [0..*] to Src2 as _Al2 on Al1.Fld1 = _Al2.Fld1 and Al1.Fld2 = _Al2.Fld2 { key Al1.Fld1, _Al2.Fld2, _Al2 }",
				"define view entity V1 as select from D1 as A1 association [0..*] to D1 as _A1 on A2.F1 = _A1.F1 and A2.F2 = _A1.F2 { key A1.F1, _A1.F1, _A1 }");
	}	
	
	@Test
	void testViewScopeShortNames() {
		prepareViewScopeShortNames();

		// after a union, the same aliases can be reused
		test("define view entity Vw as select from Src1 as Al1 { key Al1.Fld as Fld } union all select from Src2 as Al1 { key Al1.Fld as Fld }",
				"define view entity V1 as select from D1 as A1 { key A1.F1 as F1 } union all select from D2 as A1 { key A1.F1 as F1 }");

		// line-end comments
		test("define view entity Vw // cmt1" + SEP + "as select from Src1 as Al1 --cmt2" + SEP + "{ /* cmt3 */" + SEP + "cast(F1 + F2 as ty1) as F3 }",
				"define view entity V1 // comment" + SEP + "as select from D1 as A1 // comment" + SEP + "{ /* comment */" + SEP + "cast(F1 + F2 as A2) as A3 }");
		
		// comment line Commands
		test("define view entity Vw // cmt1" + SEP + "as select from Src1 as Al1" + SEP + "--cmt2" + SEP + "{" + SEP + "" + SEP + "/* cmt3 */" + SEP + "cast(F1 + F2 as ty1) as F3 }",
				"define view entity V1 // comment" + SEP + "as select from D1 as A1" + SEP + "// comment" + SEP + "{" + SEP + "" + SEP + "/* comment */" + SEP + "cast(F1 + F2 as t1) as F3 }");
		
		// comment line Tokens
		test("define view entity" + SEP + "// cmt1" + SEP + "Vw as select from Src1 as" + SEP + "--cmt2" + SEP + "Al1 { key" + SEP + "" + SEP + "/* cmt3 */" + SEP + "F1 }",
				"define view entity" + SEP + "// comment" + SEP + "V1 as select from D1 as" + SEP + "// comment" + SEP + "A1 { key" + SEP + "" + SEP + "/* comment */" + SEP + "F1 }");
		
		// comment /* ... */ inside of a line
		test("define view entity" + SEP + "// cmt1" + SEP + "Vw as select from Src1 as" + SEP + "--cmt2" + SEP + "Al1 { key /* cmt3 */ F1 }",
				"define view entity" + SEP + "// comment" + SEP + "V1 as select from D1 as" + SEP + "// comment" + SEP + "A1 { key /* comment */ F1 }");
	}

	@Test
	void testViewScopeLongNamesSimplified() {
		prepareViewScopeLongNamesSimplified();
		
		// simplified identifiers work, but make little sense in DDL
		test("define view entity Vw as select from Src1 as Al1 inner join Src2 as Al2 on Al1.Fld1 = Al2.Fld1 and Al1.Fld2 = Al2.Fld2 { key Al1.Fld1, Al2.Fld2, Al2.Fld3 }",
				"define view entity I_AnyView as select from I_AnyDataSource as AnyAlias inner join I_OtherDataSource as OtherAlias on AnyField = OtherField and ThirdField = FourthField { key AnyField, FourthField, FifthField }");
	}	

	@Test
	void testViewScopeShortNamesLiterals() {
		prepareViewScopeShortNamesLiterals();

		// annotations references
		test("define view entity Vw as select from Src1 as Al1 { @Semantics.amount.currencyCode: 'Fld2' key Al1.Fld1, @Consumption.valueHelpDefinition.entity:[{name: 'Src2', element:'FldX'}] Al2.Fld2, Al2.Fld3 }",
				"define view entity V1 as select from D1 as A1 { @Semantics.amount.currencyCode: 'F1' key A1.F2, @Consumption.valueHelpDefinition.entity:[{name: 'V2', element:'F3'}] A2.F1, A2.F4 }");
		test("@ObjectModel.foreignKey.association: '_Al2' define view entity Vw as select from Src1 as Al1 association [0..*] to Src2 as _Al2 on Al1.Fld1 = _Al2.Fld1 { key Al1.F1, _Al2.F2 }",
				"@ObjectModel.foreignKey.association: '_A1' define view entity V1 as select from D1 as A2 association [0..*] to D2 as _A1 on A2.F1 = _A1.F1 { key A2.F2, _A1.F3 }");
		test("@Semantics.interval: [ lowerBoundaryParameter: 'Par1' ] define view entity Vw with parameters Par1 : ty1, Par2 : ty2 as select from Src as Al1 { key Fld1 as Fld1, $parameters.Par1 as Fld2, $parameters.Par2 as Fld3 }",
				"@Semantics.interval: [ lowerBoundaryParameter: 'P1' ] define view entity V1 with parameters P1 : t1, P2 : t2 as select from D1 as A1 { key F1 as F1, $parameters.P1 as F2, $parameters.P2 as F3 }");
		test("@EndUserText.label: 'any text' define view entity vw as select from src as al { key fld }",
				"@EndUserText.label: 'T1' define view entity V1 as select from d1 as A1 { key f1 }");

		// unsupported annotations with comments
		test("define view entity Vw as select from Src1 as Al1 { @Semantics.amount.currencyCode: /* comment */ 'Fld2' key Al1.Fld1, @Consumption.valueHelpDefinition.entity:[{name: 'Src2', element:'FldX'}] Al2.Fld2, Al2.Fld3 }",
				"define view entity V1 as select from D1 as A1 { @Semantics.amount.currencyCode: /* comment */ 'F1' key A1.F2, @Consumption.valueHelpDefinition.entity:[{name: 'V2', element:'F3'}] A2.F1, A2.F4 }");

		// literals outside of annotations
		test("define view entity Vw as select from Src1 as Al1 { key Al1.Fld1 as Fld1, 'Lit1' as Fld2, concat('Lit2', 'Lit3') as Fld3, cast(42 as any_type) as Fld4 }",
				"define view entity V1 as select from D1 as A1 { key A1.F1 as F1, 'T1' as F2, concat('T2', 'T3') as F3, cast(2 as t1) as F4 }");

		// fields that just sound like built-in function names 
		test("define view entity Vw as select from dtab { key dtab.cast as Fld1, concat as Fld2, switch_runtime_state as Fld3 }",
				"define view entity V1 as select from d1 { key d1.f1 as F1, f2 as F2, f3 as F3 }");
	}

	@Test
	void testCommandScopeShortNamesLiterals() {
		prepareCommandScopeShortNamesLiterals();

		// typed literal and $session variable 
		test("define view entity Vw as select from Src1 as Al1 { key Al1.Fld1 as Fld1, 'Lit1' as Fld2, abap.int4'1234' as Fld3, $session.client as Fld4 }",
				"define view entity V1 as select from D1 as A1 { key A1.F1 as F1, 'T1' as F1, abap.int4'2' as F1, $session.client as F1 }");
	}

	@Test
	void testViewScopeLongNamesRemoveComments() {
		prepareViewScopeLongNamesRemoveComments();
		
		// remove line-end comments
		test("define view entity Vw // comment" + SEP + "as select from Src1 as Al1 --comment" + SEP + "{ /* comment */" + SEP + "cast(F1 + F2 as ty1) as F3 }",
				"define view entity I_AnyView" + SEP + "as select from I_AnyDataSource as AnyAlias" + SEP + "{" + SEP + "cast(AnyField + OtherField as OtherAlias) as ThirdAlias }");
		
		// remove comment line Commands
		test("define view entity Vw // comment" + SEP + "as select from Src1 as Al1" + SEP + "--comment" + SEP + "{" + SEP + SEP + "/* comment */" + SEP + "cast(F1 + F2 as ty1) as F3 }",
				"define view entity I_AnyView" + SEP + "as select from I_AnyDataSource as AnyAlias" + SEP + "{" + SEP + SEP + "cast(AnyField + OtherField as any_type) as ThirdField }");

		// remove comment line Tokens
		test("define view entity" + SEP + "// comment" + SEP + "Vw as select from Src1 as" + SEP + "--comment" + SEP + "Al1 { key" + SEP + SEP + "/* comment */" + SEP + "F1 }",
				"define view entity" + SEP + "I_AnyView as select from I_AnyDataSource as" + SEP + "AnyAlias { key" + SEP + SEP + "AnyField }");

		// comment /* ... */ inside of a line
		test("define view entity" + SEP + "// cmt1" + SEP + "Vw as select from Src1 as" + SEP + "--cmt2" + SEP + "Al1 { key /* cmt3 */ F1 }",
				"define view entity" + SEP + "I_AnyView as select from I_AnyDataSource as" + SEP + "AnyAlias { key AnyField }");
	}
}
