package com.sap.adt.abapcleaner.rulehelpers;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.programbase.ParseException;

public class DdlAnalyzerTest {
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;
	private static String[] elemRefAnnotationPaths = new String[] { "Semantics.amount.currencyCode", "Semantics.quantity.unitOfMeasure" };
	private StringBuilder sb;
	private DdlAnalyzer ddlAnalyzer;
	
	@BeforeEach
	void setUp() {
		sb = new StringBuilder();
		ddlAnalyzer = new DdlAnalyzer();
	}

	void buildSrc(String line) {
		buildCode(line);
	}

	private void buildCode(String line) {
		if (sb.length() > 0)
			sb.append(LINE_SEP);
		sb.append(StringUtil.trimEnd(line));
	}
	
	private void addFile(String fileName) {
		String sourceCode = sb.toString();
		sb = new StringBuilder();

		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, ABAP.NEWEST_RELEASE));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}
	
		ddlAnalyzer.addFile(fileName, code);
	}
	
	private void addNullCode() {
		ddlAnalyzer.addFile("any_file.txt", null);
	}

	private void addAbapReport() {
		buildSrc("REPORT any_report.");
		buildSrc("");
		buildSrc("START-OF-SELECTION.");
		buildSrc("  RETURN.");

		addFile("any_report.txt");
	}

	private void addSubBaseView() {
		buildSrc("define view I_SubBaseView");
		buildSrc("  as select from I_SubSource");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      @Semantics.amount.currencyCode: 'AssociatedCurrency'");
		buildSrc("      AssociatedAmount,");
		buildSrc("      AssociatedCurrency,");
		buildSrc("");
		buildSrc("      SubBaseField");
		buildSrc("}");

		addFile("I_SubBaseView.txt"); // without package
	}

	private void addAnyBaseView() {
		buildSrc("define view I_AnyBaseView");
		buildSrc("  as select from I_AnySource");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      @Semantics.amount.currencyCode: 'AnyCurrency'");
		buildSrc("      AnyAmount,");
		buildSrc("      AnyCurrency");
		buildSrc("}");

		addFile("AnyPackage - I_AnyBaseView.txt");
	}

	private void addOtherBaseView() {
		buildSrc("define view I_OtherBaseView");
		buildSrc("  as select from I_OtherSource as OtherSource");
		buildSrc("  association [1..1] to I_SubBaseView as _SubBase");
		buildSrc("    on _SubBase.AnyKeyField = OtherSource.AnyKeyField");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      @Semantics.quantity.unitOfMeasure: 'AnyUnit'");
		buildSrc("      AnyQuantity,");
		buildSrc("      AnyUnit,");
		buildSrc("");
		buildSrc("      _SubBase.AssociatedAmount,");
		buildSrc("      _SubBase.AssociatedCurrency,");
		buildSrc("      _SubBase");
		buildSrc("}");

		addFile("AnyPackage - I_OtherBaseView.txt");
	}

	private void addThirdBaseView() {
		buildSrc("define view I_ThirdBaseView");
		buildSrc("  as select from I_ThirdSource");
		buildSrc("{");
		buildSrc("  key AnyKeyField,");
		buildSrc("      OtherField");
		buildSrc("}");

		addFile("AnyPackage - I_ThirdBaseView.txt");
	}

	private void addConsumingView() {
		buildSrc("define view entity I_ConsumingView");
		buildSrc("  with parameters");
		buildSrc("    @Annotation.subAnno: 'value'");
		buildSrc("    P_Any : any_type");
		buildSrc("    P_Other : other_type");
		buildSrc("");
		buildSrc("  as select from I_AnyBaseView as AnyBase");
		buildSrc("    inner join I_OtherBaseView as OtherBase");
		buildSrc("      on AnyBase.AnyKeyField = OtherBase.AnyKeyField");
		buildSrc("    inner join I_ThirdBaseView as ThirdBase");
		buildSrc("      on ThirdBase.AnyKeyField = AnyBase.AnyKeyField");
		buildSrc("{");
		buildSrc("      @Annotation.subAnno: 'value'");
		buildSrc("  key AnyBase.AnyKeyField,");
		buildSrc("      AnyBase.AnyAmount,");
		buildSrc("      AnyCurrency,");
		buildSrc("");
		buildSrc("      AnyQuantity,");
		buildSrc("      OtherBase.AnyUnit,");
		buildSrc("");
		buildSrc("      AssociatedAmount,");
		buildSrc("      AssociatedCurrency,");
		buildSrc("      _SubBase.SubBaseField,");
		buildSrc("");
		buildSrc("      // from ThirdBase:");
		buildSrc("      cast(OtherField as other_type) as OtherField,");
		buildSrc("      cast(concat('a', 'b') as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("  virtual VirtualField : virtual_type");
		buildSrc("}");
		buildSrc("");
		buildSrc("union all");
		buildSrc("  select from I_OtherBaseView as OtherBase");
		buildSrc("  association [1..1] to I_ThirdBaseView as _ThirdBase");
		buildSrc("    on _ThirdBase.AnyKeyField = OtherBase.AnyKeyField");
		buildSrc("{");
		buildSrc("  key OtherBase.AnyKeyField,");
		buildSrc("      0.00                  as AnyAmount,");
		buildSrc("      ''                    as AnyCurrency,");
		buildSrc("");
		buildSrc("      0                     as AnyQuantity,");
		buildSrc("      ''                    as AnyUnit,");
		buildSrc("");
		buildSrc("      -1 * AssociatedAmount as AssociatedAmount,");
		buildSrc("      OtherBase.AssociatedCurrency : localized,");
		buildSrc("      _SubBase.SubBaseField,");
		buildSrc("");
		buildSrc("      cast(_ThirdBase.OtherField as other_type) as OtherField,");
		buildSrc("      cast(case");
		buildSrc("             when CondField = 1 then '0'");
		buildSrc("             else '1'");
		buildSrc("           end as any_type) as CalculatedField,");
		buildSrc("");
		buildSrc("  virtual VirtualField : virtual_type");
		buildSrc("}");

		addFile("AnyPackage - I_ConsumingView.txt");
	}

	private void addAggregatingView() {
		buildSrc("define view entity I_AggregatingView");
		buildSrc("");
		buildSrc("  as select from I_AnyBaseView as AnyBase");
		buildSrc("  association [0..*] to I_UnknownView as _Unknown");
		buildSrc("{");
		buildSrc("  key AnyCurrency,");
		buildSrc("      sum(AnyBase.AnyAmount) as AnyAmount,");
		buildSrc("      _Unknown[1: AnyKeyField = 42].UnknownField,");
		buildSrc("      _Unknown[1: AnyKeyField = 42].OtherUnknown.OtherUnknownField,");
		buildSrc("      _UnknownAssociation[Language = 'E'].FullyUnknown");
		buildSrc("}");
		buildSrc("group by AnyCurrency,");
		buildSrc("         _Unknown.UnknownField,");
		buildSrc("         _Unknown.OtherUnknown.OtherUnknownField,");
		buildSrc("         _UnknownAssociation.FullyUnknown");

		addFile("AnyPackage - I_AggregatingView.txt");
	}

	private String getResult(String[] annotationPaths) {
		ddlAnalyzer.finishBuild();
		ddlAnalyzer.analyzeAnnotations(annotationPaths, true);
		return ddlAnalyzer.getResult(annotationPaths, null); 
	}

	@Test
	public void testEmpty() {
		addNullCode();
		addAbapReport();
		String result = getResult(elemRefAnnotationPaths);

		// expect the header line only
		assertEquals(1, result.split(System.lineSeparator()).length);
	}
	
	@Test
	public void testViewHierarchy() {
		addSubBaseView();
		addAnyBaseView();
		addOtherBaseView();
		addThirdBaseView();
		addConsumingView();
		
		String result = getResult(elemRefAnnotationPaths);

		// expect all fields of the views to be listed
		assertEquals(1 + 36, result.split(System.lineSeparator()).length);
		
		// expect sources of currencyCode and unitOfMeasure to be found
		assertEquals(3, StringUtil.instrCount(result, "I_AnyBaseView.AnyAmount"));
		assertEquals(3, StringUtil.instrCount(result, "I_OtherBaseView.AnyQuantity"));
		assertEquals(4, StringUtil.instrCount(result, "I_SubBaseView.AssociatedAmount"));
	}
	
	@Test
	public void testViewHierarchyWithoutAnnotations() {
		addSubBaseView();
		addAnyBaseView();
		addOtherBaseView();
		addThirdBaseView();
		addConsumingView();
		
		String result = getResult(new String[] {});

		// expect all fields of the views to be listed
		assertEquals(1 + 36, result.split(System.lineSeparator()).length);
		
		// expect no sources of currencyCode and unitOfMeasure to be reported
		assertEquals(1, StringUtil.instrCount(result, "I_AnyBaseView.AnyAmount"));
		assertEquals(1, StringUtil.instrCount(result, "I_OtherBaseView.AnyQuantity"));
		assertEquals(1, StringUtil.instrCount(result, "I_SubBaseView.AssociatedAmount"));
	}
	
	@Test
	public void testAggregatingView() {
		addAnyBaseView();
		addAggregatingView();
		
		String result = getResult(elemRefAnnotationPaths);

		assertEquals(1 + 8, result.split(System.lineSeparator()).length);
		
		// expect no sources of currencyCode (or unitOfMeasure) to be found
		assertEquals(1, StringUtil.instrCount(result, "I_AnyBaseView.AnyAmount"));
	}
}
