package com.sap.adt.abapcleaner.rules.ddl.annotations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class DdlAnnotationLayoutTest extends RuleTestBase {
	private DdlAnnotationLayoutRule rule;
	
	DdlAnnotationLayoutTest() {
		super(RuleID.DDL_ANNO_LAYOUT);
		rule = (DdlAnnotationLayoutRule)getRule();
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configSpaceAfterColon.setValue(true);
	   rule.configSpaceInsideBraces.setValue(true);
	   rule.configSpaceInsideBrackets.setValue(true);

	   rule.configMaxLineLength.setValue(120);
	   rule.configMaxOneLinerElemCountMain.setValue(4);
	   rule.configMaxOneLinerElemCountList.setValue(4);

	   rule.configAlignValues.setValue(false);
	   rule.configAlignTablesInArrays.setValue(true);
	}

	@Test
	void testAllSpaces() {
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel. lifecycle. draft .expiryInterval:'PT28D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");
		buildSrc("");
		buildSrc("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                    #ANALYTICAL_KPI,");
		buildSrc("                                        #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.action:[{instance:{bound:true}},{enabled:true},{feature:''}]");

		buildExp("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildExp("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildExp("@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");
		buildExp("@ObjectModel.action: [ { instance: { bound: true } },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");

		testRule();
	}

	@Test
	void testNoSpaceAfterColon() {
		rule.configSpaceAfterColon.setValue(false);

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel. lifecycle. draft .expiryInterval:'PT28D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");

		buildExp("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval:'PT10D'");
		buildExp("@ObjectModel.lifecycle.draft.expiryInterval:'PT28D'");
		buildExp("@ObjectModel.usageType:{ serviceQuality:#D, sizeCategory:#XXL, dataClass:#MIXED }");

		testRule();
	}

	@Test
	void testNoSpaceinsideBraces() {
		rule.configSpaceInsideBraces.setValue(false);
		rule.configMaxOneLinerElemCountMain.setValue(2);

		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");
		buildSrc("");
		buildSrc("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                    #ANALYTICAL_KPI,");
		buildSrc("                                        #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.action:[{instance:{bound:true}},{enabled:true},{feature:''}]");

		buildExp("@ObjectModel.usageType: {serviceQuality: #D,");
		buildExp("                         sizeCategory: #XXL,");
		buildExp("                         dataClass: #MIXED}");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");
		buildExp("@ObjectModel.action: [ {instance: {bound: true}},");
		buildExp("                       {enabled: true},");
		buildExp("                       {feature: ''} ]");

		testRule();
	}

	@Test
	void testNoSpacesInsideBrackets() {
		rule.configSpaceInsideBrackets.setValue(false);

		buildSrc("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                    #ANALYTICAL_KPI,");
		buildSrc("                                        #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.action:[{instance:{bound:true}},{enabled:true},{feature:''}]");

		buildExp("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildExp("                                     #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                     #ANALYTICAL_KPI,");
		buildExp("                                     #ANALYTICAL_PROVIDER]");
		buildExp("@ObjectModel.action: [{ instance: { bound: true } },");
		buildExp("                      { enabled: true },");
		buildExp("                      { feature: '' }]");

		testRule();
	}

	@Test
	void testHighOneLinerLength() {
		rule.configMaxLineLength.setValue(130);

		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");
		buildSrc("");
		buildSrc("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                    #ANALYTICAL_KPI,");
		buildSrc("                                        #ANALYTICAL_PROVIDER ]");

		buildExp("@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY, #ANALYTICAL_DOCUMENT_STORE, #ANALYTICAL_KPI, #ANALYTICAL_PROVIDER ]");

		testRule();
	}

	@Test
	void testLowOneLinerLength() {
		rule.configMaxLineLength.setValue(80);

		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");
		buildSrc("");
		buildSrc("@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                    #ANALYTICAL_KPI,");
		buildSrc("                                        #ANALYTICAL_PROVIDER ]");

		buildExp("@ObjectModel.usageType: { serviceQuality: #D,");
		buildExp("                          sizeCategory: #XXL,");
		buildExp("                          dataClass: #MIXED }");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");

		testRule();
	}

	@Test
	void testMaxOneElementPerLineInMain() {
		rule.configMaxOneLinerElemCountMain.setValue(1);

		buildSrc("@UI.selectionVariant: [ {");
		buildSrc("  qualifier: 'params',");
		buildSrc("  parameters: [ {");
		buildSrc("    name: 'P_AnyParam',");
		buildSrc("    value: 'any'");
		buildSrc("  }, {");
		buildSrc("    name: 'P_OtherParam',");
		buildSrc("    value: 'other'");
		buildSrc("  }, {");
		buildSrc("    name:  'P_Third',");
		buildSrc("    value: 'third'");
		buildSrc("} ] } ]");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @Consumption.filter: { selectionType: #SINGLE,");
		buildSrc("                             multipleSelections  :false,");
		buildSrc("                            mandatory:false}");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition:");
		buildSrc("      [");
		buildSrc("        {");
		buildSrc("          entity:");
		buildSrc("          {");
		buildSrc("            name: 'I_AnyName',");
		buildSrc("            element: 'ElementName'");
		buildSrc("          }");
		buildSrc("        }");
		buildSrc("      ]");
		buildSrc("      AnyNonKeyField");
		buildSrc("}");

		buildExp("@UI.selectionVariant: [ { qualifier: 'params',");
		buildExp("                          parameters: [ { name: 'P_AnyParam',");
		buildExp("                                          value: 'any' },");
		buildExp("                                        { name: 'P_OtherParam',");
		buildExp("                                          value: 'other' },");
		buildExp("                                        { name: 'P_Third',");
		buildExp("                                          value: 'third' } ] } ]");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_OtherEntity");
		buildExp("");
		buildExp("{");
		buildExp("      @Consumption.filter: { selectionType: #SINGLE, multipleSelections: false, mandatory: false }");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("      AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testMaxOneElementPerLineInSelectList() {
		rule.configMaxOneLinerElemCountList.setValue(1);

		buildSrc("@UI.selectionVariant: [ {");
		buildSrc("  qualifier: 'params',");
		buildSrc("  parameters: [ {");
		buildSrc("    name: 'P_AnyParam',");
		buildSrc("    value: 'any'");
		buildSrc("  }, {");
		buildSrc("    name: 'P_OtherParam',");
		buildSrc("    value: 'other'");
		buildSrc("  }, {");
		buildSrc("    name:  'P_Third',");
		buildSrc("    value: 'third'");
		buildSrc("} ] } ]");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("");
		buildSrc("{");
		buildSrc("      @Consumption.filter: { selectionType: #SINGLE,");
		buildSrc("                             multipleSelections  :false,");
		buildSrc("                            mandatory:false}");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition:");
		buildSrc("      [");
		buildSrc("        {");
		buildSrc("          entity:");
		buildSrc("          {");
		buildSrc("            name: 'I_AnyName',");
		buildSrc("            element: 'ElementName'");
		buildSrc("          }");
		buildSrc("        }");
		buildSrc("      ]");
		buildSrc("      AnyNonKeyField");
		buildSrc("}");

		buildExp("@UI.selectionVariant: [ { qualifier: 'params',");
		buildExp("                          parameters: [ { name: 'P_AnyParam',   value: 'any'   },");
		buildExp("                                        { name: 'P_OtherParam', value: 'other' },");
		buildExp("                                        { name: 'P_Third',      value: 'third' } ] } ]");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_OtherEntity");
		buildExp("");
		buildExp("{");
		buildExp("      @Consumption.filter: { selectionType: #SINGLE,");
		buildExp("                             multipleSelections: false,");
		buildExp("                             mandatory: false }");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName',");
		buildExp("                                                      element: 'ElementName' } } ]");
		buildExp("      AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testAlignValues() {
		rule.configMaxOneLinerElemCountMain.setValue(1);
		rule.configAlignValues.setValue(true);

		buildSrc("@ObjectModel.usageType: { serviceQuality: #D,");
		buildSrc("                            sizeCategory:#XXL,");
		buildSrc("                                 dataClass :#MIXED }");
		buildSrc("");
		buildSrc("@UI.selectionVariant: [ {");
		buildSrc("  qualifier: 'params',");
		buildSrc("  parameters: [ {");
		buildSrc("    name: 'P_AnyParam',");
		buildSrc("    value: 'any'");
		buildSrc("  }, {");
		buildSrc("    name: 'P_OtherParam',");
		buildSrc("    value: 'other'");
		buildSrc("  }, {");
		buildSrc("    name:  'P_Third',");
		buildSrc("    value: 'third'");
		buildSrc("} ] } ]");

		buildExp("@ObjectModel.usageType: { serviceQuality: #D,");
		buildExp("                          sizeCategory:   #XXL,");
		buildExp("                          dataClass:      #MIXED }");
		buildExp("");
		buildExp("@UI.selectionVariant: [ { qualifier:  'params',");
		buildExp("                          parameters: [ { name:  'P_AnyParam',");
		buildExp("                                          value: 'any' },");
		buildExp("                                        { name:  'P_OtherParam',");
		buildExp("                                          value: 'other' },");
		buildExp("                                        { name:  'P_Third',");
		buildExp("                                          value: 'third' } ] } ]");

		testRule();
	}

	@Test
	void testDoNotAlignTables() {
		rule.configAlignTablesInArrays.setValue(false);

		buildSrc("@UI.selectionVariant: [ {");
		buildSrc("  qualifier: 'params',");
		buildSrc("  parameters: [ {");
		buildSrc("    name: 'P_AnyParam',");
		buildSrc("    value: 'any'");
		buildSrc("  }, {");
		buildSrc("    name: 'P_OtherParam',");
		buildSrc("    value: 'other'");
		buildSrc("  }, {");
		buildSrc("    name:  'P_Third',");
		buildSrc("    value: 'third'");
		buildSrc("} ] } ]");

		buildExp("@UI.selectionVariant: [ { qualifier: 'params',");
		buildExp("                          parameters: [ { name: 'P_AnyParam', value: 'any' },");
		buildExp("                                        { name: 'P_OtherParam', value: 'other' },");
		buildExp("                                        { name: 'P_Third', value: 'third' } ] } ]");

		testRule();
	}

	@Test
	void testNestingKeptButNotFilledUp() {
		// ensure that the existing nesting is kept unchanged (i.e. neither removed nor filled up)
		buildSrc("@ObjectModel . usageType  .  sizeCategory  :  #XXL");
		buildSrc("@ObjectModel.  usageType: {serviceQuality: #D   }");
		buildSrc("@ObjectModel .usageType  .dataClass  :#MIXED");

		buildExp("@ObjectModel.usageType.sizeCategory: #XXL");
		buildExp("@ObjectModel.usageType: { serviceQuality: #D }");
		buildExp("@ObjectModel.usageType.dataClass: #MIXED");

		testRule();
	}

	@Test
	void testSingleAnnotation() {
		// ensure that (multiple) nesting is kept even for an only annotation 
		rule.configSpaceAfterColon.setValue(false);
		rule.configSpaceInsideBraces.setValue(false);
		rule.configSpaceInsideBrackets.setValue(false);

		buildSrc("  @ObjectModel: { usageType: { sizeCategory: #XXL } }");
		buildSrc("  define view entity C_AnyEntity as select from I_OtherEntity");
		buildSrc("  {");
		buildSrc("    key AnyKeyField,");
		buildSrc("  }");

		buildExp("  @ObjectModel:{usageType:{sizeCategory:#XXL}}");
		buildExp("  define view entity C_AnyEntity as select from I_OtherEntity");
		buildExp("  {");
		buildExp("    key AnyKeyField,");
		buildExp("  }");

		testRule();
	}

	@Test
	void testNestedArrays() {
		// ensure that arrays in arrays are correctly 'parsed' and written
		buildSrc("@Anno:[{subAnno:[{subSubAnno:'value'}]}]");
		buildSrc("@ArrayAnno1:[[['value']]]");
		buildSrc("@ArrayAnno2:[[['valueA1a','valueA1b','valueA1c'],['valueA2a','valueA2b'],['valueA3']],[['valueB1']]]");

		buildExp("@Anno: [ { subAnno: [ { subSubAnno: 'value' } ] } ]");
		buildExp("@ArrayAnno1: [ [ [ 'value' ] ] ]");
		buildExp("@ArrayAnno2: [ [ [ 'valueA1a', 'valueA1b', 'valueA1c' ],");
		buildExp("                 [ 'valueA2a', 'valueA2b' ],");
		buildExp("                 [ 'valueA3' ] ],");
		buildExp("               [ [ 'valueB1' ] ] ]");

		testRule();
	}

	@Test
	void testUncheckedExpressions() {
		// ensure that unchecked expressions #(...) are kept unchanged
		buildSrc("@ObjectModel . usageType  .  sizeCategory  :  #('XXL')");
		buildSrc("@ObjectModel.  usageType: {serviceQuality: #('D') }");
		buildSrc("@ObjectModel .usageType  .dataClass  :#('MIXED')");

		buildExp("@ObjectModel.usageType.sizeCategory: #('XXL')");
		buildExp("@ObjectModel.usageType: { serviceQuality: #('D') }");
		buildExp("@ObjectModel.usageType.dataClass: #('MIXED')");

		testRule();
	}

	@Test
	void testDefineRole() {
		// ensure that Data Control Language annotations are processed, too 
		
		buildSrc("@EndUserText . label : 'Any text'");
		buildSrc("@MappingRole  : true");
		buildSrc("");
		buildSrc("define role I_AnyView {");
		buildSrc("  grant select on I_AnyView");
		buildSrc("    where (Field1, Field2) = aspect pfcg_auth(auth_object, mapped_field1, mapped_field2, auth_field1 = 'value');");
		buildSrc("}");

		buildExp("@EndUserText.label: 'Any text'");
		buildExp("@MappingRole: true");
		buildExp("");
		buildExp("define role I_AnyView {");
		buildExp("  grant select on I_AnyView");
		buildExp("    where (Field1, Field2) = aspect pfcg_auth(auth_object, mapped_field1, mapped_field2, auth_field1 = 'value');");
		buildExp("}");

		testRule();
	}
}
