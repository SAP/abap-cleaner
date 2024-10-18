package com.sap.adt.abapcleaner.rules.ddl.annotations;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationEmptyLines;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationNestingDepth;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationSortOrder;

public class DdlAnnotationNestingTest extends RuleTestBase {
	private DdlAnnotationNestingRule rule;
   private DdlAnnotationLayoutRule layoutRule;
	
	DdlAnnotationNestingTest() {
		super(RuleID.DDL_ANNO_NESTING);
		rule = (DdlAnnotationNestingRule)getRule();
		layoutRule = (DdlAnnotationLayoutRule)profile.getRule(RuleID.DDL_ANNO_LAYOUT);
	}

	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
	   rule.configNestingMinDepth.setEnumValue(DdlAnnotationNestingDepth.LEVEL_3);
	   rule.configNestingAllowList.setValue("");
	   rule.configNestingBlockList.setValue("");

	   rule.configSortOrder.setEnumValue(DdlAnnotationSortOrder.BY_TWO_ELEMS);

	   rule.configEmptyLinesMain.setEnumValue(DdlAnnotationEmptyLines.FOR_MULTI_LINE_OR_NEW_FIRST_ELEM);
	   rule.configEmptyLinesList.setEnumValue(DdlAnnotationEmptyLines.NEVER);

	   // the rule also uses the settings of DdlAnnotationLayoutRule, which are therefore prepared, too
	   layoutRule.configSpaceAfterColon.setValue(true);
	   layoutRule.configSpaceInsideBraces.setValue(true);
	   layoutRule.configSpaceInsideBrackets.setValue(true);

	   layoutRule.configMaxLineLength.setValue(120);
	   layoutRule.configMaxOneLinerElemCountMain.setValue(4);
	   layoutRule.configMaxOneLinerElemCountList.setValue(4);

	   layoutRule.configAlignValues.setValue(false);
	   layoutRule.configAlignTablesInArrays.setValue(true);
	}
	
	@Test
	void testNestingFromLevel3() {
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                      #ANALYTICAL_KPI,");
		buildSrc("                                      #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");
		buildExp("");
		buildExp("@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testNestingFromLevel2() {
		rule.configNestingMinDepth.setEnumValue(DdlAnnotationNestingDepth.LEVEL_2);

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                      #ANALYTICAL_KPI,");
		buildSrc("                                      #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		buildExp("@ObjectModel: { action: [ { instance.bound: true },");
		buildExp("                          { enabled: true },");
		buildExp("                          { feature: '' } ],");
		buildExp("                delegatedAction: [ { enabled: true } ],");
		buildExp("                lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' },");
		buildExp("                modelingPattern: #ANALYTICAL_QUERY,");
		buildExp("                supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                         #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                         #ANALYTICAL_KPI,");
		buildExp("                                         #ANALYTICAL_PROVIDER ],");
		buildExp("                usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED } }");

		testRule();
	}

	@Test
	void testNestingFromLevel2WithBlockList() {
		rule.configNestingMinDepth.setEnumValue(DdlAnnotationNestingDepth.LEVEL_2);
		rule.configNestingBlockList.setValue("ObjectModel.usageType");

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                      #ANALYTICAL_KPI,");
		buildSrc("                                      #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		buildExp("@ObjectModel: { action: [ { instance.bound: true },");
		buildExp("                          { enabled: true },");
		buildExp("                          { feature: '' } ],");
		buildExp("                delegatedAction: [ { enabled: true } ],");
		buildExp("                lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' },");
		buildExp("                modelingPattern: #ANALYTICAL_QUERY,");
		buildExp("                supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                         #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                         #ANALYTICAL_KPI,");
		buildExp("                                         #ANALYTICAL_PROVIDER ],");
		buildExp("                usageType.serviceQuality: #D,");
		buildExp("                usageType.sizeCategory: #XXL,");
		buildExp("                usageType.dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testNestingFromLevel5WithAllowList() {
		rule.configNestingMinDepth.setEnumValue(DdlAnnotationNestingDepth.LEVEL_5);
		rule.configNestingAllowList.setValue("ObjectModel.usageType");

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                      #ANALYTICAL_KPI,");
		buildSrc("                                      #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildExp("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");
		buildExp("");
		buildExp("@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testSortByFirstLevelOnly() {
		rule.configSortOrder.setEnumValue(DdlAnnotationSortOrder.BY_FIRST_ELEM);

		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildSrc("@Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',");
		buildSrc("                                    entity.element: 'ElementName' } ]");
		buildSrc("@Consumption.semanticObject: 'AnySemanticObject'");

		buildExp("@Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("@Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("");
		buildExp("@ObjectModel.usageType: { sizeCategory: #XXL, serviceQuality: #D, dataClass: #MIXED }");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		testRule();
	}

	@Test
	void testSortAllAlphabetically() {
		rule.configSortOrder.setEnumValue(DdlAnnotationSortOrder.BY_ALL_ELEMS);

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");

		buildExp("@ObjectModel.lifecycle.draft: { expiryInterval: 'PT28D', notificationBeforeExpiryInterval: 'PT10D' }");
		buildExp("@ObjectModel.usageType: { dataClass: #MIXED, serviceQuality: #D, sizeCategory: #XXL }");

		testRule();
	}

	@Test
	void testKeepSortOrder() {
		rule.configSortOrder.setEnumValue(DdlAnnotationSortOrder.KEEP);

		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildSrc("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildSrc("                                      #ANALYTICAL_KPI,");
		buildSrc("                                      #ANALYTICAL_PROVIDER ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("@ObjectModel.usageType: { serviceQuality: #D, sizeCategory: #XXL, dataClass: #MIXED }");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,");
		buildExp("                                      #ANALYTICAL_DOCUMENT_STORE,");
		buildExp("                                      #ANALYTICAL_KPI,");
		buildExp("                                      #ANALYTICAL_PROVIDER ]");
		buildExp("");
		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");

		testRule();
	}

	@Test
	void testAlwaysPutEmptyLines() {
		rule.configEmptyLinesMain.setEnumValue(DdlAnnotationEmptyLines.ALWAYS);

		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildSrc("@Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',");
		buildSrc("                                    entity.element: 'ElementName' } ]");
		buildSrc("@Consumption.semanticObject: 'AnySemanticObject'");

		buildExp("@Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("");
		buildExp("@Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("");
		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("@ObjectModel.usageType: { sizeCategory: #XXL, serviceQuality: #D, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testPutEmptyLinesForNewFirstElemOnly() {
		rule.configEmptyLinesMain.setEnumValue(DdlAnnotationEmptyLines.FOR_NEW_FIRST_ELEM);

		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildSrc("@Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',");
		buildSrc("                                    entity.element: 'ElementName' } ]");
		buildSrc("@Consumption.semanticObject: 'AnySemanticObject'");

		buildExp("@Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("@Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("");
		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("@ObjectModel.usageType: { sizeCategory: #XXL, serviceQuality: #D, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testNeverPutEmptyLines() {
		rule.configEmptyLinesMain.setEnumValue(DdlAnnotationEmptyLines.NEVER);

		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',");
		buildSrc("                                    entity.element: 'ElementName' } ]");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@Consumption.semanticObject: 'AnySemanticObject'");

		buildExp("@Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("@Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("@ObjectModel.usageType: { sizeCategory: #XXL, serviceQuality: #D, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testKeepEmptyLines() {
		rule.configEmptyLinesMain.setEnumValue(DdlAnnotationEmptyLines.KEEP_AS_IS);

		buildSrc("@ObjectModel.usageType.sizeCategory: #XXL");
		buildSrc("");
		buildSrc("@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'");
		buildSrc("@ObjectModel.usageType: { serviceQuality: #D }");
		buildSrc("");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.usageType.dataClass: #MIXED");
		buildSrc("@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'");
		buildSrc("@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildSrc("@Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',");
		buildSrc("                                    entity.element: 'ElementName' } ]");
		buildSrc("");
		buildSrc("@Consumption.semanticObject: 'AnySemanticObject'");

		buildExp("@Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("@Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName', element: 'ElementName' } } ]");
		buildExp("@ObjectModel.action: [ { instance.bound: true },");
		buildExp("                       { enabled: true },");
		buildExp("                       { feature: '' } ]");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("");
		buildExp("@ObjectModel.lifecycle.draft: { notificationBeforeExpiryInterval: 'PT10D', expiryInterval: 'PT28D' }");
		buildExp("");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("@ObjectModel.usageType: { sizeCategory: #XXL, serviceQuality: #D, dataClass: #MIXED }");

		testRule();
	}

	@Test
	void testAlwaysPutEmptyLinesInSelectList() {
		rule.configEmptyLinesList.setEnumValue(DdlAnnotationEmptyLines.ALWAYS);
	   layoutRule.configMaxOneLinerElemCountList.setValue(1);

		buildSrc("define view entity C_AnyEntity as select from I_OtherEntity");
		buildSrc("{");
		buildSrc("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildSrc("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildSrc("      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName', entity.element: 'ElementName' } ]");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity as select from I_OtherEntity");
		buildExp("{");
		buildExp("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName',");
		buildExp("                                                      element: 'ElementName' } } ]");
		buildExp("");
		buildExp("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testPutEmptyLinesForNewFirstElemInSelectList() {
		rule.configEmptyLinesList.setEnumValue(DdlAnnotationEmptyLines.FOR_NEW_FIRST_ELEM);
	   layoutRule.configMaxOneLinerElemCountList.setValue(1);

		buildSrc("define view entity C_AnyEntity as select from I_OtherEntity");
		buildSrc("{");
		buildSrc("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildSrc("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildSrc("      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName', entity.element: 'ElementName' } ]");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity as select from I_OtherEntity");
		buildExp("{");
		buildExp("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName',");
		buildExp("                                                      element: 'ElementName' } } ]");
		buildExp("");
		buildExp("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testNeverPutEmptyLinesInSelectList() {
	   layoutRule.configMaxOneLinerElemCountList.setValue(1);

	   buildSrc("define view entity C_AnyEntity as select from I_OtherEntity");
		buildSrc("{");
		buildSrc("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildSrc("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildSrc("      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName', entity.element: 'ElementName' } ]");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity as select from I_OtherEntity");
		buildExp("{");
		buildExp("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName',");
		buildExp("                                                      element: 'ElementName' } } ]");
		buildExp("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testKeepEmptyLinesInSelectList() {
		rule.configEmptyLinesList.setEnumValue(DdlAnnotationEmptyLines.KEEP_AS_IS);
	   layoutRule.configMaxOneLinerElemCountList.setValue(1);

		buildSrc("define view entity C_AnyEntity as select from I_OtherEntity");
		buildSrc("{");
		buildSrc("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildSrc("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildSrc("");
		buildSrc("      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName', entity.element: 'ElementName' } ]");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity as select from I_OtherEntity");
		buildExp("{");
		buildExp("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("");
		buildExp("      @Consumption.valueHelpDefinition: [ { entity: { name: 'I_AnyName',");
		buildExp("                                                      element: 'ElementName' } } ]");
		buildExp("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testSingleAnnotation() {
		buildSrc("  @ObjectModel: { usageType: { sizeCategory: #XXL } }");

		buildExp("  @ObjectModel.usageType.sizeCategory: #XXL");

		testRule();
	}

	@Test
	void testNestedArrayKept() {
		buildSrc("@Anno: [ [ [ 'value' ] ] ]");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSimplifyEarlyNesting() {
		buildSrc("@AnyAnno: { subAnno.any: #A }");
		buildSrc("@AnyAnno: { subAnno.other: #B }");
		buildSrc("@AnyAnno: { subAnno.third: #C }");

		buildExp("@AnyAnno.subAnno: { any: #A, other: #B, third: #C }");

		testRule();
	}

	@Test
	void testFactorOutCommonSubAnnotation() {
		buildSrc("@AnyAnno: { subAnno.any: 'A', subAnno.other: 'B', subAnno.third: 'C' }");
		buildSrc("@OtherAnno: [ { subAnno.any: 'A', subAnno.other: 'B', subAnno.third: 'C' } ]");

		buildExp("@AnyAnno.subAnno: { any: 'A', other: 'B', third: 'C' }");
		buildExp("");
		buildExp("@OtherAnno: [ { subAnno: { any: 'A', other: 'B', third: 'C' } } ]");

		testRule();
	}

	@Test
	void testMultipleNestedArraysUnchanged() {
		// expect the nested arrays to be structurally unchanged (apart from layout)
		buildSrc("@Anno:[{subAnno:[{subSubAnno:'value'}]}]");
		buildSrc("@ArrayAnno1:[[['value']]]");
		buildSrc("@ArrayAnno2:[[['value1a','value1b','value1c'],['value2a','value2b'],['value3']],[['value4']]]");

		buildExp("@Anno: [ { subAnno: [ { subSubAnno: 'value' } ] } ]");
		buildExp("");
		buildExp("@ArrayAnno1: [ [ [ 'value' ] ] ]");
		buildExp("");
		buildExp("@ArrayAnno2: [ [ [ 'value1a', 'value1b', 'value1c' ],");
		buildExp("                 [ 'value2a', 'value2b' ],");
		buildExp("                 [ 'value3' ] ],");
		buildExp("               [ [ 'value4' ] ] ]");

		testRule();
	}

	@Test
	void testNestingWithUncheckedExpressions() {
		buildSrc("@ObjectModel.usageType.sizeCategory:  #('XXL')");
		buildSrc("@ObjectModel.usageType: {serviceQuality: #('D') }");
		buildSrc("@ObjectModel.usageType.dataClass: #('MIXED');");

		buildExp("@ObjectModel.usageType: { sizeCategory: #('XXL'), serviceQuality: #('D'), dataClass.#('MIXED'): ; }");

		testRule();
	}

	@Test
	void testCommentAboveScope() {
		// ensure that rearranged annotations are not moved before the comment above the annotation scope
		
		buildSrc("// entity comment");
		buildSrc("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildSrc("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildSrc("");
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // element comment 1");
		buildSrc("      @Search.defaultSearchElement:true");
		buildSrc("      @Consumption.filter.mandatory: false");
		buildSrc("  key AnyKeyField,");
		buildSrc("");
		buildSrc("      // element comment 2");
		buildSrc("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildSrc("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildSrc("      AnyNonKeyField");
		buildSrc("}");

		buildExp("// entity comment");
		buildExp("@ObjectModel.delegatedAction: [ { enabled: true } ]");
		buildExp("@ObjectModel.modelingPattern: #ANALYTICAL_QUERY");
		buildExp("");
		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_OtherEntity");
		buildExp("");
		buildExp("{");
		buildExp("      // element comment 1");
		buildExp("      @Consumption.filter.mandatory: false");
		buildExp("      @Search.defaultSearchElement: true");
		buildExp("  key AnyKeyField,");
		buildExp("");
		buildExp("      // element comment 2");
		buildExp("      @Consumption.semanticObject: 'AnySemanticObject'");
		buildExp("      @ObjectModel.text.element: [ 'SemanticObjectName' ]");
		buildExp("      AnyNonKeyField");
		buildExp("}");

		testRule();
	}

	@Test
	void testInsertBelowComment() {
		// ensure that the annotation is not inserted before the comment
		
		buildSrc("define view entity C_AnyEntity");
		buildSrc("  as select from I_OtherEntity");
		buildSrc("");
		buildSrc("{");
		buildSrc("      // comment");
		buildSrc("      @Consumption.filter.multipleSelections: false");
		buildSrc("      @Consumption.filter.mandatory: false");
		buildSrc("  key AnyKeyField");
		buildSrc("}");

		buildExp("define view entity C_AnyEntity");
		buildExp("  as select from I_OtherEntity");
		buildExp("");
		buildExp("{");
		buildExp("      // comment");
		buildExp("      @Consumption.filter: { multipleSelections: false, mandatory: false }");
		buildExp("  key AnyKeyField");
		buildExp("}");

		testRule();
	}
}
