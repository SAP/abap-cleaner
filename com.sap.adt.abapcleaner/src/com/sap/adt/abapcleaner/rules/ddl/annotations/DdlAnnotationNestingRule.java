package com.sap.adt.abapcleaner.rules.ddl.annotations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigTextType;
import com.sap.adt.abapcleaner.rulebase.ConfigTextValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationCommandWriter;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationEmptyLines;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationLayout;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationNestingDepth;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationScope;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationSortOrder;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlAnnotationScopes;

public class DdlAnnotationNestingRule extends RuleForDdlAnnotationScopes {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_ANNO_NESTING; }

	@Override
	public String getDisplayName() { return "Rearrange annotations"; }

	@Override
	public String getDescription() { return "Rearranges the nesting and order of annotations and groups them with empty lines."; }

	@Override
	public String getHintsAndRestrictions() { return "Note that this rule reuses the configuration of the rule '" + DdlAnnotationLayoutRule.displayName + "', even if it is deactivated."; }
	
	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 7, 20); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'"
				+ LINE_SEP + "@ObjectModel.usageType: { serviceQuality: #D }"
				+ LINE_SEP + "@ObjectModel.usageType.sizeCategory: #XXL"
				+ LINE_SEP + "@ObjectModel.modelingPattern: #ANALYTICAL_QUERY"
				+ LINE_SEP + "@ObjectModel.supportedCapabilities: [ #ANALYTICAL_QUERY,"
				+ LINE_SEP + "                                      #ANALYTICAL_DOCUMENT_STORE,"
				+ LINE_SEP + "                                      #ANALYTICAL_KPI,"
				+ LINE_SEP + "                                      #ANALYTICAL_PROVIDER ]"
				+ LINE_SEP + "@ObjectModel.usageType.dataClass: #MIXED"
				+ LINE_SEP + "@ObjectModel.lifecycle.draft.expiryInterval: 'PT28D'"
				+ LINE_SEP + "@ObjectModel.action: [ { instance: { bound: true } }, { enabled: true }, { feature: '' } ]"		
				+ LINE_SEP + "@ObjectModel.delegatedAction: [ { enabled: true } ]"		
				+ LINE_SEP + ""
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  as select from I_OtherEntity"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "      @Consumption.filter: { selectionType: #SINGLE }"
				+ LINE_SEP + "      @Consumption.filter.multipleSelections: false"
				+ LINE_SEP + "      @Consumption.filter.mandatory: false"
				+ LINE_SEP + "      @Search.defaultSearchElement:true"
				+ LINE_SEP + "      @Search.fuzzinessThreshold: 0.7"
				+ LINE_SEP + "  key AnyKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      @Consumption.semanticObject: 'AnySemanticObject'"
				+ LINE_SEP + "      @ObjectModel.text.element: [ 'SemanticObjectName' ]"
				+ LINE_SEP + "      @Consumption.valueHelpDefinition: [ { entity.name: 'I_AnyName',"
				+ LINE_SEP + "                                            entity.element: 'ElementName' } ]"
				+ LINE_SEP + "      AnyNonKeyField"
				+ LINE_SEP + "}";
	}

	private final String[] nestingDepthTexts = new String[] { "Starting from level 2", "Starting from level 3", "Starting from level 4", 
																				 "Starting from level 5", "Starting from level 6", "Starting from level 7", 
																				 "Never", "Keep as is, but fill up given nesting", "Keep as is" };
	
   private final ConfigInfoValue configInfoNesting = new ConfigInfoValue(this, "Nesting", ConfigInfoStyle.HEADING);
   final ConfigEnumValue<DdlAnnotationNestingDepth> configNestingMinDepth = new ConfigEnumValue<DdlAnnotationNestingDepth>(this, "NestingMinDepth", "Use nesting for structured annotations:", nestingDepthTexts, DdlAnnotationNestingDepth.values(), DdlAnnotationNestingDepth.LEVEL_3);
   private final ConfigInfoValue configInfoAnnotationLists = new ConfigInfoValue(this, "To refine the above setting, you can enter comma-separated annotation names to always/never be nested (e.g. \"Consumption.valueHelpDefinition, ObjectModel.usageType, Search\"):", ConfigInfoStyle.NORMAL);
   final ConfigTextValue configNestingAllowList = new ConfigTextValue(this, "NestingAllowList", "Always allow nesting for annotations starting with:", "", ConfigTextType.DDL_ANNOTATION_LIST, 1024, "");
	final ConfigTextValue configNestingBlockList = new ConfigTextValue(this, "NestingBlockList", "Always block nesting for annotations starting with:", "", ConfigTextType.DDL_ANNOTATION_LIST, 1024, "");

   private final ConfigInfoValue configInfoOrder = new ConfigInfoValue(this, "Order", ConfigInfoStyle.HEADING);
   final ConfigEnumValue<DdlAnnotationSortOrder> configSortOrder = new ConfigEnumValue<DdlAnnotationSortOrder>(this, "SortOrder", "Order of annotations:", new String[] { "Sort by first level, but keep sub-levels as they are", "Sort by first and second level, keep others as they are", "Sort alphabetically by all levels", "Keep as is (as far as nestings allows)" }, DdlAnnotationSortOrder.values(), DdlAnnotationSortOrder.BY_TWO_ELEMS);

   private final ConfigInfoValue configInfoEmptyLines = new ConfigInfoValue(this, "Empty lines", ConfigInfoStyle.HEADING);
   final ConfigEnumValue<DdlAnnotationEmptyLines> configEmptyLinesMain = new ConfigEnumValue<DdlAnnotationEmptyLines>(this, "EmptyLinesMain", "Put empty lines between entity annotations:", new String[] { "Always", "For multi-liners or if first element differs", "Only if first element differs", "Never", "Keep as is (as far as nestings allows)" }, DdlAnnotationEmptyLines.values(), DdlAnnotationEmptyLines.FOR_MULTI_LINE_OR_NEW_FIRST_ELEM);
   final ConfigEnumValue<DdlAnnotationEmptyLines> configEmptyLinesList = new ConfigEnumValue<DdlAnnotationEmptyLines>(this, "EmptyLinesList", "Put empty lines between select list annotations:", new String[] { "Always", "For multi-liners or if first element differs", "Only if first element differs", "Never", "Keep as is (as far as nestings allows)" }, DdlAnnotationEmptyLines.values(), DdlAnnotationEmptyLines.NEVER);

	private final ConfigValue[] configValues = new ConfigValue[] { 
			configInfoNesting, configNestingMinDepth, configInfoAnnotationLists, configNestingAllowList, configNestingBlockList,   
			configInfoOrder, configSortOrder,
			configInfoEmptyLines, configEmptyLinesMain, configEmptyLinesList };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAnnotationNestingRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected void executeOn(Code code, DdlAnnotationScope scope) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		DdlAnnotationNestingDepth nestingDepth = DdlAnnotationNestingDepth.forValue(configNestingMinDepth.getValue()); 

		// determine the layout, which also depends on whether the annotations belong to the whole entity or to an element in the parameter or select list
		Command firstCommand = scope.getFirstCommand();
		Token firstToken = firstCommand.getFirstToken();
		boolean isEntityAnnoScope = (firstCommand.getParent() == null);
		DdlAnnotationEmptyLines emptyLines = DdlAnnotationEmptyLines.forValue(isEntityAnnoScope ? configEmptyLinesMain.getValue() : configEmptyLinesList.getValue());

		DdlAnnotationLayout layout = ((DdlAnnotationLayoutRule)parentProfile.getRule(RuleID.DDL_ANNO_LAYOUT)).getLayout();
		layout.setContext(firstToken.lineBreaks, firstToken.spacesLeft);
		layout.setEmptyLines(emptyLines);

		scope.finishBuild();
		scope.adjustNesting(nestingDepth, configNestingAllowList.getValue(), configNestingBlockList.getValue(), DdlAnnotationSortOrder.forValue(configSortOrder.getValue()));
		try {
			scope.determineOneLiners(layout);
		} catch (UnexpectedSyntaxException | ParseException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, null, e.getMessage());
		}
		scope.determineTablesInArrays();
		
		DdlAnnotationCommandWriter writer = DdlAnnotationCommandWriter.create(layout, scope.getMaxLevelCount());
		try {
			scope.writeTo(writer);
		} catch (UnexpectedSyntaxException | ParseException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, null, e.getMessage());
		}

		scope.transferResults(writer, code, this);
	}
}
