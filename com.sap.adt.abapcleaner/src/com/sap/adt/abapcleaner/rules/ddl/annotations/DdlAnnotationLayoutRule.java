package com.sap.adt.abapcleaner.rules.ddl.annotations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationCommandWriter;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationEmptyLines;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationLayout;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationScope;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlAnnotationScopes;

public class DdlAnnotationLayoutRule extends RuleForDdlAnnotationScopes {
	public static final String displayName = "Standardize annotation layout";
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_ANNO_LAYOUT; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Standardizes spaces, one-liners and alignment of DDL annotations."; }

	@Override
	public String getHintsAndRestrictions() { return "Starting new lines before a comma is not supported. Spaces before '@' and around '.' will always be removed."; }
	
	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 7, 27); }

	@Override
	public RuleReference[] getReferences() { return references; }

	public RuleID[] getDependentRules() {
		// most settings of this rule are reused by the DdlAnnotationNestingRule via getLayout()
		return new RuleID[] { RuleID.DDL_ANNO_NESTING }; 
	}

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@ObjectModel.lifecycle.draft.notificationBeforeExpiryInterval: 'PT10D'"
				+ LINE_SEP + "@ObjectModel. lifecycle. draft .expiryInterval:'PT28D'"
				+ LINE_SEP + "@ObjectModel.usageType: { serviceQuality: #D,"
				+ LINE_SEP + "                            sizeCategory:#XXL,"
				+ LINE_SEP + "                                 dataClass :#MIXED }"
				+ LINE_SEP + ""
				+ LINE_SEP + "@ObjectModel.supportedCapabilities: [#ANALYTICAL_QUERY,"
				+ LINE_SEP + "                                      #ANALYTICAL_DOCUMENT_STORE,"
				+ LINE_SEP + "                                    #ANALYTICAL_KPI,"
				+ LINE_SEP + "                                        #ANALYTICAL_PROVIDER ]"
				+ LINE_SEP + "@ObjectModel.action:[{instance:{bound:true}},{enabled:true},{feature:''}]"		
				+ LINE_SEP + ""
				+ LINE_SEP + "@UI.selectionVariant: [ {"
				+ LINE_SEP + "  qualifier: 'params',"
				+ LINE_SEP + "  parameters: [ {"
				+ LINE_SEP + "    name: 'P_AnyParam',"
				+ LINE_SEP + "    value: 'any'"
				+ LINE_SEP + "  }, {"
				+ LINE_SEP + "    name: 'P_OtherParam',"
				+ LINE_SEP + "    value: 'other'"
				+ LINE_SEP + "  }, {"
				+ LINE_SEP + "    name:  'P_Third',"
				+ LINE_SEP + "    value: 'third'"
				+ LINE_SEP + "} ] } ]"
				+ LINE_SEP + ""
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  as select from I_OtherEntity"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "      @Consumption.filter: { selectionType: #SINGLE,"
				+ LINE_SEP + "                             multipleSelections  :false,"
				+ LINE_SEP + "                            mandatory:false}"
				+ LINE_SEP + "      @Search.defaultSearchElement: true"
				+ LINE_SEP + "      @  Search.fuzzinessThreshold :0.7"
				+ LINE_SEP + "  key AnyKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      @ObjectModel.text.element:[   'SemanticObjectName']"
				+ LINE_SEP + "      @Consumption.valueHelpDefinition:"
				+ LINE_SEP + "      ["
				+ LINE_SEP + "        {"
				+ LINE_SEP + "          entity:"
				+ LINE_SEP + "          {"
				+ LINE_SEP + "            name: 'I_AnyName',"
				+ LINE_SEP + "            element: 'ElementName'"
				+ LINE_SEP + "          }"
				+ LINE_SEP + "        }"
				+ LINE_SEP + "      ]"
				+ LINE_SEP + "      AnyNonKeyField"
				+ LINE_SEP + "}";
	}

   private final ConfigInfoValue configInfoSpaces = new ConfigInfoValue(this, "Spaces", ConfigInfoStyle.HEADING);
	final ConfigBoolValue configSpaceAfterColon = new ConfigBoolValue(this, "SpaceAfterColon", "Space after colon", true);
	final ConfigBoolValue configSpaceInsideBraces = new ConfigBoolValue(this, "SpaceInsideBraces", "Space inside braces { ... }", true);
	final ConfigBoolValue configSpaceInsideBrackets = new ConfigBoolValue(this, "SpaceInsideBrackets", "Space inside array brackets [ ... ]", true);

   private final ConfigInfoValue configInfoOneLiners = new ConfigInfoValue(this, "One-liners", ConfigInfoStyle.HEADING);
   final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length for one-liners", "", MIN_LINE_LENGTH_DDL, DEFAULT_LINE_LENGTH_DDL, DDL.MAX_LINE_LENGTH);
   final ConfigIntValue configMaxOneLinerElemCountMain = new ConfigIntValue(this, "MaxOneLinerElemCountMain", "Max. number of elements for one-liners in entity annotations:", "", 1, 4, 10);
   final ConfigIntValue configMaxOneLinerElemCountList = new ConfigIntValue(this, "MaxOneLinerElemCountList", "Max. number of elements for one-liners in select lists etc.:", "", 1, 4, 10);

   private final ConfigInfoValue configInfoAlignment = new ConfigInfoValue(this, "Alignment", ConfigInfoStyle.HEADING);
   final ConfigBoolValue configAlignValues = new ConfigBoolValue(this, "AlignValues", "Align values of nested elements (unless an element contains a dot)", false);
   final ConfigBoolValue configAlignTablesInArrays = new ConfigBoolValue(this, "AlignTablesInArrays", "Align 'tables' of structure-identical annotations in arrays", true);
   
	private final ConfigValue[] configValues = new ConfigValue[] { 
			configInfoSpaces, configSpaceAfterColon, configSpaceInsideBraces, configSpaceInsideBrackets,
			configInfoOneLiners, configMaxLineLength, configMaxOneLinerElemCountMain, configMaxOneLinerElemCountList, 
			configInfoAlignment, configAlignValues, configAlignTablesInArrays };
	
	public int getSpacesAfterColon() { return configSpaceAfterColon.getValue() ? 1 : 0; }
	public int getSpacesInsideBraces() { return configSpaceInsideBraces.getValue() ? 1 : 0; }
	public int getSpacesInsideBrackets() { return configSpaceInsideBrackets.getValue() ? 1 : 0; }
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAnnotationLayoutRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	DdlAnnotationLayout getLayout() {
		return new DdlAnnotationLayout(
				getSpacesAfterColon(), getSpacesInsideBraces(), getSpacesInsideBrackets(),
				configMaxLineLength.getValue(), configMaxOneLinerElemCountMain.getValue(), configMaxOneLinerElemCountList.getValue(),
				configAlignValues.getValue(), configAlignTablesInArrays.getValue());
	}

	@Override
	protected void executeOn(Code code, DdlAnnotationScope scope) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// determine the layout, which also depends on whether the annotations belong to the whole entity or to an element in the parameter or select list

		DdlAnnotationLayout layout = getLayout();
		Token firstToken = scope.getFirstCommand().getFirstToken();
		layout.setContext(firstToken.lineBreaks, firstToken.spacesLeft); 
		layout.setEmptyLines(DdlAnnotationEmptyLines.KEEP_AS_IS);
		
		scope.finishBuild();
		scope.useExistingNesting();
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
