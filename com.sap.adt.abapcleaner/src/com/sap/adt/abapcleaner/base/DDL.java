package com.sap.adt.abapcleaner.base;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public final class DDL {
   public static final int INDENT_STEP = 2;

   public static final char COMMENT_SIGN = '/'; // the sign with which both // and /* start
   public static final String COMMENT_SIGN_STRING = "/"; 

   public static final String ASTERISK_COMMENT_START = "/*";
   public static final String ASTERISK_COMMENT_END = "*/";

   public static final String LINE_END_COMMENT = "//";
   public static final String LINE_END_MINUS_COMMENT = "--";

   public static final char QUOT_MARK = '\'';
   public static final String QUOT_MARK_STRING = "\'";
   public static final char QUOT_ESCAPE_CHAR = '\\';

   public static final char DOT_SIGN = '.';
   public static final String DOT_SIGN_STRING = ".";

   public static final char COMMA_SIGN = ',';
   public static final String COMMA_SIGN_STRING = ",";

   public static final char COLON_SIGN = ':';
   public static final String COLON_SIGN_STRING = ":";

   public static final char SEMICOLON_SIGN = ';';
   public static final String SEMICOLON_SIGN_STRING = ";";

   public static final char ANNOTATION_SIGN = '@';
   public static final String ANNOTATION_SIGN_STRING = "@";

   public static final String ANNOTATION_AFTER_LIST_ELEMENT_PREFIX = "@<";

   public static final char PARENS_OPEN = '(';
   public static final String PARENS_OPEN_STRING = "(";
   public static final char PARENS_CLOSE = ')';
   public static final String PARENS_CLOSE_STRING = ")";

   public static final char BRACKET_OPEN = '[';
   public static final String BRACKET_OPEN_STRING = "[";
   public static final char BRACKET_CLOSE = ']';
   public static final String BRACKET_CLOSE_STRING = "]";

   public static final char BRACE_OPEN = '{';
   public static final String BRACE_OPEN_STRING = "{";
   public static final char BRACE_CLOSE = '}';
   public static final String BRACE_CLOSE_STRING = "}";

   public static final char UNDERSCORE = '_';
   public static final String UNDERSCORE_STRING = "_";

   public static final String FUNCTION_PARAM_ASSIGNMENT_OP = "=>";

   public final static String SESSION_PREFIX = "$session"; // "$session.bs_instance_id", "$session.bs_zone_id", "$session.client", "$session.system_date", "$session.system_language", "$session.user", "$session.user_date", "$session.user_timezone"
   public final static String PARAMETER_PREFIX = "$parameters";
   public final static String PROJECTION_PREFIX = "$projection";
   public final static String TYPED_LITERAL_PREFIX = "abap."; // e.g. abap.int4'...', abap.numc'...', abap.quan'...'

   public final static String BASE_INFO_COMMENT_START = "/*+[internal] {";
   public final static String BASE_INFO_COMMENT_LINE_SEP = "\n";
   public final static String BASE_INFO_COMMENT_END = "}*/";

   public static final int MAX_LINE_LENGTH = 999; // however, activation even works for lines with 4000+ chars

   public final static String[] listElementSeparators = new String[] { COMMA_SIGN_STRING, SEMICOLON_SIGN_STRING };
   public final static String[] arithmeticOperators = new String[] { "+", "-", "*", "/" };
   public final static String divisionOperator = "/";
   public final static String[] aggregationFunctions = new String[] { "max", "min", "avg", "sum", "count" };

   private static class Collocation {
   	private final String parentFunction; // may be null
   	private final String[] keywordSequence;
   	
   	private Collocation(String parentFunction, String... keywordSequence) {
   		this.parentFunction = parentFunction;
   		this.keywordSequence = keywordSequence;
   	}
   	
   	private int getIndexOf(String keyword) {
   		int index = 0;
   		while (index < keywordSequence.length) {
   			if (keywordSequence[index].equals(keyword))
   				return index;
   			++index;
   		}
   		return -1;
   	}
   }

   /* all known DDL keywords; each keyword is mapped to all known DDL keyword collocations in which it can appear 
    * (including single-keyword collocations, if the keyword may appear stand-alone) */
   private final static HashMap<String, ArrayList<Collocation>> ddlKeywords = initializeKeywords(new String[] { 
   		"abstract entity", "all", "and", "annotate entity", "annotate view", "as", "as parent child hierarchy", "as projection on", "as select", "as select distinct", "association", "association to parent", "avg (", "between", "boxed", "case", "cast (",    
   		"composition", "count (", "cross join", "custom entity", "define abstract entity", "define custom entity", "define external entity", "define hierarchy", "define root abstract entity", "define root custom entity", "define root view", "define root view entity",   
   		"define structure", "define table", "define table function", "define transient view entity", "define view", "define view entity", "distinct", "else", "end", "exact one to exact one", "exact one to many", "exact one to one", "except select",    
   		"extend", "extend custom entity", "extend view", "extend view entity", "external entity", "external name", "from", "group by", "having", "hierarchy", "hierarchy:ascending", "hierarchy:cache force", "hierarchy:cache off", "hierarchy:cache on",    
   		"hierarchy:child to parent association", "hierarchy:cycles breakup", "hierarchy:cycles error", "hierarchy:depth", "hierarchy:descending", "hierarchy:directory", "hierarchy:filter by", "hierarchy:generate spantree", "hierarchy:load bulk",   
   		"hierarchy:load incremental", "hierarchy:load", "hierarchy:multiple parents allowed", "hierarchy:multiple parents leaves", "hierarchy:multiple parents leaves only", "hierarchy:multiple parents not allowed", "hierarchy:nodetype",   
   		"hierarchy:orphans error", "hierarchy:orphans ignore", "hierarchy:orphans root", "hierarchy:period from", "hierarchy:siblings order by", "hierarchy:source", "hierarchy:start where", "hierarchy:to", "hierarchy:valid from",   
   		"hierarchy:with parameters", "implemented by method", "include", "inner", "intersect select", "is initial", "is not initial", "is not null", "is null", "join", "key", "left outer", "like", "localized", "many to exact one", "many to many", "many to one", "max (", "min (",   
   		"not", "not null", "null", "of", "of exact one to exact one", "of exact one to many", "of exact one to one", "of many to exact one", "of many to many", "of many to one", "of one to exact one", "of one to many", "of one to one", "of to exact one", 
   		"of to many", "of to one", "on", "one to exact one", "one to many", "one to one", "or", "preserving type", "provided at runtime", "provider contract analytical_query", "provider contract transactional_interface",   
   		"provider contract transactional_query", "redefine association", "redirected to", "redirected to composition child", "redirected to parent", "reference to", "returns", "right outer join", "root abstract entity", "root custom entity",     
   		"root view", "root view entity", "select distinct from", "select from", "sum (", "table function", "then", "to", "to exact one", "to many", "to one", "union", "union all", "union all select", "union select", "view", "view entity", 
   		"virtual", "when", "where", "with", "with default filter", "with federated data", "with foreign key", "with parameters", "with suffix", "with value help", "with variant" });

   /* all known DCL keywords; each keyword is mapped to all known DCL keyword collocations in which it can appear 
    * (including single-keyword collocations, if the keyword may appear stand-alone) */
   private final static HashMap<String, ArrayList<Collocation>> dclKeywords = initializeKeywords(new String[] { 
   		"accesspolicy", "all", "and", "as projection on", "as select from", "aspect", "between", "bypass when", "combination mode and", "combination mode or", "conditions on any", "constraint id", "default false", "default true", 
   		"define accesspolicy", "define aspect", "define pfcg_mapping", "define role", "element", "else", "entity", "escape", "exists", "fallback association", "false", "field", "filter by", "for grant select on", "grant select on", "if", 
   		"if all conditions void then false", "if all conditions void then true", "if all conditions void then void", "in scenario", "including parameters", "inherit", "inheriting conditions from", "is initial", "is initial or null", 
   		"is not initial", "is not null", "is null", "like", "not", "not between", "not like", "or", "parameters with", "pfcg_filter", "pfcg_filter object", "pfcg_mapping", "prefix with", "redefinition", "replacing", "role", "root with", 
   		"super", "switchable", "then", "true", "value", "void", "where", "with", "with false", "with filter elements", "with optional elements", "with true", "with user element", "with void" });

   private final static HashSet<String> builtInDdlFunctions = initializeHashSet(new String[] { 
   		"abap_system_timezone", "abap_user_timezone", "abs", "bintohex", "ceil", "coalesce", "concat", "concat_with_space", "currency_conversion", "curr_to_decfloat_amount", "datn_add_days", "datn_add_months", "datn_days_between", "dats_add_days", "dats_add_months", "dats_days_between", "dats_from_datn", "dats_is_valid", "dats_tims_to_tstmp", "dats_to_datn", "div", "division", "floor", "fltp_to_dec", "get_numeric_value", "hextobin", "instr", "left", "length", "lower", "lpad", "ltrim", "mod", "replace", "replace_regexpr", "replace_regexpr", "right", "round", "rpad", "rtrim", "substring", "tims_from_timn", "tims_is_valid", "tims_to_timn", "tstmpl_from_utcl", "tstmpl_to_utcl", "tstmp_add_seconds", "tstmp_current_utctimestamp", "tstmp_is_valid", "tstmp_seconds_between", "tstmp_to_dats", "tstmp_to_dst", "tstmp_to_tims", "unit_conversion", "upper", "utcl_add_seconds", "utcl_current", "utcl_seconds_between",
   		// SAP-delivered analytical scalar functions
   		"calendar_operation", "calendar_shift", "column_total", "current_total", "deviation_ratio", "exponential", "fiscal_calendar_operation", "fiscal_calendar_shift", "get_cell_reference_value", "grand_total", "hry_node_sign_value", "ln", "log", "power", "ratio_of", "row_total", "sqrt" }); 

   private final static HashSet<String> builtInDclFunctions = initializeHashSet(new String[] {
   		"context_node_exists", "optional_element_exists", "sacf_check_in_use", "switch_runtime_state", "toggle_runtime_state" }); 

   public final static String[] typedLiterals = new String[] { "abap.char", "abap.clnt", "abap.cuky", "abap.curr", "abap.d16n", "abap.d34n", "abap.datn", "abap.dats", "abap.dec", "abap.decfloat16", "abap.decfloat34", "abap.fltp", "abap.int1", "abap.int2", "abap.int4", "abap.int8", "abap.lang", "abap.numc", "abap.quan", "abap.raw", "abap.rawstring", "abap.rstr", "abap.sstring", "abap.string", "abap.timn", "abap.tims", "abap.unit", "abap.utcl", "abap.utclong" };

	// [DEFINE] [ROOT] VIEW ENTITY view_entity 
   //   [WITH PARAMETERS parameter1, parameter2, ...] *AS* SELECT [DISTINCT] FROM data_source ...
	// DEFINE TRANSIENT VIEW ENTITY analytical_query PROVIDER CONTRACT ANALYTICAL_QUERY 
   //   [WITH PARAMETERS parameter1, parameter2, ...] *AS* PROJECTION ON cds_entity ...
	// [DEFINE] TABLE FUNCTION table_function 
   //   [WITH PARAMETERS parameter1, parameter2, ...] *RETURNS* { element1; element2; ...; } ...
	// [DEFINE] [ROOT] CUSTOM ENTITY custom_entity 
   //   [WITH PARAMETERS parameter1, parameter2, ...] *{* element1; element2; ...; }
   public final static String[] levelClosersAfterParameterList = new String[] { "as", "returns", DDL.BRACE_OPEN_STRING };

   /** annotations of type ElementRef or KeyElementRef (only ObjectModel.representativeKey) */
   private final static HashSet<String> elementRefAnnotations = initializeRefHashSet(new String[] {
   		"AccessControl.personalData.blockingIndicator[]",
   		"Aggregation.referenceElement[]",
   		"Analytics.dataExtraction.alternativeKey[]",
   		"Analytics.dataExtraction.delta.byElement.name",
   		"Analytics.dataExtraction.delta.changeDataCapture.mapping[].viewElement[]",
   		"Analytics.dataExtraction.filter[].viewElement",
   		"Analytics.dataExtraction.partitionBy[]",
   		"Analytics.settings.columns.hierarchicalDisplay.expandTo",
   		"Analytics.settings.rows.hierarchicalDisplay.expandTo",
   		"AnalyticsDetails.planning.distributionReference",
   		"AnalyticsDetails.query.cellReference.characteristicStructureElement",
   		"AnalyticsDetails.query.cellReference.measureStructureElement",
   		"AnalyticsDetails.query.elementHierarchy.parent",
   		"AnalyticsDetails.query.ignoreFurtherFilter.forElement[]",
   		"API.element.successor",
   		"Consumption.filter.defaultHierarchyNode.node[].element",
   		"Consumption.filter.defaultHierarchyNode.nodeType",
   		"Consumption.groupWithElement",
   		"Consumption.hierarchyNodeSelection.hierarchyElement",
   		"Consumption.labelElement",
   		"Consumption.quickInfoElement",
   		"Consumption.semanticObjectMapping.additionalBinding[].localElement",
   		"Consumption.valueHelp",
   		"Consumption.valueHelpDefinition[].additionalBinding[].localElement",
   		"Consumption.valueHelpDefinition[].enabled",
   		"EnterpriseSearch.fieldGroupForSearchQuery[].elements[]",
   		"EnterpriseSearch.filteringFacet.iconUrl",
   		"EnterpriseSearch.filteringFacet.order.byReference",
   		"EnterpriseSearch.navigation.urlBased.urlField",
   		"EnterpriseSearch.nls.compoundElements[].elements[]",
   		"EnterpriseSearch.nls.variantElements[].elements[]",
   		"EnterpriseSearch.resultItemKey[]",
   		"EnterpriseSearch.title.subTitleField",
   		"EnterpriseSearch.title.titleField",
   		"Event.previousValue.element",
   		"GenericPersistency.format.decimals",
   		"GenericPersistency.format.displayTemplate",
   		"GenericPersistency.format.exponentialDisplay.displayFormat",
   		"GenericPersistency.format.exponentialDisplay.exponentValue",
   		"GenericPersistency.format.length",
   		"GenericPersistency.propertyValue[]",
   		"Hierarchy.parentChild[].recurse.child[]",
   		"Hierarchy.parentChild[].recurse.parent[]",
   		"Hierarchy.parentChild[].recurseBy",
   		"Hierarchy.parentChild[].siblingsOrder[].by",
   		"ObjectModel.alternativeKey[].element[]",
   		"ObjectModel.collectiveValueHelp.for.element",
   		"ObjectModel.derivationFunction.inputElement[]",
   		"ObjectModel.derivationFunction.result.element",
   		"ObjectModel.derivationFunction.result.elementHigh",
   		"ObjectModel.derivationFunction.result.nodeTypeElement",
   		"ObjectModel.editableFieldFor",
   		"ObjectModel.interval.upperBoundary",
   		"ObjectModel.objectIdentifier.oidElement",
   		"ObjectModel.representativeKey",
   		"ObjectModel.semanticKey[]",
   		"ObjectModel.text.element[]",
   		"ObjectModel.uniqueIdField",
   		"ObjectModel.value.derivedFrom[]",
   		"OData.hierarchy.recursiveHierarchy[].descendantCountElement",
   		"OData.hierarchy.recursiveHierarchy[].distanceFromRootElement",
   		"OData.hierarchy.recursiveHierarchy[].drillStateElement",
   		"OData.hierarchy.recursiveHierarchy[].elementWithHierarchy",
   		"OData.hierarchy.recursiveHierarchy[].externalKeyElement",
   		"OData.hierarchy.recursiveHierarchy[].nodeElement",
   		"OData.hierarchy.recursiveHierarchy[].parentNodeElement",
   		"OData.hierarchy.recursiveHierarchy[].preorderRankElement",
   		"OData.hierarchy.recursiveHierarchy[].siblingRankElement",
   		"OData.property.valueControl",
   		"ODM.oid",
   		"Semantics.amount.currencyCode",
   		"Semantics.interval[].boundaryCodeElement",
   		"Semantics.interval[].lowerBoundaryElement",
   		"Semantics.interval[].upperBoundaryElement",
   		"Semantics.languageReference",
   		"Semantics.largeObject.fileName",
   		"Semantics.largeObject.mimeType",
   		"Semantics.nullValueIndicatorFor",
   		"Semantics.quantity.unitOfMeasure",
   		"Semantics.quantity.unitOfMeasureIsoCode",
   		"Semantics.quantity.unitOfMeasureSapCode",
   		"Semantics.timeZoneReference",
   		"UI.badge.headLine.criticality",
   		"UI.badge.headLine.targetElement",
   		"UI.badge.headLine.url",
   		"UI.badge.headLine.value",
   		"UI.badge.imageUrl",
   		"UI.badge.mainInfo.criticality",
   		"UI.badge.mainInfo.targetElement",
   		"UI.badge.mainInfo.url",
   		"UI.badge.mainInfo.value",
   		"UI.badge.secondaryInfo.criticality",
   		"UI.badge.secondaryInfo.targetElement",
   		"UI.badge.secondaryInfo.url",
   		"UI.badge.secondaryInfo.value",
   		"UI.badge.title.criticality",
   		"UI.badge.title.targetElement",
   		"UI.badge.title.url",
   		"UI.badge.title.value",
   		"UI.chart[].dimensionAttributes[].dimension",
   		"UI.chart[].dimensions[]",
   		"UI.chart[].measureAttributes[].measure",
   		"UI.chart[].measures[]",
   		"UI.connectedFields[].criticality",
   		"UI.connectedFields[].semanticObjectBinding[].localElement",
   		"UI.connectedFields[].targetElement",
   		"UI.connectedFields[].url",
   		"UI.connectedFields[].value",
   		"UI.dataFieldDefault[].criticality",
   		"UI.dataFieldDefault[].url",
   		"UI.dataFieldDefault[].value",
   		"UI.dataPoint.criticality",
   		"UI.dataPoint.criticalityCalculation.constantThresholds[].aggregationLevel[]",
   		"UI.dataPoint.criticalityCalculation.deviationRangeHighValueElement",
   		"UI.dataPoint.criticalityCalculation.deviationRangeLowValueElement",
   		"UI.dataPoint.criticalityCalculation.toleranceRangeHighValueElement",
   		"UI.dataPoint.criticalityCalculation.toleranceRangeLowValueElement",
   		"UI.dataPoint.forecastValue",
   		"UI.dataPoint.referencePeriod.end",
   		"UI.dataPoint.referencePeriod.start",
   		"UI.dataPoint.responsible",
   		"UI.dataPoint.targetValueElement",
   		"UI.dataPoint.trend",
   		"UI.dataPoint.trendCalculation.downDifferenceElement",
   		"UI.dataPoint.trendCalculation.referenceValue",
   		"UI.dataPoint.trendCalculation.strongDownDifferenceElement",
   		"UI.dataPoint.trendCalculation.strongUpDifferenceElement",
   		"UI.dataPoint.trendCalculation.upDifferenceElement",
   		"UI.facet[].targetElement",
   		"UI.facet[].url",
   		"UI.fieldGroup[].criticality",
   		"UI.fieldGroup[].semanticObjectBinding[].localElement",
   		"UI.fieldGroup[].targetElement",
   		"UI.fieldGroup[].url",
   		"UI.fieldGroup[].value",
   		"UI.headerInfo.description.criticality",
   		"UI.headerInfo.description.targetElement",
   		"UI.headerInfo.description.url",
   		"UI.headerInfo.description.value",
   		"UI.headerInfo.imageUrl",
   		"UI.headerInfo.title.criticality",
   		"UI.headerInfo.title.targetElement",
   		"UI.headerInfo.title.url",
   		"UI.headerInfo.title.value",
   		"UI.identification[].criticality",
   		"UI.identification[].semanticObjectBinding[].localElement",
   		"UI.identification[].targetElement",
   		"UI.identification[].url",
   		"UI.identification[].value",
   		"UI.kpi[].dataPoint.criticality",
   		"UI.kpi[].dataPoint.criticalityCalculation.constantThresholds[].aggregationLevel[]",
   		"UI.kpi[].dataPoint.referencePeriod.end",
   		"UI.kpi[].dataPoint.referencePeriod.start",
   		"UI.kpi[].dataPoint.responsible",
   		"UI.kpi[].dataPoint.trend",
   		"UI.kpi[].dataPoint.trendCalculation.referenceValue",
   		"UI.lineItem[].criticality",
   		"UI.lineItem[].semanticObjectBinding[].localElement",
   		"UI.lineItem[].targetElement",
   		"UI.lineItem[].url",
   		"UI.lineItem[].value",
   		"UI.note.content.mimeType",
   		"UI.note.content.value",
   		"UI.note.title.hidden",
   		"UI.note.title.value",
   		"UI.note.type.languageDependent",
   		"UI.note.type.maxLength",
   		"UI.note.type.multipleNotes",
   		"UI.note.type.name",
   		"UI.presentationVariant[].groupBy[]",
   		"UI.presentationVariant[].requestAtLeast[]",
   		"UI.presentationVariant[].sortOrder[].by",
   		"UI.presentationVariant[].total[]",
   		"UI.presentationVariant[].totalBy[]",
   		"UI.presentationVariant[].visualizations[].element",
   		"UI.selectionField[].element",
   		"UI.statusInfo[].criticality",
   		"UI.statusInfo[].semanticObjectBinding[].localElement",
   		"UI.statusInfo[].targetElement",
   		"UI.statusInfo[].url",
   		"UI.statusInfo[].value" });

   /** annotations of type ParameterRef */
   private final static HashSet<String> parameterRefAnnotations = initializeRefHashSet(new String[] {
   		"Consumption.dynamicLabel.binding[].parameter",
   		"Consumption.valueHelpDefinition[].additionalBinding[].localParameter",
   		"Semantics.interval[].lowerBoundaryParameter",
   		"Semantics.interval[].upperBoundaryParameter",
   		"UI.connectedFields[].semanticObjectBinding[].localParameter",
   		"UI.fieldGroup[].semanticObjectBinding[].localParameter",
   		"UI.identification[].semanticObjectBinding[].localParameter",
   		"UI.lineItem[].semanticObjectBinding[].localParameter",
   		"UI.selectionVariant[].parameters[].name",
   		"UI.statusInfo[].semanticObjectBinding[].localParameter" });

   /** annotations of type AssociationRef */
   private final static HashSet<String> associationRefAnnotations = initializeRefHashSet(new String[] {
   		"AccessControl.privilegedAssociations[]",
   		"Analytics.document.defaultAssociationToStorage",
   		"AnalyticsDetails.query.hierarchyAssociation",
   		"AnalyticsDetails.variable.hierarchyAssociation",
   		"Consumption.filter.hierarchyAssociation",
   		"Consumption.valueHelpDefinition[].association",
   		"Hierarchy.parentChild[].directory",
   		"ObjectModel.foreignKey.association",
   		"ObjectModel.hierarchy.association",
   		"ObjectModel.text.association",
   		"ObjectModel.text.reference.association",
   		"ObjectModel.unitConversionRate.association" });

   /** annotations of type EntityRef */
   private final static HashSet<String> entityRefAnnotations = initializeRefHashSet(new String[] {
   		"Analytics.document.storageForEntity[]",
   		"Consumption.derivation.lookupEntity",
   		"Consumption.valueHelpDefinition[].entity.name",
   		"EnterpriseSearch.hierarchy.parentChild.definition",
   		"ObjectModel.action[].parameter.dataType",
   		"ObjectModel.action[].result.dataType",
   		"ObjectModel.leadingEntity.name",
   		"OData.hierarchy.recursiveHierarchy[].entity.name",
   		"VDM.auxiliaryEntity.for.entity" });

   private static String getDdlKeywordKey(String ddlKeyword) {
		return AbapCult.toLower(ddlKeyword);
	}

   private static String getDdlRefKeywordKey(String ddlKeyword) {
		return AbapCult.toLower(ddlKeyword.replace("[]", ""));
	}

   private static HashMap<String, ArrayList<Collocation>> initializeKeywords(String[] collocations) {
   	HashMap<String, ArrayList<Collocation>> keywordMap = new HashMap<>();
   	
   	for (String collocation : collocations) {
   		collocation = getDdlKeywordKey(collocation);
   		
   		int colonPos = collocation.indexOf(":");
   		String parentFunction = null;
   		if (colonPos >= 0) {
   			parentFunction = collocation.substring(0, colonPos);
   			collocation = collocation.substring(colonPos + 1);
   		}
   		
   		String[] keywordSequence = StringUtil.split(collocation, ' ', true);
   		Collocation newCollocation = new Collocation(parentFunction, keywordSequence);
   		
   		// enter the Collocation for every keyword in the sequence except for "(", which only serves as a context for "MIN (", "MAX (" etc. 
   		for (String keywordInSequence : keywordSequence) {
   			if (keywordInSequence.equals(PARENS_OPEN_STRING))
   				continue;
   			String key = getDdlKeywordKey(keywordInSequence);
   			if (keywordMap.containsKey(key)) {
      			ArrayList<Collocation> collocationsOfKeyword = keywordMap.get(key);
      			collocationsOfKeyword.add(newCollocation);
   			} else {
   				ArrayList<Collocation> collocationsOfKeyword = new ArrayList<Collocation>();
   				collocationsOfKeyword.add(newCollocation);
   				keywordMap.put(key, collocationsOfKeyword);
   			}
   		}
   	}
   	return keywordMap;
   }
   
	private static HashSet<String> initializeHashSet(String[] keywords) {
		HashSet<String> result = new HashSet<String>();
		for (String keyword : keywords)
			result.add(getDdlKeywordKey(keyword));
		return result;
	}

	private static HashSet<String> initializeRefHashSet(String[] keywords) {
		HashSet<String> result = new HashSet<String>();
		for (String keyword : keywords)
			result.add(getDdlRefKeywordKey(keyword));
		return result;
	}

	public static boolean isNumeric(String num, boolean allowDot) {
		return ABAP.isNumeric(num, false, allowDot);
	}

	public static boolean isComparisonOperator(String text) {
		if (StringUtil.isNullOrEmpty(text))
			return false;
		
		switch (AbapCult.toUpper(text)) {
			case "<":
			case "<=":
			case "=":
			case ">=":
			case ">":
			case "<>":
			case "!=": 
				// "!=" is not listed in the documentation, but it works (unlike "==") 
				// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abencds_cond_expr_comp_v2.htm
				// and https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abencds_cond_expr_comp_v1.htm
				return true;

			default:
				return false;
		}
	}

	public static boolean isDdlKeyword(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return ddlKeywords.containsKey(getDdlKeywordKey(text));
		}
	}

	public static boolean isDclKeyword(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return dclKeywords.containsKey(getDdlKeywordKey(text));
		}
	}

	public static boolean isBuiltInDdlFunction(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return builtInDdlFunctions.contains(getDdlKeywordKey(text));
		}
	}

	public static boolean isBuiltInDclFunction(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return builtInDclFunctions.contains(getDdlKeywordKey(text));
		}
	}

	public static boolean isCharAllowedForAnyKeyword(char c) {
		return isCharAllowedForAnyKeyword(c, false);
	}

	public static boolean isCharAllowedForAnyKeyword(char c, boolean isFirstChar) {
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
			return true;
		} else if (isFirstChar) {
			return c == '$';
		} else {
			return false;
		}
	}

	public static boolean isIdentifier(String text) {
		for (int pos = 0; pos < text.length(); ++pos) {
			if (!isCharAllowedForIdentifier(text, pos, (pos == 0))) {
				return false;
			}
		}
		return true;
	}

	public static boolean isCharAllowedForIdentifier(String text, int pos, boolean isFirstChar) {
		if (StringUtil.isNullOrEmpty(text) || pos >= text.length())
			return false;

		char c = text.charAt(pos);
		
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '/') { // '_' at start is allowed for aliases, but e.g. not for fields 
			return true;
		
		} else if (isFirstChar && (c == '$' || c == '#' || c == '@')) {
			// $parameters, $session, @Annotation, #ENUM_CONSTANT
			return true;
		
		} else if (!isFirstChar && (c >= '0' && c <= '9')) {
			return true;
		
		} else {
			return false;
		}
	}
	
	public static boolean isAllowedParameterName(String text) {
		boolean isFirst = true;
		for (char c : text.toCharArray()) {
			// reusing isCharAllowedForFunction()
			if (!isCharAllowedForFunction(c, isFirst))
				return false;
			isFirst = false;
		}
		return true;
	}

	public static boolean isAllowedFunctionName(String text) {
		boolean isFirst = true;
		for (char c : text.toCharArray()) {
			if (!isCharAllowedForFunction(c, isFirst))
				return false;
			isFirst = false;
		}
		return true;
	}

	public static boolean isCharAllowedForFunction(char c, boolean isFirstChar) {
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
			return true;
		
		} else if (!isFirstChar && (c >= '0' && c <= '9')) {
			// hypothetical, because built-in functions do not use digits
			return true;
		
		} else {
			return false;
		}
	}
	
	/**
	 * returns true if the main keyword at keywordSequence.get(mainIndex) is actually a keyword;  
	 * for this, the keyword sequence surrounding the main keyword is checked against all known DDL keyword collocations 
	 * @param keywordSequence - the sequence of all consecutive (sibling) keywords, possibly with a final "("; 
	 * this sequence may include more keywords than needed for the collocation
	 * @param mainIndex - the index of the keyword in question within the keywordSequence
	 * @param parentFunction - the last keyword of the parent function (if any) of this keywordSequence,
	 * e.g. parentFunction = "HIERARCHY" for the collocation "CHILD TO PARENT ASSOCIATION" inside of "PARENT CHILD HIERARCHY( ... )"
	 * @return
	 */
	public static boolean isKnownCollocation(ArrayList<String> keywordSequence, int mainIndex, String parentFunction, Language language) {
		String[] actSequence = new String[keywordSequence.size()];
		
		String mainKeyword = getDdlKeywordKey(keywordSequence.get(mainIndex));
		ArrayList<Collocation> collocations = (language == Language.DDL) ? ddlKeywords.get(mainKeyword) : dclKeywords.get(mainKeyword);
		
		// preprocess the keyword sequence and the parent with getDdlKeywordKey() 
		int index = 0;
		for (String keyword : keywordSequence) {
			actSequence[index] = getDdlKeywordKey(keyword);
			++index;
		}
		String actParentFunction = (parentFunction == null) ? null : getDdlKeywordKey(parentFunction);
		
		// check whether one of the collocations for this keyword is a match
		for (Collocation collocation : collocations) {
			int mainCollocIndex = collocation.getIndexOf(mainKeyword);
			if (mainCollocIndex < 0) // pro forma
				continue;
			
			// check against this collocation
			if (mainCollocIndex > mainIndex || collocation.keywordSequence.length - mainCollocIndex > actSequence.length - mainIndex) {
				// insufficient number of keywords left or right
				continue;
			}
			if (collocation.parentFunction != null && (actParentFunction == null || !actParentFunction.equals(collocation.parentFunction))) {
				// no parent function found or parent function wrong
				continue;
			}
			boolean match = true;
			for (int collocIndex = 0; collocIndex < collocation.keywordSequence.length; ++collocIndex) {
				String collocKeyword = collocation.keywordSequence[collocIndex];
				String actKeyword = actSequence[collocIndex - mainCollocIndex + mainIndex];
				if (!collocKeyword.equals(actKeyword)) {
					match = false;
					break;
				}
			}
			if (match) {
				// Collocation match found
				return true;
			}
		}
		return false;
	}

	/** validates the input (which may be comma-, semicolon- or space-separated and may have spaces around periods) 
	 * and returns it as a comma-separated list of condensed annotation names with allowed chars only, 
	 * e.g. "ObjectModel.usageType, ObjectModel.lifecycle.*, Consumption". The @ sign is not returned */
	public static String toAnnotationList(String list) {
		final String separator = COMMA_SIGN_STRING + " ";
		
		if (StringUtil.isNullOrEmpty(list))
			return "";

		// allow comma- or semicolon-separated input
		String[] annotations = StringUtil.split(list, new char[] { COMMA_SIGN, SEMICOLON_SIGN }, true);
		if (annotations == null || annotations.length == 0)
			return "";

		StringBuilder sb = new StringBuilder();
		
		for (int annoIndex = 0; annoIndex < annotations.length; ++annoIndex) {
			String annotation = annotations[annoIndex];
			String[] elements = StringUtil.split(annotation, ' ', true);
			if (elements == null)
				continue;
			
			boolean expectDot = false;
			
			for (String element : elements) {
				if (element.equals(DOT_SIGN_STRING)) {
					if (expectDot) { // otherwise, ignore this dot
						sb.append(DOT_SIGN_STRING);
						expectDot = false;
					}
				} else {
					// reduce the element to the valid chars
					StringBuilder sbElement = new StringBuilder();
					for (char c : element.toCharArray()) {
						if (Character.isLetterOrDigit(c) || c == '_' || c == '.') {
							sbElement.append(c);
						}
					}
					element = sbElement.toString();
					if (element.length() > 0) {
						// if the input was 'space separated', add commas
						if (expectDot)
							sb.append(separator);
						sb.append(element);
						expectDot = !element.endsWith(DOT_SIGN_STRING);
					}
				}
			}
			
			if (annoIndex + 1 < annotations.length) {
				sb.append(separator);
			}
		}
		return sb.toString();
	}
	
	public static boolean isKnownElementRefAnnotation(String annotationPath) {
		return elementRefAnnotations.contains(getDdlRefKeywordKey(annotationPath));
	}
	
	public static boolean isKnownParameterRefAnnotation(String annotationPath) {
		return parameterRefAnnotations.contains(getDdlRefKeywordKey(annotationPath));
	}
	
	public static boolean isKnownAssociationRefAnnotation(String annotationPath) {
		return associationRefAnnotations.contains(getDdlRefKeywordKey(annotationPath));
	}

	public static boolean isKnownEntityRefAnnotation(String annotationPath) {
		return entityRefAnnotations.contains(getDdlRefKeywordKey(annotationPath));
	}
	
	public static boolean textStartsCommentAt(String text, int readPos) {
		return StringUtil.containsAnyAt(text, readPos, LINE_END_COMMENT, LINE_END_MINUS_COMMENT, ASTERISK_COMMENT_START);
	}
	
	/** splits composed identifiers such as "_AnyAlias._OtherAlias.FieldName" into String, e.g.
	 * { "_AnyAlias", ".", "_OtherAlias", ".", "FieldName" } */
	public static ArrayList<String> splitIdentifier(String identifier) {
		ArrayList<String> results = new ArrayList<String>();
		if (StringUtil.isNullOrEmpty(identifier))
			return results;

		int start = 0;
		while (start < identifier.length()) {
			boolean isIdentifier = isCharAllowedForIdentifier(identifier, start, (start == 0)); 
			int end = start + 1;
			while (end < identifier.length() && (isIdentifier == isCharAllowedForIdentifier(identifier, end, (end == 0)))) { 
				++end;
			}
			results.add(identifier.substring(start, end));
			start = end;
		}
		return results;
	}

}
