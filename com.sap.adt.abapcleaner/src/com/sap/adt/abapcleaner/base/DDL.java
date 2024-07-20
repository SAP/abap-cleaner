package com.sap.adt.abapcleaner.base;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public final class DDL {
   public static final int INDENT_STEP = 2;

   public static final char COMMENT_SIGN = '/'; // the sign with which both // and /* start

   public static final String ASTERISK_COMMENT_START = "/*";
   public static final String ASTERISK_COMMENT_END = "*/";

   public static final String LINE_END_COMMENT = "//";
   public static final String LINE_END_MINUS_COMMENT = "--";

   public static final char QUOT_MARK = '\'';
   public static final String QUOT_MARK_STRING = "\'";

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

   public final static String SESSION_PREFIX = "$session"; // "$session.bs_instance_id", "$session.bs_zone_id", "$session.client", "$session.system_date", "$session.system_language", "$session.user", "$session.user_date", "$session.user_timezone"
   public final static String PARAMETER_PREFIX = "$parameter";
   public final static String TYPED_LITERAL_PREFIX = "abap."; // e.g. abap.int4'...', abap.numc'...', abap.quan'...'

   public final static String BASE_INFO_COMMENT_START = "/*+[internal] {";
   public final static String BASE_INFO_COMMENT_LINE_SEP = "\n";
   public final static String BASE_INFO_COMMENT_END = "}*/";

   public final static String[] listElementSeparators = new String[] { COMMA_SIGN_STRING, SEMICOLON_SIGN_STRING };

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
   		"abstract entity", "all", "and", "as", "as parent child hierarchy", "as projection on", "association", "association to parent", "avg (", "between", "boxed", "case", "cast (", "composition", "count (", "cross join", "custom entity",  
   		"define abstract entity", "define custom entity", "define external entity", "define root abstract entity", "define root custom entity", "define root view", "define root view entity", "define structure", "define table", "define table function", 
   		"define transient view entity", "define view", "define view entity", "distinct", "else", "end", "exact one to exact one", "exact one to many", "exact one to one", "except select", "extend", "extend custom entity", "extend view",  
   		"extend view entity", "external entity", "external name", "group by", "having", "hierarchy", "hierarchy:ascending", "hierarchy:cache force", "hierarchy:cache off", "hierarchy:cache on", "hierarchy:child to parent association",   
   		"hierarchy:cycles breakup", "hierarchy:cycles error", "hierarchy:depth", "hierarchy:descending", "hierarchy:directory", "hierarchy:filter by", "hierarchy:generate spantree", "hierarchy:load bulk", "hierarchy:load incremental", "hierarchy:load",  
   		"hierarchy:multiple parents allowed", "hierarchy:multiple parents leaves", "hierarchy:multiple parents leaves only", "hierarchy:multiple parents not allowed", "hierarchy:nodetype", "hierarchy:orphans error", "hierarchy:orphans ignore", "hierarchy:orphans root", "hierarchy:period from",  
   		"hierarchy:siblings order by", "hierarchy:source", "hierarchy:start where", "hierarchy:to", "hierarchy:valid from", "hierarchy:with parameters", "implemented by method", "include", "inner", "intersect select", "is initial", "is null", "join", "key",  
   		"left outer", "like", "localized", "many to exact one", "many to many", "many to one", "max (", "min (", "not", "not null", "null", "on", "one to exact one", "one to many", "one to one", "or", "preserving type", "provided at runtime",  
   		"provider contract analytical_query", "provider contract transactional_interface", "provider contract transactional_query", "redefine association", "redirected to", "redirected to composition child", "redirected to parent", "reference to",  
   		"returns", "right outer join", "root abstract entity", "root custom entity", "root view", "root view entity", "select distinct from", "select from", "sum (", "table function", "then", "to", "to exact one", "to many", "to one", "union", "union all",    
   		"union all select", "union select", "view", "view entity", "virtual", "when", "where", "with", "with default filter", "with federated data", "with foreign key", "with parameters", "with suffix", "with value help" });

   private final static HashSet<String> builtInFunctions = initializeHashSet(new String[] { 
   		"abap_system_timezone", "abap_user_timezone", "abs", "bintohex", "ceil", "coalesce", "concat", "concat_with_space", "currency_conversion", "curr_to_decfloat_amount", "datn_add_days", "datn_add_months", "datn_days_between", "dats_add_days", "dats_add_months", "dats_days_between", "dats_from_datn", "dats_is_valid", "dats_tims_to_tstmp", "dats_to_datn", "div", "division", "floor", "fltp_to_dec", "get_numeric_value", "hextobin", "instr", "left", "length", "lower", "lpad", "ltrim", "mod", "replace", "replace_regexpr", "replace_regexpr", "right", "round", "rpad", "rtrim", "substring", "tims_from_timn", "tims_is_valid", "tims_to_timn", "tstmpl_from_utcl", "tstmpl_to_utcl", "tstmp_add_seconds", "tstmp_current_utctimestamp", "tstmp_is_valid", "tstmp_seconds_between", "tstmp_to_dats", "tstmp_to_dst", "tstmp_to_tims", "unit_conversion", "upper", "utcl_add_seconds", "utcl_current", "utcl_seconds_between",
   		// SAP-delivered analytical scalar functions
   		"calendar_operation", "calendar_shift", "column_total", "current_total", "deviation_ratio", "exponential", "fiscal_calendar_operation", "fiscal_calendar_shift", "get_cell_reference_value", "grand_total", "hry_node_sign_value", "ln", "log", "power", "ratio_of", "row_total", "sqrt" }); 

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
   
   /** annotations of type ElementRef */
   public final static String[] elementRefAnnotations = new String[] {  "@Semantics.amount.currencyCode",
																								"@Semantics.interval.lowerBoundaryElement",
																								"@Semantics.interval.upperBoundaryElement",
																								"@Semantics.interval.boundaryCodeElement",
																								"@Semantics.languageReference",
																								"@Semantics.largeObject.mimeType", 
																								"@Semantics.nullValueIndicatorFor",
																								"@Semantics.quantity.unitOfMeasure",
																								"@Semantics.quantity.unitOfMeasureIsoCode",
																								"@Semantics.quantity.unitOfMeasureSapCode",
																								"@Semantics.timeZoneReference" };

   private static String getDdlKeywordKey(String ddlKeyword) {
		return AbapCult.toLower(ddlKeyword);
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

	public static boolean isKeyword(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return ddlKeywords.containsKey(getDdlKeywordKey(text));
		}
	}

	public static boolean isBuiltInFunction(String text) {
		if (StringUtil.isNullOrEmpty(text)) {
			return false;
		} else {
			return builtInFunctions.contains(getDdlKeywordKey(text));
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

	public static boolean isCharAllowedForIdentifier(String text, int pos, boolean isFirstChar) {
		if (StringUtil.isNullOrEmpty(text) || pos >= text.length())
			return false;

		char c = text.charAt(pos);
		
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
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
	public static boolean isKnownCollocation(ArrayList<String> keywordSequence, int mainIndex, String parentFunction) {
		String[] actSequence = new String[keywordSequence.size()];
		
		String mainKeyword = getDdlKeywordKey(keywordSequence.get(mainIndex));
		ArrayList<Collocation> collocations = ddlKeywords.get(mainKeyword);
		
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
}
