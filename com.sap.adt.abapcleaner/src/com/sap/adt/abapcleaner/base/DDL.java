package com.sap.adt.abapcleaner.base;

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
   
   private final static HashSet<String> ddlKeywords = initializeHashSet(new String[] {
   		// keywords for entity views
   		"all", "and", "as", "association", "avg", "between", "by", "case", "cast", "composition", "count", "cross", "default", "define", "distinct", "else", "end", "entity", "exact", "except", "filter", "from", "group", "having", "initial", "inner", "intersect", "is", "join", "key", "left", "like", "many", "max", "min", "not", "null", "of", "on", "one", "or", "outer", "parameters", "preserving", "right", "root", "select", "sum", "then", "to", "type", "union", "value", "view", "when", "where", "with", 
   	   // additional keywords for projection views
   	   "analytical_query", "child", "contract", "localized", "parent", "projection", "provider", "redefine", "redirected", "transactional_interface", "transactional_query", "transient", "virtual",
   	   // additional keywords for "define custom entity" and "define abstract entity"
   	   "abstract",
   		// additional keywords for "define table function"
   	   "function", "implemented", "method", "returns",
   		// additional keywords for "define hierarchy"
   	   "allowed", "ascending", "breakup", "bulk", "cache", "cycles", "depth", "descending", "directory", "error", "force", "generate", "hierarchy", "ignore", "incremental", "leaves", "load", "multiple", "nodetype", "off", "order", "orphans", "parents", "period", "siblings", "source", "spantree", "start", "valid", 
   		// additional keywords for "define structure" and "define table"
   	   "extend", "foreign", "help", "include", "reference", "remove", "structure", "suffix", "table"
   });
   
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
			return ddlKeywords.contains(getDdlKeywordKey(text));
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
}
