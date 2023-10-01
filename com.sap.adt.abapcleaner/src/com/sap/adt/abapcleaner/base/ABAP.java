package com.sap.adt.abapcleaner.base;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * Constants and helper methods about the ABAP programming language: 
 * keywords, operators, literals, allowed characters for identifiers etc.
 */
public final class ABAP {
   public static final int INDENT_STEP = 2;

   public static final char SUBSTRING_OFFSET = '+'; // as used in value+5(3)
   public static final char SUBSTRING_LENGTH_OPEN = '('; // as used in value+5(3)
   public static final char SUBSTRING_LENGTH_CLOSE = ')'; // as used in value+5(3)
   public static final String SUBSTRING_OFFSET_STRING = "+"; // as used in value+5(3)
   public static final String SUBSTRING_LENGTH_OPEN_STRING = "("; // as used in value+5(3)
   public static final String SUBSTRING_LENGTH_CLOSE_STRING = ")"; // as used in value+5(3)
   public static final String SPACE_ASSIGNMENT_OPERATOR_SPACE = " = ";
   public static final String TABULAR_ASSIGNMENT_INFIX = "  "; // separator between two assignments in a tabular layout, e.g. "( field1 = value1<infix>field2 = value2<infix>field3 = value3 )"

   public static final String ABAP_TRUE = "abap_true";
   public static final String ABAP_FALSE = "abap_false";
   public static final String ABAP_SPACE = "space";

   public final static String TEXT_SYMBOL_PREFIX = "TEXT-";
   public final static int TEXT_SYMBOL_ID_LENGTH = 3;

   public static final String SY_PREFIX = "sy-";
   public static final String SYST_PREFIX = "syst-";
   
   public static final String SY_FIELD_SUBRC = "subrc";
   public static final String SY_FIELD_TABIX = "tabix";
   public static final String SY_FIELD_INDEX = "index";
   /** number of lines of the table accessed via DESCRIBE TABLE, LOOP AT, and READ TABLE */
   public static final String SY_FIELD_TFILL = "tfill";
   /** line size of the table accessed via DESCRIBE TABLE, LOOP AT, and READ TABLE */
   public static final String SY_FIELD_TLENG = "tleng";
   
   public static enum SyField {
   	SUBRC(SY_FIELD_SUBRC),
   	TABIX(SY_FIELD_TABIX),
   	INDEX(SY_FIELD_INDEX),
   	TFILL(SY_FIELD_TFILL),
   	TLENG(SY_FIELD_TLENG);
   	
   	public final String name;
   	public final String syField;
   	public final String systField;
   	
   	SyField(String name) {
   		this.name = name;
   		this.syField = SY_PREFIX + name;
   		this.systField = SYST_PREFIX + name;
   	}
   }

   public static final String LINE_SEPARATOR = "\r\n";
   public static final char COMPONENT_SELECTOR = '-';
   public static final String COMPONENT_SELECTOR_STRING = "-";
   public static final char COMMENT_SIGN = '\"';
   public static final String COMMENT_SIGN_STRING = "\"";
   public static final String DYNAMIC_HELP_COMMENT_SIGN = COMMENT_SIGN_STRING + "!";
   public static final String PSEUDO_COMMENT_SIGN = COMMENT_SIGN_STRING + "#"; // e.g. "#EC NEEDED
   public static final String PSEUDO_COMMENT_EC_PREFIX = PSEUDO_COMMENT_SIGN + "EC "; 
   public static final char LINE_COMMENT_SIGN = '*';
   public static final String LINE_COMMENT_SIGN_STRING = "*";
   public static final String PRAGMA_SIGN = "##";
   public static final String SPACE_COMMENT_SIGN = " \"";
   public static final String END_OF_STATEMENT_SIGN = ".";
   public static final char QUOT_MARK = '\'';
   public static final String QUOT_MARK_STRING = "\'";
   public static final char QUOT_MARK2 = '`';
   public static final String QUOT_MARK2_STRING = "`";
   public static final char PIPE = '|';
   public static final String PIPE_STRING = "|";
   public static final char BRACE_OPEN = '{';
   public static final String BRACE_OPEN_STRING = "{";
   public static final char BRACE_CLOSE = '}';
   public static final String BRACE_CLOSE_STRING = "}";
   public static final char DOT_SIGN = '.';
   public static final String DOT_SIGN_STRING = ".";
   public static final char COMMA_SIGN = ',';
   public static final String COMMA_SIGN_STRING = ",";
   public static final char COLON_SIGN = ':';
   public static final String COLON_SIGN_STRING = ":";
   public static final char AT_SIGN = '@';
   public static final String AT_SIGN_STRING = "@";
   public static final char CLOSING_PARENTHESIS = ')';
   public static final String CLOSING_PARENTHESIS_STRING = ")";
   public static final char FIELD_SYMBOL_START_SIGN = '<';
   public static final char FIELD_SYMBOL_END_SIGN = '>';
   public static final char ASSOCIATION_PREFIX = '\\';
   public static final String ASSOCIATION_PREFIX_STRING = "\\";
   public static final char TILDE = '~';
   public static final String TILDE_STRING = "~";
   public static final String ABAP_DOC_SIGN = "\"!";
   public static final char ABAP_DOC_SEP = '|';
   public static final String ABAP_DOC_SEP_STRING = "|";
	public static final String ABAP_DOC_PARAMETER_ANNOTATION = "@parameter";
	public static final String ABAP_DOC_RAISING_ANNOTATION = "@raising";
	public static final String ABAP_DOC_EXCEPTION_ANNOTATION = "@exception";
   public static final char NAMESPACE_SIGN = '/';
   public static final char TABLE_EXPR_BRACKET_OPEN = '[';
   public static final char TABLE_EXPR_BRACKET_CLOSE = ']';
   public static final char PRAGMA_PARAMETER_BRACKET_OPEN = '[';
   public static final char PRAGMA_PARAMETER_BRACKET_CLOSE = ']';

   public static final String DEFAULT_TYPE = "c";
   public static final String DEFAULT_LENGTH_OF_TYPE_C_N_X = "1";
   public static final String DEFAULT_LENGTH_OF_TYPE_P = "8";
   public static final String DEFAULT_DECIMALS_OF_TYPE_P = "0";
   
   /** escape character that can be written before the name of an operand, see
    * <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennames_escaping.htm">Escape Character for Operands</a>*/
   public static final char OPERAND_ESCAPE_CHAR = '!';
   /** escape character that can be written before the name of an operand, see
    * <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennames_escaping.htm">Escape Character for Operands</a>*/
   public static final String OPERAND_ESCAPE_CHAR_STRING = "!";
   
   /** Opening parenthesis for linking a text symbol ID to a text field literal, 
    * e.g. 'literal text'(001), where the literal text is overridden if the text symbol TEXT-001 is defined
    * (see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentext_symbols.htm">ABAP Help</a>) */
   public static final char TEXT_SYMBOL_ID_OPEN = '('; 
   /** Closing parenthesis for linking a text symbol ID to a text field literal, 
    * e.g. 'literal text'(001), where the literal text is overridden if the text symbol TEXT-001 is defined
    * (see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentext_symbols.htm">ABAP Help</a>) */
   public static final char TEXT_SYMBOL_ID_CLOSE = ')'; 

   public static final int FIRST_LINE_INDEX = 1; // line count in ABAP Development Tools and SAP GUI starts at 1, not 0
   public static final int MAX_FIELD_NAME_LENGTH = 30;
   public static final int MAX_VARIABLE_NAME_LENGTH = 30;
   public static final int MAX_LINE_LENGTH = 255;
   
   // constant for the ABAP release against which a Code document must compile
   /** expresses that Code is compiled against the newest ABAP Release, i.e. no restrictions to ABAP syntax apply */
   public static final String NEWEST_RELEASE = "";

   /** expresses that the ABAP release of the system that contains the code document is unknown (value "fallback")  */
   public static final String FALLBACK_RELEASE = "fallback";

   // constants for the release requirements of a Rule
   /** expresses that a Rule is NOT restricted to a minimum ABAP Release */
   public static final int NO_REQUIRED_RELEASE = 0;
   /** represents the ABAP Release 7.0, EhP2, in which pragmas were introduced
    * see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-71-pragmas.htm */
   public static final int REQUIRED_RELEASE_702 = 702;
   /** represents the ABAP Release 7.40, in which e.g. the NEW operator was introduced, 
    * see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-740-expressions.htm */
   public static final int REQUIRED_RELEASE_740 = 740;
   /** represents the ABAP Release 7.52, in which e.g. RAISE EXCEPTION NEW ... (instead of TYPE ...) was introduced, 
    * see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-752-expressions.htm */
   public static final int REQUIRED_RELEASE_752 = 752;
   /** represents the ABAP Release 7.54, in which e.g. calculation assignment operators were introduced, 
    * see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-754-assignments.htm */
   public static final int REQUIRED_RELEASE_754 = 754;
   /** represents the ABAP Release 7.57, in which e.g. FINAL() inline declaration for immutable variables was introduced, 
    * see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-757-expressions.htm */
   public static final int REQUIRED_RELEASE_757 = 757;
   
   // constants for the ABAP release restriction from the UI; converted with ABAP.getReleaseRestrictionNumber() and ABAP.getReleaseRestrictionName() 
   /** expresses that changes must be restricted to the syntax which was available up to a specific ABAP release
    * (see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews-75.htm">ABAP Release News</a>) */
   public final static String[] RELEASE_RESTRICTION_NAMES = new String[] { "7.02", "7.03", "7.31", "7.40", "7.41", "7.42", "7.50", "7.51", "7.52", "7.53", "7.54", "7.55", "7.56", "7.57", "7.58" };
   /** expresses that all syntax available in the latest ABAP release can be used without restriction */
   public final static String NO_RELEASE_RESTRICTION_NAME = "Latest";
   /** expresses that all syntax available in the latest ABAP release can be used without restriction */
   public final static int NO_RELEASE_RESTRICTION = 0;
   
   public final static String[] constructorOperators = new String[] {"NEW", "VALUE", "CONV", "CORRESPONDING", "CAST", "REF", "EXACT", "REDUCE", "FILTER", "COND", "SWITCH"};

   private final static String[] lowerCaseKeywords = new String[] {"abs", "any", "c", "class_constructor", "constructor", "d", "dats", "n", "i", "int1", "int2", "int4", "int8", "lines(", "me", "p", "s", "space", "string", "subrc", "sy", "tims", "timestamp", "timestampl", "x", "xsdbool", "kind", "sign", "option", "low", "high", "uzeit", "uname", "datum", "table_line", "simple"};

   /** all built-in functions, cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenbuilt_in_functions_overview.htm */
   public final static String[] builtInFunctions = new String[] {"boolc(", "boolx(", "xsdbool(", "contains(", "contains_any_of(", "contains_any_not_of(", "matches(", "line_exists(", "abs(", "ceil(", "floor(", "frac(", "sign(", "trunc(", "ipow(", "nmax(", "nmin(", "acos(", "asin(", "atan(", "cos(", "sin(", "tan(", "cosh(", "sinh(", "tanh(", "exp(", "log(", "log10(", "sqrt(", "round(", "rescale(", "charlen(", "dbmaxlen(", "numofchar(", "strlen(", "char_off(", "cmax(", "cmin(", "count(", "count_any_of(", "count_any_not_of(", "distance(", "condense(", "concat_lines_of(", "escape(", "find(", "find_end(", "find_any_of(", "find_any_not_of(", "insert(", "match(", "repeat(", "replace(", "reverse(", "segment(", "shift_left(", "shift_right(", "substring(", "substring_after(", "substring_from(", "substring_before(", "substring_to(", "to_upper(", "to_lower(", "to_mixed(", "from_mixed(", "translate(", "xstrlen(", "bit-set(", "utclong_current(", "utclong_add(", "utclong_diff(", "lines(", "line_index("};
   /** built-in functions that have named parameters (and therefore could be aligned like method calls) */
   public final static String[] builtInFunctionsWithNamedParams = new String[] {"boolx(", "contains(", "contains_any_of(", "contains_any_not_of(", "matches(", "ipow(", "nmax(", "mnim(", "round(", "rescale(", "char_off(", "cmax(", "cmin(", "count(", "count_any_of(", "count_any_not_of(", "distance(", "condense(", "concat_lines_of(", "escape(", "find(", "find_end(", "find_any_of(", "find_any_not_of(", "insert(", "match(", "repeat(", "replace(", "reverse(", "segment(", "shift_left(", "shift_right(", "substring(", "substring_after(", "substring_from(", "substring_before(", "substring_to(", "to_upper(", "to_lower(", "to_mixed(", "from_mixed(", "translate(", "utclong_add(", "utclong_diff("};
   /** built-in functions that have no named parameters but instead expect one argument of a certain type 
    * (logical expression, table expression, numeric, string / byte string, table) */
   public final static String[] builtInFunctionsWithoutNamedParams = new String[] {"boolc(", "xsdbool(", "line_exists(", "line_index(", "abs(", "ceil(", "floor(", "frac(", "sign(", "trunc(", "acos(", "asin(", "atan(", "cos(", "sin(", "tan(", "cosh(", "sinh(", "tanh(", "exp(", "log(", "log10(", "sqrt(", "bit-set(", "charlen(", "dbmaxlen(", "numofchar(", "strlen(", "xstrlen(", "utclong_current(", "lines("};
   
   // Previously, ABAP.abapKeywords also contained keywords with hyphens like "MOVE-CORRESPONDING"; now, the distinct words "MOVE" and "CORRESPONDING" are used, see method isAbapUpperCaseKeyword(String).
   // Such keywords with hyphens can be found in section "ABAP Words" of the ABAP Reference (https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenabap_words.htm): 
   // private final static String[] keywordsWithHyphen = new String[] { "*-INPUT", "ABAP-SOURCE", "ADD-CORRESPONDING", "AUTHORITY-CHECK", "BIT-AND", "BIT-NOT", "BIT-OR", "BIT-XOR", "BREAK-POINT", "BYTE-CA", "BYTE-CN", "BYTE-CO", "BYTE-CS", "BYTE-NA", "BYTE-NS", "BYTE-ORDER", "CHAR-TO-HEX", "CLASS-CODING", "CLASS-DATA", "CLASS-EVENTS", "CLASS-METHODS", "CLASS-POOL", "CUSTOMER-FUNCTION", "DISPLAY-MODE", "DIVIDE-CORRESPONDING", "EDITOR-CALL", "END-ENHANCEMENT-SECTION", "END-LINES", "END-OF-DEFINITION", "END-OF-EDITING", "END-OF-FILE", "END-OF-PAGE", "END-OF-SELECTION", "END-TEST-INJECTION", "END-TEST-SEAM", "ENHANCEMENT-POINT", "ENHANCEMENT-SECTION", "EXCEPTION-TABLE", "EXIT-COMMAND", "FIELD-GROUPS", "FIELD-SYMBOL", "FIELD-SYMBOLS", "FILTER-TABLE", "FIRST-LINE", "FIXED-POINT", "FUNCTION-POOL", "HEAD-LINES", "HELP-ID", "HELP-REQUEST", "INDEX-LINE", "INTERFACE-POOL", "INVERTED-DATE", "LEFT-JUSTIFIED", "LINE-COUNT", "LINE-SELECTION", "LINE-SIZE", "LIST-PROCESSING", "LOAD-OF-PROGRAM", "LOG-POINT", "MAJOR-ID", "MESSAGE-ID", "MINOR-ID", "MOVE-CORRESPONDING", "MULTIPLY-CORRESPONDING", "NEW-LINE", "NEW-PAGE", "NEW-SECTION", "NO-DISPLAY", "NO-EXTENSION", "NO-GAP", "NO-GAPS", "NO-GROUPING", "NO-HEADING", "NON-UNICODE", "NON-UNIQUE", "NO-SCROLLING", "NO-SIGN", "NO-TITLE", "NO-TOPOFPAGE", "NO-ZERO", "OUTPUT-LENGTH", "PARAMETER-TABLE", "PF-STATUS", "PRINT-CONTROL", "QUEUE-ONLY", "READ-ONLY", "RIGHT-JUSTIFIED", "SAP-SPOOL", "SCROLL-BOUNDARY", "SELECTION-SCREEN", "SELECTION-SET", "SELECTION-SETS", "SELECTION-TABLE", "SELECT-OPTIONS", "SHORTDUMP-ID", "START-OF-EDITING", "START-OF-SELECTION", "STEP-LOOP", "SUBTRACT-CORRESPONDING", "SYNTAX-CHECK", "SYNTAX-TRACE", "SYSTEM-CALL", "SYSTEM-EXCEPTIONS", "SYSTEM-EXIT", "TEST-INJECTION", "TEST-SEAM", "TITLE-LINES", "TOP-LINES", "TOP-OF-PAGE", "TRACE-ENTRY", "TRACE-FILE", "TRACE-TABLE", "TYPE-POOL", "TYPE-POOLS", "USER-COMMAND", "UTF-8", "VALUE-REQUEST", "VERIFICATION-MESSAGE", "WITH-HEADING", "WITH-TITLE" };
   // private final static String[] otherOldKeywordsNotInPadFile = new String[] {"CI_", "CL_ABAP_BEHAVIOR_SAVER", "CL_DBI_UTILITIES", "CL_GUI_FRONTEND_SERVICES", "CONNECT", "CORRSPEARMAN", "DECFLOAT16_DEC", "DECFLOAT34_DEC", "DISCONNECT", "FLTP_TO_DEC", "FROMH", "ITNO", "OLE"};
   
   private final static HashSet<String> abapLowerCaseKeywords = initializeAbapLowerCaseKeywords(lowerCaseKeywords);
   private final static HashSet<String> abapKeywords = initializeAbapKeywords();

   // lazy instantiation with initializeKeywordCollocations():
   private static HashSet<String> abapKeywordCollocations;
   private static HashSet<String> abapKeywordCollocationStarts;

	// cp. https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/abenabap_loops.htm
   public final static String[] loopKeywords = new String[] { "LOOP", "DO", "WHILE", "SELECT", "PROVIDE" }; 
   public final static String[] loopEndKeywords = new String[] { "ENDLOOP", "ENDDO", "ENDWHILE", "ENDSELECT", "ENDPROVIDE" };
   
   // -------------------------------------------------------------------------
   // helper methods for optional / lazy instantiation 

   // With ADT installed, ABAP keywords can be found in C:\Users\...\workspace_adt\.metadata\.plugins\com.sap.adt.tools.abapsource.parser\757 (items 112:..1367:, see token nums in 4th line: "Token<TAB>111<TAB>1367<TAB>2920")
   // (cp. ADT -> menu "Project / Properties" -> ABAP Development -> Editors -> Source Code Editors -> RND Parser -> Open Folder)
   // (however, this also contains keywords like XSDBOOL, TABLE_LINE, INT8 etc. that are usually put to lower case; line_exists is missing; "LEFTPLUS" etc. are very special keywords)
   // the following fallback keywords are used in case the 'resources\grammar.text' resource file is inaccessible: 
   private static String[] getFallbackKeywords() {
	   return new String[] { "BEGIN", "END", "TEXT", "TO", "GIVING", "APPEND", "ASSIGN", "TYPE", "INITIAL", "AT", "ON", "AUTHORITY", "BREAK", "BADI", "FUNCTION", "EXCEPTIONS", "EXPORTING", "SOURCE", "RESULT", "CATCH", "CHECK", "CLASS", "WITH", "COMMIT", "COMMUNICATION", "BIT", "LIKE", "VALUE", "CONTROLS", "CONVERT", "DATE", "TIME", "CREATE", "DATA", "KEY", "DELETE", "FROM", "DESCRIBE", "INDEX", "LINE", "IN", "EDITOR", "ENHANCEMENT", "EXIT", "EXPORT", "STRUCTURE", "MATCH", "FREE", "GENERATE", "MESSAGE", "GET", "GLOBAL", "INSTANCE", "IMPORT", "ACCEPTING", "IGNORING", "CLIENT", "ID", "INCLUDE", "INSERT", "INTERFACE", "ALL", "LEAVE", "LOOP", "FOR", "MODIFY", "OF", "FIELD", "NO", "PRINT", "LIST", "NEW", "OPEN", "COUNT", "TABLE", "AS", "PERFORM", "PROVIDE", "SYSTEM", "READ", "REFRESH", "REPLACE", "REPLACEMENT", "ROLLBACK", "SET", "LEFT", "RIGHT", "SORT", "NOT", "BETWEEN", "IS", "CAST", "MAPPING", "EXCEPT", "STATICS", "ARCHIVE", "COVER", "USING", "VIA", "SYNTAX", "C_PROFILER", "LOAD", "POP", "PUSH", "QUERY", "RENAME", "RFC_ID", "MOVE", "TABLES", "TOP", "TYPES", "PRIMARY_KEY", "WAIT", "CHAIN", "%_END", "ADD", "CALL", "DIVIDE", "MODULE", "MULTIPLY", "SELECT", "START", "SUBTRACT", "METHOD", "FINAL", "IMPORTING", "LET", "BASE", "GROUPS", "STEP", "INTO", "GROUP", "WHERE", "TABLE_LINE", "EQ", "I", "NE", "LT", "LE", "GT", "GE", "BYTE", "CA", "CO", "CS", "NA", "CN", "NS", "M", "O", "Z", "CP", "NP", "AND", "BOUND", "OR", "EQUIV", "BY", "SIZE", "ASCENDING", "DESCENDING", "WITHOUT", "MEMBERS", "THEN", "WHILE", "SUPPLIED", "ASSIGNED", "REQUESTED", "DISABLED", "%_SWITCHED_ON", "UNTIL", "LINES", "RECEIVING", "OTHERS", "OPTIONAL", "DEFAULT", "CONV", "REF", "EXACT", "CORRESPONDING", "DEEP", "APPENDING", "DISCARDING", "DUPLICATES", "CONTROL", "ENTITY", "CHANGING", "FILTER", "COND", "WHEN", "THROW", "RESUMABLE", "SHORTDUMP", "NUMBER", "BOOLC", "XSDBOOL", "WIDTH", "ALIGN", "CENTER", "PAD", "CASE", "RAW", "UPPER", "LOWER", "SIGN", "LEFTPLUS", "LEFTSPACE", "RIGHTPLUS", "RIGHTSPACE", "EXPONENT", "DECIMALS", "ZERO", "YES", "XSD", "STYLE", "SIMPLE", "SIGN_AS_POSTFIX", "SCALE_PRESERVING", "SCIENTIFIC", "SCIENTIFIC_WITH_LEADING_ZERO", "SCALE_PRESERVING_SCIENTIFIC", "ENGINEERING", "EXTENDED_MONETARY", "MONETARY", "CURRENCY", "USER", "ENVIRONMENT", "ALPHA", "OUT", "ISO", "TIMESTAMP", "SPACE", "TIMEZONE", "COUNTRY", "ELSE", "SWITCH", "REDUCE", "INIT", "NEXT", "DIV", "MOD", "COMPONENTS", "BOOLX", "BOOL", "XOR", "RANGE", "ACCORDING", "NOTIFICATIONS", "ENTITIES", "UPDATE", "ALIASES", "SORTED", "ASSIGNING", "SYMBOL", "CASTING", "REFERENCE", "HANDLE", "%_TYPE", "UNASSIGN", "BUFFER", "LOCAL", "COPY", "%_L", "%_F", "MAIN", "COMPONENT", "INCREMENT", "ASSERT", "SUBKEY", "FIELDS", "CONDITION", "FIRST", "LAST", "SELECTION", "COMMAND", "PF1", "PF2", "PF3", "PF4", "PF5", "PF6", "PF7", "PF8", "PF9", "PF01", "PF02", "PF03", "PF04", "PF05", "PF06", "PF07", "PF08", "PF09", "PF10", "PF11", "PF12", "PF13", "PF14", "PF15", "PF16", "PF17", "PF18", "PF19", "PF20", "PF21", "PF22", "PF23", "PF24", "SCREEN", "REQUEST", "HELP", "RADIOBUTTON", "BLOCK", "OUTPUT", "DISABLE", "CONTEXT", "OBJECT", "DUMMY", "BACK", "POINT", "APPLICATION", "STATEMENT", "PARAMETER", "EXCEPTION", "FLUSH", "QUEUE", "ONLY", "QUEUEONLY", "%_SETPROP", "%_GETPROP", "CUSTOMER", "ERROR_MESSAGE", "PERFORMING", "TASK", "CALLING", "COMMUNICATION_FAILURE", "SYSTEM_FAILURE", "STARTING", "DESTINATION", "%_RFCOPT", "KEEPING", "LOGICAL", "UNIT", "WORK", "BACKGROUND", "SEPARATE", "%_RFC", "%_RECEIVE_KEEP_TASK", "%_RECEIVE", "REMOTE", "DATABASE", "PROCEDURE", "CONNECTION", "TRANSFORMATION", "XML", "PARAMETERS", "OBJECTS", "OPTIONS", "XML_HEADER", "DATA_REFS", "INITIAL_COMPONENTS", "TECHNICAL_TYPES", "VALUE_HANDLING", "TRANSACTION", "%_SHD1", "SKIP", "MESSAGES", "MODE", "DIALOG", "ENDING", "BEFORE", "UNWIND", "AFTER", "CLEAR", "CHARACTER", "NULL", "IMPLEMENTATION", "DEFINITION", "DEFERRED", "PUBLIC", "TESTING", "FRIENDS", "ABSTRACT", "INHERITING", "PROTECTED", "PRIVATE", "BEHAVIOR", "SHARED", "MEMORY", "ENABLED", "RISK", "LEVEL", "HARMLESS", "DANGEROUS", "CRITICAL", "DURATION", "SHORT", "MEDIUM", "LONG", "C", "N", "X", "P", "%_NON", "%_PREDEFINED", "%_PREDEFINED_DUMMY", "NON", "BOXED", "LOCATOR", "READER", "WRITER", "LOB", "OTHER", "CLOB", "BLOB", "COLUMNS", "LENGTH", "ACTION", "LINK", "LOCK", "FAILED", "EARLY", "LATE", "MAPPED", "REPORTED", "CHANGE", "FEATURES", "DETERMINATION", "VALIDATION", "AUTHORIZATION", "PERMISSIONS", "HIERARCHY", "EVENT", "RESPONSE", "HEADER", "POINTER", "HASHED", "UNIQUE", "ALIAS", "STANDARD", "EMPTY", "EVENTS", "METHODS", "FAIL", "IGNORE", "AMDP", "CDS", "SESSION", "ANY", "PREFERRED", "RETURNING", "RAISING", "DDL", "REQUIRED", "POOL", "CLEANUP", "CLOSE", "DATASET", "%_EXPECT", "CURSOR", "CONCATENATE", "SEPARATED", "RESPECTING", "BLANKS", "COLLECT", "SIMULATION", "RESPONSES", "RETURNCODE", "SEND", "RECEIVE", "DATAINFO", "STATUSINFO", "RECEIVED", "HOLD", "ALLOCATE", "DEALLOCATE", "ACCEPT", "COMPUTE", "CONDENSE", "GAPS", "CONSTANTS", "FURTHER", "SECONDARY", "KEYS", "CONTEXTS", "CONTINUE", "SEGMENT", "TABLEVIEW", "TABSTRIP", "INVERTED", "SORTABLE", "CODE", "STAMP", "ZONE", "DAYLIGHT", "SAVING", "UTCLONG", "FRACTIONAL", "SECONDS", "TEMPORARY", "AREA", "OCCURS", "VALID", "COMMON", "PART", "DEFINE", "HDB", "LANGUAGE", "SQLSCRIPT", "ADJACENT", "COMPARING", "REPORT", "STATE", "TEXTPOOL", "%_STRICT", "SPECIFIED", "CLIENTS", "T000", "INT1", "INT2", "INT4", "INT8", "NUMC", "DEC", "CURR", "QUAN", "FLTP", "DECFLOAT16", "DECFLOAT34", "DF16_DEC", "DF16_RAW", "DF34_DEC", "DF34_RAW", "D16D", "D34D", "D16R", "D34R", "D16N", "D34N", "CHAR", "CLNT", "CUKY", "LANG", "DATS", "DATN", "SSTRING", "TIMS", "TIMN", "INNER", "OUTER", "STRING", "RAWSTRING", "ESCAPE", "DISTINCT", "MIN", "MAX", "SUM", "MEDIAN", "STDDEV", "VAR", "AVG", "OVER", "PARTITION", "ORDER", "NULLS", "ROWS", "UNBOUNDED", "PRECEDING", "CURRENT", "ROW", "FOLLOWING", "ROW_NUMBER", "RANK", "DENSE_RANK", "STRING_AGG", "LEAD", "LAG", "FIRST_VALUE", "LAST_VALUE", "CORR", "CORR_SPEARMAN", "NTILE", "PRIVILEGED", "ACCESS", "CHILD", "PARENT", "ASSOCIATION", "PERIOD", "SIBLINGS", "DEPTH", "MULTIPLE", "PARENTS", "ALLOWED", "LEAVES", "ORPHANS", "ADOPT", "ROOT", "ERROR", "CYCLES", "BREAKUP", "BULK", "INCREMENTAL", "SPANTREE", "HIERARCHY_DESCENDANTS", "HIERARCHY_ANCESTORS", "HIERARCHY_SIBLINGS", "DISTANCE", "HIERARCHY_DESCENDANTS_AGGREGATE", "JOIN", "MEASURES", "PRODUCT", "SUBTOTAL", "BALANCE", "MATCHED", "TOTAL", "HIERARCHY_ANCESTORS_AGGREGATE", "%_NATIVE_TABLE", "ADABAS", "SYBASE", "DB2", "AS400", "DB6", "MSSQLNT", "ORACLE", "OTHERWISE", "ABS", "CEIL", "FLOOR", "COALESCE", "ROUND", "SUBSTRING", "LPAD", "LTRIM", "RTRIM", "CONCAT", "INSTR", "RPAD", "CONCAT_WITH_SPACE", "DIVISION", "DATS_IS_VALID", "DATS_ADD_DAYS", "DATS_DAYS_BETWEEN", "DATS_ADD_MONTHS", "BINTOHEX", "HEXTOBIN", "TIMS_IS_VALID", "TSTMP_IS_VALID", "GROUPING", "TSTMP_CURRENT_UTCTIMESTAMP", "UUID", "UTCL_CURRENT", "UTCL_SECONDS_BETWEEN", "UTCL_ADD_SECONDS", "DATN_ADD_DAYS", "DATN_DAYS_BETWEEN", "DATN_ADD_MONTHS", "AS_GEO_JSON", "ALLOW_PRECISION_LOSS", "TO_CLOB", "TO_BLOB", "EXTRACT_YEAR", "EXTRACT_MONTH", "EXTRACT_DAY", "EXTRACT_HOUR", "EXTRACT_MINUTE", "EXTRACT_SECOND", "INITCAP", "MONTHNAME", "DAYNAME", "WEEKDAY", "ADD_MONTHS", "DAYS_BETWEEN", "ADD_DAYS", "IS_VALID", "LOCATE", "ABAP_SYSTEM_TIMEZONE", "ON_ERROR", "ABAP_USER_TIMEZONE", "DATS_TIMS_TO_TSTMP", "TZONE", "TSTMP_ADD_SECONDS", "TSTMP", "TSTMP_SECONDS_BETWEEN", "TSTMP1", "TSTMP2", "TSTMP_TO_DATS", "TSTMP_TO_DST", "TSTMP_TO_TIMS", "%_NATIVE_FN", "TSTMPL_TO_UTCL", "TSTMPL", "ON_INITIAL", "TSTMPL_FROM_UTCL", "UTCL", "ON_NULL", "DATS_TO_DATN", "DATS_FROM_DATN", "TIMS_TO_TIMN", "TIMS_FROM_TIMN", "REPLACE_REGEXPR", "PCRE", "OCCURRENCE", "CASE_SENSITIVE", "LIKE_REGEXPR", "CURRENCY_CONVERSION", "AMOUNT", "SOURCE_CURRENCY", "TARGET_CURRENCY", "EXCHANGE_RATE_DATE", "EXCHANGE_RATE_TYPE", "DECIMAL_SHIFT", "DECIMAL_SHIFT_BACK", "OCCURRENCES_REGEXPR", "UNIT_CONVERSION", "QUANTITY", "SOURCE_UNIT", "TARGET_UNIT", "SUBSTRING_REGEXPR", "LOCATE_REGEXPR", "LOCATE_REGEXPR_AFTER", "CURR_TO_DECFLOAT_AMOUNT", "D16S", "D34S", "%_NATIVE_PRED", "MANY", "ONE", "CROSS", "SETS", "HAVING", "SOME", "EXISTS", "PRIMARY", "UP", "OFFSET", "%_HINTS", "INFORMIX", "%_NATIVE_END", "UNION", "INTERSECT", "INDICATORS", "BITFIELD", "VERSION", "DYNPRO", "DEMAND", "EDIT", "MASK", "KIND", "PAGES", "PAGE", "TITLE", "HEAD", "DETAIL", "DO", "TIMES", "VARYING", "DISPLAY", "BACKUP", "ELSEIF", "ENDAT", "ENDCASE", "ENDCATCH", "ENDCLASS", "ENDDO", "ENDEXEC", "ENDFORM", "%%INT", "%_INTERNAL", "ENDFUNCTION", "ENDIF", "ENDINTERFACE", "ENDLOOP", "ENDMETHOD", "ENDMODULE", "EDITING", "%%LAST", "TIME%%", "%%SECOND", "ENDON", "ENDPROVIDE", "ENDREP", "RESULTS", "ENDSELECT", "ENDWITH", "TEST", "INJECTION", "SEAM", "ENDTRY", "ENDWHILE", "ENDENHANCEMENT", "SPOTS", "STATIC", "SECTION", "SQL", "NAMETAB", "COMPRESSION", "OFF", "HINT", "INTERNAL", "EXEC", "EXTRACT", "FETCH", "EXTENDED", "PACKAGE", "SYMBOLS", "%DUMMY%", "%_FUNCPARM", "%%FUNCPARM", "FIND", "OCCURRENCES", "REGEX", "PATTERN", "SUBMATCHES", "FORM", "%_UPD", "FORMAT", "RESET", "INPUT", "INTENSIFIED", "INVERSE", "HOTSPOT", "COLOR", "COL_BACKGROUND", "COL_HEADING", "COL_NORMAL", "COL_TOTAL", "COL_KEY", "COL_POSITIVE", "COL_NEGATIVE", "COL_GROUP", "FRAMES", "$$UNIT$$", "$$GLOBAL", "HEADING", "DEFINING", "REDUCED", "FUNCTIONALITY", "RUN", "PROPERTY", "PF", "STATUS", "PROGRAM", "EXCLUDING", "LOCALE", "MODIFIER", "POSITION", "ATTRIBUTES", "FILTERS", "OPERATIONS", "WORD", "TRACE", "FILE", "%_AMDP", "AMDPDB", "RECOMPUTE", "STATEMENTS", "DIRECTORY", "ENTRY", "%_CHECK_SELSCREEN", "PRECOMPILED", "HEADERS", "SUBROUTINE", "NAME", "%_LOADSIZE", "HIDE", "IF", "VALUES", "DUPLICATE", "UNICODE", "ENABLING", "FIXED", "ARITHMETIC", "EXTENSION", "APPENDAGE", "MAXIMUM", "PADDING", "TRUNCATION", "BOUNDARIES", "CONVERSION", "ERRORS", "HEX", "ENDIAN", "MAJOR", "MINOR", "LOGFILE", "FOUND", "RENAMING", "SUFFIX", "INFOTYPES", "PERSON", "INITIALIZATION", "SUPPORTING", "INVOCATION", "INTERFACES", "PARTIALLY", "IMPLEMENTED", "PROCESSING", "RETURN", "%_INDEX_ONLY", "TRANSPORTING", "%_SCONT_%", "LOG", "MARK", "KERNEL", "%_METHOD_BODY", "RMC", "STUB", "GRAPH", "LLANG", "WORKSPACE", "%_OBJECT", "SUPPRESS", "DETERMINISTIC", "SCHEMA", "REDEFINITION", "FULL", "PRECHECK", "NUMBERING", "VALIDATE", "SAVE", "DETERMINE", "MINIMUM", "TRDIR", "AUTO", "FILL", "CID", "EXECUTE", "AUGMENTING", "RELATING", "?TO", "PERCENTAGE", "EXPANDING", "NESTED", "TARGET", "SCROLLING", "TOPOFPAGE", "COPIES", "SAP", "IMMEDIATELY", "RECEIVER", "IDENTIFICATION", "KEEP", "SPOOL", "EXPIRATION", "LAYOUT", "DEPARTMENT", "NODES", "ASSOCIATIONS", "REDIRECTED", "BYPASSING", "ENTRIES", "BINARY", "LEGACY", "BIG", "LITTLE", "SMART", "NATIVE", "UNIX", "WINDOWS", "LINEFEED", "ENCODING", "UTF", "SKIPPING", "OVERLAY", "PACK", "MATCHCODE", "MODIF", "OBLIGATORY", "CHECKBOX", "SEARCH", "VISIBLE", "LISTBOX", "NODE", "CPI", "LPI", "MARGIN", "FONT", "BLACK", "RED", "YELLOW", "GREEN", "BLUE", "PINK", "BOUNDS", "INCLUDING", "PUT", "RAISE", "RMC_INVALID_STATUS", "RMC_SYSTEM_FAILURE", "RMC_COMMUNICATION_FAILURE", "RANGES", "OCCURENCE", "OCCURENCES", "VERBATIM", "%_WITH", "ACTUAL", "FKEQ", "FKGE", "GKEQ", "GKGE", "REJECT", "REP", "REPEATING", "KERNELINFO", "RESERVE", "RESUME", "RETRY", "%_IMMEDIATELY", "SCAN", "ABAP", "TOKENS", "STRUCTURES", "ENHANCEMENTS", "KEYWORDS", "LEVELS", "OVERFLOW", "FRAME", "ANALYSIS", "COMMENTS", "INCLUDES", "EXPLICIT", "IMPLICIT", "INACTIVE", "IMPLEMENTATIONS", "TOKENIZATION", "POOLS", "PRAGMAS", "DECLARATIONS", "BLOCKS", "TRMAC", "REPLACING", "PRESERVING", "IDENTIFIER", "ESCAPING", "SCROLL", "COLUMN", "PLACES", "FORWARD", "BACKWARD", "ABBREVIATED", "SINGLE", "CREATING", "NOWAIT", "INTERVALS", "NESTING", "SUBSCREEN", "WINDOW", "EXCLUDE", "IDS", "ULINE", "POS_LOW", "POS_HIGH", "COMMENT", "DYNAMIC", "SELECTIONS", "TABBED", "PUSHBUTTON", "TAB", "OPTION", "BT", "NB", "E", "LOW", "HIGH", "TITLEBAR", "BLANK", "BOUNDARY", "ANALYZER", "CLOCK", "RESOLUTION", "HANDLER", "INSTANCES", "ACTIVATION", "%_INTFDESC", "LOCKS", "NAMES", "FLAGS", "SHIFT", "CIRCULAR", "DELETING", "LEADING", "TRAILING", "%_SORTMODE", "STABLE", "SPLIT", "STOP", "SUMMARY", "SUMMING", "SUBMIT", "JOB", "%_INTERNAL_%_SUBMODE_%", "INCL", "SUPPLY", "RELEASE", "SWITCHSTATES", "DCL_CTE_CHECK", "CODING", "EXPAND", "SYNCPOINTS", "DID", "SOLID", "PREPARE", "%_PREPARE", "COMPARE", "VERIFY", "HASHVAL", "PREVIOUS", "PROCESS", "WEBRFC", "GPA", "SSCR_HELP_INDEX", "SUBMODE", "RABAX", "ITAB_DELETE_LIST", "ITAB_INFO", "OPCODE", "PROTOCOL", "CLUSTER", "SNAP", "DESCRIPTION", "LISTLEVEL", "STACK", "MC_OBJECT", "KERNEL_INFO", "SET_KERNEL_INFO", "EXTENSIONS", "RFC_VALUE", "RFC_TABLE", "RFC_END", "SAMPLING", "SEQUENCE", "ADMINISTRATION", "NAVIGATION", "APPLIES", "ELEMENTARY", "OBJMGR", "TRACELEVEL", "NODELETE", "MS", "TRIGGERLEVEL", "GC", "STEPTIME", "OVERALLOCATION", "FRACTION", "GARBAGE", "COLLECTION", "CLONE", "EQUAL", "CDESTR", "NUM_HANDLERS", "INST", "STATE_LIMIT", "OTR", "GET_TEXT_BY_ALIAS", "NR", "GET_TEXT_BY_GUID", "GET_STRING_BY_ALIAS", "GET_STRING_BY_GUID", "NONEX", "CLAS", "ATTR", "WRAP", "SRCENC", "DSTENC", "IGNORE_CERR", "LEN", "POS", "CINFO", "AUX", "STORAGE", "COSTS", "SHARING", "REFERENCES", "WARNMSG_GET", "LITERAL_GET", "IS_OFFSET", "LITERAL_VALUE", "LITERAL_LEN", "RC", "DESTRUCTOR", "DBLOCK", "OS_PRIVATE_ATTRIBUTES", "INVALIDATE", "PLUGIN", "NONBLOCKING", "JAVA", "SCRIPT", "CONSTRUCTOR", "EVALUATE", "COMPILE", "DESTROY", "BIND", "BREAKPOINT", "BREAKPOINTS", "HTTP", "OMRT", "FREEZE", "THAW", "PROPERTIES", "SCOPE", "THIS", "EVAL", "UNSET", "DEBUGGER", "PATH", "SUSPEND", "SY", "%%INTERNAL%%", "DURING", "TRANSLATE", "TRUNCATE", "TRANSFER", "TRY", "%_NONE", "%_FINAL", "%_CHAR", "%_FLAT", "%_ANY", "MESH", "ENUM", "GAP", "UNPACK", "VERIFICATION", "RESPONSIBLE", "MAIL", "PRIORITY", "VERI", "%_EVENTS_ALL", "%_EVENTS", "MESSAGING", "CHANNELS", "ASYNCHRONOUS", "TASKS", "VARY", "WRITE", "DD/MM/YY", "DD/MM/YYYY", "MM/DD/YY", "MM/DD/YYYY", "DDMMYY", "MMDDYY", "YYMMDD", "JUSTIFIED", "CENTERED", "ICON", "UNDER", "QUICKINFO", "RSYN", "ENDCHAIN", "%_PADVERSION", "VERSION_201401127", "$Revision$", "%_PADVERSIONING", "VER_710_730", "VER_740", "%%unsupported" };
	}

   // Keyword collocations (created for version 7.56, possibly incomplete) are used for Token.collocationContinues, mainly for the CodeMetrics class
	private static String[] getKeywordCollocationsInCommands() {
	   return new String[] {"AT LINE-SELECTION", "AT PF", "AT SELECTION-SCREEN", "AT USER-COMMAND", "CALL BADI", "CALL CUSTOMER-FUNCTION", "CALL DATABASE PROCEDURE", "CALL DIALOG", "CALL FUNCTION", "CALL METHOD", "CALL SCREEN", "CALL SELECTION-SCREEN", "CALL TRANSACTION", "CALL TRANSFORMATION", "CASE TYPE OF", "CATCH SYSTEM-EXCEPTIONS", "CLOSE CURSOR", "CLOSE DATASET", "COMMIT CONNECTION", "CONVERT DATE", "CONVERT TEXT", "CONVERT TIME STAMP", "CONVERT UTCLONG", "CREATE DATA", "CREATE OBJECT", "CREATE OBJECT", "DATA BEGIN OF", "DELETE DATASET", "DELETE FROM", "DELETE itab", "DESCRIBE DISTANCE", "DESCRIBE FIELD", "DESCRIBE LIST", "DESCRIBE TABLE", "EXEC SQL", "EXIT FROM SQL", "EXIT FROM STEP-LOOP", "FREE MEMORY", "FREE OBJECT", "GENERATE SUBROUTINE POOL", "GET BADI", "GET BIT", "GET CURSOR", "GET DATASET", "GET LOCALE", "GET PARAMETER", "GET PF-STATUS", "GET PROPERTY", "GET REFERENCE", "GET RUN TIME", "GET TIME", "GET TIME STAMP", "IMPORT DIRECTORY", "INSERT REPORT", "INSERT TEXTPOOL", "LEAVE LIST-PROCESSING", "LEAVE PROGRAM", "LEAVE SCREEN", "LEAVE TO LIST-PROCESSING", "LEAVE TO TRANSACTION", "LOOP AT", "LOOP AT GROUP", "LOOP AT SCREEN", "MODIFY LINE", "MODIFY SCREEN", "MOVE PERCENTAGE", "ON CHANGE OF", "OPEN CURSOR", "OPEN DATASET", "RAISE EVENT", "RAISE EXCEPTION", "RAISE SHORTDUMP", "READ DATASET", "READ LINE", "READ REPORT", "READ TABLE", "READ TEXTPOOL", "REFRESH CONTROL", "ROLLBACK CONNECTION", "SELECTION-SCREEN DYNAMIC SELECTIONS", "SELECTION-SCREEN EXCLUDE", "SELECTION-SCREEN FIELD SELECTION", "SELECTION-SCREEN INCLUDE", "SELECTION-SCREEN TAB", "SET ASSOCIATION", "SET BIT", "SET BLANK LINES", "SET COUNTRY", "SET CURSOR", "SET DATASET", "SET EXTENDED CHECK", "SET HANDLER", "SET HOLD DATA", "SET LANGUAGE", "SET LEFT SCROLL-BOUNDARY", "SET LOCALE", "SET MARGIN", "SET PARAMETER", "SET PF-STATUS", "SET PROPERTY", "SET RUN TIME ANALYZER", "SET RUN TIME CLOCK", "SET SCREEN", "SET TITLEBAR", "SET UPDATE TASK", "SET USER-COMMAND", "SUPPRESS DIALOG", "TRUNCATE DATASET", "TYPES BEGIN OF", "TYPES BEGIN OF ENUM", "TYPES BEGIN OF MESH", "WRITE TO"};
	}
	private static String[] getKeywordCollocationsInAdditions() {
	   return new String[] {"ABSTRACT METHODS", "ACCEPTING DUPLICATE KEYS", "ACCEPTING PADDING", "ACCEPTING TRUNCATION", "ACCORDING TO", "ACTUAL LENGTH", "ADJACENT DUPLICATES FROM", "ALL COLUMNS", "ALL FIELDS", "ALL METHODS", "ALL OCCURRENCES", "ALL OTHER COLUMNS", "AND MARK", "AND RETURN", "AND RETURN TO SCREEN", "AND SKIP FIRST SCREEN", "AND WAIT", "ARCHIVE PARAMETERS", "AREA HANDLE", "AS CHECKBOX", "AS ICON", "AS LINE", "AS LISTBOX", "AS PERSON TABLE", "AS SEARCH PATTERN", "AS SEPARATE UNIT", "AS SUBSCREEN", "AS SYMBOL", "AS TEXT", "AS WINDOW", "ASSIGN LOCAL COPY", "AT END OF", "AT FIRST", "AT LAST", "AT NEW", "AT POSITION", "BACKUP INTO", "BEFORE UNWIND", "BEGIN OF", "BEGIN OF BLOCK", "BEGIN OF COMMON PART", "BEGIN OF ENUM", "BEGIN OF MESH", "BEGIN OF TABBED BLOCK", "BEGIN OF VERSION", "BIG ENDIAN", "BINARY SEARCH", "BLOB COLUMNS", "BYPASSING BUFFER", "CALL", "CLIENT SPECIFIED", "CLOB COLUMNS", "CODE PAGE", "CODE PAGE INTO", "COMPRESSION OFF", "COMPRESSION ON", "CORRESPONDING FIELDS OF", "CREATE FINAL", "CREATE PROTECTED", "CREATE PUBLIC", "CROSS JOIN", "CURRENT LINE", "CURRENT POSITION", "DATA BUFFER", "DATA VALUES", "DAYLIGHT SAVING TIME", "DEFAULT FAIL", "DEFAULT IGNORE", "DEFINING DATABASE", "DEFINITION DEFERRED", "DIRECTORY ENTRY", "DISPLAY LIKE", "DISPLAY OFFSET", "DURATION LONG", "DURATION MEDIUM", "DURATION SHORT", "DURING LINE-SELECTION", "EDIT MASK", "ENCODING DEFAULT", "ENCODING NON-UNICODE", "ENCODING UTF-8", "END OF", "END OF BLOCK", "END OF COMMON PART", "END OF ENUM", "END OF FILE", "END OF MESH", "END OF TABBED BLOCK", "END OF VERSION", "ENDIAN INTO", "ENDING AT", "ENVIRONMENT TIME FORMAT", "EXPANDING NESTED TABLES", "EXPORTING LIST TO MEMORY", "EXTENDED RESULT", "FIELD FORMAT", "FIELD VALUE", "FINAL METHODS", "FIRST OCCURRENCE", "FIXED-POINT ARITHMETIC", "FOR ALL ENTRIES IN", "FOR ALL INSTANCES", "FOR APPENDING", "FOR ASYNCHRONOUS TASKS UNTIL", "FOR FIELD", "FOR HIGH", "FOR INPUT", "FOR LOW", "FOR MESSAGING CHANNELS UNTIL", "FOR NODE", "FOR OUTPUT", "FOR PUSH CHANNELS UNTIL", "FOR TABLE", "FOR TABLE FUNCTION", "FOR TESTING", "FOR UPDATE", "FOR USER", "FRACTIONAL SECONDS", "FRAMES OFF", "FRAMES ON", "FROM CONTEXT", "FROM DATABASE", "FROM SCREEN", "FROM TABLE", "FUNCTION KEY", "GLOBAL FRIENDS", "GROUP BY", "GROUP INDEX", "GROUP SIZE", "HASHED TABLE OF", "HOTSPOT OFF", "HOTSPOT ON", "IF FOUND", "IGNORING CASE", "IGNORING CONVERSION ERRORS", "IGNORING STRUCTURE BOUNDARIES", "IN BACKGROUND TASK", "IN BACKGROUND UNIT", "IN BINARY MODE", "IN BYTE MODE", "IN CHARACTER MODE", "IN CHAR-TO-HEX MODE", "IN LEGACY BINARY MODE", "IN LEGACY TEXT MODE", "IN PROGRAM", "IN TEXT MODE", "IN UPDATE TASK", "INCLUDE BOUND", "INCLUDING GAPS", "INHERITING FROM", "INIT DESTINATION", "INITIAL LINE", "INITIAL LINE OF", "INITIAL SIZE", "INNER JOIN", "INPUT OFF", "INPUT ON", "INTENSIFIED OFF", "INTENSIFIED ON", "INTERNAL TABLE", "INTO CORRESPONDING FIELDS OF", "INVERSE OFF", "INVERSE ON", "IS ASSIGNED", "IS BOUND", "IS INITIAL", "IS INSTANCE OF", "IS NOT ASSIGNED", "IS NOT BOUND", "IS NOT INITIAL", "IS NOT INSTANCE OF", "IS NOT NULL", "IS NOT REQUESTED", "IS NOT SUPPLIED", "IS NULL", "IS REQUESTED", "IS SUPPLIED", "KEEPING DIRECTORY ENTRY", "KEEPING TARGET LINES", "KEEPING TASK", "LEAVE TO CURRENT TRANSACTION", "LEFT DELETING LEADING", "LEFT JOIN", "LEFT OUTER JOIN", "LEFT OUTER MANY TO ONE JOIN", "LINE FORMAT", "LINE OF", "LINE VALUE FROM", "LINE VALUE INTO", "LINES OF", "LITTLE ENDIAN", "LOB HANDLE", "LOB HANDLE FOR", "LOCATOR FOR", "LOWER CASE", "MATCH COUNT", "MATCH LENGTH", "MATCH LINE", "MATCH OFFSET", "MATCHCODE OBJECT", "MAXIMUM LENGTH", "MAXIMUM WIDTH INTO", "MEMORY ID", "MEMORY OFFSET", "MESSAGE INTO", "MESSAGES INTO", "MODIF ID", "NESTING LEVEL", "NO DATABASE SELECTION", "NO DIALOG", "NO END OF LINE", "NO FIELDS", "NO FLUSH", "NO INTERVALS", "NO STANDARD PAGE HEADING", "NOT BETWEEN", "NOT IN", "NOT LIKE", "NOT NULL STRUCTURE", "NULL STRUCTURE", "NUMBER OF LINES", "NUMBER OF PAGES", "OF CURRENT PAGE", "OF PAGE", "OF PROGRAM", "OF STRUCTURE", "ON BLOCK", "ON COMMIT", "ON COMMIT LEVEL", "ON END OF", "ON END OF TASK", "ON EXIT-COMMAND", "ON HELP-REQUEST", "ON RADIOBUTTON GROUP", "ON ROLLBACK", "ON VALUE-REQUEST", "ORDER BY", "ORDER BY PRIMARY KEY", "PACKAGE SIZE", "PARTIALLY IMPLEMENTED", "PRINT OFF", "PRINT ON", "PRIVATE SECTION", "PROGRAM TYPE", "PROTECTED SECTION", "PUBLIC SECTION", "RADIOBUTTON GROUP", "RADIOBUTTON GROUPS", "READER FOR", "RECEIVE BUFFER", "RECEIVE RESULTS FROM FUNCTION", "REDUCED FUNCTIONALITY", "REF TO", "REFERENCE INTO", "RENAMING WITH SUFFIX", "REPLACEMENT CHARACTER", "REPLACEMENT COUNT", "REPLACEMENT LENGTH", "REPLACEMENT LINE", "REPLACEMENT OFFSET", "RESOLUTION HIGH", "RESOLUTION LOW", "RESPECTING BLANKS", "RESPECTING CASE", "RIGHT DELETING TRAILING", "RIGHT JOIN", "RIGHT OUTER JOIN", "RIGHT OUTER MANY TO ONE JOIN", "RISK LEVEL CRITICAL", "RISK LEVEL DANGEROUS", "RISK LEVEL HARMLESS", "SEARCH FKEQ", "SEARCH FKGE", "SEARCH GKEQ", "SEARCH GKGE", "SECTION OF", "SEND BUFFER", "SEPARATED BY", "SHARED BUFFER", "SHARED MEMORY", "SHARED MEMORY ENABLED", "SKIPPING BYTE-ORDER MARK", "SORTABLE CODE", "SORTED BY", "SORTED TABLE OF", "STANDARD TABLE OF", "STARTING AT", "STARTING NEW TASK", "TABLE FIELD", "TABLE OF", "TABLEVIEW USING SCREEN", "TIME STAMP", "TIME ZONE", "TO COLUMNS", "TO CONTEXT", "TO FIRST PAGE", "TO LAST PAGE", "TO LINE", "TO LOWER CASE", "TO PAGE", "TO SAP-SPOOL SPOOL PARAMETERS", "TO SCREEN", "TO UPPER CASE", "TRANSPORTING NO FIELDS", "TYPE HANDLE", "UNICODE ENABLING", "UNION ALL SELECT", "UNION DISTINCT SELECT", "UP TO", "USING ALL CLIENTS", "USING CLIENT", "USING CLIENTS IN", "USING EDIT MASK", "USING KEY", "USING NO EDIT MASK", "USING SELECTION-SCREEN", "USING SELECTION-SET", "USING SELECTION-SETS OF PROGRAM", "VALID BETWEEN", "VALID FROM", "VALUE CHECK", "VIA SELECTION-SCREEN", "VISIBLE LENGTH", "WHEN OTHERS", "WHEN TYPE", "WITH AUTHORITY-CHECK", "WITH BYTE-ORDER MARK", "WITH CURRENT SWITCHSTATES", "WITH DEFAULT KEY", "WITH EMPTY KEY", "WITH FRAME", "WITH FRAME TITLE", "WITH FREE SELECTIONS", "WITH HEADER LINE", "WITH HOLD", "WITH KEY", "WITH NATIVE LINEFEED", "WITH NON-UNIQUE KEY", "WITH NON-UNIQUE SORTED KEY", "WITH NULL", "WITH PRIVILEGED ACCESS", "WITH SELECTION-TABLE", "WITH SMART LINEFEED", "WITH TABLE KEY", "WITH UNIQUE HASHED KEY", "WITH UNIQUE KEY", "WITH UNIQUE SORTED KEY", "WITH UNIX LINEFEED", "WITH WINDOWS LINEFEED", "WITHOUT AUTHORITY-CHECK", "WITHOUT MEMBERS", "WITHOUT SPOOL DYNPRO", "WRITER FOR"};
	}
	
   // -------------------------------------------------------------------------

	private static String getAbapKeywordKey(String abapKeyword) {
		return AbapCult.toUpper(abapKeyword);
	}

	private static HashSet<String> initializeAbapLowerCaseKeywords(String[] keywords) {
		HashSet<String> result = new HashSet<String>();
		for (String keyword : keywords)
			result.add(getAbapKeywordKey(keyword));
		return result;
	}

	private static HashSet<String> initializeAbapKeywords() {
		HashSet<String> result = new HashSet<String>();

		try {
			// open resource of latest ABAP grammar file (.pad file)
			PadResourceResolver padResourceResolver = new PadResourceResolver(); 
			InputStream padFileContent = padResourceResolver.getPadFileContent();
			if (padFileContent == null)
				throw new IOException();
			
			// cp. com.sap.rnd.rndrt.internal.ByteCode.read(...)
			BufferedReader grammarReader = new BufferedReader(new InputStreamReader(padFileContent));
			String line = "";
			int tokenNumOfIdToken = -1;
			int realTokenCount = -1;
			while (line != null) {
				line = grammarReader.readLine();
				if (line == null || line.length() == 0) 
					break;
				
				// determine the relevant token range from the header line that starts with "Token<TAB>", e.g. the header reads:
				// Release<TAB>700
				// Patchlevel<TAB>19
				// MaxSuspiciousMachtes<TAB>3
				// Token<TAB>111<TAB>1367<TAB>2920
				String[] cells = StringUtil.split(line, '\t', false);
				if (tokenNumOfIdToken < 0) {
					if (cells != null && cells.length > 2 && cells[0].equals("Token")) { 
						tokenNumOfIdToken = Integer.valueOf(cells[1]); // e.g.  111 - token number of the "#ID#" token, i.e. the last "#...#" token
						realTokenCount = Integer.valueOf(cells[2]);    // e.g. 1367 - later entries contain AST names and categories 
					}
					continue;
				} 
				
				// expected format of relevant lines is <integer>:<TAB>"<token>", e.g. the section after the header (which ends with an empty line) reads:
				// 0:<TAB>"#ANYKW#"
				// ...
				// 111:<TAB>"#ID#"
				// 112:<TAB>"BEGIN"
				// ...
				// 1366:<TAB>"%%unsupported"
				// 1367:<TAB>"^.[1545,7]"      [number of lines following with AST names, AST categories]
				// ...
				// 2919:<TAB>"^::s,77,78,79"
				//                             [empty line]
				if (cells == null || cells.length < 2 || cells[0].length() < 2 || cells[1].length() < 3) 
					continue;
				String tokenNumStr = cells[0];
				String tokenString = cells[1];
				if (!tokenNumStr.endsWith(":") || !tokenString.startsWith("\"") || !tokenString.endsWith("\"")) 
					continue;

				// determine token ID; skip irrelevant tokens, and stop after all 'real tokens' were read
				int tokenNum = Integer.valueOf(tokenNumStr.substring(0, tokenNumStr.length() - 1));
				if (tokenNum <= tokenNumOfIdToken) 
					continue;
				if (tokenNum >= realTokenCount)
					break;

				String keyword = tokenString.substring(1, tokenString.length() - 1);

				if (addAsAbapKeyword(keyword))
					result.add(getAbapKeywordKey(keyword));
			} 
			grammarReader.close();
			
		} catch (IOException e) {
			// use provided fallback keywords (taken from .pad file of ABAP version 7.57)
			String[] fallbackKeywords = getFallbackKeywords();
			for (String keyword : fallbackKeywords) {
				if (addAsAbapKeyword(keyword))
					result.add(getAbapKeywordKey(keyword));
			}
		}
		
		return result;
	}

	private static boolean addAsAbapKeyword(String keyword) {
		// add ABAP keyword to the result, unless it starts with or contains certain special characters, e.g. $$UNIT$$, %_UPD, %_LOADSIZE etc.
		if (StringUtil.isNullOrEmpty(keyword) || keyword.charAt(0) == '$' || keyword.charAt(0) == '%' || keyword.charAt(0) == '?')
			return false;
		if (keyword.indexOf('_') >= 0) // abap_bool, abap_false, abap_true, primary_key, table_line
			return false;

		// exclude keywords that are supposed to be written in lower(!) letters and are therefore not included
		if (abapLowerCaseKeywords.contains(getAbapKeywordKey(keyword)))
			return false;
		
		return true;
	}
	
	private static void initializeKeywordCollocations() {
		String[] keywordCollocationsInCommands = getKeywordCollocationsInCommands();
		String[] keywordCollocationsInAdditions = getKeywordCollocationsInAdditions();
	   abapKeywordCollocations = initializeAbapKeywordCollocations(keywordCollocationsInCommands, keywordCollocationsInAdditions);
	   abapKeywordCollocationStarts = initializeAbapKeywordCollocationStarts(keywordCollocationsInCommands, keywordCollocationsInAdditions);
	}

	private static HashSet<String> initializeAbapKeywordCollocations(String[] inCommands, String[] inAdditions) {
		HashSet<String> result = new HashSet<String>();
		addToHashSet(inCommands, result);
		addToHashSet(inAdditions, result);
		return result;
	}

	private static void addToHashSet(String[] keywords, HashSet<String> hashSet) {
		for (String keyword : keywords) {
			String key = getAbapKeywordKey(keyword);
			if (!hashSet.contains(key))
				hashSet.add(getAbapKeywordKey(key));
		}
	}

	private static HashSet<String> initializeAbapKeywordCollocationStarts(String[] inCommands, String[] inAdditions) {
		HashSet<String> result = new HashSet<String>();
		addToHashSetOfStarts(inCommands, result);
		addToHashSetOfStarts(inAdditions, result);
		return result;
	}

	private static void addToHashSetOfStarts(String[] keywords, HashSet<String> hashSet) {
		for (String keyword : keywords) {
			String key = getAbapKeywordKey(keyword);
			do {
				int spacePos = key.lastIndexOf(' ');
				if (spacePos <= 0)
					break;
				key = key.substring(0, spacePos);
				if (!hashSet.contains(key))
					hashSet.add(getAbapKeywordKey(key));
			} while (true);
		}
	}

	public static boolean isAbapUpperCaseKeyword(String text) {
		if (StringUtil.isNullOrEmpty(text))
			return false;

		// split into components - e.g. "TYPES:", "DATA(", "FIELD-SYMBOL(", including the approx. 120 keywords with hyphens 
		// like "MOVE-CORRESPONDING", "FIELD-SYMBOLS", "CLASS-DATA", etc.
		String[] textBits = StringUtil.split(text, new char[] { '-', ':', '(' }, true);
		if (textBits == null || textBits.length == 0)
			return false;
		for (String textBit : textBits) {
			if (!abapKeywords.contains(getAbapKeywordKey(textBit)))
				return false;
		}
		return true;
	}

	public static boolean isAbapLowerCaseKeyword(String text) {
		if (StringUtil.isNullOrEmpty(text))
			return false;

		// split into components, e.g. "sy-subrc":
		String[] textBits = StringUtil.split(text, new char[] { '-', ':', '(' }, true);
		for (String textBit : textBits) {
			if (!abapLowerCaseKeywords.contains(getAbapKeywordKey(textBit)))
				return false;
		}
		return true;
	}

	public static boolean isAbapKeywordCollocation(String text) {
		// lazy instantiation, including for the sake of test suite execution time
		if (abapKeywordCollocations == null)
			initializeKeywordCollocations();
		return !StringUtil.isNullOrEmpty(text) && abapKeywordCollocations.contains(getAbapKeywordKey(text));
	}

	public static boolean isAbapKeywordCollocationStart(String text) {
		// lazy instantiation, including for the sake of test suite execution time
		if (abapKeywordCollocationStarts == null)
			initializeKeywordCollocations();
		return !StringUtil.isNullOrEmpty(text) && abapKeywordCollocationStarts.contains(getAbapKeywordKey(text));
	}

	public static boolean isCharAllowedForVariableNames(char c) {
		return isCharAllowedForVariableNames(c, false, false);
	}

	public static boolean isCharAllowedForVariableNames(char c, boolean isFirstChar, boolean allowFieldSymbols) {
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == NAMESPACE_SIGN) {
			return true;
		
		} else if (!isFirstChar && (c >= '0' && c <= '9')) {
			return true;
		
		} else if (allowFieldSymbols && ((isFirstChar && c == '<') || c == '>')) {
			return true;
		
		} else {
			return false;
		}
	}

	public static boolean isCharAllowedForTypeNames(char c, boolean isFirstChar) {
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == NAMESPACE_SIGN) {
			return true;

		} else if (!isFirstChar) {
			// type names may contain =>, e.g. 'IF_ANY_INTERFACE=>TY_S_ANY_STRUCTURE'
			return (c >= '0' && c <= '9') || c == '=' || c == '>';
		
		} else {
			return false;
		}
	}

	public static boolean isCharCommonForKeywords(char c, boolean isFirstChar) {
		// checks against the most common chars in ABAP keywords
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (!isFirstChar && c == '-');
	}

	public static boolean isCharAllowedForAnyKeyword(char c) {
		return isCharAllowedForAnyKeyword(c, false);
	}

	public static boolean isCharAllowedForAnyKeyword(char c, boolean isFirstChar) {
		// checks against all chars possible in ABAP keywords, even rare ones like numbers (in "PF01") 
		// and the hyphen (in "FIELD-SYMBOLS", "MOVE-CORRESPONDING", "CLASS-DATA" etc.)
		if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
			return true;
		else if (!isFirstChar)
			return (c >= '0' && c <= '9') || c == '-';
		else
			return false;
	}

	public static boolean isCharAllowedForIntLiterals(char c, boolean isFirstChar) {
		return (c >= '0' && c <= '9') || (isFirstChar && c == '-');
	}

	public static boolean isHexValue(String hex) {
		if (hex == null)
			return false;
		if (hex.length() % 2 != 0)
			return false;
		char[] hexChars = hex.toCharArray();
		for (char c : hexChars) {
			if ((c < '0' || c > '9') && (c < 'A' || c > 'F'))
				return false;
		}
		return true;
	}

	public static boolean consistsOfDigitsOnly(String num) {
		if (num == null)
			return true;
		char[] numChars = num.toCharArray();
		for (char c : numChars) {
			if (c < '0' || c > '9')
				return false;
		}
		return true;
	}

	public static boolean consistsOfDigitsOnlyWithNoLeading0(String num) {
		if (!consistsOfDigitsOnly(num))
			return false;
		if (StringUtil.isNullOrEmpty(num))
			return false;
		return (num.length() == 1) || (num.charAt(0) != '0');
	}

	public static boolean isInteger(String num) {
		if (num == null)
			return false;
		String numWithoutMinus = AbapCult.stringStartsWith(num, "-") ? num.substring("-".length()) : num;
		return consistsOfDigitsOnlyWithNoLeading0(numWithoutMinus);
	}

	public static String negateInteger(String num) {
		if (!isInteger(num))
			return null;
		return AbapCult.stringStartsWith(num, "-") ? num.substring("-".length()) : "-" + num;
	}

	public static boolean isAssignmentOperator(String text) {
		switch (AbapCult.toUpper(text)) {
			case "=":
			case "?=":
			case "+=":
			case "-=":
			case "*=":
			case "/=":
			case "&&=":
				return true;
			default:
				return false;
		}
	}

	public static String getSymbolicComparisonOperator(String text) {
		switch (AbapCult.toUpper(text)) {
			case "LT":
				return "<";
			case "LE":
				return "<=";
			case "EQ":
				return "=";
			case "GE":
				return ">=";
			case "GT":
				return ">";
			case "NE":
				return "<>";
			default:
				return text;
		}
	}

	public static String getTextualComparisonOperator(String text) {
		switch (text) {
			case "<":
				return "LT";
			case "<=":
				return "LE";
			case "=":
				return "EQ";
			case ">=":
				return "GE";
			case ">":
				return "GT";
			case "<>":
				return "NE";
			default:
				return text;
		}
	}

	public static boolean isComparisonOperator(String text) {
		switch (AbapCult.toUpper(text)) {
			// Comparison operators for all data types
			case "<":
			case "<=": // and obsolete =<
			case "=":
			case ">=": // and obsolete =>
			case ">":
			case "<>": // and obsolete ><

			case "LT":
			case "LE":
			case "EQ":
			case "GE":
			case "GT":
			case "NE":

			case "IN": // e.g. WHERE ... [NOT] IN ... (e.g. for range tables)
			case "BETWEEN": // ternary comparison operator ... [NOT] BETWEEN ... AND ...
				return true;

			// Comparison operators for character-like data types
			case "CO": // Contains Only
			case "CN": // Contains Not Only
			case "CA": // Contains Any
			case "NA": // Contains Not Any
			case "CS": // Contains String
			case "NS": // Contains No String
			case "CP": // Covers Pattern
			case "NP": // No Pattern
				return true;

			// Comparison operators for byte-like data types
			case "BYTE-CO":
			case "BYTE-CN":
			case "BYTE-CA":
			case "BYTE-NA":
			case "BYTE-CS":
			case "BYTE-NS":
				return true;

			// TODO: Comparison operators for bit patterns (O, Z, M) are missing,
			// see https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abenlogexp_op.htm

			default:
				return false;
		}
	}

	public static String negateComparisonOperator(String text) {
		switch (AbapCult.toUpper(text)) {
			// Comparison operators for all data types
			case "<":
			case "LT":
				return ">=";

			case "<=":
			case "=<": // obsolete
			case "LE":
				return ">";

			case "=":
			case "EQ":
				return "<>";

			case ">=":
			case "=>": // obsolete
			case "GE":
				return "<";

			case ">":
			case "GT":
				return "<=";

			case "<>":
			case "><": // obsolete
			case "NE":
				return "=";

			// Comparison operators for character-like data types
			case "CO": // Contains Only
				return "CN";
			case "CN": // Contains Not Only
				return "CO";

			case "CA": // Contains Any
				return "NA";
			case "NA": // Contains Not Any
				return "CA";

			case "CS": // Contains String
				return "NS";
			case "NS": // Contains No String
				return "CS";

			case "CP": // Covers Pattern
				return "NP";
			case "NP": // No Pattern
				return "CP";

			// Comparison operators for byte-like data types
			case "BYTE-CO":
				return "BYTE-CN";
			case "BYTE-CN":
				return "BYTE-CO";

			case "BYTE-CA":
				return "BYTE-NA";
			case "BYTE-NA":
				return "BYTE-CA";

			case "BYTE-CS":
				return "BYTE-NS";
			case "BYTE-NS":
				return "BYTE-CS";

			case "IN":
			case "BETWEEN":
				throw new IndexOutOfBoundsException("comparison operator '" + text + "' cannot be processed here, since it must be negated with the help of NOT");

			default:
				throw new IndexOutOfBoundsException("unknown comparison operator " + text);
		}
	}

	public static boolean isNumeric(String num) {
		return isNumeric(num, true, true);
	}

	public static boolean isNumeric(String num, boolean allowPlusOrMinusAtEnd, boolean allowDot) {
		if (num == null || num.length() == 0)
			return false;

		// allow minus or plus sign at start (and possibly at the end, but not both)
		int start = AbapCult.stringStartsWithAny(num, "-", "+") ? 1 : 0;
		int end = allowPlusOrMinusAtEnd && (start == 0) && AbapCult.stringEndsWithAny(num, "-", "+") ? num.length() - 1 : num.length();
		if (start == end)
			return false;
		
		boolean foundDot = false;
		for (int i = start; i < end; ++i) {
			char c = num.charAt(i);
			if (allowDot && c == DOT_SIGN) {
				// allow dot at one place
				if (foundDot)
					return false;
				foundDot = true;
			} else if (c < '0' || c > '9') {
				return false;
			}
		}
		return true;
	}

	/**
	 * returns the index of the last digit before the decimal separator, e.g. the index of the '3' in   
	 * integer or float literals such as 3, 123, -123, '123-', '3.14', '3.14-', '-3.14'  
	 * @param num - an integer or float numeric literal 
	 * @return the 0-based index of the last digit before the decimal separator
	 */
	public static int getNumericAlignmentIndex(String num) {
		int dotPos = num.indexOf(DOT_SIGN);
		if (dotPos >= 0)
			return dotPos - 1;
		for (int i = num.length() - 1; i >= 0; --i) {
			char c = num.charAt(i); 
			if (c != '-' && c != '\'')
				return i;
		}
		return num.length();
	}
	
	public static boolean isPositionAndLength(String text) {
		if (text == null || text.length() < 4)
			return false;
		if (!text.endsWith(")"))
			return false;
		int parPos = text.indexOf('(');
		if (parPos < 1)
			return false;
		// leading zeros are allowed, e.g. 'WRITE AT 001(005) ...'
		return consistsOfDigitsOnly(text.substring(0, parPos)) && consistsOfDigitsOnly(text.substring(parPos + 1, text.length() - 1));
	}
	
	public static String toVariableName(String name) {
		if (name == null)
			return "";
		StringBuilder varName = new StringBuilder(name.length());
		boolean isFirstChar = true;
		char[] nameChars = name.toCharArray();
		for (char c : nameChars) {
			varName.append(isCharAllowedForVariableNames(c, isFirstChar, false) ? c : '_');
			isFirstChar = false;
			if (varName.length() >= MAX_VARIABLE_NAME_LENGTH)
				break;
		}
		return AbapCult.toLower(varName.toString());
	}

	public static boolean mayBeVariableName(String name, boolean allowFieldSymbols) {
		if (name == null || name.length() == 0)
			return false;
		if (name.length() > MAX_VARIABLE_NAME_LENGTH)
			return false;
		
		// if the identifier starts with <, it must also end with > (and vice versa)
		boolean startsLikeFieldSymbol = (name.charAt(0) == FIELD_SYMBOL_START_SIGN); 
		boolean endsLikeFieldSymbol = (name.charAt(name.length() - 1) == FIELD_SYMBOL_END_SIGN); 
		if (startsLikeFieldSymbol != endsLikeFieldSymbol) {
			return false;
		}
		
		int pos = 0;
		boolean isFirstChar = true;
		int namespaceLength = 0; // is set to > 0 while reading a namespace '/.../' within the identifier
		while (pos < name.length()) {
			char c = name.charAt(pos);
			if (!isCharAllowedForVariableNames(c, isFirstChar, allowFieldSymbols))
				return false;
			
			isFirstChar = false;

			// ensure that in FIELD-SYMBOLS, < is only found at the beginning and > only at the end
			if ((c == FIELD_SYMBOL_START_SIGN && pos > 0) || (c == FIELD_SYMBOL_END_SIGN && pos != name.length() - 1))
				return false;
			
			// if the identifier contains (one or several) namespaces, check their validity: 
			// - '/' is allowed for variables, structures, components, methods etc., but never anywhere in FIELD-SYMBOLS  
			// - for each opening '/', there must be a closing '/', i.e. they must come in pairs
			// - inside each pair, at least 3 more characters a-z, A-Z, 0-9, _ must be found
			// - several such pairs are allowed
			// - there may be >= 0 other characters a-z, A-Z, 0-9, _ BETWEEN pairs
			// - an opening '/' may be at the start or within the identifier
			// - a closing '/' may be within the identifier or at its end
			// - if the identifier starts with /, the next character may also be a number 0-9
			if (c == NAMESPACE_SIGN) {
				if (startsLikeFieldSymbol) {
					return false; // never allowed in FIELD-SYMBOLS
				}
				if (namespaceLength == 0) {
					// opening '/' of a namespace found; we count this as the first character: 
					namespaceLength = 1;
				} else if (namespaceLength > 3) {
					// closing '/' of a valid namespace found
					namespaceLength = 0; 
				} else {
					// namespace was closed too soon: at least 3 more characters are expected inside /.../
					return false;
				}
			} else if (namespaceLength > 0) {
				// continue counting the characters that belong to the current namespace
				++namespaceLength;
			}

			++pos;
		}
		
		// if namespaceLength is still > 0, an uneven number of '/' was found and this is NOT a valid identifier 
		return (pos == name.length()) && namespaceLength == 0;
	}
	
	/**
	 * identifies the end of a variable name and returns the variable name; expects start position to be behind any @ or ! escape char
	 * @param line - a code line in which the variable name is found
	 * @param start - the start position, which is expected to be behind any @ or ! escape character
	 * @param allowFieldSymbols - true if field symbols are allowed as variables
	 * @return - the variable name that was identified, or null if start position exceeded line length
	 */
	public static String readTillEndOfVariableName(String line, int start, boolean allowFieldSymbols) {
		if (line == null)
			return null;
		if (start >= line.length())
			return null;
		boolean isFirstChar = true;
		int end = start;
		while (end < line.length() && isCharAllowedForVariableNames(line.charAt(end), isFirstChar, allowFieldSymbols)) {
			isFirstChar = false;
			++end;
		}
		return line.substring(start, end);
	}

	public static String readTillEndOfTypeName(String line, int start) {
		int end = start;
		if (line == null)
			return null;
		if (start >= line.length())
			return null;
		boolean isFirstChar = true;
		while (end < line.length() && isCharAllowedForTypeNames(line.charAt(end), isFirstChar)) {
			isFirstChar = false;
			++end;
		}
		return line.substring(start, end);
	}

	public static String inferFileNameFromCode(String code) {
		String fileName = inferRawFileNameFromCode(code);
		if (fileName != null) {
			fileName = fileName.trim();
			if (StringUtil.endsWith(fileName, ".", false))
				fileName = fileName.substring(0, fileName.length() - 1);
		}
		return fileName;
	}

	private static String inferRawFileNameFromCode(String code) {
		final int considerCharCount = 4096; // we expect the name to come up in the first few lines of the code

		String[] commentKeywords = new String[] { "Include", "Report", "Modulpool" };
		String[] mainKeywords = new String[] { "CLASS", "INTERFACE", "REPORT", "FUNCTION", "PROGRAM" };

		code = code.substring(0, Math.min(considerCharCount, code.length()));
		code = code.replaceAll("\r\n", "\n");
		String[] lines = StringUtil.split(code, '\n', true);

		for (String line : lines) {
			String lineCopy = line;
			if (StringUtil.startsWith(lineCopy,  "***INCLUDE", true))
				lineCopy = "*** INCLUDE" + lineCopy.substring("***INCLUDE".length());
			String[] words = StringUtil.split(lineCopy.trim(), ' ', true);
			if (words == null || words.length < 3)
				continue;

			if ((words[0].startsWith(ABAP.COMMENT_SIGN_STRING) || words[0].startsWith(ABAP.LINE_COMMENT_SIGN_STRING))) {
				for (String commentKeyword : commentKeywords) {
					if (AbapCult.stringEquals(words[1], commentKeyword, true))
						return words[2];
				}
			}

			for (String mainKeyword : mainKeywords) {
				if (AbapCult.stringEquals(words[0], mainKeyword, true))
					return words[1];
			}
		}
		return null;
	}
	
	public static int getReleaseRestrictionNumber(String releaseRestrictionName) {
		if (releaseRestrictionName.equals(ABAP.NO_RELEASE_RESTRICTION_NAME)) {
			return ABAP.NO_RELEASE_RESTRICTION;
		} else {
			try {
				return Integer.valueOf(releaseRestrictionName.replace(".", ""));
			} catch (NumberFormatException ex) {
				return -1;
			}
		}
	}

	public static String getReleaseRestrictionName(int releaseRestriction) {
		if (releaseRestriction == ABAP.NO_RELEASE_RESTRICTION) {
			return ABAP.NO_RELEASE_RESTRICTION_NAME;
		} else {
			for (String releaseRestrictionName : RELEASE_RESTRICTION_NAMES) {
				if (getReleaseRestrictionNumber(releaseRestrictionName) == releaseRestriction) {
					return releaseRestrictionName;
				}
			}
			// fall back to "no release restriction"
			return ABAP.NO_RELEASE_RESTRICTION_NAME;
		}
	}

	/** splits composed identifiers such as "any_class=>any_structure-any_component", or "any_class=>any_method(", 
	 * or "any_interface~any_method" etc. into an array of identifiers, e.g. ["any_class", "=>", "any_structure", "-", "any_component"] 
	 * or ["any_class", "=>", "any_method", "("] etc. */
	public static ArrayList<String> splitIdentifier(String identifier) {
		ArrayList<String> results = new ArrayList<String>();
		int start = 0;
		while (start < identifier.length()) {
			boolean isVarName = isCharAllowedForVariableNames(identifier.charAt(start), true, false); 
			int end = start + 1;
			while (end < identifier.length() && (isVarName == isCharAllowedForVariableNames(identifier.charAt(end), false, false))) { 
				++end;
			}
			results.add(identifier.substring(start, end));
			start = end;
		}
		return results;
	}

	public static boolean isFieldSymbol(String identifier) { 
		return (identifier != null && identifier.length() >= 2 && identifier.charAt(0) == ABAP.FIELD_SYMBOL_START_SIGN 
				&& identifier.charAt(identifier.length() - 1) == ABAP.FIELD_SYMBOL_END_SIGN);
	}

	public static String getPragmaWithoutParameters(String text) {
		int brackPos = text.indexOf(PRAGMA_PARAMETER_BRACKET_OPEN);
		return (brackPos < 0) ? text : text.substring(0, brackPos);  
	}
	
	public static String getPragmaParameters(String text) {
		int brackPos = text.indexOf(PRAGMA_PARAMETER_BRACKET_OPEN);
		return (brackPos < 0) ? "" : text.substring(brackPos);  
	}

	/** unescapes literals of type '...' (text field literal), `...` (text string literal) or |...| (string template) */
	public static String unescapeCharLiteral(String text) {
		if (text.length() < 2)
			return null;
		
		String innerText = text.substring(1, text.length() - 1); 
		if (text.startsWith(QUOT_MARK_STRING))
			return innerText.replaceAll(QUOT_MARK_STRING + QUOT_MARK_STRING, QUOT_MARK_STRING);
		else if (text.startsWith(QUOT_MARK2_STRING))
			return innerText.replaceAll(QUOT_MARK2_STRING + QUOT_MARK2_STRING, QUOT_MARK2_STRING);
		else
			return StringUtil.getUnescapedText(innerText);
	}

	/** returns the text field literal '...' for the supplied inner text; e.g. for ab'c, returns 'ab''c' */
	public static String toTextFieldLiteral(String innerText) {
		String textDelimited = innerText.replaceAll(QUOT_MARK_STRING, QUOT_MARK_STRING + QUOT_MARK_STRING);
		return QUOT_MARK_STRING + textDelimited + QUOT_MARK_STRING;
	}

	/** returns the text string literal `...` for the supplied inner text; e.g. for ab`c, returns `ab``c` */
	public static String toTextStringLiteral(String innerText) {
		String textDelimited = innerText.replaceAll(QUOT_MARK2_STRING, QUOT_MARK2_STRING + QUOT_MARK2_STRING);
		return QUOT_MARK2_STRING + textDelimited + QUOT_MARK2_STRING;
	}
	
	public static boolean mayBeTextSymbol(String tokenText) {
		return (tokenText.length() == TEXT_SYMBOL_PREFIX.length() + TEXT_SYMBOL_ID_LENGTH) && AbapCult.stringEquals(tokenText.substring(0, TEXT_SYMBOL_PREFIX.length()), TEXT_SYMBOL_PREFIX, true);
	}
}
