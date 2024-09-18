package com.sap.adt.abapcleaner.parser;

import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public abstract class Obfuscator {
	protected enum IdentifierType {
		TYPE("type", "ty"),
		INTERFACE("interface", "if"),
		CLASS("class", "cl"),
		EXCEPTION_CLASS("exception", "cx"),

		ATTRIBUTE("attribute", "m"),
		METHOD("method", "meth"),
		PARAMETER("param", "p"),
		CONSTANT("constant", "co"),
		VALUE("value", "v"),
		RESULT("result", "r"),
		TABLE("table", "t"),
		STRUCTURE("struc", "s"),
		COMPONENT("comp", "c"),
		INSTANCE("instance", "o"),
		REF("ref", "r"),
		EXCEPTION("exception", "x"),

		DTAB("dtab", "dtab"),
		TABALIAS("tabalias", "t"),
		COL("col", "c"),

		DDL_ENTITY("View", "V"),
		DDL_PARAMETER("Parameter", "P"),
		DDL_TYPE("Type", "t"),
		DDL_DATA_SOURCE("DataSource", "D"),
		DDL_SOURCE_ALIAS("Alias", "A"),
		DDL_FIELD("Field", "F");

		private final String infix;
		private final String shortInfix;
		
		private IdentifierType(String infix, String shortInfix) {
			this.infix = infix;
			this.shortInfix = shortInfix; 
		}
	}

	private String[] names = new String[] { "any", "other", "third", "fourth", "fifth" };

	protected final static String getKey(String identifier) {
		return identifier.toUpperCase();
	}

	public abstract void obfuscate(Code code) throws UnexpectedSyntaxAfterChanges;

	public static Obfuscator createFor(Language language, boolean commandScope, boolean createShortNames, boolean simplify, boolean removeLineEndComments, boolean removeCommentLines, boolean obfuscateLiterals) {
		if (language == Language.DDL || language == Language.DCL) {
			return new DdlObfuscator(commandScope, createShortNames, simplify, removeLineEndComments, removeCommentLines, obfuscateLiterals);
		} else {
			return new AbapObfuscator(commandScope, createShortNames, simplify, removeLineEndComments, removeCommentLines, obfuscateLiterals);
		}
	}

	// -------------------------------------------------------------------------
	
	protected final Language language;
	protected final boolean commandScope;
	protected final boolean createShortNames;
	protected final boolean simplify;
	protected final boolean removeLineEndComments;
	protected final boolean removeCommentLines;
	protected final boolean obfuscateLiterals;
	protected final boolean camelCaseStyle;
	
	protected HashMap<String, String> stringLiterals = new HashMap<>();
	protected HashMap<String, String> integerLiterals = new HashMap<>();

	protected Obfuscator(Language language, boolean commandScope, boolean createShortNames, boolean simplify, boolean removeLineEndComments, boolean removeCommentLines, boolean obfuscateLiterals) {
		this.language = language;
		this.commandScope = commandScope;
		this.createShortNames = createShortNames;
		this.simplify = simplify;
		this.removeLineEndComments = removeLineEndComments;
		this.removeCommentLines = removeCommentLines;
		this.obfuscateLiterals = obfuscateLiterals;
		this.camelCaseStyle = (language == Language.DDL || language == Language.DCL);
		
		if (camelCaseStyle) {
			// in DDL, long names are common, therefore use "Any", "Other", "Third", ..., "NinetyNinth"
			String[] firstTwenty = new String[] { "Any", "Other", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth", "Eleventh", "Twelfth", "Thirteenth", "Fourteenth", "Fifteenth", "Sixteenth", "Seventeenth", "Eighteenth", "Nineteenth" };
			String[] numberSuffixes = new String[] { "First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth" };
			String[] twentyToNinety = new String[] { "Twenty", "Thirty", "Fourty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety" };

			names = new String[99];
			int index = 0;
			for (String number : firstTwenty)
				names[index++] = number;
			for (String tens : twentyToNinety) {
				names[index++] = tens.replace("ty", "tieth");
				for (String numberSuffix : numberSuffixes) {
					names[index++] = tens + numberSuffix;
				}
			}
		}
	}

	public Code obfuscate(String codeText) throws ParseException, UnexpectedSyntaxAfterChanges {
   	Code code = Code.parse(null, ParseParams.createForWholeCode("", codeText, ABAP.NEWEST_RELEASE));
   	obfuscate(code);
   	return code;
	}

	protected void clearLiterals() {
		stringLiterals.clear();
		stringLiterals.put(" ", " ");
		stringLiterals.put("X", "X");
		stringLiterals.put("0", "0");
		stringLiterals.put("0.00", "0");
		stringLiterals.put("1", "1");

		integerLiterals.clear();
		integerLiterals.put("0", "0");
		integerLiterals.put("1", "1");
	}

	protected Command removeCommentLine(Command comment) throws IntegrityBrokenException {
		Command next = comment.getNext();
		try {
			if (next != null && next.getFirstTokenLineBreaks() < comment.getFirstTokenLineBreaks())
				next.getFirstToken().lineBreaks = comment.getFirstTokenLineBreaks();
			comment.removeFromCode();
		} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
			throw new IntegrityBrokenException(comment, e.getMessage());
		}
		return next;
	}

	protected String getNewNameFor(String key, HashMap<String, String> map, String prefix, IdentifierType identifierType) {
		return  getNewNameFor(key, map, prefix, identifierType, this.camelCaseStyle);
	}
	
	protected String getNewNameFor(String key, HashMap<String, String> map, String prefix, IdentifierType identifierType, boolean useCamelCaseStyle) {
		if (map.containsKey(key)) {
			return map.get(key);
		}

		int number;
		String numberPrefix;
		
		if (createShortNames || (language == Language.ABAP && identifierType == IdentifierType.TABALIAS)) {
			String shortInfix = identifierType.shortInfix;
			numberPrefix = useCamelCaseStyle ? StringUtil.capitalizeStart(shortInfix) : shortInfix.toLowerCase();
			number = 1;

		} else {
			// depending on the requested style (which even in DDL may be snail_case for lower case database table names and fields),
			// use appropriate prefix, infix and separator 
			String usePrefix = useCamelCaseStyle ? StringUtil.capitalizeStart(prefix) : prefix.toLowerCase();
			String useInfix = useCamelCaseStyle ? StringUtil.capitalizeStart(identifierType.infix) : identifierType.infix.toLowerCase();
			String useSeparator = useCamelCaseStyle ? "" : "_";
			
			for (String name : names) {
				String useName = useCamelCaseStyle ? StringUtil.capitalizeStart(name) : name.toLowerCase();
				String newName = usePrefix + useName + (StringUtil.isNullOrEmpty(useInfix) ? "" : useSeparator + useInfix);
				if (!map.containsValue(newName)) {
					map.put(key, newName);
					return newName;
				}
			}
			number = names.length + 1;
			numberPrefix = usePrefix + (StringUtil.isNullOrEmpty(useInfix) ? "" : useInfix + useSeparator);
		}

		do {
			String newName = numberPrefix + String.valueOf(number);
			if (!map.containsValue(newName)) {
				map.put(key, newName);
				return newName;
			}
			++number;
		} while(true);
	}
	
	protected String getNewStringLiteral(String oldText) {
		int lastCharPos = oldText.length() - 1;
		String quotStart = oldText.substring(0, 1);
		String text = oldText.substring(1, lastCharPos);
		String quotEnd = oldText.substring(lastCharPos, oldText.length());

		if (stringLiterals.containsKey(text)) {
			return quotStart + stringLiterals.get(text) + quotEnd;
		}

		String newText;
		String prefix = ABAP.isNumeric(text, true, true) ? "" : "T";
		int number = 1;
		do {
			newText = prefix + String.valueOf(number);
			if (!stringLiterals.containsValue(newText)) {
				break;
			}
			++number;
		} while(true);
		stringLiterals.put(text, newText);

		return quotStart + newText + quotEnd;
	}
	
	protected String getNewIntegerLiteral(String oldText) {
		if (integerLiterals.containsKey(oldText)) {
			return integerLiterals.get(oldText);
		}

		String newText;
		int number = 1;
		do {
			newText = String.valueOf(number);
			if (!integerLiterals.containsValue(newText)) {
				break;
			}
			++number;
		} while(true);
		integerLiterals.put(oldText, newText);

		return newText;
	}
}
