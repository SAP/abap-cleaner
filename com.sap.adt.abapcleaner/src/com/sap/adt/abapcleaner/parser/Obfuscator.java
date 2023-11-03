package com.sap.adt.abapcleaner.parser;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

/** Obfuscates identifiers (variable names, method names etc.) in the supplied Code to help transforming real-life  
 * code samples into example or test code, which can then be used as open-source. With activated developer features, 
 * obfuscation can be triggered by FrmProfiles.btnObfuscate to execute it on any (previously pasted) example code. 
 * Results must be manually reviewed and reworked. */
public class Obfuscator {
	private final static String[] varPrefixes = new String[] { 
			"lv_", "lc_", "ls_", "lt_", "lts_", "lth_", "lo_", "lr_", "lx_",
			"iv_", "ic_", "is_", "it_", "its_", "ith_", "io_", "ir_",
			"ev_", "ec_", "es_", "et_", "ets_", "eth_", "eo_", "er_",
			"cv_", "cc_", "cs_", "ct_", "cts_", "cth_", "co_", "cr_",
			"rv_", "rc_", "rs_", "rt_", "rts_", "rth_", "ro_", "rr_",
			"mv_", "mc_", "ms_", "mt_", "mts_", "mth_", "mo_", "mr_",
			"gv_", "gc_", "gs_", "gt_", "gts_", "gth_", "go_", "gr_" };

	private final static String[] typePrefixes = new String[] { 
			"cl_", "cx_", "lcl_", "if_", "lif_" };
	
	private final static String[] names = new String[] { "any", "other", "third", "fourth", "fifth" };

	private enum IdentifierType {
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
		EXCEPTION("exception", "x");
		
		public final String infix;
		public final String shortInfix;
		
		private IdentifierType(String infix, String shortInfix) {
			this.infix = infix;
			this.shortInfix = shortInfix; 
		}
	}

	// -------------------------------------------------------------------------
	
	private final boolean methodScope;
	private final boolean createShortNames;
	private final boolean simplify;
	private final boolean removeLineEndComments;
	private final boolean removeCommentLines;
	private final boolean obfuscateLiterals;
	
	private HashMap<String, String> types = new HashMap<>();
	private HashMap<String, String> variables = new HashMap<>();
	private HashMap<String, String> components = new HashMap<>();
	private HashMap<String, String> classes = new HashMap<>();
	private HashMap<String, String> methods = new HashMap<>();
	private HashMap<String, String> stringLiterals = new HashMap<>();
	private HashMap<String, String> integerLiterals = new HashMap<>();

	public Obfuscator(boolean methodScope, boolean createShortNames, boolean simplify, boolean removeLineEndComments, boolean removeCommentLines, boolean obfuscateLiterals) {
		this.methodScope = methodScope;
		this.createShortNames = createShortNames;
		this.simplify = simplify;
		this.removeLineEndComments = removeLineEndComments;
		this.removeCommentLines = removeCommentLines;
		this.obfuscateLiterals = obfuscateLiterals;
	}

	private void clearMaps(boolean clearMethods, boolean clearClasses) {
		types.clear();
		variables.clear();
		components.clear();
		
		stringLiterals.clear();
		stringLiterals.put(" ", " ");
		stringLiterals.put("X", "X");
		stringLiterals.put("0", "0");
		stringLiterals.put("0.00", "0");
		stringLiterals.put("1", "1");

		integerLiterals.clear();
		integerLiterals.put("0", "0");
		integerLiterals.put("1", "1");

		if (clearMethods) {
			methods.clear();
		}
		if (clearClasses) {
			classes.clear();
		}
	}
	
	public Code obfuscate(String codeText) throws ParseException, UnexpectedSyntaxAfterChanges {
   	Code code = Code.parse(null, ParseParams.createForWholeCode("", codeText, ABAP.NEWEST_RELEASE));
   	obfuscate(code);
   	return code;
	}
	
	public void obfuscate(Code code) throws UnexpectedSyntaxAfterChanges {
		clearMaps(true, true);
		
		Command command = code.firstCommand;
		while (command != null) {
			if (command.isClassDefinitionStart())
				clearMaps(true, false);
			else if (command.isMethodFunctionFormOrEventBlockStart())
				clearMaps(false, false);

			if (removeCommentLines && command.isCommentLine()) {
				Command next = command.getNext();
				try {
					if (next != null && next.getFirstTokenLineBreaks() < command.getFirstTokenLineBreaks())
						next.getFirstToken().lineBreaks = command.getFirstTokenLineBreaks();
					command.removeFromCode();
				} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
					throw new IntegrityBrokenException(command, e.getMessage());
				}
				command = next;
			} else {
				obfuscate(command);
				command = command.getNext();
			}
		}
	}
	
	private void obfuscate(Command command) throws UnexpectedSyntaxAfterChanges {
		if (!methodScope) {
			// obfuscate every single Command individually, 'forgetting' all identifiers used so far
			clearMaps(true, true);
		}
		
		Token token = command.getFirstToken();
		boolean condenseTillLineEnd = false;
		while (token != null) {
			// any Tokens following a changed identifier on the same line will be condensed 
			if (token.lineBreaks > 0)
				condenseTillLineEnd = false;
			else if (condenseTillLineEnd && !token.isAttached())
				token.setWhitespace();

			// if configured, remove comments within the Command
			if (removeCommentLines && token.isCommentLine() && token != command.getFirstToken()) {
				Token next = token.getNext();
				if (next != null && next.lineBreaks < token.lineBreaks)
					next.lineBreaks = token.lineBreaks;
				token.removeFromCommand();
				token = next;
				continue;
			} else if (removeLineEndComments && token.isCommentAfterCode()) {
				Token next = token.getNext();
				token.removeFromCommand();
				token = next;
				continue;
			}
			
			String oldText = token.getText();

			if (token.isQuotMarkComment()) {
				if (token.textStartsWith(ABAP.PSEUDO_COMMENT_EC_PREFIX)) {
					// keep "#EC pseudo comment
				} else if (token.textStartsWith(ABAP.DYNAMIC_HELP_COMMENT_SIGN)) {
					token.setText(ABAP.DYNAMIC_HELP_COMMENT_SIGN + " comment", false);
				} else {
					token.setText("\" comment", false);
				}
			} else if (token.isComment()) {
				token.setText("* comment", false);

			} else if (obfuscateLiterals && token.isStringLiteral()) {
				token.setText(getNewStringLiteral(oldText), true);

			} else if (obfuscateLiterals && token.isIntegerLiteral()) {
				token.setText(getNewIntegerLiteral(oldText), true);
				
			} else if (!token.isIdentifier()) {
				// do nothing
				
			} else if (token.textEqualsAny(ABAP.ABAP_TRUE, ABAP.ABAP_FALSE, ABAP.ABAP_SPACE)) {
				// do nothing
				
			} else if (token.textEqualsAny(ABAP.builtInFunctions) && token.startsFunctionalMethodCall(true)) {
				// do nothing 

			} else if (command.isAbapSqlOperation() && token.textEqualsAny(ABAP.abapSqlFunctions)) {
				// do nothing 

			} else {
				Token prevCode = token.getPrevCodeToken();

				String newText;
				boolean oldTextMayBeVarName = ABAP.mayBeVariableName(oldText, false);
				boolean prevIfFirstCode = (prevCode == command.getFirstCodeToken());
				if (prevCode != null && prevCode.isAnyKeyword("METHOD", "METHODS") && oldTextMayBeVarName) {
					newText = getNewNameFor(oldText, methods, "", IdentifierType.METHOD);

				} else if (prevCode != null && prevCode.isKeyword("CLASS") && prevIfFirstCode && oldTextMayBeVarName) {
					newText = getTypeName(oldText, classes, "cl_");

				} else if (prevCode != null && prevCode.isKeyword("INTERFACE") && prevIfFirstCode && oldTextMayBeVarName) {
					newText = getTypeName(oldText, classes, "if_");

				} else {					
					newText = obfuscateIdentifier(token);
				}
				token.setText(newText, true);
			}
			if (!AbapCult.stringEquals(oldText, token.getText(), true))
				condenseTillLineEnd = true;
			token = token.getNext();
		}
	}

	private String obfuscateIdentifier(Token token) {
		String oldText = token.getText();
		Token nextCode = token.getNextCodeToken();
		Token parent = token.getParent();

		ArrayList<String> bits = ABAP.splitIdentifier(oldText, true);
		if (bits.size() == 3 && AbapCult.stringEqualsAny(true, bits.get(0) + bits.get(1), ABAP.SY_PREFIX, ABAP.SYST_PREFIX, ABAP.TEXT_SYMBOL_PREFIX)) {
			return oldText;
		}
			
		StringBuilder sb = new StringBuilder();
		StringBuilder sbKey = new StringBuilder();
		for (int i = 0; i < bits.size(); ++ i) {
			String prevBit = (i == 0) ? "" : bits.get(i - 1);
			String bit = bits.get(i);
			boolean isLastBit = (i + 1 == bits.size());
			String nextBit = isLastBit ? "" : bits.get(i + 1);
			String newBit = null;
			
			sbKey.append(bit);

			// if chains like 'lo_object->if_any~method(' shall be removed, simply skip if~, cl_any=>, cl_any->, struc- and proceed with the rest of the identifier
			if (simplify && i + 2 < bits.size() && AbapCult.stringEqualsAny(true, nextBit, "~", "=>", "->", "-")) {
				++i;
				continue;
			}
			// if chains are removed, the key must consist of the whole chain, so 'cl_any=>method(' gets a different identifier than 'cl_other=>method('
			String key = simplify ? sbKey.toString() : bit;

			if (ABAP.isFieldSymbol(bit)) {
				newBit = ABAP.FIELD_SYMBOL_START_STRING + getVariableName(bit.substring(1, bit.length() - 1), variables, "ls_") + ABAP.FIELD_SYMBOL_END_STRING;
				
			} else if (!ABAP.isCharAllowedForVariableNames(bit.charAt(0), true, false)) {
				sb.append(bit);
				continue;
				
			} else if (bits.size() == 1 && token.isTypeIdentifier() && ABAP.isAbapLowerCaseKeyword(bit)) {
				// keep basic types like "i", "string"
				sb.append(bit);
				continue;
				
			} else if (nextBit.equals("~")) {
				newBit = getNewNameFor(key, classes, "if_", IdentifierType.INTERFACE);

			} else if (nextBit.equals("=>")) {
				newBit = getTypeName(bit, classes, "cl_");

			} else if (nextBit.equals("->")) {
				if (AbapCult.stringEquals(bit, "me", true)) {
					newBit = "me";
				} else {
					newBit = getVariableName(bit, variables, "lo_");
				}
			
			} else if (nextBit.equals("(") && nextCode != null && !nextCode.isAttached()) {
				if (token.isTypeIdentifier()) {
					newBit = getTypeName(bit, types, "ty_");
				} else {
					newBit = getNewNameFor(key, methods, "", IdentifierType.METHOD);
				}
			
			} else if (prevBit.equals("~")) {
				if (token.isTypeIdentifier()) {
					newBit = getTypeName(bit, types, "ty_");
				} else {
					newBit = getNewNameFor(key, methods, "", IdentifierType.METHOD);
				}
			} else if (nextBit.equals("-")) {
				newBit = getVariableName(bit, variables, "ls_");
			
			} else if (nextBit.equals("[")) {
				newBit = getVariableName(bit, variables, "lt_");
			
			} else if ((prevBit.equals("-") || prevBit.equals("]-")) && nextBit.equals("")) {
				if (simplify && sb.length() == 0) {
					newBit = getVariableName(bit, variables, "lv_", key);
				} else {
					newBit = getNewNameFor(key, components, "", IdentifierType.COMPONENT);
				}			

			} else if (prevBit.equals("->") && nextBit.equals("")) {
				if (simplify && sb.length() == 0) {
					newBit = getVariableName(bit, variables, "lv_", key);
				} else {
					newBit = getNewNameFor(key, variables, "mv_", IdentifierType.ATTRIBUTE);
				}
				
			} else if (parent != null && nextCode != null && nextCode.textEquals("=")) {
				if (parent.startsFunctionalMethodCall(false)) {
					newBit = getVariableName(bit, variables, "iv_");
				} else if (parent.getEndOfParamsOrComponentsList() != null) {
					newBit = getNewNameFor(key, components, "", IdentifierType.COMPONENT);
				} else  {
					newBit = getVariableName(bit, variables, "lv_");
				}
				
			} else {
				if (token.isTypeIdentifier()) {
					newBit = getTypeName(bit, types, "ty_");
				} else if (simplify && sb.length() == 0) {
					newBit = getVariableName(bit, variables, "lv_", key);
				} else {
					newBit = getVariableName(bit, variables, "lv_");
				}
			}
			sb.append(newBit);
		}
		return sb.toString();
	}
	
	private String getVariableName(String oldText, HashMap<String, String> variables, String defaultPrefix) {
		return getVariableName(oldText, variables, defaultPrefix, oldText);
	}
	
	private String getVariableName(String oldText, HashMap<String, String> variables, String defaultPrefix, String key) {
		if (variables.containsKey(oldText)) {
			return variables.get(oldText);
		}

		String prefix = determinePrefix(oldText, defaultPrefix, varPrefixes);

		// if removeIdentifierChains is true, we simplify types like CONSTANT, PARAMETER, RESULT to VALUE
		IdentifierType identifierType = IdentifierType.VALUE;
		if (prefix.length() < 2) { 
			// pro forma - keep IdentifierType.VALUE;
		
		} else if (prefix.charAt(0) == 'r') {
			identifierType = simplify ? IdentifierType.VALUE : IdentifierType.RESULT;

		} else if (prefix.charAt(1) == 'v') {
			if ("iec".indexOf(prefix.charAt(0)) >= 0) {
				identifierType = simplify ? IdentifierType.VALUE : IdentifierType.PARAMETER;
			} else {
				identifierType = IdentifierType.VALUE;
			}
		
		} else if (prefix.charAt(1) == 'c') {
			identifierType = simplify ? IdentifierType.VALUE : IdentifierType.CONSTANT;
		
		} else if (prefix.charAt(1) == 's') {
			identifierType = IdentifierType.STRUCTURE;
		
		} else if (prefix.charAt(1) == 't') {
			identifierType = IdentifierType.TABLE;
		
		} else if (prefix.charAt(1) == 'o') {
			if (prefix.charAt(0) == 'c') {
				identifierType = simplify ? IdentifierType.VALUE : IdentifierType.CONSTANT;
			} else {
				identifierType = IdentifierType.INSTANCE;
			}
		
		} else if (prefix.charAt(1) == 'r') {
			identifierType = IdentifierType.REF;
			
		} else if (prefix.charAt(1) == 'x') {
			identifierType = IdentifierType.EXCEPTION;
		}
		
		if (simplify && !prefix.equals(defaultPrefix)) {
			// if removeIdentifierChains is true, always assume local constants, tables, structures, instances, refs etc. 
			// instead of attributes m..., parameters i... / e... / c... etc.
			prefix = "l" + prefix.substring(1);
		}
		return getNewNameFor(key, variables, prefix, identifierType);
	}
	
	private String getTypeName(String oldText, HashMap<String, String> types, String defaultPrefix) {
		return getTypeName(oldText, types, defaultPrefix, oldText);
	}
	
	private String getTypeName(String oldText, HashMap<String, String> types, String defaultPrefix, String key) {
		if (types.containsKey(oldText)) {
			return types.get(oldText);
		}

		String prefix = determinePrefix(oldText, defaultPrefix, typePrefixes);

		IdentifierType identifierType = IdentifierType.TYPE;
		if (prefix.equals("cl_") || prefix.equals("lcl_")) {
			identifierType = IdentifierType.CLASS;
			
		} else if (prefix.equals("cx_")) {
			identifierType = IdentifierType.EXCEPTION_CLASS;
		
		} else if (prefix.equals("if_") || prefix.equals("lif_")) {
			identifierType = IdentifierType.INTERFACE;
		}
		
		return getNewNameFor(key, types, prefix, identifierType);
	}

	private String determinePrefix(String oldText, String defaultPrefix, String[] knownPrefixes) {
		int underscorePos = oldText.indexOf('_');
		if (underscorePos < 0) 
			return defaultPrefix;

		String prefix = oldText.substring(0, underscorePos + 1).toLowerCase();
		for (String knownPrefix : knownPrefixes) {
			if (prefix.equals(knownPrefix)) {
				return knownPrefix;
			}
		}
		return defaultPrefix;
	}

	private String getNewNameFor(String key, HashMap<String, String> map, String prefix, IdentifierType identifierType) {
		if (map.containsKey(key)) {
			return map.get(key);
		}

		int number;
		String numberPrefix;
		
		if (createShortNames) {
			numberPrefix = identifierType.shortInfix;
			number = 1;

		} else {
			String infix = identifierType.infix;
			for (String name : names) {
				String newName = prefix + name + (StringUtil.isNullOrEmpty(infix) ? "" : "_" + infix);
				if (!map.containsValue(newName)) {
					map.put(key, newName);
					return newName;
				}
			}
			number = names.length + 1;
			numberPrefix = prefix + (StringUtil.isNullOrEmpty(infix) ? "" : infix + "_");
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
	
	private String getNewStringLiteral(String oldText) {
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
	
	private String getNewIntegerLiteral(String oldText) {
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
