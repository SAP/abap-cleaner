package com.sap.adt.abapcleaner.parser;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulehelpers.SelectClause;
import com.sap.adt.abapcleaner.rulehelpers.SelectQuery;

/** Obfuscates identifiers (variable names, method names etc.) in the supplied Code to help transforming real-life  
 * code samples into example or test code, which can then be used as open-source. With activated developer features, 
 * obfuscation can be triggered by FrmProfiles.btnObfuscate to execute it on any (previously pasted) example code. 
 * Results must be manually reviewed and reworked. */
public class AbapObfuscator extends Obfuscator {
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
	
	// -------------------------------------------------------------------------
	
	private HashMap<String, String> types = new HashMap<>();
	private HashMap<String, String> builtInTypes = new HashMap<>();
	private HashMap<String, String> variables = new HashMap<>();
	private HashMap<String, String> components = new HashMap<>();
	private HashMap<String, String> classes = new HashMap<>();
	private HashMap<String, String> methods = new HashMap<>();
	// for ABAP SQL:
	private HashMap<String, String> dtabs = new HashMap<>();
	private HashMap<String, String> tabAliases = new HashMap<>();
	private HashMap<String, String> cols = new HashMap<>();

	protected AbapObfuscator(boolean commandScope, boolean createShortNames, boolean simplify, boolean removeLineEndComments, boolean removeCommentLines, boolean obfuscateLiterals) {
		super(Language.ABAP, commandScope, createShortNames, simplify, removeLineEndComments, removeCommentLines, obfuscateLiterals); 
	}

	private void clearMaps(boolean clearMethods, boolean clearClasses) {
		types.clear();
		variables.clear();
		components.clear();
		
		// only initialize builtInTypes the first time
		if (builtInTypes.isEmpty()) {
			for (String builtInAbapType : ABAP.builtInAbapTypes) {
				builtInTypes.put(getKey(builtInAbapType), builtInAbapType);
			}
			for (String builtInDdicType : ABAP.builtInDdicTypes) { 
				builtInTypes.put(getKey(builtInDdicType), builtInDdicType);
			}
		}

		clearLiterals();

		if (clearMethods) {
			methods.clear();
		}
		if (clearClasses) {
			classes.clear();
		}
		
		dtabs.clear();
		tabAliases.clear();
		cols.clear();
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
				command = removeCommentLine(command);
			} else {
				obfuscate(command);
				command = command.getNext();
			}
		}
	}
	
	private void obfuscate(Command command) throws UnexpectedSyntaxAfterChanges {
		if (commandScope) {
			// obfuscate every single Command individually (e.g. to determine structure-identical Commands), 
			// 'forgetting' all identifiers used so far
			clearMaps(true, true);
		}
		// tabaliases and cols for ABAP SQL are Command-specific
		tabAliases.clear();
		cols.clear();
		
		boolean isInOOContext = command.isInOOContext();
		boolean isSql = command.isAbapSqlOperation();
		ArrayList<SelectQuery> selectQueries = null;
		try {
			if (isSql) {
				selectQueries = SelectQuery.createQueryChainsFrom(command);
			}
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(null, e);
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
				boolean oldTextMayBeVarName = ABAP.mayBeVariableName(oldText, false, isInOOContext);
				boolean prevIfFirstCode = (prevCode == command.getFirstCodeToken());
				if (prevCode != null && prevCode.isAnyKeyword("METHOD", "METHODS") && oldTextMayBeVarName) {
					newText = getNewNameFor(getKey(oldText), methods, "", IdentifierType.METHOD);

				} else if (prevCode != null && prevCode.isKeyword("CLASS") && prevIfFirstCode && oldTextMayBeVarName) {
					newText = getTypeName(oldText, classes, "cl_", false);

				} else if (prevCode != null && prevCode.isKeyword("INTERFACE") && prevIfFirstCode && oldTextMayBeVarName) {
					newText = getTypeName(oldText, classes, "if_", false);

				} else {
					SelectClause selectClause = isSql ? determineSelectClause(token, selectQueries) : SelectClause.NONE;
					newText = obfuscateIdentifier(token, isInOOContext, selectClause);
				}
				token.setText(newText, true);
			}
			if (!AbapCult.stringEquals(oldText, token.getText(), true))
				condenseTillLineEnd = true;
			token = token.getNext();
		}
	}

	private SelectClause determineSelectClause(Token token, ArrayList<SelectQuery> selectQueries) {
		// search in subqueries first (and therefore in reverse order), because outer queries contain the Tokens of inner queries 
		for (int i = selectQueries.size() - 1; i >= 0; --i) {
			SelectQuery query = selectQueries.get(i);
			while (query != null) {
				for (SelectClause clauseType : query.getClausTypesInOrder()) {
					Term term = query.getClause(clauseType);
					if (term != null && term.contains(token)) {
						return clauseType;
					}
				}
				query = query.getNextQuery();
			}
		}
		return SelectClause.NONE;
	}

	private String obfuscateIdentifier(Token token, boolean isInOOContext, SelectClause selectClause) {
		String oldText = token.getText();
		Token prevCode = token.getPrevCodeToken();
		Token nextCode = token.getNextCodeToken();
		Token parent = token.getParent();

		// identify host variables in ABAP SQL, e.g. @var, @DATA(var) and obfuscate them as if they were outside of ABAP SQL
		if (selectClause != SelectClause.NONE) {
			if (token.getText().startsWith(ABAP.AT_SIGN_STRING)) {
				// @var
				selectClause = SelectClause.NONE;
				
			} else if (token.isAttached() && token.getParent() != null && token.getParent().textStartsWith(ABAP.AT_SIGN_STRING)) {
				// @DATA(var), @FINAL(var)
				selectClause = SelectClause.NONE;

			} else if (selectClause != SelectClause.INTO && token.isAttached() && token.getParent() != null && token.getParent().textEquals("(")) {
				// (dynamic_list) - exception: 'INTO (lv_var)', which is allowed without spaces for 'INTO ( elem1, elem2, ... )'
				selectClause = SelectClause.NONE;
			
			} else if (selectClause == SelectClause.INTO || selectClause == SelectClause.FOR_ALL_ENTRIES) {
				// INTO TABLE lt_any / FOR ALL ENTRIES IN lt_any (non-strict mode without @ for host variables)
				selectClause = SelectClause.NONE;
			
			} else if (selectClause == SelectClause.WHERE && prevCode != null && prevCode.isComparisonOperator()) {
				// 'WHERE col1 = lv_any OR col2 IN lt_any' (non-strict mode without @ for host variables)
				selectClause = SelectClause.NONE;
			}
		}

		ArrayList<String> bits = ABAP.splitIdentifier(oldText, true, isInOOContext);
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
			// exception: in SELECT FROM ... JOIN ... ON ..., 'tabalias~col' must NOT be simplified
			if (simplify && i + 2 < bits.size() && AbapCult.stringEqualsAny(true, nextBit, "~", "=>", "->", "-") && selectClause != SelectClause.FROM) {
				++i;
				continue;
			}
			// if chains are removed, the key must consist of the whole chain, so 'cl_any=>method(' gets a different identifier than 'cl_other=>method('
			String key = getKey(simplify ? sbKey.toString() : bit);

			if (ABAP.isFieldSymbol(bit)) {
				newBit = ABAP.FIELD_SYMBOL_START_STRING + getVariableName(bit.substring(1, bit.length() - 1), variables, "ls_") + ABAP.FIELD_SYMBOL_END_STRING;
				
			} else if (!ABAP.isCharAllowedForVariableNames(bit, 0, true, false, isInOOContext)) {
				sb.append(bit);
				continue;
				
			} else if (bits.size() == 1 && token.isTypeIdentifier(false) && ABAP.isAbapLowerCaseKeyword(bit)) {
				// keep basic types like "i", "string"
				sb.append(bit);
				continue;
				
			} else if (nextBit.equals("~")) {
				if (selectClause == SelectClause.NONE) {
					newBit = getNewNameFor(key, classes, "if_", IdentifierType.INTERFACE);
				} else {
					newBit = getNewNameFor(key, tabAliases, "", IdentifierType.TABALIAS);
				}

			} else if (nextBit.equals("=>")) {
				newBit = getTypeName(bit, classes, "cl_", false);

			} else if (nextBit.equals("->")) {
				if (AbapCult.stringEquals(bit, "me", true)) {
					newBit = "me";
				} else {
					newBit = getVariableName(bit, variables, "lo_");
				}
			
			} else if (nextBit.equals("(") && nextCode != null && !nextCode.isAttached()) {
				if (token.isTypeIdentifier(true)) {
					newBit = getTypeName(bit, types, "ty_", false);
				} else {
					newBit = getNewNameFor(key, methods, "", IdentifierType.METHOD);
				}
			
			} else if (prevBit.equals("~")) {
				if (selectClause != SelectClause.NONE) {
					newBit = getNewNameFor(key, cols, "", IdentifierType.COL);
				} else if (token.isTypeIdentifier(true)) {
					newBit = getTypeName(bit, types, "ty_", false);
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
				if (selectClause == SelectClause.FROM) {
					if (prevCode != null && prevCode.isKeyword("AS")) {
						newBit = getNewNameFor(key, tabAliases, "", IdentifierType.TABALIAS);
					} else if (nextCode != null && nextCode.isKeyword("AS") || prevCode != null && prevCode.isKeyword("FROM")) {
						newBit = getNewNameFor(key, dtabs, "", IdentifierType.DTAB);
					} else {
						newBit = getNewNameFor(key, cols, "", IdentifierType.COL);
					}					
				} else if (selectClause != SelectClause.NONE) {
					newBit = getNewNameFor(key, cols, "", IdentifierType.COL);
				} else if (token.isTypeIdentifier(true)) {
					newBit = getTypeName(bit, types, "ty_", (bits.size() == 1));
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
		return getVariableName(oldText, variables, defaultPrefix, getKey(oldText));
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
	
	private String getTypeName(String oldText, HashMap<String, String> types, String defaultPrefix, boolean mayBeBuiltInType) {
		return getTypeName(oldText, types, defaultPrefix, mayBeBuiltInType, getKey(oldText));
	}
	
	private String getTypeName(String oldText, HashMap<String, String> types, String defaultPrefix, boolean mayBeBuiltInType, String key) {
		if (mayBeBuiltInType && builtInTypes.containsKey(key))
			return builtInTypes.get(key);
		if (types.containsKey(key)) 
			return types.get(key);

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

}
