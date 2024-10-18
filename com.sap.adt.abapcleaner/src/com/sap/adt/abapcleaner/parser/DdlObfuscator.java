package com.sap.adt.abapcleaner.parser;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotation;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationScope;

public class DdlObfuscator extends Obfuscator {
	private final static String parameterPrefix = "P_";
	private final static String viewAliasPrefix = DDL.UNDERSCORE_STRING;
	
	// -------------------------------------------------------------------------
	
	private HashMap<String, String> builtInTypes = new HashMap<>();

	private HashMap<String, String> entities = new HashMap<>();
	private HashMap<String, String> parameters = new HashMap<>();
	private HashMap<String, String> types = new HashMap<>();
	private HashMap<String, String> dataSources = new HashMap<>();
	private HashMap<String, String> sourceAliases = new HashMap<>();
	private HashMap<String, String> fields = new HashMap<>();

	protected DdlObfuscator(boolean commandScope, boolean createShortNames, boolean simplify, boolean removeLineEndComments, boolean removeCommentLines, boolean obfuscateLiterals) {
		super(Language.DDL, commandScope, createShortNames, simplify, removeLineEndComments, removeCommentLines, obfuscateLiterals); 
	}

	private void clearMaps(boolean clearAll) {
		// only initialize builtInTypes the first time
		if (builtInTypes.isEmpty()) {
			// TODO
		}

		clearLiterals();

		sourceAliases.clear();

		if (clearAll) {
			entities.clear();
			parameters.clear();
			types.clear();
			dataSources.clear();
			fields.clear();
		}
	}
	
	public void obfuscate(Code code) throws UnexpectedSyntaxAfterChanges {
		clearMaps(true);
		
		Command command = code.firstCommand;
		while (command != null) {
			if (command.startsDdlUnionEtc())
				clearMaps(false);

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
			clearMaps(true);
		}
		
		DdlAnnotationScope scope = null;
		if (command.isDdlAnnotation()) {
			scope = new DdlAnnotationScope(true);
			try {
				scope.add(command);
			} catch (UnexpectedSyntaxBeforeChanges e) {
				scope = null;
			}
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

			if (token.isDdlInlineComment()) {
				token.setText(DDL.ASTERISK_COMMENT_START + " comment " + DDL.ASTERISK_COMMENT_END, false);
				
			} else if (token.isComment()) {
				token.setText(DDL.LINE_END_COMMENT + " comment", false);

			} else if (obfuscateLiterals && token.isStringLiteral()) {
				if (command.isDdlAnnotation()) {
					token.setText(getNewAnnotationValue(scope, token, oldText), true);
				} else {
					token.setText(getNewStringLiteral(oldText), true);
				}

			} else if (obfuscateLiterals && token.isIntegerLiteral()) {
				token.setText(getNewIntegerLiteral(oldText), true);
				
			} else if (!token.isIdentifier() || command.isDdlAnnotation()) {
				// do nothing
				
			} else if ((DDL.isBuiltInDdlFunction(token.getText()) || DDL.isBuiltInDclFunction(token.getText()))
					&& token.getNextCodeSibling() != null && token.getNextCodeSibling().textEquals(DDL.PARENS_OPEN_STRING)) {
				// do nothing 

			} else {
				String newText;
				if (token.isDdlEntityName()) {
					newText = getEntityName(oldText);
				
				} else if (token.isDdlParameterName()) {
					newText = getParameterName(oldText);
					
				} else if (token.isDdlDataSourceName()) {
					newText = getDataSourceName(oldText);
				
				} else if (token.isDdlSourceAliasDefinition()) {
					newText = getSourceAlias(oldText, true);
					
				} else if (token.isDdlTypeName()) {
					newText = getTypeName(oldText, types, true);

				} else {
					newText = obfuscateIdentifier(token);
				}
				
				token.setText(newText, true);
			}
			if (!oldText.equalsIgnoreCase(token.getText()))
				condenseTillLineEnd = true;
			token = token.getNext();
		}
	}

	private String getNewAnnotationValue(DdlAnnotationScope scope, Token token, String oldText) {
		if (scope == null)
			return getNewStringLiteral(oldText);

		for (DdlAnnotation annotation : scope.getAnnotations()) {
			if (token != annotation.getValueToken()) 
				continue;

			String ref = token.getText().substring(1, token.getTextLength() - 1);
			String path = annotation.getPath();
			
			String newRef;
			if (DDL.isKnownEntityRefAnnotation(path)) {
				newRef = getEntityName(ref);

			} else if (DDL.isKnownParameterRefAnnotation(path)) {
				newRef = getParameterName(ref);
			
			} else if (DDL.isKnownAssociationRefAnnotation(path)) {
				newRef = getSourceAlias(ref, false);
			
			} else if (DDL.isKnownElementRefAnnotation(path) || annotation.lastElementContains("element", "Element")) {
				newRef = getFieldName(ref, getKey(ref));

			} else {
				// fall back to normal string literal
				break;
			}
			return DDL.QUOT_MARK_STRING + newRef + DDL.QUOT_MARK_STRING;
		}
		return getNewStringLiteral(oldText);
	}

	private String obfuscateIdentifier(Token token) {
		String oldText = token.getText();
		Token nextCode = token.getNextCodeToken();

		ArrayList<String> bits = DDL.splitIdentifier(oldText);
		if (bits.size() == 3 && (bits.get(0).equalsIgnoreCase(DDL.SESSION_PREFIX) || (bits.get(0) + bits.get(1)).equalsIgnoreCase(DDL.TYPED_LITERAL_PREFIX))) {
			return oldText;
		}
			
		StringBuilder sb = new StringBuilder();
		StringBuilder sbKey = new StringBuilder();
		for (int i = 0; i < bits.size(); ++ i) {
			// String prevBit = (i == 0) ? "" : bits.get(i - 1);
			String bit = bits.get(i);
			boolean isLastBit = (i + 1 == bits.size());
			String nextBit = isLastBit ? "" : bits.get(i + 1);
			String newBit = null;

			sbKey.append(bit);

			// if chains like '_AnyAlias._OtherAlias.FieldName' shall be removed, simply skip _AnyAlias., _OtherAlias. and proceed with the rest of the identifier
			if (simplify && i + 2 < bits.size() && nextBit.equals(DDL.DOT_SIGN_STRING)) {
				++i;
				continue;
			}
			// if chains are removed, the key must consist of the whole chain, so '_AnyAlias.FieldName' gets a different identifier than '_OtherAlias.FieldName'
			String key = getKey(simplify ? sbKey.toString() : bit);

			if (bit.equalsIgnoreCase(DDL.PARAMETER_PREFIX) || bit.equalsIgnoreCase(DDL.PROJECTION_PREFIX) || bit.equals(DDL.DOT_SIGN_STRING)) {
				newBit = bit;
			
			} else if (bit.startsWith(DDL.UNDERSCORE_STRING) || nextBit.equals(".") || nextCode != null && nextCode.textEquals(DDL.BRACKET_OPEN_STRING)) {
				newBit = getSourceAlias(bit, false);

			} else if ((i == 0 || i == 2 && bits.get(0).equalsIgnoreCase(DDL.PARAMETER_PREFIX)) && parameters.containsKey(getKey(bit))) {
				newBit = getParameterName(bit);
				
			} else {
				newBit = getFieldName(bit, key);
			}
			
			sb.append(newBit);
		}
		return sb.toString();
	}
	
	private String getEntityName(String oldText) {
		return getNewNameFor(getKey(oldText), entities, "I_", IdentifierType.DDL_ENTITY);
	}

	private String getParameterName(String oldText) {
		return getNewNameFor(getKey(oldText), parameters, parameterPrefix, IdentifierType.DDL_PARAMETER);
	}

	private String getDataSourceName(String oldText) {
		boolean useCamelCaseStyle = !oldText.equals(oldText.toLowerCase());
		String prefix = useCamelCaseStyle ? "I_" : "";
		return getNewNameFor(getKey(oldText), dataSources, prefix, IdentifierType.DDL_DATA_SOURCE, useCamelCaseStyle);
	}

	private String getTypeName(String oldText, HashMap<String, String> types, boolean mayBeBuiltInType) {
		return getTypeName(oldText, types, mayBeBuiltInType, getKey(oldText));
	}
	
	private String getTypeName(String oldText, HashMap<String, String> types, boolean mayBeBuiltInType, String key) {
		boolean useCamelCaseStyle = !oldText.equals(oldText.toLowerCase());
		return getNewNameFor(key, types, "", IdentifierType.DDL_TYPE, useCamelCaseStyle);
	}
	
	private String getSourceAlias(String oldText, boolean isDefinition) {
		String key = getKey(oldText);
		
		// unless the alias is being defined, 'oldText' could also refer to a known data source name, if no alias was defined for it  
		if (!isDefinition && dataSources.containsKey(key)) {
			return dataSources.get(key);
		}
		
		if (oldText.startsWith(DDL.UNDERSCORE_STRING)) {
			String oldTextWithoutUnderscore = oldText.substring(1);
			return viewAliasPrefix + getNewNameFor(getKey(oldTextWithoutUnderscore), sourceAliases, "", IdentifierType.DDL_SOURCE_ALIAS);
		} else {
			return getNewNameFor(key, sourceAliases, "", IdentifierType.DDL_SOURCE_ALIAS);
		}
	}
	
	private String getFieldName(String oldText, String key) {
		// for all-lower-case field names (which might be dtab fields), use snail case, otherwise camel case 
		boolean useCamelCaseStyle = !oldText.equals(oldText.toLowerCase());
		return getNewNameFor(key, fields, "", IdentifierType.DDL_FIELD, useCamelCaseStyle);
	}
}
