package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.StringUtil;

/** represents an element of a structure, i.e. either a structure name (possibly a CDS view name / alias), 
 * or a component name (possibly a CDS view field / alias); provides information on the element type 
 * and its exact position within a Token (e.g. 'lo_instance->ms_struc-comp-subcomp', 'dtab~AnyComponent') */
public class StrucElement {
	private static final String PARAMETER_PREFIX = "P_";

	private enum ElementType {
		/** the Token as a whole is the name of a structure (i.e. possibly a CDS view name), possibly ending with '(' */
		STRUC_NAME,
		/** the Token as a whole is an alias for a CDS view name */
		STRUC_ALIAS,
		/** the Token as a whole is the name of a component (i.e. possibly the name of a CDS view field) */
		COMPONENT_NAME,
		/** the Token is an alias for a CDS view field (SELECT ... FIELDS AnyField AS alias) */
		COMPONENT_ALIAS,
		/** the Token is the name of a CDS view parameter */
		PARAMETER_NAME,
		/** the Token is an identifier that contains one or multiple component names, and possibly also a structure name,
		 * e.g. 'lo_instance->ms_struc-comp-subcomp', 'dtab~AnyComponent', 'I_AnyView-AnyComponent', ']-component';
		 * in this case, the StrucElement must be split into several StrucElements for individual component names etc. */
		MIXED;
	}

	public static StrucElement createForStructureName(Token token) {
		return new StrucElement(token, ElementType.STRUC_NAME, null);
	}

	public static StrucElement createForStructureAlias(Token token, Token parent) {
		return new StrucElement(token, ElementType.STRUC_ALIAS, parent);
	}

	public static StrucElement createForComponentName(Token token, Token parent) {
		return new StrucElement(token, ElementType.COMPONENT_NAME, parent);
	}

	public static StrucElement createForComponentAlias(Token token, Token parent) {
		return new StrucElement(token, ElementType.COMPONENT_ALIAS, parent);
	}

	public static StrucElement createForParameterName(Token token, Token parent) {
		return new StrucElement(token, ElementType.PARAMETER_NAME, parent);
	}

	// create methods for a temporary StrucElement of ElementType.MIXED, which must be split into several dedicated StrucElements
	public static StrucElement createPartialForStructureAliasAndField(Token token, Token parent) {
		return new StrucElement(token, ElementType.MIXED, parent);
	}

	public static StrucElement createPartialForExprWithComponent(Token token) {
		return new StrucElement(token, ElementType.MIXED, null);
	}

	// create methods to complete the partial StrucElement to a complete one
	public static StrucElement createForStructureName(StrucElement template, int componentSelectorPos) {
		return new StrucElement(template, ElementType.STRUC_NAME, 0, componentSelectorPos);
	}

	public static StrucElement createForStructureAlias(StrucElement template, int tildePos) {
		return new StrucElement(template, ElementType.STRUC_ALIAS, 0, tildePos);
	}

	public static StrucElement createForComponentName(StrucElement template, int textOffset, int textLength) {
		return new StrucElement(template, ElementType.COMPONENT_NAME, textOffset, textLength);
	}

	public static String getPathToComponent(Token expr, int delimiterPos) {
		Token pathStart = expr.getPrevSiblingWhileLevelCloser();
		StringBuilder sbPath = new StringBuilder();
		Token token = pathStart;
		while (token != null) {
			if (token == expr) {
				sbPath.append(token.getText().substring(0, delimiterPos));
				break;
			} else {
				sbPath.append(token.getText());
			}
			token = token.getNextCodeSibling();
		} 
		return sbPath.toString();
	}

	// =========================================================================
	
	public final Token token;
	public int textOffset;
	public int textLength;
	private final ElementType elementType;

	/** if a Command contains multiple component names from the same structure, all component name Tokens will get the same 
	 * contextId; this is calculated from some 'parent' Token (e.g. the beginning of a constructor expression or the first keyword
	 * of a Command) which is common to all component name Tokens and thus creates the same hashValue() for the contextId */
	public final int contextId;
	
	// =========================================================================

	private StrucElement(Token token, ElementType elementType, Token parent) {
		this.token = token;
		this.textOffset = 0;
		this.textLength = token.getTextLength();
		this.elementType = elementType;
		this.contextId = (parent == null) ? 0 : parent.hashCode();
	}

	private StrucElement(StrucElement template, ElementType elementType, int textOffset, int textLength) {
		if (textOffset < 0 || textLength < 0 || textOffset + textLength > template.token.getTextLength())
			throw new IllegalArgumentException();

		this.token = template.token;
		this.textOffset = textOffset;
		this.textLength = textLength;
		this.elementType = elementType;
		// refine the template's contextId (which was so far derived from the parent Token), by additionally making it depend on 
		// the 'path' to the component, e.g. 'ls_struc-', 'lo_instance->ms_struc-', 'itab[]-' etc.
		// if textOffset == 0, this is not needed, because then this element is a structure name 'ty_s_any-...' or a view alias 'a~...'
		this.contextId = (textOffset == 0 || template.token.isComment()) ? template.contextId : (template.contextId ^ getPathToComponent(template.token, textOffset).toUpperCase().hashCode());
	}

	public boolean isMixed() {
		return (elementType == ElementType.MIXED);
	}

	public boolean isStructureNameOrAlias() {
		return (elementType == ElementType.STRUC_NAME || elementType == ElementType.STRUC_ALIAS);
	}

	public boolean isComponentNameOrAlias() {
		return (elementType == ElementType.COMPONENT_NAME || elementType == ElementType.COMPONENT_ALIAS);
	}

	public boolean isAlias() {
		return (elementType == ElementType.STRUC_ALIAS || elementType == ElementType.COMPONENT_ALIAS);
	}

	public boolean isParameterName() {
		return (elementType == ElementType.PARAMETER_NAME);
	}
	
	private boolean isParameterNameWithParamPrefix() {
		return isParameterName() && StringUtil.startsWith(getTokenTextBit(), PARAMETER_PREFIX, true);
	}
	
	public void setTextRange(int textOffset, int textLength) {
		this.textOffset = textOffset;
		this.textLength = textLength;
	}

	public String getTokenTextBitWithoutParamPrefix() {
		String textBit = getTokenTextBit();
		if (isParameterNameWithParamPrefix()) {
			// remove the prefix 'P_': without this prefix, the parameter name should be a known field name
			textBit = textBit.substring(PARAMETER_PREFIX.length());
		}
		return textBit;
	}
	private String getTokenTextBit() {
		return token.getText().substring(textOffset, textOffset + textLength);
	}
	
	public boolean replaceTokenTextBitAddingParamPrefix(String changedBit) {
		if (isParameterNameWithParamPrefix()) {
			// add the prefix 'P_' again which was removed in getTokenTextBitWithoutParamPrefix(); this time, use upper case
			return replaceTokenTextBit(PARAMETER_PREFIX + changedBit);
		} else {
			return replaceTokenTextBit(changedBit);
		}
	}
	private boolean replaceTokenTextBit(String changedBit) {
		if (changedBit.length() != textLength)
			throw new IllegalArgumentException();
		String changedText = token.getText().substring(0, textOffset) + changedBit + token.getText().substring(textOffset + textLength);
		return token.setText(changedText, false);
	}
}
