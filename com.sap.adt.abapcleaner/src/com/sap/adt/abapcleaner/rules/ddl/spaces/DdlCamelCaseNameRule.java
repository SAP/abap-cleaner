package com.sap.adt.abapcleaner.rules.ddl.spaces;

import java.time.LocalDate;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.CamelCaseNames;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotation;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationScope;
import com.sap.adt.abapcleaner.rules.prettyprinter.CamelCaseNameRule;

public class DdlCamelCaseNameRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.DDL_CAMEL_CASE_NAME; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_SPACES; }

	@Override
	public String getDisplayName() { return "Use CamelCase for known entity and field names"; }

	@Override
	public String getDescription() { return "Fixes upper and lower case of known entity names and field names, and of aliases named after known entity names."; }

	public String getHintsAndRestrictions() { return "This rule only changes view and field names where they are defined; usages are automatically adjusted upon save. Custom view and field names from rule '" + CamelCaseNameRule.displayName + "' are reused."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 11); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public Language[] getSupportedLanguages() { return ddlOnly; }

	@Override
	public boolean dependsOnExternalFiles() { return true; } // this rule depends on the .txt files for custom view and field names

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view entity i_Companycode"
				+ LINE_SEP + "  as select from t001 as d"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association[0..1] to I_Country as _country"
				+ LINE_SEP + "    on $projection.Country = _country.Country"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association[0..1] to I_ChartOfAccounts as _chartofaccounts"
				+ LINE_SEP + "    on $projection.ChartOfAccounts = _chartofaccounts.ChartOfAccounts"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "      @ObjectModel.text.element: ['companycodename']" 
				+ LINE_SEP + "  key d.bukrs    as companycode," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      d.butxt    as companyCodename," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      d.ort01    as CityName," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      @ObjectModel.foreignKey.association: '_country'" 
				+ LINE_SEP + "      d.land1    as COUNTRY," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      @ObjectModel.foreignKey.association: '_CHARTOFACCOUNTS'"
				+ LINE_SEP + "      d.ktopl    as chArtOfAccOUnts,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      d.periv    as FiscalyeaRVariant," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      @Consumption.valueHelpDefinition: [{entity: { name:    'i_controllingareastdvh',"
				+ LINE_SEP + "                                                    element: 'CONTROLLINGAREA' }}]"
				+ LINE_SEP + "      d.kokrs    as controllingarea," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      d.mwska    as nontaxabletransactiontaxcode," 
				+ LINE_SEP + "      d.xvatdate as TAXRPTGDATEISACTIVE,"
				+ LINE_SEP + "      d.xskfn    as cashdiscountbaseamtisnetamt," 
				+ LINE_SEP + ""
				+ LINE_SEP + "      // Once the definitions of aliases etc. are corrected to _ChartOfAccounts etc. above,"
				+ LINE_SEP + "      // any usage of these names will automatically be adjusted once the view is saved."
				+ LINE_SEP + "      // Therefore, it would not help to fix names that are defined in a different place,"
				+ LINE_SEP + "      // because that would be undone when saving the view (except for annotation values)."
				+ LINE_SEP + "      _country,"
				+ LINE_SEP + "      _chartofaccounts"
				+ LINE_SEP + "}";
	}
	
	final ConfigBoolValue configFixEntityName = new ConfigBoolValue(this, "FixEntityName", "Fix known entity names to CamelCase", true);
	final ConfigBoolValue configFixAliases = new ConfigBoolValue(this, "FixAliases", "Fix aliases named after known entity names to CamelCase", true);
	final ConfigBoolValue configFixFieldNames = new ConfigBoolValue(this, "FixFieldNames", "Fix known field names to CamelCase", true);
	final ConfigBoolValue configFixRefsInAnnotations = new ConfigBoolValue(this, "FixRefsInAnnotations", "Fix references in annotations to CamelCase", true);
	
	final ConfigBoolValue configOnlyApprovedNames = new ConfigBoolValue(this, "OnlyApprovedNames", "Only use approved names", true);
	final ConfigBoolValue configRequireAllFieldsKnown = new ConfigBoolValue(this, "RequireAllFieldsKnown", "Only change field names if all field names in this entity are known", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configFixEntityName, configFixAliases, configFixFieldNames, configFixRefsInAnnotations, configOnlyApprovedNames, configRequireAllFieldsKnown };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlCamelCaseNameRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	public static String getAliasKey(String alias) {
		return alias.toUpperCase();
	}
	
	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		HashMap<String, String> associationAliases = new HashMap<>();

		// if configured, determine whether all field names are known (and only therefore are allowed to be changed)
		boolean allowFieldNames = true;
		boolean onlyApproved = configOnlyApprovedNames.getValue();
		if (configRequireAllFieldsKnown.getValue()) {
			Command testCommand = code.firstCommand;
			while (testCommand != null) {
				if (testCommand.isDdl() && testCommand.isDdlSelectElement()) {
					if (isFieldNameUnknown(testCommand, onlyApproved)) {
						allowFieldNames = false;
						break;
					}
				}
				testCommand = testCommand.getNext();
			}
		}

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			if (command.isDdl() && !isCommandBlocked(command)) {
				if (executeOn(code, command, associationAliases, allowFieldNames)) {
					code.addRuleUse(this, command);
				}
			}

			command = command.getNext();
		}
	}

	private boolean executeOn(Code code, Command command, HashMap<String, String> associationAliases, boolean allowFieldNames) {
		boolean changed = false;

		if (command.isCommentLine())
			return false;
		
		boolean onlyApproved = configOnlyApprovedNames.getValue();
		boolean hasParent = (command.getParent() != null);

		if (configFixEntityName.getValue() && !hasParent) 
			changed |= fixEntityName(command, onlyApproved);

		if (configFixAliases.getValue() && !hasParent)
			changed |= fixAliases(command, onlyApproved, associationAliases);

		if (configFixFieldNames.getValue() && allowFieldNames && command.isDdlSelectElement())
			changed |= fixFieldName(command, onlyApproved);
		
		if (configFixRefsInAnnotations.getValue() && command.isDdlAnnotation())
			changed |= fixRefsInAnnotations(command, onlyApproved, associationAliases, allowFieldNames);
		
		return changed;
	}
	
	private boolean isFieldNameUnknown(Command command, boolean onlyApproved) {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) // pro forma
			return false;
		
		Token fieldName = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "AS", TokenSearch.ANY_IDENTIFIER);
		String fieldNameText = "";
		if (fieldName != null && fieldName.getPrevCodeSibling().isKeyword("AS")) {
			// the alias is the field name
			fieldNameText = fieldName.getText();
		} else {
			// determine field name from "<path expression>.FieldName"
			fieldName = command.getLastCodeToken();
			if (fieldName.isComma()) {
				fieldName = fieldName.getPrevCodeSibling();
			}
			fieldNameText = fieldName.getText();
			int dotPos = fieldNameText.lastIndexOf(DDL.DOT_SIGN);
			if (dotPos > 0) {
				fieldNameText = fieldNameText.substring(dotPos + 1);  
			}
		}
		// do not consider associations
		if (fieldNameText.startsWith("_"))
			return false;
		
		CamelCaseNames fieldNames = CamelCaseNames.getFieldNames();
		String camelCaseFieldName = fieldNames.applyCamelCaseTo(fieldNameText, false, onlyApproved, parentProfile);
		return (camelCaseFieldName == null);
	}
	
	private boolean fixEntityName(Command command, boolean onlyApproved) {
		Token entityName = command.getDdlOrDclEntityNameToken();
		if (entityName == null)
			return false;
		
		Token prevCode = entityName.getPrevCodeSibling();
		if (prevCode == null || !prevCode.isAnyKeyword("VIEW", "ENTITY", "ROLE")) // exclude STRUCTURE, TABLE, HIERARCHY, FUNCTION, ACCESSPOLICY
			return false;

		CamelCaseNames viewNames = CamelCaseNames.getViewNames();
		String camelCaseEntityName = viewNames.applyCamelCaseTo(entityName.getText(), false, onlyApproved, parentProfile);
		if (camelCaseEntityName == null) 
			return false;
			
		return entityName.setText(camelCaseEntityName, false);
	}
	
	private boolean fixAliases(Command command, boolean onlyApproved, HashMap<String, String> associationAliases) {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) // pro forma
			return false;
		
		boolean isAssociation = firstCode.startsDdlAssociation();
		if (!firstCode.startsDdlJoin() && !isAssociation && !firstCode.matchesDeep(true, TokenSearch.ASTERISK, "SELECT", "FROM")) 
			return false;

		Token aliasToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "AS", TokenSearch.ANY_IDENTIFIER);
		if (aliasToken == null || !aliasToken.getPrevCodeSibling().isKeyword("AS")) // pro forma
			return false;
		
		// remove a leading underscore (for associations), if any
		String underscorePrefix = aliasToken.textStartsWith("_") ? "_" : "";
		String alias = aliasToken.getText().substring(underscorePrefix.length());

		// test whether a view with this name exists
		CamelCaseNames viewNames = CamelCaseNames.getViewNames();
		String camelCase = viewNames.applyCamelCaseTo(alias, false, onlyApproved, parentProfile);

		if (camelCase != null) {
			String camelCaseAlias = underscorePrefix + camelCase;
			if (isAssociation)
				associationAliases.put(getAliasKey(camelCaseAlias), camelCaseAlias);
			return aliasToken.setText(camelCaseAlias, false);
		}
		
		// remove the entity prefix (e.g. "I_"), if any
		String entityPrefix = "";
		int pos = alias.indexOf('_');
		if (pos == 1) {
			entityPrefix = alias.substring(0, pos).toUpperCase();
			if (!CamelCaseNames.entityPrefixExists(entityPrefix))  
				return false;
			entityPrefix += "_";
		} // otherwise continue below, leaving entityPrefix == "" and trying other prefixes before
		String aliasWithoutEntityPrefix = alias.substring(entityPrefix.length());
		
		// test whether a view name with an (added or different) entity prefix exists
		for (String testEntityPrefixLetter : CamelCaseNames.getEntityPrefixLetters()) {
			String testPrefix = testEntityPrefixLetter + "_";
			camelCase = viewNames.applyCamelCaseTo(testPrefix + aliasWithoutEntityPrefix, false, onlyApproved, parentProfile);
			if (camelCase != null) {
				String camelCaseAlias = underscorePrefix + entityPrefix + camelCase.substring(testPrefix.length());
				if (isAssociation)
					associationAliases.put(getAliasKey(camelCaseAlias), camelCaseAlias);
				return aliasToken.setText(camelCaseAlias, false);
			}
		}

		if (isAssociation)
			associationAliases.put(getAliasKey(aliasToken.getText()), aliasToken.getText());
		return false;
	}

	private boolean fixFieldName(Command command, boolean onlyApproved) {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) // pro forma
			return false;
		
		Token fieldName = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "AS", TokenSearch.ANY_IDENTIFIER);
		if (fieldName == null || fieldName.getPrevCodeSibling() == null || !fieldName.getPrevCodeSibling().isKeyword("AS")) // pro forma
			return false;
		
		CamelCaseNames fieldNames = CamelCaseNames.getFieldNames();
		String camelCaseFieldName = fieldNames.applyCamelCaseTo(fieldName.getText(), false, onlyApproved, parentProfile);
		if (camelCaseFieldName == null) 
			return false;
			
		return fieldName.setText(camelCaseFieldName, false);
	}

	private boolean fixRefsInAnnotations(Command command, boolean onlyApproved, HashMap<String, String> associationAliases, boolean allowFieldNames) {
		boolean changed = false;
		
		// create an annotation 'scope' from the current Command
		DdlAnnotationScope scope = new DdlAnnotationScope(true);
		try {
			scope.add(command);
		} catch (UnexpectedSyntaxBeforeChanges e) {
			// this scope cannot be processed, e.g. due to a comment within an annotation Command
			return false;
		}
		
		// find annotations that contain entity or element reference as their value, 
		// e.g. "@ObjectModel.representativeKey: 'FieldName'"
		for (DdlAnnotation annotation : scope.getAnnotations()) {
			Token valueToken = annotation.getValueToken();
			if (valueToken == null || !valueToken.isDdlStringLiteral())
				continue;
			String value = valueToken.getText().substring(1, valueToken.getTextLength() - 1);

			// determine the annotation path and whether contains the word "element" at the end
			String path = annotation.getPath();
			boolean referencesRemoteElement = annotation.lastElementContains("element", "Element");

			// determine the CamelCase view name / field name / association name
			String newValue = null;
			if (configFixEntityName.getValue() && DDL.isKnownEntityRefAnnotation(path)) {
				newValue = CamelCaseNames.getViewNames().applyCamelCaseTo(value, false, onlyApproved, parentProfile);

			} else if (allowFieldNames && configFixFieldNames.getValue() && (DDL.isKnownElementRefAnnotation(path) || referencesRemoteElement)) {
				newValue = CamelCaseNames.getFieldNames().applyCamelCaseTo(value, false, onlyApproved, parentProfile);
			
			} else if (configFixAliases.getValue() && DDL.isKnownAssociationRefAnnotation(path)) {
				newValue = associationAliases.get(getAliasKey(value));
			}
			
			// replace the association value
			if (newValue != null) {
				changed |= valueToken.setText(DDL.QUOT_MARK_STRING + newValue + DDL.QUOT_MARK_STRING, false);
			}
		}
		
		return changed;
	}
}
