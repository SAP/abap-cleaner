package com.sap.adt.abapcleaner.rules.prettyprinter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.StrucElement;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.Persistency;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigTextType;
import com.sap.adt.abapcleaner.rulebase.ConfigTextValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.CamelCaseNames;

public class CamelCaseNameRule extends Rule {
	public static final String displayName = "Use CamelCase for known CDS names";
	private static final String defaultCustomFieldNamesFile = "CustomFieldNames.txt";
	private static final String defaultCustomViewNamesFile = "CustomViewNames.txt";

	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_CLEANER), 
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Use uppercase for keywords and lowercase for operands", "abenlower_upper_case_guidl.htm") };
	
	@Override
	public RuleID getID() { return RuleID.CAMEL_CASE_NAME; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.PRETTY_PRINTER; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Changes known VDM CDS view names and field names to CamelCase."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule does not distinguish between structures from ABAP or CDS View context (see examples below). Please fine-tune as needed."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 3, 15); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean dependsOnExternalFiles() { return true; } // this rule depends on the .txt files for custom view and field names

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD camel_case_names." 
			+ LINE_SEP + "    DATA lt_company TYPE STANDARD TABLE OF i_companycode." 
			+ LINE_SEP + "    DATA lv_fiscal_year_variant TYPE i_companycode-fiscalyearvariant." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" Fields from CDS Views can be hard to read if shown in all-lower case."
			+ LINE_SEP + "    \" The longer the field name, the more its readability benefits from CamelCase:"
			+ LINE_SEP + "    lt_company = VALUE I_COMPANYCODE( ( companycode                  = '1234'" 
			+ LINE_SEP + "                                        companycodename              = 'Company Name'" 
			+ LINE_SEP + "                                        cityname                     = 'Berlin'" 
			+ LINE_SEP + "*                                        chartofaccounts              = 'ABCD'" 
			+ LINE_SEP + "*                                        FiscalyeaRVariant            = 'K4'" 
			+ LINE_SEP + "                                        nontaxabletransactiontaxcode = 'AB'" 
			+ LINE_SEP + "                                        taxrptgdateisactive          = abap_true" 
			+ LINE_SEP + "                                        cashdiscountbaseamtisnetamt  = abap_false ) )." 
			+ LINE_SEP 
			+ LINE_SEP + "    ASSIGN lt_company[ companycodename = 'Company Name' ] TO FIELD-SYMBOL(<ls_any>)." 
			+ LINE_SEP + "    LOOP AT lt_company ASSIGNING FIELD-SYMBOL(<ls_company>) WHERE cityname = 'Berlin'." 
			+ LINE_SEP + "      any_method( iv_cash_discount_is_net = <ls_company>-cashdiscountbaseamtisnetamt" 
			+ LINE_SEP + "                  iv_fiscal_year_variant  = <ls_company>-fiscalyearvariant )." 
			+ LINE_SEP + "    ENDLOOP." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" IMPORTANT: this rule does NOT distinguish between structures from ABAP or VDM CDS View context."
			+ LINE_SEP + "    \" Most components of the following structure consist of two words, but they do not use snake_case,"
			+ LINE_SEP + "    \" so they might be replaced with corresponding CamelCase field names from CDS context, too:"
			+ LINE_SEP + "    ls_vdm_style = VALUE #( refid        = '1'" 
			+ LINE_SEP + "                            owner        = 'anyone'" 
			+ LINE_SEP + "                            cityname     = 'Hamburg'" 
			+ LINE_SEP + "                            lognumber    = '12345'" 
			+ LINE_SEP + "                            dataprovider = 'XYZ' )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" by contrast, proper ABAP style would be snake_case; such component names are not replaced,"
			+ LINE_SEP + "    \" because they differ from the naming style of VDM CDS View fields:"
			+ LINE_SEP + "    ls_abap_style = VALUE #( ref_id         = '1'" 
			+ LINE_SEP + "                             owner          = 'anyone'" 
			+ LINE_SEP + "                             city_name      = 'Hamburg'" 
			+ LINE_SEP + "                             log_number     = '12345'" 
			+ LINE_SEP + "                             data_provider  = 'XYZ' )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" 'False positives' without underscores (such as lognumber above) are typically short."
			+ LINE_SEP + "    \" Unintentional replacement can be prevented by setting a minimum length for a 'sure match' in the"
			+ LINE_SEP + "    \" options above, since in CDS View context, field names are usually long: Among CDS field names,"
			+ LINE_SEP + "    \"   99% have 11+ characters" 
			+ LINE_SEP + "    \"   95% have 15+ characters" 
			+ LINE_SEP + "    \"   82% have 20+ characters" 
			+ LINE_SEP + "    \"   61% have 24+ characters" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" for the following fields, CamelCase variants are known, but none of them is officially approved:"
			+ LINE_SEP + "    SORT lt_web_info BY website" 
			+ LINE_SEP + "                        webaddress" 
			+ LINE_SEP + "                        visitedPlacename." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" Shorter field names as well as field names with just an initial upper case character like 'Currency'" 
			+ LINE_SEP + "    \" or 'Language' can be changed if all field names in the context (e.g. in the whole VALUE constructor)"
			+ LINE_SEP + "    \" are known field names and at least one 'sure match' was identified:"
			+ LINE_SEP + "    lt_company = VALUE #( ( companycode     = '1234'" 
			+ LINE_SEP + "                            currency        = 'EUR'" 
			+ LINE_SEP + "                            language        = 'E'" 
			+ LINE_SEP + "                            ChartofACcounts = 'ABCD' ) )." 
			+ LINE_SEP + "    \" without this restriction, the field 'currency' and 'language' might be changed in ABAP structures, too:"
			+ LINE_SEP + "    ls_abap_struc = VALUE #( obj_nr     = '1'" 
			+ LINE_SEP + "                             currency   = 'EUR'" 
			+ LINE_SEP + "                             language   = 'E'" 
			+ LINE_SEP + "                             field_name = 'any' )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" you can configure how much should be changed if one of the fields is unknown:"
			+ LINE_SEP + "    ls_unknown_struc = VALUE #( refid              = '1'" 
			+ LINE_SEP + "                                currency           = 'EUR'" 
			+ LINE_SEP + "                                unknown_field_name = 42"
			+ LINE_SEP + "                                fiscalyearvariant  = 'K4' )." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configProcessViewNames = new ConfigBoolValue(this, "ProcessViewNames", "Change known view names to CamelCase", true);
	final ConfigBoolValue configProcessFieldNames = new ConfigBoolValue(this, "ProcessFieldNames", "Change known field names to CamelCase", true);
	final ConfigBoolValue configProcessComments = new ConfigBoolValue(this, "ProcessComments", "Consider commented-out lines in VALUE or NEW constructors (expected format: '*  component = ...')", true, false, LocalDate.of(2024, 10, 16));
	final ConfigInfoValue configContextInfo = new ConfigInfoValue(this, "To decide whether to change a known field name to CamelCase, " + Program.PRODUCT_NAME + " analyzes its context, e.g. all field names in a VALUE constructor, a SELECT statement, a table key etc.", ConfigInfoStyle.NORMAL);
	final ConfigIntValue configMinLengthOfSureMatch = new ConfigIntValue(this, "MinLengthOfSureMatch", "Consider known field names a 'sure match' if they are at least", "chars long and contain an upper case letter after a lower case one", 1, 11, 30);
	final ConfigBoolValue configRequireApprovalForSureMatch = new ConfigBoolValue(this, "RequireApprovalForSureMatch", "Only consider approved names a 'sure match'", false);
	final ConfigEnumValue<CamelCaseContextAllKnownAction> configContextAllKnownAction = new ConfigEnumValue<CamelCaseContextAllKnownAction>(this, "ContextAllKnownAction", "If a context contains a 'sure match' and all field names are known:",
			new String[] { "change all known field names in the context", "change all approved field names in the context", "change 'sure matches' only" }, CamelCaseContextAllKnownAction.values(), CamelCaseContextAllKnownAction.CHANGE_ALL_KNOWN);
	final ConfigEnumValue<CamelCaseContextWithUnknownAction> configContextWithUnknownAction = new ConfigEnumValue<CamelCaseContextWithUnknownAction>(this, "ContextWithUnknownAction", "If a context contains a 'sure match' but also an unknown field name:",
			new String[] { "change all known field names in the context (discouraged)", "change all approved field names in the context (discouraged)", "change 'sure matches' only", "do not change any field names in the context" }, CamelCaseContextWithUnknownAction.values(), CamelCaseContextWithUnknownAction.CHANGE_NONE);
	final ConfigEnumValue<CamelCaseDeviationAction> configDeviationAction = new ConfigEnumValue<CamelCaseDeviationAction>(this, "DeviationAction", "Correct existing 'CameLcasE' name",
			new String[] { "if it differs from a known name", "if it differs from an approved name", "never" }, CamelCaseDeviationAction.values(), CamelCaseDeviationAction.CHANGE_IF_APPROVED);
	final ConfigInfoValue configCustomFileInfo = new ConfigInfoValue(this, "Custom view and field names can be maintained in text files inside the (local or synchronized) folder of this profile, simply using the line format CamelCaseName<ENTER>", ConfigInfoStyle.NORMAL, LocalDate.of(2024, 10, 15));
	public final ConfigTextValue configCustomViewNamesFile = new ConfigTextValue(this, "CustomViewNamesFile", "Custom view names file in profile folder:", defaultCustomViewNamesFile, ConfigTextType.FOLDER_FILE_NAME, 30, "Open ...", defaultCustomViewNamesFile, LocalDate.of(2024, 10, 15));
	public final ConfigTextValue configCustomFieldNamesFile = new ConfigTextValue(this, "CustomFieldNamesFile", "Custom field names file in profile folder:", defaultCustomFieldNamesFile, ConfigTextType.FOLDER_FILE_NAME, 30, "Open ...", defaultCustomFieldNamesFile, LocalDate.of(2024, 10, 15));

	private final ConfigValue[] configValues = new ConfigValue[] { configProcessViewNames, configProcessFieldNames, configProcessComments, 
			configContextInfo, configMinLengthOfSureMatch, configRequireApprovalForSureMatch, configContextAllKnownAction, configContextWithUnknownAction, configDeviationAction,
			configCustomFileInfo, configCustomViewNamesFile, configCustomFieldNamesFile}; 

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CamelCaseNameRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	private class ElementContext {
		private ArrayList<StrucElement> elements = new ArrayList<>();
		private int sureCount;
		private int candidateCount;
		private int failCount;
		
		private void add(StrucElement element) {
			elements.add(element);
		}
		private void clearCount() {
			sureCount = 0;
			candidateCount = 0;
			failCount = 0;
		}
	}
	
	@Override
	public String buttonClicked(ConfigValue configValue) {
		if (configValue != configCustomViewNamesFile && configValue != configCustomFieldNamesFile) // pro forma
			return null;
		
		// profiles that were just imported from an external file may not yet have a profile path
		if (StringUtil.isNullOrEmpty(parentProfile.path)) 
			return null;

		// for a read-only profile, always return the folder to be opened (i.e. do not create or open text files directly) 
		Persistency persistency = Persistency.get();
		String dir = persistency.getDirectoryName(parentProfile.path);
		if (parentProfile.isReadOnly)
			return dir;

		// for a local profile, create a text file if it does not already exist
		String folderFile = ((ConfigTextValue)configValue).getValue();
		String path = persistency.combinePaths(dir, folderFile);
		if (!persistency.fileExists(path)) {
			String LINE_SEP = System.lineSeparator();
			String viewOrFieldName = (configValue == configCustomViewNamesFile ? "view" : "field");
			String defaultText = "* CUSTOM " + viewOrFieldName.toUpperCase() + " NAMES" + LINE_SEP
									 + "* Please enter one CamelCase " + viewOrFieldName + " name per line." + LINE_SEP  
									 + "* Empty lines, * comment lines and \" line-end comments are ignored." + LINE_SEP;

			persistency.writeAllTextToFile(path, defaultText, false);
		}
		
		// return the text file to be opened (or, if it could not be created, its folder) 
		return persistency.fileExists(path) ? path : dir;
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		boolean isInsideMethod = false; // for a code snippet that only covers a part of a method, this intentionally stays false

		Command command = code.firstCommand;
		HashMap<Integer, ElementContext> fieldContexts = new HashMap<>();

		while (command != null) {
			commandForErrorMsg = command;

			// keep track of whether the command is inside a method
			if (command.isMethodFunctionOrFormStart()) 
				isInsideMethod = true;
			
			if (!isCommandBlocked(command)) {
				buildContexts(code, command, fieldContexts);
			}

			if (command.isMethodFunctionOrFormEnd()) 
				isInsideMethod = false;

			if (!isInsideMethod && fieldContexts.size() > 0) {
				// process field contexts that were built within a method; outside of methods, process them after every Command
				executeOnContexts(code, fieldContexts);
				fieldContexts.clear();
			}

			command = command.getNext();
		}
	}

	private void buildContexts(Code code, Command command, HashMap<Integer, ElementContext> contexts) {
		// we rely on the RND Parser token refinement for ABAP SQL; without that, ABAP SQL statements would have to be 
		// skipped with "if (command.isAbapSqlOperation()) return false" because token types are very complex to identify 
		// here, see the list of keywords like "COUNT(" in TokenTypeRefiner.refine():

		// get settings for this Command, depending on its context
		boolean processViewNames = configProcessViewNames.getValue();
		boolean processFieldNames = configProcessFieldNames.getValue();
		if (!processViewNames && !processFieldNames)
			return;
		boolean processComments = configProcessComments.getValue() && processFieldNames; // only commented-out field names are changed

		boolean isInOOContext = command.isInOOContext();
		boolean isDeclaration = command.isDeclaration() || command.isDeclarationInClassDef();
		boolean isAbapSql = command.isAbapSqlOperation();
		
		Token token = command.getFirstToken();
		
		while (token != null) {
			if (token.isIdentifier() || processComments && token.isAsteriskCommentLine()) { 
				buildContexts(token, isInOOContext, isDeclaration, isAbapSql, contexts);
			} 
	
			token = token.getNext();
		}
	}
	
	private void buildContexts(Token token, boolean isInOOContext, boolean isDeclaration, boolean isAbapSql, HashMap<Integer, ElementContext> contexts) {
		StrucElement strucInfo = token.getStrucInfo();
		if (strucInfo == null)
			return;

		boolean processViewNames = configProcessViewNames.getValue();
		boolean processFieldNames = configProcessFieldNames.getValue();

		// for commented-out code, only the most frequent case of a commented-out assignment in a NEW or VALUE constructor is considered
		if (token.isAsteriskCommentLine()) {
			if (strucInfo.isComponentNameOrAlias()) {
				String[] bits = token.getBitsOfCommentedOutAssignment();
				if (bits != null) { // pro forma
					int offset = token.getText().indexOf(bits[1]);
					if (offset > 0) { // pro forma
						addStrucElement(StrucElement.createForComponentName(strucInfo, offset, bits[1].length()), contexts);
					}
				}
			}
			return;
		}
		
		// split composed identifiers "any_class=>any_structure-any_component", or "any_class=>any_method(" into multiple parts
		// in order to apply the rule to each part; otherwise, "IF_ANY_INTERFACE~any_method" would be considered a 'mixed case'
		ArrayList<String> textBits = ABAP.splitIdentifier(token.getText(), false, isInOOContext);
		int textBitCount = textBits.size();
		
		if (strucInfo.isStructureNameOrAlias()) {
			if (textBitCount == 2 && textBits.get(1).equals("(")) {
				strucInfo.setTextRange(0, textBits.get(0).length());
			}
			if (processViewNames) {
				addStrucElement(strucInfo, contexts);
			}
			
		} else if (strucInfo.isComponentNameOrAlias() || strucInfo.isParameterName()) {
			if (textBitCount == 1 && processFieldNames) {
				addStrucElement(strucInfo, contexts);
			}

		} else if (isDeclaration && textBitCount == 3 && strucInfo.isMixed() && textBits.get(1).equals(ABAP.COMPONENT_SELECTOR_STRING)) {
			// e.g. 'DATA ... TYPE I_AnyView-AnyField.' 
			int strucNameLength = textBits.get(0).length();
			int componentNameLength = textBits.get(2).length();
			if (processViewNames) {
				addStrucElement(StrucElement.createForStructureName(strucInfo, strucNameLength) , contexts);
			}
			if (processFieldNames) {
				addStrucElement(StrucElement.createForComponentName(strucInfo, strucNameLength + 1, componentNameLength) , contexts);
			}
			
		} else if (isAbapSql && textBitCount == 3 && strucInfo.isMixed() && textBits.get(1).equals(ABAP.TILDE_STRING)) {
			// e.g. 'dtab~AnyComponent'
			int viewAliasLength = textBits.get(0).length();
			int fieldNameLength = textBits.get(2).length();
			if (processViewNames) {
				addStrucElement(StrucElement.createForStructureAlias(strucInfo, viewAliasLength) , contexts);
			}
			if (processFieldNames) {
				addStrucElement(StrucElement.createForComponentName(strucInfo, viewAliasLength + 1, fieldNameLength) , contexts);
			}
			
		} else if (strucInfo.isMixed() && processFieldNames) {
			// an identifier can contain one or multiple components: 
			// 'lo_instance->ms_struc-comp', 'ls_any-AnyComponent', ']-struc_comp-any_instance->ms_any-component'
			String prevTextBit = null;
			int offset = 0;
			
			for (String textBit : textBits) {
				if (prevTextBit != null && prevTextBit.endsWith(ABAP.COMPONENT_SELECTOR_STRING)) {
					// struc-comp, itab[ ... ]-comp, <...>-comp, 'itab[ ... ]-struc_comp-any_instance->ms_any-component' 
					addStrucElement(StrucElement.createForComponentName(strucInfo, offset, textBit.length()), contexts);
				}
				offset += textBit.length();
				prevTextBit = textBit;
			}
		}
	}

	private void addStrucElement(StrucElement strucElement, HashMap<Integer, ElementContext> contexts) {
		// add the element to an 'element context', which e.g. binds together all fields in a VALUE constructor, 
		// a 'SORT BY ...' clause, a 'KEY keyname COMPONENTS ...' clause, a 'LOOP AT ... WHERE' clause etc.
		ElementContext context = contexts.get(strucElement.contextId);
		if (context == null) {
			context = new ElementContext();
			contexts.put(strucElement.contextId, context);
		}
		if (!strucElement.isAlias()) {
			context.add(strucElement);
		}
	}

	private void executeOnContexts(Code code, HashMap<Integer, ElementContext> contexts) {
		int minFieldLength = configMinLengthOfSureMatch.getValue();
		boolean requireApprovalForSureMatch = configRequireApprovalForSureMatch.getValue();
		
		CamelCaseContextAllKnownAction contextAllKnownAction = CamelCaseContextAllKnownAction.forValue(configContextAllKnownAction.getValue());
		CamelCaseContextWithUnknownAction contextWithUnknownAction = CamelCaseContextWithUnknownAction.forValue(configContextWithUnknownAction.getValue());
		
		for (int contextId : contexts.keySet()) {
			ElementContext context = contexts.get(contextId);
			if (contextId == 0) {
				// this context collects all 'individual' elements that do NOT belong together 
				for (StrucElement element : context.elements) {
					if (element.textLength > minFieldLength) {
						boolean requireUpperAfterLower = !element.isStructureNameOrAlias();
						applyCamelCaseTo(code, element, requireUpperAfterLower, requireApprovalForSureMatch);
					}
				}
				
			} else {
				// if a context contains at least one sure CamelCase name, and CamelCase 'candidates' are known for all fields in 
				// the context, then put all 'candidates' to CamelCase as well
				HashSet<StrucElement> sureElements = new HashSet<>();
				HashSet<StrucElement> candidates = new HashSet<>();
				context.clearCount();
				
				for (StrucElement element : context.elements) {
					CamelCaseNames knownNames = getKnownNamesFor(element);
					String textBit = element.getTokenTextBitWithoutParamPrefix();
					
					boolean requireUpperAfterLower = !element.isStructureNameOrAlias();
					if (element.textLength >= minFieldLength && knownNames.applyCamelCaseTo(textBit, requireUpperAfterLower, requireApprovalForSureMatch, parentProfile) != null) {
						++context.sureCount;
						sureElements.add(element);
					} else if (knownNames.applyCamelCaseTo(textBit, false, false, parentProfile) != null) {
						++context.candidateCount;
						candidates.add(element);
					} else {
						++context.failCount;
					}
				}

				boolean changeSure;
				boolean changeCandidates;
				boolean requireApprovalForContext;
				if (context.failCount == 0) {
					changeSure = true;
					changeCandidates = (contextAllKnownAction == CamelCaseContextAllKnownAction.CHANGE_ALL_KNOWN)
										 || (contextAllKnownAction == CamelCaseContextAllKnownAction.CHANGE_ALL_APPROVED);
					requireApprovalForContext = (contextAllKnownAction == CamelCaseContextAllKnownAction.CHANGE_ALL_APPROVED)
													 || (contextAllKnownAction == CamelCaseContextAllKnownAction.CHANGE_SURE_ONLY && requireApprovalForSureMatch);
				} else {
					changeSure = (contextWithUnknownAction != CamelCaseContextWithUnknownAction.CHANGE_NONE);
					changeCandidates = (contextWithUnknownAction == CamelCaseContextWithUnknownAction.CHANGE_ALL_KNOWN)
										 || (contextWithUnknownAction == CamelCaseContextWithUnknownAction.CHANGE_ALL_APPROVED);
					requireApprovalForContext = (contextWithUnknownAction == CamelCaseContextWithUnknownAction.CHANGE_ALL_APPROVED)
													 || (contextWithUnknownAction == CamelCaseContextWithUnknownAction.CHANGE_SURE_ONLY && requireApprovalForSureMatch);
				}

				// change the candidates if a sure CamelCase name was found and all other names are at least candidates
				if (changeSure && context.sureCount > 0) { 
					for (StrucElement sureElement : sureElements) {
						boolean requireUpperAfterLower = !sureElement.isStructureNameOrAlias();
						applyCamelCaseTo(code, sureElement, requireUpperAfterLower, requireApprovalForContext);
					} 
				}
				
				if (changeCandidates && context.sureCount > 0 && context.candidateCount > 0) {
					for (StrucElement candidate : candidates) {
						applyCamelCaseTo(code, candidate, false, requireApprovalForContext);
					}
				}
			}
		}
	}
	
	private void applyCamelCaseTo(Code code, StrucElement element, boolean requireUpperAfterLower, boolean requireApproval) {
		String textBit = element.getTokenTextBitWithoutParamPrefix();
		if (AbapCult.isMixedCase(textBit)) {
			CamelCaseDeviationAction deviationAction = CamelCaseDeviationAction.forValue(configDeviationAction.getValue());
			if (deviationAction == CamelCaseDeviationAction.NONE) {
				return;
			} 
			requireApproval = (deviationAction == CamelCaseDeviationAction.CHANGE_IF_APPROVED);
		}

		CamelCaseNames knownNames = getKnownNamesFor(element);
		String changedTextBit = knownNames.applyCamelCaseTo(textBit, requireUpperAfterLower, requireApproval, parentProfile);
		if (changedTextBit != null && element.replaceTokenTextBitAddingParamPrefix(changedTextBit)) {
			code.addRuleUse(this, element.token.getParentCommand());
		}
	}
	
	private CamelCaseNames getKnownNamesFor(StrucElement element) {
		return element.isStructureNameOrAlias() ? CamelCaseNames.getViewNames() : CamelCaseNames.getFieldNames();
	}
}
