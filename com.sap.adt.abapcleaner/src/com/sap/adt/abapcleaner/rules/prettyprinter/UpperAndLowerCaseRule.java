package com.sap.adt.abapcleaner.rules.prettyprinter;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class UpperAndLowerCaseRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use the Pretty Printer before activating", "#use-the-pretty-printer-before-activating"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use your Pretty Printer team settings", "#use-your-pretty-printer-team-settings"),
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Use uppercase for keywords and lowercase for operands", "abenlower_upper_case_guidl.htm") };
	
	@Override
	public RuleID getID() { return RuleID.UPPER_AND_LOWER_CASE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.PRETTY_PRINTER; }

	@Override
	public String getDisplayName() { return "Convert upper and lower case"; }

	@Override
	public String getDescription() { return "Changes ABAP keywords and identifiers to upper or lower case, depending on their context inside or outside the CLASS ... DEFINITION section."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2020, 12, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() {
		// While the Clean ABAP style guide clearly states that Pretty Printer should be USED, and teams should align on  
		// common upper/lower case settings, it explicitly refrains from providing clear guidance on WHAT should be upper
		// or lower case (cp. https://github.com/SAP/styleguides/blob/main/clean-abap/sub-sections/UpperVsLowerCase.md and
		// https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md#use-your-pretty-printer-team-settings).
		// Therefore, this rule is NOT activated in the 'essential' profile, thus requiring users and teams to deliberately 
		// activate and configure it.
		return false; 
	}

	@Override
   public String getExample() {
      return "" 
   	   + LINE_SEP + "class CL_UPPER_AND_LOWER_CASE definition public create protected." 
   	   + LINE_SEP + "  public section." 
   	   + LINE_SEP + "    methods pretty_print_case" 
   	   + LINE_SEP + "      importing !iv_change_camel_case type abap_bool." 
   	   + LINE_SEP + "  PRIVATE SECTION." 
   	   + LINE_SEP + "    DATA mv_change_definition TYPE ABAP_BOOL." 
   	   + LINE_SEP + "    methods INSERT ##SHADOW[insert]."
   	   + LINE_SEP + "endclass." 
   	   + LINE_SEP + "" 
   	   + LINE_SEP + "CLASS CL_UPPER_AND_LOWER_CASE IMPLEMENTATION." 
			+ LINE_SEP + "  method pretty_print_case." 
			+ LINE_SEP + "    constants lc_any_constant value 'abcde' ##no_text." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_counter type i." 
			+ LINE_SEP + "    DATA lvMixedCaseIdentifier TYPE i." 
			+ LINE_SEP + "    DATA(lo_inline) = get_object( ) ##needed." 
			+ LINE_SEP 
			+ LINE_SEP + "    IF iv_count = abap_true AND io_inline IS NOT BOUND." 
			+ LINE_SEP + "      loop at mts_data ASSIGNING FIELD-SYMBOL(<ls_data>)." 
			+ LINE_SEP + "        lv_counter += 1." 
			+ LINE_SEP + "      endloop." 
			+ LINE_SEP + "    ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "    SORT LT_ITEM BY FISCAL_YEAR ASCENDING" 
			+ LINE_SEP + "                    PERIOD      DESCENDING" 
			+ LINE_SEP + "                    ANY_FLAG    DESCENDING." 
			+ LINE_SEP + "  ENDMETHOD."
	      + LINE_SEP + "ENDCLASS.";
   }

	private static final String[] selection = new String[] { "(unchanged)", "lower case", "upper case" };
	final ConfigEnumValue<DeriveCaseMethod> configDeriveCaseMethod = new ConfigEnumValue<DeriveCaseMethod>(this, "DeriveCaseMethod", "Auto-determine upper/lower case", new String[] { "do not auto-determine", "derive from first class definition / method implementation statement", "derive from majority of tokens" }, DeriveCaseMethod.NONE, DeriveCaseMethod.NONE, LocalDate.of(2022, 6, 24));
	final ConfigEnumValue<CaseStyle> configDefinitionKeywordStyle = new ConfigEnumValue<CaseStyle>(this, "DefinitionKeywordStyle", "Keywords in CLASS ... DEFINITION section", selection, CaseStyle.UPPER_CASE, CaseStyle.UPPER_CASE, LocalDate.of(2021, 8, 15));
	final ConfigEnumValue<CaseStyle> configDefinitionIdentifierStyle = new ConfigEnumValue<CaseStyle>(this, "DefinitionIdentifierStyle", "Identifiers in CLASS ... DEFINITION section", selection, CaseStyle.LOWER_CASE, CaseStyle.LOWER_CASE, LocalDate.of(2021, 8, 15));
	final ConfigEnumValue<CaseStyle> configImplementationKeywordStyle = new ConfigEnumValue<CaseStyle>(this, "ImplementationKeywordStyle", "Keywords in all other places", selection, CaseStyle.UPPER_CASE, CaseStyle.UPPER_CASE, LocalDate.of(2021, 8, 15));
	final ConfigEnumValue<CaseStyle> configImplementationIdentifierStyle = new ConfigEnumValue<CaseStyle>(this, "ImplementationIdentifierStyle", "Identifiers in all other places", selection, CaseStyle.LOWER_CASE, CaseStyle.LOWER_CASE, LocalDate.of(2021, 8, 15));
	final ConfigEnumValue<CaseStyle> configPragmaStyle = new ConfigEnumValue<CaseStyle>(this, "PragmaStyle", "Pragmas", selection, CaseStyle.UPPER_CASE, CaseStyle.UNCHANGED, LocalDate.of(2023, 3, 18));
	final ConfigEnumValue<CaseStyle> configPragmaParameterStyle = new ConfigEnumValue<CaseStyle>(this, "PragmaParameterStyle", "Pragma parameters", selection, CaseStyle.UPPER_CASE, CaseStyle.UNCHANGED, LocalDate.of(2023, 3, 18));
	final ConfigBoolValue configKeepMixedCaseInIdentifiers = new ConfigBoolValue(this, "KeepMixedCaseInIdentifiers", "Keep camel case identifiers", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configDeriveCaseMethod, configDefinitionKeywordStyle, configDefinitionIdentifierStyle, configImplementationKeywordStyle, configImplementationIdentifierStyle, configPragmaStyle, configPragmaParameterStyle, configKeepMixedCaseInIdentifiers };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public UpperAndLowerCaseRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// attributes determined by prepare() and used in executeOn()
	private CaseStyle definitionKeywordStyle;
	private CaseStyle definitionIdentifierStyle;
	private CaseStyle implementationKeywordStyle;
	private CaseStyle implementationIdentifierStyle;
	private CaseStyle pragmaStyle;
	private CaseStyle pragmaParameterStyle;

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configDefinitionKeywordStyle || configValue == configDefinitionIdentifierStyle
		 || configValue == configImplementationKeywordStyle || configValue == configImplementationIdentifierStyle
		 || configValue == configPragmaStyle || configValue == configPragmaParameterStyle) {
			return DeriveCaseMethod.forValue(configDeriveCaseMethod.getValue()) == DeriveCaseMethod.NONE;
		} else {
			return true;
		}
	}

	@Override
	protected void prepare(Code code) {
		DeriveCaseMethod deriveCaseMethod = DeriveCaseMethod.forValue(configDeriveCaseMethod.getValue());
		if (deriveCaseMethod == DeriveCaseMethod.NONE) {
			// copy the configuration to the attributes used in the executeOn() method
			definitionKeywordStyle = CaseStyle.forValue(configDefinitionKeywordStyle.getValue());
			definitionIdentifierStyle = CaseStyle.forValue(configDefinitionIdentifierStyle.getValue());
			implementationKeywordStyle = CaseStyle.forValue(configImplementationKeywordStyle.getValue());
			implementationIdentifierStyle = CaseStyle.forValue(configImplementationIdentifierStyle.getValue());
			pragmaStyle = CaseStyle.forValue(configPragmaStyle.getValue());
			pragmaParameterStyle = CaseStyle.forValue(configPragmaParameterStyle.getValue());
			return;
		} 
		
		// the style shall be derived from the existing code
		int definitionKeywordUpperMinusLower = 0;
		int definitionIdentifierUpperMinusLower = 0;
		int implementationKeywordUpperMinusLower = 0;
		int implementationIdentifierUpperMinusLower = 0;
		int implFallbackKeywordUpperMinusLower = 0;
		int implFallbackIdentifierUpperMinusLower = 0;
		int pragmaUpperMinusLower = 0;
		int pragmaParameterUpperMinusLower = 0;

		// derive keyword and identifier style from the the definition/implementation section respectively 
		boolean definitionStyleFound = false;
		boolean implementationStyleFound = false;
		boolean pragmaStyleFound = false;
		boolean considerAllTokens = (deriveCaseMethod == DeriveCaseMethod.FROM_MAJORITY);
		
		Command command = code.firstCommand;
		while (command != null && (considerAllTokens || !definitionStyleFound || !implementationStyleFound || !pragmaStyleFound)) {
			// Commands must be processed here irrespective of command.isBlocked()!
			if (!command.isAbap()) {
				// skip command

			} else if (isInDefinitionSection(command)) {
				// derive settings for CLASS definition section 
				if (considerAllTokens || definitionKeywordUpperMinusLower == 0) {
					definitionKeywordUpperMinusLower += countUpperMinusLower(command, TokenType.KEYWORD, considerAllTokens, false);
				}
				if (considerAllTokens || definitionIdentifierUpperMinusLower == 0) { 
					definitionIdentifierUpperMinusLower += countUpperMinusLower(command, TokenType.IDENTIFIER, considerAllTokens, false);
				}
				definitionStyleFound = (!considerAllTokens && definitionKeywordUpperMinusLower != 0 && definitionIdentifierUpperMinusLower != 0);
			
			} else if (command.isClassImplementationStart() || command.isClassEnd() && command.getPrevSibling().isClassImplementationStart()) { 
				// SE80 puts the CLASS ... IMPLEMENTATION and ENDCLASS commands to upper case only; therefore only process them
				// for fallback purposes if no method implementation is found
				if (considerAllTokens || implFallbackKeywordUpperMinusLower == 0) {
					implFallbackKeywordUpperMinusLower += countUpperMinusLower(command, TokenType.KEYWORD, considerAllTokens, false);
				}
				if (considerAllTokens || implFallbackIdentifierUpperMinusLower == 0) { 
					implFallbackIdentifierUpperMinusLower += countUpperMinusLower(command, TokenType.IDENTIFIER, considerAllTokens, false);
				}

			} else { 
				// derive settings for implementation sections (excluding the CLASS ... IMPLEMENTATION and ENDCLASS commands)
				if (considerAllTokens || implementationKeywordUpperMinusLower == 0) {
					implementationKeywordUpperMinusLower += countUpperMinusLower(command, TokenType.KEYWORD, considerAllTokens, false);
				}
				if (considerAllTokens || implementationIdentifierUpperMinusLower == 0) { 
					implementationIdentifierUpperMinusLower += countUpperMinusLower(command, TokenType.IDENTIFIER, considerAllTokens, false);
				}
				implementationStyleFound = (!considerAllTokens && implementationKeywordUpperMinusLower != 0 && implementationIdentifierUpperMinusLower != 0);
			}

			// derive settings for pragmas 
			if (considerAllTokens || pragmaUpperMinusLower == 0) {
				pragmaUpperMinusLower += countUpperMinusLower(command, TokenType.PRAGMA, considerAllTokens, false);
			}
			if (considerAllTokens || pragmaParameterUpperMinusLower == 0) { 
				pragmaParameterUpperMinusLower += countUpperMinusLower(command, TokenType.PRAGMA, considerAllTokens, true);
			}
			pragmaStyleFound = (!considerAllTokens && pragmaUpperMinusLower != 0 && pragmaParameterUpperMinusLower != 0);

			command = command.getNextNonCommentCommand();
		}

		// if no method implementation was found, use fallback settings for implementation sections, which were derived from the 
		// CLASS ... IMPLEMENTATION and ENDCLASS commands themselves
		if (implementationKeywordUpperMinusLower == 0)
			implementationKeywordUpperMinusLower = implFallbackKeywordUpperMinusLower;
		if (implementationIdentifierUpperMinusLower == 0)
			implementationIdentifierUpperMinusLower = implFallbackIdentifierUpperMinusLower;
		
		definitionKeywordStyle = getCaseStyleFor(definitionKeywordUpperMinusLower);
		definitionIdentifierStyle = getCaseStyleFor(definitionIdentifierUpperMinusLower);
		implementationKeywordStyle = getCaseStyleFor(implementationKeywordUpperMinusLower);
		implementationIdentifierStyle = getCaseStyleFor(implementationIdentifierUpperMinusLower);
		pragmaStyle = getCaseStyleFor(pragmaUpperMinusLower);
		pragmaParameterStyle = getCaseStyleFor(pragmaParameterUpperMinusLower);
	}

	private boolean isInDefinitionSection(Command command) {
		return command.isInClassDefinition() || command.isInInterfaceDefinition();
	}

	private int countUpperMinusLower(Command command, TokenType tokenType, boolean considerAllTokens, boolean pragmaParametersOnly) {
		Token token = command.getFirstToken();
		int upperMinusLower = 0;
		while (token != null) {
			if (token.type == tokenType || (tokenType == TokenType.KEYWORD && token.isTextualComparisonOp())) {
				String text = token.getText();
				if (tokenType == TokenType.PRAGMA) {
					// for a pragma, either process the pragma itself or its parameter(s), e.g. in case of '##SHADOW[insert]'
					text = pragmaParametersOnly ? ABAP.getPragmaParameters(text) : ABAP.getPragmaWithoutParameters(text);
					if (StringUtil.isNullOrEmpty(text)) {
						token = token.getNextNonCommentToken();
						continue;
					}
				}
				String textUpper = AbapCult.toUpper(text);
				String textLower = AbapCult.toLower(text);
				
				// only count fully upper or fully lower cases; note that apart from CamelCase identifiers, 
				// there is also the special case of the FIELD-SYMBOL "<>" which fails both if conditions
				if (text.equals(textUpper) && !text.equals(textLower))
					++upperMinusLower;
				else if (text.equals(textLower) && !text.equals(textUpper))
					--upperMinusLower;
				
				if (!considerAllTokens && upperMinusLower != 0)
					break;
			}
			token = token.getNextNonCommentToken();
		}
		return upperMinusLower;
	}

	private CaseStyle getCaseStyleFor(int upperMinusLower) {
		if (upperMinusLower > 0)
			return CaseStyle.UPPER_CASE;
		else if (upperMinusLower < 0)
			return CaseStyle.LOWER_CASE;
		else 
			return CaseStyle.UNCHANGED;
	}
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) {
		// we rely on the RND Parser token refinement for ABAP SQL; without that, ABAP SQL statements would have to be 
		// skipped with "if (command.isAbapSqlOperation()) return false" because token types are very complex to identify 
		// here, see the list of keywords like "COUNT(" in TokenTypeRefiner.refine():

		// get settings for this Command, depending on its context
		boolean isInDefinitionSection = isInDefinitionSection(command);
		CaseStyle keywordStyle = isInDefinitionSection ? definitionKeywordStyle : implementationKeywordStyle;
		CaseStyle identifierStyle = isInDefinitionSection ? definitionIdentifierStyle : implementationIdentifierStyle;
		boolean keepMixedCaseIdentifiers = configKeepMixedCaseInIdentifiers.getValue();
		boolean isInOOContext = command.isInOOContext();
		
		boolean changedCommand = false;
		Token token = command.getFirstToken();
		
		while (token != null) {
			String text = token.getText();
			String changedText = null;
			
			if (token.isTextSymbol()) {
				changedText = changeTextSymbol(text, keywordStyle, identifierStyle);
				
			} else if ((token.type == TokenType.KEYWORD || token.isTextualComparisonOp()) && keywordStyle != CaseStyle.UNCHANGED) {
				changedText = (keywordStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(text) : AbapCult.toUpper(text); 
				
			} else if (token.type == TokenType.IDENTIFIER && identifierStyle != CaseStyle.UNCHANGED) {
				if (keepMixedCaseIdentifiers) {
					changedText = changeIdentifierKeepingMixedCase(text, identifierStyle, isInOOContext);
				} else {
					changedText = (identifierStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(text) : AbapCult.toUpper(text);
				}
				
			} else if (token.type == TokenType.PRAGMA && (pragmaStyle != CaseStyle.UNCHANGED || pragmaParameterStyle != CaseStyle.UNCHANGED)) {
				// split the text into the pragma and its parameters, e.g. for '##SHADOW[insert]' and process both parts independently
				String pragma = ABAP.getPragmaWithoutParameters(text);
				String parameters = ABAP.getPragmaParameters(text);
				if (pragmaStyle != CaseStyle.UNCHANGED) {
					pragma = (pragmaStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(pragma) : AbapCult.toUpper(pragma);
				}
				if (pragmaParameterStyle != CaseStyle.UNCHANGED && !StringUtil.isNullOrEmpty(parameters)) {
					if (keepMixedCaseIdentifiers) {
						parameters = changeIdentifierKeepingMixedCase(parameters, pragmaParameterStyle, isInOOContext);
					} else {
						parameters = (pragmaParameterStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(parameters) : AbapCult.toUpper(parameters);
					}
				}
				changedText = pragma + parameters;
				
			} // otherwise, keep changedText == null
	
			if (changedText != null && !text.equals(changedText)) {
				token.setText(changedText, false);
				changedCommand = true;
			}
			
			token = token.getNextNonCommentToken();
		}
		return changedCommand;
	}
	
	private String changeTextSymbol(String text, CaseStyle keywordStyle, CaseStyle identifierStyle) {
		// text symbols such as TEXT-001 or TEXT-a01 are stored as one Token, but actually consist of 
		// a keyword (TEXT), an operator (-) and an alphanumeric identifier (001, a01, a_2 etc.)
		
		String prefix = text.substring(0, ABAP.TEXT_SYMBOL_PREFIX.length());
		if (keywordStyle != CaseStyle.UNCHANGED) 
			prefix = (keywordStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(prefix) : AbapCult.toUpper(prefix);

		String id = text.substring(ABAP.TEXT_SYMBOL_PREFIX.length());
		if (identifierStyle != CaseStyle.UNCHANGED) 
			id = (identifierStyle == CaseStyle.LOWER_CASE) ? AbapCult.toLower(id) : AbapCult.toUpper(id);
				
		return prefix + id;
	}

	private String changeIdentifierKeepingMixedCase(String text, CaseStyle identifierStyle, boolean isInOOContext) {
		// split composed identifiers "any_class=>any_structure-any_component", or "any_class=>any_method(" into multiple parts
		// in order to apply the rule to each part; otherwise, "IF_ANY_INTERFACE~any_method" would be considered a 'mixed case'
		StringBuilder changedTextBuilder = new StringBuilder();
		ArrayList<String> textBits = ABAP.splitIdentifier(text, false, isInOOContext);

		for (String textBit : textBits) {
			// since mixed case identifiers should be kept in this method, only change the text if it is 
			// fully upper case or fully lower case
			String changedBit;
			if (isMixedCase(textBit)) {
				changedBit = textBit;
			} else if (identifierStyle == CaseStyle.LOWER_CASE) {
				changedBit = AbapCult.toLower(textBit);
			} else {
				changedBit = AbapCult.toUpper(textBit);
			}
			
			changedTextBuilder.append(changedBit);
		}
		return changedTextBuilder.toString();
	}

	private boolean isMixedCase(String text) {
		return !AbapCult.toUpper(text).equals(text) && !AbapCult.toLower(text).equals(text);
	}
}
