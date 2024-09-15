package com.sap.adt.abapcleaner.rules.ddl.position;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlPosition;

public class DdlPositionDefineRule extends RuleForDdlPosition {
	public static final String displayName = "Break before DEFINE etc.";
	
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };
	
	@Override
	public RuleID getID() { return RuleID.DDL_POSITION_DEFINE; }

	@Override
	public String getDisplayName() { return displayName; }

	@Override
	public String getDescription() { return "Standardizes line breaks and indentation before DEFINE / EXTEND etc. keywords, entity name and WITH PARAMETERS."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 9); }

	@Override
	public RuleReference[] getReferences() { return references; }

	public RuleID[] getDependentRules() { 
		// the following rule reuses some of this rule's settings; it is therefore not strictly necessary, but more intuitive to put it further down the rule list
		return new RuleID[] { RuleID.DDL_ALIGN_ENTITY_PARAMETERS }; 
	}

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@EndUserText.label: 'Any Description' define"
				+ LINE_SEP + "   view"
				+ LINE_SEP + " entity"
				+ LINE_SEP + "C_AnyEntity with"
				+ LINE_SEP + "parameters"
				+ LINE_SEP + "// comment"
				+ LINE_SEP + "P_AnyParam   : AnyType,"
				+ LINE_SEP + "  P_OtherParam : OtherType,"
				+ LINE_SEP + "    P_ThirdParam : ThirdType"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer to one join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + "}";
	}
	
	final ConfigEnumValue<DdlLineBreakWithoutNever> configBreakBeforeDefine = new ConfigEnumValue<DdlLineBreakWithoutNever>(this, "BreakBeforeDefine", "Break before DEFINE etc. keywords:", lineBreakSelectionWithoutNever, DdlLineBreakWithoutNever.values(), DdlLineBreakWithoutNever.ALWAYS);
	final ConfigIntValue configDefineIndent = new ConfigIntValue(this, "DefineIndent", "Indent if breaking:", "", 0, 0, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeEntityName = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeEntityName", "Break before entity name:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.NEVER);
	final ConfigIntValue configEntityNameIndent = new ConfigIntValue(this, "EntityNameIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	
	final ConfigEnumValue<DdlLineBreak> configBreakBeforeWithParams = new ConfigEnumValue<DdlLineBreak>(this, "BreakBeforeWithParams", "Break before WITH PARAMETERS:", lineBreakSelection, DdlLineBreak.values(), DdlLineBreak.ALWAYS);
	final ConfigIntValue configWithParamsIndent = new ConfigIntValue(this, "WithParamsIndent", "Indent if breaking:", "", 0, 2, MAX_INDENT);
	public final ConfigIntValue configParamsIndent = new ConfigIntValue(this, "ParamsIndent", "Indent of parameters:", "", 0, 4, MAX_INDENT);

	private final ConfigValue[] configValues = new ConfigValue[] { configBreakBeforeDefine, configDefineIndent, configBreakBeforeEntityName, configEntityNameIndent, configBreakBeforeWithParams, configWithParamsIndent, configParamsIndent };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public int getParamsIndent() { return configParamsIndent.getValue(); }
	
	public DdlPositionDefineRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		boolean changed = false;

		if (command.isDdlAnnotation())
			return false;
		
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) // pro forma
			return false;
		
		DdlLineBreak breakBeforeDefine = getLineBreak(DdlLineBreakWithoutNever.forValue(configBreakBeforeDefine.getValue()));
		DdlLineBreak breakBeforeEntityName = DdlLineBreak.forValue(configBreakBeforeEntityName.getValue());
		DdlLineBreak breakBeforeWithParams = DdlLineBreak.forValue(configBreakBeforeWithParams.getValue());
		
		// break before DEFINE etc., condense definition keywords and (un)break before entity name
		Token entityName = command.getDdlOrDclEntityNameToken();
		if (entityName != null) {
			boolean emptyLine = (command.getPrev() != null);
			changed |= breakBefore(firstCode, breakBeforeDefine, emptyLine, configDefineIndent.getValue());
			changed |= condense(firstCode, entityName.getPrevCodeToken());

			changed |= breakBefore(entityName, breakBeforeEntityName, false, configEntityNameIndent.getValue());
		}
		
		// (un)break before "WITH PARAMETERS"
		Token parametersToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "WITH", "PARAMETERS");
		if (parametersToken != null) {
			Token withToken = parametersToken.getPrevCodeSibling();
			changed |= breakBefore(withToken, breakBeforeWithParams, false, configWithParamsIndent.getValue());
			changed |= condense(withToken, parametersToken);
			
			// set indent of parameters, their annotations and comments
			Command paramCommand = command.getFirstChild();
			int paramIndent = configParamsIndent.getValue();
			while (paramCommand != null) {
				// do not move comments at the end of the parameter list, as they probably belong to the following Command
				if (paramCommand.isCommentLine() && paramCommand.getNextNonCommentSibling() == null)
					break;
				paramCommand = setNewIndent(paramCommand, paramIndent);
				if (paramCommand == null) // pro forma
					break;
				paramCommand = paramCommand.getNextSibling();
			}
		} 

		return changed;
	}
}
