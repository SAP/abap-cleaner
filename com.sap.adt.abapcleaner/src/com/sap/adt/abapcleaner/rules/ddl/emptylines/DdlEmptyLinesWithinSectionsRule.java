package com.sap.adt.abapcleaner.rules.ddl.emptylines;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Section;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;

public class DdlEmptyLinesWithinSectionsRule extends RuleForDdlCommands {
	@Override
	public RuleID getID() { return RuleID.DDL_EMPTY_LINES_WITHIN; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Standardize empty lines within sections"; }

	@Override
	public String getDescription() { return "Standardizes empty lines within the sections for entity parameters, joins, associations and the select list."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 17); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "@EndUserText.label: 'Any Description'"
				+ LINE_SEP + "define view entity C_AnyEntity"
				+ LINE_SEP + "  with parameters"
				+ LINE_SEP + "    P_AnyParam:   any_parameter_type,"
				+ LINE_SEP + "    @Annotation.subAnno: 'value'"
				+ LINE_SEP + "    P_OtherParam: other_type"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from I_AnyEntity as AnyAlias"
				+ LINE_SEP + "    inner join I_OtherEntity2      as OtherAlias on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + "    left outer join I_ThirdEntity2 as ThirdAlias on  AnyAlias.IdField      = ThirdAlias.IdField"
				+ LINE_SEP + "                                                 and AnyAlias.SubIdField   = ThirdAlias.SubIdField"
				+ LINE_SEP + "                                                 and AnyAlias.ThirdIdField = ThirdAlias.ThirdIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_FourthEntity2 as _FourthAlias on AnyAlias.IdField  = _FourthAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..1] to I_FifthEntity2  as _FifthAlias  on AnyAlias.IdField  = _FifthAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [1..*] to I_SixthEntity2  as _SixthAlias  on  AnyAlias.IdField = _SixthAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + "  key AnyAlias.OtherKeyField,"
				+ LINE_SEP + "      sum(OtherAlias.AnyNonKeyField),"
				+ LINE_SEP + "      @Annotation.subAnno: 'value'"
				+ LINE_SEP + "      avg(OtherAlias.OtherNonKeyField)  as OtherNonKeyField"
				+ LINE_SEP + "      @<Annotation.otherSubAnno: 'this annotation refers to the previous element!',"
				+ LINE_SEP + "      max(OtherAlias.ThirdNonKeyField)  as ThirdNonKeyField,"
				+ LINE_SEP + "      min(ThirdAlias.FourthNonKeyField) as FourthNonKeyField,"
				+ LINE_SEP + "      max(ThirdAlias.FifthNonKeyField)  as FifthNonKeyField,"
				+ LINE_SEP + "      _FourthAlias,"
				+ LINE_SEP + "      _FifthAlias,"
				+ LINE_SEP + "      _SixthAlias"
				+ LINE_SEP + "}"
				+ LINE_SEP + "where  AnyNonKeyField  > 10"
				+ LINE_SEP + "  and OtherNonKeyField = 'A'"
				+ LINE_SEP + "group by AnyAlias.AnyKeyField"
				+ LINE_SEP + "         AnyAlias.OtherKeyField"
				+ LINE_SEP + "having sum(OtherAilas.AnyNonKeyField)     > 100"
				+ LINE_SEP + "   and avg(OtherAlias.ThirdNonKeyField)   < 42"
				+ LINE_SEP + "   and max(ThirdAlias.FourthNonKeyField) >= 'Z'"
				+ LINE_SEP + ""
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_AnyEntity2         as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    inner join I_OtherEntity as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.IdField = OtherAlias.IdField"
				+ LINE_SEP + "    left outer to one join I_ThirdEntity as ThirdAlias"
				+ LINE_SEP + "      on  AnyAlias.IdField    = ThirdAlias.IdField"
				+ LINE_SEP + "      and AnyAlias.SubIdField = ThirdAlias.SubIdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_FourthEntity as _FourthAlias"
				+ LINE_SEP + "    on AnyAlias.IdField = _FourthAlias.IdField"
				+ LINE_SEP + "  // comment on association"
				+ LINE_SEP + "  association [0..*] to I_FifthEntity as _FifthAlias"
				+ LINE_SEP + "    on AnyAlias.IdField = _FifthAlias.IdField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "  key AnyAlias.OtherKeyField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      OtherAlias.AnyNonKeyField"
				+ LINE_SEP + "}";
	}
	
	private static final String[] condenseModeSelection = new String[] { "always", "if all are one-liners", "if all are detached one-liners", "never" };
   
   final ConfigIntValue configSurroundParameterLineCountMin = new ConfigIntValue(this, "SurroundParameterLineCountMin", "Add empty lines around entity parameters that span", "lines or more", 1, 2, 99);
   final ConfigIntValue configSurroundJoinLineCountMin = new ConfigIntValue(this, "SurroundJoinLineCountMin", "Add empty lines around JOINs that span", "lines or more", 1, 2, 99);
   final ConfigIntValue configSurroundAssociationLineCountMin = new ConfigIntValue(this, "SurroundAssociationLineCountMin", "Add empty lines around ASSOCIATIONs that span", "lines or more", 1, 2, 99);
   final ConfigIntValue configSurroundElementLineCountMin = new ConfigIntValue(this, "SurroundElementLineCountMin", "Add empty lines around select list elements that span", "lines or more", 1, 2, 99);
   final ConfigIntValue configSurroundClauseLineCountMin = new ConfigIntValue(this, "SurroundClauseLineCountMin", "Add empty lines around WHERE etc. clauses that span", "lines or more", 1, 2, 99);

	final ConfigBoolValue configDetachKeyFields = new ConfigBoolValue(this, "DetachKeyFields", "Add empty line below KEY fields in select list", true);
	final ConfigBoolValue configDetachExposedAssociations = new ConfigBoolValue(this, "DetachExposedAssociations", "Add empty line above associations in select list", true);
	
	final ConfigEnumValue<DdlCondenseMode> configCondenseParameters = new ConfigEnumValue<DdlCondenseMode>(this, "CondenseParameters", "Remove empty lines between single-line parameters:", condenseModeSelection, DdlCondenseMode.values(), DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
	final ConfigEnumValue<DdlCondenseMode> configCondenseJoins = new ConfigEnumValue<DdlCondenseMode>(this, "CondenseJoins", "Remove empty lines between single-line JOINs:", condenseModeSelection, DdlCondenseMode.values(), DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
	final ConfigEnumValue<DdlCondenseMode> configCondenseAssociations = new ConfigEnumValue<DdlCondenseMode>(this, "CondenseAssociations", "Remove empty lines between single-line ASSOCIATIONs:", condenseModeSelection, DdlCondenseMode.values(), DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
	final ConfigEnumValue<DdlCondenseMode> configCondenseElements = new ConfigEnumValue<DdlCondenseMode>(this, "CondenseElements", "Remove empty lines between single-line select list elements:", condenseModeSelection, DdlCondenseMode.values(), DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS);
	final ConfigEnumValue<DdlCondenseMode> configCondenseClauses = new ConfigEnumValue<DdlCondenseMode>(this, "CondenseClauses", "Remove empty lines between single-line WHERE etc. clauses:", condenseModeSelection, DdlCondenseMode.values(), DdlCondenseMode.NEVER);

	final ConfigInfoValue configCondenseWarning = new ConfigInfoValue(this, "Warning: With this setting, intentional visual grouping might get lost.", ConfigInfoStyle.WARNING);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configSurroundParameterLineCountMin, configSurroundJoinLineCountMin, configSurroundAssociationLineCountMin, configSurroundElementLineCountMin, configSurroundClauseLineCountMin,
			configDetachKeyFields, configDetachExposedAssociations,
			configCondenseParameters, configCondenseJoins, configCondenseAssociations, configCondenseElements, configCondenseClauses, configCondenseWarning };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlEmptyLinesWithinSectionsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configCondenseWarning) {
			return (DdlCondenseMode.forValue(configCondenseParameters.getValue()).risksLossOfVisualGrouping()
					|| DdlCondenseMode.forValue(configCondenseJoins.getValue()).risksLossOfVisualGrouping()
					|| DdlCondenseMode.forValue(configCondenseAssociations.getValue()).risksLossOfVisualGrouping()
					|| DdlCondenseMode.forValue(configCondenseElements.getValue()).risksLossOfVisualGrouping()
					|| DdlCondenseMode.forValue(configCondenseClauses.getValue()).risksLossOfVisualGrouping());
		} else {
			return true;
		}
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (command.isCommentLine())
			return false;

		Command prevSibling = command.getPrevNonCommentSibling();
		try {
			if (command.startsDdlEntityParameters()) {
				// entity parameters and their annotations
				executeOnSections(code, getListElements(command, false).get(0), configSurroundParameterLineCountMin.getValue(), 
						DdlCondenseMode.forValue(configCondenseParameters.getValue()));
			
			} else if (command.startsDdlJoin() && (prevSibling == null || !prevSibling.startsDdlJoin())) { // "== null" pro forma
				// JOINs
				executeOnSections(code, getConsecutiveJoins(command), configSurroundJoinLineCountMin.getValue(),
						DdlCondenseMode.forValue(configCondenseJoins.getValue()));

			} else if (command.startsDdlAssociation() && (prevSibling == null || !prevSibling.startsDdlAssociation())) { // "== null" pro forma
				// ASSOCIATIONs
				executeOnSections(code, getConsecutiveAssociations(command), configSurroundAssociationLineCountMin.getValue(), 
						DdlCondenseMode.forValue(configCondenseAssociations.getValue()));

			} else if (command.startsDdlSelectList()) {
				// select list elements and their annotations (possibly in three groups: key fields, non-key fields, exposed associations)
				ArrayList<ArrayList<Section>> sectionsList = getListElements(command, true);
				for (ArrayList<Section> sections : sectionsList) {
					executeOnSections(code, sections, configSurroundElementLineCountMin.getValue(),
							DdlCondenseMode.forValue(configCondenseElements.getValue()));
				}
				
			} else if (command.endsDdlSelectListWithBrace()) {
				// clauses WHERE, GROUP BY, HAVING
				executeOnSections(code, getConsecutiveClauses(command.getNextNonCommentSibling()), configSurroundClauseLineCountMin.getValue(),
						DdlCondenseMode.forValue(configCondenseClauses.getValue()));
			}

		} catch (UnexpectedSyntaxException e) {
			// exceptions are only thrown in getListElements() etc., where Sections are created, so nothing was changed yet at that point  
			return false;
		}
		
		return false;
	}

	private ArrayList<ArrayList<Section>> getListElements(Command parent, boolean isSelectList) throws UnexpectedSyntaxException {
		ArrayList<Section> sections = new ArrayList<>();
		Command command = parent.getFirstChild();
		
		// ignore comments, because they may refer to multiple elements
		if (command.isCommentLine())
			command = command.getNextNonCommentSibling();
		
		Command start = command;
		while (command != null) {
			Command next = command.getNextNonCommentSibling();
			Token lastCode = command.getLastCodeToken();
			// the section ends if this is the last non-command Command 
			// or if the Command ends with "," or ";" (which could also happen on @< annotations)
			if (next == null || lastCode.textEqualsAny(DDL.listElementSeparators)) {
				sections.add(Section.create(start, command));
				start = next;
			}
			command = next;
		} 

		// for the entity parameter list, simply return an ArrayList with sections as its only entry
		ArrayList<ArrayList<Section>> sectionsList = new ArrayList<>();
		if (!isSelectList) {
			sectionsList.add(sections);
			return sectionsList;
		}
		
		// for the select list, if key fields and/or associations shall be detached from non-key fields, 
		// split sections into three lists: a) key fields, b) non-key fields, c) associations

		// find start index of associations at the end of the select list (if any)
		int associationStartIndex = sections.size();
		if (configDetachExposedAssociations.getValue()) { // otherwise pretend there are no exposed associations 
			for (int i = sections.size() - 1; i >= 0; --i) {
				if (exposesAssociation(getMainElement(sections.get(i)))) {
					associationStartIndex = i;
				} else { 
					break;
				}
			}
		}

		ArrayList<Section> keyFields = new ArrayList<>();
		ArrayList<Section> nonKeyFields = new ArrayList<>();
		ArrayList<Section> associations = new ArrayList<>();
		boolean isInKeyFieldsAtStart = configDetachKeyFields.getValue(); // otherwise pretend there are no KEY fields
		for (int i = 0; i < sections.size(); ++i) {
			Section section = sections.get(i);

			// build key fields (if they shall be detached)
			if (isInKeyFieldsAtStart) {
				if (getMainElement(section).firstCodeTokenIsKeyword("KEY")) {
					keyFields.add(section);
					continue;
				} else if (i > 0) {
					// detach first non-key field
					addEmptyLineAbove(section);
				}
				isInKeyFieldsAtStart = false;
			} 
			
			// build non-key fields
			if (i < associationStartIndex) {
				nonKeyFields.add(section);
				continue;
			} else if (i > 0 && associations.size() == 0) {
				// detach first exposed association
				addEmptyLineAbove(section);
			}
			
			// build associations (if they shall be detached)
			associations.add(section);
		}

		// return an ArrayList with up to three section lists
		if (!keyFields.isEmpty())
			sectionsList.add(keyFields);

		if (!nonKeyFields.isEmpty())
			sectionsList.add(nonKeyFields);
		
		if (!associations.isEmpty())
			sectionsList.add(associations);

		return sectionsList;
	}

	private void addEmptyLineAbove(Section section) {
		Command firstCommand = section.firstCommand;
		Token firstToken = firstCommand.getStartOfAttachedComments().getFirstToken();
		if (firstToken.setLineBreaks(Math.max(firstToken.lineBreaks, 2))) {
			firstCommand.getParentCode().addRuleUse(this, firstCommand);
		}
	}

	private Command getMainElement(Section section) {
		Command command = section.firstCommand;
		while (command != null) {
			if (!command.isCommentLine() && !command.isDdlAnnotation())
				return command;
			else if (command == section.lastCommand) // pro forma
				return null;
			command = command.getNext();
		}
		return null;
	}
	
	private boolean exposesAssociation(Command command) {
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null || !firstCode.isIdentifier())
			return false;
		
		if (!firstCode.textStartsWith(DDL.UNDERSCORE_STRING) || !DDL.isIdentifier(firstCode.getText()))
			return false;

		Token nextCode = firstCode.getNextCodeSibling();
		return (nextCode == null || nextCode.textEquals(DDL.COMMA_SIGN_STRING));
	}
	
	private ArrayList<Section> getConsecutiveJoins(Command firstJoin) throws UnexpectedSyntaxException {
		ArrayList<Section> sections = new ArrayList<>();
		Command command = firstJoin;

		while (command != null && command.startsDdlJoin()) {
			// include comments
			Command start = command.getStartOfAttachedComments();
			sections.add(Section.create(start, command));

			command = command.getNextNonCommentSibling();
		} 
		return sections;
	}

	private ArrayList<Section> getConsecutiveAssociations(Command firstJoin) throws UnexpectedSyntaxException {
		ArrayList<Section> sections = new ArrayList<>();
		Command command = firstJoin;

		while (command != null && command.startsDdlAssociation()) {
			// include attached comments
			Command start = command.getStartOfAttachedComments();
			sections.add(Section.create(start, command));

			command = command.getNextNonCommentSibling();
		} 
		return sections;
	}

	private ArrayList<Section> getConsecutiveClauses(Command firstCandidate) throws UnexpectedSyntaxException {
		ArrayList<Section> sections = new ArrayList<>();
		Command command = firstCandidate;

		while (command != null && command.startsDdlClause()) {
			// include attached comments
			Command start = command.getStartOfAttachedComments();
			sections.add(Section.create(start, command));

			command = command.getNextNonCommentSibling();
		} 
		return sections;
	}

	private void executeOnSections(Code code, ArrayList<Section> sections, int surroundLineCountMin, DdlCondenseMode condenseMode) {
		if (sections == null || sections.size() < 2)
			return;
		
		// determine whether all sections shall be condensed, because all are one-liners 
		boolean condenseAll = false;
		if (condenseMode == DdlCondenseMode.IF_ONLY_ONE_LINERS || condenseMode == DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS) {
			condenseAll = true;
			Section firstSection = sections.get(0);
			for (Section section : sections) {
				if (getLineCount(section) > 1) {
					// at least one sections spans multiple lines
					condenseAll = false;
					break;
				} else if (condenseMode == DdlCondenseMode.IF_ONLY_DETACHED_ONE_LINERS && section != firstSection 
						&& section.firstCommand.getFirstTokenLineBreaks() < 2) {
					// at least one section is attached to the previous one; if empty lines are removed now, 
					// intentional visual grouping could be lost
					condenseAll = false;
					break;
				}
			}
		}
		
		Section sectionA = sections.get(0);
		int sectionALineCount = getLineCount(sectionA);
		
		for (int index = 1; index < sections.size(); ++index) {
			Section sectionB = sections.get(index);
			int sectionBLineCount = getLineCount(sectionB);

			// while comments attached to select list elements do not count towards the line count of this element,
			// the empty lines must nevertheless be put above these attached comments, not below them
			Token firstToken = sectionB.firstCommand.getStartOfAttachedComments().getFirstToken();
			boolean changed = false;
			if (condenseAll || condenseMode == DdlCondenseMode.ALWAYS && sectionALineCount == 1 && sectionBLineCount == 1) {
				changed = firstToken.setLineBreaks(1);
			} else if (sectionALineCount >= surroundLineCountMin || sectionBLineCount >= surroundLineCountMin) {
				changed = firstToken.setLineBreaks(Math.max(firstToken.lineBreaks, 2));
			} 

			if (changed) {
				code.addRuleUse(this, sectionB.firstCommand);
			}
			
			sectionA = sectionB;
			sectionALineCount = sectionBLineCount;
		}
	}
	
	private int getLineCount(Section section) {
		return section.firstCommand.getLineBreakSumUpTo(section.lastCommand.getNext()) - section.firstCommand.getFirstTokenLineBreaks() + 1;
	}
}
