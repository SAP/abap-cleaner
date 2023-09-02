package com.sap.adt.abapcleaner.rules.alignment;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.StringUtil;

public class AlignAssignmentsRule extends Rule {
	private enum Columns {
		IDENTIFIER, 
		ASSIGNMENT_OP, 
		REST_OF_COMMAND;

		public int getValue() { return this.ordinal(); }
	}

	private static final int MAX_COLUMN_COUNT = 3;

	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Align assignments to the same object, but not to different ones", "#align-assignments-to-the-same-object-but-not-to-different-ones") };

	@Override
	public RuleID getID() { return RuleID.ALIGN_ASSIGNMENTS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align assignments to the same object"; }

	@Override
	public String getDescription() { return "Aligns assignments to the same object, e.g. to various components of the same structure."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 3); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }

	@Override
	public String getExample() {
		return  LINE_SEP + "  METHOD align_assignments_to_same_obj." 
				+ LINE_SEP + "    ro_instance->mv_name = iv_name." 
				+ LINE_SEP + "    ro_instance->mv_id = iv_id." 
				+ LINE_SEP + "    ro_instance->mv_price = iv_price." 
				+ LINE_SEP 
				+ LINE_SEP + "    ls_struc-component = 1." 
				+ LINE_SEP + "    ls_struc-comp2 = 'a'." 
				+ LINE_SEP + "    ls_struc-start_date+6(2) = 31." 
				+ LINE_SEP + "    ls_struc-c3 = VALUE #( )." 
				+ LINE_SEP 
				+ LINE_SEP + "    <ls_field_symbol>-component = 1." 
				+ LINE_SEP + "    <ls_field_symbol>-comp2 = 'a'." 
				+ LINE_SEP + "    <ls_field_symbol>-c3 = VALUE #( )." 
				+ LINE_SEP 
				+ LINE_SEP + "    \" alignment across comments and empty lines (depending on configuration):" 
				+ LINE_SEP + "    ls_struc-component += 1." 
				+ LINE_SEP + "    \"comment" 
				+ LINE_SEP + "    ls_struc-comp2 = 'a'." 
				+ LINE_SEP 
				+ LINE_SEP + "    ls_struc-c3 = VALUE #( )." 
				+ LINE_SEP 
				+ LINE_SEP + "    \" comment" 
				+ LINE_SEP + "    ls_struc-comp4 -= 1." 
				+ LINE_SEP + "  ENDMETHOD.";
	}

	final ConfigBoolValue configAlignAcrossEmptyLines = new ConfigBoolValue(this, "AlignAcrossEmptyLines", "Align across empty lines", true);
	final ConfigBoolValue configAlignAcrossCommentLines = new ConfigBoolValue(this, "AlignAcrossCommentLines", "Align across comment lines", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignAcrossEmptyLines, configAlignAcrossCommentLines };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignAssignmentsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		boolean alignAcrossEmptyLines = configAlignAcrossEmptyLines.getValue();
		boolean alignAcrossCommentLines = configAlignAcrossCommentLines.getValue();

		String lastStrucVar = null;
		Command sectionStart = null;
		int commandsInSection = 0;

		Command command = code.firstCommand;
		Command sectionEnd = null;
		boolean sectionBroken = false;
		while (command != null) {
			commandForErrorMsg = command;
			if (isCommandBlocked(command)) {
				sectionBroken = true;
				if (sectionEnd == null)
					sectionEnd = command;
				command = command.getNext();
				continue;
			}

			// skip comment line without breaking the section (if any)
			if (command.getFirstToken() == null
					|| (alignAcrossCommentLines && command.getFirstToken().isCommentLine() && (alignAcrossEmptyLines || command.getFirstTokenLineBreaks() <= 1))) {
				command = command.getNext();
				continue;
			}

			String strucVar = command.isAssignment(false) ? command.getFirstToken().getStructureVariable() : null;
			if (!StringUtil.equalsCheckingForNull(lastStrucVar, strucVar) || (!alignAcrossEmptyLines && command.getFirstTokenLineBreaks() > 1) || sectionBroken) {
				if (!StringUtil.isNullOrEmpty(lastStrucVar) && commandsInSection > 1) {
					if (sectionEnd == null)
						sectionEnd = command;
					try {
						alignSection(code, sectionStart, sectionEnd);
					} catch (UnexpectedSyntaxBeforeChanges ex) {
						// log and continue
						ex.addToLog();
					}
				}

				lastStrucVar = strucVar;
				if (!StringUtil.isNullOrEmpty(strucVar)) {
					sectionStart = command;
					sectionEnd = null;
					commandsInSection = 1;
					sectionBroken = false;
				}

			} else if (!StringUtil.isNullOrEmpty(strucVar)) {
				// if the assignment operator = is in the next line, simply skip this statement without breaking the section (if any)
				if (command.getFirstToken().getNext().lineBreaks == 0)
					++commandsInSection;
			}

			command = command.getNext();
		}
		if (!StringUtil.isNullOrEmpty(lastStrucVar) && commandsInSection > 1) {
			try {
				alignSection(code, sectionStart, sectionEnd); // sectionEnd may be null
			} catch (UnexpectedSyntaxBeforeChanges ex) {
				// log and continue
				ex.addToLog();
			}
		}
	}

	private void alignSection(Code code, Command startCommand, Command endCommand) throws UnexpectedSyntaxBeforeChanges {
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		table.getColumn(Columns.ASSIGNMENT_OP.getValue()).rightAlign = true; // if both = and += etc. appear, align the "=" and make the "+" stand out

		int firstLineBreaks = startCommand.getFirstTokenLineBreaks();
		int basicIndent = startCommand.getFirstToken().getStartIndexInLine();

		Command changeCommand = startCommand;
		while (changeCommand != endCommand) {
			commandForErrorMsg = changeCommand;
			if (changeCommand.getFirstToken() == null || changeCommand.getFirstToken().isCommentLine()) {
				changeCommand = changeCommand.getNext();
				continue;
			}

			AlignLine line = table.addLine();

			Token identifier = changeCommand.getFirstToken();
			line.setCell(Columns.IDENTIFIER.getValue(), new AlignCellToken(identifier));

			Token assignOp = identifier.getNext();
			line.setCell(Columns.ASSIGNMENT_OP.getValue(), new AlignCellToken(assignOp));

			Term restOfCommand;
			try {
				restOfCommand = Term.createForTokenRange(assignOp.getNext(), changeCommand.getLastToken());
			} catch (UnexpectedSyntaxException ex) {
				throw new UnexpectedSyntaxBeforeChanges(this, ex);
			}
			line.setCell(Columns.REST_OF_COMMAND.getValue(), new AlignCellTerm(restOfCommand));

			changeCommand = changeCommand.getNext();
		}

		if (table.getLineCount() > 0) {
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, true);
			code.addRuleUses(this, changedCommands);
		}
	}
}
