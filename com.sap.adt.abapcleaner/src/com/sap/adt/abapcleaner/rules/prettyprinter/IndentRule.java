package com.sap.adt.abapcleaner.rules.prettyprinter;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;

public class IndentRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use the Pretty Printer before activating", "#use-the-pretty-printer-before-activating"),
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Use your Pretty Printer team settings", "#use-your-pretty-printer-team-settings"),
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Comment Position", "comment-position.md") };

	@Override
	public RuleID getID() { return RuleID.INSET; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.PRETTY_PRINTER; }

	@Override
	public String getDisplayName() { return "Indent lines"; }

	@Override
	public String getDescription() { return "Moves all commands and comments to the correct indentation."; }

	@Override
	public String getHintsAndRestrictions() { return "Relative indents within the lines of a command (or command chain) are not changed by this rule."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 2); }

	@Override
	public RuleReference[] getReferences() { return references; }
   
	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_PARAMETERS } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "METHOD pretty_print_indent." 
			+ LINE_SEP + "DO 4 TIMES." 
			+ LINE_SEP + "LOOP AT its_table ASSIGNING FIELD-SYMBOL(<ls_row>)." 
			+ LINE_SEP + "IF <ls_row>-ignore = abap_true." 
			+ LINE_SEP + "\" comment" 
			+ LINE_SEP + "CONTINUE." 
			+ LINE_SEP + "ENDIF." 
			+ LINE_SEP + "TRY." 
			+ LINE_SEP + "\" comment" 
			+ LINE_SEP + "IF iv_value = 1." 
			+ LINE_SEP + "\" comment on next line" 
			+ LINE_SEP + "iv_value += 3." 
			+ LINE_SEP + "\" comment with no empty line above it" 
			+ LINE_SEP + "ELSEIF iv_value = 2." 
			+ LINE_SEP + "iv_value += 2." 
			+ LINE_SEP 
			+ LINE_SEP + "\" comment on ELSE branch with an empty line above it" 
			+ LINE_SEP + "ELSE." 
			+ LINE_SEP + "iv_value += 1." 
			+ LINE_SEP + "ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "CASE iv_value." 
			+ LINE_SEP + "\" comment on WHEN" 
			+ LINE_SEP + "WHEN 1." 
			+ LINE_SEP + "\" comment on next line" 
			+ LINE_SEP + "iv_value += 1." 
			+ LINE_SEP 
			+ LINE_SEP + "\" comment on WHEN with an empty line above it" 
			+ LINE_SEP + "WHEN 2." 
			+ LINE_SEP + "iv_value += 2." 
			+ LINE_SEP + "\" comment with no empty line above it" 
			+ LINE_SEP + "WHEN 3." 
			+ LINE_SEP + "iv_value += 3." 
			+ LINE_SEP + "ENDCASE." 
			+ LINE_SEP 
			+ LINE_SEP + "\" comment on CATCH with an empty line above it" 
			+ LINE_SEP + "CATCH cx_any." 
			+ LINE_SEP + "\" no handler" 
			+ LINE_SEP + "ENDTRY." 
			+ LINE_SEP + "ENDLOOP." 
			+ LINE_SEP + "ENDDO." 
			+ LINE_SEP + "ENDMETHOD.";
   }

	private static final String[] alignSelection = new String[] { "always", "if comment is preceded by blank line", "never" };

	final ConfigBoolValue configExecuteOnClassDefinitionSections = new ConfigBoolValue(this, "ExecuteOnClassDefinitionSections", "Execute on CLASS ... DEFINITION sections", true);
	final ConfigEnumValue<AlignWithNextCommandMode> configAlignWithFollowingElse = new ConfigEnumValue<AlignWithNextCommandMode>(this, "AlignWithFollowingElse", "Align with following ELSEIF or ELSE", alignSelection, AlignWithNextCommandMode.values(), AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
	final ConfigEnumValue<AlignWithNextCommandMode> configAlignWithFollowingWhen = new ConfigEnumValue<AlignWithNextCommandMode>(this, "AlignWithFollowingWhen", "Align with following WHEN", alignSelection, AlignWithNextCommandMode.values(), AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
	final ConfigEnumValue<AlignWithNextCommandMode> configAlignWithFollowingCatch = new ConfigEnumValue<AlignWithNextCommandMode>(this, "AlignWithFollowingCatch", "Align with following CATCH or CLEANUP", alignSelection, AlignWithNextCommandMode.values(), AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE);
	
	private final ConfigValue[] configValues = new ConfigValue[] { configExecuteOnClassDefinitionSections, configAlignWithFollowingElse, configAlignWithFollowingWhen, configAlignWithFollowingCatch };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private AlignWithNextCommandMode getAlignWithFollowingElse() { return AlignWithNextCommandMode.forValue(configAlignWithFollowingElse.getValue()); }
	private AlignWithNextCommandMode getAlignWithFollowingWhen() { return AlignWithNextCommandMode.forValue(configAlignWithFollowingWhen.getValue()); }
	private AlignWithNextCommandMode getAlignWithFollowingCatch() { return AlignWithNextCommandMode.forValue(configAlignWithFollowingCatch.getValue()); }

	public IndentRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		boolean executeOnClassDefinitionSections = configExecuteOnClassDefinitionSections.getValue();
		AlignWithNextCommandMode alignWithFollowingElse = getAlignWithFollowingElse();
		AlignWithNextCommandMode alignWithFollowingWhen = getAlignWithFollowingWhen();
		AlignWithNextCommandMode alignWithFollowingCatch = getAlignWithFollowingCatch();
		boolean anyAlignmentWithFollowing = (alignWithFollowingElse != AlignWithNextCommandMode.NEVER) || (alignWithFollowingWhen != AlignWithNextCommandMode.NEVER)
				|| (alignWithFollowingCatch != AlignWithNextCommandMode.NEVER);

		Command command = code.firstCommand;
		Command alignCommand = null; // is set to a Command which preceding comment lines shall be aligned with

		while (command != null) {
			// once the "alignCommand" is reached, do not use it any further
			if (command == alignCommand)
				alignCommand = null;

			commandForErrorMsg = command;
			if (isCommandBlocked(command) || (!executeOnClassDefinitionSections && command.isInClassDefinition()) || command.isAsteriskCommentLine()) {
				command = command.getNext();
				continue;
			}

			// determine whether this command shall rather be aligned with a following "ELSEIF", "ELSE", "WHEN", "CATCH", or "CLEANUP" command
			if (anyAlignmentWithFollowing && alignCommand == null && command.isQuotMarkCommentLine()) {
				alignCommand = command;
				do
					alignCommand = alignCommand.getNext();
				while (alignCommand != null && alignCommand.getFirstToken().isQuotMarkComment() && alignCommand.getFirstTokenLineBreaks() == 1);
				if (alignCommand != null) {
					AlignWithNextCommandMode alignMode;
					if (alignCommand.firstCodeTokenIsAnyKeyword("ELSEIF", "ELSE"))
						alignMode = alignWithFollowingElse;
					else if (alignCommand.firstCodeTokenIsKeyword("WHEN"))
						alignMode = alignWithFollowingWhen;
					else if (alignCommand.firstCodeTokenIsAnyKeyword("CATCH", "CLEANUP"))
						alignMode = alignWithFollowingCatch;
					else
						alignMode = AlignWithNextCommandMode.NEVER;

					if (alignMode == AlignWithNextCommandMode.NEVER || (alignMode == AlignWithNextCommandMode.IF_BLANK_LINE_ABOVE && command.getFirstTokenLineBreaks() < 2))
						alignCommand = null;
				}
			}

			executeOn(code, command, (alignCommand != null ? alignCommand : command).getIndent());

			command = command.getNext();
		}
	}

	public final void executeOn(Code code, Command command, int newIndent) {
		Token firstToken = command.getFirstToken();
		// skip commands that follow on the same line as the previous command
		if (firstToken.lineBreaks == 0 && !command.isFirstCommandInCode()) {
			return;
		}
		if (firstToken.spacesLeft != newIndent) {
			command.addIndent(newIndent - firstToken.spacesLeft, 0);
			code.addRuleUse(this, command);
		}
	}
}