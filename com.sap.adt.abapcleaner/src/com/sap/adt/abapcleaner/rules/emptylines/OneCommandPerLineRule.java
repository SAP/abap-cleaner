package com.sap.adt.abapcleaner.rules.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class OneCommandPerLineRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "No more than one statement per line", "#no-more-than-one-statement-per-line") };

	@Override
	public RuleID getID() { return RuleID.ONE_COMMAND_PER_LINE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Move commands to own lines"; }

	@Override
	public String getDescription() { return "If a line contains multiple commands, each command is moved to its own line."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2025, 9, 26); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean isEssential() { return true; }
	
	@Override
   public String getExample() {
      return "" 
   			+ LINE_SEP + "  METHOD any_method."
   			+ LINE_SEP + "    DATA lv_x TYPE i VALUE 1. DATA lv_y TYPE i VALUE 2. DATA lv_z TYPE i VALUE 3."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    lv_x += 2.lv_y -= 4.lv_z *= 6."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    IF lv_x > lv_y. IF lv_y < lv_z."
   			+ LINE_SEP + "      lv_x = lv_z - lv_y.ELSE.lv_y = lv_z - lv_x.ENDIF."
   			+ LINE_SEP + "    ENDIF."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    DO 3 TIMES.lv_y *= 3.DO 5 TIMES.IF lv_x < lv_y + lv_z.lv_x += 2.lv_y -= 1.ELSE.lv_z *= 1.ENDIF.ENDDO.ENDDO."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    CASE lv_x."
   			+ LINE_SEP + "      WHEN 1. lv_x += 1. lv_y += 1."
   			+ LINE_SEP + "      WHEN 2. lv_y -= 1. lv_z -= 2."
   			+ LINE_SEP + "      WHEN 3. lv_z *= 2. lv_x *= 3."
   			+ LINE_SEP + "    ENDCASE."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    CASE lv_x."
   			+ LINE_SEP + "      WHEN 1. lv_x += 1. do_something( iv_value = lv_x"
   			+ LINE_SEP + "                                       iv_name  = 'X' )."
   			+ LINE_SEP + "      WHEN 2. lv_y -= 1. do_something( iv_value = lv_y"
   			+ LINE_SEP + "                                       iv_name  = 'Y' )."
   			+ LINE_SEP + "      WHEN 3. lv_z *= 2. do_something( iv_value = lv_z )."
   			+ LINE_SEP + "        do_something_else( iv_value = lv_z )."
   			+ LINE_SEP + "    ENDCASE. RETURN lv_x + lv_y + lv_z."
   			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configKeepOneLinersAfterWhen = new ConfigBoolValue(this, "KeepOneLinersAfterWhen", "Keep one-liners after WHEN", true);
	final ConfigBoolValue configKeepMultiLinersAfterWhen = new ConfigBoolValue(this, "KeepMultiLinersAfterWhen", "Keep multi-liners after WHEN", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configKeepOneLinersAfterWhen, configKeepMultiLinersAfterWhen };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public OneCommandPerLineRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstToken();
		if (command.isFirstCommandInCode() || firstToken.lineBreaks > 0 || command.isCommentLine()) {
			return false;
		
		} else if (firstToken.isPeriod()) {
			// e.g. 'lv_x += 1..' - leave this case to the EmptyCommandRule 
			return false;
		
		} else if (command.isPragmaLine()) {
			// e.g. 'DATA lv_x TYPE i. ##NEEDED' - leave this case to the PragmaPositionRule
			return false;

		} else if (command.getPrev().firstCodeTokenIsKeyword("INCLUDE") && firstToken.matchesOnSiblings(false, "TYPES", ":")) {
			// skip cases with 'INCLUDE ... . TYPES:', e.g.
			// TYPES:
			//    BEGIN OF ty_s_any,
			//      any_comp   TYPE i,
			//      other_comp TYPE string.
			//      INCLUDE TYPE ty_s_other. TYPES:
			//    END OF ty_s_any.
			Token nextNext = firstToken.getNext().getNext();
			if (nextNext.lineBreaks > 0) {
				return false;
			}
		}

		Command firstCommandInLine = getFirstCommandInLine(command);
		
		// skip Commands after WHEN, depending on configuration
		if (firstCommandInLine.firstCodeTokenIsKeyword("WHEN")) {
			boolean isOneLiner = isOneLinerAfterWhen(firstCommandInLine);
			if (isOneLiner && configKeepOneLinersAfterWhen.getValue()) {
				return false;
			} else if (!isOneLiner && configKeepMultiLinersAfterWhen.getValue()) {
				return false;
			}
		}

		// adjust the indent of both the Command at line start and the current Command,
		// using the previous Command as an anchor point, respectively
		boolean changed = false;
		Command prevCommand = getFirstCommandInLine(firstCommandInLine.getPrevNonCommentCommand());
		changed |= setRelativeIndent(prevCommand, firstCommandInLine);
		changed |= setRelativeIndent(firstCommandInLine, command);
		return changed;
	}
	
	private boolean setRelativeIndent(Command prevCommand, Command command) {
		Token firstToken = command.getFirstToken();
		
		int newIndent = command.getIndent();
		if (prevCommand != null) {
			// adjust newIndent to the previous Command, the indent of which may deviate from what it should be
			int deviation = prevCommand.getFirstToken().spacesLeft - prevCommand.getIndent();
			newIndent = Math.max(newIndent + deviation, 0);
		}

		boolean changed = false;
		if (firstToken.lineBreaks == 0) {
			// before adjusting spaces, move the first Token down by one line (esp. if this Command spans multiple lines)
			int oldStartIndex = firstToken.getStartIndexInLine();
			changed |= firstToken.setWhitespace(1, oldStartIndex);
		}
		int add = newIndent - firstToken.getStartIndexInLine();
		changed |= firstToken.setSpacesLeftAdjustingIndent(newIndent, true);
		if (changed) 
			command.getParentCode().addRuleUse(this, command);
		
		// also adjust the other Commands that follow on the same line
		Command nextCommand = command.getNext();
		while (nextCommand != null && nextCommand.getFirstTokenLineBreaks() == 0) {
			Token startToken = nextCommand.getFirstToken().getNext();
			if (startToken != null) {
				nextCommand.addIndent(add, 0, startToken);
			}
			nextCommand = nextCommand.getNext();
		}
		
		return changed;
	}

	private Command getFirstCommandInLine(Command command) {
		while (command != null) {
			if (command.isFirstCommandInCode() || command.getFirstTokenLineBreaks() > 0) {
				break;
			}
			command = command.getPrev();
		}
		return command;
	}
	
	private boolean isOneLinerAfterWhen(Command whenCommand) {
		// determine whether any of the Commands after WHEN contains multiple lines
		Command testCommand = whenCommand.getNext();
		while (testCommand != null) {
			if (testCommand.getFirstTokenLineBreaks() > 0) {
				// if the WHEN block contains another non-comment Command on its own line, 
				// then do not consider this WHEN block as a one-liner, either
				if (testCommand.getParent() == whenCommand) {
					return false;
				}
				break;
			}
			if (testCommand.getLineBreakSum() > 0) {
				return false;
			}
			testCommand = testCommand.getNextNonCommentCommand();
		}
		return true;
	}
}