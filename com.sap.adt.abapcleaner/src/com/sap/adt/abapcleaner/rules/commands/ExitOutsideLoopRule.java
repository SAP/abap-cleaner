package com.sap.adt.abapcleaner.rules.commands;

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

public class ExitOutsideLoopRule extends RuleForCommands {
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "Only use RETURN to exit procedures", "abenexit_procedure_guidl.htm") };

	@Override
	public RuleID getID() { return RuleID.EXIT_OUTSIDE_LOOP; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.COMMANDS; }

	@Override
	public String getDisplayName() { return "Replace EXIT outside loop with RETURN"; }

	@Override
	public String getDescription() { return "Replaces EXIT outside loop with RETURN to exit the processing block."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2025, 9, 27); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

	@Override
   public String getExample() {
      return "" 
   		+ LINE_SEP + "REPORT any_report." 
			+ LINE_SEP 
			+ LINE_SEP + "CLASS any_class IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD any_method." 
			+ LINE_SEP + "    DO 5 TIMES." 
			+ LINE_SEP + "      IF sy-index = 4." 
			+ LINE_SEP + "        \" this continues after ENDDO" 
			+ LINE_SEP + "        EXIT." 
			+ LINE_SEP + "      ENDIF." 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP 
			+ LINE_SEP + "    CASE iv_action." 
			+ LINE_SEP + "      WHEN 'exit'." 
			+ LINE_SEP + "        EXIT." 
			+ LINE_SEP + "      WHEN OTHERS." 
			+ LINE_SEP + "        do_something( )." 
			+ LINE_SEP + "    ENDCASE." 
			+ LINE_SEP + "  ENDMETHOD." 
			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP 
			+ LINE_SEP + "INITIALIZATION." 
			+ LINE_SEP + "  IF a = b." 
			+ LINE_SEP + "    EXIT." 
			+ LINE_SEP + "  ENDIF." 
			+ LINE_SEP 
			+ LINE_SEP + "START-OF-SELECTION." 
			+ LINE_SEP + "  PERFORM any_form." 
			+ LINE_SEP + "  EXIT." 
			+ LINE_SEP + "  PERFORM unreachable_code." 
			+ LINE_SEP 
			+ LINE_SEP + "FORM any_form." 
			+ LINE_SEP + "  LOOP AT gt_any_table ASSIGNING <ls_any> WHERE comp1 > 10." 
			+ LINE_SEP + "    \" this continues after ENDLOOP" 
			+ LINE_SEP + "    EXIT."
			+ LINE_SEP + "  ENDLOOP." 
			+ LINE_SEP + "  IF sy-subrc = 0." 
			+ LINE_SEP + "    EXIT." 
			+ LINE_SEP + "  ENDIF." 
			+ LINE_SEP + "ENDFORM.";
   }

	final ConfigBoolValue configApplyToProcedures = new ConfigBoolValue(this, "ApplyToProcedures", "Apply to procedures (METHOD, FUNCTION, FORM)", true);
	final ConfigBoolValue configApplyToEventBlocks = new ConfigBoolValue(this, "ApplyToEventBlocks", "Apply to event blocks (INITIALIZATION, START-OF-SELECTION etc.)", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configApplyToProcedures, configApplyToEventBlocks };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ExitOutsideLoopRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Token firstToken = command.getFirstCodeToken();
		if (firstToken == null || !firstToken.matchesOnSiblings(true, "EXIT", ".")) {
			return false;
		}
		
		// if the code snippet does not contain an entire processing block, do not apply this rule,
		// as the snippet might be surrounded by a loop
		boolean apply = false; 

		Command parent = command.getParent();
		while (parent != null) {
			if (parent.startsLoop()) 
				return false;
			
			if (parent.isMethodFunctionOrFormStart()) {
				apply = configApplyToProcedures.getValue();
				break;
			} else if (parent.startsEventBlock()) {
				apply = configApplyToEventBlocks.getValue();
				break;
			}
			parent = parent.getParent();
		}
		
		if (apply) {
			firstToken.setText("RETURN", false);
		}
		return apply;
	}
}
