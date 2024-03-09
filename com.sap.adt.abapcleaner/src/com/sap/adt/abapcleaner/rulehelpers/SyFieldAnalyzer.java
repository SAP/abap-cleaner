package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.ABAP.SyField;
import com.sap.adt.abapcleaner.parser.Command;

public class SyFieldAnalyzer extends ProgramFlowAnalyzer {
	private ArrayList<Command> commandsReadingSy = new ArrayList<>();
	private HashSet<Command> commandsHashed = new HashSet<>();

	private ABAP.SyField syField;

	/**
	 * for the supplied system field and start command, determines all subsequent commands in the program flow
	 * in which the system field is evaluated (before it is again changed by a different command)
	 *  
	 * @param syField - the system field 
	 * @param command - the Command that changes the system field
	 * @return - non-null list of commands in which the supplied system field is evaluated 
	 */
	public static ArrayList<Command> getSyFieldReadersFor(ABAP.SyField syField, Command command) {
		SyFieldAnalyzer analyzer = new SyFieldAnalyzer(syField);
		analyzer.analyze(command);
		return analyzer.commandsReadingSy;
	}
	
	private SyFieldAnalyzer(ABAP.SyField syField) {
		this.syField = syField;
	}
	
	@Override
	protected boolean skipAnalysisAfter(Command command) {
		// check whether the command evaluates the system field in question
		if (command.readsSyField(syField) && !commandsHashed.contains(command)) {
			commandsHashed.add(command);
			commandsReadingSy.add(command);
		}
		// skip analysis after the command if it changes the system field
		if (command.changesSyField(syField)) {
			return true;
		} else if (syField == SyField.SUBRC && command.firstCodeTokenIsKeyword("PERFORM") && command.getNextNonCommentCommand() != null) {
			// if SY-SUBRC is evaluated directly after PERFORM, then consider this PERFORM to be changing SY-SUBRC
			// (although PERFORM ... FORM ... ENDFORM do themselves NOT change SY-SUBRC)
			return command.getNextNonCommentCommand().readsSyField(syField);
		} else {
			return false;
		}
	}

	@Override
	protected FlowResult getMergedResult(boolean hasSkipBranch, boolean hasStopBranch, boolean hasContinueBranch) {
		// SKIP and STOP are equivalent in this case, therefore only hasContinueBranch is relevant for the merged result
		return hasContinueBranch ? FlowResult.CONTINUE : FlowResult.SKIP;
	}
}
