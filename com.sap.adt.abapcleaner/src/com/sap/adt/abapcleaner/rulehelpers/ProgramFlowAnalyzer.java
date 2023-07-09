package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;

public abstract class ProgramFlowAnalyzer {
	/** returns true if the Command's execution block does not need to be evaluated further */
	protected abstract boolean skipAnalysisAfter(Command command);
	
	/** determines the overall FlowResult of multiple branches with divergent results */
	protected abstract FlowResult getMergedResult(boolean hasSkipBranch, boolean hasStopBranch, boolean hasContinueBranch);

	public enum FlowResult {
		/** the execution block contains a Command that satisfies {@link ProgramFlowAnalyzer#skipAnalysisAfter(Command)};
		 * therefore analysis can be skipped for the rest of the execution block */
		SKIP,
		/** the execution block ends with RETURN, RAISE, CONTINUE, EXIT, STOP, or LEAVE; no 'SKIP' Command was found */
		STOP,
		/** program flow analysis must continue behind the execution block; no 'SKIP' Command was found */
		CONTINUE;
	}

	private class LoopFlow {
		private boolean continueOrCheckFound = false;
		private boolean exitFound = false;
		
		private boolean getContinueCheckOrExitFound() {
			return continueOrCheckFound || exitFound;
		}
	}
	
	protected Command initialStartCommand;
	private HashMap<Command, FlowResult> resultOfBlock = new HashMap<>();
	
	public FlowResult analyze(Command startCommand) {
		this.initialStartCommand = startCommand;

		if (startCommand == null || startCommand.isInClassDefinition() || startCommand.isInInterfaceDefinition()
				|| startCommand.isMethodFunctionFormOrEventBlockEnd() || startCommand.isClassOrInterfaceEnd()) {
			// no program flow to analyze
			return FlowResult.CONTINUE;
		}
		
		Command command = startCommand;
		LoopFlow loopFlow = command.isInLoop() ? new LoopFlow() : null;

		ArrayList<FlowResult> branchResults = new ArrayList<>();
		boolean isStartScope = true;
		do {
			// analyze the current block up to its end, starting from the current Command (i.e. not necessarily from block start!) 
			FlowResult branchResult = analyzeBlock(command, null, isStartScope, loopFlow);
			if (!branchResults.isEmpty()) {
				// merge the result of this block with the result of a DO / WHILE / LOOP etc. repetition (see below)
				branchResults.add(branchResult);
				branchResult = getMergedResult(branchResults);
			} 
			
			if (branchResult != FlowResult.CONTINUE) {
				if (loopFlow != null && loopFlow.getContinueCheckOrExitFound()) {
					// continue below, because CONTINUE, CHECK or EXIT was encountered
				} else {
					break;
				}
			}
			
			// determine the Command that closes the current block 
			if (command.getParent() == null)
				break;
			Command closingCommand = command.getParent().getNextSibling();
			if (closingCommand == null || closingCommand.isMethodFunctionFormOrEventBlockEnd() || closingCommand.isClassOrInterfaceEnd()) 
				break;
			
			branchResults.clear();
			isStartScope = false;
			if (closingCommand.endsLoop()) {
				// analyze the section from block start up to the current Command (next loop cycle)
				Command startOfBlock = closingCommand.getPrevSibling();
				if (branchResult != FlowResult.CONTINUE && loopFlow != null && !loopFlow.continueOrCheckFound) {
					// if the above branch would does not continue, but analysis continues only because of an EXIT,
					// (whereas no CONTINUE or CHECK found), then do NOT consider the loop repetition
				} else if (skipAnalysisAfter(startOfBlock)) {
					// if flow analysis shall be skipped behind the start command itself, then do not consider the loop repetition
				} else {
					FlowResult repeatResult = analyzeBlock(startOfBlock.getFirstChild(), command, isStartScope, loopFlow);
					branchResults.add(repeatResult);
				}
				// discard the previous LoopFlow instance, because we are now outside of that loop
				loopFlow = closingCommand.isInLoop() ? new LoopFlow() : null;
			} else {
				// move to the last closing command (e.g. ELSEIF -> ELSE -> ENDIF)
				while (closingCommand != null && closingCommand.getOpensLevel()) {
					closingCommand = closingCommand.getNextSibling();
				}
			}
			
			// continue to analyze on parent level, starting from the Command that closed the previous block 
			command = closingCommand;
		} while (command != null);
		
		return FlowResult.CONTINUE;
	}
	
	private FlowResult analyzeBlock(Command startCommand, Command lastCommand, boolean fromStartScope, LoopFlow loopFlow) {
		Command command = startCommand;
		if (lastCommand == null && resultOfBlock.containsKey(startCommand))
			return resultOfBlock.get(startCommand);

		while (command != null) {
			// analyze branches (e.g. LOOP ... ENDLOOP / IF ... ELSEIF ... ELSE ... ENDIF / CASE ... WHEN ... ENDCASE)
			if (command.getOpensLevel()) {
				boolean isLoop = command.startsLoop();
				LoopFlow innerLoopFlow = isLoop ? new LoopFlow() : loopFlow;
				ArrayList<FlowResult> branchResults = new ArrayList<>();
				command = analyzeBranches(command, innerLoopFlow, fromStartScope, branchResults);
				
				// merge the results from the branches
				FlowResult mergedResult = getMergedResult(branchResults);
				if (mergedResult != FlowResult.CONTINUE) {
					if (isLoop && innerLoopFlow.getContinueCheckOrExitFound()) {
						// continue below (i.e. behind the loop), because CONTINUE, CHECK or EXIT was found
					} else {
						return mergedResult;
					}
				}
				// continue to analyze the closing command (e.g. ENDLOOP) as part of the outer branch
				continue;
			}				

			// analyze the current command, but skip the initialStartCommand when analyzing the start scope; 
			// however, do analyze the initialStartCommand from a wider scope, because it might e.g. evaluate its
			// own result from the previous loop cycle (e.g. with respect to a changed system field):
			if (command == initialStartCommand && fromStartScope) {
				// skip analysis 
			} else if (skipAnalysisAfter(command)) { 
				return FlowResult.SKIP;
			}

			// determine whether the Command stops the flow of this block; note that this must be done only AFTER  
			// analyzeCommand(), which may need to evaluate the logic of 'RETURN sy-subrc.', 'CHECK <condition>.' etc.
			if (stopsBlockExecution(command, loopFlow)) {
				return FlowResult.STOP;
			}
			
			if (command == lastCommand) {
				return FlowResult.CONTINUE;
			}
			command = command.getNextSibling();
		}
		
		return FlowResult.CONTINUE;
	}
	
	private Command analyzeBranches(Command startCommand, LoopFlow innerLoopFlow, boolean fromStartScope, ArrayList<FlowResult> branchResults) {
		Command command = startCommand;
		boolean skipRemaining = false;
		do {
			boolean doAnalyze = (command != initialStartCommand || !fromStartScope);
			if (skipRemaining) {
				// do nothing
			} else if (doAnalyze && skipAnalysisAfter(command)) {
				// if the evaluation of this branch's parent Command is a match, then skip all following branches,
				// because they cannot be entered without that evaluation
				skipRemaining = true;
				branchResults.add(FlowResult.SKIP);
			} else if (command.hasChildren()) {
				// recursively get the result from this branch
				// determine the LoopFlow instance:
				// - if this Command starts a loop, then instantiate a new LoopFlow 
				// - otherwise continue with the supplied one (which may or may not come from the analyze() method)
				FlowResult branchResult = analyzeBlock(command.getFirstChild(), null, fromStartScope, innerLoopFlow);
				resultOfBlock.put(command.getFirstChild(), branchResult);
				branchResults.add(branchResult);
			} else if (command.firstCodeTokenIsKeyword("CASE")) {
				// CASE is considered a sibling of WHEN, but this is not an empty branch
			} else {
				// for an empty branch, add CONTINUE 
				branchResults.add(FlowResult.CONTINUE);
			}
			// lastCommand should rather NOT be considered here to avoid evaluation of partial branches
			command = command.getNextSibling();
		} while (command.getOpensLevel());
		
		if (!skipRemaining) {
			// if an "ELSE" branch or a "WHEN OTHERS" branch is missing, add CONTINUE for the 'invisible' branch;
			// the same applies to all kinds of LOOPs that could be skipped if there is nothing to loop over 
			// (therefore DO. and DO # TIMES. are excluded here, because they are sure to be executed at least once) 
			Command lastBranch = command.getPrevSibling();
			Token lastBranchToken = lastBranch.getFirstCodeToken();
			if (lastBranchToken == null) {
				// should never happen
			} else if (lastBranchToken.isAnyKeyword("IF", "ELSEIF", "ON") // missing ELSE 
					|| lastBranchToken.isKeyword("WHEN") && lastBranchToken.getNextCodeSibling() != null && !lastBranchToken.getNextCodeSibling().isKeyword("OTHERS")
					|| lastBranch.startsLoop() && !lastBranchToken.matchesOnSiblings(true, "DO", ".") && !lastBranchToken.matchesOnSiblings(true, "DO", TokenSearch.ANY_LITERAL, "TIMES", ".")) {
				branchResults.add(FlowResult.CONTINUE);
			} 
		}
		
		return command;
	}
	
	private FlowResult getMergedResult(ArrayList<FlowResult> branchResults) {
		boolean hasSkipBranch = false;
		boolean hasStopBranch = false;
		boolean hasContinueBranch = false;

		for (FlowResult branchResult : branchResults) {
			hasSkipBranch |= (branchResult == FlowResult.SKIP);
			hasStopBranch |= (branchResult == FlowResult.STOP);
			hasContinueBranch |= (branchResult == FlowResult.CONTINUE);
		}
		return getMergedResult(hasSkipBranch, hasStopBranch, hasContinueBranch); 
	}
	
	private boolean stopsBlockExecution(Command command, LoopFlow loopFlow) {
		// unconditional stops, including LEAVE PROGRAM, LEAVE [TO] TRANSACTION/SCREEN/LIST-PROCESSING etc.
		if (command.firstCodeTokenIsAnyKeyword("RETURN", "RAISE", "STOP", "LEAVE")) { 
			return true;

		} else if (loopFlow == null) {
			// outside of a loop, only EXIT must be considered, because CHECK may be passed
			return command.firstCodeTokenIsKeyword("EXIT");

		} else {
			// even if (the end of) a loop body terminates with .SKIP or .STOP, the caller must later consider...
			// - the beginning of the loop, if CONTINUE or CHECK was found, and 
			// - the block following the loop, if CONTINUE, CHECK or EXIT was found  
			if (command.firstCodeTokenIsKeyword("CONTINUE")) {
				loopFlow.continueOrCheckFound = true;
				return true;
			} else if (command.firstCodeTokenIsKeyword("CHECK")) {
				loopFlow.continueOrCheckFound = true;
				return false; // the CHECK may be passed!
			} else if (command.firstCodeTokenIsKeyword("EXIT")) {
				loopFlow.exitFound = true;
				return true;
			} else {
				return false;
			}
		}
	}
}
