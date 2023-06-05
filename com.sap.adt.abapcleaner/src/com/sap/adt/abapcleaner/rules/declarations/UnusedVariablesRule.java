package com.sap.adt.abapcleaner.rules.declarations;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.LocalVariables;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;

import java.time.LocalDate;

public class UnusedVariablesRule extends RuleForDeclarations {
   private final static RuleReference[] references = new RuleReference[] {new RuleReference(RuleSource.ABAP_CLEANER)};

   private static UnusedVariableMeasure convertToMeasure(UnusedVariableMeasureIfAssigned measureIfAssigned) {
      switch (measureIfAssigned) {
         case ADD_TODO_COMMENT:
            return UnusedVariableMeasure.ADD_TODO_COMMENT;
         case IGNORE:
            return UnusedVariableMeasure.IGNORE;
         default:
            throw new IndexOutOfBoundsException("unexpected MeasureIfAssigned value");
      }
   }

   // ----------------------------------------------------------------------

	@Override
	public RuleID getID() { return RuleID.UNUSED_VARIABLES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Delete unused variables"; }

	@Override
	public String getDescription() {
		return "Deletes unused variables, or comments them out if they are 'used' in commented-out code. TODO comments can be added for variables that are assigned but never used.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 15); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.CHAIN_OF_ONE, RuleID.UPPER_AND_LOWER_CASE, RuleID.INSET }; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD delete_unused_variables." 
			+ LINE_SEP + "    CONSTANTS lc_unused        TYPE i VALUE 1200." 
			+ LINE_SEP + "    CONSTANTS lc_commented_out TYPE i VALUE 2." 
			+ LINE_SEP + "    CONSTANTS lc_used_constant TYPE i VALUE 3." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_unused_a      TYPE i." 
			+ LINE_SEP + "    DATA: lv_unused_b      TYPE string," 
			+ LINE_SEP + "          lv_used_var      TYPE i," 
			+ LINE_SEP + "          lv_commented_out LIKE lv_used_var." 
			+ LINE_SEP 
			+ LINE_SEP + "    DATA lv_only_assigned TYPE i." 
			+ LINE_SEP + "    DATA lv_assigned_but_used_incomment TYPE i." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" with the ##NEEDED pragma, an unused variable will be kept and no TODO added;" 
			+ LINE_SEP + "    \" the pragma also prevents a warning from the Extended Check (SLIN)" 
			+ LINE_SEP + "    DATA lv_unused_but_needed TYPE string ##NEEDED." 
			+ LINE_SEP 
			+ LINE_SEP + "    FIELD-SYMBOLS:" 
			+ LINE_SEP + "      <ls_unused_a>      TYPE ty_s_structure," 
			+ LINE_SEP + "      <ls_commented_out> LIKE LINE OF its_table," 
			+ LINE_SEP + "      <ls_used>          TYPE tt_table." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ev_count." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the following variable assigned but unused; however, the call may have side effects" 
			+ LINE_SEP + "    DATA(lo_unused_util) = get_utility( ). " 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_used_var = 0." 
			+ LINE_SEP + "    DO lc_used_constant TIMES." 
			+ LINE_SEP + "      LOOP AT its_table ASSIGNING <ls_used>." 
			+ LINE_SEP + "        lv_used_var += <ls_used>-num." 
			+ LINE_SEP + "      ENDLOOP." 
			+ LINE_SEP + "    ENDDO." 
			+ LINE_SEP + "    ev_count = lv_used_var." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" lv_only_assigned is only used to modifying its own value" 
			+ LINE_SEP + "    CLEAR lv_only_assigned." 
			+ LINE_SEP + "    lv_only_assigned = lv_only_assigned + 2." 
			+ LINE_SEP + "    MULTIPLY lv_only_assigned BY 2." 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_assigned_but_used_incomment = 1." 
			+ LINE_SEP 
			+ LINE_SEP + "*    this comment mentions the variable lv_unused_a, but that does not count:" 
			+ LINE_SEP + "*    only variables in commented-out ABAP code count, not variables in text comments!" 
			+ LINE_SEP + "*" 
			+ LINE_SEP + "*    lv_commented_out = 0." 
			+ LINE_SEP + "*    DO lc_commented_out * lv_assigned_but_used_incomment TIMES." 
			+ LINE_SEP + "*      LOOP AT its_table ASSIGNING <ls_commented_out>." 
			+ LINE_SEP + "*        lv_commented_out += 1." 
			+ LINE_SEP + "*      ENDLOOP." 
			+ LINE_SEP + "*    ENDDO." 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	private static final String[] measureTexts = new String[] { "delete", "comment out with *", "comment out with \"", "add TODO comment", "ignore" };
	private static final String[] measureTextsIfAssigned = new String[] { "add TODO comment", "ignore" };

	final ConfigEnumValue<UnusedVariableMeasure> configMeasureForVarsNeverUsed = new ConfigEnumValue<UnusedVariableMeasure>(this, "MeasureForVarsNeverUsed", "Measure for local variables that are never used:", measureTexts, UnusedVariableMeasure.DELETE);
	final ConfigEnumValue<UnusedVariableMeasure> configMeasureForVarsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableMeasure>(this, "MeasureForVarsOnlyUsedInComment", "Measure for variables only used in commented-out code:", measureTexts, UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
	final ConfigEnumValue<UnusedVariableMeasureIfAssigned> configMeasureForAssignedVars = new ConfigEnumValue<UnusedVariableMeasureIfAssigned>(this, "MeasureForAssignedVars", "Measure for assigned but unused local variables:", measureTextsIfAssigned, UnusedVariableMeasureIfAssigned.ADD_TODO_COMMENT);
	final ConfigEnumValue<UnusedVariableMeasureIfAssigned> configMeasureForAssignedVarsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableMeasureIfAssigned>(this, "MeasureForAssignedVarsOnlyUsedInComment", "Measure for assigned variables only used in commented-out code:", measureTextsIfAssigned, UnusedVariableMeasureIfAssigned.ADD_TODO_COMMENT);
	final ConfigEnumValue<UnusedVariableMeasure> configMeasureForConstantsNeverUsed = new ConfigEnumValue<UnusedVariableMeasure>(this, "MeasureForConstantsNeverUsed", "Measure for local constants that are never used:", measureTexts, UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);
	final ConfigEnumValue<UnusedVariableMeasure> configMeasureForConstantsOnlyUsedInComment = new ConfigEnumValue<UnusedVariableMeasure>(this, "MeasureForConstantsOnlyUsedInComment", "Measure for constants only used in commented-out code:", measureTexts, UnusedVariableMeasure.COMMENT_OUT_WITH_ASTERISK);

	private final ConfigValue[] configValues = new ConfigValue[] { configMeasureForVarsNeverUsed, configMeasureForVarsOnlyUsedInComment, configMeasureForAssignedVars, configMeasureForAssignedVarsOnlyUsedInComment,
			configMeasureForConstantsNeverUsed, configMeasureForConstantsOnlyUsedInComment };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	// TODO: create configuration options for prefix and suffix?
	private static final String prefix = "TODO: ";
	private static final String suffix = " (" + Program.PRODUCT_NAME + ")";
	private static final String varAssignedButOnlyUsedInCommentMsg = prefix + "variable is assigned but only used in commented-out code" + suffix;
	private static final String varAssignedButNeverUsedAddPragmaMsg = prefix + "variable is assigned but never used; add pragma ##NEEDED" + suffix;
	private static final String varAssignedButNeverUsedMsg = prefix + "variable is assigned but never used" + suffix;
	private static final String varOnlyUsedInCommentMsg = prefix + "variable is only used in commented-out code" + suffix;
	private static final String constOnlyUsedInCommentMsg = prefix + "constant is only used in commented-out code" + suffix;
	private static final String varNeverUsedMsg = prefix + "variable is never used" + suffix;
	private static final String constNeverUsedMsg = prefix + "constant is never used" + suffix;

	public static final String[] varMessages = new String[] { ABAP.COMMENT_SIGN_STRING + " " + varAssignedButOnlyUsedInCommentMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varAssignedButNeverUsedAddPragmaMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varAssignedButNeverUsedMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varOnlyUsedInCommentMsg, 
																				  ABAP.COMMENT_SIGN_STRING + " " + varNeverUsedMsg };
	
	public static final String[] constMessages = new String[] { ABAP.COMMENT_SIGN_STRING + " " + constOnlyUsedInCommentMsg, 
																					 ABAP.COMMENT_SIGN_STRING + " " + constNeverUsedMsg };
	
	public UnusedVariablesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// nothing to do on class definition level
		return;
	}

	@Override
	protected void executeOn(Code code, Command methodStart, LocalVariables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		// skip this method if macros are used inside the method to avoid deletion of variables that are used inside 
		// the macros (note that macro code may be local or 'out of sight')
		if (localVariables.getMethodUsesMacros())
			return;
		
		for (VariableInfo varInfo : localVariables.getLocalsInDeclarationOrder()) {
			Command command = varInfo.declarationToken.getParentCommand();
			// .isBlocked must be considered only at this point, NOT earlier when usage information is gathered 
			if (isCommandBlocked(command)) 
				continue;
			commandForErrorMsg = command;

			if (varInfo.isUsed()) {
				// if an earlier cleanup produced a comment above the declaration of this variable, remove it (now that the variable or constant is used)
				try {
					if (command.removeMatchingCommentAboveLineOf(varInfo.declarationToken, (varInfo.isConstant ? constMessages : varMessages)))
						code.addRuleUse(this, command);
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				}
				continue;
			}
			
			// determine the measure to be taken; this is NOT dependent on "usedCountInSelfAssignment" (neither in active code nor comment code)
			UnusedVariableMeasure measure;
			if (varInfo.isAssigned())
				measure = convertToMeasure(UnusedVariableMeasureIfAssigned.forValue((varInfo.isUsedInComment()) ? configMeasureForAssignedVarsOnlyUsedInComment.getValue() : configMeasureForAssignedVars.getValue()));
			else if (varInfo.isUsedInComment() || varInfo.isAssignedInComment())
				measure = UnusedVariableMeasure.forValue(varInfo.isConstant ? configMeasureForConstantsOnlyUsedInComment.getValue() : configMeasureForVarsOnlyUsedInComment.getValue());
			else
				measure = UnusedVariableMeasure.forValue(varInfo.isConstant ? configMeasureForConstantsNeverUsed.getValue() : configMeasureForVarsNeverUsed.getValue());

			if (measure == UnusedVariableMeasure.IGNORE)
				continue;

			// add a to-do comment, if required by the measure, or  
			// - if the variable is declared inline with DATA(...) or FIELD-SYMBOL(...)
			// - if the variable is declared with DATA/CONSTANTS/STATICS BEGIN OF ... (bound structure data)  
			if (measure == UnusedVariableMeasure.ADD_TODO_COMMENT || varInfo.isDeclaredInline || varInfo.isBoundStructuredData) {
				String message;
				if (varInfo.isAssigned()) {
					if (varInfo.isUsedInComment())
						message = varAssignedButOnlyUsedInCommentMsg;
					else if (command.firstCodeTokenIsKeyword("MESSAGE")) // MESSAGE ... INTO DATA( )
						message = varAssignedButNeverUsedAddPragmaMsg;
					else
						message = varAssignedButNeverUsedMsg;
				} else if (varInfo.isUsedInComment() || varInfo.isAssignedInComment()) {
					message = (varInfo.isConstant ? constOnlyUsedInCommentMsg : varOnlyUsedInCommentMsg);
				} else {
					message = (varInfo.isConstant ? constNeverUsedMsg : varNeverUsedMsg);
				}
				boolean commentAdded = false;
				try {
					if (command.getClosesLevel()) { // in case of 'CATCH ... INTO DATA(...)', the comment cannot be added as a 'previous sibling', because that must be the TRY statement 
						commentAdded = (command.appendCommentToLineOf(varInfo.declarationToken, message) != null);
					} else {
						commentAdded = (command.putCommentAboveLineOf(varInfo.declarationToken, message) != null);
					}
				} catch (UnexpectedSyntaxException ex) {
					throw new UnexpectedSyntaxAfterChanges(this, ex);
				}
				if (commentAdded)
					code.addRuleUse(this, command);
				continue;
			}

			// get information on the line to be deleted / commented out and the surrounding lines
			Command originalCommand = (command.originalCommand != null) ? command.originalCommand : command;
			Token identifier = varInfo.declarationToken;
			Token keyword = command.getFirstToken();
			if (keyword.isComment()) // just to be sure; however, with the call to command.splitOutLeadingCommentLines() below, this should not happen anymore
				keyword = keyword.getNextNonCommentSibling();
			Token colon = keyword.getNext().isChainColon() ? keyword.getNext() : null;
			// boolean isKeywordOnSameLine = (identifier.lineBreaks == 0);
			Token commaOrPeriod = identifier.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ".|,");
			Token lastTokenInLine = (commaOrPeriod.getNext() != null && commaOrPeriod.getNext().isCommentAfterCode()) ? commaOrPeriod.getNext() : commaOrPeriod;
			boolean isInChain = command.isSimpleChain();
			boolean isFirstInChain = isInChain && identifier.getPrevNonCommentToken().isChainColon();
			boolean isLastInChain = isInChain && commaOrPeriod.isPeriod();
			boolean isOnlyOneInChain = isFirstInChain && isLastInChain;
			Token prevComma = (isInChain && !isFirstInChain) ? identifier.getPrevNonCommentSibling() : null;
			Token nextIdentifier = (isInChain && !isLastInChain) ? lastTokenInLine.getNextNonCommentSibling() : null;
			Token firstTokenInLine = (isInChain && !isFirstInChain) ? identifier : keyword;

			// make the surrounding chain work without the line to be deleted / commented out
			if (isInChain && !isOnlyOneInChain) {
				if (isLastInChain) {
					prevComma.setText(ABAP.DOT_SIGN_STRING, false);
					prevComma.type = TokenType.PERIOD;
				} else if (isFirstInChain) {
					nextIdentifier.copyWhitespaceFrom(identifier);
					int lineBreaks = (measure == UnusedVariableMeasure.DELETE) ? keyword.lineBreaks : 1;
					nextIdentifier.insertLeftSibling(Token.createForAbap(lineBreaks, keyword.spacesLeft, keyword.getText(), TokenType.KEYWORD, keyword.sourceLineNum));
					if (colon != null)
						nextIdentifier.insertLeftSibling(Token.createForAbap(0, colon.spacesLeft, colon.getText(), TokenType.COLON, colon.sourceLineNum));
				} else if (nextIdentifier != null && nextIdentifier.lineBreaks == 0) {
					nextIdentifier.copyWhitespaceFrom(identifier);
				}
			}

			switch (measure) {
				case COMMENT_OUT_WITH_ASTERISK:
				case COMMENT_OUT_WITH_QUOT:
					// create a comment line
					String lineText = Command.sectionToString(firstTokenInLine, lastTokenInLine, true);
					int indent = 0;
					if (measure == UnusedVariableMeasure.COMMENT_OUT_WITH_QUOT) {
						indent = firstTokenInLine.getStartIndexInLine();
						lineText = ABAP.COMMENT_SIGN_STRING + " " + StringUtil.trimStart(lineText);
					} else {
						lineText = ABAP.LINE_COMMENT_SIGN_STRING + lineText;
					}
					Token newComment = Token.createForAbap(Math.max(firstTokenInLine.lineBreaks, 1), indent, lineText, TokenType.COMMENT, firstTokenInLine.sourceLineNum);

					// insert the comment line, possibly as a new Command before or after the current one
					if ((isFirstInChain || isLastInChain) && !isOnlyOneInChain) {
						Command newCommand = Command.create(newComment, originalCommand);
						try {
							newCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
						} catch (ParseException e) {
							throw new UnexpectedSyntaxAfterChanges(null, command, "parse error in commented-out line");
						}

						// code.addRuleUse(this, newCommand); is not required, because it will have the same effect as "code.addRuleUse(this, command)" below
						if (isFirstInChain)
							command.insertLeftSibling(newCommand);
						else
							command.getNext().insertLeftSibling(newCommand);
						command.originalCommand = originalCommand;
					} else {
						firstTokenInLine.insertLeftSibling(newComment, false, true);
					}

					// remove the old declaration code
					try {
						// referential integrity cannot be checked at this point, because we may still need to split out leading comment lines
						Term.createForTokenRange(firstTokenInLine, lastTokenInLine).removeFromCommand(true);
					} catch (UnexpectedSyntaxException ex) {
						throw new UnexpectedSyntaxAfterChanges(this, ex);
					}
					break;

				case DELETE:
					// remove the declaration code (and possibly the whole Command)
					try {
						if (isInChain && !isOnlyOneInChain) {
							Term termToDelete = Term.createForTokenRange(firstTokenInLine, lastTokenInLine); 
							termToDelete.removeFromCommand(true);
						} else {
							// transfer line breaks to next command to keep sections separate
							if (command.getNext() != null && command.getFirstTokenLineBreaks() > command.getNext().getFirstTokenLineBreaks())
								command.getNext().getFirstToken().lineBreaks = command.getFirstTokenLineBreaks();
							command.removeFromCode();
						}
					} catch (UnexpectedSyntaxException ex) {
						throw new UnexpectedSyntaxAfterChanges(this, ex);
					}
					break;
				default:
					throw new IllegalArgumentException("Unknown Measure!");
			}

			// if the (remaining) Command starts with one or several comment lines, create separate Commands from it;
			// this may happen if the first line of the Command was deleted or commented out, and the next line(s) already were comment lines
			if (command.splitOutLeadingCommentLines(originalCommand))
				command.originalCommand = originalCommand;

			command.testReferentialIntegrity(true, true);
			
			code.addRuleUse(this, command);
		}
	}

}
