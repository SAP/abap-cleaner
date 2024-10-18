package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Section;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.Variables;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;

public class LocalDeclarationOrderRule extends RuleForDeclarations {
	private enum DeclarationType {
		CONSTANTS,
		STATICS,
		DATA,
		FIELD_SYMBOLS,
		ALL_BUT_CONSTANTS;
	}

	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Scope of Variable", "scope-of-variable.md"), 
			new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.LOCAL_DECLARATION_ORDER; } 

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Rearrange local declarations"; }

	@Override
	public String getDescription() { return "Rearranges up-front declarations of TYPES, CONSTANTS, DATA, and FIELD-SYMBOLS. " 
	+ "Chains are kept and only rearranged within, therefore this rule should be used in combination with the '" + ChainRule.displayName + "' rule. "; }

	@Override
	public String getHintsAndRestrictions() { return "Moving declarations to their innermost block is discouraged, but may help to refactor long methods."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 26); }
 
	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_DECLARATIONS, RuleID.UNUSED_VARIABLES } ; }

	@Override
	public boolean isActiveByDefault() { return false; }
	
	@Override
   public String getExample() { 
		return ""
		+ LINE_SEP + "  METHOD rearrange_local_declarations."
		+ LINE_SEP + "    TYPES: BEGIN OF ty_s_data,"
		+ LINE_SEP + "             name  TYPE string,"
		+ LINE_SEP + "             count TYPE i,"
		+ LINE_SEP + "           END OF ty_s_data."
		+ LINE_SEP + "    DATA ls_data_avg TYPE ty_s_data."
		+ LINE_SEP + "    TYPES ty_ts_data TYPE SORTED TABLE OF ty_s_data WITH NON-UNIQUE KEY name."
		+ LINE_SEP + "    STATICS sv_call_count TYPE i."
		+ LINE_SEP + "    sv_call_count += 1."
		+ LINE_SEP + ""
		+ LINE_SEP + "    DATA lts_result TYPE ty_ts_data."
		+ LINE_SEP + "    CLEAR lts_result."
		+ LINE_SEP + "    DATA ls_data_max TYPE ty_s_data."
		+ LINE_SEP + "    CONSTANTS lc_name_average TYPE string VALUE `average`."
		+ LINE_SEP + "    DATA lts_data TYPE ty_ts_data."
		+ LINE_SEP + "    lts_data = CORRESPONDING #( get_data( ) )."
		+ LINE_SEP + ""
		+ LINE_SEP + "    \" field-symbol for calculating the average count"
		+ LINE_SEP + "    FIELD-SYMBOLS <ls_data_avg> TYPE ty_s_data."
		+ LINE_SEP + ""
		+ LINE_SEP + "*    UNASSIGN <ls_data_avg>."
		+ LINE_SEP + "    IF iv_get_maximum = abap_true."
		+ LINE_SEP + "      \" field-symbol for calculating the maximum count"
		+ LINE_SEP + "      FIELD-SYMBOLS <ls_data_max> TYPE ty_s_data."
		+ LINE_SEP + "      CONSTANTS lc_name_maximum TYPE string VALUE `maximum`."
		+ LINE_SEP + ""
		+ LINE_SEP + "      ls_data_max-name = lc_name_maximum."
		+ LINE_SEP + "      LOOP AT lts_data ASSIGNING <ls_data_max>."
		+ LINE_SEP + "        IF <ls_data_max>-count > ls_data_max-count."
		+ LINE_SEP + "          ls_data_max-count = <ls_data_max>-count."
		+ LINE_SEP + "        ENDIF."
		+ LINE_SEP + "      ENDLOOP."
		+ LINE_SEP + ""
		+ LINE_SEP + "      INSERT ls_data_max INTO TABLE lts_result."
		+ LINE_SEP + "    ENDIF."
		+ LINE_SEP + ""
		+ LINE_SEP + "    IF iv_get_average = abap_true."
		+ LINE_SEP + "      ls_data_avg-name = lc_name_average."
		+ LINE_SEP + "      LOOP AT lts_data ASSIGNING <ls_data_avg>."
		+ LINE_SEP + "        ls_data_avg-count += <ls_data_avg>-count."
		+ LINE_SEP + "      ENDLOOP."
		+ LINE_SEP + "      ls_data_avg-count /= lines( lts_data )."
		+ LINE_SEP + ""
		+ LINE_SEP + "      INSERT ls_data_avg INTO TABLE lts_result."
		+ LINE_SEP + "    ENDIF."
		+ LINE_SEP + ""
		+ LINE_SEP + "    CONSTANTS lc_name_call_count TYPE string VALUE `call count`."
		+ LINE_SEP + "    INSERT VALUE #( name  = lc_name_call_count"
		+ LINE_SEP + "                    count = sv_call_count ) INTO TABLE lts_result."
		+ LINE_SEP + ""
		+ LINE_SEP + "    rts_result = CORRESPONDING #( lts_result )."
		+ LINE_SEP + "  ENDMETHOD.";
   }

	private String[] typesDeclarationOrderNames = new String[] { "move to method start, keeping current order" }; 
	private String[] declarationOrderNames = new String[] { "move to method start, keeping current order", "move to method start, order by first usage", "move to innermost block, order by first usage" };
	
	final ConfigEnumValue<LocalTypesDeclarationOrder> configTypesOrder = new ConfigEnumValue<LocalTypesDeclarationOrder>(this, "TypesOrder", "Rearrange TYPES", typesDeclarationOrderNames, LocalTypesDeclarationOrder.values(), LocalTypesDeclarationOrder.METHOD_START_KEEP_ORDER);
	final ConfigEnumValue<LocalDeclarationOrder> configConstantsOrder = new ConfigEnumValue<LocalDeclarationOrder>(this, "ConstantsOrder", "Rearrange CONSTANTS", declarationOrderNames, LocalDeclarationOrder.values(), LocalDeclarationOrder.METHOD_START_KEEP_ORDER);
	final ConfigEnumValue<LocalDeclarationOrder> configStaticsOrder = new ConfigEnumValue<LocalDeclarationOrder>(this, "StaticsOrder", "Rearrange STATICS", declarationOrderNames, LocalDeclarationOrder.values(), LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
	final ConfigEnumValue<LocalDeclarationOrder> configDataOrder = new ConfigEnumValue<LocalDeclarationOrder>(this, "DataOrder", "Rearrange DATA", declarationOrderNames, LocalDeclarationOrder.values(), LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
	final ConfigEnumValue<LocalDeclarationOrder> configFieldSymbolsOrder = new ConfigEnumValue<LocalDeclarationOrder>(this, "FieldSymbolsOrder", "Rearrange FIELD-SYMBOLS", declarationOrderNames, LocalDeclarationOrder.values(), LocalDeclarationOrder.METHOD_START_CHANGE_ORDER);
	final ConfigBoolValue configDistinctBlocks = new ConfigBoolValue(this, "DistinctBlocks", "Create distinct blocks for STATICS, DATA, and FIELD-SYMBOLS", true);
	final ConfigBoolValue configEmptyLine = new ConfigBoolValue(this, "EmptyLine", "Put empty line between blocks of STATICS, DATA, and FIELD-SYMBOLS)", true);
	final ConfigBoolValue configMoveComments = new ConfigBoolValue(this, "MoveComments", "Move attached comments", true);
	final ConfigBoolValue configRearrangeChains = new ConfigBoolValue(this, "RearrangeChains", "Rearrange declarations within chains", true);
	final ConfigBoolValue configConsiderComments = new ConfigBoolValue(this, "ConsiderComments", "Consider usage in commented-out code", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configTypesOrder, configConstantsOrder, configStaticsOrder, configDataOrder, configFieldSymbolsOrder, configDistinctBlocks, configEmptyLine, configMoveComments, configRearrangeChains, configConsiderComments };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }
	
	private LocalDeclarationOrder getConstantsOrder() { return LocalDeclarationOrder.forValue(configConstantsOrder.getValue()); }
	private LocalDeclarationOrder getStaticsOrder() { return LocalDeclarationOrder.forValue(configStaticsOrder.getValue()); }
	private LocalDeclarationOrder getDataOrder() { return LocalDeclarationOrder.forValue(configDataOrder.getValue()); }
	private LocalDeclarationOrder getFieldSymbolsOrder() { return LocalDeclarationOrder.forValue(configFieldSymbolsOrder.getValue()); }
	
	public LocalDeclarationOrderRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		HashMap<Command, Command> writePosOfParent = new HashMap<>(); 
		HashMap<Command, Token> writePosOfChain = new HashMap<>(); 
		
		if (isCommandBlocked(methodStart) || localVariables.isEmpty())
			return;
		
		// move TYPES to start of method
		Command command = methodStart.getNext();
		while (command != null && command != methodStart.getNextSibling()) {
			if (command.firstCodeTokenIsKeyword("TYPES")) {
				Section section = getSectionOfTypesDeclarations(command);
				// as the Section will be moved, determine the next Command already now 
				Command next = section.lastCommand.getNext();
				moveSection(section, methodStart, methodStart, code, writePosOfParent, localVariables);
				command = next;
			} else {
				command = command.getNext();
			}
		}
		
		rearrange(DeclarationType.CONSTANTS, getConstantsOrder(), code, methodStart, localVariables, writePosOfParent, writePosOfChain);
		if (configDistinctBlocks.getValue()) {
			rearrange(DeclarationType.STATICS, getStaticsOrder(), code, methodStart, localVariables, writePosOfParent, writePosOfChain);
			rearrange(DeclarationType.DATA, getDataOrder(), code, methodStart, localVariables, writePosOfParent, writePosOfChain);
			rearrange(DeclarationType.FIELD_SYMBOLS, getFieldSymbolsOrder(), code, methodStart, localVariables, writePosOfParent, writePosOfChain);
		} else {
			rearrange(DeclarationType.ALL_BUT_CONSTANTS, getDataOrder(), code, methodStart, localVariables, writePosOfParent, writePosOfChain);
		}
	}

	/** rearranges all declarations of the supplied type in the configured order */
	private void rearrange(DeclarationType declarationType, LocalDeclarationOrder useOrder, Code code, Command methodStart, Variables localVariables, HashMap<Command, Command> writePosOfParent, HashMap<Command, Token> writePosOfChain) throws UnexpectedSyntaxAfterChanges {
		boolean rearrangeChains = configRearrangeChains.getValue();
		boolean considerComments = configConsiderComments.getValue();
		
		for (VariableInfo varInfo : getLocalsOrder(useOrder, localVariables)) {
			if (!variableMatchesFilter(varInfo, declarationType))
				continue;
			
			Command declaration = varInfo.declarationToken.getParentCommand();

			// ensure that a declaration that contains 'BEGIN OF' or 'END OF' is NOT processed, because this must NOT be
			// moved to any other place, and its elements must NOT be rearranged within. Example:
		   //   DATA: BEGIN OF ls_struc.
		   //           INCLUDE STRUCTURE incl_struc.
		   //   DATA: END OF ls_struc,
		   //         lv_any TYPE i.
		   // (a less restrictive condition could allow processing of declarations in which all BEGIN OFs are closed 
			// with corresponding(!) END OFs)
			if (declaration.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, "BEGIN|END", "OF"))
				continue;
			
			// rearrange declaration chains within  
			if (declaration.isSimpleChain())  {
				// if the chain contains multiple elements, rearrange it within
				if (rearrangeChains && declaration.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, ABAP.COMMA_SIGN_STRING)) {
					Term term = getTermInChain(varInfo.declarationToken);
					moveTermInChain(term, declaration, code, writePosOfChain);
				}
				
				// Only move the whole chain below if this is (now) the first variable within the chain 
				if (!varInfo.declarationToken.getPrevCodeSibling().isChainColon())
					continue;
			}

			// determine the Command behind which the declaration shall be moved; chains with multiple declarations are simply kept at method start
			Command enclosingCommand = null;
			if (useOrder == LocalDeclarationOrder.ENCLOSING_BLOCK_CHANGE_ORDER) {
				enclosingCommand = varInfo.getEnclosingCommand(considerComments); 
			}
			boolean chainsMultipleVars = declaration.isSimpleChain() && declaration.getFirstToken().matchesOnSiblings(true, TokenSearch.ASTERISK, ABAP.COMMA_SIGN_STRING);
			if (enclosingCommand == null || chainsMultipleVars)
				enclosingCommand = methodStart;

			Section section = getSection(declaration, declaration);
			moveSection(section, methodStart, enclosingCommand, code, writePosOfParent, localVariables);
		}
	}

	/** returns the order in which local variables shall be rearranged, depending on configuration */
	private Iterable<VariableInfo> getLocalsOrder(LocalDeclarationOrder useOrder, Variables localVariables) {
		if (useOrder == LocalDeclarationOrder.METHOD_START_KEEP_ORDER) 
			return localVariables.getLocalsInDeclarationOrder();
		else if (configConsiderComments.getValue())
			return localVariables.getLocalsInUsageOrder();
		else 
			return localVariables.getLocalsInNonCommentUsageOrder();
	}

	/** returns true if the supplied variable shall be processed under the supplied declaration type; 
	 * always returns false for inline declarations and for declarations that cannot be moved 
	 * because their LIKE clause refers to a variable that is declared inline */
	private boolean variableMatchesFilter(VariableInfo varInfo, DeclarationType declarationType) {
		if (varInfo.isParameter() || varInfo.isDeclaredInline || varInfo.declarationCannotBeMoved)
			return false;
		
		Token keyword = varInfo.declarationToken.getParentCommand().getFirstCodeToken();
		
		switch(declarationType) {
			case CONSTANTS:
				return keyword.isKeyword("CONSTANTS");
			case STATICS:
				return keyword.isKeyword("STATICS");
			case DATA:
				return keyword.isKeyword("DATA");
			case FIELD_SYMBOLS:
				return keyword.isKeyword("FIELD-SYMBOLS");
			case ALL_BUT_CONSTANTS:
				return keyword.isAnyKeyword("STATICS", "DATA", "FIELD-SYMBOLS");
			default:
				throw new IllegalArgumentException();
		}
	}

	// -------------------------------------------------------------------------
	// rearranging declaration commands within a method
	
	/** returns a section of subsequent TYPES declarations (and comments between them), starting from the supplied Command */
	private Section getSectionOfTypesDeclarations(Command startCommand) throws UnexpectedSyntaxAfterChanges {
		// extend the Section as long as TYPES are defined, also allowing for comments in between TYPES
		Command lastCommand = startCommand;
		Command testCommand = startCommand;
		do {
			lastCommand = testCommand;
			testCommand = testCommand.getNextSibling();
			if (testCommand == null || isCommandBlocked(testCommand))
				break;
		} while (testCommand.firstCodeTokenIsAnyKeyword("TYPES", "INCLUDE") || testCommand.isCommentLine());

		// reduce the Section again to NOT include final comments
		if (lastCommand.isCommentLine()) 
			lastCommand = lastCommand.getPrevNonCommentSibling();

		return getSection(startCommand, lastCommand);
	}
	
	/** creates a Section, extending the supplied command range by preceding attached comments and following pragmas */
	private Section getSection(Command startCommand, Command lastCommand) throws UnexpectedSyntaxAfterChanges  {
		try {
			return Section.create(extendSectionStart(startCommand), extendSectionEnd(lastCommand));
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}

	/** extends the start of a to-be-moved Section with preceding attached comments */
	private Command extendSectionStart(Command startCommand) {
		Command command;

		if (configMoveComments.getValue()) {
			// move to the start of the attached comments, unless they are blocked
			command = startCommand;
			while (command.getFirstTokenLineBreaks() == 1) {
				Command prevCommand = command.getPrev();
				if (prevCommand == null || isCommandBlocked(prevCommand))
					break;
				if (!prevCommand.isCommentLine())
					break;
				command = prevCommand;
			}
	
			if (command == startCommand)
				return command;

			// at the very beginning of a method, consider comments to belong to the method (not the declaration)
			if (command.getFirstTokenLineBreaks() == 1 && command.getPrev() != null && command.getPrev().isMethodFunctionFormOrEventBlockStart()) {
				// use below logic to only include attached to-do comments and ABAP doc comments
			} else {
				// if below the startCommand, there are empty lines or further comment lines before the next non-declaration 
				// command, include the comment in the section
				Command next = startCommand.getNextSibling();
				while (next != null) {
					if (next.isCommentLine() || next.getFirstTokenLineBreaks() > 1)
						return command;
					if (!next.isDeclaration())
						break;
					next = next.getNextSibling();
				}
			}
		}
		
		// otherwise, only include preceding comments of the following type: 
		// - to-do comments generated by UnusedVariablesRule
		// - ABAP doc "! comments (which are also possible for local variables)
		Command testCommand = startCommand;
		do {
			command = testCommand;
			if (command.getFirstTokenLineBreaks() > 1)
				break;

			testCommand = command.getPrevSibling();
			if (testCommand == null)
				break;

			if (testCommand.isAbapDoc()) {
				continue;
			} else if (testCommand.isCommentLine()) {
				String commentText = testCommand.getFirstToken().getText();
				if (StringUtil.containsAny(commentText, UnusedVariablesRule.varMessages) 
				 || StringUtil.containsAny(commentText, UnusedVariablesRule.constMessages)) {
					continue;
				}
			}
			break;
		} while(true);

		return command;
	}

	/** extends the start of a to-be-moved Section with (incorrectly positioned) pragmas that follow the last command 
	 * on the same line (note that a pragma after a period is an independent Command) */
	private Command extendSectionEnd(Command lastCommand) {
		Command command = lastCommand;
		while (command.getNext() != null) {
			Command next = command.getNext();
			if (!next.isPragmaLine() || next.getFirstToken().lineBreaks > 0) 
				break;
			command = next;
		};
		return command;
	}

	/** moves the supplied Section to the next write position under the supplied ('enclosing') parent Command, adjusts empty lines 
	 * before and after the (old and new) position of the section, and updates the write position */
	private void moveSection(Section section, Command methodStart, Command parent, Code code, HashMap<Command, Command> writePosOfParent, Variables localVariables) throws UnexpectedSyntaxAfterChanges {
		// do not move the section if any of its Commands are blocked
		Command command = section.firstCommand;
		while (command != null) {
			if (isCommandBlocked(command))
				return;
			if (command == section.lastCommand)
				break;
			command = command.getNext();
		}
		
		// determine the write position, i.e. the Command before which the Section shall be inserted
		Command writePos = writePosOfParent.get(parent);
		if (writePos == null) {
			writePos = getInitialWritePos(parent, (parent == methodStart));
			writePosOfParent.put(parent, writePos);
		}

		// if a section contains declarations "LIKE ...", it can only be moved it if all referenced local declarations 
		// are found before writePos; note that even TYPES could be declared LIKE a DATA declaration etc.
		if (!canSectionBeMoved(section, writePos, methodStart, localVariables))
			return;

		// remember line breaks before the section for later
		int oldLineBreaksBeforeSection = section.firstCommand.getFirstTokenLineBreaks();

		// calculate line breaks of the section start (even if the section is already in the correct place);
		// if we move it directly behind the parent command, simply keep the existing number of line breaks there 
		// (e.g. for method starts, this is specifically covered by the EmptyLinesWithinMethodsRule;
		// similarly, after complex multi-line IF or LOOP statements, an extra line may be wanted)
		int lineBreaks;
		if (writePos == parent.getNext() && writePos.getFirstTokenLineBreaks() > 0) {
			lineBreaks = parent.getNext().getFirstTokenLineBreaks();
		} else {
			Command firstDeclaration = section.firstCommand.isCommentLine() ? section.firstCommand.getNextNonCommentSibling() : section.firstCommand;
			lineBreaks = getLineBreaksBetween(writePos.getPrevSibling(), firstDeclaration); // or (..., section.firstCommand)
		}
		Token firstToken = section.firstCommand.getFirstToken(); 

		boolean wasSectionMoved = false;
		if (section.contains(writePos)) {
			// for sections that are already in the right place, keep existing empty lines; after rearranging declarations 
			// once, this allows for purposefully introducing extra lines which are then kept
			if (firstToken.setLineBreaks(Math.max(lineBreaks, firstToken.lineBreaks)))
				code.addRuleUse(this, section.firstCommand);

			// if the section already is in the correct place, simply adjust writePos
			writePos = section.lastCommand.getNextSibling(); 
			// writePos may be null now, e.g. if the method only contains CONSTANTS declarations
			writePosOfParent.put(parent, writePos);
			
			// only adjust line breaks for the next command (see below) if it is NOT a declaration
			Command next = section.lastCommand.getNextNonCommentSibling();
			if (next != null && next.isDeclaration()) {
				return;
			}

		} else {
			if (firstToken.setLineBreaks(lineBreaks)) 
				code.addRuleUse(this, section.firstCommand);

			// if the first command of the section is itself in a write position, update that write position
			for (Command otherParent : writePosOfParent.keySet()) {
				Command otherWritePos = writePosOfParent.get(otherParent);
				if (section.firstCommand == otherWritePos) {
					writePosOfParent.put(otherParent, section.lastCommand.getNextSibling());
					break;
				}
			}
			
			// adjust line breaks of the command that was behind the section before it is moved
			Command next = section.lastCommand.getNext();
			if (next != null) {
				int lineBreaksAfterSection;
				Command prevSibling = section.firstCommand.getPrevSibling();
				Command nextNonCommentSibling = section.lastCommand.getNextNonCommentSibling();
				lineBreaksAfterSection = getLineBreaksBetween(prevSibling, nextNonCommentSibling);
				if (prevSibling == null) {
					lineBreaksAfterSection = oldLineBreaksBeforeSection;
				} else if (prevSibling.isDeclaration() && nextNonCommentSibling != null && !nextNonCommentSibling.isDeclaration()) {
					// keep line breaks as they were beforehand
					lineBreaksAfterSection = next.getFirstToken().lineBreaks; // getLineBreaksBetween(prevSibling, nextNonCommentSibling);
				} else {
					lineBreaksAfterSection = Math.max(oldLineBreaksBeforeSection, next.getFirstTokenLineBreaks());
				}
				next.getFirstToken().lineBreaks = lineBreaksAfterSection;
			}
	
			// adjust indentation of the section (before moving it)
			int oldIndent = section.firstCommand.getIndent();
			int newIndent = writePos.getIndent();
			section.addIndent(newIndent - oldIndent);
			
			// move the section to its new place
			section.removeFromCode();
			writePos.insertLeftSibling(section);
			wasSectionMoved = true;
			
			code.addRuleUses(this, section);
		}

		// adjust line breaks of the command that is now behind the section
		Command nextSibling = section.lastCommand.getNextSibling();
		if (nextSibling != null) {
			lineBreaks = nextSibling.getFirstTokenLineBreaks();
			// if the next Command is not a declaration, always put an empty line
			Command nextNonCommentSibling = section.lastCommand.getNextNonCommentSibling();
			if (nextNonCommentSibling != null && !nextNonCommentSibling.isDeclaration()) {
				if (wasSectionMoved) {
					// if the section was just moved in front of the next Command, put exactly one empty line 
					lineBreaks = 2;
				} else {
					// if the next Command was there already, ensure an empty line, but possibly keep more 
					lineBreaks = Math.max(nextSibling.getFirstToken().lineBreaks, 2);
				}
			} else {
				// find the last declaration command of the section (.lastCommand may be a pragma line)
				Command lastDeclaration = section.lastCommand;
				while (lastDeclaration.isPragmaLine())
					lastDeclaration = lastDeclaration.getPrevNonCommentSibling(); 
				lineBreaks = getLineBreaksBetween(lastDeclaration, nextNonCommentSibling); // or (..., nextSibling)
			}
			if (nextSibling.getFirstToken().setLineBreaks(lineBreaks)) {
				code.addRuleUse(this, nextSibling);
			}
		} 
	}

	private boolean canSectionBeMoved(Section section, Command writePos, Command methodStart, Variables localVariables) {
		HashSet<Command> declarationsBefore = null;
		for (VariableInfo testLocal : localVariables.getLocalsInDeclarationOrder()) {
			if (testLocal.isParameter() || testLocal.getTypeSource() == null || !section.contains(testLocal.declarationToken.getParentCommand())) {
				continue;
			}
			// build a hash set of all declaration Commands before the write position (just in time) 
			if (declarationsBefore == null) {
				declarationsBefore = new HashSet<>();
				Command commandBefore = writePos.getPrev();
				while (commandBefore != null && commandBefore != methodStart) {
					if (commandBefore.isDeclaration()) {
						declarationsBefore.add(commandBefore);
					}
					commandBefore = commandBefore.getPrev();
				}
			}
			if (!declarationsBefore.contains(testLocal.getTypeSource().declarationToken.getParentCommand())) {
				return false;
			}
		}
		return true;
	}
	
	/** determines the initial write position inside the 'enclosing' parent Command */
	private Command getInitialWritePos(Command parent, boolean parentIsMethodStart) throws UnexpectedSyntaxAfterChanges {
		Command writePos = parent.getFirstChild();
		if (writePos == null)
			throw new UnexpectedSyntaxAfterChanges(this, parent, "Expected commands inside this block.");
	
		if (!writePos.isCommentLine()) 
			return writePos;
		
		// in inner blocks, always put declarations first (before existing comments)
		if (!parentIsMethodStart)
			return writePos;
		
		// at method start, skip initial comments, unless they are attached to the following code
		writePos = writePos.getNextNonCommentSibling();
		if (writePos == null)
			throw new UnexpectedSyntaxAfterChanges(this, parent, "Expected non-comment commands inside this block.");

		// if the next non-comment command is a declaration, reuse the logic for related comments
		if (writePos.isDeclaration()) {
			return extendSectionStart(writePos);
		} 
		
		// otherwise, if the comments are both attached to the method start and the following code, 
		// the method start keeps them (i.e. the initial write position is after the comments)
		Command startOfAttached = writePos.getStartOfAttachedComments();
		if (startOfAttached != parent.getFirstChild() && startOfAttached.getFirstTokenLineBreaks() > 1) {
			return startOfAttached;
		} 

		return writePos;
	}

	/** returns the number of line breaks to put between the supplied commands (or between command 1 and the comment lines 
	 * that precede the supplied non-comment command 2) */
	private int getLineBreaksBetween(Command command1, Command command2) {
		// no empty line at the beginning and and of an indented block
		if (command1 == null || command2 == null)
			return 1;

		// empty line between declaration and comment
		if (command1.isDeclaration() && command2.isCommentLine())
			return 2;
		// empty line between comment line and declaration
		if (command1.isCommentLine() && command2.isDeclaration())
			return 2;

		Token keyword1 = command1.getFirstCodeToken();
		Token keyword2 = command2.getFirstCodeToken();
		if (keyword1 == null || keyword2 == null) {
			// no change if one of the commands is a comment line
			return command2.getFirstTokenLineBreaks();
		}
		
		// between two declarations:
		if (keyword1.textEquals(keyword2.getText())) {
			// no empty line between declarations of the same type
			return 1;
		} else if (keyword1.isAnyKeyword("TYPES", "CONSTANTS") || keyword2.isAnyKeyword("TYPES", "CONSTANTS")) {
			// empty line between different declaration blocks if one of them is TYPES or CONSTANTS
			return 2;
		} else {
			// if configured, empty line between different blocks of STATICS, DATA, and FIELD-SYMBOLS
			return configEmptyLine.getValue() ? 2 : 1;
		}
	}
	
	// -------------------------------------------------------------------------
	// rearranging declarations within a chain
	
	/** creates a Term that contains all Tokens that belong to the supplied declaration Token, including pragmas, 
	 * preceding comment lines and line-end comments */
	private Term getTermInChain(Token declarationToken) throws UnexpectedSyntaxAfterChanges {
		Token start = declarationToken;
		Token last = declarationToken;
		
		// process cases of 'BEGIN OF struc, ..., END OF struc' as a whole 
		// (however, currently, processing of such chains is completely prevented in rearrange())
		Token prev = start.getPrevCodeSibling();
		Token prevPrev = (prev == null) ? null : prev.getPrevCodeSibling();
		if (prevPrev != null && prevPrev.isKeyword("BEGIN") && prev.isKeyword("OF")) {
			start = prevPrev;
			last = start.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "END", "OF", declarationToken.getText());
			if (last == null)
				return null;
		}
		
		try {
			return Term.createForTokenRange(extendTermStart(start), extendTermEnd(last));
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}
	
	/** extends the start of a to-be-moved Term with preceding pragmas and comment lines */
	private Token extendTermStart(Token declarationToken) {
		// include pragmas and comment lines before the declaration token
		Token start = declarationToken;
		do {
			Token prev = start.getPrev();
			if (start.lineBreaks == 0 && prev.isPragma()) {
				// continue below
			} else if (!prev.isCommentLine()) {
				return start;
			}
			start = prev;
		} while(true);
	}

	/** extends the end of a to-be-moved Term with (incorrectly positioned) pragmas that follow the comma or period 
	 * on the same line, as well as line-end comments */
	private Token extendTermEnd(Token declarationToken) {
		Token commaOrPeriod = declarationToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ",|.");
		Token last = commaOrPeriod;
		
		// include line-end pragmas and comments, except when the next declaration also follows on the same line
		do {
			Token next = last.getNext();
			if (next == null || next.lineBreaks > 0)
				return last;

			// if the same line continues with another declaration, then don't include pragmas after the comma  
			if (!next.isPragmaOrComment()) {
				return commaOrPeriod;
			}
			
			last = next;
		} while(true);
	}

	/** moves the supplied Term to the next write position within the chain, adjusts whitespace before and after the 
	 * (old and new) position of the Term, and updates the write position */
	private void moveTermInChain(Term term, Command declaration, Code code, HashMap<Command, Token> writePosOfChain) throws IntegrityBrokenException {
		// do not move the Term if no Term could be determined, or if the chain Command is blocked
		if (term == null || isCommandBlocked(declaration))
			return;

		// determine the write position, i.e. the Token after(!) which the Term shall be inserted
		Token initialWritePos = getInitialWritePosOfChain(declaration);
		Token writePos = writePosOfChain.get(declaration);
		if (writePos == null) {
			writePos = initialWritePos;
			writePosOfChain.put(declaration, writePos);
		}

		// determine the first identifier after the chain start (or possibly a keyword in case of BEGIN OF struc, ..., END OF struc)
		Token firstIdentifier = initialWritePos.getNextTokenOfTypes(TokenType.IDENTIFIER, TokenType.KEYWORD); 
		int indent = (firstIdentifier == null) ? declaration.getFirstToken().spacesLeft + ABAP.INDENT_STEP : firstIdentifier.getStartIndexInLine();
		
		if (term.contains(writePos.getNext())) {
			writePosOfChain.put(declaration, term.lastToken);
			
		} else {
			// adjust whitespace of term start
			if (term.firstToken.isAsteriskCommentLine()) {
				term.firstToken.lineBreaks = 1;
			} else if (writePos == initialWritePos && firstIdentifier != null) {
				term.firstToken.copyWhitespaceFrom(firstIdentifier);
			} else {
				term.firstToken.setWhitespace(1, indent);
			}
			
			// adjust whitespace of declaration after the term before it is moved
			Token tokenAfterTerm = term.lastToken.getNext();
			if (tokenAfterTerm != null) {
				if (tokenAfterTerm.isAsteriskCommentLine()) {
					tokenAfterTerm.lineBreaks = 1;
				} else {
					tokenAfterTerm.setWhitespace(1, indent);
				}
			}
			
			// if the term ends with a period, change that into a comma, and change the previous comma into a period  
			Token period = declaration.getLastCodeToken();
			if (term.contains(period)) {
				period.setText(ABAP.COMMA_SIGN_STRING, false);
				period.type = TokenType.COMMA;
				
				Token comma = term.firstToken.getPrevTokenOfType(TokenType.COMMA);
				comma.setText(ABAP.DOT_SIGN_STRING, false);
				comma.type = TokenType.PERIOD;
			}
			
			// move the term to its new place
			term.removeFromCommand(false);
			writePos.insertRightSibling(term);
			
			writePosOfChain.put(declaration, term.lastToken);
			
			code.addRuleUse(this, declaration);
		}
		
		// adjust whitespace of the declaration that is now behind the term
		Token next = term.lastToken.getNext();
		if (next != null) {
			if (next.isAsteriskCommentLine()) {
				next.lineBreaks = 1;
			} else {
				next.setWhitespace(1, indent);
			}
		}
	}

	/** determines the initial write position inside the supplied declaration chain */
	private Token getInitialWritePosOfChain(Command declaration) {
		Token chainColon = declaration.getFirstToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ":");
		Token writePos = chainColon;
		
		// include line-end pragmas and comments, except when the next declaration also follows on the same line
		do {
			Token next = writePos.getNext();
			if (next == null || next.lineBreaks > 0)
				return writePos;

			// if the same line continues with another declaration, then don't include pragmas after the comma  
			if (!next.isPragmaOrComment()) {
				return chainColon;
			}
			
			writePos = next;
		} while(true);
	}

}
