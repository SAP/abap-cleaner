package com.sap.adt.abapcleaner.rules.declarations;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoStyle;
import com.sap.adt.abapcleaner.rulebase.ConfigInfoValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class ClassDefinitionRule extends RuleForCommands {
	private enum OptionType {
		CLASS_DEF,
		PUBLIC,
		INHERITING,
		ABSTRACT,
		FINAL,
		CREATE,
		SHARED_MEMORY,
		FOR_TESTING,
		RISK_LEVEL,
		DURATION,
		BEHAVIOR,
		FRIENDS,
		LOCAL_FRIENDS;
		
		public int getValue() { 
			return this.ordinal(); 
		}
		public static OptionType forValue(int value) { 
			return values()[value]; 
		}
	}
	private static final int optionTypeCount = 13;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_KEYWORD_DOCU, "CLASS, class_options", "abapclass_options.htm") };

	@Override
	public RuleID getID() { return RuleID.CLASS_DEFINITION; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DECLARATIONS; }

	@Override
	public String getDisplayName() { return "Standardize CLASS ... DEFINITION"; }

	@Override
	public String getDescription() { return "Rearranges CLASS ... DEFINITION options into the order given in the ABAP Keyword Documentation and standardizes their layout."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 10, 28); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.UPPER_AND_LOWER_CASE } ; }

   @Override
   public String getExample() {
      return "" 
     		+ LINE_SEP + "\" here are some frequent CLASS ... DEFINITION patterns, including common displacement of options:"
    		+ LINE_SEP + "CLASS cl_any_class DEFINITION" 
    		+ LINE_SEP + "  INHERITING FROM cl_any_parent" 
    		+ LINE_SEP + "  PUBLIC CREATE PUBLIC." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_other_class DEFINITION PUBLIC FINAL CREATE PUBLIC. \"#EC INTF_IN_CLASS" 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_any_test_class DEFINITION" 
    		+ LINE_SEP + "  FINAL INHERITING FROM cl_any_test_base" 
    		+ LINE_SEP + "  FOR TESTING" 
    		+ LINE_SEP + "  RISK LEVEL HARMLESS" 
    		+ LINE_SEP + "  DURATION SHORT." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_other_test_class DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_any_class_with_friend DEFINITION" 
    		+ LINE_SEP + "PUBLIC FINAL CREATE PRIVATE GLOBAL FRIENDS cl_any_friend." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_other_class_with_friend DEFINITION PUBLIC" 
    		+ LINE_SEP + "  INHERITING FROM   cl_other_parent CREATE PRIVATE" 
    		+ LINE_SEP + "  GLOBAL FRIENDS cl_any_friend   cl_other_friend" 
    		+ LINE_SEP + "   cl_third_friend." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_any_rap_behavior_def_class DEFINITION ABSTRACT FINAL PUBLIC" 
    		+ LINE_SEP + "FOR BEHAVIOR OF bdef." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "CLASS cl_any_rap_event_handler_class DEFINITION" 
    		+ LINE_SEP + "                                     PUBLIC ABSTRACT FINAL FOR EVENTS OF  bdef." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "\" for multi-line definitions, an empty line is added above PUBLIC SECTION" 
    		+ LINE_SEP + "CLASS cl_any_shared_memory_enabled DEFINITION" 
    		+ LINE_SEP + "  PUBLIC FINAL CREATE   PUBLIC SHARED" 
    		+ LINE_SEP + "  MEMORY ENABLED." 
			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP + "ENDCLASS." 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "" 
    		+ LINE_SEP + "\" definition of local friends is a different statement, but can be formatted similarly:" 
    		+ LINE_SEP + "CLASS cl_any_local_class DEFINITION DEFERRED." 
    		+ LINE_SEP + "CLASS cl_other_local_class DEFINITION DEFERRED." 
    		+ LINE_SEP + "CLASS cl_public_class DEFINITION LOCAL FRIENDS "
    		+ LINE_SEP + "    cl_any_local_class"
    		+ LINE_SEP + "    cl_other_local_class."; 
   }

	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length", "", MIN_LINE_LENGTH_ABAP, DEFAULT_LINE_LENGTH_ABAP, ABAP.MAX_LINE_LENGTH);
	final ConfigEnumValue<ClassDefIndentStyle> configIndentStyle = new ConfigEnumValue<ClassDefIndentStyle>(this, "IndentStyle", "Indentation:",
			new String[] { "2 spaces", "4 spaces", "align with class name", "align with DEFINITION keyword" }, ClassDefIndentStyle.values(), ClassDefIndentStyle.PLUS_2);
	final ConfigEnumValue<ClassDefOneLinerAction> configOneLinerAction = new ConfigEnumValue<ClassDefOneLinerAction>(this, "OneLinerAction", "One-/Two-liners:",
			new String[] { "create after CLASS ... DEFINITION", "create below CLASS ... DEFINITION", "keep existing", "always split to multi-line" }, ClassDefOneLinerAction.values(), ClassDefOneLinerAction.KEEP);
	final ConfigInfoValue configMultiLiners = new ConfigInfoValue(this, "For multi-liners, start new line for:", ConfigInfoStyle.NORMAL);
	final ConfigBoolValue configNewLineForPublic = new ConfigBoolValue(this, "NewLineForPublic", "PUBLIC", true);
	final ConfigBoolValue configNewLineForInheriting = new ConfigBoolValue(this, "NewLineForInheriting", "INHERITING FROM ...", true);
	final ConfigBoolValue configNewLineForAbstractOrFinal = new ConfigBoolValue(this, "NewLineForAbstractOrFinal", "ABSTRACT, FINAL", false);
	final ConfigBoolValue configNewLineForCreate = new ConfigBoolValue(this, "NewLineForCreate", "CREATE...", true);
	final ConfigBoolValue configNewLineForSharedMemory = new ConfigBoolValue(this, "NewLineForSharedMemory", "SHARED MEMORY ENABLED", true);
	final ConfigBoolValue configNewLineForTesting = new ConfigBoolValue(this, "NewLineForTesting", "FOR TESTING ...", true);
	final ConfigBoolValue configNewLineForRiskAndDuration = new ConfigBoolValue(this, "NewLineForRiskAndDuration", "RISK LEVEL ..., DURATION ...", false);
	final ConfigBoolValue configNewLineForBehavior = new ConfigBoolValue(this, "NewLineForBehavior", "FOR BEHAVIOR OF ..., FOR EVENTS OF ...", true);
	final ConfigBoolValue configNewLineForFriends = new ConfigBoolValue(this, "NewLineForFriends", "[GLOBAL] FRIENDS ...", true);
	final ConfigBoolValue configNewLineForLocalFriends = new ConfigBoolValue(this, "NewLineForLocalFriends", "LOCAL FRIENDS ...", false);
	final ConfigBoolValue configNewLineForFriendNames = new ConfigBoolValue(this, "NewLineForFriendNames", "Further class names of FRIENDS", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configIndentStyle, configOneLinerAction, configMultiLiners, 
			configNewLineForPublic, configNewLineForInheriting, configNewLineForAbstractOrFinal, configNewLineForCreate, configNewLineForSharedMemory, 
			configNewLineForTesting, configNewLineForRiskAndDuration, configNewLineForBehavior, configNewLineForFriends, configNewLineForLocalFriends, configNewLineForFriendNames };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public ClassDefinitionRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// process CLASS ... DEFINITION, including CLASS ... DEFINITION LOCAL FRIENDS, but NOT ... DEFERRED or ... LOAD 
		if (!command.getFirstToken().matchesOnSiblings(true, "CLASS", TokenSearch.ANY_IDENTIFIER, "DEFINITION"))
			return false;
		else if (command.getFirstToken().matchesOnSiblings(true, "CLASS", TokenSearch.ANY_IDENTIFIER, "DEFINITION", "DEFERRED|LOAD"))
			return false;

		// comments are currently not supported, except at the very end 
		Token comment = command.getFirstToken().getNextTokenOfType(TokenType.COMMENT);
		if (comment != null && comment.getNext() != null)
			return false;

		// read configuration
		boolean[] ownLines = getOwnLinesConfiguration();
		ClassDefIndentStyle indentStyle = ClassDefIndentStyle.forValue(configIndentStyle.getValue()); 
		
		// identify the options used in this CLASS ... DEFINITION, storing them on terms[]
		Term[] options = identifyOptions(command);
		if (options == null)
			return false;
		Term classDef = options[OptionType.CLASS_DEF.getValue()];

		// determine whether all is currently on one line (after or below CLASS ... DEFINITION) 
		boolean isOneLiner = !command.containsLineBreaksBetween(classDef.lastToken.getNext(), null, false);
		boolean breaksAfterClassDef = (classDef.lastToken.getNext().lineBreaks == 0);
		
		// remove spaces within options
		boolean changed = false;
		if (condenseOptions(options))
			changed = true;
		
		// determine the indent (must be done after removing spaces above!)
		int indent = determineIndent(indentStyle, classDef);
		
		// determine whether a one-liner can be kept or created
		ClassDefOneLinerAction action = determineAction(options, indent, isOneLiner, breaksAfterClassDef);
		
		// rearrange the options and set line breaks
		int maxLineLength = configMaxLineLength.getValue();
		OptionType lastOptionType = OptionType.CLASS_DEF;
		Token writePos = command.getFirstCodeToken();

		for (int i = 0; i < optionTypeCount; ++i) {
			OptionType optionType = OptionType.forValue(i);
			Term option = options[i];
			if (option == null) 
				continue;

			// if needed, move the option to the 'write position' 
			if (option.firstToken != writePos) {
				option.removeFromCommand(false);
				writePos.insertLeftSibling(option);
				changed = true;
			}
			writePos = option.getNext();

			if (optionType != OptionType.CLASS_DEF) {
				// determine whether to start the next line
				boolean nextLine = false;
				if (action == ClassDefOneLinerAction.SPLIT) {
					nextLine = ownLines[i]; 
				} else if (action == ClassDefOneLinerAction.CREATE_ON_NEXT_LINE) {
					nextLine = (lastOptionType == OptionType.CLASS_DEF);
				}
				// force line break if continuing after previous token would exceed line length
				if (!nextLine && option.firstToken.getPrev().getEndIndexInLine() + option.getSumTextAndSpaceWidth() > maxLineLength) {
					nextLine = true;
				}
				if (option.firstToken.setWhitespace(nextLine ? 1 : 0, nextLine ? indent : 1)) {
					changed = true;
				}
			}

			lastOptionType = optionType;
		}

		// align multiple friend names
		if (alignFriendNames(options, action))
			changed = true;
		
		// put remaining pragmas behind the last option, if possible
		if (alignPragmas(indent, writePos))
			changed = true;

		// attach period (which could have been moved when options were removed and inserted)
		Token period = command.getLastNonCommentToken();
		if (period.isPeriod() && !period.getPrev().isComment() && period.setWhitespace(0, 0)) 
			changed = true;
		
		// after a multi-line CLASS ... DEFINITION, ensure that there is an empty line above the following PUBLIC SECTION
		// (as EmptyLinesInClassDefinitionRule was already executed earlier)
		Command nextCommand = command.getNext();
		if (nextCommand != null && !isCommandBlocked(nextCommand) && nextCommand.getFirstToken().matchesOnSiblings(true, "PUBLIC|PROTECTED|PRIVATE", "SECTION")) {
			if (command.containsInnerLineBreaks(false) && nextCommand.getFirstToken().lineBreaks < 2) {
				nextCommand.getFirstToken().lineBreaks = 2;
				code.addRuleUse(this, nextCommand);
			}
		}
		return changed;
	}

	private int determineIndent(ClassDefIndentStyle indentStyle, Term classDef) {
		int indent = indentStyle.indent;
		if (indentStyle == ClassDefIndentStyle.BELOW_DEFINITION) {
			indent = classDef.lastToken.getStartIndexInLine();
		} else if (indentStyle == ClassDefIndentStyle.BELOW_NAME) {
			// usually 6, but in case of an initial pragma, this may be more
			indent = classDef.firstToken.getNextCodeSibling().getStartIndexInLine();
		}
		return indent;
	}

	private boolean[] getOwnLinesConfiguration() {
		boolean[] ownLines = new boolean[optionTypeCount];

		// ownLines[OptionType.CLASS_DEF.getValue()] = configNewLineForClassDef.getValue();
		ownLines[OptionType.PUBLIC.getValue()] = configNewLineForPublic.getValue();
		ownLines[OptionType.INHERITING.getValue()] = configNewLineForInheriting.getValue();
		ownLines[OptionType.ABSTRACT.getValue()] = configNewLineForAbstractOrFinal.getValue();
		ownLines[OptionType.FINAL.getValue()] = configNewLineForAbstractOrFinal.getValue();
		ownLines[OptionType.CREATE.getValue()] = configNewLineForCreate.getValue();
		ownLines[OptionType.SHARED_MEMORY.getValue()] = configNewLineForSharedMemory.getValue();
		ownLines[OptionType.FOR_TESTING.getValue()] = configNewLineForTesting.getValue();
		ownLines[OptionType.RISK_LEVEL.getValue()] = configNewLineForRiskAndDuration.getValue();
		ownLines[OptionType.DURATION.getValue()] = configNewLineForRiskAndDuration.getValue();
		ownLines[OptionType.BEHAVIOR.getValue()] = configNewLineForBehavior.getValue();
		ownLines[OptionType.FRIENDS.getValue()] = configNewLineForFriends.getValue();
		ownLines[OptionType.LOCAL_FRIENDS.getValue()] = configNewLineForLocalFriends.getValue();
		
		return ownLines;
	}

	private Term[] identifyOptions(Command command) {
		Term[] options = new Term[optionTypeCount];

		// identify the options used in this CLASS ... DEFINITION, storing them on terms[]
		Token token = command.getFirstCodeToken();
		while (token != null && !token.isPeriod()) {
			Token startToken = token;

			// pragmas between the options are skipped for now; when options are reorganized, they will automatically 
			// end up at the end of the command 
			if (token.isPragma()) {
				token = token.getNext();
				continue;
			} 
			
			try {
				token = addOption(token, options, OptionType.CLASS_DEF, "CLASS", TokenSearch.ANY_IDENTIFIER, "DEFINITION");
				token = addOption(token, options, OptionType.PUBLIC, "PUBLIC");
				token = addOption(token, options, OptionType.INHERITING, "INHERITING", "FROM", TokenSearch.ANY_IDENTIFIER);
				token = addOption(token, options, OptionType.ABSTRACT, "ABSTRACT");
				token = addOption(token, options, OptionType.FINAL, "FINAL");
				token = addOption(token, options, OptionType.CREATE, "CREATE", "PUBLIC|PROTECTED|PRIVATE");
				token = addOption(token, options, OptionType.SHARED_MEMORY, "SHARED", "MEMORY", "ENABLED");

				token = addOption(token, options, OptionType.FOR_TESTING, "FOR", "TESTING");
				token = addOption(token, options, OptionType.RISK_LEVEL, "RISK", "LEVEL", "CRITICAL|DANGEROUS|HARMLESS");
				token = addOption(token, options, OptionType.DURATION, "DURATION", "SHORT|MEDIUM|LONG");
				
				// along with FOR BEHAVIOR OF ..., also include 'CLASS cl_event_handler DEFINITION PUBLIC [ABSTRACT] [FINAL] FOR EVENTS OF bdef.' 
				token = addOption(token, options, OptionType.BEHAVIOR, "FOR", "BEHAVIOR|EVENTS", "OF", TokenSearch.ANY_IDENTIFIER);
				token = addOption(token, options, OptionType.FRIENDS, TokenSearch.makeOptional("GLOBAL"), "FRIENDS", TokenSearch.ANY_IDENTIFIER);
				// 'CLASS class DEFINITION LOCAL FRIENDS class1 class2 ... intf1  intf2  ...' 
				token = addOption(token, options, OptionType.LOCAL_FRIENDS, "LOCAL", "FRIENDS", TokenSearch.ANY_IDENTIFIER);

			} catch (UnexpectedSyntaxException ex) {
				return null;
			}
			
			// if no option was identified, the syntax is unexpected
			if (token == startToken) {
				return null;
			}
		}
		return options;
	}

	private Token addOption(Token start, Term[] terms, OptionType optionType, String... texts) throws UnexpectedSyntaxException {
		Token last = start.getLastTokenOnSiblings(true, texts);
		if (last == null) 
			return start;
		
		if (optionType == OptionType.FRIENDS || optionType == OptionType.LOCAL_FRIENDS) {
			Token token = last;
			while (token != null && token.isIdentifier()) {
				last = token;
				token = token.getNextCodeSibling();
			}
		}
		
		terms[optionType.getValue()] = Term.createForTokenRange(start, last);
		return last.getNext();
	}
	
	private ClassDefOneLinerAction determineAction(Term[] options, int indent, boolean isOneLiner, boolean breaksAfterClassDef) {
		ClassDefOneLinerAction action = ClassDefOneLinerAction.forValue(configOneLinerAction.getValue());

		// if one-liners shall be kept, determine which action to take
		if (action == ClassDefOneLinerAction.KEEP) {
			if (isOneLiner) {
				action = breaksAfterClassDef ? ClassDefOneLinerAction.CREATE_ON_SAME_LINE 
													  : ClassDefOneLinerAction.CREATE_ON_NEXT_LINE;
			} else {
				action = ClassDefOneLinerAction.SPLIT;
			}
		}
		if (action == ClassDefOneLinerAction.SPLIT) 
			return action;

		int totalWidth = 0;
		for (int i = 0; i < optionTypeCount; ++i) {
			OptionType optionType = OptionType.forValue(i);
			Term option = options[i];
			if (option != null && optionType != OptionType.CLASS_DEF) {
				totalWidth += 1 + option.getSumTextAndSpaceWidth();
			}
		}
		int availableWidth = configMaxLineLength.getValue();
		if (action == ClassDefOneLinerAction.CREATE_ON_SAME_LINE) {
			availableWidth -= options[OptionType.CLASS_DEF.getValue()].lastToken.getEndIndexInLine();
		} else { // action == ClassDefOneLinerAction.CREATE_ON_NEXT_LINE
			availableWidth -= indent;
		}
		if (totalWidth > availableWidth) {
			action = ClassDefOneLinerAction.SPLIT;
		}
		return action;
	}

	private boolean condenseOptions(Term[] options) {
		boolean changed = false;
		for (int i = 0; i < optionTypeCount; ++i) {
			Term option = options[i];
			if (option == null)
				continue;
			// in the FRIENDS list, only condense up to the first identifier
			if (i == OptionType.FRIENDS.getValue() || i == OptionType.LOCAL_FRIENDS.getValue()) {
				Token condenseToken = option.firstToken.getNextCodeSibling();
				while (condenseToken != null) {
					if (condenseToken.setWhitespace()) {
						changed = true;
					}
					if (condenseToken.isIdentifier()) {
						break;
					}
					condenseToken = condenseToken.getNextCodeSibling();
				}
			} else if (option.condense()) {
				changed = true;
			}
		}
		return changed;
	}

	private boolean alignFriendNames(Term[] options, ClassDefOneLinerAction oneLinerAction) {
		int maxLineLength = configMaxLineLength.getValue();

		boolean changed = false;
		Term friends = options[OptionType.FRIENDS.getValue()];
		if (friends == null)
			friends = options[OptionType.LOCAL_FRIENDS.getValue()];
		
		if (friends != null) {
			boolean distinctLineForFriendNames = configNewLineForFriendNames.getValue();
			Token friend = friends.firstToken.getLastTokenOnSiblings(true, TokenSearch.makeOptional("GLOBAL|LOCAL"), "FRIENDS", TokenSearch.ANY_IDENTIFIER);
			boolean isFirstFriend = true;
			int friendIndent = 0;

			while (friend != null) {
				if (isFirstFriend) {
					// the space before the first friend name was already condensed in .condenseOptions()
					friendIndent = friend.getStartIndexInLine();
					isFirstFriend = false;
				} else {
					boolean nextLine = (oneLinerAction == ClassDefOneLinerAction.SPLIT && distinctLineForFriendNames
											|| friend.getPrev().isComment() // in case this will be supported in the future
											|| friend.getPrev().getEndIndexInLine() + 1 + friend.getTextLength() > maxLineLength);
					if (friend.setWhitespace(nextLine ? 1 : 0, nextLine ? friendIndent : 1)) {
						changed = true;
					}
				}
				if (friend == friends.lastToken)
					break;
				friend = friend.getNextCodeSibling();
			}
		}
		return changed;
	}

	private boolean alignPragmas(int indent, Token writePos) {
		int maxLineLength = configMaxLineLength.getValue();
		
		boolean changed = false;
		while (writePos != null && writePos.isPragma()) {
			if (writePos.getPrev().getEndIndexInLine() + 1 + writePos.getTextLength() > maxLineLength) {
				if (writePos.setWhitespace(1, indent)) {
					changed = true;
				}
			} else if (writePos.setWhitespace()) {
				changed = true;
			}
			writePos = writePos.getNextSibling();
		}
		return changed;
	}
}
