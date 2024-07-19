package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class NeedlessSpacesRule extends Rule {
	private static class TokenPos {
		final Token token;
		final int startIndexInLine;
		final int lineNumber;
		final int commandNumber;
		
		TokenPos(Token token, int startIndexInLine, int lineNumber, int commandNumber) {
			this.token = token;
			this.startIndexInLine = startIndexInLine;
			this.lineNumber = lineNumber;
			this.commandNumber = commandNumber;
		}
	}

	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Condense your code", "#condense-your-code") };

	@Override
	public RuleID getID() { return RuleID.NEEDLESS_SPACES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Remove needless spaces"; }

	@Override
	public String getDescription() { return "Removes multiple spaces where no alignment intention can be identified."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule deliberately skips commands and expressions which are covered by more dedicated rules on alignment and spaces."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 3, 10); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_METHODS_DECLARATION,
																					RuleID.ALIGN_METHODS_FOR_TESTING,
																					RuleID.ALIGN_METHODS_REDEFINITION,
																					RuleID.ALIGN_ALIASES_FOR,
																					RuleID.ALIGN_DECLARATIONS,
																					RuleID.ALIGN_ASSIGNMENTS,
																					RuleID.ALIGN_WITH_SECOND_WORD,
																					RuleID.ALIGN_PARAMETERS, 
																					RuleID.ALIGN_LOGICAL_EXPRESSIONS,
																					RuleID.ALIGN_COND_EXPRESSIONS } ; }
	
	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD   remove_needless_spaces." 
			+ LINE_SEP + "    CLEAR:    ev_value," 
			+ LINE_SEP + "              ev_other_value." 
			+ LINE_SEP + "    CLEAR     ev_third_value." 
			+ LINE_SEP 
			+ LINE_SEP + "    SORT lt_table   BY  first_comp    ASCENDING" 
			+ LINE_SEP + "                        second_comp   DESCENDING" 
			+ LINE_SEP + "                        third_comp." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" these assignments contain needless spaces, but also intentional alignment" 
			+ LINE_SEP + "    a           =   1.     \" comment A" 
			+ LINE_SEP + "    bbb         =   3.     \" comment B" 
			+ LINE_SEP 
			+ LINE_SEP + "    ccccc       =   5.     \" comment C" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" the next two assignments may also be intentionally aligned with previous ones," 
			+ LINE_SEP + "    \" however, with a comment in between, they may as well be aligned independently"
			+ LINE_SEP + "    ddddddd     =   7.      \" comment D" 
			+ LINE_SEP + "    eeeeeeeee   =   9.      \" comment E" 
			+ LINE_SEP 
			+ LINE_SEP + "    \" existing right-alignment of assignment operators is kept:" 
			+ LINE_SEP + "    lv_instance     ?=   get_utility( )." 
			+ LINE_SEP + "    lv_any_value     =   'abc'   &&  'def'." 
			+ LINE_SEP + "    lv_other_value   =   get_value( )  +  get_another_value( )." 
			+ LINE_SEP + "    lv_third_value  +=   3    *   4." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" for numbers, existing alignment by the decimal separator is preserved:" 
			+ LINE_SEP + "    lts_table[ 1 ]-value    =      42." 
			+ LINE_SEP + "    lts_table[ 2 ]-value    =    -100." 
			+ LINE_SEP + "    lts_table[ 3 ]-value    =      '3.1415'." 
			+ LINE_SEP + "    lts_table[ 4 ]-value    =    '-12.34'." 
			+ LINE_SEP 
			+ LINE_SEP + "    ev_result = cl_any_factory=>get(     )->get_utility(   )->get_value(      )." 
			+ LINE_SEP 
			+ LINE_SEP + "    get_util(    )->any_method( iv_any_param   = get_default_value(    )"
			+ LINE_SEP + "                                iv_other_param = VALUE #(       ) )."
			+ LINE_SEP 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigBoolValue configSearchAcrossEmptyLines = new ConfigBoolValue(this, "SearchAcrossEmptyLines", "Search for intended alignment across empty lines", true);
	final ConfigBoolValue configSearchAcrossCommentLines = new ConfigBoolValue(this, "SearchAcrossCommentLines", "Search for intended alignment across comment lines", true);
	final ConfigBoolValue configProcessLineEndComments = new ConfigBoolValue(this, "ProcessLineEndComments", "Process line-end comments, too", false);
	// option was moved from SpaceAroundTextLiteralRule (formerly SpacesInEmptyBracketsRule), option "RemoveMultiSpaceIfEmpty"
	public final ConfigBoolValue configProcessEmptyBrackets = new ConfigBoolValue(this, "ProcessEmptyBrackets", "Remove multiple spaces from empty parentheses", true, false, LocalDate.of(2023, 10, 31));

	private final ConfigValue[] configValues = new ConfigBoolValue[] { configSearchAcrossEmptyLines, configSearchAcrossCommentLines, configProcessLineEndComments, configProcessEmptyBrackets };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public NeedlessSpacesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		boolean alignAcrossEmptyLines = configSearchAcrossEmptyLines.getValue();
		boolean alignAcrossCommentLines = configSearchAcrossCommentLines.getValue();
		
		Command startCommand = code.firstCommand;
		while (startCommand != null) {
			// determine a section [command; endCommand) to process
			boolean containsNonBlockedCommand = false;
			boolean skipCommand = false;
			Command command = startCommand;
			do {
				// break if the whole Command shall be skipped
				if (skipWholeCommand(command)) {
					skipCommand = true;
					break;
				}
				
				// determine whether at least one Command in the section is not blocked
				if (!isCommandBlocked(command))
					containsNonBlockedCommand = true;
				
				Command prevCommand = command;
				command = command.getNext();

				// a sections breaks...
				// a) at the end of the Code
				if (command == null)
					break;
				
				// b) if indent changes
				if (command.getIndent() != prevCommand.getIndent()) { 
					// exception: do not break inside of IF ... ENDIF and CASE ... ENDCASE blocks, because there may be 
					// intentional alignment across ELSEIF / ELSE / WHEN lines (cp. .tokenBreaksSequence())
					if (command.firstCodeTokenIsAnyKeyword("ELSEIF", "ELSE") && !prevCommand.firstCodeTokenIsKeyword("IF")) {
						// continue section
					} else if (prevCommand.firstCodeTokenIsAnyKeyword("ELSEIF", "ELSE") && !command.firstCodeTokenIsKeyword("ENDIF")) {
						// continue section
					} else if (command.firstCodeTokenIsKeyword("WHEN") && !prevCommand.firstCodeTokenIsKeyword("CASE")) {
						// continue section
					} else if (prevCommand.firstCodeTokenIsKeyword("WHEN") && !command.firstCodeTokenIsKeyword("ENDCASE")) {
						// continue section
					} else { 
						break;
					}
				}
				
				// c) with empty lines or comment lines (depending on configuration)
				if (!alignAcrossEmptyLines && command.getFirstTokenLineBreaks() > 1)
					break;
				if (!alignAcrossCommentLines && command.isCommentLine())
					break;
				
				// d) at non-ABAP commands
				if (!command.isAbap())
					break;
			} while (true);
			
			if (containsNonBlockedCommand) {
				if (configProcessEmptyBrackets.getValue()) {
					removeSpacesFromEmptyBrackets(startCommand, command);
				}
				HashMap<Integer, Token> firstTokenOfLine = new HashMap<>();
				HashMap<Integer, ArrayList<TokenPos>> tokensOfXPos = groupTokensByXPos(startCommand, command, firstTokenOfLine);
				executeOn(code, tokensOfXPos, firstTokenOfLine);
			}
			
			startCommand = skipCommand ? command.getNext() : command;
		}
	}

	private boolean skipWholeCommand(Command command) {
		// skip non-ABAP
		if (!command.isAbap())
			return true;
		
		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null)
			return false;

		// skip SELECT
		if (firstCode.isAnyKeyword("SELECT", "UPDATE"))
			return true;

		// skip scope of AlignDeclarationsRule
		if (command.isDeclaration() || firstCode.matchesOnSiblings(true, "INCLUDE", "TYPE"))
			return true;

		// skip scope of AlignMethodsDeclarationRule, AlignMethodsForTestingRule and AlignMethodsRedefinitionRule 
		if (firstCode.isAnyKeyword("METHODS", "CLASS-METHODS", "SELECT"))
			return true;

		// skip scope of AlignParametersRule (for its scope within parts of a Command, see .getSkipUntilToken() below)
		if (firstCode.matchesOnSiblings(true, "CALL", "METHOD|FUNCTION|BADI") 
		 || firstCode.matchesOnSiblings(true, "CREATE", "OBJECT")
		 || firstCode.matchesOnSiblings(true, "RAISE", TokenSearch.makeOptional("RESUMABLE"), "EXCEPTION", "TYPE", TokenSearch.ANY_IDENTIFIER, "EXPORTING")
		 || firstCode.matchesOnSiblings(true, "RAISE", "SHORTDUMP", "TYPE", TokenSearch.ANY_IDENTIFIER, "EXPORTING")
		 || firstCode.matchesOnSiblings(true, "READ|DELETE", "TABLE")) {
			return true;
		}
		
		// for logical expressions, see .getSkipUntilToken() below; this is also used for logical expressions after
		//	"IF", "ELSEIF", "CHECK", "WHILE", because there may still be a line-end comment to process  
		
		return false;
	}

	private void removeSpacesFromEmptyBrackets(Command startCommand, Command endCommand) {
		Command command = startCommand;

		while (command != endCommand) {
			if (isCommandBlocked(command)) {
				command = command.getNext();
				continue;
			}

			Token token = command.getFirstToken();
			while (token != null) {
				Token next = token.getNext();
				if (token.getOpensLevel() && !token.isLiteral() && next != null
						&& next == token.getNextSibling() && next.lineBreaks == 0 && next.spacesLeft > 1) {
					
					int startIndex = next.getStartIndexInLine();
					int addIndent = 1 - next.spacesLeft;
					next.spacesLeft = 1;
					command.addIndent(addIndent, startIndex, next, null, true);
					command.getParentCode().addRuleUse(this, command);
				}
				token = token.getNext();
			}
			command = command.getNext();
		}		
	}
	
	private HashMap<Integer, ArrayList<TokenPos>> groupTokensByXPos(Command startCommand, Command endCommand, HashMap<Integer, Token> firstTokenOfLine) {
		HashMap<Integer, ArrayList<TokenPos>> tokensOfXPos = new HashMap<>();

		// create a HashMap of Tokens by their horizontal position
		Command command = startCommand;
		int lineNumber = 0;
		int commandNumber = 0;
		while (command != endCommand) {
			// if comment lines are part of the range (i.e. the Rule shall work across them), 
			// ignore them and do NOT increase line/command counters 
			if (command.isCommentLine()) {
				command = command.getNext();
				continue;
			}
			
			// add Tokens of the current Command to the HashMap, however, skip sections that are better aligned with 
			// specialized rules, esp. the AlignParametersRule
			Token token = command.getFirstToken();
			Token skipUntilToken = null;
			int xPos = 0;
			while (token != null) {
				// always (even in to-be-skipped sections), update position information
				if (token.lineBreaks > 0) {
					xPos = token.spacesLeft;
					// only increase by 1 (not by .lineBreaks)
					lineNumber += 1;
					firstTokenOfLine.put(lineNumber, token);
				} else {
					xPos += token.spacesLeft;
				}
				
				// has the end of a to-be-skipped section been reached?
				if (skipUntilToken != null) {
					if (token == skipUntilToken) {
						// calculate the next skip, if any - e.g. in chained calls, the Token with the closing parenthesis
						// directly opens the next parenthesis: cl_any_factory=>get( )->any_method( ... ).
						skipUntilToken = getSkipUntilToken(token);
					}
					xPos += token.getTextLength();
					token = token.getNext();
					continue;
				} 

				// if this Token starts a section that shall be skipped, determine the end of that section;
				// (otherwise, these two methods simply return null, and nothing must be skipped)
				// skipping only starts with the next Token 
				skipUntilToken = getSkipUntilToken(token);
				
				// enter this Token to the list of its X position * 2 (for left-alignment)
				TokenPos tokenPos = new TokenPos(token, xPos, lineNumber, commandNumber);
				addTokenPos(tokenPos, 2 * xPos, tokensOfXPos);
				
				// if the Token may be right-aligned or aligned at the decimal separator, also add it to the list of 
				// 'alignment position * 2 + 1', thus ensuring that special alignment will be processed first below  
				if (token.isAssignmentOperator() || token.isComparisonOperator()) {
					// enter right-alignment position as a special position
					int specialXPos = xPos + token.getTextLength() - 1;
					addTokenPos(tokenPos, 2 * specialXPos + 1, tokensOfXPos);
				} else if (token.isIntegerLiteral() || token.isFloatLiteral() ) {
					// enter numeric alignment position as a special position
					int specialXPos = xPos + ABAP.getNumericAlignmentIndex(token.getText());
					addTokenPos(tokenPos, 2 * specialXPos + 1, tokensOfXPos);
				}
				
				xPos += token.getTextLength();
				token = token.getNext();
			}
			command = command.getNext();
			++commandNumber;
		}
		
		return tokensOfXPos;
	}

	private Token getSkipUntilToken(Token token) {
		Token endToken;
		
		// skip scope of AlignParametersRule
		endToken = token.getEndOfParamsOrComponentsList();
		if (endToken != null)
			return endToken;
		
		// skip scope of AlignLogicalExpressionsRule
		endToken = token.getLastTokenOfLogicalExpression();
		if (endToken != null)
			return endToken.getNextCodeToken();

		// skip scope of AlignCondExpressionsRule
		Token prev = token.getPrevCodeSibling();
		if (token.getOpensLevel() && prev != null && prev.isAnyKeyword("COND", "SWITCH")) 
			return token.getNextSibling();

		// skip empty parentheses and brackets, because they are handled in a dedicated section 
		// (since 'skip scope of AlignParametersRule' above will skip inner cases)
		if (token.getOpensLevel() && token.getNext() == token.getNextSibling()) 
			return token.getNext();

		return null;
	}

	private void addTokenPos(TokenPos tokenPos, int xPos, HashMap<Integer, ArrayList<TokenPos>> tokensOfXPos) {
		if (tokensOfXPos.containsKey(xPos)) {
			tokensOfXPos.get(xPos).add(tokenPos);
		} else {
			ArrayList<TokenPos> tokenPosList = new ArrayList<TokenPos>();
			tokenPosList.add(tokenPos);
			tokensOfXPos.put(xPos, tokenPosList);
		}
	}

	private void executeOn(Code code, HashMap<Integer, ArrayList<TokenPos>> tokensOfXPos, HashMap<Integer, Token> firstTokenOfLine) {
		if (tokensOfXPos.isEmpty()) 
			return;

		int[] xPositions = getSortedXPositions(tokensOfXPos);
		
		// store the Tokens that were already processed with special alignment (right-aligned or aligned at decimal separator)
		HashSet<Token> processedTokens = new HashSet<>();
		
		// iterate over the X positions that were found (from right to left)
		for (int xPosIndex = xPositions.length - 1; xPosIndex >= 0; --xPosIndex) {
			int xPos = xPositions[xPosIndex];
			ArrayList<TokenPos> tokenPosList = tokensOfXPos.get(xPos);
			boolean isSpecialAlignment = (xPos % 2 != 0);
			int realXPos = xPos / 2;
			
			// determine sequences of matching Tokens with the same X position 
			int startIndex = 0;
			do {
				TokenPos firstTokenPos = tokenPosList.get(startIndex);
				
				// find endIndex of sequence
				TokenPos prevTokenPos = firstTokenPos; 
				int endIndex = startIndex;
				boolean skipSequence = false;
				do {
					// a sequence that contains an already-processed (e.g. right-aligned) Token will be skipped below
					if (processedTokens.contains(prevTokenPos.token))
						skipSequence = true;
					
					++endIndex;
					if (endIndex == tokenPosList.size())
						break;
					TokenPos testTokenPos = tokenPosList.get(endIndex);
					if (tokenBreaksSequence(firstTokenPos, prevTokenPos, testTokenPos, realXPos, firstTokenOfLine)) {
						break;
					}
					prevTokenPos = testTokenPos;
				} while(true);
				
				// unless this sequence shall be skipped (if one or several Tokens were already processed earlier with 
				// special alignment), process this sequence, condensing space if possible 
				if (!skipSequence) {
					processSequence(code, tokenPosList, startIndex, endIndex, isSpecialAlignment, processedTokens);
				}
				
				startIndex = endIndex;
			} while(startIndex < tokenPosList.size());
			
		}
	}

	private int[] getSortedXPositions(HashMap<Integer, ArrayList<TokenPos>> tokensOfXPos) {
		int[] xPositions = new int[tokensOfXPos.size()];
		int xPosIndex = 0;
		for (Integer xPos : tokensOfXPos.keySet()) {
			xPositions[xPosIndex++] = xPos;
		}
		Arrays.sort(xPositions);
		return xPositions;
	}

	private boolean tokenBreaksSequence(TokenPos firstTokenPos, TokenPos prevTokenPos, TokenPos testTokenPos, int xPos, HashMap<Integer, Token> firstTokenOfLine) {
		Token firstToken = firstTokenPos.token;
		TokenType firstType = firstToken.type;

		// across different Commands, only allow skipping of one or multiple lines if
		// - we are aligning comments 
		// - or (with non-comments) further left of the to-be-aligned Tokens, there is no text in the skipped lines
		//   - except when the skipped lines are 'WHEN' commands 
		if (testTokenPos.token.isCommentAfterCode()) {
			// do NOT consider text in skipped lines  
		} else if (testTokenPos.commandNumber != prevTokenPos.commandNumber && testTokenPos.lineNumber > prevTokenPos.lineNumber + 1) {
			boolean skippedLinesBreakConnection = false;
			for (int testLine = prevTokenPos.lineNumber + 1; testLine < testTokenPos.lineNumber; ++testLine) {
				Token firstTokenOfSkippedLine = firstTokenOfLine.get(testLine); 
				if (firstTokenOfSkippedLine.spacesLeft < xPos && !firstTokenOfSkippedLine.getParentCommand().firstCodeTokenIsAnyKeyword("ELSEIF", "ELSE", "WHEN")) {
					skippedLinesBreakConnection = true;
					break;
				}
			}
			if (skippedLinesBreakConnection)
				return true;
		}
		
		// check match by Token type
		Token testToken = testTokenPos.token;
		TokenType testType = testToken.type;
		if (firstToken.getPrev() != null && firstToken.getPrev().isAssignmentOperator() 
		  && testToken.getPrev() != null && testToken.getPrev().isAssignmentOperator()) {
			// if both Tokens are preceded by an assignment operator, this is always considered a match
			
		} else if (!tokenTypesMatch(firstType, testType)) {
			return true;
		}
		
		return false;
	}
	
	private boolean tokenTypesMatch(TokenType type1, TokenType type2) {
		// the Tokens : , . do not match anything, i.e. each of them shall be processed on its own
		final TokenType[] tokenTypesWithoutMatch = new TokenType[] { TokenType.COLON, TokenType.COMMA, TokenType.PERIOD };
		for (TokenType tokenType : tokenTypesWithoutMatch) {
			if (type1 == tokenType || type2 == tokenType) 
				return false;
		}
		
		// assignment operators only match assignment operators; comparison operators only match comparison operators; 
		// comments only match comments; pragmas only match pragmas
		final TokenType[] tokenTypesRequiringMatch = new TokenType[] { TokenType.ASSIGNMENT_OP, TokenType.COMPARISON_OP, TokenType.COMMENT, TokenType.PRAGMA };
		for (TokenType tokenType : tokenTypesRequiringMatch) {
			if (type1 == tokenType || type2 == tokenType)
				return (type1 == type2);
		}
		
		// any combination of the remaining types (LITERAL, KEYWORD, IDENTIFIER, OTHER_OP) is considered a match
		return true;
	}

	private boolean processSequence(Code code, ArrayList<TokenPos> tokenPosList, int startIndex, int endIndex, boolean isSpecialAlignment, HashSet<Token> processedTokens) {
		// for the current sequence [startIndex; endIndex), determine 
		// - the number of spaces to spare
		// - whether all its Tokens are first Tokens in their line
		// - whether all its Tokens have the same length
		int spacesToSpare = Integer.MAX_VALUE;
		boolean hasFirstTokensInLineOnly = true;
		boolean allTokensHaveSameStartIndex = true;

		// in case of special alignment, i.e. right-aligned assignment operators or decimal-separator-aligned numbers:
		int maxPrevEndIndex = 0;
		if (isSpecialAlignment) {
			TokenPos firstTokenPos = tokenPosList.get(startIndex);
			for (int index = startIndex; index < endIndex; ++index) {
				TokenPos testTokenPos = tokenPosList.get(index);
				Token testToken = testTokenPos.token;

				// determine the maximum end index of the previous Token in the same line for this sequence; otherwise,  
				// we could get an overlapping of left-aligned and right-aligned Tokens as in:
				//   lv_long_var_name = 'abc'.
				//   lv_short_name  &&= 'def'.
				if (!testToken.isFirstTokenInLine()) {
					int prevEndIndex = testToken.getPrev().getEndIndexInLine();
					maxPrevEndIndex = Math.max(maxPrevEndIndex, prevEndIndex);
				}
				
				// find out whether all the special-aligned Tokens happen to be left-aligned, too
				if (testTokenPos.startIndexInLine != firstTokenPos.startIndexInLine)
					allTokensHaveSameStartIndex = false;
			}	

			// do NOT align the Tokens now if they are all left-aligned (this includes the case that this sequence only 
			// contains one single Token): in such cases, they will be processed again later at a lower X position for  
			// left-alignment, and this may then include further left-aligned Tokens
			if (allTokensHaveSameStartIndex)
				return false;

			// now that it is sure that there are several special-aligned Tokens, ensure that will NOT be moved later when 
			// their (left) X position is processed for left-alignment; this must be prevented even if spacesToSpare == 0, 
			// otherwise one of the Tokens could be moved on its own later
			for (int index = startIndex; index < endIndex; ++index) 
				processedTokens.add(tokenPosList.get(index).token);
		}

		Command lastCommand = null;
		for (int index = startIndex; index < endIndex; ++index) {
			TokenPos testTokenPos = tokenPosList.get(index);
			Token testToken = testTokenPos.token;
			// leave the job of removing the space before commas and periods to the SpaceBeforePeriodRule -
			// otherwise, we could use: '... : testToken.isCommaOrPeriod() ? testToken.spacesLeft : testToken.spacesLeft - 1;'
			int testSpacesToSpare = isSpecialAlignment ? testToken.getStartIndexInLine() - maxPrevEndIndex - 1 
																	 : testToken.spacesLeft - 1;
			spacesToSpare = Math.min(spacesToSpare, testSpacesToSpare);

			if (!testToken.isFirstTokenInLine())
				hasFirstTokensInLineOnly = false;

			Command testCommand = testToken.getParentCommand();
			if (testCommand != lastCommand && testCommand.isBlocked(getID(), abapOnly)) {
				// do not process a sequence that contains a blocked Command in order to avoid partial alignment 
				return false;
			}
			
			testCommand = lastCommand;
		}

		if (hasFirstTokensInLineOnly || spacesToSpare <= 0) 
			return false;
		
		// remove spacesToSpare spaces for all Tokens in the sequence  
		boolean processLineEndComments = configProcessLineEndComments.getValue();
		for (int index = startIndex; index < endIndex; ++index) {
			Token token =  tokenPosList.get(index).token;
			if (!processLineEndComments && token.isCommentAfterCode())
				continue;
			Command changeCommand = token.getParentCommand();
			commandForErrorMsg = changeCommand;
			token.setSpacesLeftAdjustingIndent(token.spacesLeft - spacesToSpare, false);
			code.addRuleUse(this, changeCommand);
		}
		return true;
	}
}
