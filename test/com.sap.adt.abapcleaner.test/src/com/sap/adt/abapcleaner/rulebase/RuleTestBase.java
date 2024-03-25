package com.sap.adt.abapcleaner.rulebase;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.comparer.ChangeStats;
import com.sap.adt.abapcleaner.comparer.ChangeTypes;
import com.sap.adt.abapcleaner.comparer.CompareDoc;
import com.sap.adt.abapcleaner.comparer.DisplayLine;
import com.sap.adt.abapcleaner.comparer.DisplaySide;
import com.sap.adt.abapcleaner.comparer.HighlightBit;
import com.sap.adt.abapcleaner.comparer.DiffDoc;
import com.sap.adt.abapcleaner.comparer.DiffLine;
import com.sap.adt.abapcleaner.comparer.DiffNavigator;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.MemoryAccessType;
import com.sap.adt.abapcleaner.parser.ParseParams;
import com.sap.adt.abapcleaner.parser.StressTestParams;
import com.sap.adt.abapcleaner.parser.StressTestType;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.CompareException;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.Task;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;

public abstract class RuleTestBase {
	/** -1 to deactivate stress test; 7 for medium (+ 50% duration), 31 for thorough (+ 100% duration) stress test */
	private static final int STRESS_TEST_TOKEN_INDEX_MAX = 31;  
	private static final String LINE_SEP = ABAP.LINE_SEPARATOR;

	protected static Profile profile = Profile.createDefault();
	protected RuleID ruleID;

	/** the ABAP release against which this code must compile (e.g. "757" for release 7.57; null if unknown), 
	 * see <a href="https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abennews.htm">ABAP - Release News</a> */
	protected String abapReleaseOfCode;
	
	/** the release restriction which the user selects on the UI (e.g. 757 for release 7.57), and which may be more   
	 * restrictive than the ABAP release against which the code is compiled (e.g. to ensure downports) */
	protected int releaseRestrictionFromUI;

	protected StringBuilder sourceCodeBuilder = new StringBuilder();
	protected StringBuilder expCodeBuilder = new StringBuilder();

	private boolean checkSyntaxAfterParse = true;
	private boolean checkRuleWasUsed = true;
	private HashSet<Integer> commandIndicesToBlock = new HashSet<>();
	
	protected RuleTestBase(RuleID ruleID) {
		this.ruleID = ruleID;
		this.abapReleaseOfCode = ABAP.NEWEST_RELEASE;
		this.releaseRestrictionFromUI = ABAP.NO_RELEASE_RESTRICTION; 
		Program.setLogForTesting();
	}
	
	protected Rule getRule() {
		return profile.getRule(ruleID);
	}
	
	@BeforeEach
	void basicSetUp() {
		getRule().setDefault();
		abapReleaseOfCode = ABAP.NEWEST_RELEASE; // unless deliberately changed with setAbapRelease()
		releaseRestrictionFromUI = ABAP.NO_RELEASE_RESTRICTION; // unless deliberately changed with setReleaseRestriction()
	}

	// =========================================================================
	// tests for each rule (executed once for each concrete child class of RuleTestBase)
	
	@Test
	void testExampleCode() { 
		String sourceCode = getRule().getExample();
		assertFalse(StringUtil.isNullOrEmpty(sourceCode));
		
		// ensure that the example can be parsed
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, abapReleaseOfCode));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}

		// test the referential integrity of the parse result of the example
		try {
			code.testReferentialIntegrity(true);
			switch (ruleID) {
				// some rules deliberately contain examples that can be compiled and executed, but some Tokens are marked  
				// as syntax errors (red text color in ADT) by the RND Parser (see comments in getExample() methods) 
				case EMPTY_COMMAND:
				case SPACES_IN_EMPTY_BRACKETS:
					break;
				default:
					code.checkSyntax(false);
					break;
			}
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code: " + e1.getMessage());
			return;
		} 

	}

	// =========================================================================
	
	protected void setAbapReleaseOfCode(String abapReleaseOfCode) {
		this.abapReleaseOfCode = abapReleaseOfCode;
	}

	protected void setReleaseRestrictionFromUI(int releaseRestrictionFromUI) {
		this.releaseRestrictionFromUI = releaseRestrictionFromUI;
	}
	
	protected void buildSrc(String line) {
		buildCode(sourceCodeBuilder, line);
	}

	protected void buildExp(String line) {
		buildCode(expCodeBuilder, line);
	}

	private void buildCode(StringBuilder sb, String line) {
		if (sb.length() > 0)
			sb.append(LINE_SEP);
		sb.append(StringUtil.trimEnd(line));
	}

	protected void copyExpFromSrc() {
		expCodeBuilder = new StringBuilder();
		expCodeBuilder.append(sourceCodeBuilder);
	}
	
	protected void putAnyMethodAroundSrcAndExp() {
		putAroundSrcAndExp("  METHOD any_method." + LINE_SEP, LINE_SEP + "  ENDMETHOD.");
	}
	
	protected void putAnyClassDefAroundSrcAndExp() {
		putAroundSrcAndExp("CLASS any_class DEFINITION." + LINE_SEP + "  PUBLIC SECTION." + LINE_SEP, LINE_SEP + "ENDCLASS.");
	}

	protected void putAnyInterfaceDefAroundSrcAndExp() {
		putAroundSrcAndExp("INTERFACE if_any_interface PUBLIC." + LINE_SEP, LINE_SEP + "ENDINTERFACE.");
	}

	protected void putAnyFormAroundSrcAndExp() {
		putAroundSrcAndExp("FORM any_form." + LINE_SEP, LINE_SEP + "ENDFORM.");
	}
	
	private void putAroundSrcAndExp(String start, String end) {
		sourceCodeBuilder.insert(0, start);
		sourceCodeBuilder.append(end);

		expCodeBuilder.insert(0, start);
		expCodeBuilder.append(end);
	}

	protected void deactivateSyntaxCheckAfterParse() {
		checkSyntaxAfterParse = false;
	}
	
	protected void deactivateRuleUseCheck() {
		checkRuleWasUsed = false;
	}
	
	protected void blockCommand(int commandIndex) {
		commandIndicesToBlock.add(commandIndex);
	}
	
	@Test
	void testRuleInfo() {
		Rule rule = getRule();
		assertEquals(rule.getID(), ruleID);

		// make sure that all required info fields are maintained (i.e. non-empty)
		assertTrue(rule.getDisplayName().length() > 0);
		assertTrue(rule.getDescription().length() > 0);
		assertTrue(rule.getDescription().indexOf(System.lineSeparator()) < 0);
		assertNotNull(rule.getHintsAndRestrictions());
		assertTrue(rule.getReferences().length > 0);
		assertTrue(rule.getExample().length() > 0);
		assertEquals(rule.getDisplayName(), rule.toString());
		assertNotNull(rule.getNameInSettings());
		assertNotNull(rule.getSupportedLanguages());
		assertTrue(rule.getSupportedLanguages().length > 0);
		
		for (RuleReference reference : rule.getReferences()) {
			assertTrue(!reference.hasLink() || reference.getLink().length() > 0);
			assertTrue(reference.getSourceText().length() > 0);
			assertTrue(reference.toString().length() > 0);
		}
	}
	
	protected void testRule() {
		// parse the source code
		String sourceCode = sourceCodeBuilder.toString();
		Code code;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCode, abapReleaseOfCode));
		} catch (ParseException e) {
			fail(e.getMessage());
			return;
		}

		// test the referential integrity of the parse result
		try {
			code.testReferentialIntegrity(true);
			if (checkSyntaxAfterParse) {
				code.checkSyntax(false);
			}
		} catch (IntegrityBrokenException e1) {
			fail("Error after parsing source code: " + e1.getMessage());
			return;
		} 

		// assert that rebuilding the code from the objects (Code, Command, Token etc.) results in the same source text
		String parseCheckErrors = code.compareWithSource(sourceCode, 10); // null if rebuilt code matches source text
		if (parseCheckErrors != null) 
			fail(parseCheckErrors);
		
		// retrieve oldCodeDisplayLines before the Rule under test is executed
		ArrayList<DisplayLine> oldCodeDisplayLines = code.toDisplayLines();

		// remember all non-comment Tokens and their texts and trigger calculation of MemoryAccessType
		HashMap<Token, String> textOfToken = new HashMap<>();
		Command command = code.firstCommand;
		while (command != null) {
			Token token = command.getFirstToken();
			while (token != null) {
				textOfToken.put(token, token.getText());
				token.getMemoryAccessType();
				token = token.getNext();
			}
			command = command.getNext();
		}

		// apply prepared blocking to specific commands
		blockCommands(code);
		
		// call the Rule under test and get the actual result code
		try {
			getRule().executeIfAllowedOn(code, releaseRestrictionFromUI);
		} catch (UnexpectedSyntaxBeforeChanges | UnexpectedSyntaxAfterChanges e) {
			fail(e.getMessage());
			return;
		}
		String act = code.toString();

		// test the referential integrity of the resulting objects (Code, Command, Token etc.)
		try {
			code.testReferentialIntegrity(true);
			code.checkSyntax(true);
		} catch (IntegrityBrokenException e1) {
			fail("Error after executing rule '" + getRule().getDisplayName() + "': " + e1.getMessage());
		} 

		// ensure that buffered MemoryAccessType values are still valid (i.e. they match the freshly calculated values)
		command = code.firstCommand;
		while (command != null) {
			Token token = command.getFirstToken();
			while (token != null) {
				MemoryAccessType actBufferedAccessType = token.getMemoryAccessType();
				if (actBufferedAccessType != MemoryAccessType.NONE) {
					token.invalidateMemoryAccessType();
					MemoryAccessType expAccessType = token.getMemoryAccessType(); 
					if (expAccessType == MemoryAccessType.WRITE || expAccessType == MemoryAccessType.READ_WRITE || expAccessType == MemoryAccessType.READ_WRITE_POSSIBLE) {
						// expected WRITE or READ_WRITE or READ_WRITE_POSSIBLE requires an exact match
						assertEquals(expAccessType, actBufferedAccessType);
					} else {
						// otherwise, any combination of .NONE, .READ or .READ_OR_NONE is okay 
						assertTrue(actBufferedAccessType != MemoryAccessType.WRITE && actBufferedAccessType != MemoryAccessType.READ_WRITE && actBufferedAccessType != MemoryAccessType.READ_WRITE_POSSIBLE);
					}
				}
				token = token.getNext();
			}
			command = command.getNext();
		}
		
		// determine whether tokens were added or removed, or non-comments were changed or added
		boolean wasAnyTokenAdded = false;
		boolean wasAnyTokenRemoved = false;
		boolean wasAnyNonCommentLetterAdded = false;
		boolean wasAnyNonCommentTextChanged = false;
		String firstText = null;
		int keptTokenCount = 0;
		command = code.firstCommand;
		while (command != null) {
			Token token = command.getFirstToken();
			while (token != null) {
				if (textOfToken.containsKey(token)) {
					++keptTokenCount;
					if (!wasAnyNonCommentTextChanged && !token.isComment()) {
						String oldText = textOfToken.get(token); 
						if (!token.getText().equals(oldText)) {
							wasAnyNonCommentTextChanged = true;
						}
					}
				} else {
					wasAnyTokenAdded = true;
					if (!wasAnyNonCommentLetterAdded && !token.isComment() && token.textContainsAnyLetter()) {
						wasAnyNonCommentLetterAdded = true;
						firstText = token.getText();
					}
				}
				token = token.getNext();
			}
			command = command.getNext();
		}
		wasAnyTokenRemoved = (keptTokenCount != textOfToken.size());
		
		// if any non-comment Token was added with a text that contains any letter (a-z etc.), 
		// ensure that this Rule has an explicit dependency on the UpperAndLowerCaseRule 
		// (because that would need to ensure that added keywords etc. have the right casing)
		if (wasAnyNonCommentLetterAdded && !profile.getRule(ruleID).dependsOn(RuleID.UPPER_AND_LOWER_CASE)) {
			fail("rule " + ruleID.name() + " must configure a dependency on rule " + RuleID.UPPER_AND_LOWER_CASE.name() + ", as it introduced Token '" + firstText + "'");
		}
		
		// ensure that a rule that belongs to the rule group ALIGNMENT does NOT add or remove any Token, or change any non-comment  
		// text (however, comments may change, e.g. ABAP Doc with '@parameter ... | ', or commented out assignments)
		if (profile.getRule(ruleID).getGroupID() == RuleGroupID.ALIGNMENT) {
			if (wasAnyTokenAdded || wasAnyTokenRemoved || wasAnyNonCommentTextChanged) {
				fail("rule " + ruleID.name() + " belongs to RuleGroupID." + RuleGroupID.ALIGNMENT.name() + " but added or removed a Token.");
			}
		}
		
		// find first difference (if any) between actual and expected result code and create a message that highlights the difference
		String msg = "";
		String exp = expCodeBuilder.toString();
		if (!exp.equals(act)) {
			int minLen = Math.min(act.length(), exp.length());
			int line = 0;
			int lineOffset = 0;
			for (int i = 0; i < minLen; ++i) {
				if (act.charAt(i) != exp.charAt(i)) {
					msg = "Cleaned code differs in line " + String.valueOf(line) + " at char " + String.valueOf(i - lineOffset) + ": ==> expected:";
					msg += LINE_SEP + exp.substring(lineOffset, i) + " [#>] " + getTextUntilLineEnd(exp, i) + " -- but got:";
					msg += LINE_SEP + act.substring(lineOffset, i) + " [#>] " + getTextUntilLineEnd(act, i) + " -- Complete code: ";
					break;
				}
				if (act.charAt(i) == '\n') {
					++line;
					lineOffset = i + 1;
				}
			}
		}

		// let the test framework do the comparison again 
		assertEquals(LINE_SEP + LINE_SEP + exp, LINE_SEP + LINE_SEP + act, msg); // use of LINE_SEP increases readability in case of errors

		// compare changed code with source code
		ArrayList<DisplayLine> newCodeDisplayLines = code.toDisplayLines();
		CompareDoc doc1 = CompareDoc.createFromDisplayLines(oldCodeDisplayLines);
		CompareDoc doc2 = CompareDoc.createFromDisplayLines(newCodeDisplayLines);
		DiffDoc diffDoc = null;
		try {
			diffDoc = doc1.compareTo(doc2, null);
		} catch (CompareException ex) {
			fail(ex.getMessage());
			return;
		}

		// check the statistics of used rules
		RuleStats[] ruleStats = diffDoc.getRuleStats(profile);
		if (sourceCode.equals(exp)) {
			// ensure that no rule was used 
			assertEquals(ruleStats.length, 0);
		} else if (checkRuleWasUsed) {
			// ensure that the current rule was marked as being used in at least one of the Code lines
			assertTrue(ruleStats.length > 0);
			boolean foundCurrentRule = false;
			for (RuleStats ruleStat : ruleStats) {
				if (ruleStat.getRuleID() == ruleID) {
					foundCurrentRule = true;
					assertTrue(ruleStat.isUsed());
				}
			}
			assertTrue(foundCurrentRule);
		}

		// check whether DiffNavigator can navigate on the differences
		testDiffNavigator(code, diffDoc);

		// check the Rule can be effectively blocked
		try {
			// parse the source code again and block the rule for all Commands in the Code
			Code codeBlocked = Code.parse(null, ParseParams.createForTest(sourceCode, abapReleaseOfCode));
			codeBlocked.setBlockedRule(ruleID, true);

			// assert that this time, the code remains unchanged
			getRule().prepare(codeBlocked);
			getRule().executeOn(codeBlocked, ABAP.NO_RELEASE_RESTRICTION);
			String actBlocked = codeBlocked.toString();
			assertEquals(sourceCode, actBlocked, "code changed although Rule was blocked!");

		} catch (ParseException | UnexpectedSyntaxBeforeChanges | UnexpectedSyntaxAfterChanges e) {
			fail(e.getMessage());
			return;
		}
		
		// perform stress test by inserting line-end comments, comment lines, pragmas and colons after the first few Tokens
		// and checking referential integrity after calling the rule under test (whether or not it was executed with the additional Tokens)
		StressTestParams stressTestParams = StressTestParams.create(0, STRESS_TEST_TOKEN_INDEX_MAX, StressTestType.getAll()); 		
		for (StressTestType stressTestType : stressTestParams.stressTestTypes) {
			for (int index = stressTestParams.insertAfterTokenIndexMin; index <= stressTestParams.insertAfterTokenIndexMax; ++index) {
				boolean continueStressTestType = runStressTest(stressTestType, index);
				if (!continueStressTestType) {
					break;
				}
			}
		}
	}
	
	private void blockCommands(Code code) {
		if (commandIndicesToBlock.isEmpty())
			return;
		Command command = code.firstCommand;
		int index = 0;
		while (command != null) {
			if (commandIndicesToBlock.contains(index)) {
				command.getChangeControl().setBlockedRule(ruleID, true);
			}
			++index;
			command = command.getNext();
		}
	}

	private boolean runStressTest(StressTestType stressTestType, int insertAfterTokenIndex) {
		// parse the source code
		Code code = null;
		try {
			code = Code.parse(null, ParseParams.createForTest(sourceCodeBuilder.toString(), abapReleaseOfCode));
		} catch (ParseException e) {
			fail(e.getMessage());
		}
		
		// apply prepared blocking to specific commands
		blockCommands(code);

		String stressTestInfo = " [stress test: inserted " + stressTestType.description + " after token #" + String.valueOf(insertAfterTokenIndex) + "]";
		try {
			if (!code.insertStressTestTokentAt(insertAfterTokenIndex, stressTestType)) {
				return false;
			}
		} catch (IntegrityBrokenException ex) {
			fail("Error inserting stress test tokens:" + ex.getMessage() + stressTestInfo );
		}

		// call the Rule under test; the additional stress test Tokens may prevent the rule from being executed, 
		// but in any case, the result must keep referential integrity
		try {
			getRule().executeIfAllowedOn(code, releaseRestrictionFromUI);
		} catch (UnexpectedSyntaxBeforeChanges | UnexpectedSyntaxAfterChanges e) {
			fail(e.getMessage() + stressTestInfo);
		}

		// test the referential integrity of the resulting objects (Code, Command, Token etc.)
		try {
			code.testReferentialIntegrity(true);
			if (stressTestType != StressTestType.COLON) {
				code.checkSyntax(true);
			}
		} catch (IntegrityBrokenException e1) {
			fail("Error after executing rule '" + getRule().getDisplayName() + "': " + e1.getMessage() + stressTestInfo);
		} 
		return true;
	}
	
	private void testDiffNavigator(Code code, DiffDoc diffDoc) {
		DiffNavigator diffNav = DiffNavigator.create();
		diffNav.refreshCode(code, diffDoc, 0, 0, 0);
		
		int lineCount = diffNav.getLineCount();

		assertEquals(0, diffNav.getCurLine());
		assertTrue(diffNav.getLineCount() > 0);
		assertEquals(lineCount == 00, diffNav.isEmpty());
		
		// getLineNumberDigits()
		if (lineCount < 10)
			assertEquals(1, diffNav.getLineNumberDigits());
		else if (lineCount >= 10 && lineCount <= 99)
			assertEquals(2, diffNav.getLineNumberDigits());
		else
			assertTrue(diffNav.getLineNumberDigits() > 3);
		
		// selection functionality
		diffNav.selectAll();
		assertTrue(diffNav.areLinesSelected());
		assertEquals(lineCount - 1, diffNav.getCurLine());
		assertEquals(0, diffNav.getSelStartLine());
		assertEquals(0, diffNav.getSelectionLineMin());
		assertEquals(lineCount - 1, diffNav.getSelectionLineMax());

		// getSelectedText()
		diffNav.selectAll();
		assertEquals(sourceCodeBuilder.toString() + ABAP.LINE_SEPARATOR, diffNav.getSelectedText(DisplaySide.LEFT, ABAP.LINE_SEPARATOR));
		assertEquals(expCodeBuilder.toString() + ABAP.LINE_SEPARATOR, diffNav.getSelectedText(DisplaySide.RIGHT, ABAP.LINE_SEPARATOR));
		
		// getLine()
		assertNull(diffNav.getLine(-1));
		assertNotNull(diffNav.getLine(0));
		assertNotNull(diffNav.getLine(lineCount - 1));
		assertNull(diffNav.getLine(lineCount));

		assertFalse(diffNav.isInSearchMode());
		String codeText = code.toString();
		assertEquals(codeText, diffNav.getCodeToString());
		
		
		diffNav.setCurLine(0,  true);
		assertFalse(diffNav.areLinesSelected());
		
		// simple move functionality
		diffNav.moveToFirstLine(true);
		assertEquals(0, diffNav.getCurLine());
		for (int line = 1; line < lineCount; ++line) {
			diffNav.moveToNextLine(true);
			assertEquals(line, diffNav.getCurLine());
		}
		diffNav.moveToLastLine(true);
		assertEquals(lineCount - 1, diffNav.getCurLine());
		for (int line = lineCount - 2; line >= 0; --line) {
			diffNav.moveToPrevLine(true);
			assertEquals(line, diffNav.getCurLine());
		}
		diffNav.moveToLine(lineCount / 2, true);
		assertEquals(lineCount / 2, diffNav.getCurLine());
		
		assertFalse(diffNav.moveToLineInDoc(-1, DisplaySide.RIGHT));
		assertTrue(diffNav.moveToLineInDoc(1, DisplaySide.LEFT));
		assertTrue(diffNav.moveToLineInDoc(1, DisplaySide.RIGHT));
		
		// page move functionality
		final int step = 3;
		diffNav.moveToFirstLine(true);
		for (int line = step; line < lineCount; line += step) {
			diffNav.moveToNextPage(step, true);
			assertEquals(line, diffNav.getCurLine());
		}
		diffNav.moveToLastLine(true);
		for (int line = lineCount - 1 - step; line >= 0; line -= step) {
			diffNav.moveToPrevPage(step, true);
			assertEquals(line, diffNav.getCurLine());
		}

		// move to change functionality
		diffNav.setHighlight(ChangeTypes.createAllChanges());
		assertTrue(diffNav.showAddedAndDeletedLines());
		assertTrue(diffNav.showAnyChanges());
		diffNav.moveToFirstLine(true);
		int changeCountNext = diffNav.isLineHighlighted(diffNav.getLine(0)) ? 1 : 0;
		while (diffNav.moveToNextChange())
			++changeCountNext;
		diffNav.moveToLastLine(true);
		int changeCountPrev = diffNav.isLineHighlighted(diffNav.getLine(lineCount - 1)) ? 1 : 0;
		while (diffNav.moveToPrevChange())
			++changeCountPrev;
		// counts may differ by a maximum of 1, depending on whether the last and/or first command is highlighted
		assertTrue(Math.abs(changeCountNext - changeCountPrev) <= 1);
		ChangeStats changeStats = diffNav.getChangeStats();
		assertNotNull(changeStats);
		assertEquals((changeCountNext == 0 && changeCountPrev == 0), (changeStats.getTotalCount() == 0));
		
		// move to screen with changes functionality
		diffNav.moveToFirstLine(true);
		int screenCountNext = 0;
		while (diffNav.moveToNextScreenWithChanges(diffNav.getCurLine())) {
			++screenCountNext;
		}
		diffNav.moveToLastLine(true);
		int screenCountPrev = 0;
		while (diffNav.moveToPrevScreenWithChanges(diffNav.getCurLine())) {
			++screenCountPrev;
		}
		assertTrue(screenCountNext <= lineCount);
		assertTrue(screenCountPrev <= lineCount);
		
		// test search functionality by searching any word found somewhere in the middle of the code text
		diffNav.moveToFirstLine(true);
		diffNav.clearSearchPos();
		int startPos = codeText.length() / 2;
		while (startPos < codeText.length() && !Character.isLetterOrDigit(codeText.charAt(startPos)))
			++startPos;
		if (startPos + 1 < codeText.length() && lineCount >= 3) {
			int endPos = startPos + 1;
			while (endPos < codeText.length() && Character.isLetterOrDigit(codeText.charAt(endPos)))
				++endPos;
			if (endPos < codeText.length()) {
				String anyWord = codeText.substring(startPos, endPos);
				
				diffNav.setSearchMode(true);
				assertTrue(diffNav.isInSearchMode());
				char[] anyWordChars = anyWord.toCharArray();
				int length = 0;
				for (char c : anyWordChars) {
					diffNav.addCharToSearchText(c);
					++length;
					assertEquals(anyWord.substring(0, length), diffNav.getSearchText());
					assertTrue(diffNav.search(true,  true,  false,  true,  false,  true, false));
				}
				diffNav.addCharToSearchText('\u00B3');
				assertFalse(diffNav.search(true,  true,  false,  true,  false,  true, false));
				diffNav.removeLastCharFromSearchText();
				
				diffNav.moveToLastLine(true);
				diffNav.clearSearchPos();
				assertTrue(diffNav.search(false,  false,  false,  true,  false,  true, false));
			}
		}
		diffNav.setSearchMode(true); // reset search text to ""
		assertFalse(diffNav.search(true,  true,  true,  true,  false,  true,  false));
		
		// highlight functionality
		for (int mode = 0; mode < 2; ++mode) {
			boolean expHighlight = (mode == 0);
			if (expHighlight)
				diffNav.setHighlight(ChangeTypes.createAllChanges());
			else
				diffNav.setHighlight(ChangeTypes.createNoChanges());
			for (int line = 0; line < lineCount; ++line) {
				DiffLine diffLine = diffNav.getLine(line);
				DisplayLine dispLine = diffLine.rightLine;
				if (dispLine != null && dispLine.getHighlightBits() != null) {
					for (HighlightBit bit : dispLine.getHighlightBits()) {
						assertEquals(expHighlight, diffNav.isLineBitHighlighted(bit));
					}
				}
			}
		}
		
		// reprocess a selection of the code, blocking the rule at the first changed line
		diffNav.moveToFirstLine(true);
		if (!diffNav.moveToNextChange()) {
			// the change (if any) must be in the first line
			diffNav.moveToFirstLine(true);
		}
		diffNav.setBlockRuleInSelection(ruleID, true);
		try {
			Task result = diffNav.reprocessSelection(profile, releaseRestrictionFromUI, code.sourceName);
			if (checkSyntaxAfterParse) {
				assertTrue(result.getSuccess());
			}
		} catch (IntegrityBrokenException e) {
			if (checkSyntaxAfterParse) {
				fail(e.getMessage());
			}
		}
	}

	private String getTextUntilLineEnd(String code, int start) {
		int lineEnd = StringUtil.indexOfAny(code, new char[] { '\r', '\n' }, start);
		return code.substring(start, (lineEnd < 0) ? code.length() : lineEnd);
	}

	protected void testRuleExpectingExc() throws ParseException, UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		Code code = Code.parse(null, ParseParams.createForTest(sourceCodeBuilder.toString(), abapReleaseOfCode));
		getRule().executeIfAllowedOn(code, releaseRestrictionFromUI);
		assertEquals(LINE_SEP + LINE_SEP + expCodeBuilder.toString(), LINE_SEP + LINE_SEP + code.toString()); // use of LINE_SEP increases readability in case of errors
	}
}
