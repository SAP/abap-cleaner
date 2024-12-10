package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rules.alignment.SelectListLayout;

public class DdlAlignFieldListsRule extends RuleForDdlCommands {
	private enum Columns {
		FIELD;
		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 1;

	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_FIELD_LISTS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align name list and GROUP BY list"; }

	@Override
	public String getDescription() { return "Aligns name lists of DDIC-based views and GROUP BY field lists."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 15); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view I_AnyDDicBasedView"
				+ LINE_SEP + " ("
				+ LINE_SEP + "  AnyFieldAlias,"
				+ LINE_SEP + "OtherFieldAlias,"
				+ LINE_SEP + " ThirdFieldAlias, FourthFieldAliasWithLongName, FifthFieldAlias"
				+ LINE_SEP + "    )"
				+ LINE_SEP + ""
				+ LINE_SEP + "  as select from  I_AnySource as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer join I_OtherSource as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.AnyField = OtherAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_ThirdSource as _ThirdAlias"
				+ LINE_SEP + "     on _ThirdAlias.OtherField = AnyAlias.OtherField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "  key AnyAlias.AnyField,"
				+ LINE_SEP + "      OtherAlias.OtherField,"
				+ LINE_SEP + "//      OtherAlias.CommentedOutField,"
				+ LINE_SEP + "      OtherAlias.ThirdField,"
				+ LINE_SEP + "      _ThirdAlias.FourthFieldWithLongName,"
				+ LINE_SEP + "      _ThirdAlias._FourthAlias.FifthField,"
				+ LINE_SEP + "}"
				+ LINE_SEP + "group by"
				+ LINE_SEP + "       AnyAlias.AnyField,"
				+ LINE_SEP + "     OtherAlias.OtherField,"
				+ LINE_SEP + "// OtherAlias.CommentedOutField,"
				+ LINE_SEP + " OtherAlias.ThirdField, _ThirdAlias.FourthFieldWithLongName,"
				+ LINE_SEP + "      _ThirdAlias._FourthAlias.FifthField";
	}
	
   private final String[] layoutTexts = new String[] { "multi-line", "single line", "derive from majority", "keep as is" };

   final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length", "", MIN_LINE_LENGTH_DDL, DEFAULT_LINE_LENGTH_DDL, DDL.MAX_LINE_LENGTH);
	final ConfigEnumValue<DdlNameListPos> configNameListPos = new ConfigEnumValue<DdlNameListPos>(this, "NameListPos", "Name list position:",
			new String[] { "continue after view name", "below line start + 2", "below line start + 4", "below view name + 2", "below view name + 4", "keep as is" }, DdlNameListPos.values(), DdlNameListPos.LINE_START_PLUS_2);
   final ConfigEnumValue<SelectListLayout> configNameListLayout = new ConfigEnumValue<SelectListLayout>(this, "NameListLayout", "Name lists layout:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);

	final ConfigEnumValue<DdlGroupByListPos> configGroupByListPos = new ConfigEnumValue<DdlGroupByListPos>(this, "GroupByListPos", "GROUP BY list position:",
			new String[] { "continue after GROUP BY", "below line start + 2", "below line start + 4", "keep as is" }, DdlGroupByListPos.values(), DdlGroupByListPos.CONTINUE);
	final ConfigEnumValue<SelectListLayout> configComplexGroupByListLayout = new ConfigEnumValue<SelectListLayout>(this, "ComplexGroupByListLayout", "GROUP BY lists with complex fields:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);
	final ConfigEnumValue<SelectListLayout> configSimpleGroupByListLayout = new ConfigEnumValue<SelectListLayout>(this, "SimpleGroupByListLayout", "GROUP BY lists with simple fields only:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);
	final ConfigBoolValue configConsiderDotAsComplex = new ConfigBoolValue(this, "ConsiderDotAsComplex", "Consider 'Alias.Field' as complex", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configNameListPos, configNameListLayout, configGroupByListPos, configComplexGroupByListLayout, configSimpleGroupByListLayout, configConsiderDotAsComplex };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignFieldListsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
      if (command.getParent() != null)
      	return false;

		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null)
			return false;

		// align name list (DDIC-based views only)
		Token parensOpen = firstCode.getLastTokenOnSiblings(true, TokenSearch.makeOptional("DEFINE"), TokenSearch.makeOptional("ROOT"), "VIEW", TokenSearch.ANY_IDENTIFIER, DDL.PARENS_OPEN_STRING);
		if (parensOpen != null && parensOpen.hasChildren()) {
			return alignNameList(code, command, parensOpen);
		}
		
		// align GROUP BY clause
      Token byToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "GROUP", "BY");
      if (byToken != null) { 
			return alignGroupBy(code, command, byToken);
      }
      
		return false;
	}

	private boolean alignNameList(Code code, Command command, Token parensOpen) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		boolean changed = false;		

		Token viewName = parensOpen.getPrevCodeSibling();
      Token listStart = parensOpen.getNextCodeToken();
		Token parensClose = parensOpen.getNextCodeSibling();

      AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		changed |= buildTableFromCommaSepList(table, listStart, parensClose);

		DdlNameListPos listPos = DdlNameListPos.forValue(configNameListPos.getValue());
		int firstLineBreaks;
		int basicIndent;
		switch(listPos) {
			case CONTINUE:
				if (parensOpen.getPrev().isComment()) {
					changed |= parensOpen.setWhitespace(1, viewName.getEndIndexInLine());
				} else {
					changed |= parensOpen.setWhitespace(0, 0);
				}
				firstLineBreaks = 0;
				basicIndent = parensOpen.getEndIndexInLine() + 1;
				if (parensClose.getPrev().isComment()) {
					changed |= parensClose.setWhitespace(1, viewName.getEndIndexInLine());
				} else {
					changed |= parensClose.setWhitespace(0, 1);
				}
				break;
				
			case LINE_START_PLUS_2:
				changed |= parensOpen.setWhitespace(1, 0);
				firstLineBreaks = 1;
				basicIndent = 2;
				changed |= parensClose.setWhitespace(1, 0);
				break;
				
			case LINE_START_PLUS_4:
				changed |= parensOpen.setWhitespace(1, 2);
				firstLineBreaks = 1;
				basicIndent = 4;
				changed |= parensClose.setWhitespace(1, 2);
				break;

			case VIEW_NAME_PLUS_2:
				int viewStartIndex = viewName.getStartIndexInLine();
				changed |= parensOpen.setWhitespace(1, viewStartIndex);
				firstLineBreaks = 1;
				basicIndent = viewName.getStartIndexInLine() + 2;
				changed |= parensClose.setWhitespace(1, viewStartIndex);
				break;
			
			case VIEW_NAME_PLUS_4:
				int viewStartIndexPlus2 = viewName.getStartIndexInLine() + 2;
				changed |= parensOpen.setWhitespace(1, viewStartIndexPlus2);
				firstLineBreaks = 1;
				basicIndent = viewName.getStartIndexInLine() + 4;
				changed |= parensClose.setWhitespace(1, viewStartIndexPlus2);
				break;
			
			default: // KEEP_AS_IS:
				firstLineBreaks = listStart.lineBreaks;
				basicIndent = listStart.getStartIndexInLine();
		}
		
		SelectListLayout complexLayout = SelectListLayout.forValue(configNameListLayout.getValue());
		SelectListLayout simpleLayout = complexLayout;
		changed |= alignTable(code, table, parensOpen, listStart, parensClose, firstLineBreaks, basicIndent, complexLayout, simpleLayout);

		return changed;
	}

	private boolean alignGroupBy(Code code, Command command, Token byToken) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		boolean changed = false;		

      Token firstClauseToken = byToken.getPrevCodeSibling();
      Token listStart = byToken.getNextCodeSibling();
      Token lastClauseToken = command.getLastCodeToken();
		
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		changed |= buildTableFromCommaSepList(table, listStart, lastClauseToken);
      
		DdlGroupByListPos listPos = DdlGroupByListPos.forValue(configGroupByListPos.getValue());
		int firstLineBreaks;
		int basicIndent;
		switch(listPos) {
			case CONTINUE:
				firstLineBreaks = 0;
				basicIndent = byToken.getEndIndexInLine() + 1;
				break;
				
			case LINE_START_PLUS_2:
				firstLineBreaks = 1;
				basicIndent = byToken.getFirstTokenInLine().spacesLeft + 2;
				break;

			case LINE_START_PLUS_4:
				firstLineBreaks = 1;
				basicIndent = byToken.getFirstTokenInLine().spacesLeft + 4;
				break;
			
			default: // KEEP_AS_IS:
				firstLineBreaks = listStart.lineBreaks;
				basicIndent = listStart.getStartIndexInLine();
		}
		
		SelectListLayout complexLayout = SelectListLayout.forValue(configComplexGroupByListLayout.getValue());
		SelectListLayout simpleLayout = SelectListLayout.forValue(configSimpleGroupByListLayout.getValue());
      changed |= alignTable(code, table, firstClauseToken, listStart, lastClauseToken, firstLineBreaks, basicIndent, complexLayout, simpleLayout);
      
      return changed;
	}

	private boolean buildTableFromCommaSepList(AlignTable table, Token listStart, Token lastClauseToken) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		boolean changed = false;
		Token token = listStart;
		do {
			Token firstInTerm = token;
			Token lastInTerm = firstInTerm;
			while (token != null && !token.textEqualsAny(DDL.COMMA_SIGN_STRING, DDL.SEMICOLON_SIGN_STRING)) {
				lastInTerm = token;
				if (token == lastClauseToken) {
					token = null;
					break;
				}
				token = token.getNextCodeSibling();
			}
			AlignLine line = table.addLine();
			Term term;
			try {
				term = Term.createForTokenRange(firstInTerm, lastInTerm);
			} catch (UnexpectedSyntaxException e) {
				if (changed) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				} else {
					throw new UnexpectedSyntaxBeforeChanges(this, e);
				}
			}
			line.setCell(Columns.FIELD.getValue(), new AlignCellTerm(term));
			if (token == null)
				break;
			
			if (token.isComma() && !token.isAttached() && !token.getPrev().isComment()) {
				changed |= token.setWhitespace(0, 0);
			}
			
			token = token.getNextCodeSibling();
		} while (token != null);
		return changed;
	}

	private boolean alignTable(Code code, AlignTable table, Token firstClauseToken, Token listStart, Token lastClauseToken, 
			int firstLineBreaks, int basicIndent, SelectListLayout complexFieldsLayout, SelectListLayout simpleFieldsLayout) {
		
		if (table.isEmpty() || complexFieldsLayout == SelectListLayout.KEEP_AS_IS && simpleFieldsLayout == SelectListLayout.KEEP_AS_IS)
			return false;
		
		SelectListLayout layout = determineLayout(complexFieldsLayout, simpleFieldsLayout, table);
		if (layout == SelectListLayout.KEEP_AS_IS) 
			return false;
		
		if (layout == SelectListLayout.DERIVE) {
			// derive SelectListLayout.MULTI_LINE or .ONE_LINE from the majority of fields in the current layout
			int lineBreakCount = 0;
			int continueCount = 0;
			for (AlignLine line : table.getLines()) {
				if (line == table.getLine(0)) {
					// the first entry doesn't count
					continue;
				} else if (line.getFirstToken().lineBreaks > 0) {
					++lineBreakCount;
				} else {
					++continueCount;
				}
			}
			layout = (lineBreakCount > continueCount) ? SelectListLayout.MULTI_LINE : SelectListLayout.ONE_LINE;
			// continue below in case the current layout does not match maximum line width, or indentation is inconsistent
		}

		boolean changed = false;
		
		// ensure that the opening parenthesis continues the line; also, change '(a, b)' or '(a, b )' into '( a, b )';
		// make sure the list starts directly behind the keyword(s)
		changed |= !listStart.getPrev().isComment() && listStart.setWhitespace();
		
		// layout the field list
		if (layout == SelectListLayout.ONE_LINE) {
			changed |= firstClauseToken.condenseUpTo(lastClauseToken, configMaxLineLength.getValue(), basicIndent, true);
			
		} else { // layout == SelectListLayout.MULTI_LINE
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, true, true, true);
			code.addRuleUses(this, changedCommands);
		}
		
		// align comments
		Token tokenBeforeListStart = listStart.getPrevCodeToken(); // view name (for name list) or "BY" (for GROUP BY list)  
		Token token = listStart;
		while (token.getPrev() != null && token.getPrev().isCommentLine())
			token = token.getPrev();
		// move the first comment behind the keyword(s) that start the clause, if that fits the basic indent
		if (token.isCommentLine() && token.spacesLeft > 0 && token.getPrev() == tokenBeforeListStart) {
			if (tokenBeforeListStart.getEndIndexInLine() + 1 == basicIndent) {
				changed |= token.setWhitespace();
				token = token.getNext();
			}
		}
		// align all other comments of this clause
		while (token != null) {
			changed |= token.isCommentLine() && indentComment(token, basicIndent);
			if (token == lastClauseToken) {
				break;
			}
			token = token.getNextSibling();
		}
		return changed;
	}
	
	private SelectListLayout determineLayout(SelectListLayout complexFieldsLayout, SelectListLayout simpleFieldsLayout, AlignTable table) {
		if (complexFieldsLayout == simpleFieldsLayout) 
			return complexFieldsLayout;

		boolean hasMultiTokenField = false;
		boolean hasDot = false;
		
		for (AlignLine line : table.getLines()) {
			AlignCell cell = line.getCell(Columns.FIELD.getValue());
			if (cell.getFirstToken() != cell.getLastToken()) { // pro forma
				hasMultiTokenField = true;
			} else if (cell.getFirstToken().getText().indexOf(DDL.DOT_SIGN_STRING) >= 0) {
				hasDot = true;
			}
		}
		boolean isComplex = hasMultiTokenField || (hasDot && configConsiderDotAsComplex.getValue()); 
		return isComplex ? complexFieldsLayout : simpleFieldsLayout;
	}

	private boolean indentComment(Token token, int basicIndent) {
		if (token.spacesLeft > 0 || !token.textStartsWith(DDL.LINE_END_COMMENT)) {
			return token.setWhitespace(token.lineBreaks, basicIndent);
			
		} else if (token.getTextLength() > DDL.LINE_END_COMMENT.length()) {
			String commentText = token.getText();
			int commentSignEnd = StringUtil.findFirstNonChar(commentText, 0, DDL.COMMENT_SIGN);
			if (commentSignEnd < 0)
				return false;
			int textPos = StringUtil.findFirstNonSpace(commentText, commentSignEnd);
			if (textPos <= 0) // pro forma
				return false;
			
			// put (basicIndent) spaces between the comment signs and the commented-out text
			String newCommentText = commentText.substring(0, commentSignEnd) + StringUtil.repeatChar(' ', basicIndent) + commentText.substring(textPos);
			if (!commentText.equals(newCommentText)) {
				token.setText(newCommentText, false);
				return true;
			}
		}
		return false;
	}
}
