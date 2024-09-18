package com.sap.adt.abapcleaner.rules.ddl.alignment;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Section;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellToken;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentifier;

public class DdlAlignSelectListRule extends RuleForDdlCommands {
	private enum Columns {
		KEY_OR_VIRTUAL,
		ELEMENT,
		AS_ALIAS;
		public int getValue() { return this.ordinal(); }
	}
	private static final int MAX_COLUMN_COUNT = 3;

	@Override
	public RuleID getID() { return RuleID.DDL_ALIGN_SELECT_LIST; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ALIGNMENT; }

	@Override
	public String getDisplayName() { return "Align select list"; }

	@Override
	public String getDescription() { return "Aligns key and non-key elements in the select list and their aliases."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 16); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "define view I_AnyView"
				+ LINE_SEP + "  as select from I_AnySource as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer join I_OtherSource as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.AnyField = OtherAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_ThirdSource as _ThirdAlias"
				+ LINE_SEP + "     on _ThirdAlias.AnyField = AnyAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "   @ObjectModel.text.element: ['ThirdFieldAlias']"
				+ LINE_SEP + "  key AnyAlias.AnyField as AnyFieldAlias,"
				+ LINE_SEP + " key   AnyAlias.OtherField as OtherFieldAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // non-key fields"
				+ LINE_SEP + "   OtherAlias.ThirdField as ThirdFieldAlias,"
				+ LINE_SEP + "    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,"
				+ LINE_SEP + "       OtherAlias.FifthField"
				+ LINE_SEP + "         as FifthFieldAlias,"
				+ LINE_SEP + "  // complex element with multiple dots:"
				+ LINE_SEP + "         _ThirdAlias._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "// calculated fields"
				+ LINE_SEP + "// A textual comment like this one should not be at line start, but should be aligned with the elements."
				+ LINE_SEP + "// Only commented-out code should have comment signs at line start, so Ctrl+> can easily uncomment it."
				+ LINE_SEP + " cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthField as any_type) as CalculatedField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "//   // Commented-out select list elements are also being aligned, as shown below. For this to work,"
				+ LINE_SEP + "//     // - their comment signs must be at the very start of the line (as Ctrl+< would produce them), and"
				+ LINE_SEP + "//    // - the commented-out element must not span multiple comment lines:"
				+ LINE_SEP + "// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,"
				+ LINE_SEP + "//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,"
				+ LINE_SEP + "//  OtherAlias.CommentedOutFieldWithoutAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "    // associations"
				+ LINE_SEP + " _ThirdAlias"
				+ LINE_SEP + "}"
				+ LINE_SEP + "union all"
				+ LINE_SEP + "  select from I_AnySource2 as AnyAlias"
				+ LINE_SEP + ""
				+ LINE_SEP + "    left outer join I_OtherSource2 as OtherAlias"
				+ LINE_SEP + "      on AnyAlias.AnyField = OtherAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "  association [0..*] to I_ThirdSource2 as _ThirdAliasWithVeryLongName"
				+ LINE_SEP + "     on _ThirdAliasWithVeryLongName.AnyField = AnyAlias.AnyField"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "   // key fields"
				+ LINE_SEP + "  key AnyAlias.AnyField     as AnyFieldAlias,"
				+ LINE_SEP + " key   AnyAlias.OtherField as OtherFieldAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // non-key fields"
				+ LINE_SEP + "   OtherAlias.ThirdField as ThirdFieldAlias,"
				+ LINE_SEP + "    OtherAlias.FourthFieldWithLongName as FourthFieldAlias,"
				+ LINE_SEP + "       OtherAlias.FifthField        as FifthFieldAlias,"
				+ LINE_SEP + "         _ThirdAliasWithVeryLongName.FieldNameThatGetsNoAlias,"
				+ LINE_SEP + "         _ThirdAliasWithVeryLongName._AnyAssociation._OtherAssociation.FieldNameThatGetsNoAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "// calculated fields"
				+ LINE_SEP + " cast(OtherAlias.ThirdField + OtherAlias.FourthFieldWithLongName + OtherAlias.FifthFieldWithLongName"
				+ LINE_SEP + "      + OtherAlias.SixthField as any_type) as CalculatedField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "// key OtherAlias.CommentedOutField as CommentedOutFieldAlias,"
				+ LINE_SEP + "//OtherAlias.OtherCommentedOutField as OtherCommentedOutFieldAlias,"
				+ LINE_SEP + "//  OtherAlias.CommentedOutFieldWithoutAlias,"
				+ LINE_SEP + ""
				+ LINE_SEP + "    // associations"
				+ LINE_SEP + " _ThirdAliasWithVeryLongName"
				+ LINE_SEP + "}";
	}
	
	final ConfigBoolValue configAlignKeyKeyword = new ConfigBoolValue(this, "AlignKeyKeyword", "Align keyword KEY / VIRTUAL", true);
	final ConfigBoolValue configAlignAliases = new ConfigBoolValue(this, "AlignAliases", "Align aliases", true);
	final ConfigBoolValue configConsiderSimpleElementsWithoutAlias = new ConfigBoolValue(this, "ConsiderSimpleElementsWithoutAlias", "Consider simple elements without alias (e.g. 'AnySource.AnyField') for alias position", true);
	final ConfigBoolValue configConsiderComplexElementsWithoutAlias = new ConfigBoolValue(this, "ConsiderComplexElementsWithoutAlias", "Consider complex elements without alias (but with calculations or 2+ dots) for alias position", false);
	final ConfigBoolValue configConsiderAllElementLines = new ConfigBoolValue(this, "ConsiderAllElementLines", "Consider all lines of multi-line elements for alias position", false);
   final ConfigIntValue configMaxAliasStart = new ConfigIntValue(this, "MaxAliasStart", "Maximum start position of aligned aliases:", " - Aliases that would start beyond this position are moved to the next line.", 60, 120, DDL.MAX_LINE_LENGTH);
	final ConfigBoolValue configAlignTextualComments = new ConfigBoolValue(this, "AlignTextualComments", "Align textual comments from line start with elements", true);
	final ConfigBoolValue configAlignCommentedOutCode = new ConfigBoolValue(this, "AlignCommentedOutCode", "Align commented-out code with active code", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configAlignKeyKeyword, configAlignAliases, configConsiderSimpleElementsWithoutAlias, configConsiderComplexElementsWithoutAlias, configConsiderAllElementLines, configMaxAliasStart, configAlignTextualComments, configAlignCommentedOutCode };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public DdlAlignSelectListRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		if (command.getParent() != null || !command.getOpensLevel())
			return false;

		// only continue if the Command starts a select list with "{ select_list" or "SELECT [DISTINCT] select_list FROM"
		Token lastCode = command.getLastCodeToken();
		if (lastCode == null) { // pro forma
			return false;
		} else if (!lastCode.textEquals(DDL.BRACE_OPEN_STRING) && !lastCode.isAnyKeyword("SELECT", "DISTINCT")) {
			return false;
		}
		
		// build and align table of SELECT and JOIN Commands
		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		ArrayList<Command> lineStartComments = new ArrayList<>();
		ArrayList<Section> textualMultiLineComments = new ArrayList<>(); 
		CommentIdentifier commentIdentifier = new CommentIdentifier(); 
		int indent = lastCode.getFirstTokenInLine().getStartIndexInLine() + DDL.INDENT_STEP; 
		try {
			command = command.getFirstChild();
			while (command != null) {
				if (command.startsMultiLineDdlComment()) {
					// textual multi-line comments will be recorded and aligned with the elements; 
					// multi-line comments that contain code will be completely skipped and not changed
					command = buildTextualMultiLineComments(command, textualMultiLineComments, commentIdentifier);
					if (command == null) { // pro forma 
						break;
					}

				} else {
					buildTable(table, command, lineStartComments, commentIdentifier);
				}
				command = command.getNextSibling(); // including comments and annotations
			}
			alignTable(code, table, indent, textualMultiLineComments, lineStartComments, commentIdentifier);
		} catch (UnexpectedSyntaxException e) {
			// exceptions are only thrown before the alignment is done, therefore no need to throw an UnexpectedSyntaxAfterChanges exception
			return false;
		}
		
		// code.addRuleUse() was already called in alignTable()
		return false;
	}
	
	private Command buildTextualMultiLineComments(Command commentStart, ArrayList<Section> textualMultiLineComments, CommentIdentifier commentIdentifier) throws UnexpectedSyntaxException {
		Command commentLast = commentStart.getLastOfMultiLineDdlComment();
		
		Command command = commentStart;
		while (command != null) {
			if (commentIdentifier.identifyComment(command.getFirstToken().getText(), false, Language.DDL).isCode()) {
				// at least part of the multi-line comment appears to be code; it will therefore not be aligned
				break;
			} else if (command == commentLast) {
				// the entire multi-line comment appears to be textual; it can therefore be aligned
				textualMultiLineComments.add(Section.create(commentStart, commentLast));
				break;
			}
			command = command.getNext();
		}

		return commentLast;
	}

	private void buildTable(AlignTable table, Command command, ArrayList<Command> lineStartComments, CommentIdentifier commentIdentifier) throws UnexpectedSyntaxException {
		// comments at line start will be handled in alignTable() and do NOT create a line in the AlignTable
		if (command.isCommentLine() && command.getFirstToken().spacesLeft == 0) {
			// exception: textual comments will be aligned with the elements below, unless configured otherwise
			if (!configAlignTextualComments.getValue() || commentIdentifier.identifyComment(command.getFirstToken().getText(), false, Language.DDL).isCode()) {
				lineStartComments.add(command);
				return;
			} else if (StringUtil.findFirstNonChar(command.getFirstToken().getText(), 0, DDL.COMMENT_SIGN) < 0 && lineStartComments.size() > 0 && lineStartComments.get(lineStartComments.size() - 1) == command.getPrev()) {
				// a line-start comment that only consists of "//" or "///" etc. and is preceded by another line-start comment is added, too
				lineStartComments.add(command);
				return;
			}
		} 
		
		AlignLine line = table.addLine();
		
		// for annotations and (non-line-start) comments, only fill the 'ELEMENT' cell and configure it to not contribute to column width
		if (command.isDdlAnnotation() || command.isCommentLine()) {
			Term commentOrAnnotationTerm = Term.createForTokenRange(command.getFirstToken(), command.getLastToken());
			AlignCellTerm commentOrAnnotationCell = AlignCellTerm.createSpecial(commentOrAnnotationTerm, 0, true);
			line.setCell(Columns.ELEMENT.getValue(), commentOrAnnotationCell);
			return;
		}

		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) // pro forma
			return;

		// determine keyword "KEY" or "VIRTUAL"
		Token elementStart = firstCode;
		if (firstCode.isAnyKeyword("KEY", "VIRTUAL")) {
			line.setCell(Columns.KEY_OR_VIRTUAL.getValue(), new AlignCellToken(firstCode));
			elementStart = firstCode.getNextCodeSibling();
		}

		// determine alias
		Token aliasName = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "AS", TokenSearch.ANY_IDENTIFIER);
		Token asToken = (aliasName != null) ? aliasName.getPrevCodeSibling() : null;
		
		// fill 'ELEMENT' cell with the main part of the list element
		Token elementLast = (asToken != null) ? asToken.getPrevCodeSibling() : command.getLastCodeToken();
		Term elementTerm = Term.createForTokenRange(elementStart, elementLast);
		AlignCellTerm elementCell = new AlignCellTerm(elementTerm);
		if (asToken == null) {
			// a simple element has just the elementStart Token before the "," and contains one . at max (e.g. "FieldName," or "Alias.FieldName,")
			boolean isSimple = (elementStart.getNext() == elementLast && StringUtil.instrCount(elementStart.getText(), DDL.DOT_SIGN) <= 1);
			boolean considerElement = isSimple ? configConsiderSimpleElementsWithoutAlias.getValue() : configConsiderComplexElementsWithoutAlias.getValue();
			if (!considerElement) {
				// this element has no alias, therefore do not consider its width for the position of other aliases 
				elementCell.setOverrideTextWidth(1);
			}
		}
		line.setCell(Columns.ELEMENT.getValue(), elementCell);
		
		// fill alias cell
		if (asToken != null) {
			Term aliasTerm = Term.createForTokenRange(asToken, aliasName);
			line.setCell(Columns.AS_ALIAS.getValue(), new AlignCellTerm(aliasTerm));
		} 
	}
	
	private void alignTable(Code code, AlignTable table, int basicIndent, ArrayList<Section> textualMultiLineComments, ArrayList<Command> lineStartComments, CommentIdentifier commentIdentifier) throws UnexpectedSyntaxException {
		AlignColumn keywordColumn = table.getColumn(Columns.KEY_OR_VIRTUAL.getValue());
		AlignColumn aliasColumn = table.getColumn(Columns.AS_ALIAS.getValue());

		final boolean alignKeywords = configAlignKeyKeyword.getValue(); 
		final boolean alignAliases = configAlignAliases.getValue();
		
		// join columns if they shall not be aligned
		Columns columnWithElements = Columns.ELEMENT;
		if (!alignKeywords && !keywordColumn.isEmpty()) { 
			table.getColumn(Columns.ELEMENT.getValue()).joinIntoPreviousColumns(true);
			columnWithElements = Columns.KEY_OR_VIRTUAL;
		}

		if (alignAliases) {
			aliasColumn.setForceMaxIndent(configMaxAliasStart.getValue());
		} else {
			aliasColumn.joinIntoPreviousColumns(true, true);
		}
		
		// if configured, only consider the length of the last parameter assignment line
		// (this can only be done now, because the data source may have been joined into the KEYWORDS column above)
		if (!configConsiderAllElementLines.getValue() ) {
			AlignColumn elementColumn = table.getColumn(columnWithElements.getValue()); 
			for (int line = 0; line < table.getLineCount(); ++line) {
				AlignCellTerm elementCell = (AlignCellTerm)elementColumn.getCellFromLine(line);
				if (!elementCell.overridesTextWidth() && !elementCell.getTerm().isOnSingleLine()) {
					int condensedWidth = elementCell.getLastToken().getEndIndexInLine() - elementCell.getFirstToken().getStartIndexInLine();
					elementCell.setOverrideTextWidth(condensedWidth);
				}
			}
		}
		
		// align the table
		Command[] changedCommands = table.align(basicIndent, 1, true);
		code.addRuleUses(this, changedCommands);

		// align textual multi-line comments (while ignoring multi-line comments that contain code)
		if (configAlignTextualComments.getValue()) {
			int commentIndent = table.getColumn(columnWithElements.getValue()).getEffectiveIndent();
			for (Section textualMultiLineComment : textualMultiLineComments) {
				int addIndent = commentIndent - textualMultiLineComment.firstCommand.getFirstToken().getStartIndexInLine();
				if (addIndent != 0) {
					textualMultiLineComment.addIndent(addIndent);
					code.addRuleUses(this, textualMultiLineComment);
				}
			}
		}

		// ----------------------------------------------------------------------
		// align commented-out code, i.e.
		// - commented-out code lines that contain entire elements: "[KEY|VIRTUAL] element [AS alias],"
		// - commented-out comments, e.g. "//   // comment"
		
		if (!configAlignCommentedOutCode.getValue())
			return;
		
		int elementColumnIndent = (columnWithElements == Columns.ELEMENT) ? table.getColumn(Columns.ELEMENT.getValue()).getEffectiveIndent() : 0;
		int aliasColumnIndent = alignAliases ? table.getColumn(Columns.AS_ALIAS.getValue()).getEffectiveIndent() : 0; 

		for (Command comment : lineStartComments) {
			// skip this comment if it has no content
			String commentText = comment.getFirstToken().getText();
			int commentSignEnd = StringUtil.findFirstNonChar(commentText, 0, DDL.COMMENT_SIGN);
			if (commentSignEnd < 0)
				continue;
			int commentStart = StringUtil.findFirstNonChar(commentText, commentSignEnd, ' ');
			if (commentStart < 0) // pro forma
				continue;

			boolean isCommentInCode = StringUtil.containsAnyAt(commentText, commentStart, DDL.LINE_END_COMMENT, DDL.LINE_END_MINUS_COMMENT, DDL.ASTERISK_COMMENT_START);
			
			// check for other reasons to skip the comment
			if (!isCommentInCode && skipComment(comment, commentIdentifier))
				continue;
			
			// determine the keyword ("key" or "virtual"), the element and (optionally) the alias
			int firstWordEnd = commentText.indexOf(' ', commentStart);
			String firstWord = (firstWordEnd < 0) ? "" : commentText.substring(commentStart, firstWordEnd);
			String keyKeyword = "";
			int elementStart;
			if (firstWord.equals("key") || firstWord.equals("virtual")) {
				keyKeyword = firstWord;
				elementStart = StringUtil.findFirstNonChar(commentText, firstWordEnd, ' ');
			} else {
				elementStart = commentStart;
			}
			// the alias is identified by the keyword " as " with no closing parenthesis behind it
			int asPos = isCommentInCode ? -1 : commentText.lastIndexOf(" as ");
			if (asPos <= 0 || commentText.indexOf(")", asPos) >= 0) {
				asPos = commentText.length();
			}
			String element = (elementStart < 0) ? "" : commentText.substring(elementStart, asPos);
			String alias = (asPos < commentText.length()) ? commentText.substring(asPos + 1) : null;
			
			// build comment anew: 
			// - comment signs at line start
			StringBuilder newComment = new StringBuilder();
			newComment.append(commentText.substring(0, commentSignEnd));
			int initialLength = newComment.length(); // only after putting the comment signs
			
			// - keyword "key" or "virtual" (if any)
			newComment.append(StringUtil.repeatChar(' ', basicIndent));
			newComment.append(keyKeyword);
			
			// - element
			int currentLength = newComment.length() - initialLength;
			int spaceCount = Math.max(elementColumnIndent - currentLength, 1);
			if (!alignKeywords && StringUtil.isNullOrEmpty(keyKeyword)
					|| alignKeywords && keywordColumn.isEmpty()) {
				spaceCount = 0;
			}
			newComment.append(StringUtil.repeatChar(' ', spaceCount));
			newComment.append(element);
			
			// - alias (if any)
			if (alias != null) { // otherwise, 'element' already ends with a comma
				currentLength = newComment.length() - initialLength;
				spaceCount = Math.max(aliasColumnIndent - currentLength, 1);
				newComment.append(StringUtil.repeatChar(' ', spaceCount));
				newComment.append(alias);
			}
			
			// change comment text
			if (comment.getFirstToken().setText(newComment.toString(), false)) {
				code.addRuleUse(this, comment);
			}
		}
	}

	private boolean skipComment(Command comment, CommentIdentifier commentIdentifier) {
		String commentText = comment.getFirstToken().getText();

		// skip this comment if it appears to be textual
		if (commentIdentifier.identifyComment(commentText, false, Language.DDL).isTextual()) 
			return true;

		// skip this comment if it appears to continue a list element, rather than starting a new one
		if (!commentStartsNewElement(comment, commentIdentifier))
			return true;
		
		// skip this comment if it does not end with a comma (with no closing parenthesis behind it)
		int commaPos = commentText.lastIndexOf(",");
		if (commaPos <= 0 || commentText.indexOf(")", commaPos) >= 0) 
			return true;

		// process the comment
		return false;
	}

	private boolean commentStartsNewElement(Command comment, CommentIdentifier commentIdentifier) {
		// determine whether the previous element ends with a comma; otherwise do not align this comment line, 
		// because it may just continue a multi-line element, and the correct indentation would be hard to determine
		
		// find the previous Command that represents a select list element (possibly commented-out and possibly partial) 
		Command prevElement = comment.getPrev();
		while (prevElement != null) {
			if (prevElement.isDdlAnnotation() || prevElement.isCommentedOutDdlAnnotation()) {
				// continue below to skip annotations

			} else if (!prevElement.isCommentLine()) {
				// actual code found
				break;
			
			} else if (commentIdentifier.identifyComment(prevElement.getFirstToken().getText(), true, Language.DDL).isCode()) {
				// commented-out code found
				break;
			}
			prevElement = prevElement.getPrev();
		}

		// determine whether the (supposed) previous element ends with a comma
		if (prevElement == null) {
			return true;	

		} else if (prevElement.isCommentLine()) {
			return prevElement.getLastToken().textEndsWith(DDL.COMMA_SIGN_STRING);	
		
		} else if (prevElement.getLastCodeToken() != null){
			return prevElement.getLastCodeToken().textEndsWith(DDL.COMMA_SIGN_STRING);
		
		} else {
			return false;
		}
	}
}
