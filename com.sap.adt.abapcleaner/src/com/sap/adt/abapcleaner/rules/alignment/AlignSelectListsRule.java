package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.parser.TokenType;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigIntValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rulehelpers.SelectClause;
import com.sap.adt.abapcleaner.rulehelpers.SelectQuery;

public class AlignSelectListsRule extends RuleForCommands {
	public static final String DEFAULT_NAME = "Align SELECT lists";

	public enum Columns {
		FIELD, 
		ADDITIONS;
		public int getValue() { return this.ordinal(); }
	}
	public static final int MAX_COLUMN_COUNT = 2;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_SELECT_LISTS; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return DEFAULT_NAME; }

	@Override
	public String getDescription() { return "Aligns the field lists after SELECT, FIELDS, ORDER BY, GROUP BY, and INTO ( ... ) of the ABAP SQL statements SELECT, WITH, and OPEN CURSOR."; }

	@Override
	public String getHintsAndRestrictions() { return "GROUPING SETS ( ... ) is kept unchanged."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 11, 18); }

	@Override
	public RuleReference[] getReferences() { return references; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_select_lists." 
			+ LINE_SEP + "    \" when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc."
			+ LINE_SEP + "    \" was already determined by the rule '" + AlignSelectClausesRule.DEFAULT_NAME + "';"
			+ LINE_SEP + "    \" this rule now aligns the field lists after SELECT, FIELDS, ORDER BY, GROUP BY, and INTO ( ... ):"
			+ LINE_SEP + "    SELECT first_column,"
			+ LINE_SEP + "      second_column,"
			+ LINE_SEP + "      third_column, fourth_column,"
			+ LINE_SEP + "      fifth_column, sixth_column,"
			+ LINE_SEP + "*  seventh_column"
			+ LINE_SEP + "      eigth_column"
			+ LINE_SEP + "      FROM any_dtab"
			+ LINE_SEP + "      ORDER BY seventh_column ASCENDING NULLS FIRST, first_column DESCENDING NULLS LAST, second_column DESCENDING"
			+ LINE_SEP + "      INTO TABLE @DATA(lt_any_table)."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT t1~any_col AS any_alias, t1~other_col AS other_alias,"
			+ LINE_SEP + "       t2~third_col,"
			+ LINE_SEP + "       SUM( fourth_col ) AS fourth_col, SUM( fifth_col ) AS fifth_col"
			+ LINE_SEP + "      FROM any_dtab AS t1"
			+ LINE_SEP + "           INNER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col"
			+ LINE_SEP + "      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table ##too_many_itab_fields"
			+ LINE_SEP + "      WHERE t1~any_col    = @ms_any_struc-any_comp"
			+ LINE_SEP + "        AND t2~third_col IN @lt_other_table"
			+ LINE_SEP + "      GROUP BY t1~any_col,"
			+ LINE_SEP + "      t1~other_col,"
			+ LINE_SEP + "      t2~third_col,"
			+ LINE_SEP + "      fourth_col, fifth_col."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT any_col, other_col, third_col, fourth_col,"
			+ LINE_SEP + "      fifth_col, sixth_column_with_long_name, seventh_column_with_long_name"
			+ LINE_SEP + "      FROM any_dtab"
			+ LINE_SEP + "      WHERE any_col   IN @ir_any_ref"
			+ LINE_SEP + "        AND other_col IN @ir_other_ref"
			+ LINE_SEP + "      ORDER BY fifth_col,"
			+ LINE_SEP + "             any_col,"
			+ LINE_SEP + "              other_col"
			+ LINE_SEP + "      INTO TABLE @DATA(lt_any_table)."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT SINGLE t1~any_col t2~other_col t1~third_col t2~fourth_col"
			+ LINE_SEP + "      FROM any_dtab"
			+ LINE_SEP + "           INNER JOIN fifth_col ON t2~col_6 = t1~col_6"
			+ LINE_SEP + "      WHERE col_7 = @is_any_struc-any_comp"
			+ LINE_SEP + "        AND col_8 = @is_any_struc-other_comp"
			+ LINE_SEP + "      INTO ( @lv_any_value, @lv_other_value,"
			+ LINE_SEP + "      @DATA(lv_third_value),"
			+ LINE_SEP + "     @FINAL(lv_fourth_value) )."
			+ LINE_SEP + "  ENDMETHOD.";
   }

   private final String[] layoutTexts = new String[] { "multi-line", "single line", "derive from majority", "keep as is" };
	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length", "", 80, 120, ABAP.MAX_LINE_LENGTH);
	final ConfigEnumValue<SelectListLayout> configComplexSelectListLayout = new ConfigEnumValue<SelectListLayout>(this, "ComplexSelectListLayout", "Select lists with complex fields:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);
	final ConfigEnumValue<SelectListLayout> configSimpleSelectListLayout = new ConfigEnumValue<SelectListLayout>(this, "SimpleSelectListLayout", "Select lists with simple fields only:", layoutTexts, SelectListLayout.values(), SelectListLayout.DERIVE);
	final ConfigEnumValue<SelectListLayout> configComplexGroupByListLayout = new ConfigEnumValue<SelectListLayout>(this, "ComplexGroupByListLayout", "GROUP BY lists with complex fields:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);
	final ConfigEnumValue<SelectListLayout> configSimpleGroupByListLayout = new ConfigEnumValue<SelectListLayout>(this, "SimpleGroupByListLayout", "GROUP BY lists with simple fields only:", layoutTexts, SelectListLayout.values(), SelectListLayout.DERIVE);
	final ConfigEnumValue<SelectListLayout> configComplexOrderByListLayout = new ConfigEnumValue<SelectListLayout>(this, "ComplexOrderByListLayout", "ORDER BY lists with complex fields:", layoutTexts, SelectListLayout.values(), SelectListLayout.MULTI_LINE);
	final ConfigEnumValue<SelectListLayout> configSimpleOrderByListLayout = new ConfigEnumValue<SelectListLayout>(this, "SimpleOrderByListLayout", "ORDER BY lists with simple fields only:", layoutTexts, SelectListLayout.values(), SelectListLayout.DERIVE);
	final ConfigEnumValue<SelectListLayout> configSelectIntoLayout = new ConfigEnumValue<SelectListLayout>(this, "SelectIntoLayout", "INTO (...) list:", layoutTexts, SelectListLayout.values(), SelectListLayout.DERIVE);
	
	final ConfigBoolValue configConsiderTildeAsComplex = new ConfigBoolValue(this, "ConsiderTildeAsComplex", "Consider 'tabalias~col' as complex", true);
	final ConfigBoolValue configAlignAsInSelectList = new ConfigBoolValue(this, "AlignAsInSelectList", "Align AS in multi-line select list", true);
	final ConfigBoolValue configAlignAdditionsInOrderByList = new ConfigBoolValue(this, "AlignAdditionsInOrderByList", "Align ASCENDING, DESCENDING etc. in multi-line ORDER BY list", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, 
			configComplexSelectListLayout, configSimpleSelectListLayout, configComplexGroupByListLayout, configSimpleGroupByListLayout, 
			configComplexOrderByListLayout, configSimpleOrderByListLayout, configSelectIntoLayout, 
			configConsiderTildeAsComplex, configAlignAsInSelectList, configAlignAdditionsInOrderByList };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignSelectListsRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxAfterChanges, UnexpectedSyntaxBeforeChanges {
		if (!command.isAbapSqlOperation())
			return false;
		
		ArrayList<SelectQuery> queryStarts = null;
		try {
			queryStarts = SelectQuery.createQueryChainsFrom(command);
		} catch (UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxBeforeChanges(this, e);
		}

		for (SelectQuery queryStart : queryStarts) {
			SelectQuery query = queryStart;
			while (query != null) {
				// skip one-liners
				if (query.isOneLiner()) {
					query = query.getNextQuery();
					continue;
				}
				
				boolean changed = false;
				try {
					changed |= executeOn(code, query, SelectClause.SELECT, SelectListLayout.forValue(configComplexSelectListLayout.getValue()), SelectListLayout.forValue(configSimpleSelectListLayout.getValue()));
					changed |= executeOn(code, query, SelectClause.FIELDS, SelectListLayout.forValue(configComplexSelectListLayout.getValue()), SelectListLayout.forValue(configSimpleSelectListLayout.getValue())); 
					changed |= executeOn(code, query, SelectClause.GROUP_BY, SelectListLayout.forValue(configComplexGroupByListLayout.getValue()), SelectListLayout.forValue(configSimpleGroupByListLayout.getValue())); 
					changed |= executeOn(code, query, SelectClause.ORDER_BY, SelectListLayout.forValue(configComplexOrderByListLayout.getValue()), SelectListLayout.forValue(configSimpleOrderByListLayout.getValue())); 
					changed |= executeOn(code, query, SelectClause.INTO, SelectListLayout.forValue(configSelectIntoLayout.getValue()), SelectListLayout.forValue(configSelectIntoLayout.getValue()));
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				}

				if (changed)
					code.addRuleUse(this, command);

				query = query.getNextQuery();
			}
		}

		return false;
	}
	
	private boolean executeOn(Code code, SelectQuery query, SelectClause clauseType, SelectListLayout complexFieldsLayout, SelectListLayout simpleFieldsLayout) throws UnexpectedSyntaxException {
		if (complexFieldsLayout == SelectListLayout.KEEP_AS_IS && simpleFieldsLayout == SelectListLayout.KEEP_AS_IS)
			return false;
		
		Term clause = query.getClause(clauseType);
		if (clause == null)
			return false;
		
		// skip cases without any list
		boolean skip = false;
		Token start = null;
		if (clauseType == SelectClause.SELECT && !query.hasSelectListInSelectClause()) {
			// SELECT [SINGLE [FOR UPDATE]]
			skip = true;
					
		} else if (clauseType == SelectClause.SELECT || clauseType == SelectClause.FIELDS) {
			start = clause.firstToken.getNextCodeSibling();
			while (start.isAnyKeyword("SINGLE", "DISTINCT")) 
				start = start.getNextCodeSibling();
			// skip 'SELECT|FIELDS [DISTINCT] (column_syntax)' and 'SELECT|FIELDS [DISTINCT] *'
			skip = opensDynamicSyntax(start, clause) || start.textEquals("*") && start == clause.lastToken;
			
		} else if (clauseType == SelectClause.GROUP_BY) {
			// skip 'GROUP BY (grouping_syntax)'
			start = clause.firstToken.getNextCodeSibling().getNextCodeSibling();
			skip = opensDynamicSyntax(start, clause);
		
		} else if (clauseType == SelectClause.ORDER_BY) {
			start = clause.firstToken.getNextCodeSibling().getNextCodeSibling();
			// skip 'ORDER BY PRIMARY KEY' and 'ORDER BY (column_syntax)'
			skip = start.matchesOnSiblings(true, "PRIMARY", "KEY") || opensDynamicSyntax(start, clause);
		
		} else if (clauseType == SelectClause.INTO) {
			start = clause.firstToken.getNextCodeSibling();
			// skip everything except 'INTO (elem1, elem2,  ...)' 
			if (start.textEquals("(") && start.hasCodeChildren() && start.getNextSibling() == clause.lastToken) {
				start = start.getFirstCodeChild();
			} else {
				skip = true;
			}
		}
		if (skip) {
			return false;
		}
		
		boolean hasComma;
		if (clauseType == SelectClause.INTO) {
			Token firstCodeChild = clause.firstToken.getNextCodeSibling().getFirstCodeChild(); 
			hasComma = (firstCodeChild != null && firstCodeChild.matchesOnSiblings(true, TokenSearch.ASTERISK, ","));
		} else {
			hasComma = (clause.findSiblingOfTypeAndTexts(TokenType.COMMA, ",") != null);
		}

		// read field lists:
		// SELECT|FIELDS [DISTINCT] {..., data_source~*, ..., col_spec [AS alias], ...} 
		// ORDER BY { {col1|a1|sql_exp1} [ASCENDING|DESCENDING] [{NULLS FIRST}|{NULLS LAST}], 
      //            {col2|a2|sql_exp2} [ASCENDING|DESCENDING] [{NULLS FIRST}|{NULLS LAST}], ...}
		// GROUP BY { sql_exp1, sql_exp2 ... 
		//            grouping_sets1, grouping_sets2, ...} 
		// INTO (elem1, elem2,  ...) 

		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		boolean changed = false;
		if (hasComma) { 
			changed |= buildTableFromCommaSepList(table, clauseType, clause, start);
		} else { 
			changed |= buildTableFromSpaceSepList(table, clauseType, clause, start);
		}
		
		SelectListLayout layout = determineLayout(clauseType, complexFieldsLayout, simpleFieldsLayout, table);
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

		// ensure that the opening parenthesis continues the line after INTO;   
		// also, change '(a, b)' or '(a, b )' into '( a, b )': those three are syntactically fine, but '( a, b)' would be erroneous;
		// for single elements, '(a)' and '( a )' are fine, but '(a )' and '( a)' are erroneous
		if (clauseType == SelectClause.INTO) {
			Token openingParens = start.getParent();
			Token closingParens = openingParens.getNextSibling();
			changed |= !openingParens.getPrev().isComment() && openingParens.setWhitespace();
			changed |= start.isAttached() && start.setWhitespace();
			changed |= closingParens.isAttached() && closingParens.setWhitespace();
		} else {
			// for SELECT, FIELDS, GROUP BY, ORDER BY, make sure the list starts directly behind the keyword(s)
			changed |= !start.getPrev().isComment() && start.setWhitespace();
		}
		
		// layout the field list
		int basicIndent = start.getPrevCodeToken().getEndIndexInLine() + 1;
		if (layout == SelectListLayout.ONE_LINE) {
			changed |= clause.firstToken.condenseUpTo(clause.lastToken, configMaxLineLength.getValue(), basicIndent, true);
			
		} else { // layout == SelectListLayout.MULTI_LINE
			// if the FROM or INTO clause continue after the SELECT list, and the SELECT list will be split to multiple
			// lines, then move the FROM or INTO clause to the next line, using the indent from the clause after it
			if (clauseType == SelectClause.SELECT && table.getLineCount() > 1) {
				SelectClause nextClauseType = query.getNextClauseInOrder(clauseType);
				SelectClause nextNextClauseType = query.getNextClauseInOrder(nextClauseType);
				if ((nextClauseType == SelectClause.FROM || nextClauseType == SelectClause.INTO) 
						&& (nextNextClauseType != SelectClause.NONE && nextNextClauseType != SelectClause.UNION)) {
					Term nextClause = query.getClause(nextClauseType);
					Term nextNextClause = query.getClause(nextNextClauseType);
					if (nextClause.firstToken.lineBreaks == 0 && nextNextClause.firstToken.lineBreaks > 0) {
						changed |= nextClause.setFirstTokenWhitespace(1, nextNextClause.firstToken.spacesLeft);
					}
				}
			}

			// determine whether to align the additions 'AS ...', 'ASCENDING' etc.
			boolean alignAdditions = true;
			if (clauseType == SelectClause.SELECT || clauseType == SelectClause.FIELDS) {
				alignAdditions = configAlignAsInSelectList.getValue();
			} else { // clauseType == clauseType.ORDER_BY
				alignAdditions = configAlignAdditionsInOrderByList.getValue();
			}
			if (!alignAdditions)
				table.getColumn(Columns.ADDITIONS.getValue()).joinIntoPreviousColumns(true, true);

			int firstLineBreaks = 0;
			Command[] changedCommands = table.align(basicIndent, firstLineBreaks, true);
			code.addRuleUses(this, changedCommands);
		}
		
		// align comments
		Token token = start;
		while (token.getPrev() != null && token.getPrev().isCommentLine())
			token = token.getPrev();
		// move the first comment behind the keyword(s) that start the clause, if that fits the basic indent 
		if (token.isQuotMarkCommentLine() && token.getPrev() != null && token.getPrev().isKeyword()) {
			if (token.getPrev().getEndIndexInLine() + 1 == basicIndent) {
				changed |= token.setWhitespace();
				token = token.getNext();
			}
		}
		// align all other comments of this clause
		while (token != null) {
			changed |= token.isCommentLine() && indentComment(token, basicIndent);
			if (token == clause.lastToken) {
				break;
			}
			token = token.getNextSibling();
		}
		return changed;
	}

	private boolean indentComment(Token token, int basicIndent) {
		if (token.isQuotMarkCommentLine()) {
			return token.setWhitespace(token.lineBreaks, basicIndent);
			
		} else if (token.isAsteriskCommentLine() && token.getTextLength() > 1) {
			String commentText = token.getText();
			int commentSignEnd = StringUtil.findFirstNonChar(commentText, 0, ABAP.LINE_COMMENT_SIGN);
			if (commentSignEnd < 0)
				return false;
			int textPos = StringUtil.findFirstNonSpace(commentText, commentSignEnd);
			if (textPos <= 0) // pro forma
				return false;
			
			// put (basicIndent) spaces between the asterisk sign(s) and the commented-out text
			String newCommentText = commentText.substring(0, commentSignEnd) + StringUtil.repeatChar(' ', basicIndent) + commentText.substring(textPos);
			if (!commentText.equals(newCommentText)) {
				token.setText(newCommentText, false);
				return true;
			}
		}
		return false;
	}

	private boolean opensDynamicSyntax(Token token, Term clause) {
		if (!token.textEquals("(") || !token.hasChildren())
			return false;
		Token next = token.getNext();
		Token next2 = next.getNext();
		return (next.isAttached() && next2.textEquals(")") && next2 == clause.lastToken);
	}

	private boolean buildTableFromCommaSepList(AlignTable table, SelectClause clauseType, Term clause, Token start) throws UnexpectedSyntaxException {
		boolean changed = false;
		Token token = start;
		do {
			// determine the next field
			Token firstInTerm = token;
			Token lastInTerm = firstInTerm;
			while (token != null && !token.isComma() && !token.isAnyKeyword("AS", "ASCENDING", "DESCENDING", "NULLS")) {
				lastInTerm = token;
				if (token == clause.lastToken) {
					token = null;
					break;
				}
				token = token.getNextCodeSibling();
			}
			AlignLine line = table.addLine();
			line.setCell(Columns.FIELD.getValue(), new AlignCellTerm(Term.createForTokenRange(firstInTerm, lastInTerm)));
			if (token == null)
				break;
			
			// determine possible additions behind the field
			if (!token.isComma()) {
				firstInTerm = token;
				lastInTerm = firstInTerm;
				while (token != null && !token.isComma()) {
					lastInTerm = token;
					if (token == clause.lastToken) {
						token = null;
						break;
					}
					token = token.getNextCodeSibling();
				}
				line.setCell(Columns.ADDITIONS.getValue(), new AlignCellTerm(Term.createForTokenRange(firstInTerm, lastInTerm)));
			}
			if (token == null) {
				break;
			} else if (token.isComma() && !token.isAttached() && token.lineBreaks == 0) {
				changed |= token.setWhitespace(0, 0);
			}
			
			token = token.getNextCodeSibling();
		} while (token != null);
		return changed;
	}
	
	private boolean buildTableFromSpaceSepList(AlignTable table, SelectClause clauseType, Term clause, Token start) throws UnexpectedSyntaxException {
		Token token = start;
		do {
			// determine the next field
			Term field;
			if (clauseType == SelectClause.GROUP_BY && token.matchesOnSiblings(true, "GROUPING", "SETS|SETS(")) {
				// GROUPING SETS ( ... )
				Token closingParens = token.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, ")");
				field = Term.createForTokenRange(token, closingParens);
			} else {
				field = Term.createArithmetic(token);
			}
			AlignLine line = table.addLine();
			line.setCell(Columns.FIELD.getValue(), new AlignCellTerm(field));
			if (field.lastToken == clause.lastToken)
				break;
			
			// determine possible additions behind the field
			Term additions = null;
			token = field.getNextCodeSibling();
			if (token == null) {
				break;
			} else if (token.isKeyword("AS")) {
				additions = Term.createForTokenRange(token, token.getNextCodeSibling());
			} else if (token.isAnyKeyword("ASCENDING", "DESCENDING", "NULLS")) { // [ASCENDING|DESCENDING] [{NULLS FIRST}|{NULLS LAST}],
				Token firstInTerm = token;
				Token lastInTerm = firstInTerm;
				while (token.isAnyKeyword("ASCENDING", "DESCENDING", "NULLS", "FIRST", "LAST")) {
					lastInTerm  = token;
					token = token.getNextCodeSibling();
				}
				additions = Term.createForTokenRange(firstInTerm, lastInTerm);
			}
			if (additions != null) {
				line.setCell(Columns.ADDITIONS.getValue(), new AlignCellTerm(additions));
				if (additions.lastToken == clause.lastToken)
					break;
				token = additions.getNextCodeSibling();
			}
		} while (token != null);
		return false;
	}

	private SelectListLayout determineLayout(SelectClause clauseType, SelectListLayout complexFieldsLayout, SelectListLayout simpleFieldsLayout, AlignTable table) {
		if (complexFieldsLayout == simpleFieldsLayout) 
			return complexFieldsLayout;

		boolean hasTilde = false;
		boolean hasMultiTokenField = false;
		boolean hasAdditions = false;
		
		for (AlignLine line : table.getLines()) {
			AlignCell cell = line.getCell(Columns.FIELD.getValue());
			if (cell.getFirstToken() != cell.getLastToken()) {
				hasMultiTokenField = true;
			} else if (cell.getFirstToken().getText().indexOf('~') >= 0) {
				hasTilde = true;
			}
			if (line.getCell(Columns.ADDITIONS.getValue()) != null) {
				hasAdditions = true;
			}
		}
		boolean isComplex = hasMultiTokenField || hasAdditions || (hasTilde && configConsiderTildeAsComplex.getValue()); 
		return isComplex ? complexFieldsLayout : simpleFieldsLayout;
	}
}
