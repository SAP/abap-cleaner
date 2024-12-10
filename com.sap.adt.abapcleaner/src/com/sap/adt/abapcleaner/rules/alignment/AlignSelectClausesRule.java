package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
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
import com.sap.adt.abapcleaner.rulehelpers.SelectClause;
import com.sap.adt.abapcleaner.rulehelpers.SelectQuery;

public class AlignSelectClausesRule extends RuleForCommands {
	public static final String DEFAULT_NAME = "Align SELECT clauses";
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_SELECT_CLAUSES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return DEFAULT_NAME; }

	@Override
	public String getDescription() { return "Aligns the keywords SELECT, FROM, FIELDS, WHERE, GROUP BY etc. in the (main- or sub-)query clauses of the ABAP SQL statements SELECT, WITH, and OPEN CURSOR."; }

	@Override
	public String getHintsAndRestrictions() { return "Field lists, joins and logical expressions in the SELECT clauses are aligned by dedicated rules: '" + AlignSelectFromRule.DEFAULT_NAME + "', '" + AlignSelectListsRule.DEFAULT_NAME + "' and '" + AlignLogicalExpressionsRule.DEFAULT_NAME + "'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 11, 18); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_SELECT_FROM, RuleID.ALIGN_SELECT_LISTS, RuleID.ALIGN_LOGICAL_EXPRESSIONS }; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_select_clauses." 
			+ LINE_SEP + "    SELECT SINGLE any_col INTO lv_any_value FROM any_dtab WHERE other_col = gc_any_constant."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \" one-liners can be created from short SELECT statements that fit into one line; otherwise," 
			+ LINE_SEP + "    \" if configured, FROM (or INTO) can be moved behind a short and simple SELECT list" 
			+ LINE_SEP + "    SELECT any_col"
			+ LINE_SEP + "    FROM any_dtab"
			+ LINE_SEP + "    INTO TABLE lt_any_table."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    SELECT any_col, other_col, third_col"
			+ LINE_SEP + "    INTO TABLE lt_any_table"
			+ LINE_SEP + "    FROM any_dtab"
			+ LINE_SEP + "    WHERE any_col < 10."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    SELECT any_col, other_col, third_col FROM any_dtab APPENDING CORRESPONDING FIELDS OF TABLE @lt_any_table"
			+ LINE_SEP + "           WHERE other_col IN ( SELECT other_col FROM other_dtab WHERE fourth_col = @lv_any_value )"
			+ LINE_SEP + "             AND third_col IN ( SELECT third_col FROM third_dtab WHERE fifth_col > @lv_other_value ). \"#EC CI_BUFFSUBQ."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    SELECT *"
			+ LINE_SEP + "           FROM any_dtab"
			+ LINE_SEP + "         WHERE any_col = @lv_any_value"
			+ LINE_SEP + "            INTO CORRESPONDING FIELDS OF TABLE NEW @lv_other_value."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    SELECT any_col"
			+ LINE_SEP + "      FROM any_dtab"
			+ LINE_SEP + "        WHERE any_col IN ( SELECT any_col"
			+ LINE_SEP + "          FROM other_dtab"
			+ LINE_SEP + "         WHERE other_col = 'X' )"
			+ LINE_SEP + "          INTO CORRESPONDING FIELDS OF @ls_any_struc. \"#EC CI_BUFFSUBQ"
			+ LINE_SEP + "    ENDSELECT."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \" the alignment of joins can be configured with rule '" + AlignSelectFromRule.DEFAULT_NAME + "'" 
			+ LINE_SEP + "    SELECT FROM any_dtab AS t1"
			+ LINE_SEP + "                INNER JOIN other_dtab AS t2 ON  t1~any_col   = t2~any_col"
			+ LINE_SEP + "                                            AND t1~other_col = t2~other_col"
			+ LINE_SEP + "                LEFT OUTER JOIN third_dtab AS t3 ON t1~third_col = t3~third_col"
			+ LINE_SEP + "      FIELDS (lt_fields)"
			+ LINE_SEP + "       GROUP BY (lt_group)"
			+ LINE_SEP + "             INTO CORRESPONDING FIELDS OF TABLE @lts_any_table."
			+ LINE_SEP + ""
			+ LINE_SEP + "    \" UNION, INTERSECT and EXCEPT combine the result sets of multiple SELECTs:" 
			+ LINE_SEP + "    SELECT a AS c1, b AS c2, c AS c3"
			+ LINE_SEP + "           FROM any_dtab"
			+ LINE_SEP + "           UNION DISTINCT"
			+ LINE_SEP + "    SELECT d AS c1, e AS c2, f AS c3"
			+ LINE_SEP + "          FROM other_dtab"
			+ LINE_SEP + "      UNION DISTINCT"
			+ LINE_SEP + "        SELECT g AS c1, h AS c2, i AS c3"
			+ LINE_SEP + "          FROM third_dtab"
			+ LINE_SEP + "            INTO TABLE @FINAL(lt_distinct_result)."
			+ LINE_SEP + "" 
			+ LINE_SEP + "    \" the final INTO ... clause belongs to the whole statement, not to the last SELECT!" 
			+ LINE_SEP + "    SELECT a AS c1, b AS c2, c AS c3, d AS c4"
			+ LINE_SEP + "            FROM any_dtab"
			+ LINE_SEP + "    INTERSECT ( SELECT d AS c1, e AS c2, f AS c3, g AS c4"
			+ LINE_SEP + "        FROM other_dtab"
			+ LINE_SEP + "    UNION"
			+ LINE_SEP + "    SELECT i AS c1, j AS c2, k AS c3, l AS c4"
			+ LINE_SEP + "               FROM third_dtab )"
			+ LINE_SEP + "               INTO TABLE @FINAL(lt_result)."
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigIntValue configMaxLineLength = new ConfigIntValue(this, "MaxLineLength", "Maximum line length for one-liners:", "", MIN_LINE_LENGTH_ABAP, DEFAULT_LINE_LENGTH_ABAP, ABAP.MAX_LINE_LENGTH);
	final ConfigEnumValue<SelectOneLinerAction> configMainQueryOneLinerAction = new ConfigEnumValue<SelectOneLinerAction>(this, "MainQueryOneLinerAction", "Mainquery one-liners:",
		new String[] { "create if possible", "keep existing", "always split" }, SelectOneLinerAction.values(), SelectOneLinerAction.KEEP_EXISTING);
	final ConfigEnumValue<SelectOneLinerAction> configSubQueryOneLinerAction = new ConfigEnumValue<SelectOneLinerAction>(this, "SubQueryOneLinerAction", "Subquery one-liners:",
		new String[] { "create if possible", "keep existing", "always split" }, SelectOneLinerAction.values(), SelectOneLinerAction.KEEP_EXISTING);
	final ConfigIntValue configMaxSelectListLengthBeforeFrom = new ConfigIntValue(this, "MaxSelectListLengthBeforeFrom", "Put simple FROM behind SELECT list with up to", "chars width", 0, 30, 100);
	final ConfigIntValue configMaxSelectListLengthBeforeInto = new ConfigIntValue(this, "MaxSelectListLengthBeforeInto", "Put simple INTO behind SELECT list with up to", "chars width (non-strict mode)", 0, 30, 100);
	final ConfigBoolValue configNewLineForFromWithJoins = new ConfigBoolValue(this, "NewLineForFromWithJoins", "Always start new line for FROM with joins", true);
	final ConfigEnumValue<SelectClauseIndent> configSelectClauseIndent = new ConfigEnumValue<SelectClauseIndent>(this, "SelectClauseIndent", "Indent of clause keywords:", new String[] { "add 2", "add 4", "add 7" }, SelectClauseIndent.values(), SelectClauseIndent.PLUS_2);

	final ConfigEnumValue<SelectUnionIndent> configSelectUnionIndent = new ConfigEnumValue<SelectUnionIndent>(this, "SelectUnionIndent", "Indent of UNION / INTERSECT / EXCEPT:", new String[] { "add 0", "add 2", "add 4", "add 7" }, SelectUnionIndent.values(), SelectUnionIndent.PLUS_0);
	final ConfigEnumValue<SelectUnionNextSelectPos> configNextSelectPos = new ConfigEnumValue<SelectUnionNextSelectPos>(this, "NextSelectPos", "Position of next SELECT after UNION etc.:", 
		new String[] { "continue after UNION etc.", "continue or put below second word", "below UNION etc.", "below UNION etc. +2", "below UNION etc. +4", "below second word of UNION etc." }, SelectUnionNextSelectPos.values(), SelectUnionNextSelectPos.BELOW_PLUS_0);
	final ConfigEnumValue<SelectUnionIntoIndent> configSelectUnionIntoIndent = new ConfigEnumValue<SelectUnionIntoIndent>(this, "SelectUnionIntoIndent", "Indent of final INTO after UNION etc.:", new String[] { "add 0", "add 2", "add 4", "add 7" }, SelectUnionIntoIndent.values(), SelectUnionIntoIndent.PLUS_0);

	private final ConfigValue[] configValues = new ConfigValue[] { configMaxLineLength, configMainQueryOneLinerAction, configSubQueryOneLinerAction, 
			configMaxSelectListLengthBeforeFrom, configMaxSelectListLengthBeforeInto, configNewLineForFromWithJoins, configSelectClauseIndent,
			configSelectUnionIndent, configNextSelectPos, configSelectUnionIntoIndent };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public AlignSelectClausesRule(Profile profile) {
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
				if (executeOn(query)) {
					code.addRuleUse(this, command);
				}
				query = query.getNextQuery();
			}
		}

		return false;
	}

	private boolean executeOn(SelectQuery query) {
		boolean changed = false;

		Term unionClause = query.getClause(SelectClause.UNION);
		Term selectClause = query.getSelectClause();
		Token selectToken = query.getSelectToken();

		// if this is not the start query ...
		if (unionClause != null && query.prevQuery != null) {
			// position UNION / INTERSECT / EXCEPT 
			SelectUnionIndent unionIndent = SelectUnionIndent.forValue(configSelectUnionIndent.getValue());
			Token refToken = query.prevQuery.getSelectToken();
			
			// the UNION etc. may not to the previous SELECT alone, but to multiple SELECTs in parentheses;
			// in such a case, the indent should be based on the opening parenthesis, NOT on the previous SELECT:
			// SELECT ...
			//   INTERSECT
			//   ( SELECT ...
			//       EXCEPT SELECT ... )
			//     UNION ... // should go here, NOT below the last SELECT +2
			Token prevCode = unionClause.firstToken.getPrevCodeToken();
			if (prevCode != null && prevCode.textEquals(")")) {
				Token testToken = refToken;
				while (testToken.getParent() != null) {
					testToken = testToken.getParent();
					if (testToken.getNextSibling() == prevCode) {
						refToken = testToken;
						break;
					}
				}
			}
			
			int prevSelectIndent = refToken.getStartIndexInLine();
			int lineBreaks = Math.max(1, unionClause.firstToken.lineBreaks);
			int spacesLeft = prevSelectIndent + unionIndent.getAddIndent();
			changed |= unionClause.setFirstTokenWhitespace(lineBreaks, spacesLeft); 
			changed |= unionClause.condense();
			
			// if SELECT is preceded by one or multiple "(", find the first "(", changing this to "( ( SELECT ..." 
			Token changeToken = selectToken;
			do {
				Token prev = changeToken.getPrev();
				if (prev == null || !prev.textEquals("("))
					break;
				if (changeToken == selectToken) {
					// move the whole SELECT term 
					changed |= selectClause.setFirstTokenWhitespace(0, 1);
				} else {
					changed |= changeToken.setWhitespace();
				}
				changeToken = prev;
			} while(true);

			// position SELECT or the first "(" preceding it
			boolean unionIsSingleWord = (unionClause.firstToken == unionClause.lastToken);
			boolean canContinue = !changeToken.getPrev().isComment();
			SelectUnionNextSelectPos nextSelectPos = SelectUnionNextSelectPos.forValue(configNextSelectPos.getValue());
			if (canContinue && (nextSelectPos == SelectUnionNextSelectPos.CONTINUE || unionIsSingleWord && nextSelectPos == SelectUnionNextSelectPos.CONTINUE_OR_BELOW_SECOND)) {
				lineBreaks = 0; 
				spacesLeft = 1;
			} else if (nextSelectPos == SelectUnionNextSelectPos.CONTINUE || nextSelectPos == SelectUnionNextSelectPos.CONTINUE_OR_BELOW_SECOND || nextSelectPos == SelectUnionNextSelectPos.BELOW_SECOND_WORD) {
				lineBreaks = 1;
				spacesLeft = query.prevQuery.getSelectToken().getStartIndexInLine() + unionIndent.getAddIndent() + unionClause.firstToken.getTextLength() + 1;
			} else {
				lineBreaks = Math.max(1, changeToken.lineBreaks);
				spacesLeft = query.prevQuery.getSelectToken().getStartIndexInLine() + unionIndent.getAddIndent() + nextSelectPos.getAddIndent();
			}
			if (changeToken == selectToken) {
				// move the whole SELECT term 
				changed |= selectClause.setFirstTokenWhitespace(lineBreaks, spacesLeft);
			} else {
				changed |= changeToken.setWhitespace(lineBreaks, spacesLeft);
			}
		}
		
		// create or keep one-liners
		SelectOneLinerAction oneLinerAction = query.isMainQuery() ? SelectOneLinerAction.forValue(configMainQueryOneLinerAction.getValue())
				 																	 : SelectOneLinerAction.forValue(configSubQueryOneLinerAction.getValue());
		if (oneLinerAction != SelectOneLinerAction.ALWAYS_SPLIT) {
			// create one-liner or keep existing one-liner (if maximum line length is observed)
			boolean isOneLiner = query.isOneLiner();
			if (isOneLiner || oneLinerAction == SelectOneLinerAction.CREATE) {
				// check whether the query fits into maximum line length
				int length = query.getLengthOnOneLine();
				if (length > 0 && selectToken.getStartIndexInLine() + length <= configMaxLineLength.getValue()) {
					changed |= query.condense();
					return changed;
				}
			}
		}
		
		// position other clauses
		SelectClauseIndent clauseIndent = SelectClauseIndent.forValue(configSelectClauseIndent.getValue());
		SelectUnionIntoIndent finalIntoIndent = SelectUnionIntoIndent.forValue(configSelectUnionIntoIndent.getValue());
		boolean useFinalIntoIndent = (query.isMainQuery() && query.getNextQuery() != null);
		
		for (SelectClause clauseType : query.getClausTypesInOrder()) {
			// UNION and SELECT were already processed above
			if (clauseType == SelectClause.UNION || clauseType == SelectClause.SELECT)
				continue;
			Term clause = query.getClause(clauseType);
			
			// put simple FROM behind a SELECT list with few fields (non-strict mode) (if FROM is the next clause)
			boolean isSimple = clauseType == SelectClause.FROM && !query.hasJoinOrParameters()
								 || clauseType == SelectClause.INTO && !query.hasIntoList();
			if (isSimple && selectClause.isOnSingleLine() && clause.isOnSingleLine() && selectClause.lastToken.getNext() == clause.firstToken) {
				int selectClauseWidth = selectClause.getSumTextAndSpaceWidth(true);
				int totalLength = selectClause.firstToken.getStartIndexInLine() + selectClauseWidth + 1 + clause.getSumTextAndSpaceWidth(true);
				int maxSelectListLength = (clauseType == SelectClause.FROM) ? configMaxSelectListLengthBeforeFrom.getValue() : configMaxSelectListLengthBeforeInto.getValue(); 
				if (selectClauseWidth <= maxSelectListLength && totalLength <= configMaxLineLength.getValue()) {
					changed |= selectClause.condense();
					changed |= clause.condense();
					changed |= clause.firstToken.setWhitespace();
					continue;
				}
			} else if (clauseType == SelectClause.FROM && clause.firstToken.lineBreaks == 0) {
				// keep simple FROM behind SELECT
				if (!configNewLineForFromWithJoins.getValue()) {
					continue;
				} else if (clause.firstToken.getPrev().isKeyword("SELECT") && clauseIndent == SelectClauseIndent.PLUS_7) {
					// for 'SELECT FROM ...', no need to move 'FROM' down by one line if indent is 7!
					continue;
				}
			}
			
			// determine indent for clause
			int lineBreaks = Math.max(1, clause.firstToken.lineBreaks);
			int spacesLeft = selectToken.getStartIndexInLine(); 
			if (useFinalIntoIndent && (clauseType == SelectClause.INTO || clauseType == SelectClause.UP_TO_OFFSET || clauseType == SelectClause.ABAP_OPTIONS)) {
				// final INTO etc. clause of the main query (with one or several UNION queries in between) 
				spacesLeft += finalIntoIndent.getAddIndent();
			} else {
				spacesLeft += clauseIndent.getAddIndent();
			}

			changed |= clause.setFirstTokenWhitespace(lineBreaks, spacesLeft);

			// continue after WHERE and INTO
			Token next = clause.firstToken.getNext();
			if (clauseType == SelectClause.WHERE && next.isCode() && next.lineBreaks > 0) {
				changed |= clause.setTokenWhitespace(next, 0, 1);
			} else if (clauseType == SelectClause.INTO && next.isCode() && (next.lineBreaks > 0 || next.spacesLeft > 1)) {
				changed |= clause.setTokenWhitespace(next, 0, 1);
			}
		}

		return changed;
	}
}
