package com.sap.adt.abapcleaner.rules.alignment;

import java.time.LocalDate;
import java.util.ArrayList;

import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.AlignCell;
import com.sap.adt.abapcleaner.rulehelpers.AlignCellTerm;
import com.sap.adt.abapcleaner.rulehelpers.AlignColumn;
import com.sap.adt.abapcleaner.rulehelpers.AlignLine;
import com.sap.adt.abapcleaner.rulehelpers.AlignTable;
import com.sap.adt.abapcleaner.rulehelpers.SelectClause;
import com.sap.adt.abapcleaner.rulehelpers.SelectQuery;

public class AlignSelectFromRule extends RuleForCommands {
	public static final String DEFAULT_NAME = "Align SELECT ... FROM ... JOIN";

	public enum Columns {
		JOIN,
		TABLE_NAME, 
		AS,
		ON;
		public int getValue() { return this.ordinal(); }
	}
	public static final int MAX_COLUMN_COUNT = 4;

	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.ALIGN_SELECT_FROM; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.ALIGNMENT; }

	@Override
	public String getDisplayName() { return DEFAULT_NAME; }

	@Override
	public String getDescription() { return "Aligns the FROM clause, including joins, in the ABAP SQL statements SELECT, WITH, and OPEN CURSOR."; }

	@Override
	public String getHintsAndRestrictions() { return "One-liners are kept. Logical expressions after JOIN ... ON are aligned by the rule '" + AlignLogicalExpressionsRule.DEFAULT_NAME + "'. HIERARCHY...( ) is kept unchanged."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2023, 11, 18); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.ALIGN_LOGICAL_EXPRESSIONS }; }

   @Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD align_select_from." 
			+ LINE_SEP + "    \" when this rule is executed, the position of the keywords FROM, FIELDS, WHERE etc."
			+ LINE_SEP + "    \" was already determined by the rule '" + AlignSelectClausesRule.DEFAULT_NAME + "';"
			+ LINE_SEP + "    \" this rule now aligns the FROM clause and possible JOINs in it:"
			+ LINE_SEP + "    SELECT FROM first_dtab AS t1"
			+ LINE_SEP + "             CROSS JOIN second_dtab AS t2"
			+ LINE_SEP + "           FIELDS t1~a AS a1,"
			+ LINE_SEP + "                  t1~b AS b1,"
			+ LINE_SEP + "                  t2~c AS c2,"
			+ LINE_SEP + "                  t2~d AS d2"
			+ LINE_SEP + "           WHERE t2~d = t1~d"
			+ LINE_SEP + "           ORDER BY t1~d"
			+ LINE_SEP + "           INTO CORRESPONDING FIELDS OF TABLE @lt_table."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT"
			+ LINE_SEP + "      FROM any_dtab AS t1"
			+ LINE_SEP + "        INNER JOIN other_dtab AS t2 ON t2~a = t1~a"
			+ LINE_SEP + "          LEFT OUTER JOIN third_dtab AS t3 ON t3~b = t2~a"
			+ LINE_SEP + "            INNER JOIN fourth_dtab AS t4 ON  t4~c = t3~b"
			+ LINE_SEP + "                                         AND t4~d = t3~d"
			+ LINE_SEP + "      FIELDS t1~a AS a1,"
			+ LINE_SEP + "             t1~b AS b1,"
			+ LINE_SEP + "             t2~c AS c2,"
			+ LINE_SEP + "             t3~d AS d3,"
			+ LINE_SEP + "             t3~e AS e3,"
			+ LINE_SEP + "             t4~f AS f4"
			+ LINE_SEP + "      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT"
			+ LINE_SEP + "      FROM ( any_dtab AS t1"
			+ LINE_SEP + "           INNER JOIN"
			+ LINE_SEP + "             other_dtab AS t2 ON t2~any_col = t1~any_col )"
			+ LINE_SEP + "           LEFT OUTER JOIN"
			+ LINE_SEP + "           third_dtab AS t3 ON t3~other_col = t2~any_col"
			+ LINE_SEP + "             INNER JOIN"
			+ LINE_SEP + "             fourth_dtab AS t4 ON t4~other_col = t3~other_col"
			+ LINE_SEP + "      FIELDS t1~third_col AS any_alias,"
			+ LINE_SEP + "             t4~fifth_col AS other_alias"
			+ LINE_SEP + "      INTO CORRESPONDING FIELDS OF TABLE @lt_any_table."
			+ LINE_SEP + ""
			+ LINE_SEP + "    SELECT"
			+ LINE_SEP + "      FROM any_dtab AS t1 LEFT OUTER JOIN other_dtab AS t2 ON t1~any_col = t2~any_col USING CLIENT '123'"
			+ LINE_SEP + "      FIELDS t2~other_col, t1~any_col, t2~third_col"
			+ LINE_SEP + "      INTO TABLE @FINAL(lt_any_table)."
			+ LINE_SEP + "  ENDMETHOD.";
   }

	final ConfigEnumValue<SelectFromTablePosition> configFirstTableNamePosition = new ConfigEnumValue<SelectFromTablePosition>(this, "FirstTableNamePosition", "Position of first table name:", new String[] { "continue after FROM", "below FROM", "below FROM +2" }, SelectFromTablePosition.values(), SelectFromTablePosition.CONTINUE);
	final ConfigEnumValue<SelectFromJoinIndent> configSelectJoinIndent = new ConfigEnumValue<SelectFromJoinIndent>(this, "SelectJoinIndent", "Position of INNER etc. JOIN:", new String[] { "next line", "next line +2", "next line +4" }, SelectFromJoinIndent.values(), SelectFromJoinIndent.PLUS_2);
	final ConfigEnumValue<SelectFromTablePosition> configFurtherTableNamePositions = new ConfigEnumValue<SelectFromTablePosition>(this, "FurtherTableNamePositions", "Position of further tables names:", new String[] { "continue after join", "below join", "below join +2" }, SelectFromTablePosition.values(), SelectFromTablePosition.BELOW_PLUS_2);
	final ConfigEnumValue<SelectFromOnPosition> configSelectOnPosition = new ConfigEnumValue<SelectFromOnPosition>(this, "SelectOnPosition", "Position of ON condition:", new String[] { "continue after table name", "next line", "next line +2", "below second word of join" }, SelectFromOnPosition.values(), SelectFromOnPosition.CONTINUE);
	final ConfigBoolValue configAlignAsAcrossJoins = new ConfigBoolValue(this, "AlignAsAcrossJoins", "Align AS across multiple joins", false);
	final ConfigBoolValue configAlignOnAcrossJoins = new ConfigBoolValue(this, "AlignOnAcrossJoins", "Align ON across multiple joins (if ON is behind join)", false);
	final ConfigEnumValue<SelectFromClientPosition> configSelectClientPosition = new ConfigEnumValue<SelectFromClientPosition>(this, "SelectClientPosition", "Own line for CLIENT additions:", new String[] { "never", "keep as is", "only after joins", "always" }, SelectFromClientPosition.values(), SelectFromClientPosition.OWN_LINE_AFTER_JOINS);

	private final ConfigValue[] configValues = new ConfigValue[] { configFirstTableNamePosition, configSelectJoinIndent, configFurtherTableNamePositions, configSelectOnPosition, configAlignAsAcrossJoins, configAlignOnAcrossJoins, configSelectClientPosition };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	@Override
	public boolean isConfigValueEnabled(ConfigValue configValue) {
		if (configValue == configAlignOnAcrossJoins) {
			return (SelectFromOnPosition.forValue(configSelectOnPosition.getValue()) == SelectFromOnPosition.CONTINUE); 
		} else {
			return true; 
		}
	}
	
	public AlignSelectFromRule(Profile profile) {
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
				try {
					if (query.isOneLiner()) {
						// skip one-liners
					} else if (executeOn(code, command, query)) {
						code.addRuleUse(this, command);
					}
				} catch (UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, e);
				}
				query = query.getNextQuery();
			}
		}	

		return false;
	}

	private boolean executeOn(Code code, Command command, SelectQuery query) throws UnexpectedSyntaxException {
		Term fromClause = query.getClause(SelectClause.FROM);
		if (fromClause == null) // pro forma
			return false;

		boolean continueAfterJoin = (SelectFromTablePosition.forValue(configFurtherTableNamePositions.getValue()) == SelectFromTablePosition.CONTINUE);
		
		Token fromToken = fromClause.firstToken;
		
		ArrayList<AlignTable> allTables = new ArrayList<>();
		ArrayList<AlignTable> tableOfLevel = new ArrayList<>();

		AlignTable table = new AlignTable(MAX_COLUMN_COUNT);
		allTables.add(table);
		tableOfLevel.add(table);

		// position the first table name, on which the first AlignTable is based
		Token firstTableNameStart = fromToken.getNextCodeToken();
		boolean changed = positionFirstTableName(fromToken, firstTableNameStart);

		// preliminarily set the first TABLE_NAME cell
		AlignLine line = table.addLine();
		Term firstTableName = Term.createForTokenRange(firstTableNameStart, firstTableNameStart.getNextSiblingWhileLevelOpener());
		line.setCell(Columns.TABLE_NAME.getValue(), new AlignCellTerm(firstTableName));
		
		Token token = firstTableNameStart;
		while (token != null) {
			boolean opensParens = token.textEquals("(");
			// {[INNER [cardinality]] JOIN}|{LEFT|RIGHT [OUTER [cardinality]] JOIN}|{CROSS JOIN} 
			boolean startsJoin = token.isAnyKeyword("LEFT", "RIGHT", "INNER", "CROSS", "JOIN");
			boolean startsAs = (token.isKeyword("AS") && token.getNextCodeSibling() != null && token.getNextCodeSibling().isIdentifier());
			boolean startsOn = token.isKeyword("ON");
			boolean startsClientHandling = token.matchesOnSiblings(true, "CLIENT", "SPECIFIED") 
												 || token.matchesOnSiblings(true, "USING", "CLIENT")
												 || token.matchesOnSiblings(true, "USING", "CLIENTS", "IN")
												 || token.matchesOnSiblings(true, "USING", "ALL", "CLIENTS");

			if (opensParens || startsAs || startsJoin || startsOn || startsClientHandling) {
				// expand the TABLE_NAME up to this point
				enhanceTableNameUpToToken(table, line, token);
			}
			
			if (startsJoin) {
				Token joinToken = token;
				while (joinToken != null && joinToken.isKeyword() && !joinToken.isKeyword("JOIN"))
					joinToken = joinToken.getNextCodeSibling();

				if (joinToken != null && joinToken.isKeyword("JOIN")) {
					line = table.addLine();
					int addIndent = calculateAddIndent(table, line, Columns.JOIN);
					
					Term term = Term.createForTokenRange(token, joinToken);
					changed |= term.condense();
					if (continueAfterJoin) {
						// only enter the JOIN cell to the table if the table name continues after it
						line.setCell(Columns.JOIN.getValue(), AlignCellTerm.createSpecial(term, addIndent, false));
					} else {
						// otherwise, just position the JOIN here and put the table name to the next line:
						changed |= token.setWhitespace(Math.max(token.lineBreaks, 1), table.getFirstToken().getStartIndexInLine() + addIndent);
					}
					token = joinToken;
					
					Token next = token.getNextCodeToken();
					
					// preliminarily set a TABLE_NAME
					addIndent = calculateAddIndent(table, line, Columns.TABLE_NAME);
					term = Term.createForTokenRange(next, next.getNextSiblingWhileLevelOpener());
					line.setCell(Columns.TABLE_NAME.getValue(), AlignCellTerm.createSpecial(term, addIndent, false));

					if (SelectFromTablePosition.forValue(configFurtherTableNamePositions.getValue()) == SelectFromTablePosition.CONTINUE) {
						changed |= !next.getPrev().isComment() && next.setWhitespace();
					}
				}
				
			} else if (startsAs) {
				if (line.getCell(Columns.AS.getValue()) != null) // pro forma
					line = table.addLine();
				line.setCell(Columns.AS.getValue(), new AlignCellTerm(Term.createForTokenRange(token, token.getNextCodeSibling())));

			} else if (startsOn) {
				// find the (possibly previous) AlignLine which this ON clause belongs to (but keep 'line' unchanged)
				AlignLine lineForOn = null;
				for (int i = table.getLineCount() - 1; i > 0; --i) {
					if (table.getLine(i).getCell(Columns.ON.getValue()) == null) {
						lineForOn = table.getLine(i);
						break;
					}
				}
				if (lineForOn == null) // pro forma
					lineForOn = table.addLine();

				Token lastInLogExpr = token.getLastTokenOfLogicalExpression();
				if (lastInLogExpr != null) {
					int addIndent = calculateAddIndent(table, lineForOn, Columns.ON);
					lineForOn.setCell(Columns.ON.getValue(), AlignCellTerm.createSpecial(Term.createForTokenRange(token, lastInLogExpr), addIndent, false));
					token = lastInLogExpr;
				}

			} else if (startsClientHandling) {
				SelectFromClientPosition clientHandlingPos = SelectFromClientPosition.forValue(configSelectClientPosition.getValue());
				if (clientHandlingPos != SelectFromClientPosition.KEEP_AS_IS) {
					boolean ownLine = (clientHandlingPos == SelectFromClientPosition.OWN_LINE)
									   || (clientHandlingPos == SelectFromClientPosition.OWN_LINE_AFTER_JOINS) && table.getLineCount() > 1;
					if (ownLine) {
						changed |= token.setWhitespace(Math.max(token.lineBreaks, 1), table.getFirstToken().getStartIndexInLine());
					} else {
						changed |= !token.getPrev().isComment() && token.setWhitespace();
					}
				}
				break;
			}
			
			if (token == fromClause.lastToken)
				break;
			
			// do NOT move into dynamic cases like 'FROM (from)'
			boolean moveInsideNewLevel = token.textEquals("(") && token.hasChildren() && !token.getNext().isAttached();
			
			if (token.getOpensLevel() && !moveInsideNewLevel) {
				// skip content in parentheses, esp. 'HIERARCHY_...('
				token = token.getNextSiblingWhileLevelOpener();

			} else if (moveInsideNewLevel) {
				// move into one or several parentheses, adding a new(!) table on each level
				do {
					enhanceTableNameWithParens(table, line, token);
					
					// start a new AlignTable for the content of the new parenthesis
					table = new AlignTable(MAX_COLUMN_COUNT);
					allTables.add(table);
					tableOfLevel.add(table);
					
					// preliminarily set the TABLE_NAME cell, leaving the JOIN column empty
					token = token.getNextCodeToken();
					Token tableNameStart = token;
					line = table.addLine();
					Term prelimTableName = Term.createForTokenRange(tableNameStart, tableNameStart.getNextSiblingWhileLevelOpener());
					line.setCell(Columns.TABLE_NAME.getValue(), new AlignCellTerm(prelimTableName));
				} while (token.textEquals("(") && token.hasChildren() && !token.getNext().isAttached());
				
			} else if (token.getNextCodeSibling() == null) {
				// move out of a parenthesis, going back to the table of the previous level
				token = token.getNextCodeToken();
				do {
					if (tableOfLevel.size() > 1) {
						tableOfLevel.remove(tableOfLevel.size() - 1);
					}
					table = tableOfLevel.get(tableOfLevel.size() - 1);
					line = table.getLastLine();
					
					if (token == fromClause.lastToken)
						break;
					token = token.getNextCodeToken();
				} while (token.textEquals(")"));

				if (token == fromClause.lastToken)
					break;

			} else {
				token = token.getNextCodeSibling();
			}
		}

		alignAllTables(code, continueAfterJoin, allTables);
		return changed;
	}

	private boolean positionFirstTableName(Token fromToken, Token tableNameStart) {
		SelectFromTablePosition firstTableNamePos = SelectFromTablePosition.forValue(configFirstTableNamePosition.getValue());
		int lineBreaks;
		int spacesLeft;
		if (firstTableNamePos != SelectFromTablePosition.CONTINUE) {
			lineBreaks = 1;
			spacesLeft = fromToken.getStartIndexInLine() + firstTableNamePos.getAddIndent();
		} else if (tableNameStart.getPrev().isComment()) {
			lineBreaks = 1;
			spacesLeft = fromToken.getEndIndexInLine() + 1;
		} else {
			lineBreaks = 0;
			spacesLeft = 1;
		}
		return tableNameStart.setWhitespace(lineBreaks, spacesLeft);
	}

	private int calculateAddIndent(AlignTable table, AlignLine line, Columns column) {
		SelectFromTablePosition furtherTableNamePos = SelectFromTablePosition.forValue(configFurtherTableNamePositions.getValue());
		SelectFromJoinIndent joinIndent = SelectFromJoinIndent.forValue(configSelectJoinIndent.getValue());
		SelectFromOnPosition onPos = SelectFromOnPosition.forValue(configSelectOnPosition.getValue());

		// determine how much the current column is indented, based on the start position of the table's start Token, 
		// which is either the 'FROM' keyword or the opening '(' 
		int addPerJoin = joinIndent.getAddIndent();
		int addPerTableName = (furtherTableNamePos == SelectFromTablePosition.CONTINUE) ? 0 : furtherTableNamePos.getAddIndent();
		int furtherTableCount = (line == table.getLastLine()) ? table.getLineCount() - 1 : table.getIndexOfLine(line);
		int tableNameIndentOfLine = furtherTableCount * (addPerJoin + addPerTableName);
		
		if (column == Columns.JOIN) {
			return (table.getLineCount() == 1) ? 0 : tableNameIndentOfLine - addPerTableName;

		} else if (column == Columns.TABLE_NAME) {
			return (furtherTableNamePos == SelectFromTablePosition.CONTINUE) ? 0 : tableNameIndentOfLine;
			
		} else if (column == Columns.ON) {
			if (onPos == SelectFromOnPosition.CONTINUE)
				return 0;
			
			int add = 0; // .BELOW_JOIN
			if (onPos == SelectFromOnPosition.BELOW_JOIN_PLUS_2) {
				add = 2;
			} else if (onPos == SelectFromOnPosition.BELOW_JOIN_WORD_2) {
				AlignCell joinCell = line.getCell(Columns.JOIN.getValue());
				add = (joinCell == null) ? 0 : joinCell.getFirstToken().getTextLength() + 1;
			}
			return tableNameIndentOfLine + add;
			
		} else { // pro forma - this method should not be called for .AS, which is always put behind the table name
			throw new IllegalArgumentException();
		}
	}

	private void enhanceTableNameUpToToken(AlignTable table, AlignLine line, Token token) throws UnexpectedSyntaxException {
		if (areAsAndOnCellsEmpty(line)) {
			AlignCell tableNameCell = line.getCell(Columns.TABLE_NAME.getValue());
			if (tableNameCell.getFirstToken() != token && tableNameCell.getLastToken() != token.getPrevCodeToken()) {
				Term updatedTableName = Term.createForTokenRange(tableNameCell.getFirstToken(), token.getPrevCodeToken());
				int addIndent = calculateAddIndent(table, line, Columns.TABLE_NAME);
				line.overwriteCell(Columns.TABLE_NAME.getValue(), AlignCellTerm.createSpecial(updatedTableName, addIndent, false));
			}
		}
	}

	private void enhanceTableNameWithParens(AlignTable table, AlignLine line, Token openingparenthesis) throws UnexpectedSyntaxException {
		if (areAsAndOnCellsEmpty(line)) {
			// put the whole parenthesis as the TABLE_NAME cell (however, override width with 1)
			Token tableNameStart = line.getCell(Columns.TABLE_NAME.getValue()).getFirstToken();
			Term updatedTableName = Term.createForTokenRange(tableNameStart, openingparenthesis.getNextSiblingWhileLevelOpener());
			int addIndent = calculateAddIndent(table, line, Columns.TABLE_NAME);
			line.overwriteCell(Columns.TABLE_NAME.getValue(), AlignCellTerm.createSpecial(updatedTableName, addIndent, true));
		}
	}
	
	private boolean areAsAndOnCellsEmpty(AlignLine line) {
		return (line.getCell(Columns.AS.getValue()) == null && line.getCell(Columns.ON.getValue()) == null);
	}
	
	private void alignAllTables(Code code, boolean continueAfterJoin, ArrayList<AlignTable> allTables) throws UnexpectedSyntaxException {
		for (AlignTable alignTable : allTables) {
			AlignColumn joinColumn = alignTable.getColumn(Columns.JOIN.getValue());
			AlignColumn tableNameColumn = alignTable.getColumn(Columns.TABLE_NAME.getValue());
			AlignColumn asColumn = alignTable.getColumn(Columns.AS.getValue());
			AlignColumn onColumn = alignTable.getColumn(Columns.ON.getValue());

			if (continueAfterJoin) {
				Command[] changedCommands = tableNameColumn.joinIntoPreviousColumns(true, true);
				code.addRuleUses(this, changedCommands);
			} // otherwise, the JOIN column was NOT filled above, therefore nothing else to do 
			
			// merge columns that shall NOT be aligned
			if (!configAlignAsAcrossJoins.getValue() || asColumn.getCellCount() < 2) {
				Command[] changedCommands = asColumn.joinIntoPreviousColumns(true, true);
				code.addRuleUses(this, changedCommands);
			}
			if (SelectFromOnPosition.forValue(configSelectOnPosition.getValue()) == SelectFromOnPosition.CONTINUE) {
				if (!configAlignOnAcrossJoins.getValue()) {
					Command[] changedCommands = onColumn.joinIntoPreviousColumns(true, true);
					code.addRuleUses(this, changedCommands);
				}
			} else {
				asColumn.setForceLineBreakAfter(false);
				onColumn.setForceIndent(joinColumn, 0);
			}

			Token firstToken = alignTable.getFirstToken();
			Command[] changedCommands = alignTable.align(firstToken.getStartIndexInLine(), firstToken.lineBreaks, true, false, false);
			for (Command changedCommand : changedCommands) {
				code.addRuleUse(this, changedCommand);
			}
		}
	}
}
