package com.sap.adt.abapcleaner.rules.ddl.position;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulehelpers.RuleForDdlPosition;

public abstract class RuleForDdlPositionJoinOrAssociation extends RuleForDdlPosition {
	protected abstract boolean executeOn(Code code, Command command, Token firstCode);

	protected final String[] conditionLineBreakSelection = new String[] { "Always", "If view contains multi-line condition", "Keep as is", "Never" };

	private boolean viewContainsMultiLineJoinCond;
	private boolean viewContainsMultiLineAssociationCond;
	protected int joinCount;
	protected int associationCount;
	
	protected RuleForDdlPositionJoinOrAssociation(Profile profile) {
		super(profile);
	}

	@Override
	protected void prepare(Code code) {
		Command command = code.firstCommand;

		// reset the JOIN / ASSOCIATION count; the first JOIN / ASSOCIATION will always get an empty line above
		joinCount = 0;
		associationCount = 0;

		// determine whether the view contains a multi-line JOIN / ASSOCIATION condition, or if one of the data sources has parameters
		viewContainsMultiLineJoinCond = false;
		viewContainsMultiLineAssociationCond = false;
		while (command != null) {
			Token firstCode = command.getFirstCodeToken();
			if (command.isDdl() && !command.isCommentLine() && firstCode != null) {
				boolean isJoin = firstCode.startsDdlJoin();
				boolean isAssociation = firstCode.startsDdlAssociation();
				if (isJoin || isAssociation) {
					Token dataSourceToken = getJoinOrAssociationDataSource(command);
					Token onToken = firstCode.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "ON");
					if (dataSourceToken != null && onToken != null) {
						boolean hasParameters = dataSourceToken.getNextCodeSibling().textEquals(DDL.PARENS_OPEN_STRING);
						Token lastTokenOfLogExpr = onToken.getLastTokenOfDdlLogicalExpression();
						if (hasParameters || command.containsLineBreaksBetween(onToken, lastTokenOfLogExpr, false)) {
							if (isJoin) {
								viewContainsMultiLineJoinCond = true;
							}
							if (isAssociation) {
								viewContainsMultiLineAssociationCond = true;
							}
						}
					}
				}
			}			
			command = command.getNextSibling(); // JOINs and ASSOCIATIONs are only found on top level
		}
	}
	
	protected DdlLineBreak getLineBreakForCondition(DdlConditionLineBreak conditionLineBreak, boolean isJoin) {
		switch (conditionLineBreak) {
			case ALWAYS:
				return DdlLineBreak.ALWAYS;
			case IF_MULTI_LINE_FOUND:
				boolean containsMultiLineCond = isJoin ? viewContainsMultiLineJoinCond : viewContainsMultiLineAssociationCond;
				return containsMultiLineCond ? DdlLineBreak.ALWAYS : DdlLineBreak.NEVER;
			case KEEP_AS_IS:
				return DdlLineBreak.KEEP_AS_IS;
			case NEVER:
				return DdlLineBreak.NEVER;
			default: // pro forma
				return DdlLineBreak.ALWAYS;
		}
	}

	@Override
	protected boolean executeOn(Code code, Command command) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		if (command.getParent() != null)
			return false;

		Token firstCode = command.getFirstCodeToken();
		if (firstCode == null) { // pro forma
			return false;
		} else if (firstCode.textEquals(DDL.BRACE_OPEN_STRING)) {
			// reset JOIN / ASSOCIATION count in case the select list is followed by a UNION etc.
			joinCount = 0;
			associationCount = 0;
			return false;
		}

		// continue on specific implementation in DdlPositionJoinRule or DdlPositionAssociationRule 
		return executeOn(code, command, firstCode);
	}		
	
	private Token getJoinOrAssociationDataSource(Command command) {
		// find the data source by skipping all JOIN / ASSOCIATION keywords and (cardinality) brackets,
		// e.g. "EXACT ONE TO EXACT ONE JOIN" or "ASSOCIATION [1..1] TO"
		Token keywordEnd = command.getFirstCodeToken();
		while (keywordEnd != null && (keywordEnd.isKeyword() || keywordEnd.textEqualsAny(DDL.BRACKET_OPEN_STRING, DDL.BRACKET_CLOSE_STRING))) {
			keywordEnd = keywordEnd.getNextCodeSibling();
		}
		return keywordEnd;
	}
	
	protected boolean executeOn(Code code, Command command, Token firstCode, boolean isJoin, boolean isFirst,
			DdlLineBreak breakBeforeKeywords, int indentKeywords, DdlLineBreak breakBeforeDataSource, int indentDataSource,
			DdlLineBreak breakBeforeCondition, int indentCondition, DdlLineBreak breakBeforeFilter, int indentFilter) {
		
		Token dataSourceToken = getJoinOrAssociationDataSource(command);
		Token lastKeyword = dataSourceToken.getPrevCodeSibling();
		
		// find the JOIN condition (optional) or the ASSOCIATION condition (mandatory)
		Token onToken = dataSourceToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "ON");
		int oldOnTokenStartIndex = (onToken == null) ? 0 : onToken.getStartIndexInLine();

		// find the ASSOCIATION filter (optional)
		Token filterKeywordsFirst = null;
		Token filterKeywordsLast = null;
		int oldFilterTokenStartIndex = 0;
		if (!isJoin && onToken != null) { // "onToken != null" pro forma 
			filterKeywordsLast = onToken.getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "WITH", "DEFAULT", "FILTER");
			if (filterKeywordsLast != null) {
				oldFilterTokenStartIndex = filterKeywordsLast.getStartIndexInLine(); 
				filterKeywordsFirst = filterKeywordsLast.getPrevCodeSibling().getPrevCodeSibling();
			}
		}
		
		// break before JOIN / ASSOCIATION keywords, e.g. "EXACT ONE TO EXACT ONE JOIN" or "ASSOCIATION [1..1] TO"
		boolean changed = false;
		changed |= breakBefore(firstCode, breakBeforeKeywords, isFirst, indentKeywords);
		changed |= condense(firstCode, lastKeyword);
		
		// break before JOIN / ASSOCIATION data source
		changed |= breakBefore(dataSourceToken, breakBeforeDataSource, false, indentDataSource);
		
		// break before JOIN / ASSOCIATION condition
		if (onToken != null) {
			changed |= breakBefore(onToken, breakBeforeCondition, false, indentCondition);
			
			// move the rest of the JOIN / ASSOCIATION condition like the ON keyword was moved
			int addIndent = onToken.getStartIndexInLine() - oldOnTokenStartIndex;
			changed |= command.addIndent(addIndent, 0, onToken.getNext(), filterKeywordsFirst, false);
		}
		
		// break before ASSOCIATION filter
		if (filterKeywordsFirst != null) {
			changed |= breakBefore(filterKeywordsFirst, breakBeforeFilter, false, indentFilter);
			changed |= condense(filterKeywordsFirst, filterKeywordsLast);

			// move the rest of the filter condition like the FILTER keyword was moved
			int addIndent = filterKeywordsLast.getStartIndexInLine() - oldFilterTokenStartIndex;
			changed |= command.addIndent(addIndent, 0, filterKeywordsLast.getNext(), null, false);
		}
		
		return changed;
	}
}
