package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;
import com.sap.adt.abapcleaner.rulehelpers.*;

/**
 * Replaces * comments with " comments, if the comment contains English or German text (rather than ABAP code).
 * If consecutive lines have different indents, the left-most text gets the new spacesLeft = 1, the others keep relative indents.
 * To distinguish text comments from commented-out code, the CommentIdentifier is used.
 */
public class CommentTypeRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] { 
			new RuleReference(RuleSource.ABAP_STYLE_GUIDE, "Comment with \", not with *", "#comment-with--not-with-"), 
			new RuleReference(RuleSource.CODE_PAL_FOR_ABAP, "Comment Type", "comment-type.md") };

	@Override
	public RuleID getID() { return RuleID.COMMENT_TYPE; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Comment with \", not with * (for text)"; }

	@Override
	public String getDescription() {
		return "Replaces * comments with \" comments, if the comment contains English text (rather than ABAP code). If consecutive lines have different indents, relative indents are kept.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 5); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public RuleID[] getDependentRules() { return new RuleID[] { RuleID.INSET } ; }

	@Override
	public boolean isEssential() { return true; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD comment_type_for_text." 
			+ LINE_SEP + "*****************************************************************" 
			+ LINE_SEP + "* this check automatically distinguishes between " 
			+ LINE_SEP + "*   a) text comments, which will be transformed into \" comments" 
			+ LINE_SEP + "*   b) commented-out code lines, which will be kept as * comments" 
			+ LINE_SEP + "*****************************************************************" 
			+ LINE_SEP 
			+ LINE_SEP + "* ------ 'given' ----------------" 
			+ LINE_SEP + "* create a contract with two fulfillments " 
			+ LINE_SEP + "    lo_contract_crt = create_contract( )." 
			+ LINE_SEP 
			+ LINE_SEP + "*    \" fulfill the contract" 
			+ LINE_SEP + "*    lo_contract_ff1 = fulfill( io_contract           = lo_contract_crt" 
			+ LINE_SEP + "*                               iv_fulfillment_number = lc_fulfill_num_1 )." 
			+ LINE_SEP + "*    lo_contract_ff2 = fulfill( io_contract           = lo_contract_ff1" 
			+ LINE_SEP + "*                               iv_fulfillment_number = lc_fulfill_num_2 )." 
			+ LINE_SEP 
			+ LINE_SEP + "* ------ 'when' ----------------" 
			+ LINE_SEP + "* perform contract change" 
			+ LINE_SEP + "    lo_contract_chg = change_contract( io_contract               = lo_contract_ff2" 
			+ LINE_SEP + "                                       iv_contract_change_number = lc_contract_change_num )." 
			+ LINE_SEP 
			+ LINE_SEP + "* ------ 'then' ----------------" 
			+ LINE_SEP + "* set expected values " 
			+ LINE_SEP + "    lo_contract_exp = lo_contract_ff2." 
			+ LINE_SEP + "    lo_contract_act = lo_contract_chg." 
			+ LINE_SEP 
			+ LINE_SEP + "*    lo_contract_exp->assert_equals_fully( io_act_contract = lo_contract_act )." 
			+ LINE_SEP 
			+ LINE_SEP + "*    \" sometimes, the asterisk is also used to separate code sections" 
			+ LINE_SEP + "***  leading asterisks" 
			+ LINE_SEP + "*    trailing asterisks ******************************" 
			+ LINE_SEP + "**** leading and trailing asterisks ******************" 
			+ LINE_SEP + "******************************************************" 
			+ LINE_SEP + "  ENDMETHOD.";
   }

   final ConfigEnumValue<CommentSeparatorAction> configActionForLeadingAsterisks = new ConfigEnumValue<CommentSeparatorAction>(this, "MeasureForLeadingAsterisks", "Leading *** separators:", new String[] { "keep", "convert to ---", "convert to ===", "remove" }, CommentSeparatorAction.values(), CommentSeparatorAction.DELETE);
	final ConfigEnumValue<CommentSeparatorAction> configActionForTrailingAsterisks = new ConfigEnumValue<CommentSeparatorAction>(this, "MeasureForTrailingAsterisks", "Trailing *** separators:", new String[] { "keep", "convert to ---", "convert to ===", "remove" }, CommentSeparatorAction.values(), CommentSeparatorAction.CONVERT_TO_HYPHEN);

	private final ConfigValue[] configValues = new ConfigValue[] { configActionForLeadingAsterisks, configActionForTrailingAsterisks };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CommentTypeRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) {
		if (code == null)
			throw new NullPointerException("code");

		CommentIdentifier identifier = new CommentIdentifier();

		CommentSeparatorAction actionForLeadingAsterisks = CommentSeparatorAction.forValue(configActionForLeadingAsterisks.getValue());
		CommentSeparatorAction actionForTrailingAsterisks = CommentSeparatorAction.forValue(configActionForTrailingAsterisks.getValue());

		Command command = code.firstCommand;
		while (command != null) {
			commandForErrorMsg = command;

			// find next * comment section
			while (command != null && !command.isAsteriskCommentLine())
				command = command.getNext();
			if (command == null)
				break;
			if (isCommandBlocked(command)) {
				command = command.getNext();
				continue;
			}

			// find end of * comment section while analyzing the comment lines up to that point
			Command startCommand = command;
			int codeLineCount = 0;
			int textLineCount = 0;
			int autoGeneratedCount = 0;
			int minIndent = Integer.MAX_VALUE;
			String lastComment = null;
			String curComment = command.getFirstToken().getText();
			while (command != null && command.isAsteriskCommentLine() && !command.getChangeControl().isRuleBlocked(getID())) {
				Command nextCommand = command.getNext();
				String nextComment = (nextCommand != null && nextCommand.isAsteriskCommentLine()) ? nextCommand.getFirstToken().getText() : null;
				CommentIdentification commentIdentification = identifier.identifyComment(curComment, false, lastComment, nextComment, false, Language.ABAP);
				if (commentIdentification.indent >= 0)
					minIndent = Math.min(minIndent, commentIdentification.indent);
				if (commentIdentification.isCode())
					++codeLineCount;
				else
					++textLineCount;
				if (commentIdentification.isAutoGenerated)
					++autoGeneratedCount;

				lastComment = curComment;
				curComment = nextComment;
				command = nextCommand;
				commandForErrorMsg = command;
				if (command == null || command.getFirstTokenLineBreaks() >= 2)
					break;
			}
			if (minIndent == Integer.MAX_VALUE)
				minIndent = 0;

			// if the majority of lines was identified as ABAP code (rather than English text), 
			// or if at least three lines seem to be auto-generated comments, then leave the whole section untouched
			if (codeLineCount >= textLineCount || autoGeneratedCount >= 3)
				continue;

			// ... otherwise the whole section is considered to be text and is transformed
			Command changeCommand = startCommand;
			while (changeCommand != command) {
				commandForErrorMsg = changeCommand;
				String text = changeCommand.getFirstToken().getText();
				if (minIndent > 0) {
					// apart from the initial *, the first (minIndent) chars of text will either be spaces, or separator chars like "****", "====", or "----"
					String prefix = (text.length() <= 1) ? "" : text.substring(1, Math.min(minIndent, text.length() - 1));
					String mainText = (minIndent >= text.length()) ? "" : text.substring(minIndent);
					prefix = prefix.trim();
					if (StringUtil.isNullOrEmpty(prefix) || StringUtil.isNullOrEmpty(mainText) || prefix.charAt(prefix.length() - 1) == mainText.charAt(0))
						text = prefix + mainText;
					else // e.g. "*** comment"
						text = prefix + " " + mainText;
				} else {
					text = text.substring(1);
				}

				// replace leading / trailing *** asterisks
				if (actionForLeadingAsterisks != CommentSeparatorAction.KEEP && text.startsWith(ABAP.LINE_COMMENT_SIGN_STRING)) {
					String textTemp = StringUtil.trimStart(text, ABAP.LINE_COMMENT_SIGN);
					if (!StringUtil.isNullOrEmpty(textTemp.trim())) {
						String newSep = createSeparator(text.length() - textTemp.length(), actionForLeadingAsterisks);
						text = StringUtil.isNullOrEmpty(newSep) ? StringUtil.trimStart(textTemp, ' ') : newSep + textTemp;
					}
				}
				if (actionForTrailingAsterisks != CommentSeparatorAction.KEEP && text.endsWith(ABAP.LINE_COMMENT_SIGN_STRING)) {
					String textTemp = StringUtil.trimEnd(text, ABAP.LINE_COMMENT_SIGN);
					String newSep = createSeparator(text.length() - textTemp.length(), actionForTrailingAsterisks);
					text = StringUtil.isNullOrEmpty(newSep) ? StringUtil.trimEnd(textTemp, ' ') : textTemp + newSep;
				}

				if (text.startsWith(ABAP.COMMENT_SIGN_STRING)) // avoid results with double comment signs: " " comment
					text = StringUtil.trimStart(text.substring(ABAP.COMMENT_SIGN_STRING.length()));

				// determine the indent; if the code is not properly aligned (e.g. because IndentRule was not (yet) applied),
				// then "misalign" the comment as well, depending on the indent of the parent Command
				int correctIndent = changeCommand.getIndent();
				Command parent = changeCommand.getParent();
				int misalignment = (parent == null || parent.getFirstToken() == null) ? 0 : parent.getFirstToken().spacesLeft - parent.getIndent();

				changeCommand.getFirstToken().spacesLeft = correctIndent + misalignment;
				changeCommand.getFirstToken().setText(ABAP.COMMENT_SIGN_STRING + " " + text, false);
				code.addRuleUse(this, changeCommand);

				changeCommand = changeCommand.getNext();
			}

		}
	}

	private static String createSeparator(int length, CommentSeparatorAction actionForLeadingAsterisks) {
		switch (actionForLeadingAsterisks) {
			case KEEP:
				return StringUtil.repeatChar(ABAP.LINE_COMMENT_SIGN, length);
			case CONVERT_TO_HYPHEN:
				return StringUtil.repeatChar('-', length);
			case CONVERT_TO_EQUALS:
				return StringUtil.repeatChar('=', length);
			case DELETE:
				return "";
			default:
				throw new IndexOutOfBoundsException("unknown separator action");
		}
	}
}
