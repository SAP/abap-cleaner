package com.sap.adt.abapcleaner.rules.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForTokens;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;

public class SpaceAroundCommentSignRule extends RuleForTokens {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.SPACE_AROUND_COMMENT_SIGN; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SPACES; }

	@Override
	public String getDisplayName() { return "Put spaces around \" comment sign"; }

	@Override
	public String getDescription() { return "Ensures that there is at least one space before and after the \" comment sign."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2021, 1, 14); } // the original SpaceAfterCommentSignRule had LocalDate.of(2021, 1, 3)

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
			+ LINE_SEP + "  METHOD space_around_comment_sign." 
			+ LINE_SEP + "    \"Comment signs" 
			+ LINE_SEP + "    \"are NOT the same as \"quotation marks\"," 
			+ LINE_SEP + "    \"so it looks much better" 
			+ LINE_SEP + "    \"to put a space between the \" and the text." 
			+ LINE_SEP 
			+ LINE_SEP + "    CLEAR ev_result.  \"the same is true at line end" 
			+ LINE_SEP 
			+ LINE_SEP + "    lv_value = 0.\"comment" 
			+ LINE_SEP 
			+ LINE_SEP + "    ls_pair = VALUE #(\" initial comment" 
			+ LINE_SEP + "                       a = '3.1415'\" pi" 
			+ LINE_SEP + "                       b = '1.4142'\" sqrt(2)" 
			+ LINE_SEP + "                      ).\"final comment" 
			+ LINE_SEP + "  ENDMETHOD.";
   }

	public final ConfigBoolValue configSpaceBeforeCommentSign = new ConfigBoolValue(this, "SpaceBeforeCommentSign", "Separate code and \" comment with a space", true);
	public final ConfigBoolValue configSpaceAfterCommentSign = new ConfigBoolValue(this, "SpaceAfterCommentSign", "Start \" comment with space", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configSpaceBeforeCommentSign, configSpaceAfterCommentSign };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public SpaceAroundCommentSignRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@Override
	protected boolean executeOn(Code code, Command command, Token token, int releaseRestriction) {
		final String commentSign = ABAP.COMMENT_SIGN_STRING;
		boolean changed = false;
		
		// ensure space before comment sign
		if (token.isQuotMarkComment() && !token.isFirstTokenInLine() && token.spacesLeft == 0 
				&& configSpaceBeforeCommentSign.getValue()) {
			++token.spacesLeft;
			changed = true;
		} 
		
		// ensure space after comment sign
		// do not change comments that start with "# (ABAP.pseudoCommentSign), "! (ABAP.dynamicHelpCommentSign), "" (commented-out comment)
		if (token.isQuotMarkComment() && token.getTextLength() > commentSign.length() && Character.isLetterOrDigit(token.getText().charAt(1))
				&& configSpaceAfterCommentSign.getValue()) {
			token.setText(commentSign + " " + token.getText().substring(commentSign.length()), false);
			changed = true;
		} 

		return changed;
	}
}
