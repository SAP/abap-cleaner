package com.sap.adt.abapcleaner.rulehelpers;

import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;

public abstract class RuleForDdlAnnotationScopes extends Rule {
	protected abstract void executeOn(Code code, DdlAnnotationScope scope) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException;

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_ANNOTATIONS; }

	@Override
	public Language[] getSupportedLanguages() { return ddlOrDcl; }
	
	protected RuleForDdlAnnotationScopes(Profile profile) {
		super(profile);
	}

	@Override
	protected void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges {
		final boolean allowCommentsInAnnotations = false; // not yet supported
		
		Command command = code.firstCommand;
		DdlAnnotationScope scope = new DdlAnnotationScope(allowCommentsInAnnotations );

		boolean skipSection = false;
		while (command != null) {
			if (!command.isDdlOrDcl() || command.isCommentLine()) {
				// skip Command
			
			} else if (!command.isDdlAnnotationBeforeListElement()) { 
				// process this annotation section, then start a new section
				if (!skipSection && !scope.isEmpty()) {
					executeOn(code, scope);
				}
				scope = new DdlAnnotationScope(allowCommentsInAnnotations );
				skipSection = false;
			
			} else if (isCommandBlocked(command)) {
				skipSection = true;

			} else if (!skipSection) {
				try {
					scope.add(command);
				} catch (UnexpectedSyntaxBeforeChanges e) {
					// this scope cannot be processed, e.g. due to a comment within an annotation Command
					skipSection = true;
				}
			}			
			command = command.getNext();
		}
		if (!skipSection && !scope.isEmpty()) {
			executeOn(code, scope);
		}
	}
}
