package com.sap.adt.abapcleaner.rules.ddl.spaces;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.DDL;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxBeforeChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDdlCommands;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentification;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentifier;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotation;
import com.sap.adt.abapcleaner.rulehelpers.DdlAnnotationScope;

public class DdlTypoRule extends RuleForDdlCommands {
	@Override
	public RuleID getID() { return RuleID.DDL_TYPO; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.DDL_SPACES; }

	@Override
	public String getDisplayName() { return "Correct frequent typos in DDL comments"; }

	@Override
	public String getDescription() { return "Corrects frequent typos in DDL comments and adds warnings about typos in annotation values. Only considers typos with unambiguous correction."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 9, 12); }

	@Override
	public String getExample() {
		return "" 
				+ LINE_SEP + "// no authorisation check requried"
				+ LINE_SEP + "@AccessControl.authorizationCheck: #NOT_REQUIRED"
				+ LINE_SEP + ""
				+ LINE_SEP + "@EndUserText.label: 'main dcoument imformation'"
				+ LINE_SEP + ""
				+ LINE_SEP + "@Metadata.allowExtensions: true"
				+ LINE_SEP + ""
				+ LINE_SEP + "// to fix typos in references, the element itself must be renamed, too"
				+ LINE_SEP + "@ObjectModel.semanticKey: [ 'Mesage', 'Valididty' ]"
				+ LINE_SEP + ""
				+ LINE_SEP + "define view C_AnyView"
				+ LINE_SEP + "  as select from I_AnyDocument as Doc"
				+ LINE_SEP + ""
				+ LINE_SEP + "  // assocation to coresponding other view"
				+ LINE_SEP + "  association[1..* ] to I_OtherView as _OtherAlias"
				+ LINE_SEP + "    on Doc.DocumentId = _OtherAlias.DocumentId"
				+ LINE_SEP + ""
				+ LINE_SEP + "{"
				+ LINE_SEP + "      // docuent ID"
				+ LINE_SEP + "  key Doc.DocumentId,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      -- docuemnt name and attibutes"
				+ LINE_SEP + "      Doc.DocumentName,"
				+ LINE_SEP + "      Doc.Mesage,"
				+ LINE_SEP + "      Doc.Valididty,"
				+ LINE_SEP + "      Doc.CreatedOn,"
				+ LINE_SEP + "      Doc.CreatedBy,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      // caculate total ammount"
				+ LINE_SEP + "      (Doc.InitialAmount + Doc.DeltaAmount) as TotalAmount,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      /* to optimise perfomance, only expose small selction of"
				+ LINE_SEP + "         fields neccessary for the applicaton; futher detials"
				+ LINE_SEP + "         could be made avaialable separatly via extention */"
				+ LINE_SEP + "      Doc.AnyField,"
				+ LINE_SEP + "      Doc.OtherField,"
				+ LINE_SEP + ""
				+ LINE_SEP + "      _OtherAlias"
				+ LINE_SEP + "}";
	}
	
	final ConfigBoolValue configCorrectTypos = new ConfigBoolValue(this, "CorrectTypos", "Correct frequent typos", true);
	final ConfigBoolValue configConvertBritishToAmerican = new ConfigBoolValue(this, "ConvertBritishToAmerican", "Change from British English to American English", true);
	
	final ConfigBoolValue configProcessComments = new ConfigBoolValue(this, "ProcessComments", "Apply on comments", true);
	final ConfigBoolValue configProcessAnnotations = new ConfigBoolValue(this, "ProcessAnnotations", "Add TODO on annotation values", true); 
	final ConfigBoolValue configProcessAnnotationRefs = new ConfigBoolValue(this, "ProcessAnnotationRefs", "Add TODO on annotation references", false); 

	private final ConfigValue[] configValues = new ConfigValue[] { configCorrectTypos, configConvertBritishToAmerican, configProcessComments, configProcessAnnotations, configProcessAnnotationRefs };
	
	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	private final String TODO_PREFIX = "TODO: ";
	private final String TODO_SUFFIX = " (" + Program.PRODUCT_NAME + ")";
	
	public DdlTypoRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	// -------------------------------------------------------------------------
	
	@Override
	protected boolean executeOn(Code code, Command command, int releaseRestriction) throws UnexpectedSyntaxBeforeChanges, UnexpectedSyntaxAfterChanges, IntegrityBrokenException {
		boolean changed = false;

		if (!configCorrectTypos.getValue() && !configConvertBritishToAmerican.getValue())
			return false;
		
		CommentIdentifier identifier = new CommentIdentifier();

		if (command.isDdlAnnotation()) {
			// process annotation values
			if (configProcessAnnotations.getValue() || configProcessAnnotationRefs.getValue()) {
				changed |= correctTyposInAnnotations(code, command, identifier); 
			}
			
		} else if (configProcessComments.getValue()) {
			// process comments - which may also be comment Tokens inside of a non-comment Command
			Token token = command.getFirstToken();
			while (token != null) {
				if (token.isComment() && !token.isCommentedOutDdlAnnotation()) 
					changed |= correctTyposInComment(command, token, identifier); 
				token = token.getNext();
			}
		}
		return changed;
	}

	private boolean correctTyposInComment(Command command, Token token, CommentIdentifier identifier) {
		final boolean correctTypos = configCorrectTypos.getValue();
		final boolean convertBritishToAmerican = configConvertBritishToAmerican.getValue();
		
		// do NOT process the TODO comments which were generated by an earlier run of this rule, 
		// because they contain typos, too!
		if (token.textStartsWith(DDL.LINE_END_COMMENT + " " + TODO_PREFIX) && token.textEndsWith(TODO_SUFFIX)) {
			return false;
		}

		String tokenText = token.getText();
		CommentIdentification identification = identifier.identifyComment(tokenText, true, Language.DDL);
		if (!identification.isEnglish()) 
			return false;

		CommentIdentifier.CorrectionResult result = identifier.correctComment(tokenText, correctTypos, convertBritishToAmerican);
		if (result == null || !result.wasAnythingCorrected()) 
			return false;

		token.setText(result.correctedText, false);
		return true;
	}
	
	private boolean correctTyposInAnnotations(Code code, Command command, CommentIdentifier identifier) throws UnexpectedSyntaxAfterChanges {
		final boolean correctTypos = configCorrectTypos.getValue();
		final boolean convertBritishToAmerican = configConvertBritishToAmerican.getValue();

		// create an annotation 'scope' from the current Command
		DdlAnnotationScope scope = new DdlAnnotationScope(true);
		try {
			scope.add(command);
		} catch (UnexpectedSyntaxBeforeChanges e) {
			// this scope cannot be processed, e.g. due to a comment within an annotation Command
			return false;
		}
		
		// find annotations with values 
		for (DdlAnnotation annotation : scope.getAnnotations()) {
			// skip annotations without value that reference identifiers 
			Token valueToken = annotation.getValueToken();
			if (valueToken == null || !valueToken.isDdlStringLiteral())
				continue;

			// determine whether the annotation reference entity/view/association/parameter names 
			String path = annotation.getPath();
			boolean isRef = DDL.isKnownEntityRefAnnotation(path) || DDL.isKnownElementRefAnnotation(path) 
							 || DDL.isKnownAssociationRefAnnotation(path) || DDL.isKnownParameterRefAnnotation(path)
							 || annotation.lastElementContains("element", "Element");
			boolean process = isRef ? configProcessAnnotationRefs.getValue() :configProcessAnnotations.getValue();
			if (!process)	
				continue;

			// skip values that don't seem to contain English text
			String value = valueToken.getText().substring(1, valueToken.getTextLength() - 1);
			CommentIdentification identification = identifier.identifyComment(DDL.COMMENT_SIGN_STRING + value, true, Language.DDL);
			if (!identification.isEnglish()) 
				continue;

			CommentIdentifier.CorrectionResult result = identifier.correctLiteral(value, '\\', correctTypos, convertBritishToAmerican);
			if (result == null || !result.wasAnythingCorrected()) 
				continue;
			
			for (String correction : result.corrections) {
				String todoText = TODO_PREFIX + "check spelling: " + correction + TODO_SUFFIX;
				try {
					Token newComment = command.putCommentAboveLineOf(command.getFirstToken(), todoText);
					if (newComment != null) {
						code.addRuleUse(this, newComment.getParentCommand());
					}
				} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, command, "error while inserting comment line");
				}
			}
		}
		return false;
	}
}
