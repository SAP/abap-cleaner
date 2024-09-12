package com.sap.adt.abapcleaner.rules.syntax;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.Program;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.Rule;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentification;
import com.sap.adt.abapcleaner.rulehelpers.CommentIdentifier;

public class TypoRule extends Rule {
	private final static RuleReference[] references = new RuleReference[] { new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.TYPO; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.SYNTAX; }

	@Override
	public String getDisplayName() { return "Correct frequent typos"; }

	@Override
	public String getDescription() {
		return "Corrects frequent typos in textual comments, ABAP documentation, and literals. Only considers typos with unambiguous correction.";
	}

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2022, 10, 22); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
		String freqTypoCount = Cult.format(CommentIdentifier.getFrequentEnglishTypoCount()); 
      return "" 
      	+ LINE_SEP + "\"! This rule changes occurences of " + freqTypoCount + " frequent errorneous words to the correspondig correct word." 
      	+ LINE_SEP + "\"! Typos are only changed if the correct word is unambiguous, as with 'succesfull' and 'availble'."  
      	+ LINE_SEP + "\"! If there is any doubt - e.g. wether 'convered' meant 'covered' or 'converted' - NO chnage is made."  
			+ LINE_SEP + "\"! " 
      	+ LINE_SEP + "\"! Additionally, some frequent word forms can be changed from British English to American English,"  
      	+ LINE_SEP + "\"! e.g. 'flavour', 'customisation', 'favourite' and 'initialisation'."  
			+ LINE_SEP + "CLASS cl_frequent_typos DEFINITION." 
			+ LINE_SEP + "  PUBLIC SECTION." 
			+ LINE_SEP + "    \"! <p class=\"shorttext synchronized\" lang=\"EN\">Synchronised comment releated to method defintion</p>" 
			+ LINE_SEP + "    \"! Additonal ABAP Doc comment which is not synchronised and only visibile in ADT"
			+ LINE_SEP + "    \"!"
			+ LINE_SEP + "    \"! @parameter iv_any   | <p class=\"shorttext synchronized\">coresponding parameter desciption</p>" 
			+ LINE_SEP + "    \"! @parameter is_other | <p class=\"shorttext synchronized\">required strucutre paramater</p>" 
			+ LINE_SEP + "    METHODS correct_frequent_typos" 
			+ LINE_SEP + "      IMPORTING iv_any   TYPE string" 
			+ LINE_SEP + "                is_other TYPE ty_s_any." 
			+ LINE_SEP + "ENDCLASS." 
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "CLASS cl_frequent_typos IMPLEMENTATION." 
			+ LINE_SEP + "  METHOD correct_frequent_typos." 
			+ LINE_SEP + "*   very usefull comment on the busines logic in this method"
			+ LINE_SEP + "*   and some describtion of its behaviour, depending on input paremeters"
			+ LINE_SEP 
			+ LINE_SEP + "    \" typos in text string literal"
			+ LINE_SEP + "    task = `calcualte all availabe attributes`." 
			+ LINE_SEP + "    perform_task( task )."
			+ LINE_SEP 
			+ LINE_SEP + "    \" British English in string template"
			+ LINE_SEP + "    info = |initialising { lv_count } occurences out of { lv_total }|." 
			+ LINE_SEP + "    show_info( info )." 
			+ LINE_SEP 
			+ LINE_SEP + "    \" typos in text field literal"
			+ LINE_SEP + "    message = 'analysing the optimisation of required authorisations'." 
			+ LINE_SEP + "    show_message( message )."
			+ LINE_SEP 
			+ LINE_SEP + "    \" The next comment is followed by a MESSAGE command, so it is likely that it contains the short text" 
			+ LINE_SEP + "    \" of the message; in this case, typos may have to be manually corrected in the message class, too:" 
			+ LINE_SEP + "    \" 'Fatal error ocurred! Processing was canceld.'" 
			+ LINE_SEP + "    MESSAGE e042(any_message_class) INTO lv_msg_str." 
			+ LINE_SEP + "    lo_message_handler->add_symessage( )."
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD top_typos." 
			+ LINE_SEP + "    \" TOP TWELVE TYPOS by frequency in ABAP comments" 
			+ LINE_SEP + "    \" ----------------------------------------------" 
			+ LINE_SEP + "    \"  1. wether" 
			+ LINE_SEP + "    \"  2. occurr; occured, occured; occurance, occurence" 
			+ LINE_SEP + "    \"  3. successfull, succesful, succesfull, sucessfull; succesfully, sucessfully"
			+ LINE_SEP + "    \"  4. neccessary, neccesary, necesary" 
			+ LINE_SEP + "    \"  5. existance; existant; exisiting, exisitng; exsits, exsists"
			+ LINE_SEP + "    \"  6. paramter, paramater; paramters, paremeters, parmeters, parametrs, parametes"
			+ LINE_SEP + "    \"  7. transfered, tranfered"
			+ LINE_SEP + "    \"  8. seperate; seperated; seperator; separatly"
			+ LINE_SEP + "    \"  9. dependant"
			+ LINE_SEP + "    \" 10. strucutre, strucure, struture, strcuture, sturcture, strcture, stucture"
			+ LINE_SEP + "    \" 11. choosen, choosed"
			+ LINE_SEP + "    \" 12. allready, alredy"
			+ LINE_SEP 
			+ LINE_SEP + "    \" TOP TEN WORDS by number of typo variants" 
			+ LINE_SEP + "    \" ----------------------------------------" 
			+ LINE_SEP + "    \" 7: strucutre, strucure, struture, strcuture, sturcture, strcture, stucture"
			+ LINE_SEP + "    \"    availabe, avaliable, avaialable, avaialble, availble, availible, avialable" 
			+ LINE_SEP + "    \" 6: coresponding, correspondig, corrsponding, corresonding, corresponing, correspoding"
			+ LINE_SEP + "    \" 5: paramters, paremeters, parmeters, parametrs, parametes"
			+ LINE_SEP + "    \"    docuemnt, docuent, doucment, docuemt, docment"
			+ LINE_SEP + "    \" 4: successfull, succesful, succesfull, sucessfull"
			+ LINE_SEP + "    \"    selction, seleciton, selecton, seletion"
			+ LINE_SEP + "    \"    initalize, intialize, initalze, initialze" 
			+ LINE_SEP + "    \"    atrributes, attibutes, atttributes, atributes" 
			+ LINE_SEP + "    \"    applicaton, aplication, appliaction, applicaiton"
			+ LINE_SEP + "  ENDMETHOD."
/*
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD show_frequent_typos." 
			+ getCommentLinesFrom("frequent typos", CommentIdentifier.getFrequentEnglishTypos(), true)
			+ LINE_SEP + "  ENDMETHOD."
			+ LINE_SEP 
			+ LINE_SEP 
			+ LINE_SEP + "  METHOD show_frequent_british_english." 
			+ getCommentLinesFrom("frequent British English forms", CommentIdentifier.getFrequentBritishEnglish(), false)
			+ LINE_SEP + "  ENDMETHOD."
 */
			+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigBoolValue configCorrectTypos = new ConfigBoolValue(this, "CorrectTypos", "Correct frequent typos", true);
	final ConfigBoolValue configConvertBritishToAmerican = new ConfigBoolValue(this, "ConvertBritishToAmerican", "Change from British English to American English", true); 
	final ConfigBoolValue configProcessAbapDoc = new ConfigBoolValue(this, "ProcessAbapDoc", "Apply on ABAP Doc", true);
	final ConfigBoolValue configProcessShorttexts = new ConfigBoolValue(this, "ProcessShorttexts", "Apply on (synchronized) shorttexts", true); 
	final ConfigBoolValue configProcessComments = new ConfigBoolValue(this, "ProcessComments", "Apply on textual comments", true); 
	final ConfigBoolValue configAddTodoBeforeMessage = new ConfigBoolValue(this, "AddTodoBeforeMessage", "Add TODO if textual comment is followed by MESSAGE command", true); 
   final ConfigEnumValue<TypoAction> configActionForLiterals = new ConfigEnumValue<TypoAction>(this, "MeasureForLiterals", "Apply on literals:", new String[] { "change directly", "add TODO comment", "keep unchanged" }, TypoAction.values(), TypoAction.ADD_TODO_COMMENT);

	private final ConfigValue[] configValues = new ConfigValue[] { configCorrectTypos, configConvertBritishToAmerican, configProcessAbapDoc, configProcessShorttexts, configProcessComments, configAddTodoBeforeMessage, configActionForLiterals };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public TypoRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	@SuppressWarnings("unused")
	private String getCommentLinesFrom(String wordTypePlural, HashMap<String, String> list, boolean startNewLineForEachLetter) {
		final int MAX_LENGTH = 115;
		final String LINE_PREFIX = LINE_SEP + "    " + ABAP.COMMENT_SIGN_STRING + " ";
		final String WORD_SEP = " - ";
		
		ArrayList<String> words = new ArrayList<String>();
		for (String word : list.keySet()) {
			// skip upper case words if the same word also exists in lower case
			if (word.length() > 0 && Character.isUpperCase(word.charAt(0)) && list.containsKey(word.toLowerCase()))
				continue;
			String correction = list.get(word);
			if (!StringUtil.isNullOrEmpty(correction))
				words.add(word);
		}
		Collections.sort(words);
		
		StringBuilder result = new StringBuilder();
		StringBuilder line = new StringBuilder();
		String lastWord = "";
		for (String word : words) {
			boolean isNewLetter = startNewLineForEachLetter && lastWord.length() > 0 && word.length() > 0 && lastWord.charAt(0) != word.charAt(0); 
			if (LINE_PREFIX.length() + line.length() + WORD_SEP.length() + word.length() > MAX_LENGTH || isNewLetter) {
				result.append(LINE_PREFIX).append(line.toString());
				if (isNewLetter)
					result.append(LINE_SEP);
				line = new StringBuilder();
			}
			if (line.length() > 0)
				line.append(WORD_SEP);
			line.append(word);
			lastWord = word;
		}
		if (line.length() > 0)
			result.append(LINE_PREFIX).append(line.toString());
		
		String info1 = Cult.format(words.size()) + " " + wordTypePlural + " are currently considered:";
		String info2 = StringUtil.repeatChar('-', info1.length());
		return  LINE_PREFIX + info1
				+ LINE_PREFIX + info2
				+ result.toString();
	}

	@Override
	public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		if (code == null)
			throw new NullPointerException("code");

		if (!configCorrectTypos.getValue() && !configConvertBritishToAmerican.getValue())
			return;

		CommentIdentifier identifier = new CommentIdentifier();

		Command command = code.firstCommand;
		while (command != null) {
			if (isCommandBlocked(command)) {
				command = command.getNext();
				continue;
			}
			commandForErrorMsg = command;
			
			boolean isAbapDoc = command.isAbapDoc(); 
			boolean commandChanged = false;

			Token token = command.getFirstToken();
			while (token != null) {
				if (token.isComment() || token.isStringLiteral()) {
					if (executeOn(command, token, isAbapDoc, identifier)) { 
						commandChanged = true;
					}
				}
				token = token.getNext();
			}
			if (commandChanged) {
				code.addRuleUse(this, command);
			}
				
			command = command.getNext();
		}
	}
	
	private boolean executeOn(Command command, Token token, boolean isAbapDoc, CommentIdentifier identifier) throws UnexpectedSyntaxAfterChanges {
		final String TODO_PREFIX = "TODO: ";
		final String TODO_SUFFIX = " (" + Program.PRODUCT_NAME + ")";
		
		String text = token.getText();
		int textStart = 0;
		int textEnd = text.length();

		boolean changeDirectly = true;
		CommentIdentifier.CorrectionResult result = null;

		if (token.isComment()) {
			// do NOT process the TODO comments which were generated by an earlier run of this rule, 
			// because they contain typos, too!
			if (token.textEndsWith(TODO_SUFFIX) && token.textStartsWith(ABAP.COMMENT_SIGN_STRING + " " + TODO_PREFIX)) {
				return false;
			}
			
			boolean processToken = false;
			if (isAbapDoc) {
				// process ABAP Doc or a synchronized shorttext
				int tokenPos = text.indexOf("<p class=\"shorttext");
				if (tokenPos < 0) {
					processToken = configProcessAbapDoc.getValue();
				} else if (configProcessShorttexts.getValue()) {
					textStart = text.indexOf('>', tokenPos);
					if (textStart >= 0) {
						textEnd = text.indexOf("</p>", textStart);
						processToken = (textEnd >= 0);
					}
				}
			} else  {
				// process normal comment
				processToken = configProcessComments.getValue();
				CommentIdentification identification = identifier.identifyComment(text, true, Language.ABAP);
				if (!identification.isEnglish()) {
					processToken = false;
				}
				// if the comment is directly followed by a MESSAGE command, assume that the comment contains the message text;
				// in such case, add a TODO comment instead of changing the typo directly, because the message class would need 
				// to be changed, too
				if (configAddTodoBeforeMessage.getValue()) {
					Command nextCommand = command.getNext();
					Token nextToken = (nextCommand == null) ? null : nextCommand.getFirstCodeToken();
					if (nextToken != null && nextToken.isKeyword("MESSAGE")) {
						changeDirectly = false;
					}
				}
			}
			if (!processToken) 
				return false;
			result = identifier.correctComment(text.substring(textStart, textEnd), configCorrectTypos.getValue(), configConvertBritishToAmerican.getValue());

		} else if (token.isStringLiteral()) { 
		   TypoAction actionForLiterals = TypoAction.forValue(configActionForLiterals.getValue());
			if (actionForLiterals == TypoAction.KEEP_UNCHANGED) 
				return false;
			changeDirectly = (actionForLiterals == TypoAction.CHANGE_DIRECTLY);
			// character literal '...', string literal `...`, or string template |...| etc.
			char delimiter = token.getText().charAt(0);
			textStart = 1;
			textEnd = token.getTextLength() - 1;
			if (textEnd <= textStart) 
				return false;
			char escapeChar = (delimiter == ABAP.QUOT_MARK || delimiter == ABAP.QUOT_MARK2) ? delimiter : '\\';
			result = identifier.correctLiteral(text.substring(textStart, textEnd), escapeChar, configCorrectTypos.getValue(), configConvertBritishToAmerican.getValue());
		}

		if (result == null || !result.wasAnythingCorrected()) 
			return false;

		if (changeDirectly) {
			token.setText(text.substring(0, textStart) + result.correctedText + text.substring(textEnd), false);
		} else if (result.corrections != null) {
			for (String correction : result.corrections) {
				String todoText = TODO_PREFIX + "check spelling: " + correction + TODO_SUFFIX;
				try {
					command.putCommentAboveLineOf(token, todoText);
				} catch (IntegrityBrokenException | UnexpectedSyntaxException e) {
					throw new UnexpectedSyntaxAfterChanges(this, command, "error while inserting comment line");
				}
			}
		}
		return true;
	}
}
