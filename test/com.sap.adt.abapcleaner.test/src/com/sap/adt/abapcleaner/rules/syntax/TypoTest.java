package com.sap.adt.abapcleaner.rules.syntax;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleTestBase;

public class TypoTest extends RuleTestBase {
	private TypoRule rule;
	
	TypoTest() {
		super(RuleID.TYPO);
		rule = (TypoRule)getRule();
	}
	
	@BeforeEach
	void setUp() {
		// setup default test configuration (may be modified in the individual test methods)
		rule.configCorrectTypos.setValue(true);
		rule.configConvertBritishToAmerican.setValue(true); 
		rule.configProcessAbapDoc.setValue(true);
		rule.configProcessShorttexts.setValue(true); 
		rule.configProcessComments.setValue(true); 
		rule.configAddTodoBeforeMessage.setValue(true); 
		rule.configActionForLiterals.setEnumValue(TypoAction.ADD_TODO_COMMENT);
	}

	@Test
	void testSynchronizedShorttexts() {
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"EN\">Synchronised comment releated to method defintion</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any   | <p class=\"shorttext synchronized\">coresponding parameter desciption</p>");
		buildSrc("    \"! @parameter is_other | <p class=\"shorttext synchronized\">required strucutre paramater</p>");

		buildExp("    \"! <p class=\"shorttext synchronized\" lang=\"EN\">Synchronized comment related to method definition</p>");
		buildExp("    \"!");
		buildExp("    \"! @parameter iv_any   | <p class=\"shorttext synchronized\">corresponding parameter description</p>");
		buildExp("    \"! @parameter is_other | <p class=\"shorttext synchronized\">required structure parameter</p>");

		putAnyClassDefAroundSrcAndExp();

		testRule();
	}

	@Test
	void testSynchronizedShorttextsDefect() {
		// prepare defects in the HTML tags:
		// - <p ... not closed
		// - <p...> closed with </em>
		// - <p...> followed by another <p>
		// and expect that the typos remain unchanged  
		
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"EN\" Synchronised comment releated to method defintion");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any   | <p class=\"shorttext synchronized\">coresponding parameter desciption</em>");
		buildSrc("    \"! @parameter is_other | <p class=\"shorttext synchronized\">required strucutre paramater<p>");

		putAnyClassDefAroundSrcAndExp();

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testSynchronizedShorttextsUnchanged() {
		rule.configProcessShorttexts.setValue(false);
		
		buildSrc("    \"! <p class=\"shorttext synchronized\" lang=\"EN\">Synchronised comment releated to method defintion</p>");
		buildSrc("    \"!");
		buildSrc("    \"! @parameter iv_any   | <p class=\"shorttext synchronized\">coresponding parameter desciption</p>");
		buildSrc("    \"! @parameter is_other | <p class=\"shorttext synchronized\">required strucutre paramater</p>");

		putAnyClassDefAroundSrcAndExp();
		
		copyExpFromSrc();

		testRule();
	}
	
	@Test
	void testAbapDoc() {
		buildSrc("\"! This rule changes occurences of 553 frequent errorneous words to the correspondig correct word.");
		buildSrc("\"! Typos are only changed if the correct word is unambiguous, as with 'succesfull' and 'availble'.");
		buildSrc("\"! If there is any doubt - e.g. wether 'convered' meant 'covered' or 'converted' - NO chnage is made.");

		buildExp("\"! This rule changes occurrences of 553 frequent erroneous words to the corresponding correct word.");
		buildExp("\"! Typos are only changed if the correct word is unambiguous, as with 'successful' and 'available'.");
		buildExp("\"! If there is any doubt - e.g. whether 'convered' meant 'covered' or 'converted' - NO change is made.");

		testRule();
	}

	@Test
	void testAbapDocUnchanged() {
		rule.configProcessAbapDoc.setValue(false);

		buildSrc("\"! This rule changes occurences of 553 frequent errorneous words to the correspondig correct word.");
		buildSrc("\"! Typos are only changed if the correct word is unambiguous, as with 'succesfull' and 'availble'.");
		buildSrc("\"! If there is any doubt - e.g. wether 'convered' meant 'covered' or 'converted' - NO chnage is made.");

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testComments() {
		buildSrc("    \" occurr; occured, occured; occurance, occurence");
		buildSrc("* successfull, succesful, succesfull, sucessfull; succesfully, sucessfully");
		buildSrc("* \"  neccessary, neccesary, necesary");

		buildExp("    \" occur; occurred, occurred; occurrence, occurrence");
		buildExp("* successful, successful, successful, successful; successfully, successfully");
		buildExp("* \"  necessary, necessary, necessary");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testGermanCommentsUnchanged() {
		buildSrc("    \" ein Kommentar in deutscher Sprache sollte NICHT ge�ndert werden, auch wenn");
		buildSrc("* er W�rter wie Docment, Funtion und maxium enth�lt, die als englische Tippfehler bekannt sind.");

		putAnyMethodAroundSrcAndExp();

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testCommentsUnchanged() {
		rule.configProcessComments.setValue(false);
		
		buildSrc("    \" occurr; occured, occured; occurance, occurence");
		buildSrc("* successfull, succesful, succesfull, sucessfull; succesfully, sucessfully");
		buildSrc("* \"  neccessary, neccesary, necesary");

		putAnyMethodAroundSrcAndExp();

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testAddTodoBeforeMessage() {
		buildSrc("    \" 'Fatal error ocurred! Processing was canceld.'");
		buildSrc("    MESSAGE e042(any_message_class) INTO lv_msg_str.");

		buildExp("    \" TODO: check spelling: ocurred (typo) -> occurred (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: canceld (typo) -> cancelled (ABAP cleaner)");
		buildExp("    \" 'Fatal error ocurred! Processing was canceld.'");
		buildExp("    MESSAGE e042(any_message_class) INTO lv_msg_str.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testDontAddTodoBeforeAssignment() {
		// ensure that the second line is not mistaken for a MESSAGE command, 
		// and that typos are therefore directly changed within the comment  

		buildSrc("    \" 'Fatal error ocurred! Processing was canceld.'");
		buildSrc("    message = `abc`.");
		buildSrc("");
		buildSrc("    \" 'Fatal error ocurred! Processing was canceld.'");
		buildSrc("    WRITE |message|.");

		buildExp("    \" 'Fatal error occurred! Processing was cancelled.'");
		buildExp("    message = `abc`.");
		buildExp("");
		buildExp("    \" 'Fatal error occurred! Processing was cancelled.'");
		buildExp("    WRITE |message|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testChangeCommentBeforeMessage() {
		rule.configAddTodoBeforeMessage.setValue(false);

		buildSrc("    \" 'Fatal error ocurred! Processing was canceld.'");
		buildSrc("    MESSAGE e042(any_message_class) INTO lv_msg_str.");

		buildExp("    \" 'Fatal error occurred! Processing was cancelled.'");
		buildExp("    MESSAGE e042(any_message_class) INTO lv_msg_str.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testExecutableCode() {
		// expect that typos in executable code (e.g. in identifiers and components) are NOT changed

		buildSrc("    DATA occurr TYPE i.");
		buildSrc("    DATA successfull TYPE i.");
		buildSrc("    DATA neccessary TYPE string.");
		buildSrc("");
		buildSrc("    occurr = successfull.");
		buildSrc("    neccessary = |test { occurr } and { successfull } and { docment-ehancement }.|.");

		putAnyMethodAroundSrcAndExp();

		copyExpFromSrc();

		testRule();
	}

	@Test
	void testLiteralsAddTodo() {
		buildSrc("    task = `calcualte all availabe attributes`.");
		buildSrc("");
		buildSrc("    info = |initialising { lv_count } occurences|.");
		buildSrc("");
		buildSrc("    message = 'analysing the optimisation of required authorisations'.");

		buildExp("    \" TODO: check spelling: calcualte (typo) -> calculate (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: availabe (typo) -> available (ABAP cleaner)");
		buildExp("    task = `calcualte all availabe attributes`.");
		buildExp("");
		buildExp("    \" TODO: check spelling: initialising (BE) -> initializing (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: occurences (typo) -> occurrences (ABAP cleaner)");
		buildExp("    info = |initialising { lv_count } occurences|.");
		buildExp("");
		buildExp("    \" TODO: check spelling: analysing (BE) -> analyzing (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: optimisation (BE) -> optimization (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: authorisations (BE) -> authorizations (ABAP cleaner)");
		buildExp("    message = 'analysing the optimisation of required authorisations'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLiteralsAvoidDuplicateTodo() {
		// expect that for existing TODOs (regardless of their sequence), no duplicates are added;
		// however, expect the missing TODO for 'authorisations (BE)' to be added at the end
		
		buildSrc("    \" TODO: check spelling: calcualte (typo) -> calculate (ABAP cleaner)");
		buildSrc("    \" TODO: check spelling: availabe (typo) -> available (ABAP cleaner)");
		buildSrc("    task = `calcualte all availabe attributes`.");
		buildSrc("");
		buildSrc("    \" TODO: check spelling: occurences (typo) -> occurrences (ABAP cleaner)");
		buildSrc("    \" TODO: check spelling: initialising (BE) -> initializing (ABAP cleaner)");
		buildSrc("    info = |initialising { lv_count } occurences|.");
		buildSrc("");
		buildSrc("    \" TODO: check spelling: optimisation (BE) -> optimization (ABAP cleaner)");
		buildSrc("    \" TODO: check spelling: analysing (BE) -> analyzing (ABAP cleaner)");
		buildSrc("    message = 'analysing the optimisation of required authorisations'.");

		buildExp("    \" TODO: check spelling: calcualte (typo) -> calculate (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: availabe (typo) -> available (ABAP cleaner)");
		buildExp("    task = `calcualte all availabe attributes`.");
		buildExp("");
		buildExp("    \" TODO: check spelling: occurences (typo) -> occurrences (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: initialising (BE) -> initializing (ABAP cleaner)");
		buildExp("    info = |initialising { lv_count } occurences|.");
		buildExp("");
		buildExp("    \" TODO: check spelling: optimisation (BE) -> optimization (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: analysing (BE) -> analyzing (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: authorisations (BE) -> authorizations (ABAP cleaner)");
		buildExp("    message = 'analysing the optimisation of required authorisations'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}

	@Test
	void testLiteralsWithBracketsChangeDirectly() {
		// ensure that typos are identified and changed, esp. at the start and end of the literals
		
		rule.configActionForLiterals.setEnumValue(TypoAction.CHANGE_DIRECTLY);

		buildSrc("    task = `calcualte all (availabe) attributes`.");
		buildSrc("");
		buildSrc("    info = |initialising { lv_count } occurences|.");
		buildSrc("");
		buildSrc("    message = 'analysing the [optimisation] of required authorisations'.");

		buildExp("    task = `calculate all (available) attributes`.");
		buildExp("");
		buildExp("    info = |initializing { lv_count } occurrences|.");
		buildExp("");
		buildExp("    message = 'analyzing the [optimization] of required authorizations'.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testLiteralsUnchanged() {
		rule.configActionForLiterals.setEnumValue(TypoAction.KEEP_UNCHANGED);

		buildSrc("    task = `calcualte all availabe attributes`.");
		buildSrc("");
		buildSrc("    info = |initialising { lv_count } occurences|.");
		buildSrc("");
		buildSrc("    message = 'analysing the optimisation of required authorisations'.");

		putAnyMethodAroundSrcAndExp();
		
		copyExpFromSrc();

		testRule();
	}

	@Test
	void testWordsAttachedToStringTemplateChars() {
		rule.configActionForLiterals.setEnumValue(TypoAction.CHANGE_DIRECTLY);

		buildSrc("    info = |analysing{ TAB }availabe{ TAB }occurences|.");
		buildSrc("    message = |optimisation|.");

		buildExp("    info = |analyzing{ TAB }available{ TAB }occurrences|.");
		buildExp("    message = |optimization|.");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testEmptyLiterals() {
		buildSrc("    task = ``.");
		buildSrc("");
		buildSrc("    info = |{ lv_count }|.");
		buildSrc("");
		buildSrc("    message = ''.");

		putAnyMethodAroundSrcAndExp();
		
		copyExpFromSrc();

		testRule();
	}


	@Test
	void testLiteralsWithEscapingAddTodo() {
		// ensure that escape chars are correctly identified, esp. with apostrophes inside text field literals '...'
	
		// note that in the middle line, both Java and ABAP require escaping, so we have 'double escaping';  
		// in ABAP, this line would be: info = |didnt \\ \"does'nt\", dont't|.
		// and is changed to:           info = |didn't \\ \"doesn't\", don't|.
		
		buildSrc("    task = `didnt, does'nt, dont't`.");
		buildSrc("");
		buildSrc("    info = |didnt \\\\ \\\"does'nt\\\", dont't|.");
		buildSrc("");
		buildSrc("    message = 'didnt, does''nt, dont''t'.");

		buildExp("    \" TODO: check spelling: didnt (typo) -> didn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: does'nt (typo) -> doesn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: dont't (typo) -> don't (ABAP cleaner)");
		buildExp("    task = `didnt, does'nt, dont't`.");
		buildExp("");
		buildExp("    \" TODO: check spelling: didnt (typo) -> didn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: does'nt (typo) -> doesn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: dont't (typo) -> don't (ABAP cleaner)");
		buildExp("    info = |didnt \\\\ \\\"does'nt\\\", dont't|.");
		buildExp("");
		buildExp("    \" TODO: check spelling: didnt (typo) -> didn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: does'nt (typo) -> doesn't (ABAP cleaner)");
		buildExp("    \" TODO: check spelling: dont't (typo) -> don't (ABAP cleaner)");
		buildExp("    message = 'didnt, does''nt, dont''t'.");

		testRule();
	}

	@Test
	void testLiteralsWithEscapingChangeDirectly() {
		rule.configActionForLiterals.setEnumValue(TypoAction.CHANGE_DIRECTLY);

		buildSrc("    task = `didnt, does'nt, dont't`.");
		buildSrc("");
		buildSrc("    info = |didnt \\\\ \\\"does'nt\\\", dont't|.");
		buildSrc("");
		buildSrc("    message = 'didnt, does''nt, dont''t'.");

		buildExp("    task = `didn't, doesn't, don't`.");
		buildExp("");
		buildExp("    info = |didn't \\\\ \\\"doesn't\\\", don't|.");
		buildExp("");
		buildExp("    message = 'didn''t, doesn''t, don''t'.");

		putAnyMethodAroundSrcAndExp();
		
		testRule();
	}


	@Test
	void testChangeBritishButKeepTypos() {
		rule.configCorrectTypos.setValue(false);

		buildSrc("*   very usefull comment on the busines logic in this method");
		buildSrc("*   and some describtion of its behaviour, depending on input paremeters");

		buildExp("*   very usefull comment on the busines logic in this method");
		buildExp("*   and some describtion of its behavior, depending on input paremeters");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}

	@Test
	void testCorrectTyposButKeepBritish() {
		rule.configConvertBritishToAmerican.setValue(false);

		buildSrc("*   very usefull comment on the busines logic in this method");
		buildSrc("*   and some describtion of its behaviour, depending on input paremeters");

		buildExp("*   very useful comment on the business logic in this method");
		buildExp("*   and some description of its behaviour, depending on input parameters");

		putAnyMethodAroundSrcAndExp();

		testRule();
	}


	@Test
	void testNeitherTyposNorBritish() {
		rule.configCorrectTypos.setValue(false);
		rule.configConvertBritishToAmerican.setValue(false);

		buildSrc("\"! This rule changes occurences of errorneous words to the correspondig correct word.");
		buildSrc("CLASS cl_frequent_typos IMPLEMENTATION.");
		buildSrc("  METHOD correct_frequent_typos.");
		buildSrc("*   very usefull comment on the busines logic in this method");
		buildSrc("    task = `calcualte all availabe attributes`.");
		buildSrc("  ENDMETHOD.");
		buildSrc("ENDCLASS.");

		copyExpFromSrc();

		testRule();
	}
}
