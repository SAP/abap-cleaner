package com.sap.adt.abapcleaner.rules.emptylines;

import java.time.LocalDate;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Section;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigEnumValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.MethodInfo;
import com.sap.adt.abapcleaner.rules.ddl.emptylines.RemoveCommentCondition;

public class CdsTestClassLinesRule extends RuleForDeclarations {
	private static final String[] abapDocInstructions = new String[] { 
			"In CLASS_SETUP =  corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.",
			"In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.",
			"In CLASS_TEARDOWN =  Generated database entities (doubles & clones) should be deleted at the end of test class execution.",
			"In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.",
			"SETUP method creates a common start state for each test method =",
			"SETUP method creates a common start state for each test method,",
			"clear_doubles clears the test data for all the doubles used in the test method before each test method execution.",
			"In this method test data is inserted into the generated double(s) and the test is executed and",
			"the results should be asserted with the actuals."
	};
	private static final String[] toDoComments = new String[] {
			"TODO: Provide the test data here",
			"TODO: Provide the parameter data here"
	};
	
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_CLEANER) };

	@Override
	public RuleID getID() { return RuleID.CDS_TEST_CLASS_LINES; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.EMPTY_LINES; }

	@Override
	public String getDisplayName() { return "Standardize test classes for CDS views"; }

	@Override
	public String getDescription() { return "Standardizes empty lines and removes repetitive ABAP Doc instructions from test classes for CDS views."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule only works on local test classes with a \"!@testing <CDS view name> annotation, as created by the ADT command 'New ABAP Test Class'."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 10, 16); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
   public String getExample() {
      return "" 
   			+ LINE_SEP + "\"!@testing C_RAPERFOBLGNWITHTOCURPERDAMT"
   			+ LINE_SEP + "CLASS ltc_c_raperfoblgnwithtocurperd DEFINITION FINAL"
   			+ LINE_SEP + "  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  PRIVATE SECTION."
   			+ LINE_SEP + "    CLASS-DATA environment TYPE REF TO if_cds_test_environment."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY."
   			+ LINE_SEP + "    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY."
   			+ LINE_SEP + "    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \"! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created."
   			+ LINE_SEP + "    CLASS-METHODS class_setup RAISING cx_static_check."
   			+ LINE_SEP + "    \"! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution."
   			+ LINE_SEP + "    CLASS-METHODS class_teardown."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \"! SETUP method creates a common start state for each test method,"
   			+ LINE_SEP + "    \"! clear_doubles clears the test data for all the doubles used in the test method before each test method execution."
   			+ LINE_SEP + "    METHODS setup RAISING cx_static_check."
   			+ LINE_SEP + "    METHODS prepare_testdata."
   			+ LINE_SEP + "    \"! In this method test data is inserted into the generated double(s) and the test is executed and"
   			+ LINE_SEP + "    \"! the results should be asserted with the actuals."
   			+ LINE_SEP + "    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check."
   			+ LINE_SEP + "ENDCLASS."
   			+ LINE_SEP + ""
   			+ LINE_SEP + ""
   			+ LINE_SEP + "CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION."
   			+ LINE_SEP + "  METHOD class_setup."
   			+ LINE_SEP + "    environment = cl_cds_test_environment=>create( i_for_entity = 'C_RAPERFOBLGNWITHTOCURPERDAMT' )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  METHOD setup."
   			+ LINE_SEP + "    environment->clear_doubles( )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  METHOD class_teardown."
   			+ LINE_SEP + "    environment->destroy( )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  METHOD aunit_for_cds_method."
   			+ LINE_SEP + "    prepare_testdata( )."
   			+ LINE_SEP + "    \" query the view under test"
   			+ LINE_SEP + "    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt"
   			+ LINE_SEP + "      INTO TABLE @act_results."
   			+ LINE_SEP + "    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  METHOD prepare_testdata."
   			+ LINE_SEP + "    \" Prepare test data for 'i_businesspartner'"
   			+ LINE_SEP + "    \" TODO: Provide the test data here"
   			+ LINE_SEP + "    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )"
   			+ LINE_SEP + "                                    ( BusinessPartner = 'other' ) )."
   			+ LINE_SEP + "    environment->insert_test_data( i_data = td_i_businesspartner )."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \" Prepare test data for 'i_customer'"
   			+ LINE_SEP + "    \" TODO: Provide the test data here"
   			+ LINE_SEP + "    td_i_customer = VALUE #( ( ) )."
   			+ LINE_SEP + "    environment->insert_test_data( i_data = td_i_customer )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigBoolValue configRemoveAbapDoc = new ConfigBoolValue(this, "RemoveAbapDoc", "Remove generated instructions in ABAP Doc", true);
	final ConfigEnumValue<RemoveCommentCondition> configRemoveToDoComments = new ConfigEnumValue<RemoveCommentCondition>(this, "RemoveToDoComments", "Remove comment 'TODO: Provide the test data here'",
			new String[] { "always", "if empty VALUE constructor was changed", "never" }, RemoveCommentCondition.values(), RemoveCommentCondition.IF_VALUE_HAS_CONTENT);
	final ConfigBoolValue configEmptyLineAboveInsertTestData = new ConfigBoolValue(this, "EmptyLineAboveInsertTestData", "Insert empty line between multi-line VALUE and INSERT_TEST_DATA", true);
	final ConfigBoolValue configEmptyLineAboveSelect = new ConfigBoolValue(this, "EmptyLineAboveSelect", "Insert empty line above SELECT statement", true);
	final ConfigBoolValue configEmptyLineBelowSelect = new ConfigBoolValue(this, "EmptyLineBelowSelect", "Insert empty line below SELECT statement", true);
	public final ConfigBoolValue configMovePrepareMethods = new ConfigBoolValue(this, "MovePrepareMethods", "Move PREPARE_... method(s) above the first FOR TESTING method", false);

	private final ConfigValue[] configValues = new ConfigValue[] { configRemoveAbapDoc, configRemoveToDoComments, configEmptyLineAboveInsertTestData, configEmptyLineAboveSelect, configEmptyLineBelowSelect, configMovePrepareMethods };

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CdsTestClassLinesRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	private ClassInfo curClassInfo = null;

	@Override
	protected boolean skipLocalVariableContexts() { return true; }

	@Override
	protected void executeOnClassDefinition(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// cp. CamelCaseInCdsTestClassRule.executeOnClassDefinition()
		curClassInfo = null;
		
		// only process local test classes 
		Command declarationCommand = classInfo.declarationCommand;
		if (!declarationCommand.isLocalTestClassDefinitionStart())
			return;
		
		// ensure that the test class is annotated with "!@testing
		// the "!@testing annotation must be part of the attached ABAP Doc comments (with no non-ABAP-Doc comments in between)  
		Command comment = declarationCommand.getPrevSibling();
		boolean foundTestingAnno = false;
		while (comment != null && comment.isAbapDoc()) {
			if (comment.isTestingAnnotation()) {
				foundTestingAnno = true;
				break;
			}
			comment = comment.getPrevSibling();
		}
		if (!foundTestingAnno) 
			return;
		curClassInfo = classInfo;
		
		// process Commands in DEFINITION section
		Command command = declarationCommand;
		while (command != null && !command.firstCodeTokenIsKeyword("ENDCLASS")) {
			commandForErrorMsg = command;

			Command nextCommand = command.getNext();
			if (!isCommandBlocked(command)) {
				if (command.firstCodeTokenIsAnyKeyword("CLASS-METHODS", "METHODS")) {
					// remove ABAP Doc instructions in Tokens of [CLASS-]METHODS: chains
					if (configRemoveAbapDoc.getValue()) {
						removeAbapDocTokens(code, command);
					}
				} else if (command.isAbapDoc() && isAnyComment(command.getFirstToken(), abapDocInstructions)) {
					// removes ABAP Doc instructions in own comment lines
					if (configRemoveAbapDoc.getValue()) {
						removeAbapDocCommand(code, command);
					}
				} 
			}
			command = nextCommand;
		}
	}

	@Override
	protected void executeOnClassImplementation(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// this is called once when the CLASS ... IMPLEMENTATION Command is reached

		if (classInfo == null || classInfo != curClassInfo)
			return;
		
		// move PREPARE_ method above first FOR TESTING method
		if (configMovePrepareMethods.getValue()) {
			movePrepareMethods(code, classInfo);
		}
		
		// process all Commands in the IMPLEMENTATION section
		Command classImplementationStart = classInfo.getImplementationStart(); 
		Command command = classImplementationStart;
		while (command != null && command != classImplementationStart.getNextSibling()) {
			commandForErrorMsg = command;

			Command nextCommand = command.getNext();
			if (!isCommandBlocked(command)) {
				Token firstCode = command.getFirstCodeToken();

				if (isAnyComment(command.getFirstToken(), toDoComments)) {
					// remove to-do comments in own comment lines
					if (canRemoveToDo(command)) {
						removeAbapDocCommand(code, command);
					}
					
				} else if (command.firstCodeTokenIsKeyword("SELECT")) {
					// ensure an empty line between a PREPARE_...( ) call and 'SELECT'
					if (configEmptyLineAboveSelect.getValue() || configEmptyLineBelowSelect.getValue()) { 
						ensureEmptyLineAroundSelect(code, command);
					}
					
				} else if (firstCode != null && firstCode.textEndsWith("->insert_test_data(") && firstCode.hasChildren()) {
					// ensure an empty line before 'environment->insert_test_data( ... )' if the previous Command spans multiple lines
					if (configEmptyLineAboveInsertTestData.getValue()) {
						ensureEmptyLineBeforeInsertTestData(code, command);
					}
				} 
			}
			command = nextCommand;
		}
	}

	private void movePrepareMethods(Code code, ClassInfo classInfo) throws IntegrityBrokenException {
		Command firstForTestingMethod = null;
		Command classImplementationStart = classInfo.getImplementationStart();
		if (isCommandBlocked(classImplementationStart))
			return;
		
		Command command = classImplementationStart.getFirstChild();
		while (command != null) {
			// remember the next sibling now
			Command nextCommand = command.getNextNonCommentSibling();

			if (!command.isMethodStart()) {
				command = nextCommand;
				continue;
			}

			String methodName = command.getDefinedName();
			MethodInfo methodInfo = classInfo.getMethod(methodName);
			if (methodInfo != null && methodInfo.isForTesting) { // "!= null" pro forma
				if (firstForTestingMethod == null) {
					firstForTestingMethod = command.getStartOfAttachedComments();
				}
				
			} else if (StringUtil.startsWith(methodName, "PREPARE_", true) && firstForTestingMethod != null 
					&& !isCommandBlocked(command)) {
				// remember the Command after ENDMETHOD to continue there
				nextCommand = nextCommand.getNextNonCommentSibling();
				try {
					// move the PREPARE_... method implementation above the first FOR TESTING method implementation
					Section prepareMethod = Section.create(command.getStartOfAttachedComments(), command.getNextSibling());
					prepareMethod.removeFromCode();
					firstForTestingMethod.insertLeftSibling(prepareMethod);
					
					// put the 'rule use' on the CLASS .. IMPLEMENTATION command to ensure that if it is deactivated,  
					// reprocessing always covers the whole CLASS .. IMPLEMENTATION section
					code.addRuleUse(this, classImplementationStart); 
				} catch (UnexpectedSyntaxException e) {
					// skip this method
				}
			}
			command = nextCommand;
		}
	}

	/** removes ABAP Doc instructions in Tokens of [CLASS-]METHODS: chains */
	private void removeAbapDocTokens(Code code, Command command) throws UnexpectedSyntaxAfterChanges {
		boolean changed = false;
		Token token = command.getFirstToken();
		while (token != null) {
			Token nextToken = token.getNext();
			if (token.isAbapDocCommentLine() && isAnyComment(token, abapDocInstructions)) {
				nextToken.setLineBreaks(Math.max(nextToken.lineBreaks, token.lineBreaks));
				token.removeFromCommand();
				changed = true;
			}
			token = nextToken;
		}
		if (changed) {
			code.addRuleUse(this, command);
		}
	}

	private boolean canRemoveToDo(Command command) {
		RemoveCommentCondition condition = RemoveCommentCondition.forValue(configRemoveToDoComments.getValue());
		if (condition == RemoveCommentCondition.ALWAYS)
			return true;
		else if (condition == RemoveCommentCondition.NEVER)
			return false;
		// for RemoveCommentCondition.IF_VALUE_HAS_CONTENT, return true if the auto-generated VALUE #( ( ) ) constructor 
		// in the next Command was changed somehow
		Command nextCommand = command.getNextNonCommentCommand();
		Token valueToken = nextCommand.getFirstToken().getLastTokenOnSiblings(true, TokenSearch.ASTERISK, "VALUE");
		if (valueToken == null)
			return true;
		return !valueToken.matchesDeep(true, "VALUE", "#(", "(", ")", ")");
	}

	private boolean isAnyComment(Token token, String[] commentTexts) {
		if (!token.isCommentLine())
			return false;
		
		String prefix = token.isAbapDocComment() ? ABAP.ABAP_DOC_SIGN : ABAP.COMMENT_SIGN_STRING;
		String text = token.getText().substring(prefix.length()).trim();
		for (String commentText : commentTexts) {
			if (text.equals(commentText)) {
				return true;
			}
		}
		return false;
	}

	/** removes ABAP Doc instructions or to-do comments (in own comment lines) */
	private void removeAbapDocCommand(Code code, Command command) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Command nextCodeCommand = command.getNextNonCommentCommand();
		if (isCommandBlocked(nextCodeCommand))
			return;

		Command nextCommand = command.getNext();
		nextCommand.getFirstToken().setLineBreaks(Math.max(nextCommand.getFirstTokenLineBreaks(), command.getFirstTokenLineBreaks()));
		try {
			command.removeFromCode();
			if (nextCodeCommand != null) { // pro forma
				code.addRuleUse(this, nextCodeCommand);
			}
		} catch(UnexpectedSyntaxException e) {
			throw new UnexpectedSyntaxAfterChanges(this, e);
		}
	}
	
	/** ensures an empty line above and/or below a 'SELECT' statement */
	private void ensureEmptyLineAroundSelect(Code code, Command command) {
		if (configEmptyLineAboveSelect.getValue()) {
			// ensure an empty line before SELECT (or its attached comments), unless it is at the beginning of a block (i.e. after METHOD etc.)
			Command selectStart = command.getStartOfAttachedComments();
			if (selectStart.getPrevSibling() != null) {
				Token selectStartToken = selectStart.getFirstToken();
				if (selectStartToken.setLineBreaks(Math.max(selectStartToken.lineBreaks, 2))) {
					code.addRuleUse(this, selectStart);
				}
			}
		}
		if (configEmptyLineBelowSelect.getValue()) {
			// ensure an empty line after SELECT, unless it is at the end of a block
			Command nextSibling = command.getNextSibling();
			if (nextSibling != null) {
				Token nextStartToken = nextSibling.getFirstToken();
				if (nextStartToken.setLineBreaks(Math.max(nextStartToken.lineBreaks, 2))) {
					code.addRuleUse(this, nextSibling);
				}
			}
		}
	}

	/** ensures an empty line before 'environment->insert_test_data( ... )' if the previous Command spans multiple lines */
	private void ensureEmptyLineBeforeInsertTestData(Code code, Command command) {
		Token firstToken = command.getFirstToken();
		Command prevCommand = command.getPrev();
		if (!prevCommand.isCommentLine() && prevCommand.containsInnerLineBreaks(false)) {
			if (firstToken.setLineBreaks(Math.max(firstToken.lineBreaks, 2))) {
				code.addRuleUse(this, command);
			}
		}
	}
}
