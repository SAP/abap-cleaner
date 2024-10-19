package com.sap.adt.abapcleaner.rules.prettyprinter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Code;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.parser.TokenSearch;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;
import com.sap.adt.abapcleaner.rulebase.ConfigBoolValue;
import com.sap.adt.abapcleaner.rulebase.ConfigValue;
import com.sap.adt.abapcleaner.rulebase.Profile;
import com.sap.adt.abapcleaner.rulebase.RuleForDeclarations;
import com.sap.adt.abapcleaner.rulebase.RuleGroupID;
import com.sap.adt.abapcleaner.rulebase.RuleID;
import com.sap.adt.abapcleaner.rulebase.RuleReference;
import com.sap.adt.abapcleaner.rulebase.RuleSource;
import com.sap.adt.abapcleaner.rulehelpers.CamelCaseNames;
import com.sap.adt.abapcleaner.rulehelpers.ClassInfo;
import com.sap.adt.abapcleaner.rulehelpers.SelectClause;
import com.sap.adt.abapcleaner.rulehelpers.SelectQuery;
import com.sap.adt.abapcleaner.rulehelpers.VariableInfo;
import com.sap.adt.abapcleaner.rulehelpers.Variables;

public class CamelCaseInCdsTestRule extends RuleForDeclarations {
	private static final String[] prepareTestDataPrefixes = new String[] {
			ABAP.COMMENT_SIGN_STRING + "Prepare test data for '",
			ABAP.COMMENT_SIGN_STRING + " Prepare test data for '"
	};
	private static final String pseudoCommentNoWhere = ABAP.PSEUDO_COMMENT_EC_PREFIX + "CI_NOWHERE";
	
	private final static RuleReference[] references = new RuleReference[] {
			new RuleReference(RuleSource.ABAP_CLEANER) };

	private final static String getKey(String value) {
		return value.toUpperCase();
	}

	@Override
	public RuleID getID() { return RuleID.CAMEL_CASE_IN_CDS_TEST; }

	@Override
	public RuleGroupID getGroupID() { return RuleGroupID.PRETTY_PRINTER; }

	@Override
	public String getDisplayName() { return "Use CamelCase in test class for CDS view"; }

	@Override
	public String getDescription() { return "Changes known VDM CDS view names to CamelCase (in class name, variable names, literals, comments etc.)."; }

	@Override
	public String getHintsAndRestrictions() { return "This rule only works on local test classes with a \"!@testing <CDS view name> annotation, as created by the ADT command 'New ABAP Test Class'. Custom view and field names from rule '" + CamelCaseNameRule.displayName + "' are reused."; }

	@Override
	public LocalDate getDateCreated() { return LocalDate.of(2024, 10, 14); }

	@Override
	public RuleReference[] getReferences() { return references; }

	@Override
	public boolean dependsOnExternalFiles() { return true; } // this rule depends on the .txt files for custom view and field names

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
   			+ LINE_SEP + "    \" table names are put to CamelCase if they start with a prefix like td_, lt_, lts_ etc.,"
   			+ LINE_SEP + "    \" followed by the CDS view name from the respective TYPE definition (possibly shortened)"
   			+ LINE_SEP + "    DATA td_i_raperfoblgnwithtocurperda TYPE STANDARD TABLE OF i_raperfoblgnwithtocurperdamt WITH EMPTY KEY."
   			+ LINE_SEP + "    DATA td_i_businesspartner           TYPE STANDARD TABLE OF i_businesspartner WITH EMPTY KEY."
   			+ LINE_SEP + "    DATA td_i_customer                  TYPE STANDARD TABLE OF i_customer WITH EMPTY KEY."
   			+ LINE_SEP + "    DATA act_results                    TYPE STANDARD TABLE OF c_raperfoblgnwithtocurperdamt WITH EMPTY KEY."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    CLASS-METHODS class_setup RAISING cx_static_check."
   			+ LINE_SEP + "    CLASS-METHODS class_teardown."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    METHODS setup RAISING cx_static_check."
   			+ LINE_SEP + "    METHODS prepare_testdata."
   			+ LINE_SEP + "    METHODS aunit_for_cds_method FOR TESTING RAISING cx_static_check."
   			+ LINE_SEP + "ENDCLASS."
   			+ LINE_SEP + ""
   			+ LINE_SEP + ""
   			+ LINE_SEP + "CLASS ltc_c_raperfoblgnwithtocurperd IMPLEMENTATION."
   			+ LINE_SEP + "  METHOD class_setup."
   			+ LINE_SEP + "    \" CamelCase can even be applied to literals: inside the test environment, to_upper( ) is used"
   			+ LINE_SEP + "    environment = cl_cds_test_environment=>create("
   			+ LINE_SEP + "                    i_for_entity      = 'C_RAPERFOBLGNWITHTOCURPERDAMT'"
   			+ LINE_SEP + "                    i_dependency_list = VALUE #( type ='CDS_VIEW'"
   			+ LINE_SEP + "                                                 ( name = 'I_RAPERFOBLGNWITHTOCURPERDAMT' )"
   			+ LINE_SEP + "                                                 ( name = 'I_BUSINESSPARTNER' )"
   			+ LINE_SEP + "                                                 ( name = 'I_CUSTOMER' ) ) )."
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
   			+ LINE_SEP + "  METHOD prepare_testdata."
   			+ LINE_SEP + "    \" Prepare test data for 'i_raperfoblgnwithtocurperdamt'"
   			+ LINE_SEP + "    td_i_raperfoblgnwithtocurperda = VALUE #( ( ) )."
   			+ LINE_SEP + "    environment->insert_test_data( i_data = td_i_raperfoblgnwithtocurperda )."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \" Prepare test data for 'i_businesspartner'"
   			+ LINE_SEP + "    td_i_businesspartner = VALUE #( ( BusinessPartner = 'any' )"
   			+ LINE_SEP + "                                    ( BusinessPartner = 'other' ) )."
   			+ LINE_SEP + "    environment->insert_test_data( i_data = td_i_businesspartner )."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \" Prepare test data for 'i_customer'"
   			+ LINE_SEP + "    td_i_customer = VALUE #( ( Customer = 'Any' ) )."
   			+ LINE_SEP + "    environment->insert_test_data( i_data = td_i_customer )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "  METHOD aunit_for_cds_method."
   			+ LINE_SEP + "    prepare_testdata( )."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    \" since a SELECT on the view under test only accesses the test doubles that were prepared above,"
   			+ LINE_SEP + "    \" a WHERE clause is often not required, so #EC CI_NOWHERE can be added to satisfy code checks"
   			+ LINE_SEP + "    SELECT * FROM C_RAPerfOblgnWithToCurPerdAmt INTO TABLE @act_results."
   			+ LINE_SEP + ""
   			+ LINE_SEP + "    cl_abap_unit_assert=>fail( msg = 'Place your assertions here' )."
   			+ LINE_SEP + "  ENDMETHOD."
   			+ LINE_SEP + "ENDCLASS.";
   }

	final ConfigBoolValue configProcessClassName = new ConfigBoolValue(this, "ProcessClassName", "Apply CamelCase to class name", true);
	final ConfigBoolValue configProcessVariableNames = new ConfigBoolValue(this, "ProcessVariableNames", "Apply CamelCase to variable names", true);
	final ConfigBoolValue configProcessLiterals = new ConfigBoolValue(this, "ProcessLiterals", "Apply CamelCase to literals in CL_CDS_TEST_ENVIRONMENT=>CREATE calls", true);
	final ConfigBoolValue configProcessComments = new ConfigBoolValue(this, "ProcessComments", "Apply CamelCase to generated comments 'Prepare test data for ...'", true);
	final ConfigBoolValue configAddPseudoCommentNoWhere = new ConfigBoolValue(this, "AddPseudoCommentNoWhere", "Add pseudo comment \"#EC CI_NOWHERE to SELECT on tested view", true);

	private final ConfigValue[] configValues = new ConfigValue[] { configProcessClassName, configProcessVariableNames, configProcessLiterals, configProcessComments, configAddPseudoCommentNoWhere }; 

	@Override
	public ConfigValue[] getConfigValues() { return configValues; }

	public CamelCaseInCdsTestRule(Profile profile) {
		super(profile);
		initializeConfiguration();
	}

	private ClassInfo curClassInfo = null;
	private HashMap<VariableInfo, String> camelCaseNameOfVariable = new HashMap<>();
	private HashSet<String> doubledEntityNames = new HashSet<>();
	
	@Override
	protected void executeOnClassDefinition(Code code, ClassInfo classInfo, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// cp. CdsTestClassRule.executeOnClassDefinition()
		curClassInfo = null;
		camelCaseNameOfVariable.clear();
		doubledEntityNames.clear();
		
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
				// right away apply CamelCase to the "!@testing <view name> annotation 
				String camelCaseViewName = applyToTestingAnnotation(code, comment); 
				if (camelCaseViewName != null && configProcessClassName.getValue()) {
					// apply to the class name, which is likely to be derived from one of the @testing annotations 
					applyToDeclaration(code, declarationCommand.getFirstCodeToken().getNextCodeSibling(), camelCaseViewName, null); // variable = null is used for the LTC_ class name
				}
				// continue, because a hierarchy test may contain multiple "!@testing annotations!
			}
			comment = comment.getPrevSibling();
		}
		if (!foundTestingAnno) 
			return;
		curClassInfo = classInfo;
		
		// build hash map from attributes
		Variables attributes = classInfo.getVariables();
		for (VariableInfo attribute : attributes.localsInDeclarationOrder) {
			addDeclaration(code, attribute);
		}
	}

	@Override
	protected void executeOn(Code code, Command methodStart, Variables localVariables, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
		// only process methods if the class fulfilled the conditions of executeOn(Code, ClassInfo, int)
		if (curClassInfo == null || localVariables.getClassInfo() != curClassInfo)
			return;
		
		// enhance hash map with local variables (this can safely be done for multiple methods, because the HashMap key is the local VariableInfo instance)
		for (VariableInfo localVariable : localVariables.localsInDeclarationOrder) {
			addDeclaration(code, localVariable);
		}
		
		// if the CLASS_SETUP method is processed, also process the class name of the CLASS ... IMPLEMENTATION command
		String methodName = methodStart.getDefinedName(); 
		if (methodName != null && methodName.equalsIgnoreCase("CLASS_SETUP")) { // "!= null" pro forma
			Command classImplStart = methodStart.getParent();
			if (classImplStart != null && classImplStart.firstCodeTokenIsKeyword("CLASS")) { // pro forma
				applyToUsage(code, classImplStart.getFirstCodeToken().getNextCodeSibling(), null); // VariableInfo = null is used for the LTC_ class name
			}
		}
		
		// process the Commands in this method
		Command command = methodStart;
		while (command != null && command != methodStart.getNextSibling()) {
			commandForErrorMsg = command;

			Command nextCommand = command.getNext();
			if (!isCommandBlocked(command)) {
				executeOnCommandInMethod(code, command, localVariables.getClassInfo(), localVariables);
			}
			command = nextCommand;
		}
	}

	private String applyToTestingAnnotation(Code code, Command comment) {
		if (isCommandBlocked(comment))
			return null;
		
		Token commentToken = comment.getFirstToken();
		String annotationText = commentToken.getText().stripTrailing();
		int testingPos = annotationText.indexOf("testing ") + "testing ".length(); // must be lower case for the annotation to work
		while (testingPos < annotationText.length() && annotationText.charAt(testingPos) == ' ')
			++testingPos;
		
		if (applyCamelCaseTo(code, commentToken, annotationText.substring(0, testingPos), "", false)) {
			return commentToken.getText().substring(testingPos);
		} else {
			return null;
		}
	}
	
	private void addDeclaration(Code code, VariableInfo variable) {
		Token declarationToken = variable.declarationToken;
		if (isCommandBlocked(declarationToken.getParentCommand()))
			return;
		
		// determine whether this is a table definition and find the table type 
		Token token = declarationToken;
		Token tableType = null;
		while (token != null && !token.isComma()) {
			if (token.matchesOnSiblings(true, "TABLE", "OF", TokenSearch.ANY_IDENTIFIER)) {
				tableType = token.getNextCodeSibling().getNextCodeSibling();
				break;
			}
			token = token.getNextCodeSibling();
		}
		if (tableType == null)
			return;

		// apply CamelCase to the table type
		if (applyCamelCaseTo(code, tableType, "", "", false) && configProcessVariableNames.getValue()) {
			// apply CamelCase to the variable name, if it consists of a prefix and the table type, e.g. LT_I_ANYVIEW
			applyToDeclaration(code, declarationToken, tableType.getText(), variable);
		}
	}
	
	private void applyToDeclaration(Code code, Token declarationToken, String camelCaseViewName, VariableInfo variable) {
		if (isCommandBlocked(declarationToken.getParentCommand()))
			return;
		
		// determine the variable name, which is expected to have a prefix like TD_, LT_, LTS_, MT_, MTS_ etc.
		String name = declarationToken.getText();
		int underscorePos = name.indexOf('_');
		if (underscorePos < 0 || underscorePos > 4) 
			return;
		int prefixLength = underscorePos + 1;
		String nameWithoutPrefix = name.substring(prefixLength);
		int maxLength = ABAP.MAX_VARIABLE_NAME_LENGTH - prefixLength;
		
		// if the identifier contains a prefix and the CDS View name (possibly shortened to 30 or 29 chars), 
		// change it to upper case and add it to the hash table
		if (camelCaseViewName.length() > maxLength) {
			camelCaseViewName = camelCaseViewName.substring(0, maxLength);
		}
		// sometimes, variable names are shortened to 29 chars
		if (!nameWithoutPrefix.equalsIgnoreCase(camelCaseViewName) && camelCaseViewName.length() > maxLength - 1) {
			camelCaseViewName = camelCaseViewName.substring(0, maxLength - 1);
		}
		if (nameWithoutPrefix.equalsIgnoreCase(camelCaseViewName)) {
			String newName = name.substring(0, prefixLength) + camelCaseViewName;
			declarationToken.setText(newName, false);
			code.addRuleUse(this, declarationToken.getParentCommand());
			camelCaseNameOfVariable.put(variable, newName);
		} 
	}
	
	private void applyToUsage(Code code, Token token, VariableInfo variable) {
		Command command = token.getParentCommand();
		if (isCommandBlocked(command))
			return;
		String newName = camelCaseNameOfVariable.get(variable);
		if (newName != null && token.setText(newName, false)) {
			code.addRuleUse(this, token.getParentCommand());
		}
	}
	
	private void executeOnCommandInMethod(Code code, Command command, ClassInfo classInfo, Variables localVariables) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Token firstCode = command.getFirstCodeToken();

		if (firstCode != null && firstCode.matchesOnSiblings(true, TokenSearch.ANY_IDENTIFIER, "=", 
				"cl_cds_test_environment=>create(|cl_cds_test_environment=>create_for_multiple_cds(")) {
			// apply CamelCase to literals in cl_cds_test_environment=>create(...)|create_for_multiple_cds(...) Commands
			String methodName = localVariables.getMethodInfo().name;
			boolean isSetupMethod = AbapCult.stringEqualsAny(true, methodName, "CLASS_SETUP", "SETUP");
			if (configProcessLiterals.getValue())
				applyToLiterals(code, command, isSetupMethod);
			return;
			
		} else if (command.isCommentLine() && command.getFirstToken().textStartsWithAny(prepareTestDataPrefixes)) {
			// apply CamelCase to the comment "Prepare test data for 'i_anyview'"
			if (configProcessComments.getValue())
				applyToComment(code, command);
			return;

		} else if (command.firstCodeTokenIsKeyword("SELECT")) {
			// add the pseudo-comment "#EC CI_NOWHERE to a SELECT from a doubled entity without WHERE clause
			if (configAddPseudoCommentNoWhere.getValue()) 
				addPseudCommentNoWhere(code, command);
			return;
		
		} else {
			// apply CamelCase to variables usages
			if (classInfo != null && configProcessVariableNames.getValue()) { // "!= null" pro forma
				applyToVarUsages(code, command, classInfo, localVariables);
			}
		}
	}

	private void applyToLiterals(Code code, Command command, boolean isInSetupMethod) {
		// apply CamelCase to literals in the following commands:
		// 'environment = cl_cds_test_environment=>create( [i_for_entity =] 'I_ANYVIEW' 
		//                                                 i_dependency_list = VALUE #( ( name = 'i_otherview' type ='CDS_VIEW' ) ... ) ).'
		// 'environment = cl_cds_test_environment=>create_for_multiple_cds( [i_for_entities =] VALUE #( ... ( i_for_entity = 'I_ANYVIEW' )
		//                                                                                                  ( i_for_entity = 'I_OTHERVIEW' ) ... ).'

		boolean addToTestedDoubles = isInSetupMethod;
		Token firstCode = command.getFirstCodeToken();
		Token parentToken = firstCode.getNextCodeSibling().getNextCodeSibling();
		Token token = parentToken.getFirstChild();
		while (token != null && token != parentToken.getNextCodeSibling()) {
			if (token.getPrev() == parentToken && token.isLiteral()) {
				applyCamelCaseTo(code, token, ABAP.QUOT_MARK_STRING, ABAP.QUOT_MARK_STRING, addToTestedDoubles);
			} else if (token.matchesOnSiblings(true, "i_for_entity|name", "=", TokenSearch.ANY_LITERAL)) {
				token = token.getNextCodeSibling().getNextCodeSibling();
				applyCamelCaseTo(code, token, ABAP.QUOT_MARK_STRING, ABAP.QUOT_MARK_STRING, addToTestedDoubles);
			}
			token = token.getNextCodeToken();
		}
	}
	
	/** applies CamelCase to the comment "Prepare test data for 'i_anyview'" */
	private void applyToComment(Code code, Command command) {
		Token firstToken = command.getFirstToken();
		for (String prefix : prepareTestDataPrefixes) {
			if (firstToken.textStartsWith(prefix)) {
				applyCamelCaseTo(code, firstToken, prefix, "'", false);
				break;
			}
		}
	}

	/** adds the pseudo-comment "#EC CI_NOWHERE to a SELECT statement if it selects from one of the views 
	    for which the CDS test double environment was initialized */
	private boolean addPseudCommentNoWhere(Code code, Command command) throws IntegrityBrokenException {
		// only add the pseudo-comment ...

		// - to single queries	
		ArrayList<SelectQuery> queries;
		try {
			queries = SelectQuery.createQueryChainsFrom(command);
		} catch (UnexpectedSyntaxException e) {
			return false;
		}
		if (queries.size() != 1)
			return false;
		SelectQuery query = queries.get(0);
		if (query.getNextQuery() != null)
			return false;

		// - if no WHERE clause is found
		if (query.hasClause(SelectClause.WHERE))
			return false;

		// - if a CDS double was created for the data source
		Term fromQuery = query.getClause(SelectClause.FROM);
		Token dataSource = fromQuery.firstToken.getNextCodeSibling();
		String dataSourceName = StringUtil.removeSuffix(dataSource.getText(), "(", false);
		if (!dataSource.isIdentifier() || !doubledEntityNames.contains(getKey(dataSourceName))) 
			return false;
		
		// - if no such pseudo-comment is found 
		Token token = command.getFirstToken();
		while (token != null) {
			if (token.isComment() && token.textEquals(pseudoCommentNoWhere))
				return false;
			token = token.getNext();
		}

		// - if the last Token is not a comment
		Token lastToken = command.getLastToken();
		if (lastToken.isComment()) 
			return false;

		lastToken.insertRightSibling(Token.createForAbap(0, 1, pseudoCommentNoWhere, lastToken.sourceLineNum));
		code.addRuleUse(this, command);
		return true;
	}

	/** applies CamelCase to variables usages */
	private void applyToVarUsages(Code code, Command command, ClassInfo classInfo, Variables localVariables) {
		Token token = command.getFirstCodeToken();
		while (token != null) {
			Token nextCode = token.getNextCodeSibling();
			if (token.getParent() != null && nextCode != null && nextCode.textEquals("=")) {
				// skip token, because this seems to be a formal parameter
			} else if (token.isIdentifier()) {
				// determine the local variable or attribute, if any
				VariableInfo variableInfo = classInfo.getLocalVariableOrAttribute(localVariables, token.getText(), false, true);
				if (variableInfo != null) {
					applyToUsage(code, token, variableInfo);
				}
			}
			token = token.getNextCodeToken();
		}
	}

	private boolean applyCamelCaseTo(Code code, Token token, String skipPrefix, String skipSuffix, boolean addToTestedDoubles) {
		// only process the token if one of the expected skipPrefixes is found
		String text = token.getText();
		int bitStart = skipPrefix.length();
		if (!StringUtil.isNullOrEmpty(skipPrefix) && !AbapCult.stringStartsWith(text, skipPrefix, true)) // pro forma
			return false;

		// only process the token if the expected skipSuffix is found
		int bitEnd = text.length() - skipSuffix.length();
		if (!StringUtil.isNullOrEmpty(skipSuffix) && !AbapCult.stringEndsWith(text, skipSuffix, true)) // pro forma
			return false;
		
		// apply CamelCase to the text bit between the skipPrefix and skipSuffix
		String textBit = text.substring(bitStart, bitEnd);
		CamelCaseNames knownNames = CamelCaseNames.getViewNames();
		String changedTextBit = knownNames.applyCamelCaseTo(textBit, false, false, parentProfile);
		if (changedTextBit == null)  
			return false;
		if (addToTestedDoubles)
			doubledEntityNames.add(getKey(changedTextBit));
		
		// change the token text (its length will never change)
		if (token.setText(text.substring(0, bitStart) + changedTextBit + text.substring(bitEnd), false))
			code.addRuleUse(this, token.getParentCommand());

		// always return true if a CamelCase name is known (regardless of whether the Token was changed) 
		return true;
	}
}
