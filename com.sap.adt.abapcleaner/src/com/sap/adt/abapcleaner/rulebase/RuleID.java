package com.sap.adt.abapcleaner.rulebase;

/**
 * Determines the execution order of the Rules. 
 * 
 * Dependencies between Rules are expressed with the {@link Rule#getDependentRules()} method: 
 * This method returns the {@link RuleID}s of Rules that must only be executed AFTER the Rule 
 * that overrides the method. The dependency is enforced by RuleTest.testDependencies().    
 */
public enum RuleID  {
   // Empty Lines
   EMPTY_LINES_WITHIN_METHODS,
   EMPTY_LINES_OUTSIDE_METHODS,
   EMPTY_LINES_IN_CLASS_DEFINITION,
   
   // Spaces 
   SPACES_IN_EMPTY_BRACKETS, // for compatibility, this old RuleID is kept, while the class was renamed from SpacesInEmptyBracketsRule to SpaceAroundTextLiteralRule
   CLOSING_BRACKETS_POSITION,  
   SPACE_BEFORE_PERIOD,
   SPACE_AROUND_COMMENT_SIGN, // merged version of SPACE_BEFORE_COMMENT_SIGN and SPACE_AFTER_COMMENT_SIGN 
   NEEDLESS_SPACES, 
   
   // Declarations
   DECLARATION_CHAIN, // for compatibility, this old RuleID is kept, while the class was renamed from DeclarationChainRule to ChainRule
   NEEDLESS_CLEAR,
   LOCAL_DECLARATION_ORDER,
   UNUSED_PARAMETERS,
   UNUSED_VARIABLES,
   CHAIN_OF_ONE,
   IMPLICIT_TYPE,
   FINAL_VARIABLE,
   CLASS_DEFINITION,
   ESCAPE_CHAR_FOR_PARAMS,
   EMPTY_SECTIONS,
   ABAP_DOC_PARAMETERS,
   ABAP_DOC_LANG,

   // Syntax [obsolete/outdated]
   // - comments and pragmas
   COMMENT_TYPE,
   END_OF_COMMENT,
   PSEUDO_COMMENT,
   PRAGMA_POSITION,
   TYPO,
   // - operators, assignment
   EQUALS_SIGN_CHAIN,
   CALCULATION_ASSIGNMENT,
   COMPARISON_OPERATOR,  
   NOT_IS,
   LOGICAL_OPERATOR_POSITION,
   STRING_TEMPLATE,
   NEEDLESS_PARENTHESES,
   // - empty commands
   EMPTY_COMMAND,
   // - VALUE statements
   VALUE_STATEMENT,
   // - method calls
   SELF_REFERENCE_ME,
   RECEIVING_KEYWORD,
   EXPORTING_KEYWORD,
   
   // Commands [obsolete/outdated]
   // - CHECK, IF ... CONTINUE/RETURN
   CHECK_OUTSIDE_LOOP,
   CHECK_IN_LOOP,
   IF_BLOCK_AT_LOOP_END,
   IF_BLOCK_AT_METHOD_END,
   // - method calls 
   CALL_METHOD,
   CREATE_OBJECT,
   RAISE_TYPE,
   // - misc
   ADD_TO_ETC,
   MOVE_TO,
   TRANSLATE,
   CONDENSE,
   DESCRIBE_TABLE,
   READ_TABLE,
   // - ASSERTs
   ASSERT_EQUALS_BOOLEAN,
   ASSERT_EQUALS_SUBRC,
   ASSERT_CLASS,
   ASSERT_PARAMETER_ORDER,

   // Pretty Printer
   UPPER_AND_LOWER_CASE,
   CAMEL_CASE_NAME,
   INSET,

	// Alignment 
   ALIGN_ABAP_DOC,
   ALIGN_METHODS_DECLARATION,
   ALIGN_METHODS_FOR_TESTING,
   ALIGN_METHODS_REDEFINITION,
   ALIGN_ALIASES_FOR,
   ALIGN_DECLARATIONS,
   ALIGN_ASSIGNMENTS,
   ALIGN_WITH_SECOND_WORD,
   ALIGN_CLEAR_FREE_AND_SORT,
   ALIGN_SELECT_CLAUSES,
   ALIGN_SELECT_FROM,
   ALIGN_SELECT_LISTS,
   ALIGN_PARAMETERS, 
   ALIGN_LOGICAL_EXPRESSIONS,
   ALIGN_COND_EXPRESSIONS,
   ALIGN_FORM_DECLARATION,
   ALIGN_PERFORM, 
   
   // DDL Annotations
   DDL_ANNO_LAYOUT,
   DDL_ANNO_NESTING,
   
   // DDL Positions
   DDL_POSITION_SELECT,
   DDL_POSITION_JOIN,
   DDL_POSITION_ASSOCIATION,
   DDL_POSITION_BRACES;

	public static final int SIZE = java.lang.Integer.SIZE;

	public int getValue() { return this.ordinal(); }

	public static RuleID forValue(int value) {
		return values()[value];
	}
}