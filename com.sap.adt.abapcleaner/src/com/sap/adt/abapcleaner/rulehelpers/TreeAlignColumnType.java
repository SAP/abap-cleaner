package com.sap.adt.abapcleaner.rulehelpers;

enum TreeAlignColumnType  {
   COMMAND, // may or may not be used; the IF, WHERE etc. may as well be aligned with the BOOL_OPERATOR

   // column types for LogicalExpressions with BindingLevel EQUIV / AND / OR
   OPEN_BRACKET_FOR_BOOL_OP, // "(" - followed by the "content" column types listed below for BindingLevel NOT / COMPARISON_OR_PREDICATE
   BOOL_OPERATOR, // "AND", "OR", "EQUIV" - followed by the "content" column types listed below for BindingLevel NOT / COMPARISON_OR_PREDICATE
   CLOSE_BRACKET_FOR_BOOL_OP, // ")" - after one or several BOOL_OPERATOR and "content" column types were added

   // "content" column types for LogicalExpressions with BindingLevel NOT / COMPARISON_OR_PREDICATE 
   // 1. opening
   OPEN_BRACKET_FOR_NOT, // "("
   NOT_KEYWORD, // "NOT"
   OPEN_BRACKET_FOR_REL, // "("
   // 2. a) either for RelationalExpressionType.PREDICATE_FUNCTION:
   REL_FUNCTION, // "line_exists( ... )", "matches( ... )"
   //    b) or for RelationalExpressionType.COMPARISON and .PREDICATE_EXPRESSION:
   REL_TERM1, // "a"
   REL_OPERATOR, // "=", "<=", "<>", "IS"
   REL_TERM2, // "b", "BOUND", "NOT BOUND", "ASSIGNED" etc.
   // 3. closing
   CLOSE_BRACKET_FOR_REL, // ")"
   CLOSE_BRACKET_FOR_NOT; // ")"

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static TreeAlignColumnType forValue(int value) {
      return values()[value];
   }
}