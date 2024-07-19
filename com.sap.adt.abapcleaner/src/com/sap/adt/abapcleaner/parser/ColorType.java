package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.comparer.TextBit;

/**
 * Enumeration of the color types available to display ABAP code.
 * {@link Token#toTextBits(int, boolean)} splits a {@link Token} into (one or several) {@link TextBit}s 
 * of one color type each.   
 */
public enum ColorType  {
   NONE,
   KEYWORD,
   DECLARATION_KEYWORD,
   IDENTIFIER, // e.g. variable or method name
   IDENTIFIER_WRITE_POS, // for highlighting variables in a write position
   DDL_IDENTIFIER_DATA_ELEMENT, // the data element used in "cast(... as data_element)"
   USUAL_OPERATOR,
   TOKEN_OPERATOR,
   NUMBER,
   STRING_LITERAL,
   COMMENT,
   DDL_KEYWORD,
   DDL_ANNOTATION,
   NON_ABAP;

   public static final int SIZE = java.lang.Integer.SIZE;
}