package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.comparer.TextBit;

/**
 * Enumeration of the color types available to display ABAP code.
 * {@link Token#toTextBits(int)} splits a {@link Token} into (one or several) {@link TextBit}s 
 * of one color type each.   
 */
public enum ColorType  {
   NONE,
   KEYWORD,
   DECLARATION_KEYWORD,
   IDENTIFIER, // e.g. variable or method name
   IDENTIFIER_WRITE_POS, // for highlighting variables in a write position
   USUAL_OPERATOR,
   TOKEN_OPERATOR,
   NUMBER,
   STRING_LITERAL,
   COMMENT,
   NON_ABAP;

   public static final int SIZE = java.lang.Integer.SIZE;

   public int getValue() {
      return this.ordinal();
   }

   public static ColorType forValue(int value) {
      return values()[value];
   }
}