package com.sap.adt.abapcleaner.programbase;

import com.sap.adt.abapcleaner.parser.*;

/**
 * Represents an exception that is thrown when an integrity check detects 
 * invalid object references after modifying {@link Code} with a {@link Rule}, 
 * indicating a programming error 
 * (see testReferentialIntegrity() methods in classes {@link Code}, {@link Command} and {@link Token}).  
 */
public class IntegrityBrokenException extends UnexpectedSyntaxAfterChanges {
	private static final long serialVersionUID = 1L;

	public IntegrityBrokenException(Code code, String message) {
      super(null, code, message);
   }

   public IntegrityBrokenException(Command command, String message) {
      super(null, command, message);
   }

   public IntegrityBrokenException(Token token, String message) {
      super(null, token, message);
   }
}