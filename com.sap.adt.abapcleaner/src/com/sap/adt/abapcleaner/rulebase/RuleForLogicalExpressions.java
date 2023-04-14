package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;

/** 
 base class for a Rule that works on logical expressions
*/
public abstract class RuleForLogicalExpressions extends Rule {
   protected abstract boolean executeOn(Code code, Command command, Token keyword, Token end, int releaseRestriction) throws UnexpectedSyntaxAfterChanges;

   protected RuleForLogicalExpressions(Profile profile) {
      super(profile);
   }

   @Override
   public void executeOn(Code code, int releaseRestriction) throws UnexpectedSyntaxAfterChanges {
      if (code == null)
         throw new NullPointerException("code");

      Command command = code.firstCommand;
      while (command != null) {
         commandForErrorMsg = command;
         if (isCommandBlocked(command) || command.containsChainColon()) {
            command = command.getNext();
            continue;
         }

         Token token = command.getFirstToken();
         do {
         	// does this Token start a logical expression? If so, get the Token following the logical expression  
         	Token end = token.getEndOfLogicalExpression();
         	if (end != null) {
               if (executeOn(code, command, token, end, releaseRestriction))
                  code.addRuleUse(this, command, token);
               token = end;
         	} else {
               token = token.getNext();
         	}
         } while (token != null);
         command = command.getNext();
      }
   }
}
