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

         boolean isAbapSql = command.isAbapSqlOperation();
         Token token = command.getFirstToken();
         do {
         	// does this Token start a logical expression? If so, get the Token following the logical expression  
         	Token lastInLogExpr = token.getLastTokenOfLogicalExpression();
         	if (lastInLogExpr != null) {
               if (executeOn(code, command, token, lastInLogExpr.getNextCodeToken(), releaseRestriction))
                  code.addRuleUse(this, command, token);
               // in ABAP SQL, a WHERE could contain an inner, independent WHERE, e.g. 'WHERE any_col IN ( SELECT ... WHERE ... )', 
               // therefore we must continue inside (not after) the logical expression
               token = isAbapSql ? token.getNextCodeToken() : lastInLogExpr.getNextCodeToken();
         	} else {
               token = token.getNextCodeToken();
         	}
         } while (token != null);
         command = command.getNext();
      }
   }
}
