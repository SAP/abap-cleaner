package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;

import java.util.*;

/**
 * <p>Compiles metrics on ABAP keywords. This class is used on two levels of hierarchy:</p>
 * <ul>
 * <li>as part of {@link CodeMetrics}, it measures the frequency of ABAP keywords 
 * within the {@link Command}s of a {@link Code}</li> 
 * <li>as part of {@link KeywordCount}, it measures the frequency of ABAP keywords 
 * within the context of other ABAP keywords. This helps to evaluate which keywords are used together how often.</li>
 * </ul> 
 */
class KeywordMetrics {
   private HashMap<String, KeywordCount> keywordCounts = new HashMap<String, KeywordCount>();

   final void addKeyword(String keyword, String sourceName, int sourceLineNum, String[] contextKeywords) {
      addKeyword(keyword, sourceName, sourceLineNum, contextKeywords, 0);
   }
   final void addKeyword(String keyword, String sourceName, int sourceLineNum, String[] contextKeywords, int contextStartIndex) {
      if (keywordCounts.containsKey(keyword))
         keywordCounts.get(keyword).addInstance(contextKeywords, contextStartIndex);
      else {
         String firstInstance = (sourceName == null) ? null : sourceName + ", line " + Cult.format(sourceLineNum);
         keywordCounts.put(keyword, new KeywordCount(keyword, firstInstance, contextKeywords, contextStartIndex));
      }
   }

   final void addContextKeywords(String[] contextKeywords, int contextStartIndex) {
      for (int i = contextStartIndex; i < contextKeywords.length; ++i) {
         String contextKeyword = contextKeywords[i];
         if (keywordCounts.containsKey(contextKeyword))
            keywordCounts.get(contextKeyword).addInstance();
         else
            keywordCounts.put(contextKeyword, new KeywordCount(contextKeyword, null));
      }
   }

   final String toList() {
      ArrayList<String> result = new ArrayList<String>();
      for (KeywordCount keywordCount : keywordCounts.values())
         result.add(keywordCount.toLine());
      Collections.sort(result);
      return StringUtil.join(System.lineSeparator(), result.toArray(new String[0]));
   }

   final String toContextInfo() {
      ArrayList<KeywordCount> results = new ArrayList<KeywordCount>(keywordCounts.values());
      Collections.sort(results, new KeywordCount.ComparerByCount());
      StringBuilder result = new StringBuilder();
      for (KeywordCount keywordCount : results) {
         if (result.length() > 0)
            result.append(", ");
         result.append(keywordCount.toContextInfo());
      }
      return result.toString();
   }
}