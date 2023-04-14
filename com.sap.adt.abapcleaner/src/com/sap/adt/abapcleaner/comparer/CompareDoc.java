package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.programbase.*;
import java.util.*;

/**
 * <p>Represents a single text document which is split into {@link CompareLine}s 
 * and can be compared with another {@link CompareDoc}
 * (i.e. a changed version of the text document) 
 * using the {@link #compareTo(CompareDoc, IProgress) compareTo()} method. 
 * The result of the comparison is a {@link DiffDoc}.</p>
 */
public class CompareDoc {
   private static final double MATCH_THRESHOLD_IN_SEQUENCE = 0.3;
   private static final double MATCH_THRESHOLD_OUT_OF_SEQUENCE = 0.3;

   private ArrayList<CompareLine> lines = new ArrayList<CompareLine>();

   public static CompareDoc createFromDisplayLines(ArrayList<DisplayLine> lines) {
   	return new CompareDoc(lines);
   }
   
   public static CompareDoc createFromText(String text, String lineSeparator) {
   	return new CompareDoc(text, lineSeparator);
   }
   
   private CompareDoc(ArrayList<DisplayLine> lines) {
      if (lines == null)
         throw new NullPointerException("lines");
      for (DisplayLine line : lines)
         this.lines.add(new CompareLine(line));
   }

   private CompareDoc(String text, String lineSeparator) {
      if (text == null)
         throw new NullPointerException("text");

      String[] lines = StringUtil.split(text, lineSeparator, false);
      for (int i = 0; i < lines.length; ++i)
         this.lines.add(new CompareLine(DisplayLine.create(null, lines[i], i)));
   }


	/**
	 * 
	 * @param other
	 * @param progress  may be null
	 */
   public final DiffDoc compareTo(CompareDoc other, IProgress progress) throws CompareException {
      // algorithm: 
      // - starting from items (1, 1), match consecutively; if items do not match, slowly increase search radius (+0, +1, +2, ...) 
      //   and find matches for any(!) two unmatched items within the radius; once they are found, continue with radius = 0 
      // - for items not matched by this, perform an 'inner' pass in which unmatched items are compared by the percentage of common text 
      //   (add up the length of all common words and divide by the (average) total length of all words of the two lines) 
      //   Compare only within the vertical 'windows' that are left over by the 'outer' pass! 
      // - items that are still unmatched, are considered to be added / deleted 
      // - create list of line pairs (including unmatched lines with a non-line on the other side) with line status equal / changed / deleted / added 
      // - for matched (but not completely identical) lines/words (incl. whitespace!), calculate where to highlight differences left/right 

      if (other == null)
         throw new NullPointerException("other");

      int maxA = lines.size();
      int maxB = other.lines.size();
      HashMap<String, Integer> lineScopeA = new HashMap<String, Integer>();
      HashMap<String, Integer> lineScopeB = new HashMap<String, Integer>();

      if (progress != null)
         progress.report(TaskType.COMPARER, 0.0);

      // find pairs of matching (or otherwise, at least similar) lines
      int indexA = 0;
      int indexB = 0;
      try {
         while (indexA < maxA && indexB < maxB) {
            if (progress != null && progress.isCancellationPending())
               return null;
            if (progress != null)
               progress.report(TaskType.COMPARER, (indexA + indexB) / (double)(maxA + maxB));

            // is next line pair in sequence a direct match?
            CompareLine lineA = lines.get(indexA);
            CompareLine lineB = other.lines.get(indexB);
            if (lineA.matchesCurrentOrOriginalCommand(lineB) && lineA.simplifiedText.equals(lineB.simplifiedText)) {
               lineA.setMatch(lineB);
               ++indexA;
               ++indexB;
               continue;
            }

            // gradually increase search distance to find next line pair with a direct match;
            // with the HashMap's hash algorithm, this only requires linear runtime
            boolean found = false;
            // do not match empty lines, because confidence is too low that they really are a match 
            String compareStringA = lineA.getCompareString();
            if (!StringUtil.isNullOrEmpty(compareStringA))
               lineScopeA.put(compareStringA, indexA);
            String compareStringB = lineB.getCompareString();
            if (!StringUtil.isNullOrEmpty(compareStringB))
               lineScopeB.put(compareStringB, indexB);
            int testIndexA = indexA + 1;
            int testIndexB = indexB + 1;
            while (testIndexA < maxA || testIndexB < maxB) {
               if (testIndexA < maxA) {
                  lineA = lines.get(testIndexA);
                  compareStringA = lineA.getCompareString();
                  if (lineScopeB.containsKey(compareStringA)) {
                     int matchIndexB = lineScopeB.get(compareStringA);
                     if (indexB < matchIndexB)
                        findSimilarLines(indexA, testIndexA, indexB, matchIndexB, other);
                     lineA.setMatch(other.lines.get(matchIndexB));
                     indexA = testIndexA + 1;
                     indexB = matchIndexB + 1;
                     found = true;
                     break;
                  }
                  // if two lines have the same text, it is enough to enter the first one
                  if (!StringUtil.isNullOrEmpty(compareStringA) && !lineScopeA.containsKey(compareStringA))
                     lineScopeA.put(compareStringA, testIndexA);
               }
               if (testIndexB < maxB) {
                  lineB = other.lines.get(testIndexB);
                  compareStringB = lineB.getCompareString();
                  if (lineScopeA.containsKey(compareStringB)) {
                  	int matchIndexA = lineScopeA.get(compareStringB);
                     if (indexA < matchIndexA)
                        findSimilarLines(indexA, matchIndexA, indexB, testIndexB, other);
                     lines.get(matchIndexA).setMatch(lineB);
                     indexA = matchIndexA + 1;
                     indexB = testIndexB + 1;
                     found = true;
                     break;
                  }
                  // if two lines have the same text, it is enough to enter the first one
                  if (!StringUtil.isNullOrEmpty(compareStringB) && !lineScopeB.containsKey(compareStringB))
                     lineScopeB.put(compareStringB, testIndexB);
               }
               ++testIndexA;
               ++testIndexB;
            }

            // clear Dictionaries for next usage, also freeing memory
            lineScopeA.clear();
            lineScopeB.clear();

            if (!found) {
            	// at the end of the document, find similar lines for whatever could not be matched yet
            	findSimilarLines(indexA, maxA, indexB, maxB, other);
               break;
            }
         }
      } catch (RuntimeException ex) {
         throw new CompareException(indexA, indexB, ex);
      }

      // create resulting DiffDoc  
      DiffDoc result = new DiffDoc();
      indexA = 0;
      indexB = 0;
      try {
         while (indexA < maxA || indexB < maxB) {
            CompareLine lineA = (indexA < maxA) ? lines.get(indexA) : null;
            CompareLine lineB = (indexB < maxB) ? other.lines.get(indexB) : null;
            if (lineA != null && lineB != null && lineA.hasMatch() && lineB.hasMatch()) {
               if (lineA.getMatch() != lineB)
                  throw new IllegalStateException("Expected lines A and B to match!");
               // the status depends on exact equality of the two lines: 
               LineStatus status = (lineA.getText().equals(lineB.getText())) ? LineStatus.EQUAL : LineStatus.CHANGED;
               if (status == LineStatus.CHANGED) {
                  // compare on word level
                  lineA.compareTo(lineB);
               }
               result.add(lineA, lineB, status);
               ++indexA;
               ++indexB;
               continue;
            }

            while (indexA < maxA && indexB < maxB) {
               lineA = lines.get(indexA);
               lineB = other.lines.get(indexB);
               if (lineA.hasMatch() || lineB.hasMatch())
                  break;
               result.add(lineA, lineB, LineStatus.LEFT_DELETED_RIGHT_ADDED);
               ++indexA;
               ++indexB;
            }

            while (indexA < maxA) {
               lineA = lines.get(indexA);
               if (lineA.hasMatch())
                  break;
               result.add(lineA, null, LineStatus.LEFT_DELETED);
               ++indexA;
            }
            while (indexB < maxB) {
               lineB = other.lines.get(indexB);
               if (lineB.hasMatch())
                  break;
               result.add(null, lineB, LineStatus.RIGHT_ADDED);
               ++indexB;
            }
         }
         return result;
      } catch (RuntimeException ex) {
         throw new CompareException(indexA, indexB, ex);
      }
   }

   private void findSimilarLines(int startIndexA, int maxA, int startIndexB, int maxB, CompareDoc other) {
      int indexA = startIndexA;
      int indexB = startIndexB;
      while (indexA < maxA && indexB < maxB) {
         // is the next line pair in the sequence a similarity match?
         CompareLine lineA = lines.get(indexA);
         CompareLine lineB = other.lines.get(indexB);
         if (lineA.matchesCurrentOrOriginalCommand(lineB) && lineA.calcMatchRatioWith(lineB) >= MATCH_THRESHOLD_IN_SEQUENCE) {
            lineA.setMatch(lineB);
            ++indexA;
            ++indexB;
            continue;
         }

         // gradually increase search distance to find the next line pair with a similarity match; 
         // since each pair requires extra computation, this requires quadratic runtime
         boolean found = false;
         int testIndexA = indexA + 1;
         int testIndexB = indexB + 1;
         while (testIndexA < maxA || testIndexB < maxB) {
            if (testIndexA < maxA) {
               lineA = lines.get(testIndexA);
               int testMaxB = Math.min(testIndexB + 1, maxB);
               for (int testB = indexB; testB < testMaxB; ++testB) {
                  lineB = other.lines.get(testB);
                  if (lineA.matchesCurrentOrOriginalCommand(lineB) && lineA.calcMatchRatioWith(lineB) >= MATCH_THRESHOLD_OUT_OF_SEQUENCE) {
                     lineA.setMatch(lineB);
                     indexA = testIndexA + 1;
                     indexB = testB + 1;
                     found = true;
                     break;
                  }
               }
               if (found)
                  break;
            }
            if (testIndexB < maxB) {
               lineB = other.lines.get(testIndexB);
               int testMaxA = Math.min(testIndexA + 1, maxA);
               for (int testA = indexA; testA < testMaxA; ++testA) {
                  lineA = lines.get(testA);
                  if (lineA.getParentCommand() == lineB.getParentCommand() && lineA.calcMatchRatioWith(lineB) >= MATCH_THRESHOLD_OUT_OF_SEQUENCE) {
                     lineA.setMatch(lineB);
                     indexA = testA + 1;
                     indexB = testIndexB + 1;
                     found = true;
                     break;
                  }
               }
               if (found)
                  break;
            }
            ++testIndexA;
            ++testIndexB;
         }

         if (!found)
            return;
      }

   }
}
