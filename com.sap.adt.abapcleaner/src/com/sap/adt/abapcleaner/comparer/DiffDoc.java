package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.parser.*;
import com.sap.adt.abapcleaner.rulebase.*;
import java.util.*;

/**
 * Represents the juxtaposition ('diff view') of two different versions of a whole text document.  
 * Consists of a list of {@link DiffLine}s, each of which shows the changes that were done to one line in the document.  
 */
public class DiffDoc {
   private ArrayList<DiffLine> lines = new ArrayList<DiffLine>();

   final int getLineCount() {
      return lines.size();
   }
   final DiffLine getLine(int index) {
      return lines.get(index);
   }

   DiffDoc() {
   }

   final void add(CompareLine leftLine, CompareLine rightLine, LineStatus status) {
      int index = lines.size();
      DisplayLine leftDisplayLine = (leftLine == null) ? null : leftLine.displayLine;
      DisplayLine rightDisplayLine = (rightLine == null) ? null : rightLine.displayLine;
      ChangeTypes changeType;
      if (status == LineStatus.CHANGED) {
         if (leftLine == null)
            throw new NullPointerException("leftLine");
         if (rightLine == null)
            throw new NullPointerException("rightLine");
         ArrayList<HighlightBit> leftHighlightBits = leftLine.getHighlightBits();
         ArrayList<HighlightBit> rightHighlightBits = rightLine.getHighlightBits();
         leftDisplayLine.setHighlightBits(leftHighlightBits);
         rightDisplayLine.setHighlightBits(rightHighlightBits);
         changeType = ChangeTypes.createFrom(leftHighlightBits, rightHighlightBits);
      } else {
      	changeType = ChangeTypes.createNoChanges();
      }

      lines.add(new DiffLine(index, leftDisplayLine, rightDisplayLine, status, changeType));
   }

   /** 
    creates a two-column, (unformatted) text-only display of the {@link DiffDoc}.
    
    @return 
   */
   public final String toText() {
      StringBuilder result = new StringBuilder();
      for (DiffLine line : lines) {
         if (line.status == LineStatus.CHANGED) {
            result.append(line.leftLine.getTextWithHighlightedChanges()).append("\t");
            result.append(line.rightLine.getTextWithHighlightedChanges()).append("\t");
         } else {
            result.append(line.leftLine == null ? "" : line.leftLine.getText()).append("\t");
            result.append(line.rightLine == null ? "" : line.rightLine.getText()).append("\t");
         }
         result.append(line.status.toString());
         if (line.status == LineStatus.CHANGED)
            result.append(" (inner space change: " + String.valueOf(line.changeTypes.innerSpaceChanges) + ", case change: " + String.valueOf(line.changeTypes.caseChanges) + ")");
         result.append(System.lineSeparator());
      }
      return result.toString();
   }

   final Command getCommandAt(int lineIndex) {
      if (lineIndex < 0 || lineIndex >= getLineCount())
         return null;
      DiffLine line = lines.get(lineIndex);
      if (line.rightLine != null)
      	return line.rightLine.parentCommand;
      else if (line.leftLine != null) 
      	return line.leftLine.parentCommand;
      else
      	return null;
   }


   private String getTextAt(int lineIndex) {
      return getTextAt(lineIndex, DisplaySide.RIGHT);
   }
   private String getTextAt(int lineIndex, DisplaySide displaySide) {
      if (lineIndex < 0 || lineIndex >= getLineCount())
         return null;
      DisplayLine displayLine = lines.get(lineIndex).getDisplayLine(displaySide);
      return (displayLine == null) ? null : displayLine.getText();
   }


   final int getLineOfNextChangedCommand(int startLine, boolean searchDown, ChangeTypes filter) {
      int lineIndex = startLine;
      Command curCommand = getCommandAt(lineIndex);
      do {
         // find next/previous deleted/added/changed line
         lineIndex += searchDown ? 1 : -1;
         if (lineIndex < 0 || lineIndex >= getLineCount())
            return -1;
         if (!lines.get(lineIndex).matchesChangeTypesFilter(filter))
            continue;

         // continue searching if this line belongs to the same command
         Command nextCommand = getCommandAt(lineIndex);
         if (curCommand != null && nextCommand == curCommand)
            continue;

         // return the first changed line of the new command
         int testIndex = lineIndex;
         if (!searchDown && nextCommand != null) {
            while (testIndex > 0 && getCommandAt(testIndex - 1) == nextCommand) {
               --testIndex;
               if (lines.get(testIndex).matchesChangeTypesFilter(filter))
                  lineIndex = testIndex;
            }
         }
         return lineIndex;
      } while (true);
   }

   private int getFirstLineOfCommandAt(int lineIndex, boolean ignoreEmptyLines, DisplaySide displaySide) {
      Command command = getCommandAt(lineIndex);
      if (command == null)
         return lineIndex;
      int testIndex = lineIndex;
      while (testIndex > 0 && getCommandAt(testIndex - 1) == command) {
         --testIndex;
         if (!StringUtil.isNullOrEmpty(getTextAt(testIndex)))
            lineIndex = testIndex;
      }
      return lineIndex;
   }

   private int getLastLineOfCommandAt(int lineIndex) {
      Command command = getCommandAt(lineIndex);
      if (command == null)
         return lineIndex;
      while (lineIndex + 1 < getLineCount() && getCommandAt(lineIndex + 1) == command)
         ++lineIndex;
      return lineIndex;
   }

   public final int getFirstLineInCleanupRange() {
   	for (int lineIndex = 0; lineIndex < getLineCount(); ++lineIndex) {
   		Command command = getCommandAt(lineIndex);
   		if (command != null && command.isInCleanupRange())
   			return lineIndex;
   	}
   	return 0;
   }

   final String getTextOfLineRange(int startLine, int lastLine, DisplaySide displaySide, String lineSeparator) {
      if (startLine < 0)
         return "";

      startLine = getFirstLineOfCommandAt(startLine, true, displaySide);
      lastLine = getLastLineOfCommandAt(lastLine);

      // build the result text
      StringBuilder result = new StringBuilder();
      for (int i = startLine; i <= lastLine; ++i) {
         DisplayLine line = lines.get(i).getDisplayLine(displaySide);
         if (line != null) // otherwise, skip line!
            result.append(line.getText()).append(lineSeparator);
      }
      return result.toString();
   }


   public final RuleStats[] getRuleStats(Profile profile) {
      return getRuleStatsOfLineRange(profile, 0, lines.size());
   }
   final RuleStats[] getRuleStatsOfLineRange(Profile profile, int startLine, int lastLine) {
      int[] ruleUseCount = new int[Rule.RULE_COUNT];
      int[] ruleBlockedCount = new int[Rule.RULE_COUNT];

		// if the last line is an empty line in both displays, it is not intuitive that this belongs to the next command. 
      // This logic is also applied in DiffNavigator.reprocessSelection(...)
      if (lastLine < lines.size() - 1) 
      	lastLine = getLastNonEmptyOrChangedLineInRange(startLine, lastLine);
      
      Command command = getCommandAt(startLine);
      Command lastCommand = getCommandAt(lastLine);
      while (command != null) {
         command.getChangeControl().addToRuleStats(ruleUseCount, ruleBlockedCount);
         if (command == lastCommand)
            break;
         command = command.getNext();
      }

      ArrayList<RuleStats> result = new ArrayList<RuleStats>();
      for (int i = 0; i < Rule.RULE_COUNT; ++i) {
         if (ruleUseCount[i] > 0 || ruleBlockedCount[i] > 0)
            result.add(RuleStats.create(profile.getRule(RuleID.forValue(i)), ruleUseCount[i], ruleBlockedCount[i]));
      }
      return result.toArray(new RuleStats[0]);
   }

   final int getLastNonEmptyOrChangedLineInRange(int startLine, int lastLine) {
   	lastLine = Math.min(lastLine, lines.size() - 1);
		while (lastLine > startLine && isLineEmptyWithNoChanges(lastLine))
			--lastLine;
		return lastLine;
   }
   
   final int getFirstNonEmptyOrChangedLineInRange(int startLine, int lastLine) {
   	startLine = Math.max(startLine,  0);
   	while (startLine < lastLine && isLineEmptyWithNoChanges(startLine))
   		++startLine;
   	return startLine;
   }
   
   final int getLastNonEmptyLineInRange(int startLine, int lastLine) {
   	lastLine = Math.min(lastLine, lines.size() - 1);
		while (lastLine > startLine && isLineEmpty(lastLine))
			--lastLine;
		return lastLine;
   }
   
   final int getFirstNonEmptyLineInRange(int startLine, int lastLine) {
   	startLine = Math.max(startLine,  0);
   	while (startLine < lastLine && isLineEmpty(startLine))
   		++startLine;
   	return startLine;
   }
   
   final void replacePart(int startLine, int lastLine, DiffDoc diffDocPart) {
      lines.subList(startLine, lastLine + 1).clear();
      lines.addAll(startLine, diffDocPart.lines);

      int indexInDocLeft = 0;
      int indexInDocRight = 0;
      for (int i = 0; i < getLineCount(); ++i) {
         DiffLine line = lines.get(i);
         line.index = i;
         if (line.leftLine != null)
            line.leftLine.indexInDoc = indexInDocLeft++;
         if (line.rightLine != null)
            line.rightLine.indexInDoc = indexInDocRight++;
      }
   }

   final int findFirstLineOfCommand(Command command, DisplaySide displaySide, boolean skipEmptyLines) {
      for (int i = 0; i < getLineCount(); ++i) {
      	DiffLine line = lines.get(i);
         DisplayLine displayLine = (displaySide == DisplaySide.LEFT) ? line.leftLine : line.rightLine; //  (line.leftLine) != null ? ...
         if (displayLine != null && displayLine.parentCommand == command && (!skipEmptyLines || !displayLine.isEmpty()))
            return i;
      }
      return -1;
   }


   final int findLastLineOfCommand(Command command, DisplaySide displaySide) {
      for (int i = getLineCount() - 1; i >= 0; --i) {
      	DiffLine line = lines.get(i);
         DisplayLine displayLine = (displaySide == DisplaySide.LEFT) ? line.leftLine : line.rightLine; // (line.leftLine) != null
         if (displayLine != null && displayLine.parentCommand == command)
            return i;
      }
      return -1;
   }


   final int findLineIndexOfLineNum(int docLineNum, DisplaySide displaySide) {
      for (int i = getLineCount() - 1; i >= 0; --i) {
         DisplayLine line = lines.get(i).getDisplayLine(displaySide);
         if (line != null && line.indexInDoc == docLineNum)
            return i;
      }
      return -1;
   }

   final ChangeStats getChangeStats() {
      int indentCount = 0;
      int innerSpaceCount = 0;
      int caseCount = 0;
      int contentCount = 0;

      for (DiffLine line : lines) {
         if (line.changeTypes.indentChanges)
            ++indentCount;
         if (line.changeTypes.innerSpaceChanges)
            ++innerSpaceCount;
         if (line.changeTypes.caseChanges)
            ++caseCount;
         if (line.status == LineStatus.LEFT_DELETED || line.status == LineStatus.RIGHT_ADDED || line.status == LineStatus.LEFT_DELETED_RIGHT_ADDED || (line.status == LineStatus.CHANGED && line.changeTypes.textOrLineChanges))
            ++contentCount;
      }
      return ChangeStats.create(indentCount, innerSpaceCount, caseCount, contentCount);
   }
   
   final boolean isLineEmptyWithNoChanges(int lineIndex) {
      return lineIndex >= 0 && lineIndex < getLineCount() && lines.get(lineIndex).isEmptyWithNoChanges();
   }
   
   final boolean isLineEmpty(int lineIndex) {
      return lineIndex >= 0 && lineIndex < getLineCount() && lines.get(lineIndex).isEmpty();
   }
   
   final int findFirstLineOfSourceLine(int sourceLineNum) {
      for (int i = 0; i < getLineCount(); ++i) {
      	if (lines.get(i).containsSourceLineNum(sourceLineNum))
      		return i;
      }
      return -1;
   }
   
   final int findLastLineOfSourceLine(int sourceLineNum) {
      for (int i = getLineCount() - 1; i >= 0; --i) {
      	if (lines.get(i).containsSourceLineNum(sourceLineNum))
      		return i;
      }
      return -1;
   }
   
   public final int getChangedLineCount() {
   	int result = 0;
      for (int i = 0; i < getLineCount(); ++i) {
      	if (lines.get(i).status != LineStatus.EQUAL)
      		++result;
      }
      return result;
  }
}