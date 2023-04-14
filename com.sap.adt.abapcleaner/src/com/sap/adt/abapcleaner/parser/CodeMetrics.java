package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.programbase.Job;

/**
 * <p>Compiles metrics such as line count, {@link Command} count and {@link Token} count 
 * for the {@link Code} that is processed in a {@link Job}.</p>
 * 
 * <p>Specifically, {@link KeywordMetrics} are compiled 
 * both for the first ABAP keyword within each {@link Command} ({@link CodeMetrics#firstKeywordsMetrics})
 * and for all other ABAP keywords ({@link #otherKeywordsMetrics}).</p>  
 */
public class CodeMetrics {
	private int fileCount;
	private int lineSum;
	private int commandSum;
	private int tokenSum;
	private int byteSum;

	private KeywordMetrics firstKeywordsMetrics = new KeywordMetrics();
	private KeywordMetrics otherKeywordsMetrics = new KeywordMetrics();

	public final int getFileCount() { return fileCount; }

	public final int getLineSum() { return lineSum; }

	public final int getCommandSum() { return commandSum; }

	public final int getTokenSum() { return tokenSum; }

	public final int getByteSum() { return byteSum; }

	public final String getFirstKeywordsMetrics() { return firstKeywordsMetrics.toList(); }

	public final String getOtherKeywordsMetrics() { return otherKeywordsMetrics.toList(); }

	public final void addFile(int lineCount, int commandCount, int tokenCount, int byteCount) {
		++fileCount;
		lineSum += lineCount;
		commandSum += commandCount;
		tokenSum += tokenCount;
		byteSum += byteCount;
	}

	public final void addCode(String sourceCode, String sourceName, Code code) {
		int lineCount = StringUtil.instrCount(sourceCode, '\n');
		addFile(lineCount, code.commandCount, code.getTotalTokenCount(), sourceCode.length());

		Command command = code.firstCommand;
		while (command != null) {
			if (!command.isCommentLine()) {
				int sourceLineNum = command.getFirstToken().sourceLineNum;
				String[] collocations = command.getAllKeywordsWithCollocations();

				Token firstCode = command.getFirstCodeToken();
				boolean commandStartsWithKeyword = (firstCode != null) && firstCode.isKeyword();
				String firstCollocation;
				if (commandStartsWithKeyword)
					firstCollocation = collocations[0];
				else if (command.isAssignment())
					firstCollocation = "(assignment)";
				else if (command.isFunctionalCallOrCallChain())
					firstCollocation = "(functional call)";
				else
					firstCollocation = "(other)";
				firstKeywordsMetrics.addKeyword(firstCollocation, sourceName, sourceLineNum, collocations, (commandStartsWithKeyword ? 1 : 0));

				int startIndex = (commandStartsWithKeyword ? 1 : 0);
				for (int i = startIndex; i < collocations.length; ++i)
					otherKeywordsMetrics.addKeyword(collocations[i], sourceName, sourceLineNum, new String[] { firstCollocation });
			}
			command = command.getNext();
		}
	}

	public final String getSummary(double processingDuration_ms, int parseExceptionCount) {
		StringBuilder summary = new StringBuilder();
		summary.append("Processed " + Cult.format(fileCount) + " files");
		summary.append(" with " + Cult.format(tokenSum) + " tokens");
		summary.append(" in " + Cult.format(commandSum) + " commands");
		summary.append(System.lineSeparator());
		summary.append("(" + Cult.format(lineSum) + " lines");
		summary.append(", " + Cult.format(byteSum) + " chars)");
		summary.append(" in " + Cult.format((int) processingDuration_ms) + " ms");
		summary.append(System.lineSeparator());
		
		if (parseExceptionCount > 0)
			summary.append(Cult.format(parseExceptionCount) + " parse errors").append(System.lineSeparator());

		return summary.toString();
	}
	
	public String getKeywordMetricsDetails() {
		String lineSep = System.lineSeparator();
		
		StringBuilder result = new StringBuilder();
		result.append("Frequency of keywords at command start").append(lineSep);
		result.append(lineSep);
		result.append("Start keyword\tInstance count\tFirst instance\tFrequency of further keywords in command").append(lineSep);
		result.append(getFirstKeywordsMetrics());
		result.append(lineSep);
		result.append(lineSep);
		result.append(lineSep);
		result.append("Frequency of additional keywords within a command").append(lineSep);
		result.append(lineSep);
		result.append("Additional keyword\tInstance count\tFirst instance\tFrequency by start keyword").append(lineSep);
		result.append(getOtherKeywordsMetrics());
		result.append(lineSep);

		return result.toString();
	}
}