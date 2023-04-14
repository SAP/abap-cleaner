package com.sap.adt.abapcleaner.parser;

import java.util.*;

/**
 * <p>Holds statistical information on an ABAP {@link #keyword}. 
 * This information is compiled when analyzing ABAP {@link Code} with {@link CodeMetrics}.</p>
 * 
 * <p>A KeywordCount instance stores for its ABAP {@link #keyword}</p>
 * <ul>
 * <li>the occurrence {@link #count},</li> 
 * <li>the {@link #firstInstance} where the keyword was found,</li>
 * <li>and possibly the keyword's {@link #context}, meaning the frequency of other ABAP keywords 
 * that appear in the same {@link Command} together with this {@link #keyword}.</li>
 * </ul>
 * 
 * <p>Note that the {@link #context} is itself a {@link KeywordMetrics} instance that recursively 
 * uses {@link KeywordCount}s; these lower-level {@link KeywordCount} instances, however, 
 * have {@link #context} == null.</p>
 */
class KeywordCount {
	/** the ABAP keyword for which this {@link KeywordCount} instance holds statistical information */
	final String keyword;
	private int count;

	final int getCount() { return count; }

	/** name and line of the source {@link Code} in which this {@link #keyword} was first encountered */
	final String firstInstance;

	/** frequency of other ABAP keywords that appear in the same {@link Command} together with this {@link #keyword} */
	private KeywordMetrics context;

	public static class ComparerByCount implements Comparator<KeywordCount> {
		public final int compare(KeywordCount a, KeywordCount b) {
			if (a.count < b.count)
				return 1;
			else if (a.count > b.count)
				return -1;
			else
				return 0;
		}
	}

	KeywordCount(String keyword, String firstInstance) {
		this(keyword, firstInstance, null, 0);
	}
	KeywordCount(String keyword, String firstInstance, String[] contextKeywords, int contextStartIndex) {
		this.keyword = keyword;
		count = 1;
		this.firstInstance = firstInstance;

		if (contextKeywords != null) {
			context = new KeywordMetrics();
			context.addContextKeywords(contextKeywords, contextStartIndex);
		}
	}

	final void addInstance() {
		addInstance(null, 0);
	}
	final void addInstance(String[] contextKeywords, int contextStartIndex) {
		++count;

		if (contextKeywords != null) {
			if (context == null)
				context = new KeywordMetrics();
			context.addContextKeywords(contextKeywords, contextStartIndex);
		}
	}

	final String toLine() {
		return toLine("\t");
	}
	final String toLine(String separator) {
		return keyword + separator + String.valueOf(count) + separator + firstInstance + separator + (context == null ? "" : context.toContextInfo());
	}

	final String toContextInfo() {
		return String.valueOf(count) + " " + keyword;
	}
}