package com.sap.adt.abapcleaner.rulebase;

import com.sap.adt.abapcleaner.programbase.*;
import com.sap.adt.abapcleaner.base.StringUtil;

public class RuleReference {
	public static String baseLink(RuleSource source) {
		switch (source) {
			case CODE_PAL_FOR_ABAP:
				return "https://github.com/SAP/code-pal-for-abap/blob/master/docs/checks/";
			case ABAP_STYLE_GUIDE:
				return "https://github.com/SAP/styleguides/blob/main/clean-abap/CleanABAP.md";
			case ABAP_KEYWORD_DOCU:
				return "https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=";
			case ABAP_CLEANER:
				return Persistency.get().getExistingDirs(FileType.HELP)[0];
			default:
				return "";
		}
	}

	public static String sourceName(RuleSource source) {
		switch (source) {
			case CODE_PAL_FOR_ABAP:
				return "Code Pal for ABAP";
			case ABAP_STYLE_GUIDE:
				return "Clean ABAP Styleguide";
			case ABAP_KEYWORD_DOCU:
				return "ABAP Keyword Documentation";
			case ABAP_CLEANER:
				return Program.PRODUCT_NAME;
			default:
				throw new IndexOutOfBoundsException("Unknown RuleSource!");
		}
	}

	// -------------------------------------------------------------------------
	
	public final RuleSource source;
	public final String chapterTitle;
	private String subLink;
	public final boolean isContradicting;

	public final String getLink() { return hasLink() ? (subLink.startsWith("https://") ? subLink : baseLink(source) + subLink) : ""; }

	public final boolean hasLink() { return !StringUtil.isNullOrEmpty(subLink); }


	public RuleReference(RuleSource source) {
		this(source, "", "", false);
	}
	public RuleReference(RuleSource source, String chapterTitle) {
		this(source, chapterTitle, "", false);
	}
	public RuleReference(RuleSource source, String chapterTitle, String subLink) {
		this(source, chapterTitle, subLink, false);
	}
	public RuleReference(RuleSource source, String chapterTitle, String subLink, boolean isContradicting) {
		this.source = source;
		this.chapterTitle = chapterTitle;
		this.subLink = subLink;
		this.isContradicting = isContradicting;
	}

	public final String getSourceText() {
		String prefix = isContradicting ? "<-> " : "";
		return prefix + sourceName(source);
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();
		result.append(getSourceText());
		if (!StringUtil.isNullOrEmpty(chapterTitle))
			result.append(": " + chapterTitle);
		if (hasLink())
			result.append(" - " + getLink());
		return result.toString();
	}
}