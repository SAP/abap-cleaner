package com.sap.adt.abapcleaner.base;

public class MarkdownBuilder {
	// cp. list of languages known to GitHub: https://github.com/github-linguist/linguist/blob/master/lib/linguist/languages.yml
	public static final String ABAP_LANGUAGE_NAME = "ABAP";
	public static final String DDL_LANGUAGE_NAME = "ASDDLS";
	public static final String SQL_LANGUAGE_NAME = "SQL";

	public static final String MARKDOWN_EXTENSION = ".md";

	private static final char HEADER_CHAR = '#';
	private static final char ESCAPE_CHAR = '\\';
	private static final char BLOCK_QUOTE_CHAR = '>';
	
	private static final char IMAGE_START = '!';
	private static final String IMAGE_HOVER_TEXT_START = " \"";
	private static final String IMAGE_HOVER_TEXT_END = "\"";

	private static final char LINK_TEXT_START = '[';
	private static final char LINK_TEXT_END = ']';
	private static final char LINK_URL_START = '(';
	private static final char LINK_URL_END = ')';

	private static final char INLINE_CODE_START = '`';
	private static final char INLINE_CODE_END = '`';

	private static final String UNORDERED_LIST_START = "* ";
	private static final String ORDERED_LIST_START = "1. ";

	private static final String TOGGLE_ITALIC = "*";
	private static final String TOGGLE_BOLD = "**";
	private static final String TOGGLE_CODE_BLOCK = "```";

	private static final String LINE_SEPARATOR = "\r\n";
	private static final String CHARS_TO_ESCAPE = "\\`*_{}[]()#+!"; // should - and . be added?
	
	private static final int MIN_LEVEL = 1;
	private static final int MAX_LEVEL = 8;
	private static final String ILLEGAL_LEVEL_MSG = "Level must be between 1 and 8";

	private StringBuilder sb = new StringBuilder();
	private String endParagraphText = null;
	private boolean isInList = false;

	public static MarkdownBuilder create() {
		return new MarkdownBuilder();
	}
	
	private MarkdownBuilder() {
	}
	
	private static String escapeText(String text) {
		StringBuilder result = new StringBuilder();
		char[] textChars = text.toCharArray();
		for (char c : textChars) {
			if (CHARS_TO_ESCAPE.indexOf(c) >= 0)
				result.append(ESCAPE_CHAR);
			result.append(c);
		}
		return result.toString();
	}

	public MarkdownBuilder startNewHeading(String text, int level) {
		if (level < MIN_LEVEL || level > MAX_LEVEL)
			throw new IllegalArgumentException(ILLEGAL_LEVEL_MSG);
		
		finishPreviousParagraph(false);
		sb.append(StringUtil.repeatChar(HEADER_CHAR, level)).append(' ');
		sb.append(escapeText(text));
		endParagraphText = LINE_SEPARATOR + LINE_SEPARATOR;
		return this;
	}

	public MarkdownBuilder startNewParagraph() {
		finishPreviousParagraph(false);
		isInList = false;
		endParagraphText = LINE_SEPARATOR + LINE_SEPARATOR;
		return this;
	}

	public MarkdownBuilder startNewBlockQuote(String text, int level) {
		if (level < MIN_LEVEL || level > MAX_LEVEL)
			throw new IllegalArgumentException(ILLEGAL_LEVEL_MSG);
		
		finishPreviousParagraph(false);
		sb.append(StringUtil.repeatChar(BLOCK_QUOTE_CHAR, level)).append(' ');
		sb.append(escapeText(text));
		endParagraphText = LINE_SEPARATOR + LINE_SEPARATOR;
		return this;
	}

	/**
	 * starts a new code block and fills it with the supplied code
	 * @param code - the code to show in the code block
	 * @param language - programming language used for syntax highlighting
	 */
	public MarkdownBuilder startNewCodeBlock(String code, Language language) {
   	String languageName;
   	if (language == Language.ABAP) {
   		languageName = ABAP_LANGUAGE_NAME;
   		
   	} else if (language == Language.DDL || language == Language.DCL) {
   		languageName = DDL_LANGUAGE_NAME;
   	
   	} else if (language == Language.SQL) {
   		languageName = SQL_LANGUAGE_NAME;
   	
   	} else {
   		languageName = ""; // auto-detect
   	}
   	
		finishPreviousParagraph(false);
		sb.append(TOGGLE_CODE_BLOCK).append(languageName).append(LINE_SEPARATOR);
		sb.append(code); // escapeText(code) is not necessary here (but wouldn't harm, either) 
		isInList = false;
		endParagraphText = LINE_SEPARATOR + TOGGLE_CODE_BLOCK + LINE_SEPARATOR + LINE_SEPARATOR;
		return this;
	}

	public MarkdownBuilder startNewBullet(int level) {
		if (level < MIN_LEVEL || level > MAX_LEVEL)
			throw new IllegalArgumentException(ILLEGAL_LEVEL_MSG);
		finishPreviousParagraph(true);
		for (int i = MIN_LEVEL; i < level; ++i)
			sb.append("  ");
		sb.append(UNORDERED_LIST_START);
		isInList = true;
		endParagraphText = LINE_SEPARATOR;
		return this;
	}

	public MarkdownBuilder startNewOrderedListItem(int level) {
		if (level < MIN_LEVEL || level > MAX_LEVEL)
			throw new IllegalArgumentException(ILLEGAL_LEVEL_MSG);
		finishPreviousParagraph(true);
		for (int i = MIN_LEVEL; i < level; ++i)
			sb.append("  ");
		sb.append(ORDERED_LIST_START);
		isInList = true;
		endParagraphText = LINE_SEPARATOR;
		return this;
	}

	public MarkdownBuilder finishBuild() {
		finishPreviousParagraph(false);
		return this;
	}

	private void finishPreviousParagraph(boolean startingListItem) {
		if (!StringUtil.isNullOrEmpty(endParagraphText)) {
			sb.append(endParagraphText);
		}
		if (isInList && !startingListItem)
			sb.append(LINE_SEPARATOR);
		endParagraphText = null;
	}

	public MarkdownBuilder appendText(String text) {
		sb.append(escapeText(text));
		return this;
	}
	
	public MarkdownBuilder appendBoldText(String text) {
		sb.append(TOGGLE_BOLD).append(escapeText(text)).append(TOGGLE_BOLD);
		return this;
	}
	
	public MarkdownBuilder appendItalicText(String text) {
		sb.append(TOGGLE_ITALIC).append(escapeText(text)).append(TOGGLE_ITALIC);
		return this;
	}
	
	public MarkdownBuilder appendLink(String url) {
		sb.append(url);
		return this;
	}
	
	public MarkdownBuilder appendLink(String text, String url) {
		sb.append(LINK_TEXT_START).append(escapeText(text)).append(LINK_TEXT_END);
		sb.append(LINK_URL_START).append(url).append(LINK_URL_END);
		return this;
	}
	
	public MarkdownBuilder appendBoldLink(String text, String url) {
		sb.append(LINK_TEXT_START).append(TOGGLE_BOLD).append(escapeText(text)).append(TOGGLE_BOLD).append(LINK_TEXT_END);
		sb.append(LINK_URL_START).append(url).append(LINK_URL_END);
		return this;
	}
	
	public MarkdownBuilder appendImage(String altText, String url, String hoverText) {
		sb.append(IMAGE_START).append(LINK_TEXT_START).append(escapeText(altText)).append(LINK_TEXT_END);
		sb.append(LINK_URL_START).append(url);
		if (!StringUtil.isNullOrEmpty(hoverText)) {
			sb.append(IMAGE_HOVER_TEXT_START).append(escapeText(hoverText)).append(IMAGE_HOVER_TEXT_END);
		}
		sb.append(LINK_URL_END);
		return this;
	}
	
	public MarkdownBuilder appendInlineCode(String code) {
		sb.append(INLINE_CODE_START).append(code).append(INLINE_CODE_END);
		return this;
	}
	
	@Override
	public String toString() {
		return sb.toString();
	}
}
