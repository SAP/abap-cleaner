package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;

import com.sap.adt.abapcleaner.base.ABAP;
import com.sap.adt.abapcleaner.base.AbapCult;
import com.sap.adt.abapcleaner.base.StringUtil;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Section;
import com.sap.adt.abapcleaner.parser.Term;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.IntegrityBrokenException;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxAfterChanges;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class AbapDoc {
	public class AbapDocSection {
		public final Token firstToken;
		public final Token lastToken;
		public final boolean hasDescription;

		public AbapDocSection(Token firstToken, Token lastToken, boolean hasDescription) {
			this.firstToken = firstToken;
			this.lastToken = lastToken;
			this.hasDescription = hasDescription;
		}
	}


	private boolean inChain;
	private Token tokenAfterHeader;
	
	private int spacesLeft;
	/** the default start tag for new descriptions, e.g. &lt;p class="shorttext synchronized" lang="en"&gt; */
	private String defaultStartTag = "";
	/** the default start tag for new descriptions, e.g. &lt;/p&gt; */
	private String defaultEndTag = "";
	
	/** main description section */
	private AbapDocSection header;

	/** parameter descriptions, starting with @parameter */
	private HashMap<String, AbapDocSection> parameters = new HashMap<>();
	/** exception descriptions (class-based or non-class-based), starting with @raising or @exception */
	private HashMap<String, AbapDocSection> exceptions = new HashMap<>();

	public static AbapDoc readFromChain(Token methodNameToken) {
		// identify comment line Tokens preceding the method name Token
		AbapDoc abapDoc = new AbapDoc(true);
		abapDoc.tokenAfterHeader = methodNameToken;

		Token lastToken = methodNameToken.getPrev();
		Token token = lastToken;
		
		Token firstToken = null;
		Token lastTokenInSection = null;
		while (token != null && token.isAbapDocComment()) {
			firstToken = token;
			if (lastTokenInSection == null)
				lastTokenInSection = token;
			if (abapDoc.tryAddSection(firstToken, lastTokenInSection)) {
				lastTokenInSection = null;
			}
			token = token.getPrev();
		}
		if (lastTokenInSection != null) {
			abapDoc.setHeader(firstToken, lastTokenInSection);
		}
		return abapDoc;
	}

	public static AbapDoc readFromNonChain(Command methodDeclaration) {
		// identify Commands before the METHODS Command
		AbapDoc abapDoc = new AbapDoc(false);
		abapDoc.tokenAfterHeader = methodDeclaration.getFirstToken();

		Command lastCommand = methodDeclaration.getPrev();
		Command command = lastCommand;
		
		Command firstCommand = null;
		Command lastCommandInSection = null;
		while (command != null && command.isAbapDoc()) {
			firstCommand = command;
			if (lastCommandInSection == null)
				lastCommandInSection = command;
			if (abapDoc.tryAddSection(firstCommand.getFirstToken(), lastCommandInSection.getFirstToken())) {
				lastCommandInSection = null;
			}
			command = command.getPrev();
		}
		if (lastCommandInSection != null) {
			abapDoc.setHeader(firstCommand.getFirstToken(), lastCommandInSection.getFirstToken());
		}
		return abapDoc;
	}

	// -------------------------------------------------------------------------
	
	private AbapDoc(boolean inChain) {
		this.inChain = inChain;
	}
	
	public String getKey(String name) {
		return name.toUpperCase();
	}

	private void setHeader(Token firstToken, Token lastToken) {
		header = new AbapDocSection(firstToken, lastToken, hasDescription(firstToken, lastToken));
		updateDefaultsForNewDocLines(firstToken, lastToken);
	}
	
	private boolean tryAddSection(Token firstToken, Token lastToken) {
		// determine whether firstToken starts the documentation of a parameter or exception
		ArrayList<String> words = getWords(firstToken, lastToken, 2);
		if (words.size() < 2) {
			return false;
		} else if (!AbapCult.stringEqualsAny(true, words.get(0), ABAP.ABAP_DOC_PARAMETER_ANNOTATION, ABAP.ABAP_DOC_RAISING_ANNOTATION, ABAP.ABAP_DOC_EXCEPTION_ANNOTATION)) {
			return false;
		}
		
		// add the parameter / exception documentation to the respective HashMap
		tokenAfterHeader = firstToken;
		String key = getKey(words.get(1));
		AbapDocSection section = new AbapDocSection(firstToken, lastToken, hasDescription(firstToken, lastToken));
		if (AbapCult.stringEquals(words.get(0), ABAP.ABAP_DOC_PARAMETER_ANNOTATION, true)) {
			parameters.put(key, section);
		} else {
			exceptions.put(key, section);
		}
		
		updateDefaultsForNewDocLines(firstToken, lastToken);

		return true;
	}

	private void updateDefaultsForNewDocLines(Token firstToken, Token lastToken) {
		// update the default start and end tag, if any is found (thus taking the first <p ...> tag in the ABAP Doc)
		String text = getText(firstToken, lastToken);
		int parTagStart = text.indexOf("<p");
		if (parTagStart >= 0) {
			int parTagEnd = text.indexOf(">", parTagStart);
			if (parTagEnd >= 0) {
				defaultStartTag = text.substring(parTagStart, parTagEnd + 1);
				defaultEndTag = "</p>";
			}
		}
		
		// update the default indent
		spacesLeft = firstToken.getStartIndexInLine();
	}
	
	public boolean isSynchronized() {
		return defaultStartTag != null && StringUtil.containsIgnoringCase(defaultStartTag, "synchronized");
	}
	
	private ArrayList<String> getWords(Token firstToken, Token lastToken, int stopCount) {
		ArrayList<String> words = new ArrayList<>();
		Token token = firstToken;
		while (token != null && (stopCount <= 0 || words.size() < stopCount)) {
			String text = token.getText();
			if (!text.startsWith(ABAP.ABAP_DOC_SIGN))
				return words;
			text = text.substring(ABAP.ABAP_DOC_SIGN.length());
			String[] wordsInText = StringUtil.split(text, ' ', true);
			if (wordsInText != null) {
				for (String word : wordsInText)
					words.add(word);
			}
			if (token == lastToken)
				break;
			token = getNextToken(token);
		}
		return words;
	}

	private boolean hasDescription(Token firstToken, Token lastToken) {
		String text = getText(firstToken, lastToken).trim();
		int sepPos = text.indexOf(ABAP.ABAP_DOC_SEP_STRING);
		if (sepPos < 0 || sepPos + 1 == text.length())
			return false;
		text = StringUtil.removeTags(text.substring(sepPos + 1).trim()).trim();
		return !StringUtil.isNullOrEmpty(text);
	}

	private String getText(Token firstToken, Token lastToken) {
		StringBuilder sb = new StringBuilder();
		Token token = firstToken;
		while (token != null) {
			String text = token.getText();
			if (!text.startsWith(ABAP.ABAP_DOC_SIGN))
				break;;
			if (sb.length() > 0)
				sb.append(" ");
			sb.append(text.substring(ABAP.ABAP_DOC_SIGN.length()));
			if (token == lastToken)
				break;
			token = getNextToken(token);
		}
		return sb.toString();
	}

	public Token getNextToken(Token token) {
		if (inChain) {
			return token.getNext();
		} else {
			return token.getParentCommand().getNext().getFirstToken();
		}
	}

	public boolean isEmpty() {
		return (header == null) && parameters.isEmpty() && exceptions.isEmpty();
	}

	public Token getTokenAfterHeader() { 
		return tokenAfterHeader;
	}

	public int getParameterCount() {
		return parameters.size();
	}
	public boolean hasParameter(String name) {
		return parameters.containsKey(getKey(name));		
	}
	public AbapDocSection getParameter(String name) {
		return parameters.get(getKey(name));		
	}
	public Iterable<String> getParameterNames() {
		return parameters.keySet();		
	}

	public int getExceptionCount() {
		return exceptions.size();
	}
	public boolean hasException(String name) {
		return exceptions.containsKey(getKey(name));		
	}
	public AbapDocSection getException(String name) {
		return exceptions.get(getKey(name));		
	}
	public Iterable<String> getExceptionNames() {
		return exceptions.keySet();		
	}
	
	public boolean hasAnyParameterOrException() {
		return (getParameterCount() > 0 || getExceptionCount() > 0);
	}
	
	public void moveSectionTo(Token writePos, AbapDocSection abapDocSection) throws UnexpectedSyntaxException, IntegrityBrokenException {
		adjustLineBreaks(writePos, abapDocSection.firstToken);
		if (inChain) {
			Term term = Term.createForTokenRange(abapDocSection.firstToken, abapDocSection.lastToken);
			term.removeFromCommand(false);
			writePos.insertLeftSibling(term);
		} else {
			Section commandSection = Section.create(abapDocSection.firstToken.getParentCommand(), abapDocSection.lastToken.getParentCommand());
			commandSection.removeFromCode();
			writePos.getParentCommand().insertLeftSibling(commandSection);
		}
	}
	
	public void insertParameterDocBefore(Token writePos, String name, boolean forceNonSynchronized) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		String text = getNewDocLine(ABAP.ABAP_DOC_PARAMETER_ANNOTATION, name, forceNonSynchronized); 
		ensureEmptyLineAfterHeader(writePos);
		parameters.put(getKey(name), insertLineBefore(writePos, text));
	}
	public void insertRaisingDocBefore(Token writePos, String name, boolean forceNonSynchronized) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		String text = getNewDocLine(ABAP.ABAP_DOC_RAISING_ANNOTATION, name, forceNonSynchronized); 
		ensureEmptyLineAfterHeader(writePos);
		exceptions.put(getKey(name), insertLineBefore(writePos, text));
	}
	public void insertExceptionDocBefore(Token writePos, String name, boolean forceNonSynchronized) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		String text = getNewDocLine(ABAP.ABAP_DOC_EXCEPTION_ANNOTATION, name, forceNonSynchronized); 
		ensureEmptyLineAfterHeader(writePos);
		exceptions.put(getKey(name), insertLineBefore(writePos, text));
	}

	private String getNewDocLine(String annotation, String name, boolean forceNonSynchronized) {
		// ABAP Doc shows the parameter / exception name in lower case, even if it is in upper case in the declaration
		String line = ABAP.ABAP_DOC_SIGN + " " + annotation + " " + name.toLowerCase() + " " + ABAP.ABAP_DOC_SEP_STRING;
		if (!StringUtil.isNullOrEmpty(defaultStartTag) && !forceNonSynchronized)
			line += " " + defaultStartTag + defaultEndTag;
		return line;
	}

	private void ensureEmptyLineAfterHeader(Token writePos) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		if (header == null || !parameters.isEmpty() || !exceptions.isEmpty())
			return;
		if (header.lastToken.textEquals(ABAP.DYNAMIC_HELP_COMMENT_SIGN))
			return;
		insertLineBefore(writePos, ABAP.DYNAMIC_HELP_COMMENT_SIGN);
	}

	private AbapDocSection insertLineBefore(Token writePos, String text) throws IntegrityBrokenException, UnexpectedSyntaxAfterChanges {
		Token newLine = Token.createForAbap(1, spacesLeft, text, 0);
		adjustLineBreaks(writePos, newLine);

		if (inChain) {
			writePos.insertLeftSibling(newLine);
		} else {
			Command command = writePos.getParentCommand();
			Command newCommand = Command.create(newLine, command);
			try {
				newCommand.finishBuild(command.getSourceTextStart(), command.getSourceTextEnd());
			} catch (ParseException e) {
				throw new UnexpectedSyntaxAfterChanges(null, command, "parse error in newly created command");
			}
			command.insertLeftSibling(newCommand);
		}
		return new AbapDocSection(newLine, newLine, false);
	}
	
	private void adjustLineBreaks(Token writePos, Token firstToken) {
		if (writePos.lineBreaks == 0) {
			firstToken.copyWhitespaceFrom(writePos);
			writePos.setWhitespace(1, spacesLeft);
		}
	}

	public void removeSection(AbapDocSection abapDocSection) throws UnexpectedSyntaxException, UnexpectedSyntaxAfterChanges {
		Token nextToken = getNextToken(abapDocSection.firstToken);
		adjustLineBreaks(nextToken, abapDocSection.firstToken);
		if (inChain) {
			Term term = Term.createForTokenRange(abapDocSection.firstToken, abapDocSection.lastToken);
			term.removeFromCommand(false);
		} else {
			Section commandSection = Section.create(abapDocSection.firstToken.getParentCommand(), abapDocSection.lastToken.getParentCommand());
			commandSection.removeFromCode();
		}
		
		// if this section was the last parameter or exception documentation, remove the empty line at the end of the header
		nextToken = getNextToken(header.lastToken);
		if (header != null && !nextToken.isAbapDocCommentLine() 
				&& header.firstToken != header.lastToken && header.lastToken.textEquals(ABAP.ABAP_DOC_SIGN)) {
			if (inChain) {
				header.lastToken.removeFromCommand();
			} else {
				header.lastToken.getParentCommand().removeFromCode();
			}
		}
	}
}
