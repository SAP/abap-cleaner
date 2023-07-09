package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.programbase.*;

/**
 * Parses the ABAP code string that is supplied to the {@link #Parser(String)} constructor into {@link Token}s 
 * and {@link Command}s, and appends them to the {@link Code} instance which was supplied to the 
 * {@link #parse(IProgress, Code)} and {@link #parse(IProgress, Code, int, int)} methods.
 */
class Parser {
	private String text;
	private Command curCommand;
	private Command lastCommand;
	private Token lastToken;

	static Parser create(String text) {
		return new Parser(text);
	}
	
	private Parser(String text) {
		if (text == null)
			throw new NullPointerException("text");
		this.text = text; 
	}

	final Code parse(IProgress progress, Code code) throws ParseException {
		return parse(progress, code, 1, 0);
	}

	final Code parse(IProgress progress, Code code, int lineNumOffset, int surroundingTextOffset) throws ParseException {
		Tokenizer tokenizer = new Tokenizer(text, lineNumOffset, progress);

		curCommand = null;
		lastCommand = null;
		lastToken = null;

		try {
			int commandStart = tokenizer.getReadPos();

			do {
				int tokenStart = tokenizer.getReadPos();
				Token token = tokenizer.getNext();

				if (token == null) {
					// finalize the last Command 
					if (curCommand != null) {
						curCommand.finishBuild(surroundingTextOffset + commandStart, surroundingTextOffset + text.length());
						if (lastCommand != null)
							lastCommand.addNext(curCommand);
					}
					break;
				}
				
				if (curCommand == null) {
					// create first command with this Token
					curCommand = Command.create(code, token, tokenizer.getCurLanguage());
				} else if (curCommand.canAdd(token)) {
					// add Token to current command
					lastToken.addNext(token);
				} else {
					// finalize Command
					curCommand.finishBuild(surroundingTextOffset + commandStart, surroundingTextOffset + tokenStart);
					if (lastCommand != null)
						lastCommand.addNext(curCommand);

					// determine whether a non-ABAP section was started, i.e. a section inside
					// "EXEC SQL ... ENDEXEC" or "METHOD <identifier> BY DATABASE PROCEDURE|FUNCTION|GRAPH ... ENDMETHOD"
					if (curCommand.getLanguage() == Language.ABAP) {
						Language nextLanguage = curCommand.getLanguageOfNextCommand();
						if (nextLanguage != Language.ABAP) {
							// unless the non-ABAP section is empty (i.e. token is already ENDEXEC or ENDMETHOD), 
							// create the current Token again, because we now know that it belongs to a non-ABAP language; 
							// also, tell the Tokenizer which keyword will end the non-ABAP section
							String endOfNonAbapSection = curCommand.firstCodeTokenIsKeyword("EXEC") ? "ENDEXEC" : "ENDMETHOD";
							if (!token.textEquals(endOfNonAbapSection)) {
								token = tokenizer.changeToNonAbapLanguage(token, nextLanguage, endOfNonAbapSection);
							}
						}
					}

					lastCommand = curCommand;

					// create new command with this Token
					curCommand = Command.create(code, token, tokenizer.getCurLanguage());
					commandStart = tokenStart;
				}
				lastToken = token;

				if (progress != null && progress.isCancellationPending())
					return null;
			} while(true);
			
			code.finishBuild();
			return code;

		} catch (UnexpectedSyntaxException ex) {
			throw new ParseException(code, tokenizer.getLineNum(), ex);
		} catch (RuntimeException ex) {
			throw new ParseException(code, tokenizer.getLineNum(), ex.getMessage());
		}
	}
}