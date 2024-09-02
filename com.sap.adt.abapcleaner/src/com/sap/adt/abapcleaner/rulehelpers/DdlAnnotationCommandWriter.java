package com.sap.adt.abapcleaner.rulehelpers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.Command;
import com.sap.adt.abapcleaner.parser.Token;
import com.sap.adt.abapcleaner.programbase.ParseException;
import com.sap.adt.abapcleaner.programbase.UnexpectedSyntaxException;

public class DdlAnnotationCommandWriter extends DdlAnnotationWriter {
	private ArrayList<Command> commands = new ArrayList<>();
	private HashMap<Command, HashSet<Command>> originalCommandsOfNewCommand = new HashMap<>();
	private Command curCommand;
	private HashSet<Command> curOriginalCommands;
	
	public static DdlAnnotationCommandWriter create(DdlAnnotationLayout layout, int maxLevelCount) {
		return new DdlAnnotationCommandWriter(layout, maxLevelCount);
	}

	public DdlAnnotationCommandWriter(DdlAnnotationLayout layout, int maxLevelCount) {
		super(layout, maxLevelCount);
	}
	
	@Override
	protected void addToken(int lineBreaks, int spacesLeft, String text, Language language) throws UnexpectedSyntaxException {
		Token token = Token.create(lineBreaks, spacesLeft, text, originalValueToken.sourceLineNum, language);
		if (curCommand == null) {
			curCommand = Command.create(token, originalCommand, language);
			curOriginalCommands = new HashSet<>();
		} else {
			curCommand.getLastToken().addNext(token);
		}
	}

	@Override
	protected void addToLastTokenText(String text) {
		Token lastToken = curCommand.getLastToken(); 
		lastToken.setText(lastToken.getText() + text, false);
	}

	@Override
	protected void endAnnotation(Command originalCommand) {
		// TODO: remember all originalCommands on which curCommand is based (this is n:m)
		if (curOriginalCommands != null) {
			curOriginalCommands.add(originalCommand);
		}
	}
	
	@Override
	protected void finishCommandBuild() throws ParseException {
		if (curCommand != null) {
			curCommand.finishBuild(originalCommand.getSourceLineNumStart(), originalCommand.getSourceLineNumLast());

			commands.add(curCommand);
			originalCommandsOfNewCommand.put(curCommand, curOriginalCommands);
			
			curCommand = null;
			curOriginalCommands = null;
		}
	}

	@Override
	public String toText() { // for debugging
		StringBuilder sb = new StringBuilder();
		for (Command command : commands) {
			sb.append(command.toString());
		}
		return sb.toString();
	}

	public ArrayList<Command> getNewCommands() {
		return commands;
	}

	public HashSet<Command> getOriginalCommandsOf(Command newCommand) {
		return originalCommandsOfNewCommand.get(newCommand);
	}
}
