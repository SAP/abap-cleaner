package com.sap.adt.abapcleaner.parser;

import com.sap.adt.abapcleaner.base.Cult;
import com.sap.adt.abapcleaner.programbase.*;

/**
 * <p>A Section may either be a single {@link Command} or 
 * any range of {@link Command}s that are (possibly remote) siblings.</p>
 *  
 * <p>The Section's {@link #firstCommand} and {@link #lastCommand} are siblings; the {@link #lastCommand} is always childless.
 * Sections can be removed with {@link #removeFromCode()} 
 * and inserted elsewhere with {@link Command#insertRightSibling(Section, boolean)}.</p>
 */
public class Section {
	public final Command firstCommand;
	public final Command lastCommand;

	public static Section create(Command firstCommand, Command lastCommand) throws UnexpectedSyntaxException {
		return new Section(firstCommand, lastCommand);
	}
	
	private Section(Command firstCommand, Command lastCommand) throws UnexpectedSyntaxException {
		if (firstCommand == null)
			throw new NullPointerException("firstCommand");
		if (lastCommand == null)
			throw new NullPointerException("lastCommand");

		this.firstCommand = firstCommand;
		this.lastCommand = lastCommand;

		if (lastCommand.hasChildren()) {
			throw new UnexpectedSyntaxException(this.lastCommand,
					"Command '" + this.lastCommand.firstToken.text + " ...' unexpected as last command of a Section, since it has child commands!");
		}
		
		// firstCommand and lastCommand must be siblings
		Command command = this.firstCommand;
		while (command != this.lastCommand) {
			command = command.getNextSibling();
			if (command == null) {
				throw new UnexpectedSyntaxException(this.lastCommand, "The first and last Command of a Section must be siblings, but " 
						+ "'" + firstCommand.firstToken.text + " ...' (line " + Cult.format(firstCommand.getSourceLineNumStart()) + ") and " 
						+ "'" + lastCommand.firstToken.text + " ...' (line " + Cult.format(lastCommand.getSourceLineNumStart()) + ") are not.");
			}
		}
	}

	public final boolean isSingleCommand() { return (firstCommand == lastCommand); }

	private Code getParentCode() { return firstCommand.getParentCode(); }

	final Command getParent() { return firstCommand.getParent(); }

	final void setParent(Command value) {
		Command command = firstCommand;
		command.setParent(value);
		while (command != lastCommand) {
			command = command.getNextSibling();
			command.setParent(value);
		}
	}

	final Command getPrev() { return firstCommand.getPrev(); }
	final void setPrev(Command value) {
		firstCommand.setPrev(value);
	}

	final Command getNext() { return lastCommand.getNext(); } // lastCommand is sure to be childless
	final void setNext(Command value) {
		lastCommand.setNext(value);
	}

	final Command getPrevSibling() { return firstCommand.getPrevSibling(); }
	final void setPrevSibling(Command value) {
		firstCommand.setPrevSibling(value);
	}

	final Command getNextSibling() { return lastCommand.getNextSibling(); }
	final void setNextSibling(Command value) {
		lastCommand.setNextSibling(value);
	}

	final int getCommandCountWithChildren() {
		Command command = firstCommand;
		int result = 1;
		while (command != lastCommand) {
			command = command.getNext();
			++result;
		}
		return result;
	}

	final int getSiblingCount() {
		Command command = firstCommand;
		int result = 1;
		while (command != lastCommand) {
			command = command.getNextSibling();
			++result;
		}
		return result;
	}

	public final void removeFromCode() {
		if (getParentCode().firstCommand == firstCommand)
			getParentCode().firstCommand = getNext();
		if (getParentCode().lastCommand == lastCommand)
			getParentCode().lastCommand = getPrev();
		getParentCode().commandCount -= getCommandCountWithChildren();

		if (getParent() != null) {
			if (getParent().getFirstChild() == firstCommand && getParent().getLastChild() == lastCommand) {
				getParent().setFirstChild(null);
				getParent().setLastChild(null);
			} else if (getParent().getFirstChild() == firstCommand) {
				getParent().setFirstChild(getNext());
			} else if (getParent().getLastChild() == lastCommand) {
				getParent().setLastChild(getPrev());
			}
		}
		if (getPrev() != null)
			getPrev().setNext(getNext());
		if (getNext() != null)
			getNext().setPrev(getPrev());

		if (getPrevSibling() != null)
			getPrevSibling().setNextSibling(getNextSibling());
		if (getNextSibling() != null)
			getNextSibling().setPrevSibling(getPrevSibling());
	}

	/**
	 * adds the provided (and possibly negative) number of spaces to the indent of all lines covered by this Section;
	 * returns true if whitespace was changed
	 * 
	 * @param spaceCount
	 */
	public final boolean addIndent(int spaceCount) {
		if (spaceCount == 0)
			return false;

		boolean changed = false;
		Command command = firstCommand;
		do {
			Token token = command.firstToken;
			while (token != null) {
				if (token.lineBreaks > 0 && !token.isAsteriskCommentLine()) {
					token.spacesLeft = Math.max(token.spacesLeft + spaceCount, 0);
					changed = true;
				}
				token = token.getNext();
			}
			if (command == lastCommand)
				break;
			command = command.getNext();
		} while (command != null);

		return changed;
	}

	@Override
	public String toString() {
		StringBuilder result = new StringBuilder();
		Command command = firstCommand;
		result.append(command.toString());
		while (command != lastCommand) {
			command = command.getNext();
			result.append(command.toString());
		}
		return result.toString();
	}
	
	void setParentCode(Code parentCode) {
		Command command = firstCommand;
		do {
			command.setParentCode(parentCode);
			if (command == lastCommand)
				break;
			command = command.getNext();
		} while (command != null);
	}
	
	public boolean contains(Command searchCommand) {
		Command command = firstCommand;
		do {
			if (command == searchCommand)
				return true;
			if (command == lastCommand)
				break;
			command = command.getNext();
		} while (command != null);

		return false;
	}
}
