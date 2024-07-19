package com.sap.adt.abapcleaner.comparer;

import com.sap.adt.abapcleaner.base.Language;
import com.sap.adt.abapcleaner.parser.*;
import java.util.*;

/**
 * <p>Represents a single (code) line. Note that a {@link Command} may span several lines and therefore several DisplayLines. 
 * To support display of such a code line, it is split into {@link #textBits}, each of which can be displayed with one {@link ColorType}.</p>
 *
 * <p>When comparing two versions of code (original and changed version), each resulting {@link DiffLine} 
 * consists of a 'left' and a 'right' DisplayLine. To support highlighting of code changes (e.g. with background color highlighting), 
 * {@link #getHighlightBits()} returns the ranges (start position, length) of text sections in which different types of changes were detected ({@link HighlightBitType}).</p>
 */
public class DisplayLine {
	// provided in the constructor
	final Command parentCommand;
	private String text;
	public int indexInDoc; // 0-based
	private ArrayList<TextBit> textBits;

	// results of the comparison
	private ArrayList<HighlightBit> highlightBits; // only for changed lines

	public final String getText() { return text; }

	public final boolean isEmpty() { return text.length() == 0; }

	public final Iterable<TextBit> getTextBits() { return textBits; }

	public final Iterable<HighlightBit> getHighlightBits() { return highlightBits; }

	public final int getHighlightBitCount() { return highlightBits == null ? 0 : highlightBits.size(); }

	public final boolean isCommandInCleanupRange() { return parentCommand != null && parentCommand.isInCleanupRange(); }

	public final boolean isAbapCommand() { return parentCommand != null && parentCommand.isAbap(); }
	
	public final boolean isDdlCommand() { return parentCommand != null && parentCommand.isDdl(); }
	
	public final Language getLanguage() { return (parentCommand != null ? parentCommand.getLanguage() : Language.ABAP); }
	
	@Override
	public String toString() { // for debugging
		return text;
	}

	public static DisplayLine create(Command parentCommand, String text, int indexInDoc) {
	 	return new DisplayLine(parentCommand, text, indexInDoc, null); 
	}

	public static DisplayLine create(Command parentCommand, String text, int indexInDoc, ArrayList<TextBit> textBits) {
	 	return new DisplayLine(parentCommand, text, indexInDoc, textBits); 
	}

	private DisplayLine(Command parentCommand, String text, int indexInDoc, ArrayList<TextBit> textBits) {
		this.parentCommand = parentCommand;
		this.text = text;
		this.indexInDoc = indexInDoc;
		this.textBits = textBits;
	}

	final void setHighlightBits(ArrayList<HighlightBit> bits) { highlightBits = bits; }

	public final String getTextWithHighlightedChanges() {
		if (highlightBits == null || highlightBits.isEmpty())
			return text;

		int writtenCharCount = 0;
		StringBuilder result = new StringBuilder();
		for (HighlightBit bit : highlightBits) {
			if (bit.start > writtenCharCount)
				result.append(text.substring(writtenCharCount, bit.start));
			result.append("{").append(text.substring(bit.start, bit.start + bit.length)).append("}");
			writtenCharCount = bit.getEnd();
		}
		if (writtenCharCount < text.length())
			result.append(text.substring(writtenCharCount));
		return result.toString();
	}
}