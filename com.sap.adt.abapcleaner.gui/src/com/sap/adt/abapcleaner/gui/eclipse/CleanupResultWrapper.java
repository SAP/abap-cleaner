package com.sap.adt.abapcleaner.gui.eclipse;

import com.sap.adt.abapcleaner.parser.CleanupResult;
import org.eclipse.jface.text.ITextSelection;

public class CleanupResultWrapper implements ITextSelection {
	private CleanupResult result;
	
	public CleanupResultWrapper(CleanupResult result) {
		this.result = result;
	}

	@Override
	public boolean isEmpty() { 
		return (result.length > 0); 
	}

	@Override
	public int getOffset() {
		return result.offset; 
	}

	@Override
	public int getLength() {
		return result.length; 
	}

	@Override
	public int getStartLine() { 
		return result.startLine; 
	}

	@Override
	public int getEndLine() { 
		return result.endLine; 
	}

	@Override
	public String getText() { 
		return result.getSelectedText(); 
	}
}
