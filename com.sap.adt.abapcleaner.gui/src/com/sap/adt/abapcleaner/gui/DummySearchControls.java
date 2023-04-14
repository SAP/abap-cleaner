package com.sap.adt.abapcleaner.gui;

class DummySearchControls implements ISearchControls {
	public final void searchModeChanged(boolean searcheMode) {
	}

	public final void searchTextChanged(String text, boolean found) {
	}

	public final boolean searchLeftDisplay() {
		return false;
	}

	public final boolean searchRightDisplay() {
		return true;
	}

	public final boolean searchChangedLines() {
		return false;
	}

	public final boolean matchCase() {
		return false;
	}

	public final boolean matchWholeWord() {
		return false;
	}
}
