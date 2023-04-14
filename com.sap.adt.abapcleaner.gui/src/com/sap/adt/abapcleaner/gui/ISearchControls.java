package com.sap.adt.abapcleaner.gui;

public interface ISearchControls {
   void searchModeChanged(boolean searchMode);
   void searchTextChanged(String text, boolean found);
   boolean searchLeftDisplay();
   boolean searchRightDisplay();
   boolean searchChangedLines();
   boolean matchCase();
   boolean matchWholeWord();
}