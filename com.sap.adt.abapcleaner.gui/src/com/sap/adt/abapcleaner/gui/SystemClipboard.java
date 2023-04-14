package com.sap.adt.abapcleaner.gui;

import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.lang.IllegalStateException;

public class SystemClipboard {
	private static Clipboard getClipboard() { 
		return Toolkit.getDefaultToolkit().getSystemClipboard(); 
	}
	
	public static boolean containsText() {
		return getClipboard().isDataFlavorAvailable(DataFlavor.stringFlavor);
	}
	
	public static String getText() {
		Clipboard clip = getClipboard();
		DataFlavor dataFlavor = DataFlavor.stringFlavor;

		try {
			if (clip.isDataFlavorAvailable(dataFlavor)) {
				return (String) clip.getData(dataFlavor);
			} else {
				return null;
			}
		} catch (UnsupportedFlavorException | IOException | IllegalStateException e) {
			return null;
		}
	}

	public static void setText(String text) {
		getClipboard().setContents(new StringSelection(text), null);
	}
}
