package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

public class Message {
   public static void show(String message) {
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent())); // SWT.YES | SWT.NO | SWT.ICON_QUESTION
	   messageBox.setMessage(message);
	   messageBox.open();
   }

   public static void show(String message, String title) {
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent())); // SWT.YES | SWT.NO | SWT.ICON_QUESTION
	   messageBox.setMessage(message);
	   messageBox.setText(title);
	   messageBox.open();
   }

   public static int show(String message, String title, int buttons) {
   	// buttons: e.g. SWT.YES | SWT.NO | SWT.ICON_QUESTION
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent()), buttons); 
	   messageBox.setMessage(message);
	   messageBox.setText(title);
	   return messageBox.open();
   }
}
