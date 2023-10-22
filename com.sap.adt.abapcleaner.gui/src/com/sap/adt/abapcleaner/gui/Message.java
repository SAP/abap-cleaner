package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

public class Message {
   public static void show(String message, Shell caller) {
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent()), SWT.APPLICATION_MODAL); // SWT.YES | SWT.NO | SWT.ICON_QUESTION
	   messageBox.setMessage(message);
	   messageBox.open();
	   bringCallerToFront(caller);
   }

   public static void show(String message, String title, Shell caller) {
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent()), SWT.APPLICATION_MODAL); // SWT.YES | SWT.NO | SWT.ICON_QUESTION
	   messageBox.setMessage(message);
	   messageBox.setText(title);
	   messageBox.open();
	   bringCallerToFront(caller);
   }

   public static int show(String message, String title, int buttons, Shell caller) {
   	// buttons: e.g. SWT.YES | SWT.NO | SWT.ICON_QUESTION
   	MessageBox messageBox = new MessageBox(new Shell(Display.getCurrent()), buttons | SWT.APPLICATION_MODAL); 
	   messageBox.setMessage(message);
	   messageBox.setText(title);
	   int result = messageBox.open();
	   bringCallerToFront(caller);
		return result;
   }
   
   private static void bringCallerToFront(Shell caller) {
		if (caller != null && !caller.getMinimized()) {
			caller.forceActive();
		}
   }
}
