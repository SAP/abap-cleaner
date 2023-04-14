package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Control;

interface IConfigDisplay {
   void remove(Control[] controls);
   void configurationChanged();
}
