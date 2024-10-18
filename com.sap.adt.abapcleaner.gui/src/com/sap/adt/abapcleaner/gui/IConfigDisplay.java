package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.widgets.Control;

import com.sap.adt.abapcleaner.rulebase.ConfigValue;

interface IConfigDisplay {
   void remove(Control[] controls);
   void configurationChanged();
   void buttonClicked(ConfigValue configValue);
}
