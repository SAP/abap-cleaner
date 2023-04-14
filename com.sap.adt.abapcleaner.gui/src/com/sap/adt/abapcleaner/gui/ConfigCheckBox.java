package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

import com.sap.adt.abapcleaner.rulebase.*;

class ConfigCheckBox extends ConfigControl {
   private Button chkValue;

   @Override
   public Control[] getControls() {
      return new Control[] {chkValue};
   }
   @Override
	public Control[] getControlsForHighlight() { 
   	return new Control[] {chkValue};
  	}
   private ConfigBoolValue getConfigBoolValue() {
      return (ConfigBoolValue)configValue;
   }

   ConfigCheckBox(ConfigBoolValue configValue, IConfigDisplay configDisplay, Composite parent) {
      super(configValue, configDisplay);
      chkValue = new Button(parent, SWT.CHECK);
      chkValue.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		chkValue.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectionChanged();
			}
		});
      chkValue.setText(configValue.description);
      chkValue.setSelection(configValue.getValue());
   }

   @Override
   protected void detachControls() {
      if (chkValue != null) {
      	removeListeners(chkValue, SWT.Selection);
         chkValue = null;
      }
   }

   @Override
   public void setDefault() {
      if (chkValue != null) {
         chkValue.setSelection(getConfigBoolValue().defaultValue);
         selectionChanged();
      }
   }

   @Override
   public void setEnabled(boolean enabled) {
   	chkValue.setEnabled(enabled);
   }
   
   private void selectionChanged() {
      getConfigBoolValue().setValue(chkValue.getSelection());
      configDisplay.configurationChanged();
   }

}
