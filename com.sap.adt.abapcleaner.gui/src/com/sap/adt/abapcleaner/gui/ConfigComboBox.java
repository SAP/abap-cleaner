package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import com.sap.adt.abapcleaner.rulebase.*;

class ConfigComboBox extends ConfigControl {
   private Label lblDescription;
   private Combo cboValue;

   @Override
   public Control[] getControls() {
      return new Control[] { lblDescription, cboValue };
   }
   @Override
	public Control[] getControlsForHighlight() { 
   	return new Control[] { lblDescription };
  	}
   private ConfigSelectionValue getConfigSelectionValue() {
      return (ConfigSelectionValue)configValue;
   }

   ConfigComboBox(ConfigSelectionValue configValue, IConfigDisplay configDisplay, Composite parent) {
      super(configValue, configDisplay);

      lblDescription = new Label(parent, SWT.NONE);
      lblDescription.setText(configValue.description);

      cboValue = new Combo(parent, SWT.READ_ONLY);
      cboValue.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 2, 1));
		cboValue.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				selectionChanged();
			}
		});

		cboValue.removeAll();
      String[] items = configValue.selection;
      for (String item : items)
      	cboValue.add(item);
      cboValue.select(configValue.getValue());
}

    @Override
   protected void detachControls() {
      if (lblDescription != null)
         lblDescription = null;

      if (cboValue != null) {
      	removeListeners(cboValue, SWT.Selection);
         cboValue = null;
      }
   }

   @Override
   public void setDefault() {
      if (cboValue != null) {
         cboValue.select(getConfigSelectionValue().defaultValue);
         selectionChanged();
      }
   }
   
   @Override
   public void setEnabled(boolean enabled) {
   	cboValue.setEnabled(enabled);
   }
   
   private void selectionChanged() {
	   getConfigSelectionValue().setValue(cboValue.getSelectionIndex());
      configDisplay.configurationChanged();
   }
}
