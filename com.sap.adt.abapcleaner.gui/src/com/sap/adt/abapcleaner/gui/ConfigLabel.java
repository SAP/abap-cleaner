package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import com.sap.adt.abapcleaner.rulebase.*;

class ConfigLabel extends ConfigControl {
   private Label lblDescription;

   @Override
   public Control[] getControls() {
      return new Control[] { lblDescription };
   }
   @Override
	public Control[] getControlsForHighlight() { 
   	return new Control[] { lblDescription };
  	}

   ConfigLabel(ConfigInfoValue configValue, IConfigDisplay configDisplay, Composite parent) {
      super(configValue, configDisplay);

      lblDescription = new Label(parent, SWT.NONE);
      lblDescription.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
      lblDescription.setText(configValue.description);
      if (configValue.isWarning)
      	lblDescription.setForeground(new Color(255, 0, 0));
   }

   @Override
   protected void detachControls() {
      if (lblDescription != null)
         lblDescription = null;
   }

   @Override
   public void setDefault() {
   }

   @Override
   public void setEnabled(boolean enabled) {
   	lblDescription.setText(enabled ? configValue.description : " ");
   	if (enabled)
   		lblDescription.requestLayout();
   }
}
