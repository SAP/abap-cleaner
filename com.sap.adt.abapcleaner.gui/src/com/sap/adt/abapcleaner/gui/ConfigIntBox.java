package com.sap.adt.abapcleaner.gui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import com.sap.adt.abapcleaner.base.*;
import com.sap.adt.abapcleaner.rulebase.*;

class ConfigIntBox extends ConfigControl {
   private Label lblDescription;
   private Text txtValue;
   private Label lblUnit;

   @Override
   public Control[] getControls() {
      return new Control[] { lblDescription, txtValue, lblUnit };
   }
   @Override
	public Control[] getControlsForHighlight() { 
   	return new Control[] { lblDescription };
  	}
   private ConfigIntValue getConfigIntValue() {
      return (ConfigIntValue)configValue;
   }

   ConfigIntBox(ConfigIntValue configValue, IConfigDisplay configDisplay, Composite parent) {
      super(configValue, configDisplay);

      lblDescription = new Label(parent, SWT.NONE);
      lblDescription.setText(configValue.description);

      txtValue = new Text(parent, SWT.BORDER | SWT.RIGHT);
      txtValue.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent arg0) {
		      getConfigIntValue().setValue(SettingsCult.parseInt(txtValue.getText()));
		      configDisplay.configurationChanged();
			}
		});
      txtValue.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				txtValueKeyPressed(e);
			}
		});
      txtValue.addFocusListener(new FocusAdapter() {
			@Override
			public void focusGained(FocusEvent e) {
		      txtValue.selectAll();
			}
			@Override
			public void focusLost(FocusEvent e) {
		      int currentValue = SettingsCult.parseInt(txtValue.getText());
		      int newValue = Math.min(Math.max(currentValue, getConfigIntValue().minValue), getConfigIntValue().maxValue);
		      if (newValue != currentValue)
		      	txtValue.setText(SettingsCult.toString(newValue)); 
			}
		});
      txtValue.setText(SettingsCult.toString(configValue.getValue()));

      // set the width hint depending on the width of the maximum value
      GC gc = new GC(txtValue);
      gc.setFont(txtValue.getFont());
      GridData gridData = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
      gridData.widthHint = gc.stringExtent(SettingsCult.toString(configValue.maxValue)).x;
      txtValue.setLayoutData(gridData);
      gc.dispose();
      
      lblUnit = new Label(parent, SWT.NONE);
      lblUnit.setText(configValue.unit);
   }

   @Override
   protected void detachControls() {
      if (lblDescription != null)
         lblDescription = null;

      if (txtValue != null) {
      	removeListeners(txtValue, SWT.Modify);
      	removeListeners(txtValue, SWT.Verify);
      	removeListeners(txtValue, SWT.KeyDown);
      	removeListeners(txtValue, SWT.FocusIn);
         txtValue = null;
      }

      if (lblUnit != null)
         lblUnit = null;
   }

   @Override
   public void setDefault() {
      if (txtValue != null)
         txtValue.setText(SettingsCult.toString(getConfigIntValue().defaultValue));
   }

   @Override
   public void setEnabled(boolean writable, boolean enabled) {
   	txtValue.setEnabled(writable && enabled);
   }
   
   private void txtValueKeyPressed(KeyEvent e) {
      int currentValue = SettingsCult.parseInt(txtValue.getText());
      boolean controlPressed = ((e.stateMask & SWT.CONTROL) != 0);
      int newValue;
      switch (e.keyCode) {
         case SWT.ARROW_DOWN:
            newValue = currentValue - getConfigIntValue().smallStep;
            break;
         case SWT.ARROW_UP:
            newValue = currentValue + getConfigIntValue().smallStep;
            break;
         case SWT.PAGE_DOWN:
            newValue = currentValue - getConfigIntValue().largeStep;
            break;
         case SWT.PAGE_UP:
            newValue = currentValue + getConfigIntValue().largeStep;
            break;
         case SWT.HOME:
            if (controlPressed) {
            	newValue = getConfigIntValue().minValue;
            	break;
            } else {
            	return;
            }
         case SWT.END:
            if (controlPressed) {
            	newValue = getConfigIntValue().maxValue;
            	break;
            } else {
            	return;
            }
         case SWT.DEL:
            if (controlPressed) {
               newValue = getConfigIntValue().defaultValue;
               break;
            } else {
            	return;
            }
         case SWT.BS: // allow backspace key 
         	return;
         default:
            if (e.character < '0' || e.character > '9')
               e.doit = false;
            return;
      }
      e.doit = false;
      newValue = Math.min(Math.max(newValue, getConfigIntValue().minValue), getConfigIntValue().maxValue);
      if (newValue != currentValue)
         txtValue.setText(SettingsCult.toString(newValue));
   }
}
